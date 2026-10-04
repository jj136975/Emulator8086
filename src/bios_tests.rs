//! Integration tests that run the real BIOS ROM on a fully wired machine.
//!
//! Each test powers the machine on, lets the BIOS get as far as it can, and
//! then either inspects the result or runs a small guest stub that calls a
//! BIOS service.
//!
//! The invariant: `run_stub` waits for its stub to park in the `CLI; HLT` it
//! appends (the test boot sector parks the same way), and the BIOS never
//! parks: it halts only with interrupts enabled, waiting for a key.
//! `Machine::parked` tells the two apart by that alone, and `run_until`
//! gives up after `MAX_FRAMES` batches instead of hanging the suite.

use std::sync::mpsc::Sender;

use crate::io::disk::{DiskController, DiskImage, FLOPPY_144_SIZE};
use crate::io::keyboard::{EmulatorEvent, KeyboardController};
use crate::vm::runtime::{CpuFlag, Prefixes, Runtime};

const BIOS_BIN: &[u8] = include_bytes!("../assets/rom/bios.bin");

/// Segment where guest stubs are loaded.
const STUB_SEG: u16 = 0x1000;
/// `step_frame` batches a test waits before giving up on the guest.
const MAX_FRAMES: usize = 2000;

/// Where the test boot sector records the drive number it was booted from,
/// and the marker it writes next to it.
const BOOT_DRIVE_ADDR: usize = 0x0500;
const BOOT_MARKER_ADDR: usize = 0x0501;
const BOOT_MARKER: u8 = 0xB0;

const FLAG_CARRY: u16 = 0x0001;
const FLAG_INTERRUPT: u16 = 0x0200;

struct Machine {
    rt: Runtime,
    keys: Sender<EmulatorEvent>,
}

impl Machine {
    /// A machine with the given disks and the ROM loaded, about to execute
    /// its first instruction.
    fn power_on(disks: DiskController) -> Self {
        let (mut rt, keys) = Runtime::new_test_machine(disks);
        rt.load_rom_image(BIOS_BIN);
        Self { rt, keys }
    }

    /// Powers on and runs until the CPU halts: either a boot sector parked
    /// it, or the BIOS is waiting for a key.
    fn boot(disks: DiskController) -> Self {
        let mut machine = Self::power_on(disks);
        machine.run_until(|rt| rt.cpu.halted);
        machine
    }

    fn run_until(&mut self, done: impl Fn(&Runtime) -> bool) {
        for _ in 0..MAX_FRAMES {
            self.rt.step_frame();
            if done(&self.rt) {
                return;
            }
        }
        panic!("guest did not reach the expected state");
    }

    /// The CPU was stopped by `CLI; HLT`, the way stubs and the test boot
    /// sector end. (The BIOS waiting for a key halts with interrupts on.)
    fn parked(rt: &Runtime) -> bool {
        rt.cpu.halted && !rt.cpu.check_flag(CpuFlag::Interrupt)
    }

    /// Points the CPU at `code`, loaded at STUB_SEG:0000, on its own stack
    /// and with interrupts enabled. Nothing is executed yet.
    fn load_stub(&mut self, code: &[u8]) {
        let base = (STUB_SEG as usize) << 4;
        for (i, byte) in code.iter().enumerate() {
            self.rt.cpu.memory.write_byte(base + i, *byte);
        }
        let registers = &mut self.rt.cpu.registers;
        registers.cs.reg_mut().set(STUB_SEG);
        registers.pc.set(0);
        registers.ss.reg_mut().set(0x2000);
        registers.sp.set(0x1000);
        self.rt.cpu.halted = false;
        self.rt.cpu.set_flag(CpuFlag::Interrupt);
        self.rt.prefixes = Prefixes::default();
    }

    /// Runs `code` until it falls into the `CLI; HLT` appended here.
    fn run_stub(&mut self, code: &[u8]) {
        let mut program = code.to_vec();
        program.extend_from_slice(&[0xFA, 0xF4]);
        self.load_stub(&program);
        self.run_until(Self::parked);
    }

    /// Lets `nanos` of timer time go by, then runs one frame; `steps` times.
    fn feed_time(&mut self, steps: usize, nanos: u64) {
        for _ in 0..steps {
            self.rt.credit_pit_time(nanos);
            self.rt.step_frame();
        }
    }

    /// The BIOS tick counter at 0040:006C, incremented by every INT 08h.
    fn timer_ticks(&self) -> u32 {
        let memory = &self.rt.cpu.memory;
        memory.read_word(0x046C) as u32 | (memory.read_word(0x046E) as u32) << 16
    }

    /// Sends a key press followed by its release.
    fn press(&self, scancode: u8) {
        self.keys.send(EmulatorEvent::Scancode(scancode)).unwrap();
        self.keys
            .send(EmulatorEvent::ScanRelease(scancode | 0x80))
            .unwrap();
    }

    /// The 80x25 text screen, one line per row.
    fn screen_text(&self) -> String {
        let mut text = String::new();
        for row in 0..25 {
            for col in 0..80 {
                let ch = self.rt.cpu.memory.read_byte(0xB8000 + (row * 80 + col) * 2);
                text.push(if (0x20..0x7F).contains(&ch) {
                    ch as char
                } else {
                    ' '
                });
            }
            text.push('\n');
        }
        text
    }

    /// The drive the test boot sector was started from, if it ran.
    fn booted_drive(&self) -> Option<u8> {
        let memory = &self.rt.cpu.memory;
        (memory.read_byte(BOOT_MARKER_ADDR) == BOOT_MARKER)
            .then(|| memory.read_byte(BOOT_DRIVE_ADDR))
    }
}

/// A boot sector that records the drive number the BIOS passed in DL and
/// parks the CPU.
fn boot_sector() -> Vec<u8> {
    let mut sector = vec![0u8; 512];
    let code = [
        0x88,
        0x16,
        0x00,
        0x05, // MOV [0x0500],DL
        0xC6,
        0x06,
        0x01,
        0x05,
        BOOT_MARKER, // MOV BYTE [0x0501],marker
        0xFA,        // CLI
        0xF4,        // HLT
    ];
    sector[..code.len()].copy_from_slice(&code);
    sector[510] = 0x55;
    sector[511] = 0xAA;
    sector
}

fn blank_floppy() -> DiskImage {
    DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false)
}

fn bootable_floppy() -> DiskImage {
    let mut image = blank_floppy();
    image.write_sectors(0, 0, 1, 1, &boot_sector()).unwrap();
    image
}

fn bootable_hard_disk() -> DiskImage {
    let mut image = DiskImage::new_in_memory_hard_disk(1);
    image.write_sectors(0, 0, 1, 1, &boot_sector()).unwrap();
    image
}

fn disks(drives: Vec<(u8, DiskImage)>) -> DiskController {
    let mut controller = DiskController::new();
    for (drive, image) in drives {
        controller.attach(drive, image);
    }
    controller
}

// ---------------------------------------------------------------------------
// INT 19h — bootstrap
// ---------------------------------------------------------------------------

#[test]
fn boots_floppy_before_hard_disk_by_default() {
    let machine = Machine::boot(disks(vec![
        (0x00, bootable_floppy()),
        (0x80, bootable_hard_disk()),
    ]));
    assert_eq!(machine.booted_drive(), Some(0x00));
}

/// The boot order given on the command line reaches the BIOS, which tries
/// the drives in that order rather than floppy A: first.
#[test]
fn boot_order_from_the_emulator_is_honoured() {
    let mut controller = disks(vec![
        (0x00, bootable_floppy()),
        (0x80, bootable_hard_disk()),
    ]);
    controller.set_boot_order(vec![0x80, 0x00]);
    let machine = Machine::boot(controller);
    assert_eq!(machine.booted_drive(), Some(0x80));
}

#[test]
fn boot_skips_drives_without_a_boot_signature() {
    let machine = Machine::boot(disks(vec![
        (0x00, blank_floppy()),
        (0x80, bootable_hard_disk()),
    ]));
    assert_eq!(machine.booted_drive(), Some(0x80));
}

#[test]
fn boot_skips_missing_drives() {
    let machine = Machine::boot(disks(vec![(0x80, bootable_hard_disk())]));
    assert_eq!(machine.booted_drive(), Some(0x80));
}

/// When no drive boots, the BIOS reaches its failure path: it says so and
/// waits, with interrupts enabled, for a key to try again.
#[test]
fn no_bootable_disk_reports_and_waits_for_a_key() {
    let machine = Machine::boot(disks(vec![(0x00, blank_floppy())]));
    assert_eq!(machine.booted_drive(), None);
    assert!(
        machine.screen_text().contains("No bootable disk"),
        "screen was:\n{}",
        machine.screen_text()
    );
    assert!(
        machine.rt.cpu.check_flag(CpuFlag::Interrupt),
        "the BIOS must wait with interrupts enabled so a key can wake it"
    );
}

#[test]
fn boot_is_retried_after_a_keypress() {
    let mut machine = Machine::boot(disks(vec![]));
    assert_eq!(machine.booted_drive(), None);

    machine
        .rt
        .disks
        .borrow_mut()
        .attach(0x00, bootable_floppy());
    machine.press(0x1C); // Enter
    machine.run_until(Machine::parked);

    assert_eq!(machine.booted_drive(), Some(0x00));
}

/// `Runtime::reset` reboots the guest: it starts over from the reset vector
/// and boots again.
#[test]
fn reset_reboots_the_guest() {
    let mut machine = Machine::boot(disks(vec![(0x00, bootable_floppy())]));
    assert_eq!(machine.booted_drive(), Some(0x00));
    machine.rt.cpu.memory.write_byte(BOOT_MARKER_ADDR, 0);

    machine.rt.reset();
    machine.run_until(Machine::parked);

    assert_eq!(
        machine.booted_drive(),
        Some(0x00),
        "the boot sector must run again"
    );
}

// ---------------------------------------------------------------------------
// Keyboard
// ---------------------------------------------------------------------------

/// A key pressed before the BIOS has programmed the interrupt controller
/// does not block the keyboard: the keys after it still reach the guest.
#[test]
fn key_pressed_before_boot_does_not_block_the_keyboard() {
    let mut machine = Machine::power_on(disks(vec![]));
    machine.press(0x1E); // 'a', before the first instruction runs
    machine.run_until(|rt| rt.cpu.halted);

    machine.press(0x1F); // 's'
    machine.run_stub(&[
        0xB4, 0x00, // MOV AH,00h   (wait for a key)
        0xCD, 0x16, // INT 16h
    ]);
    assert_eq!(
        machine.rt.cpu.registers.ax.word(),
        0x1F73,
        "scancode 1Fh, ASCII 's'"
    );
}

/// The machine is wired to the keyboard controller its frontend hands to
/// `Runtime::new`, so the frontend decides where key events come from.
#[test]
fn machine_uses_the_keyboard_supplied_by_the_frontend() {
    let (keys, key_events) = std::sync::mpsc::channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    let mut rt = Runtime::new(vec![], vec![], None, false, keyboard).unwrap();
    rt.load_rom_image(BIOS_BIN);
    let mut machine = Machine { rt, keys };
    machine.run_until(|rt| rt.cpu.halted);

    machine.press(0x1F); // 's'
    machine.run_stub(&[
        0xB4, 0x00, // MOV AH,00h   (wait for a key)
        0xCD, 0x16, // INT 16h
    ]);
    assert_eq!(
        machine.rt.cpu.registers.ax.word(),
        0x1F73,
        "scancode 1Fh, ASCII 's'"
    );
}

// ---------------------------------------------------------------------------
// INT 13h — disk
// ---------------------------------------------------------------------------

/// INT 13h returns through `retf 2`, so the caller gets the handler's own
/// flags: interrupts enabled, and the carry flag of the disk trap.
#[test]
fn int13_returns_with_interrupts_enabled() {
    let mut machine = Machine::boot(disks(vec![]));
    machine.run_stub(&[
        0xB4, 0x08, // MOV AH,08h   (get drive parameters)
        0xB2, 0x80, // MOV DL,80h   (no such drive)
        0xCD, 0x13, // INT 13h
        0x9C, // PUSHF
        0x5B, // POP BX
    ]);
    let flags = machine.rt.cpu.registers.bx.word();
    assert_ne!(flags & FLAG_INTERRUPT, 0, "IF must be set after INT 13h");
    assert_ne!(
        flags & FLAG_CARRY,
        0,
        "CF from the disk trap must reach the caller"
    );
}

// ---------------------------------------------------------------------------
// INT 10h — video
// ---------------------------------------------------------------------------

/// AH=00h preserves BX, CX, DX, SI, DI and BP.
#[test]
fn int10_set_mode_preserves_registers() {
    let mut machine = Machine::boot(disks(vec![]));
    machine.run_stub(&[
        0xBB, 0x34, 0x12, // MOV BX,1234h
        0xB9, 0x78, 0x56, // MOV CX,5678h
        0xBA, 0xBC, 0x9A, // MOV DX,9ABCh
        0xBE, 0x11, 0x11, // MOV SI,1111h
        0xBF, 0x22, 0x22, // MOV DI,2222h
        0xBD, 0x33, 0x33, // MOV BP,3333h
        0xB8, 0x03, 0x00, // MOV AX,0003h   (set 80x25 text mode)
        0xCD, 0x10, // INT 10h
    ]);
    let registers = &machine.rt.cpu.registers;
    assert_eq!(registers.bx.word(), 0x1234, "BX");
    assert_eq!(registers.cx.word(), 0x5678, "CX");
    assert_eq!(registers.dx.word(), 0x9ABC, "DX");
    assert_eq!(registers.si.word(), 0x1111, "SI");
    assert_eq!(registers.di.word(), 0x2222, "DI");
    assert_eq!(registers.bp.word(), 0x3333, "BP");
}

// ---------------------------------------------------------------------------
// Timer — 8253 channel 0 -> IRQ0 -> INT 08h
// ---------------------------------------------------------------------------

/// Within a time slice a guest idling in HLT is looked at every millisecond,
/// not once per displayed frame, so its timer keeps ticking.
#[test]
fn timer_ticks_reach_an_idle_guest_during_a_time_slice() {
    // The BIOS has programmed 18.2 Hz and waits for a key in HLT.
    let mut machine = Machine::boot(disks(vec![]));
    let before = machine.timer_ticks();
    machine.rt.run_slice(std::time::Duration::from_millis(250));
    let ticks = machine.timer_ticks() - before;
    assert!(
        (3..=6).contains(&ticks),
        "{ticks} ticks in 250 ms, expected 4 or 5"
    );
}

/// The vertical retrace bit of port 3DAh follows timer time, not the reads
/// of the port: 70 frames in a second, however that second is delivered, so
/// a program that paces itself by retrace runs at the same speed on any host.
#[test]
fn vertical_retrace_comes_70_times_per_second_of_timer_time() {
    let mut machine = Machine::boot(disks(vec![]));
    machine.rt.use_manual_clock();
    machine.load_stub(&[
        0xBA, 0xDA, 0x03, // MOV DX,03DAh
        0x31, 0xDB, // XOR BX,BX
        0xEC, 0xA8, 0x08, 0x75, 0xFB, // wait while in retrace: IN AL,DX ; TEST AL,8 ; JNZ
        0xEC, 0xA8, 0x08, 0x74, 0xFB, // wait for retrace:      IN AL,DX ; TEST AL,8 ; JZ
        0x43, // INC BX
        0xEB, 0xF3, // JMP back to the first wait
    ]);
    machine.feed_time(50, 20_000_000); // one second, 20 ms at a time
    assert_eq!(machine.rt.cpu.registers.bx.word(), 70);
}

/// Guest idle loops: halted between interrupts, or spinning.
const IDLE_HALTED: [u8; 3] = [0xF4, 0xEB, 0xFD]; // HLT ; JMP back to the HLT
const IDLE_BUSY: [u8; 2] = [0xEB, 0xFE]; // JMP $

/// Boots, switches to a hand-fed clock, and starts a guest that programs
/// timer channel 0 with `control` and `divisor` and then idles in `idle`.
/// The BIOS tick counter is zero when this returns.
fn machine_with_timer(control: u8, divisor: u16, idle: &[u8]) -> Machine {
    let [lo, hi] = divisor.to_le_bytes();
    let mut code = vec![
        0xB0, control, 0xE6, 0x43, // MOV AL,control ; OUT 43h,AL
        0xB0, lo, 0xE6, 0x40, // MOV AL,lo ; OUT 40h,AL
        0xB0, hi, 0xE6, 0x40, // MOV AL,hi ; OUT 40h,AL
        0xFB, // STI
    ];
    code.extend_from_slice(idle);

    let mut machine = Machine::boot(disks(vec![]));
    machine.rt.use_manual_clock();
    machine.load_stub(&code);
    machine.rt.step_frame(); // runs the programming sequence; no time passes
    for addr in 0x046C..0x0470 {
        machine.rt.cpu.memory.write_byte(addr, 0);
    }
    machine
}

/// Mode 2 holds its output low for a single clock, so both edges fall into
/// the same batch of timer time; the rising edge's interrupt request stays
/// latched until the halted guest takes it.
#[test]
fn rate_generator_ticks_reach_a_halted_guest() {
    let mut machine = machine_with_timer(0x34, 0, &IDLE_HALTED);
    machine.feed_time(50, 20_000_000); // one second, 20 ms at a time
    assert_eq!(machine.timer_ticks(), 18); // 1,193,182 clocks / 65,536
}

/// A timer faster than the rate at which a halted guest is polled delivers
/// every tick.
#[test]
fn fast_timer_ticks_reach_a_halted_guest() {
    let mut machine = machine_with_timer(0x36, 1000, &IDLE_HALTED);
    machine.feed_time(50, 20_000_000);
    assert_eq!(machine.timer_ticks(), 1193); // 1,193,182 clocks / 1,000
}

/// The timer time owed after a gap between two frames is paid out a few
/// clocks per instruction, so every tick in the gap reaches the guest.
#[test]
fn timer_ticks_owed_after_a_frame_gap_are_all_delivered() {
    let mut machine = machine_with_timer(0x36, 1000, &IDLE_BUSY);
    machine.feed_time(1, 14_000_000); // one 14 ms gap = 16,704 clocks
    assert_eq!(machine.timer_ticks(), 16);
}

/// Writing a new count to a mode 0 one-shot sets its output low again, so
/// the next terminal count interrupts again.
#[test]
fn one_shot_timer_rearmed_with_a_new_count_interrupts_again() {
    let rearm_and_wait = [
        0xF4, // HLT                 (until the first interrupt)
        0xB0, 0xE8, 0xE6, 0x40, // MOV AL,E8h ; OUT 40h,AL
        0xB0, 0x03, 0xE6, 0x40, // MOV AL,03h ; OUT 40h,AL   (count 1000 again)
        0xF4, // HLT                 (until the second interrupt)
        0xFA, 0xF4, // CLI ; HLT
    ];
    let mut machine = machine_with_timer(0x30, 1000, &rearm_and_wait);
    machine.feed_time(2, 1_000_000); // 1 ms = 1,193 clocks each
    assert_eq!(machine.timer_ticks(), 2);
    assert!(
        Machine::parked(&machine.rt),
        "the guest must get past its second HLT"
    );
}

// ===========================================================================
// Helpers for the BIOS service tests below
// ===========================================================================

impl Machine {
    /// Powered on with no disks: POST has run and the BIOS is waiting for a
    /// key before it retries the boot.
    fn booted() -> Self {
        Self::boot(disks(vec![]))
    }

    fn bda_byte(&self, offset: usize) -> u8 {
        self.rt.cpu.memory.read_byte(0x0400 + offset)
    }

    fn bda_word(&self, offset: usize) -> u16 {
        self.rt.cpu.memory.read_word(0x0400 + offset)
    }

    /// A cell of text page `page` (pages are 4096 bytes apart): character in
    /// the low byte, attribute in the high byte.
    fn text_cell(&self, page: usize, row: usize, col: usize) -> u16 {
        self.rt
            .cpu
            .memory
            .read_word(0xB8000 + page * 4096 + (row * 80 + col) * 2)
    }

    fn set_text_cell(&mut self, page: usize, row: usize, col: usize, cell: u16) {
        self.rt
            .cpu
            .memory
            .write_word(0xB8000 + page * 4096 + (row * 80 + col) * 2, cell);
    }

    /// Reads register `index` of an index/data port pair.
    fn indexed_register(&mut self, index_port: u16, index: u8) -> u8 {
        self.rt.port_out_byte(index_port, index);
        self.rt.port_in_byte(index_port + 1)
    }

    /// CRTC display start address (registers 0Ch/0Dh), in character cells.
    fn display_start(&mut self) -> u16 {
        let high = self.indexed_register(0x3D4, 0x0C);
        let low = self.indexed_register(0x3D4, 0x0D);
        u16::from_be_bytes([high, low])
    }

    /// CRTC cursor location (registers 0Eh/0Fh), in character cells.
    fn cursor_location(&mut self) -> u16 {
        let high = self.indexed_register(0x3D4, 0x0E);
        let low = self.indexed_register(0x3D4, 0x0F);
        u16::from_be_bytes([high, low])
    }
}

// ===========================================================================
// INT 10h AH=00h — set video mode
// ===========================================================================

/// A mode set resets the CRTC display start and cursor location, the
/// cursors of all eight pages and the cursor shape, whatever the previous
/// program left in them.
#[test]
fn set_mode_resets_pages_and_cursors() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x01, 0x05, // MOV AX,0501h
        0xCD, 0x10, // INT 10h   (show page 1)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x01, // MOV BH,01h
        0xBA, 0x03, 0x02, // MOV DX,0203h
        0xCD, 0x10, // INT 10h   (page 1 cursor to row 2, column 3)
        0xB4, 0x01, // MOV AH,01h
        0xB9, 0x00, 0x20, // MOV CX,2000h
        0xCD, 0x10, // INT 10h   (hide the cursor)
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (set 80x25 text mode)
    ]);
    assert_eq!(machine.display_start(), 0, "CRTC display start");
    assert_eq!(machine.cursor_location(), 0, "CRTC cursor location");
    assert_eq!(machine.bda_byte(0x62), 0, "active page");
    assert_eq!(machine.bda_word(0x4E), 0, "offset of the active page");
    for page in 0..8 {
        assert_eq!(
            machine.bda_word(0x50 + page * 2),
            0,
            "cursor of page {page}"
        );
    }
    assert_eq!(machine.bda_word(0x60), 0x0607, "cursor shape");
    assert_eq!(
        machine.indexed_register(0x3D4, 0x0A),
        0x06,
        "CRTC cursor start line"
    );
    assert_eq!(
        machine.indexed_register(0x3D4, 0x0B),
        0x07,
        "CRTC cursor end line"
    );
}

/// A mode set also undoes a display start that a program wrote to the CRTC
/// itself, as programs that flip pages without the BIOS do.
#[test]
fn set_mode_undoes_a_display_start_written_to_the_crtc() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xBA, 0xD4, 0x03, // MOV DX,03D4h
        0xB0, 0x0C, // MOV AL,0Ch
        0xEE, // OUT DX,AL
        0x42, // INC DX
        0xB0, 0x08, // MOV AL,08h
        0xEE, // OUT DX,AL   (CRTC start address high = 08h: display page 1)
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (set 80x25 text mode)
    ]);
    assert_eq!(machine.display_start(), 0);
}

/// Mode 3 blanks all eight text pages, not only the 2000 cells of page 0.
#[test]
fn set_mode_3_blanks_every_text_page() {
    let mut machine = Machine::booted();
    for page in 0..8 {
        machine.set_text_cell(page, 0, 0, 0x4E58);
        machine.set_text_cell(page, 24, 79, 0x4E58);
    }
    machine.run_stub(&[
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (set 80x25 text mode)
    ]);
    for page in 0..8 {
        assert_eq!(
            machine.text_cell(page, 0, 0),
            0x0720,
            "first cell of page {page}"
        );
        assert_eq!(
            machine.text_cell(page, 24, 79),
            0x0720,
            "last cell of page {page}"
        );
    }
}

/// Going back to mode 3 reprograms the whole register set that mode 13h
/// changed, not just the 256-colour bit, and writes the BDA copies of the
/// CGA mode and colour registers.
#[test]
fn set_mode_3_reprograms_the_adapter_after_mode_13h() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x13, 0x00, // MOV AX,0013h
        0xCD, 0x10, // INT 10h   (320x200, 256 colours)
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (back to 80x25 text)
    ]);
    assert_eq!(machine.rt.port_in_byte(0x3CC), 0x67, "miscellaneous output");
    assert_eq!(
        machine.indexed_register(0x3C4, 0x04),
        0x02,
        "sequencer memory mode"
    );
    assert_eq!(machine.indexed_register(0x3CE, 0x05), 0x10, "graphics mode");
    assert_eq!(
        machine.indexed_register(0x3CE, 0x06),
        0x0E,
        "graphics miscellaneous"
    );
    assert_eq!(machine.bda_byte(0x49), 0x03, "video mode");
    assert_eq!(machine.bda_word(0x4A), 80, "columns");
    assert_eq!(
        machine.bda_byte(0x65),
        0x29,
        "copy of the mode control register"
    );
    assert_eq!(
        machine.bda_byte(0x66),
        0x30,
        "copy of the colour select register"
    );
}

// ===========================================================================
// INT 10h AH=00h — palette
// ===========================================================================

impl Machine {
    /// A DAC register (red, green, blue; 6 bits each), read through ports
    /// 3C7h/3C9h.
    fn dac_register(&mut self, index: u8) -> [u8; 3] {
        self.rt.port_out_byte(0x3C7, index);
        [0; 3].map(|_| self.rt.port_in_byte(0x3C9))
    }

    /// An attribute controller register, read through ports 3C0h/3C1h.
    fn attribute_register(&mut self, index: u8) -> u8 {
        self.rt.port_in_byte(0x3DA); // the next write to 3C0h selects a register
        self.rt.port_out_byte(0x3C0, index | 0x20); // bit 5 keeps the display on
        let value = self.rt.port_in_byte(0x3C1);
        self.rt.port_in_byte(0x3DA);
        value
    }

    /// What text colour number `colour` (0-15) is displayed as: its attribute
    /// controller palette register selects the DAC register.
    fn text_colour(&mut self, colour: u8) -> [u8; 3] {
        let index = self.attribute_register(colour) & 0x3F;
        self.dac_register(index)
    }
}

/// The 16 text colours, as 6-bit red, green, blue.
const TEXT_COLOURS: [[u8; 3]; 16] = [
    [0, 0, 0],
    [0, 0, 42],
    [0, 42, 0],
    [0, 42, 42],
    [42, 0, 0],
    [42, 0, 42],
    [42, 21, 0],
    [42, 42, 42],
    [21, 21, 21],
    [21, 21, 63],
    [21, 63, 21],
    [21, 63, 63],
    [63, 21, 21],
    [63, 21, 63],
    [63, 63, 21],
    [63, 63, 63],
];

#[test]
fn text_colours_after_power_on() {
    let mut machine = Machine::booted();
    for (colour, rgb) in TEXT_COLOURS.iter().enumerate() {
        assert_eq!(
            machine.text_colour(colour as u8),
            *rgb,
            "text colour {colour}"
        );
    }
}

/// A mode set reloads the DAC and the pixel mask, so the palette a
/// 256-colour program loaded does not carry over into text mode (after
/// DOOM, the DOS prompt would otherwise be almost black on black).
#[test]
fn set_mode_3_restores_the_text_colours() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x13, 0x00, // MOV AX,0013h
        0xCD, 0x10, // INT 10h   (320x200, 256 colours)
        0xBA, 0xC8, 0x03, // MOV DX,03C8h
        0x30, 0xC0, // XOR AL,AL
        0xEE, // OUT DX,AL   (start at DAC register 0)
        0x42, // INC DX
        0xB9, 0xC0, 0x00, // MOV CX,00C0h   (64 registers of 3 components)
        0xB0, 0x02, // MOV AL,02h
        // fill:
        0xEE, // OUT DX,AL   (an almost black palette)
        0xE2, 0xFD, // LOOP fill
        0xBA, 0xC6, 0x03, // MOV DX,03C6h
        0xB0, 0x0F, // MOV AL,0Fh
        0xEE, // OUT DX,AL   (pixel mask)
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (back to 80x25 text)
    ]);
    for (colour, rgb) in TEXT_COLOURS.iter().enumerate() {
        assert_eq!(
            machine.text_colour(colour as u8),
            *rgb,
            "text colour {colour}"
        );
    }
    assert_eq!(machine.rt.port_in_byte(0x3C6), 0xFF, "DAC pixel mask");
}

/// Mode 13h loads the default 256-colour palette rather than keeping what
/// the DAC holds, which after power-on is black for every colour above 31.
#[test]
fn set_mode_13h_loads_the_default_256_colour_palette() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x13, 0x00, // MOV AX,0013h
        0xCD, 0x10, // INT 10h   (320x200, 256 colours)
    ]);
    let expected: [(u8, [u8; 3]); 14] = [
        (0x00, [0, 0, 0]),
        (0x06, [42, 21, 0]),
        (0x0F, [63, 63, 63]),
        (0x11, [5, 5, 5]),
        (0x1F, [63, 63, 63]),
        (0x20, [0, 0, 63]),
        (0x28, [63, 0, 0]),
        (0x2C, [63, 63, 0]),
        (0x30, [0, 63, 0]),
        (0x50, [45, 45, 63]),
        (0x68, [0, 0, 28]),
        (0xB0, [0, 0, 16]),
        (0xF7, [11, 12, 16]),
        (0xFF, [0, 0, 0]),
    ];
    for (index, rgb) in expected {
        assert_eq!(
            machine.dac_register(index),
            rgb,
            "DAC register {index:02X}h"
        );
    }
}

/// A mode set programs the attribute controller, so a palette or mode
/// register one program changed does not carry over into the next mode.
#[test]
fn set_mode_programs_the_attribute_controller() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x13, 0x00, // MOV AX,0013h
        0xCD, 0x10, // INT 10h   (320x200, 256 colours)
    ]);
    assert_eq!(
        machine.attribute_register(0x06),
        0x06,
        "mode 13h palette is the identity"
    );
    assert_eq!(
        machine.attribute_register(0x0F),
        0x0F,
        "mode 13h palette is the identity"
    );
    assert_eq!(
        machine.attribute_register(0x10),
        0x41,
        "mode control: 256-colour graphics"
    );
    assert_eq!(machine.attribute_register(0x13), 0x00, "pixel panning");

    machine.run_stub(&[
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (80x25 text)
    ]);
    assert_eq!(machine.attribute_register(0x06), 0x14, "brown");
    assert_eq!(machine.attribute_register(0x08), 0x38, "dark grey");
    assert_eq!(machine.attribute_register(0x0F), 0x3F, "white");
    assert_eq!(
        machine.attribute_register(0x10),
        0x0C,
        "mode control: text, blink"
    );
    assert_eq!(
        machine.attribute_register(0x12),
        0x0F,
        "colour plane enable"
    );
    assert_eq!(machine.attribute_register(0x13), 0x08, "pixel panning");
}

// ===========================================================================
// INT 09h — scancode translation
// ===========================================================================

impl Machine {
    /// Feeds raw scancodes to the keyboard: make codes, and break codes with
    /// bit 7 set.
    fn send_scancodes(&self, codes: &[u8]) {
        for &code in codes {
            let event = if code & 0x80 == 0 {
                EmulatorEvent::Scancode(code)
            } else {
                EmulatorEvent::ScanRelease(code)
            };
            self.keys.send(event).unwrap();
        }
    }

    /// Types `codes` while the guest idles with interrupts enabled, and
    /// returns the keystrokes INT 09h queued (scan code in the high byte,
    /// character in the low byte). The BIOS buffer is left empty.
    fn typed(&mut self, codes: &[u8]) -> Vec<u16> {
        self.load_stub(&IDLE_HALTED);
        self.send_scancodes(codes);
        for _ in 0..3 {
            self.rt.step_frame();
        }
        self.take_keystrokes()
    }

    /// Empties the BIOS keyboard buffer and returns what was in it.
    fn take_keystrokes(&mut self) -> Vec<u16> {
        let (start, end) = (self.bda_word(0x80), self.bda_word(0x82));
        let tail = self.bda_word(0x1C);
        let mut head = self.bda_word(0x1A);
        let mut keys = Vec::new();
        while head != tail {
            keys.push(self.bda_word(head as usize));
            head += 2;
            if head >= end {
                head = start;
            }
        }
        self.rt.cpu.memory.write_word(0x041A, tail);
        keys
    }
}

/// Insert toggles the insert state and is queued as well, so a program
/// reading the keyboard through the BIOS sees the key.
#[test]
fn insert_key_is_queued() {
    let mut machine = Machine::booted();
    assert_eq!(machine.typed(&[0x52, 0xD2]), [0x5200]);
    assert_eq!(machine.bda_byte(0x17) & 0x80, 0x80, "insert state is on");

    // Auto-repeat while the key is held neither toggles nor queues again.
    assert_eq!(machine.typed(&[0x52, 0x52, 0x52, 0xD2]), [0x5200]);
    assert_eq!(machine.bda_byte(0x17) & 0x80, 0x00, "insert state is off");
}

/// Shift+Tab is queued as back-tab (0F00h), although the shifted table
/// holds 0 for Tab, the value that otherwise means "no key".
#[test]
fn shift_tab_is_queued_as_back_tab() {
    let mut machine = Machine::booted();
    assert_eq!(machine.typed(&[0x0F, 0x8F]), [0x0F09], "Tab");
    assert_eq!(
        machine.typed(&[0x2A, 0x0F, 0x8F, 0xAA]),
        [0x0F00],
        "left Shift+Tab"
    );
    assert_eq!(
        machine.typed(&[0x36, 0x0F, 0x8F, 0xB6]),
        [0x0F00],
        "right Shift+Tab"
    );
}

/// The cursor keys with Ctrl have scan codes of their own, so Ctrl+Left can
/// be told from Left (DOSKEY and other line editors move by words with it).
#[test]
fn ctrl_with_cursor_keys_has_its_own_scan_codes() {
    let mut machine = Machine::booted();
    let cases: [(u8, u16, &str); 10] = [
        (0x4B, 0x7300, "Ctrl+Left"),
        (0x4D, 0x7400, "Ctrl+Right"),
        (0x4F, 0x7500, "Ctrl+End"),
        (0x51, 0x7600, "Ctrl+PgDn"),
        (0x47, 0x7700, "Ctrl+Home"),
        (0x49, 0x8400, "Ctrl+PgUp"),
        (0x48, 0x8D00, "Ctrl+Up"),
        (0x50, 0x9100, "Ctrl+Down"),
        (0x53, 0x9300, "Ctrl+Del"),
        (0x4C, 0x8F00, "Ctrl+keypad 5"),
    ];
    for (scancode, key, name) in cases {
        let typed = machine.typed(&[0x1D, scancode, scancode | 0x80, 0x9D]);
        assert_eq!(typed, [key], "{name}");
    }
    assert_eq!(machine.typed(&[0x4B, 0xCB]), [0x4B00], "Left without Ctrl");
}

/// Keys with Ctrl or Alt, and the keypad keys, are queued with the codes of
/// the IBM tables: Ctrl with a key that is not a letter has a code of its
/// own or none, Alt with the digit row gives scan codes 78h-83h, Alt+Space
/// a space, Alt+Tab A500h, and the keypad - and + keys their characters.
#[test]
fn ctrl_alt_and_keypad_keys_follow_the_ibm_tables() {
    const NONE: u8 = 0;
    const CTRL: u8 = 0x1D;
    const ALT: u8 = 0x38;
    const SHIFT: u8 = 0x2A;
    let cases: &[(&str, u8, u8, &[u16])] = &[
        ("Ctrl+A", CTRL, 0x1E, &[0x1E01]),
        ("Ctrl+Esc", CTRL, 0x01, &[0x011B]),
        ("Ctrl+Backspace", CTRL, 0x0E, &[0x0E7F]),
        ("Ctrl+Enter", CTRL, 0x1C, &[0x1C0A]),
        ("Ctrl+2", CTRL, 0x03, &[0x0300]),
        ("Ctrl+6", CTRL, 0x07, &[0x071E]),
        ("Ctrl+-", CTRL, 0x0C, &[0x0C1F]),
        ("Ctrl+[", CTRL, 0x1A, &[0x1A1B]),
        ("Ctrl+]", CTRL, 0x1B, &[0x1B1D]),
        ("Ctrl+\\", CTRL, 0x2B, &[0x2B1C]),
        ("Ctrl+Space", CTRL, 0x39, &[0x3920]),
        ("Ctrl+PrtSc", CTRL, 0x37, &[0x7200]),
        ("Ctrl+F1", CTRL, 0x3B, &[0x5E00]),
        ("Ctrl+1", CTRL, 0x02, &[]),
        ("Ctrl+Tab", CTRL, 0x0F, &[]),
        ("Ctrl+;", CTRL, 0x27, &[]),
        ("Alt+A", ALT, 0x1E, &[0x1E00]),
        ("Alt+1", ALT, 0x02, &[0x7800]),
        ("Alt+0", ALT, 0x0B, &[0x8100]),
        ("Alt+=", ALT, 0x0D, &[0x8300]),
        ("Alt+Space", ALT, 0x39, &[0x3920]),
        ("Alt+Tab", ALT, 0x0F, &[0xA500]),
        ("Alt+Esc", ALT, 0x01, &[0x0100]),
        ("Alt+F1", ALT, 0x3B, &[0x6800]),
        ("Alt+Left", ALT, 0x4B, &[0x9B00]),
        ("Alt+keypad -", ALT, 0x4A, &[]),
        ("keypad -", NONE, 0x4A, &[0x4A2D]),
        ("keypad +", NONE, 0x4E, &[0x4E2B]),
        ("keypad 5", NONE, 0x4C, &[0x4C00]),
        ("keypad *", NONE, 0x37, &[0x372A]),
        ("F1", NONE, 0x3B, &[0x3B00]),
        ("Shift+F1", SHIFT, 0x3B, &[0x5400]),
        ("a", NONE, 0x1E, &[0x1E61]),
        ("Shift+a", SHIFT, 0x1E, &[0x1E41]),
        ("Shift+1", SHIFT, 0x02, &[0x0221]),
    ];
    let mut machine = Machine::booted();
    for &(name, modifier, scancode, expected) in cases {
        let typed = if modifier == NONE {
            machine.typed(&[scancode, scancode | 0x80])
        } else {
            machine.typed(&[modifier, scancode, scancode | 0x80, modifier | 0x80])
        };
        assert_eq!(typed, expected, "{name}");
    }

    // Caps Lock swaps the case of letters and leaves other keys alone.
    assert_eq!(
        machine.typed(&[0x3A, 0xBA, 0x1E, 0x9E]),
        [0x1E41],
        "Caps Lock, a"
    );
    assert_eq!(
        machine.typed(&[0x2A, 0x1E, 0x9E, 0xAA]),
        [0x1E61],
        "Caps Lock, Shift+a"
    );
    assert_eq!(machine.typed(&[0x02, 0x82]), [0x0231], "Caps Lock, 1");
}

/// Every printable ASCII character has one key on the US layout: each of
/// the 47 character keys queues one character alone and another with Shift,
/// and the space bar a space. The window sends key positions, so this
/// translation is all that stands between a key and its character.
#[test]
fn every_printable_character_has_its_key_on_the_us_layout() {
    // Scancode of the first key of each row, then the characters its keys
    // type without and with Shift.
    const ROWS: [(u8, &str, &str); 4] = [
        (0x02, "1234567890-=", "!@#$%^&*()_+"),
        (0x10, "qwertyuiop[]", "QWERTYUIOP{}"),
        (0x1E, "asdfghjkl;'`", "ASDFGHJKL:\"~"),
        (0x2B, "\\zxcvbnm,./", "|ZXCVBNM<>?"),
    ];
    const SHIFT: u8 = 0x2A;

    let mut machine = Machine::booted();
    let mut characters = vec![b' '];
    assert_eq!(machine.typed(&[0x39, 0xB9]), [0x3920], "space bar");
    for (first, plain, shifted) in ROWS {
        for (index, (plain, shifted)) in plain.bytes().zip(shifted.bytes()).enumerate() {
            let scancode = first + index as u8;
            let key = (scancode as u16) << 8;
            assert_eq!(
                machine.typed(&[scancode, scancode | 0x80]),
                [key | plain as u16],
                "{:?}",
                plain as char
            );
            assert_eq!(
                machine.typed(&[SHIFT, scancode, scancode | 0x80, SHIFT | 0x80]),
                [key | shifted as u16],
                "{:?}",
                shifted as char
            );
            characters.extend([plain, shifted]);
        }
    }

    characters.sort_unstable();
    assert_eq!(
        characters,
        (b' '..=b'~').collect::<Vec<u8>>(),
        "the keys type each printable character exactly once"
    );
}

/// Ctrl+Scroll Lock is the Break key, not a Scroll Lock toggle: the buffer
/// is flushed, bit 7 of 40:71 is set, INT 1Bh is called and a 0000h
/// keystroke is queued.
#[test]
fn ctrl_break_calls_int_1bh() {
    let mut machine = Machine::booted();
    machine.load_stub(&[
        0x31, 0xC0, // XOR AX,AX
        0x8E, 0xD8, // MOV DS,AX
        0xC7, 0x06, 0x6C, 0x00, 0x11, 0x00, // MOV WORD [006Ch],handler
        0x8C, 0x0E, 0x6E, 0x00, // MOV [006Eh],CS   (INT 1Bh -> handler)
        // idle:
        0xF4, // HLT
        0xEB, 0xFD, // JMP idle
        // handler:
        0x2E, 0xFE, 0x06, 0x80, 0x00, // INC BYTE [CS:0080h]   (count the calls)
        0xCF, // IRET
    ]);
    machine.rt.step_frame(); // the stub hooks INT 1Bh and idles
    machine.send_scancodes(&[0x1E, 0x9E]); // 'a', still unread when Break is pressed
    machine.send_scancodes(&[0x1D, 0x46, 0xC6, 0x9D]); // Ctrl+Scroll Lock
    for _ in 0..3 {
        machine.rt.step_frame();
    }
    let int_1b_calls = machine
        .rt
        .cpu
        .memory
        .read_byte(((STUB_SEG as usize) << 4) + 0x80);
    assert_eq!(int_1b_calls, 1, "INT 1Bh calls");
    assert_eq!(
        machine.take_keystrokes(),
        [0x0000],
        "buffer flushed, dummy key queued"
    );
    assert_eq!(machine.bda_byte(0x71), 0x80, "break flag");
    assert_eq!(
        machine.bda_byte(0x17) & 0x10,
        0,
        "Scroll Lock state untouched"
    );
}

/// Ctrl+Alt+Del restarts the machine instead of queuing a Del keystroke.
#[test]
fn ctrl_alt_del_restarts_the_machine() {
    // A boot sector that counts how often it was started and then idles with
    // interrupts enabled.
    let boot_code = [
        0xFE, 0x06, 0x02, 0x05, // INC BYTE [0502h]   (DS = 0 on entry)
        // idle:
        0xF4, // HLT
        0xEB, 0xFD, // JMP idle
    ];
    let mut sector = vec![0u8; 512];
    sector[..boot_code.len()].copy_from_slice(&boot_code);
    sector[510] = 0x55;
    sector[511] = 0xAA;
    let mut floppy = blank_floppy();
    floppy.write_sectors(0, 0, 1, 1, &sector).unwrap();

    let mut machine = Machine::boot(disks(vec![(0x00, floppy)]));
    assert_eq!(machine.rt.cpu.memory.read_byte(0x0502), 1, "booted once");

    machine.send_scancodes(&[0x1D, 0x38, 0x53]); // Ctrl, Alt, Del held down
    for _ in 0..10 {
        machine.rt.step_frame();
    }
    assert_eq!(machine.rt.cpu.memory.read_byte(0x0502), 2, "booted again");
    assert_eq!(machine.bda_byte(0x17), 0, "keyboard flags start afresh");
    assert!(machine.take_keystrokes().is_empty(), "nothing queued");
}

// ===========================================================================
// INT 10h AH=00h — which modes exist
// ===========================================================================

/// A mode the BIOS does not support is refused: AH=0Fh still reports mode 3
/// and the screen is untouched, so a program probing for a graphics or
/// 40-column mode learns that it does not have it.
#[test]
fn unsupported_video_modes_are_rejected() {
    for mode in [0x00u8, 0x01, 0x04, 0x06, 0x07, 0x0D, 0x12, 0x7F] {
        let mut machine = Machine::booted();
        machine.set_text_cell(0, 5, 5, 0x4E58);
        machine.run_stub(&[
            0xB4, 0x00, // MOV AH,00h
            0xB0, mode, // MOV AL,mode
            0xCD, 0x10, // INT 10h   (set video mode)
            0xB4, 0x0F, // MOV AH,0Fh
            0xCD, 0x10, // INT 10h   (get video mode)
        ]);
        let ax = machine.rt.cpu.registers.ax.word();
        assert_eq!(ax, 0x5003, "mode {mode:02X}h: still 80 columns, mode 3");
        assert_eq!(
            machine.bda_byte(0x49),
            0x03,
            "mode {mode:02X}h: BDA video mode"
        );
        assert_eq!(
            machine.text_cell(0, 5, 5),
            0x4E58,
            "mode {mode:02X}h: screen untouched"
        );
    }
}

#[test]
fn video_mode_2_is_80x25_text() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x02, 0x00, // MOV AX,0002h
        0xCD, 0x10, // INT 10h   (set video mode 2)
        0xB4, 0x0F, // MOV AH,0Fh
        0xCD, 0x10, // INT 10h   (get video mode)
    ]);
    assert_eq!(machine.rt.cpu.registers.ax.word(), 0x5002);
    assert_eq!(
        machine.indexed_register(0x3CE, 0x05) & 0x40,
        0,
        "not a 256-colour mode"
    );
}

/// Bit 7 of the mode number keeps the video memory and is not part of the
/// mode: 83h sets mode 3 and 93h mode 13h, and neither clears the memory.
#[test]
fn set_mode_with_bit_7_keeps_video_memory() {
    let mut machine = Machine::booted();
    machine.set_text_cell(0, 5, 5, 0x4E58);
    machine.run_stub(&[
        0xB8, 0x83, 0x00, // MOV AX,0083h
        0xCD, 0x10, // INT 10h   (mode 3, keep video memory)
    ]);
    assert_eq!(machine.bda_byte(0x49), 0x03, "video mode");
    assert_eq!(machine.text_cell(0, 5, 5), 0x4E58, "text memory kept");

    machine.rt.cpu.memory.write_byte(0xA0000 + 1234, 0x5A);
    machine.run_stub(&[
        0xB8, 0x93, 0x00, // MOV AX,0093h
        0xCD, 0x10, // INT 10h   (mode 13h, keep video memory)
    ]);
    assert_eq!(machine.bda_byte(0x49), 0x13, "video mode");
    assert_eq!(
        machine.indexed_register(0x3CE, 0x05) & 0x40,
        0x40,
        "256-colour mode"
    );
    assert_eq!(
        machine.rt.cpu.memory.read_byte(0xA0000 + 1234),
        0x5A,
        "frame buffer kept"
    );
}

// ===========================================================================
// INT 10h — other functions
// ===========================================================================

/// AH=0Fh returns the active page in BH and leaves BL, CX and DX as they
/// were.
#[test]
fn int10_get_mode_preserves_bl() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xBB, 0x55, 0x55, // MOV BX,5555h
        0xB9, 0x78, 0x56, // MOV CX,5678h
        0xBA, 0xBC, 0x9A, // MOV DX,9ABCh
        0xB4, 0x0F, // MOV AH,0Fh
        0xCD, 0x10, // INT 10h   (get video mode)
    ]);
    let registers = &machine.rt.cpu.registers;
    assert_eq!(registers.ax.word(), 0x5003, "80 columns, mode 3");
    assert_eq!(
        registers.bx.word(),
        0x0055,
        "BH = active page, BL untouched"
    );
    assert_eq!(registers.cx.word(), 0x5678, "CX");
    assert_eq!(registers.dx.word(), 0x9ABC, "DX");
}

/// The hardware cursor location counts from the start of video memory, so
/// it includes the start of the displayed page, not just row * 80 + column.
#[test]
fn cursor_location_includes_the_page_start() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x01, 0x05, // MOV AX,0501h
        0xCD, 0x10, // INT 10h   (show page 1)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x01, // MOV BH,01h
        0xBA, 0x03, 0x02, // MOV DX,0203h
        0xCD, 0x10, // INT 10h   (page 1 cursor to row 2, column 3)
    ]);
    assert_eq!(machine.display_start(), 2048, "page 1 starts 4096 bytes in");
    assert_eq!(machine.cursor_location(), 2048 + 2 * 80 + 3);
}

/// Selecting a page moves the hardware cursor to that page's cursor and
/// records the page's offset at 40:4E.
#[test]
fn selecting_a_page_shows_its_cursor() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x00, // MOV BH,00h
        0xBA, 0x09, 0x07, // MOV DX,0709h
        0xCD, 0x10, // INT 10h   (page 0 cursor to row 7, column 9)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x01, // MOV BH,01h
        0xBA, 0x10, 0x05, // MOV DX,0510h
        0xCD, 0x10, // INT 10h   (page 1 cursor to row 5, column 16)
        0xB8, 0x01, 0x05, // MOV AX,0501h
        0xCD, 0x10, // INT 10h   (show page 1)
    ]);
    assert_eq!(machine.bda_byte(0x62), 1, "active page");
    assert_eq!(machine.bda_word(0x4E), 0x1000, "offset of the active page");
    assert_eq!(machine.display_start(), 2048);
    assert_eq!(machine.cursor_location(), 2048 + 5 * 80 + 16);

    machine.run_stub(&[
        0xB8, 0x00, 0x05, // MOV AX,0500h
        0xCD, 0x10, // INT 10h   (show page 0)
    ]);
    assert_eq!(machine.bda_word(0x4E), 0, "offset of the active page");
    assert_eq!(machine.display_start(), 0);
    assert_eq!(
        machine.cursor_location(),
        7 * 80 + 9,
        "the cursor page 0 had"
    );
}

/// Page numbers above 7 are ignored: AH=02h writes nothing past the eight
/// cursor slots, and AH=05h does not select the page.
#[test]
fn out_of_range_pages_are_ignored() {
    let mut machine = Machine::booted();
    // Everything from the BDA up, except the timer fields INT 08h updates.
    let snapshot = |machine: &Machine| -> Vec<u8> {
        (0x0400..0x0700)
            .filter(|addr| !(0x046C..0x0471).contains(addr))
            .map(|addr| machine.rt.cpu.memory.read_byte(addr))
            .collect()
    };
    let before = snapshot(&machine);
    machine.run_stub(&[
        0xBA, 0x34, 0x12, // MOV DX,1234h
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x08, // MOV BH,08h
        0xCD, 0x10, // INT 10h   (set cursor, page 8)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x09, // MOV BH,09h
        0xCD, 0x10, // INT 10h   (page 9)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x58, // MOV BH,58h
        0xCD, 0x10, // INT 10h   (page 58h)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0xFF, // MOV BH,0FFh
        0xCD, 0x10, // INT 10h   (page FFh)
        0xB8, 0x09, 0x05, // MOV AX,0509h
        0xCD, 0x10, // INT 10h   (show page 9)
    ]);
    assert!(
        snapshot(&machine) == before,
        "memory outside the cursor table was written"
    );
    assert_eq!(machine.bda_byte(0x62), 0, "active page");
    assert_eq!(machine.display_start(), 0);
}

/// Teletype output goes to the active page, at that page's cursor, so it is
/// visible whichever page is displayed.
#[test]
fn teletype_writes_to_the_active_page() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x00, // MOV BH,00h
        0xBA, 0x0A, 0x0A, // MOV DX,0A0Ah
        0xCD, 0x10, // INT 10h   (page 0 cursor to row 10, column 10)
        0xB8, 0x01, 0x05, // MOV AX,0501h
        0xCD, 0x10, // INT 10h   (show page 1)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x01, // MOV BH,01h
        0xBA, 0x05, 0x03, // MOV DX,0305h
        0xCD, 0x10, // INT 10h   (page 1 cursor to row 3, column 5)
        0xB8, 0x5A, 0x0E, // MOV AX,0E5Ah
        0xBB, 0x07, 0x00, // MOV BX,0007h
        0xCD, 0x10, // INT 10h   (teletype 'Z')
    ]);
    assert_eq!(
        machine.text_cell(1, 3, 5) & 0x00FF,
        b'Z' as u16,
        "page 1, row 3, column 5"
    );
    assert_eq!(
        machine.text_cell(0, 10, 10),
        0x0720,
        "page 0 at its own cursor"
    );
    assert_eq!(machine.bda_word(0x52), 0x0306, "page 1 cursor advanced");
    assert_eq!(machine.bda_word(0x50), 0x0A0A, "page 0 cursor untouched");
    assert_eq!(machine.cursor_location(), 2048 + 3 * 80 + 6);
}

/// A teletype scroll moves the active page and no other.
#[test]
fn teletype_scrolls_the_active_page() {
    let mut machine = Machine::booted();
    machine.set_text_cell(1, 1, 0, 0x0741);
    machine.set_text_cell(0, 1, 0, 0x0742);
    machine.run_stub(&[
        0xB8, 0x01, 0x05, // MOV AX,0501h
        0xCD, 0x10, // INT 10h   (show page 1)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x01, // MOV BH,01h
        0xBA, 0x00, 0x18, // MOV DX,1800h
        0xCD, 0x10, // INT 10h   (page 1 cursor to the last row)
        0xB8, 0x0A, 0x0E, // MOV AX,0E0Ah
        0xCD, 0x10, // INT 10h   (teletype line feed)
    ]);
    assert_eq!(machine.text_cell(1, 0, 0), 0x0741, "page 1 moved up a line");
    assert_eq!(machine.text_cell(0, 1, 0), 0x0742, "page 0 did not move");
    assert_eq!(
        machine.bda_word(0x52),
        0x1800,
        "cursor stays on the last row"
    );
}

/// The line a teletype scroll brings in takes the attribute at the cursor,
/// not grey on black, so a screen cleared to another colour stays that
/// colour.
#[test]
fn teletype_scroll_keeps_the_screen_attribute() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x00, 0x06, // MOV AX,0600h
        0xB7, 0x1F, // MOV BH,1Fh
        0x31, 0xC9, // XOR CX,CX
        0xBA, 0x4F, 0x18, // MOV DX,184Fh
        0xCD, 0x10, // INT 10h   (clear the screen to white on blue)
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x00, // MOV BH,00h
        0xBA, 0x00, 0x18, // MOV DX,1800h
        0xCD, 0x10, // INT 10h   (cursor to the last row)
        0xB8, 0x0A, 0x0E, // MOV AX,0E0Ah
        0xCD, 0x10, // INT 10h   (teletype line feed)
    ]);
    assert_eq!(machine.text_cell(0, 23, 0), 0x1F20);
    assert_eq!(
        machine.text_cell(0, 24, 0),
        0x1F20,
        "first cell of the new line"
    );
    assert_eq!(
        machine.text_cell(0, 24, 79),
        0x1F20,
        "last cell of the new line"
    );
}

// ===========================================================================
// INT 10h AH=13h — write string
// ===========================================================================

/// AH=13h moves the cursor only with AL bit 0: without it the cursor stays
/// where it was before the call. AX is preserved, and an empty string moves
/// nothing.
#[test]
fn write_string_moves_the_cursor_only_when_asked() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xEB, 0x03, // JMP SHORT start
        // msg:
        0x41, 0x42, 0x43, // DB 'ABC'
        // start:
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x00, // MOV BH,00h
        0xBA, 0x0A, 0x0A, // MOV DX,0A0Ah
        0xCD, 0x10, // INT 10h   (cursor to row 10, column 10)
        0x0E, // PUSH CS
        0x07, // POP ES
        0xBD, 0x02, 0x00, // MOV BP,msg
        0xB8, 0x00, 0x13, // MOV AX,1300h   (write string, cursor does not move)
        0xBB, 0x4E, 0x00, // MOV BX,004Eh   (page 0, yellow on red)
        0xB9, 0x03, 0x00, // MOV CX,0003h
        0xBA, 0x03, 0x02, // MOV DX,0203h   (at row 2, column 3)
        0xCD, 0x10, // INT 10h
    ]);
    assert_eq!(machine.text_cell(0, 2, 3), 0x4E41);
    assert_eq!(machine.text_cell(0, 2, 4), 0x4E42);
    assert_eq!(machine.text_cell(0, 2, 5), 0x4E43);
    assert_eq!(
        machine.bda_word(0x50),
        0x0A0A,
        "cursor where it was before the call"
    );
    assert_eq!(machine.cursor_location(), 10 * 80 + 10);
    assert_eq!(machine.rt.cpu.registers.ax.word(), 0x1300, "AX preserved");

    machine.run_stub(&[
        0xEB, 0x03, // JMP SHORT start
        // msg:
        0x41, 0x42, 0x43, // DB 'ABC'
        // start:
        0x0E, // PUSH CS
        0x07, // POP ES
        0xBD, 0x02, 0x00, // MOV BP,msg
        0xB8, 0x01, 0x13, // MOV AX,1301h   (write string, cursor follows)
        0xBB, 0x4E, 0x00, // MOV BX,004Eh
        0xB9, 0x03, 0x00, // MOV CX,0003h
        0xBA, 0x03, 0x04, // MOV DX,0403h   (at row 4, column 3)
        0xCD, 0x10, // INT 10h
    ]);
    assert_eq!(machine.text_cell(0, 4, 3), 0x4E41);
    assert_eq!(machine.bda_word(0x50), 0x0406, "cursor after the string");

    machine.run_stub(&[
        0xEB, 0x03, // JMP SHORT start
        // msg:
        0x41, 0x42, 0x43, // DB 'ABC'
        // start:
        0x0E, // PUSH CS
        0x07, // POP ES
        0xBD, 0x02, 0x00, // MOV BP,msg
        0xB8, 0x01, 0x13, // MOV AX,1301h
        0xBB, 0x4E, 0x00, // MOV BX,004Eh
        0x31, 0xC9, // XOR CX,CX   (no characters)
        0xBA, 0x0C, 0x0C, // MOV DX,0C0Ch
        0xCD, 0x10, // INT 10h
    ]);
    assert_eq!(
        machine.bda_word(0x50),
        0x0406,
        "an empty string does nothing"
    );
}

/// AH=13h reads the string forward whatever the direction flag.
#[test]
fn write_string_reads_forward_whatever_the_direction_flag() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xEB, 0x03, // JMP SHORT start
        // msg:
        0x41, 0x42, 0x43, // DB 'ABC'
        // start:
        0x0E, // PUSH CS
        0x07, // POP ES
        0xBD, 0x02, 0x00, // MOV BP,msg
        0xB8, 0x01, 0x13, // MOV AX,1301h
        0xBB, 0x07, 0x00, // MOV BX,0007h
        0xB9, 0x03, 0x00, // MOV CX,0003h
        0xBA, 0x03, 0x02, // MOV DX,0203h
        0xFD, // STD
        0xCD, 0x10, // INT 10h   (write string with the direction flag set)
        0xFC, // CLD
    ]);
    assert_eq!(machine.text_cell(0, 2, 3), 0x0741);
    assert_eq!(machine.text_cell(0, 2, 4), 0x0742);
    assert_eq!(machine.text_cell(0, 2, 5), 0x0743);
}

/// AH=13h writes to the page in BH and moves only that page's cursor.
#[test]
fn write_string_writes_to_the_requested_page() {
    let mut machine = Machine::booted();
    let page_0_cursor = machine.bda_word(0x50);
    machine.run_stub(&[
        0xEB, 0x03, // JMP SHORT start
        // msg:
        0x41, 0x42, 0x43, // DB 'ABC'
        // start:
        0x0E, // PUSH CS
        0x07, // POP ES
        0xBD, 0x02, 0x00, // MOV BP,msg
        0xB8, 0x01, 0x13, // MOV AX,1301h
        0xBB, 0x4E, 0x01, // MOV BX,014Eh   (page 1)
        0xB9, 0x03, 0x00, // MOV CX,0003h
        0xBA, 0x03, 0x02, // MOV DX,0203h
        0xCD, 0x10, // INT 10h
    ]);
    assert_eq!(machine.text_cell(1, 2, 3), 0x4E41);
    assert_eq!(machine.text_cell(1, 2, 5), 0x4E43);
    assert_eq!(
        machine.text_cell(0, 2, 3) & 0x00FF,
        0x20,
        "page 0 untouched"
    );
    assert_eq!(
        machine.bda_word(0x52),
        0x0206,
        "page 1 cursor after the string"
    );
    assert_eq!(
        machine.bda_word(0x50),
        page_0_cursor,
        "page 0 cursor untouched"
    );
}

/// With AL bit 1 each character is followed by its attribute; CR, LF, BS and
/// BEL move the cursor and carry no attribute byte.
#[test]
fn write_string_with_attributes_and_control_characters() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xEB, 0x08, // JMP SHORT start
        // msg:
        0x41, 0x1F, 0x42, 0x2E, 0x0D, 0x0A, 0x43, 0x4F, // DB 'A',1Fh,'B',2Eh,0Dh,0Ah,'C',4Fh
        // start:
        0x0E, // PUSH CS
        0x07, // POP ES
        0xBD, 0x02, 0x00, // MOV BP,msg
        0xB8, 0x03, 0x13, // MOV AX,1303h   (character/attribute pairs, cursor follows)
        0xBB, 0x00, 0x00, // MOV BX,0000h
        0xB9, 0x05, 0x00, // MOV CX,0005h   (A, B, CR, LF, C)
        0xBA, 0x03, 0x02, // MOV DX,0203h
        0xCD, 0x10, // INT 10h
    ]);
    assert_eq!(machine.text_cell(0, 2, 3), 0x1F41, "A");
    assert_eq!(machine.text_cell(0, 2, 4), 0x2E42, "B");
    assert_eq!(machine.text_cell(0, 3, 0), 0x4F43, "C, after CR and LF");
    assert_eq!(machine.bda_word(0x50), 0x0301, "cursor after the string");
}

// ===========================================================================
// INT 10h — scroll windows and repeat counts
// ===========================================================================

impl Machine {
    /// Fills the 25 rows of text page `page` with `cell`.
    fn fill_text_page(&mut self, page: usize, cell: u16) {
        for row in 0..25 {
            for col in 0..80 {
                self.set_text_cell(page, row, col, cell);
            }
        }
    }

    /// Whether every cell in the 25 rows of text page `page` is `cell`.
    fn text_page_is(&self, page: usize, cell: u16) -> bool {
        (0..25).all(|row| (0..80).all(|col| self.text_cell(page, row, col) == cell))
    }
}

/// A scroll window whose top is below its bottom, or whose left edge is
/// right of its right edge, is empty: scrolling it changes no page and
/// leaves CX and DX as they were.
#[test]
fn scroll_with_an_inverted_window_does_nothing() {
    let mut machine = Machine::booted();
    machine.fill_text_page(0, 0x4E58);
    machine.fill_text_page(1, 0x4E58);
    machine.run_stub(&[
        0xB8, 0x00, 0x06, // MOV AX,0600h
        0xB7, 0x07, // MOV BH,07h
        0xB9, 0x00, 0x05, // MOV CX,0500h
        0xBA, 0x4F, 0x04, // MOV DX,044Fh
        0xCD, 0x10, // INT 10h   (clear rows 5 to 4)
        0xB8, 0x01, 0x07, // MOV AX,0701h
        0xCD, 0x10, // INT 10h   (scroll rows 5 to 4 down)
        0xB8, 0x01, 0x06, // MOV AX,0601h
        0xB9, 0x0A, 0x00, // MOV CX,000Ah
        0xBA, 0x09, 0x18, // MOV DX,1809h
        0xCD, 0x10, // INT 10h   (scroll columns 10 to 9 up)
    ]);
    assert!(machine.text_page_is(0, 0x4E58), "page 0 was written");
    assert!(machine.text_page_is(1, 0x4E58), "page 1 was written");
    let registers = &machine.rt.cpu.registers;
    assert_eq!(registers.cx.word(), 0x000A, "CX");
    assert_eq!(registers.dx.word(), 0x1809, "DX");
}

/// A scroll window reaching past the screen is clipped to it: DX = FFFFh
/// clears the whole page and nothing beyond it.
#[test]
fn scroll_window_is_clipped_to_the_screen() {
    let mut machine = Machine::booted();
    machine.fill_text_page(0, 0x4E58);
    machine.fill_text_page(1, 0x4E58);
    machine.set_text_cell(0, 25, 0, 0x4E58); // just past the last row
    machine.run_stub(&[
        0xB8, 0x00, 0x06, // MOV AX,0600h
        0xB7, 0x1F, // MOV BH,1Fh
        0x31, 0xC9, // XOR CX,CX
        0xBA, 0xFF, 0xFF, // MOV DX,0FFFFh
        0xCD, 0x10, // INT 10h   (clear from 0,0 to row 255, column 255)
    ]);
    assert!(
        machine.text_page_is(0, 0x1F20),
        "the whole screen is cleared"
    );
    assert_eq!(
        machine.text_cell(0, 25, 0),
        0x4E58,
        "nothing past the last row"
    );
    assert!(machine.text_page_is(1, 0x4E58), "page 1 was written");
    assert_eq!(machine.rt.cpu.registers.dx.word(), 0xFFFF, "DX");
}

/// AH=09h and AH=0Ah with a count of zero write nothing, rather than 65536
/// characters.
#[test]
fn write_char_with_a_zero_count_writes_nothing() {
    let mut machine = Machine::booted();
    machine.fill_text_page(0, 0x0741);
    machine.fill_text_page(1, 0x0741);
    machine.run_stub(&[
        0xB4, 0x02, // MOV AH,02h
        0xB7, 0x00, // MOV BH,00h
        0xBA, 0x0A, 0x0A, // MOV DX,0A0Ah
        0xCD, 0x10, // INT 10h   (cursor to row 10, column 10)
        0xB8, 0x78, 0x09, // MOV AX,0978h
        0xBB, 0x4E, 0x00, // MOV BX,004Eh
        0x31, 0xC9, // XOR CX,CX
        0xCD, 0x10, // INT 10h   (write character and attribute, zero times)
        0xB8, 0x78, 0x0A, // MOV AX,0A78h
        0xCD, 0x10, // INT 10h   (write character, zero times)
    ]);
    assert!(machine.text_page_is(0, 0x0741), "page 0 was written");
    assert!(machine.text_page_is(1, 0x0741), "page 1 was written");
}

// ===========================================================================
// INT 10h — DAC registers, blink select, pixels
// ===========================================================================

/// INT 10h AH=10h sets and reads the DAC registers, for programs that load
/// their palette through the BIOS rather than the DAC ports.
#[test]
fn dac_registers_can_be_set_and_read_through_the_bios() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xEB, 0x0F, // JMP SHORT start
        // table:
        0x0A, 0x0B, 0x0C, 0x14, 0x15, 0x16, 0x1E, 0x1F,
        0x20, // DB 10,11,12, 20,21,22, 30,31,32
        // buffer:
        0xEE, 0xEE, 0xEE, 0xEE, 0xEE, 0xEE, // DB 0EEh,0EEh,0EEh,0EEh,0EEh,0EEh
        // start:
        0xB8, 0x13, 0x00, // MOV AX,0013h
        0xCD, 0x10, // INT 10h   (320x200, 256 colours)
        0x0E, // PUSH CS
        0x07, // POP ES
        0xB8, 0x12, 0x10, // MOV AX,1012h
        0xBB, 0x64, 0x00, // MOV BX,0064h
        0xB9, 0x03, 0x00, // MOV CX,0003h
        0xBA, 0x02, 0x00, // MOV DX,table
        0xCD, 0x10, // INT 10h   (set DAC registers 100-102 from the table)
        0xB8, 0x10, 0x10, // MOV AX,1010h
        0xBB, 0xC8, 0x00, // MOV BX,00C8h
        0xB6, 0x01, // MOV DH,01h
        0xB9, 0x03, 0x02, // MOV CX,0203h
        0xCD, 0x10, // INT 10h   (set DAC register 200 to 1,2,3)
        0xB8, 0x17, 0x10, // MOV AX,1017h
        0xBB, 0x65, 0x00, // MOV BX,0065h
        0xB9, 0x02, 0x00, // MOV CX,0002h
        0xBA, 0x0B, 0x00, // MOV DX,buffer
        0xCD, 0x10, // INT 10h   (read DAC registers 101-102 into the buffer)
        0xB8, 0x15, 0x10, // MOV AX,1015h
        0xBB, 0x65, 0x00, // MOV BX,0065h
        0xB9, 0x66, 0x66, // MOV CX,6666h
        0xBA, 0x55, 0x55, // MOV DX,5555h
        0xCD, 0x10, // INT 10h   (read DAC register 101 into DH,CH,CL)
    ]);
    assert_eq!(
        machine.dac_register(100),
        [10, 11, 12],
        "AX=1012h, first register"
    );
    assert_eq!(
        machine.dac_register(102),
        [30, 31, 32],
        "AX=1012h, last register"
    );
    assert_eq!(machine.dac_register(200), [1, 2, 3], "AX=1010h");

    let registers = &machine.rt.cpu.registers;
    assert_eq!(registers.dx.word() >> 8, 20, "AX=1015h: red in DH");
    assert_eq!(
        registers.cx.word(),
        21 << 8 | 22,
        "AX=1015h: green in CH, blue in CL"
    );

    let buffer = ((STUB_SEG as usize) << 4) + 0x000B;
    let read_back: Vec<u8> = (0..6)
        .map(|i| machine.rt.cpu.memory.read_byte(buffer + i))
        .collect();
    assert_eq!(read_back, [20, 21, 22, 30, 31, 32], "AX=1017h");
}

/// AX=1003h switches text between blinking and sixteen background colours,
/// and a mode set goes back to blinking.
#[test]
fn blink_and_bright_backgrounds_can_be_selected() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (80x25 text)
        0xB8, 0x03, 0x10, // MOV AX,1003h
        0xB3, 0x00, // MOV BL,00h
        0xCD, 0x10, // INT 10h   (bright backgrounds instead of blinking)
    ]);
    assert_eq!(
        machine.attribute_register(0x10) & 0x08,
        0,
        "attribute mode control: no blink"
    );
    assert_eq!(
        machine.bda_byte(0x65) & 0x20,
        0,
        "copy of the mode control register"
    );

    machine.run_stub(&[
        0xB8, 0x03, 0x10, // MOV AX,1003h
        0xB3, 0x01, // MOV BL,01h
        0xCD, 0x10, // INT 10h   (blinking)
    ]);
    assert_eq!(
        machine.attribute_register(0x10) & 0x08,
        0x08,
        "attribute mode control: blink"
    );
    assert_eq!(
        machine.bda_byte(0x65) & 0x20,
        0x20,
        "copy of the mode control register"
    );

    // A mode set goes back to blinking.
    machine.run_stub(&[
        0xB8, 0x03, 0x10, // MOV AX,1003h
        0xB3, 0x00, // MOV BL,00h
        0xCD, 0x10, // INT 10h   (bright backgrounds)
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (80x25 text)
    ]);
    assert_eq!(machine.attribute_register(0x10), 0x0C);
    assert_eq!(machine.bda_byte(0x65), 0x29);
}

/// AH=0Ch and AH=0Dh write and read pixels in mode 13h; a pixel off the
/// screen is ignored.
#[test]
fn pixels_can_be_written_and_read_in_mode_13h() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xB8, 0x13, 0x00, // MOV AX,0013h
        0xCD, 0x10, // INT 10h   (320x200, 256 colours)
        0xB8, 0x0F, 0x0C, // MOV AX,0C0Fh
        0xB7, 0x00, // MOV BH,00h
        0xB9, 0x0A, 0x00, // MOV CX,000Ah
        0xBA, 0x05, 0x00, // MOV DX,0005h
        0xCD, 0x10, // INT 10h   (pixel at column 10, row 5 = colour 15)
        0xB8, 0x0F, 0x0C, // MOV AX,0C0Fh
        0xB9, 0x40, 0x01, // MOV CX,0140h
        0xCD, 0x10, // INT 10h   (column 320: ignored)
        0xB4, 0x0D, // MOV AH,0Dh
        0xB9, 0x0A, 0x00, // MOV CX,000Ah
        0xCD, 0x10, // INT 10h   (read the pixel at column 10, row 5)
    ]);
    let frame_buffer = |machine: &Machine, x: usize, y: usize| {
        machine.rt.cpu.memory.read_byte(0xA0000 + y * 320 + x)
    };
    assert_eq!(frame_buffer(&machine, 10, 5), 0x0F, "pixel written");
    assert_eq!(
        frame_buffer(&machine, 0, 6),
        0x00,
        "column 320 is off screen"
    );
    assert_eq!(
        machine.rt.cpu.registers.ax.word() & 0x00FF,
        0x0F,
        "pixel read back"
    );

    // In text mode there are no pixels: writes are ignored, reads return 0.
    machine
        .rt
        .cpu
        .memory
        .write_byte(0xA0000 + 5 * 320 + 10, 0x33);
    machine.run_stub(&[
        0xB8, 0x03, 0x00, // MOV AX,0003h
        0xCD, 0x10, // INT 10h   (80x25 text)
        0xB8, 0x0F, 0x0C, // MOV AX,0C0Fh
        0xB7, 0x00, // MOV BH,00h
        0xB9, 0x0A, 0x00, // MOV CX,000Ah
        0xBA, 0x05, 0x00, // MOV DX,0005h
        0xCD, 0x10, // INT 10h   (write pixel)
        0xB8, 0x55, 0x0D, // MOV AX,0D55h
        0xCD, 0x10, // INT 10h   (read pixel)
    ]);
    assert_eq!(frame_buffer(&machine, 10, 5), 0x33);
    assert_eq!(machine.rt.cpu.registers.ax.word() & 0x00FF, 0x00);
}

// ===========================================================================
// POST — video
// ===========================================================================

/// POST sets the video mode the way a mode set does: the adapter is
/// programmed, the CRTC cursor shape matches the BDA, and the BDA holds the
/// rows, the character height and the register copies.
#[test]
fn post_sets_the_video_mode() {
    let mut machine = Machine::booted();
    assert_eq!(machine.bda_byte(0x49), 0x03, "video mode");
    assert_eq!(machine.bda_word(0x4A), 80, "columns");
    assert_eq!(machine.bda_word(0x4C), 4096, "page size");
    assert_eq!(machine.bda_word(0x63), 0x03D4, "CRTC base port");
    assert_eq!(machine.bda_byte(0x84), 24, "rows - 1");
    assert_eq!(machine.bda_word(0x85), 16, "character height");
    assert_eq!(
        machine.bda_byte(0x65),
        0x29,
        "copy of the mode control register"
    );
    assert_eq!(
        machine.bda_byte(0x66),
        0x30,
        "copy of the colour select register"
    );
    assert_eq!(machine.bda_word(0x60), 0x0607, "cursor shape");
    assert_eq!(
        machine.indexed_register(0x3D4, 0x0A),
        0x06,
        "CRTC cursor start line"
    );
    assert_eq!(
        machine.indexed_register(0x3D4, 0x0B),
        0x07,
        "CRTC cursor end line"
    );
    assert_eq!(machine.attribute_register(0x06), 0x14, "text palette");
}

/// A restart through the reset vector puts the adapter back in mode 3 with
/// page 0 displayed, whatever state it was left in, so it agrees with the
/// BDA.
#[test]
fn restart_returns_the_adapter_to_text_mode() {
    let mut machine = Machine::booted();
    machine.load_stub(&[
        0xB8, 0x13, 0x00, // MOV AX,0013h
        0xCD, 0x10, // INT 10h   (320x200, 256 colours)
        0xBA, 0xD4, 0x03, // MOV DX,03D4h
        0xB0, 0x0C, // MOV AL,0Ch
        0xEE, // OUT DX,AL
        0x42, // INC DX
        0xB0, 0x08, // MOV AL,08h
        0xEE, // OUT DX,AL   (display start high = 08h)
        0xEA, 0x00, 0x00, 0xFF, 0xFF, // JMP 0FFFFh:0000h   (restart)
    ]);
    machine.run_until(|rt| rt.cpu.halted); // POST again, then the wait for a key

    assert_eq!(
        machine.indexed_register(0x3CE, 0x05) & 0x40,
        0,
        "256-colour mode is off"
    );
    assert_eq!(machine.display_start(), 0, "CRTC display start");
    assert_eq!(machine.bda_byte(0x49), 0x03, "video mode");
    assert_eq!(machine.bda_word(0x85), 16, "character height");
    assert!(machine.screen_text().contains("No bootable disk"));
}

// ===========================================================================
// POST — equipment word
// ===========================================================================

/// The number of diskette drives in the equipment word (INT 11h bits 7-6,
/// with bit 0 saying there are any) is what INT 13h AH=15h reports for
/// units 0-3. DOS treats B: as a second name for A: when the word says one
/// drive, which would leave a second diskette image out of reach.
#[test]
fn equipment_word_counts_the_diskette_drives() {
    for (units, expected) in [(2u8, 0x0041u16), (3, 0x0081), (4, 0x00C1)] {
        let drives = (0..units).map(|unit| (unit, blank_floppy())).collect();
        let mut machine = Machine::boot(disks(drives));
        machine.run_stub(&[
            0xCD, 0x11, // INT 11h   (equipment list)
        ]);
        let equipment = machine.rt.cpu.registers.ax.word();
        assert_eq!(equipment & 0x00C1, expected, "{units} diskette drives");
        assert_eq!(
            equipment & 0x0030,
            0x0020,
            "initial video mode: 80x25 colour"
        );
        assert_eq!(
            machine.bda_word(0x10),
            equipment,
            "INT 11h returns the BDA word"
        );
    }
}

// ===========================================================================
// INT 19h — what counts as a boot sector
// ===========================================================================

/// The test boot sector with its last two bytes replaced.
fn boot_sector_ending(last_two: [u8; 2]) -> Vec<u8> {
    let mut sector = boot_sector();
    sector[510..].copy_from_slice(&last_two);
    sector
}

fn floppy_with_first_sector(sector: &[u8]) -> DiskImage {
    let mut image = blank_floppy();
    image.write_sectors(0, 0, 1, 1, sector).unwrap();
    image
}

/// A diskette boots without the 55AAh signature: diskettes older than the
/// convention, and the boot sectors of some games, do not end in it.
#[test]
fn diskette_boots_without_the_55aa_signature() {
    let floppy = floppy_with_first_sector(&boot_sector_ending([0x00, 0x00]));
    let machine = Machine::boot(disks(vec![(0x00, floppy)]));
    assert_eq!(machine.booted_drive(), Some(0x00));
}

/// A diskette whose first sector cannot be code - all zeros, or one fill
/// byte repeated as on a formatted but empty disk - is passed over, with or
/// without the signature at its end, and the next device boots.
#[test]
fn blank_diskettes_are_skipped() {
    for fill in [0x00u8, 0xF6, 0xE5] {
        for last_two in [[fill, fill], [0x55, 0xAA]] {
            let mut sector = vec![fill; 512];
            sector[510..].copy_from_slice(&last_two);
            let machine = Machine::boot(disks(vec![
                (0x00, floppy_with_first_sector(&sector)),
                (0x80, bootable_hard_disk()),
            ]));
            assert_eq!(
                machine.booted_drive(),
                Some(0x80),
                "diskette filled with {fill:02X}h, ending in {last_two:02X?}"
            );
        }
    }
}

/// Hard disks keep the rule that the sector must end in 55AAh.
#[test]
fn hard_disk_without_the_55aa_signature_is_not_booted() {
    let mut disk = DiskImage::new_in_memory_hard_disk(1);
    disk.write_sectors(0, 0, 1, 1, &boot_sector_ending([0x00, 0x00]))
        .unwrap();
    let machine = Machine::boot(disks(vec![(0x80, disk)]));
    assert_eq!(machine.booted_drive(), None);
    assert!(machine.screen_text().contains("No bootable disk"));
}

// ===========================================================================
// INT 16h — keyboard services
// ===========================================================================

/// INT 16h enables interrupts on entry, as the IBM BIOS does, and AH=01h
/// returns with them on: a program polling AH=01h with interrupts off still
/// lets IRQ1 in and sees the key.
#[test]
fn int16_status_lets_the_keyboard_in_when_called_with_interrupts_off() {
    let mut machine = Machine::booted();
    machine.send_scancodes(&[0x1E, 0x9E]); // 'a'
    machine.run_stub(&[
        0xFA, // CLI
        0xB9, 0xD0, 0x07, // MOV CX,07D0h   (up to 2000 polls)
        // poll:
        0xB4, 0x01, // MOV AH,01h
        0xCD, 0x16, // INT 16h   (keystroke waiting?)
        0x75, 0x06, // JNZ got
        0xE2, 0xF8, // LOOP poll
        0x31, 0xDB, // XOR BX,BX   (never saw one)
        0xEB, 0x02, // JMP SHORT done
        // got:
        0x89, 0xC3, // MOV BX,AX   (the keystroke)
        // done:
        0x9C, // PUSHF
        0x5A, // POP DX   (flags INT 16h returned with)
    ]);
    let registers = &machine.rt.cpu.registers;
    assert_eq!(registers.bx.word(), 0x1E61, "the keystroke AH=01h reported");
    assert_ne!(
        registers.dx.word() & FLAG_INTERRUPT,
        0,
        "AH=01h returns with IF set"
    );
    assert_eq!(
        machine.take_keystrokes(),
        [0x1E61],
        "AH=01h leaves the key in the buffer"
    );
}

/// AH=12h returns in AH the extended shift flags, whose layout is not that
/// of the byte at 40:18: bits 0 and 1 are left Ctrl and left Alt (which this
/// BIOS never sets at 40:18), and bit 7 is SysReq, not "Insert held".
#[test]
fn int16_extended_shift_flags() {
    let mut machine = Machine::booted();
    machine.typed(&[0x1D, 0x38, 0x52]); // Ctrl, Alt and Insert held down
    machine.run_stub(&[
        0xB4, 0x02, // MOV AH,02h
        0xCD, 0x16, // INT 16h   (shift flags)
        0x88, 0xC3, // MOV BL,AL
        0xB4, 0x12, // MOV AH,12h
        0xCD, 0x16, // INT 16h   (extended shift flags)
    ]);
    let registers = &machine.rt.cpu.registers;
    assert_eq!(
        registers.bx.word() & 0x00FF,
        0x8C,
        "AH=02h: Insert on, Alt, Ctrl"
    );
    assert_eq!(
        registers.ax.word(),
        0x038C,
        "AH=12h: AL as AH=02h, AH = left Ctrl + left Alt"
    );
}

// ===========================================================================
// INT 15h — system services
// ===========================================================================

/// A function the BIOS does not implement returns CF set and AH=86h, as the
/// IBM BIOS does. The keyboard intercept (AH=4Fh) is the one call that must
/// hand AX back untouched, with CF set, so that INT 09h goes on to process
/// the scancode.
#[test]
fn int15_unsupported_functions_return_ah_86h() {
    let mut machine = Machine::booted();
    machine.run_stub(&[
        0xBB, 0x34, 0x12, // MOV BX,1234h
        0xB8, 0xA5, 0xC0, // MOV AX,0C0A5h
        0xCD, 0x15, // INT 15h   (AH=C0h: not supported)
        0x9C, // PUSHF
        0x59, // POP CX
        0x89, 0xC6, // MOV SI,AX
        0xB8, 0x1E, 0x4F, // MOV AX,4F1Eh
        0xF9, // STC
        0xCD, 0x15, // INT 15h   (AH=4Fh: keyboard intercept)
        0x9C, // PUSHF
        0x5A, // POP DX
        0x89, 0xC7, // MOV DI,AX
        0xB8, 0xA5, 0x88, // MOV AX,88A5h
        0xF9, // STC
        0xCD, 0x15, // INT 15h   (AH=88h: extended memory size)
    ]);
    let cpu = &machine.rt.cpu;
    let registers = &cpu.registers;
    assert_eq!(
        registers.si.word(),
        0x86A5,
        "unsupported: AH=86h, AL untouched"
    );
    assert_ne!(registers.cx.word() & FLAG_CARRY, 0, "unsupported: CF set");
    assert_eq!(
        registers.di.word(),
        0x4F1E,
        "keyboard intercept: AX untouched"
    );
    assert_ne!(
        registers.dx.word() & FLAG_CARRY,
        0,
        "keyboard intercept: CF set"
    );
    assert_eq!(registers.ax.word(), 0x0000, "no extended memory");
    assert!(!cpu.check_flag(CpuFlag::Carry), "AH=88h: CF clear");
    assert_eq!(registers.bx.word(), 0x1234, "BX is not touched");
}

// ===========================================================================
// INT 08h — timer tick
// ===========================================================================

/// INT 08h calls the user hook INT 1Ch before it sends the end-of-interrupt,
/// as the IBM BIOS does: IRQ0 is still in service, so the interrupt
/// controller holds the next tick back until the hook has returned, and a
/// hook that enables interrupts and runs longer than a tick period is not
/// entered again on top of itself.
#[test]
fn timer_hook_is_not_re_entered_by_the_next_tick() {
    // What the hook records, in the stub's segment.
    const DATA: usize = ((STUB_SEG as usize) << 4) + 0x0200;

    let mut machine = Machine::booted();
    machine.rt.use_manual_clock();
    for addr in DATA..DATA + 6 {
        machine.rt.cpu.memory.write_byte(addr, 0);
    }
    machine.load_stub(&[
        0x31, 0xC0, // XOR AX,AX
        0x8E, 0xC0, // MOV ES,AX
        0xFA, // CLI
        0x26, 0xC7, 0x06, 0x70, 0x00, 0x21,
        0x00, // MOV WORD [ES:0070h],hook   (INT 1Ch vector)
        0x26, 0x8C, 0x0E, 0x72, 0x00, // MOV [ES:0072h],CS
        0xB0, 0x36, // MOV AL,36h
        0xE6, 0x43, // OUT 43h,AL   (timer channel 0: mode 3)
        0xB0, 0xE8, // MOV AL,0E8h
        0xE6, 0x40, // OUT 40h,AL
        0xB0, 0x03, // MOV AL,03h
        0xE6, 0x40, // OUT 40h,AL   (one tick every 1000 clocks)
        0xFB, // STI
        // idle:
        0xF4, // HLT
        0xEB, 0xFD, // JMP idle
        // hook:
        0x50, // PUSH AX
        0x51, // PUSH CX
        0xB0, 0x0B, // MOV AL,0Bh
        0xE6, 0x20, // OUT 20h,AL   (next read of port 20h: in-service register)
        0xE4, 0x20, // IN AL,20h
        0x2E, 0xA2, 0x02, 0x02, // MOV [CS:0202h],AL
        0xB0, 0x0A, // MOV AL,0Ah
        0xE6, 0x20, // OUT 20h,AL   (back to the request register)
        0x2E, 0xFE, 0x06, 0x00, 0x02, // INC BYTE [CS:0200h]   (hooks running)
        0x2E, 0xA0, 0x00, 0x02, // MOV AL,[CS:0200h]
        0x2E, 0x3A, 0x06, 0x01, 0x02, // CMP AL,[CS:0201h]
        0x76, 0x04, // JBE spin
        0x2E, 0xA2, 0x01, 0x02, // MOV [CS:0201h],AL   (the most that ever ran at once)
        // spin:
        0xFB, // STI
        0xB9, 0xD0, 0x07, // MOV CX,2000
        // delay:
        0xE2, 0xFE, // LOOP delay   (several tick periods)
        0x2E, 0xFE, 0x0E, 0x00, 0x02, // DEC BYTE [CS:0200h]
        0x2E, 0xFF, 0x06, 0x04, 0x02, // INC WORD [CS:0204h]   (hooks completed)
        0x59, // POP CX
        0x58, // POP AX
        0xCF, // IRET
    ]);
    machine.rt.step_frame(); // installs the hook and starts the timer; no time passes
    machine.feed_time(1, 20_000_000); // 23 tick periods
    machine.run_until(|rt| rt.cpu.halted); // back in the idle loop

    let memory = &machine.rt.cpu.memory;
    let running = memory.read_byte(DATA);
    let most_at_once = memory.read_byte(DATA + 1);
    let in_service = memory.read_byte(DATA + 2);
    let completed = memory.read_word(DATA + 4);
    assert_eq!(running, 0, "every hook has returned");
    assert!(
        completed >= 2,
        "the hook keeps being called: {completed} calls"
    );
    assert_eq!(
        (in_service & 0x01, most_at_once),
        (0x01, 1),
        "(IRQ0 in service while the hook runs, hooks running at once)"
    );
}
