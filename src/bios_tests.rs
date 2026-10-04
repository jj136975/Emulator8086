//! Integration tests that run the real BIOS ROM on a fully wired machine.
//!
//! Each test powers the machine on, lets the BIOS get as far as it can, and
//! then either inspects the result or runs a small guest stub that calls a
//! BIOS service.

use std::sync::mpsc::Sender;

use crate::io::disk::{DiskController, DiskImage, FLOPPY_144_SIZE};
use crate::io::keyboard::EmulatorEvent;
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
    /// Powers on with the given disks and runs until the CPU halts: either a
    /// boot sector parked it, or the BIOS is waiting for a key.
    fn boot(disks: DiskController) -> Self {
        let (mut rt, keys) = Runtime::new_test_machine(disks);
        rt.load_rom_image(BIOS_BIN);
        let mut machine = Self { rt, keys };
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

    /// Runs `code` at STUB_SEG:0000 on its own stack, with interrupts
    /// enabled, until it falls into the `CLI; HLT` appended here.
    fn run_stub(&mut self, code: &[u8]) {
        let base = (STUB_SEG as usize) << 4;
        for (i, byte) in code.iter().chain(&[0xFA, 0xF4]).enumerate() {
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
        self.run_until(Self::parked);
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
                text.push(if (0x20..0x7F).contains(&ch) { ch as char } else { ' ' });
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
        0x88, 0x16, 0x00, 0x05, // MOV [0x0500],DL
        0xC6, 0x06, 0x01, 0x05, BOOT_MARKER, // MOV BYTE [0x0501],marker
        0xFA, // CLI
        0xF4, // HLT
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

/// Regression: the boot order given on the command line never reached the
/// BIOS, which always tried floppy A: first.
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

/// Regression: the retry counter shared a register with the sector number,
/// so the failure path was unreachable and the BIOS scanned ROM bytes as
/// drive numbers forever.
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

// ---------------------------------------------------------------------------
// INT 13h — disk
// ---------------------------------------------------------------------------

/// Regression: the handler returned through `retf 2` without re-enabling
/// interrupts, so every disk call left the caller with IF=0.
#[test]
fn int13_returns_with_interrupts_enabled() {
    let mut machine = Machine::boot(disks(vec![]));
    machine.run_stub(&[
        0xB4, 0x08, // MOV AH,08h   (get drive parameters)
        0xB2, 0x00, // MOV DL,00h   (no such drive)
        0xCD, 0x13, // INT 13h
        0x9C, // PUSHF
        0x5B, // POP BX
    ]);
    let flags = machine.rt.cpu.registers.bx.word();
    assert_ne!(flags & FLAG_INTERRUPT, 0, "IF must be set after INT 13h");
    assert_ne!(flags & FLAG_CARRY, 0, "CF from the disk trap must reach the caller");
}

// ---------------------------------------------------------------------------
// INT 10h — video
// ---------------------------------------------------------------------------

/// Regression: AH=00h used BX as scratch without saving it.
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
