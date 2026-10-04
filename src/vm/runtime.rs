use crate::io::bus::IoBus;
use crate::io::console::DebugConsole;
use crate::io::disk::{
    DiskController, DiskImage, DiskSource, DiskSpec, BOOT_ORDER_PORT, DISK_TRAP_PORT,
    HD_DEFAULT_SIZE_MB,
};
use crate::io::dma::{DmaController, DmaPageRegisters};
use crate::io::fdc::FdcStub;
use crate::io::hdc::HdcStub;
use crate::io::keyboard::KeyboardController;
use crate::io::pic::{Pic, PicDevice};
use crate::io::vga::VgaDevice;
use crate::vm::cli::enter_monitor;
use crate::vm::cpu::{Cpu, CpuType};
use crate::vm::instructions::process;
use crate::vm::memory::{Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::CpuFlag::{Carry, Interrupt, Overflow, Sign, Trap, Zero};
use log::debug;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::io::Write;
use std::ops::DerefMut;
use std::path::PathBuf;
use std::rc::Rc;
use std::time::Instant;
use crate::io::pit::Pit;

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum CpuFlag {
    Carry = 0,
    Parity = 2,
    AuxCarry = 4,
    Zero = 6,
    Sign = 7,
    Trap = 8,
    Interrupt = 9,
    Directional = 10,
    Overflow = 11,
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum SegmentType {
    ES = 0b_00,
    CS = 0b_01,
    SS = 0b_10,
    DS = 0b_11,
}

impl From<u8> for SegmentType {
    fn from(value: u8) -> Self {
        match value {
            0b_00 => SegmentType::ES,
            0b_01 => SegmentType::CS,
            0b_10 => SegmentType::SS,
            0b_11 => SegmentType::DS,
            _ => panic!("Unknown segment: {}", value),
        }
    }
}

/// Prefixes latched for the instruction currently being decoded.
///
/// Every prefix byte is consumed by its own `process` call, which latches it
/// here and sets `pending` so the next call knows it continues the same
/// instruction. The latches are independent, as on the real CPU, so prefixes
/// combine in any order. LOCK has no effect on a single-CPU machine and is
/// only tracked through `pending`.
#[derive(Clone, Copy, Default)]
pub struct Prefixes {
    /// REP/REPE/REPZ (`Some(true)`, opcode F3) or REPNE/REPNZ (`Some(false)`, opcode F2).
    pub rep: Option<bool>,
    /// Segment override for the instruction's memory operand.
    pub segment: Option<SegmentType>,
    /// The previous `process` call consumed a prefix byte.
    pub pending: bool,
}

/// Instructions executed per `step_frame` call.
const RENDER_INTERVAL: u64 = 50_000;

/// Input clock of the 8253 timer, in Hz.
const PIT_CLOCK_HZ: u64 = 1_193_182;
const NANOS_PER_SEC: u64 = 1_000_000_000;
/// The host clock is read once per this many instructions (and whenever the
/// CPU is halted); reading it for every instruction would dominate run time.
const CLOCK_SAMPLE_INTERVAL: u64 = 256;
/// Timer clocks handed to the 8253 per executed instruction while it is
/// behind: few enough that the guest gets to service every timer interrupt,
/// enough to catch up within a few thousand instructions.
const PIT_CLOCKS_PER_INSTRUCTION: u64 = 4;
/// Most timer clocks the guest can be owed (250 ms worth). After a longer
/// stall the excess is dropped rather than replayed as a burst of ticks.
const PIT_MAX_DEBT: u64 = PIT_CLOCK_HZ / 4;

pub struct Runtime {
    pub(crate) cpu: Cpu,
    running: bool,
    pub prefixes: Prefixes,
    pub instruction_count: u64,
    pub trace: bool,
    pub interrupt_inhibit: bool,

    // Devices
    io_bus: IoBus,
    pub disks: Rc<RefCell<DiskController>>,
    keyboard: Option<Rc<RefCell<KeyboardController>>>,
    pub vga: Option<Rc<RefCell<VgaDevice>>>,
    pit: Rc<RefCell<Pit>>,

    // Timing
    /// Host time of the last clock sample.
    last_clock_sample: Instant,
    /// Timer clocks owed to the 8253 for host time that has already passed.
    pit_debt: u64,
    /// Remainder of the time-to-clocks conversion, in units of 1e-9 clock.
    pit_frac: u64,
    /// Timer time is fed in by hand (tests) instead of from the host clock.
    manual_clock: bool,
}

impl Runtime {
    #[cfg(test)]
    pub fn new_test() -> Self {
        let mut memory = Box::new(Memory::new());

        let registers = Registers::new(memory.deref_mut());
        Self {
            cpu: Cpu {
                cpu_type: CpuType::Intel8086,
                registers,
                memory,
                flags: 0,
                halted: false,
                pic: Pic::new(),
            },
            running: true,
            prefixes: Prefixes::default(),
            io_bus: IoBus::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,

            disks: Rc::new(RefCell::new(DiskController::new())),
            keyboard: None,
            vga: None,
            pit: Rc::new(RefCell::new(Pit::new())),
            last_clock_sample: Instant::now(),
            pit_debt: 0,
            pit_frac: 0,
            manual_clock: false,
        }
    }

    pub fn new(
        disk_specs: Vec<DiskSpec>,
        hd_specs: Vec<String>,
        boot_order: Option<Vec<u8>>,
        trace: bool,
    ) -> Self {
        let mut disk_ctrl = DiskController::new();

        for spec in disk_specs {
            let image = match &spec.source {
                DiskSource::FilePath(path) => {
                    if spec.drive == 0 {
                        // Boot drive: must exist
                        DiskImage::open(path, spec.readonly).expect("Failed to open disk image")
                    } else {
                        // Non-boot drive: create if it doesn't exist
                        DiskImage::open_or_create(path, spec.readonly)
                            .expect("Failed to open/create disk image")
                    }
                }
                DiskSource::Memory(size) => {
                    let mut img = DiskImage::new_in_memory_sized(*size, spec.readonly);
                    if let Err(e) = img.format_fat() {
                        eprintln!("Warning: could not format floppy: {}", e);
                    }
                    img
                }
            };
            disk_ctrl.attach(spec.drive, image);
        }

        // Build hard disks vec
        for (id, spec) in hd_specs.into_iter().enumerate() {
            let image = if spec.eq_ignore_ascii_case("memory") {
                DiskImage::new_in_memory_hard_disk(HD_DEFAULT_SIZE_MB)
            } else {
                let path = PathBuf::from(spec);
                DiskImage::open_or_create_hard_disk(&path, HD_DEFAULT_SIZE_MB)
                    .expect("Failed to open/create hard disk image")
            };
            disk_ctrl.attach(0x80 + id as u8, image);
        }

        if let Some(order) = boot_order {
            disk_ctrl.set_boot_order(order);
        }

        let mut vm = Self::with_devices(disk_ctrl, KeyboardController::new());
        vm.trace = trace;
        vm
    }

    /// Builds a machine with the full device set wired to the port bus.
    fn with_devices(disk_ctrl: DiskController, keyboard: KeyboardController) -> Self {
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

        let mut io_bus = IoBus::new();
        let keyboard = Rc::new(RefCell::new(keyboard));
        let vga = Rc::new(RefCell::new(VgaDevice::new()));
        let disk_ctrl = Rc::new(RefCell::new(disk_ctrl));
        let pit = Rc::new(RefCell::new(Pit::new()));

        keyboard.borrow_mut().set_pit(Rc::clone(&pit));

        io_bus.register(0x00, 0x0F, Box::new(DmaController::new()));
        io_bus.register(0x20, 0x21, Box::new(PicDevice));
        io_bus.register(0x40, 0x43, Box::new(Rc::clone(&pit)));
        io_bus.register(0x60, 0x63, Box::new(Rc::clone(&keyboard)));
        io_bus.register(0x81, 0x87, Box::new(DmaPageRegisters::new()));
        io_bus.register(DISK_TRAP_PORT, BOOT_ORDER_PORT, Box::new(Rc::clone(&disk_ctrl)));
        io_bus.register(0xE9, 0xE9, Box::new(DebugConsole));
        io_bus.register(0x320, 0x323, Box::new(HdcStub::new()));
        io_bus.register(0x3C0, 0x3DA, Box::new(Rc::clone(&vga)));
        io_bus.register(0x3F0, 0x3F7, Box::new(FdcStub::new()));

        Self {
            cpu: Cpu {
                cpu_type: CpuType::Intel8086,
                registers,
                memory,
                flags: 0xF002, // 8086 reset state: interrupts disabled
                halted: false,
                pic: Pic::new(),
            },
            running: true,
            prefixes: Prefixes::default(),
            io_bus,
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,

            disks: disk_ctrl,
            keyboard: Some(keyboard),
            vga: Some(vga),
            pit,
            last_clock_sample: Instant::now(),
            pit_debt: 0,
            pit_frac: 0,
            manual_clock: false,
        }
    }

    /// A fully wired machine for integration tests. The keyboard is fed
    /// through the returned channel instead of a terminal thread.
    #[cfg(test)]
    pub fn new_test_machine(
        disk_ctrl: DiskController,
    ) -> (Self, std::sync::mpsc::Sender<crate::io::keyboard::EmulatorEvent>) {
        let (tx, rx) = std::sync::mpsc::channel();
        let vm = Self::with_devices(disk_ctrl, KeyboardController::with_receiver(rx));
        (vm, tx)
    }

    pub(crate) fn load_rom(&mut self, path: PathBuf) {
        let rom_data = std::fs::read(path).expect("Failed to read ROM file");
        self.load_rom_image(&rom_data);
    }

    /// Copies a ROM image to F000:0000 and write-protects the region.
    pub(crate) fn load_rom_image(&mut self, rom_data: &[u8]) {
        if rom_data.len() > 0x10000 {
            panic!("ROM image too large (max 64KB)");
        }

        let load_address = 0xF0000;
        Memory::copy_data(&mut self.cpu.memory, load_address, rom_data);
        self.cpu.memory.enable_rom_protection();
    }

    /// Execute one batch of CPU instructions, tick PIT, poll keyboard, and
    /// handle interrupts. Returns `false` when the emulator should stop.
    /// Called from the terminal `run()` loop and the GUI event loop.
    pub fn step_frame(&mut self) -> bool {
        // Host time that went by between two calls (rendering, vsync,
        // sleeping) is credited on the first pass.
        let mut sample_clock = true;

        for _ in 0..RENDER_INTERVAL {
            if !self.running {
                return false;
            }

            if self.trace {
                debug!("{:#?}", self);
            }

            // The trap flag is sampled before the instruction runs: the
            // instruction after a POPF or IRET that sets TF still executes,
            // and only then does the single-step interrupt fire.
            let mut single_step = false;
            if !self.cpu.halted {
                single_step = self.cpu.check_flag(Trap);
                process(self);
                self.instruction_count += 1;
            }

            // Timer
            if sample_clock
                || self.cpu.halted
                || self.instruction_count % CLOCK_SAMPLE_INTERVAL == 0
            {
                sample_clock = false;
                self.sample_host_clock();
            }
            if self.pit_debt > 0 {
                self.run_pit();
            }

            // Keyboard polling
            if self.instruction_count % 1000 == 0 || self.cpu.halted {
                if let Some(kb) = &self.keyboard {
                    match kb.borrow_mut().poll(&mut self.cpu.pic) {
                        Some(crate::io::keyboard::EmulatorEvent::Quit) => {
                            self.running = false;
                            return false;
                        }
                        Some(crate::io::keyboard::EmulatorEvent::EnterCLI) => {
                            enter_monitor(&mut self.disks.borrow_mut());
                            if let Some(vga) = &self.vga {
                                vga.borrow_mut().queue_full_redraw();
                            }
                            self.last_clock_sample = Instant::now();
                        }
                        Some(crate::io::keyboard::EmulatorEvent::DumpVga) => {
                            if let Some(vga) = &self.vga {
                                vga.borrow().dump(&self.cpu.memory, "vga_dump.txt");
                            }
                        }
                        _ => {}
                    }
                }
            }

            // Interrupt handling. Interrupts are not recognised between a
            // prefix byte and the instruction it belongs to, nor at the
            // boundary right after STI or a load of SS.
            if self.interrupt_inhibit || self.prefixes.pending {
                self.interrupt_inhibit = false;
                continue;
            }
            if self.cpu.check_flag(Interrupt) && self.cpu.pic.has_interrupt() {
                let vector = self.cpu.pic.acknowledge();
                self.cpu.halted = false;
                self.handle_interrupt(vector);
            }
            // Single-step has the lowest priority: if an IRQ was taken at this
            // boundary too, the trap fires on entry to its handler.
            if single_step {
                self.cpu.halted = false;
                self.handle_interrupt(1);
            }
            if self.cpu.halted {
                break; // nothing to run until an interrupt arrives
            }
        }

        self.running
    }

    /// Credits the timer with the host time elapsed since the last sample.
    fn sample_host_clock(&mut self) {
        if self.manual_clock {
            return;
        }
        let now = Instant::now();
        let elapsed = now.duration_since(self.last_clock_sample);
        self.last_clock_sample = now;
        self.credit_pit_time(elapsed.as_nanos().min(NANOS_PER_SEC as u128) as u64);
    }

    /// Converts `nanos` of elapsed time (at most one second per call) into
    /// timer clocks owed to the 8253. The sub-clock remainder is carried
    /// over, so no time is lost to rounding however often this is called.
    pub(crate) fn credit_pit_time(&mut self, nanos: u64) {
        self.pit_frac += nanos.min(NANOS_PER_SEC) * PIT_CLOCK_HZ;
        self.pit_debt = (self.pit_debt + self.pit_frac / NANOS_PER_SEC).min(PIT_MAX_DEBT);
        self.pit_frac %= NANOS_PER_SEC;
    }

    /// Hands owed clocks to the 8253 and raises IRQ0 on each rising edge of
    /// channel 0's output. The request stays latched until it is
    /// acknowledged, so an edge is never lost however much time is delivered
    /// at once. A running CPU gets a few clocks per instruction; a halted
    /// one gets everything up to the edge that can wake it.
    fn run_pit(&mut self) {
        loop {
            let budget = if self.cpu.halted {
                self.pit_debt
            } else {
                self.pit_debt.min(PIT_CLOCKS_PER_INSTRUCTION)
            };
            let advance = self.pit.borrow_mut().advance(budget as u32);
            self.pit_debt -= advance.clocks as u64;
            if advance.ch0_rising_edge {
                self.cpu.pic.request_interrupt(0);
            }
            let wakes_cpu = self.cpu.check_flag(Interrupt) && self.cpu.pic.has_interrupt();
            if !self.cpu.halted || self.pit_debt == 0 || wakes_cpu {
                break;
            }
        }
    }

    /// Stops following the host clock, so a test can feed timer time by hand
    /// through `credit_pit_time`.
    #[cfg(test)]
    pub(crate) fn use_manual_clock(&mut self) {
        self.manual_clock = true;
        self.pit_debt = 0;
        self.pit_frac = 0;
    }

    /// Terminal-mode main loop. Uses crossterm for display and keyboard.
    pub fn run(&mut self) {
        debug!(
            "Starting emulator with boot order: {:02X?}",
            self.disks.borrow().boot_order()
        );
        if self.trace {
            debug!("Tracing Enabled");
        }

        let _ = crossterm::terminal::enable_raw_mode();
        let _ = write!(std::io::stdout(), "\x1B[2J\x1B[H");
        let _ = std::io::stdout().flush();

        while self.running {
            self.step_frame();

            // Render VGA
            if let Some(vga) = &self.vga {
                let mut vga = vga.borrow_mut();
                vga.tick(RENDER_INTERVAL);
                vga.render(&self.cpu.memory);
            }

            // Sleep when halted to avoid busy-waiting
            if self.cpu.halted {
                std::thread::sleep(std::time::Duration::from_millis(10));
            }
        }

        let _ = crossterm::terminal::disable_raw_mode();
        let _ = writeln!(std::io::stdout(), "\x1B[0m\x1B[?25h");
        let _ = std::io::stdout().flush();

        debug!("Emulator exited after {} instructions", self.instruction_count);
    }

    pub fn port_in_byte(&mut self, port: u16) -> u8 {
        self.io_bus.port_in_byte(port, &mut self.cpu)
    }

    pub fn port_in_word(&mut self, port: u16) -> u16 {
        self.io_bus.port_in_word(port, &mut self.cpu)
    }

    pub fn port_out_byte(&mut self, port: u16, value: u8) {
        self.io_bus.port_out_byte(port, value, &mut self.cpu);
    }

    pub fn port_out_word(&mut self, port: u16, value: u16) {
        self.io_bus.port_out_word(port, value, &mut self.cpu);
    }

    /// Get a reference to the PIT (for keyboard controller setup).
    #[cfg(feature = "gui")]
    pub fn pit_ref(&self) -> &Rc<RefCell<crate::io::pit::Pit>> {
        &self.pit
    }

    /// Replace the keyboard controller (used by GUI mode to swap in a
    /// winit-driven controller). Also re-registers ports 0x60-0x63.
    #[cfg(feature = "gui")]
    pub fn replace_keyboard(&mut self, kb: Rc<RefCell<KeyboardController>>) {
        self.io_bus.replace(0x60, 0x63, Box::new(Rc::clone(&kb)));
        self.keyboard = Some(kb);
    }

    pub(crate) fn handle_interrupt(&mut self, vector: u8) {
        let addr = (vector as usize) * 4;

        // Push flags with 8086 fixed bits applied
        let flags = (self.cpu.flags & 0x0FD5) | 0xF002;
        self.push_word(flags);
        self.push_word(self.cpu.registers.cs.reg().word());
        self.push_word(self.cpu.registers.pc.word());

        // Clear IF and TF
        self.cpu.unset_flag(Interrupt);
        self.cpu.unset_flag(Trap);

        // Load new CS:IP from IVT
        self.cpu.registers.pc.set(self.cpu.memory.read_word(addr));
        self.cpu
            .registers
            .cs
            .reg_mut()
            .set(self.cpu.memory.read_word(addr + 2));
    }

    pub fn push_word(&mut self, word: u16) {
        let address = self.cpu.registers.sp.operation(2, u16::wrapping_sub);
        self.cpu.registers.ss.write_word(address, word);
    }

    pub fn pop_word(&mut self) -> u16 {
        let address = self.cpu.registers.sp.word();
        self.cpu.registers.sp.operation(2, u16::wrapping_add);
        self.cpu.registers.ss.read_word(address)
    }

    pub fn data_segment(&mut self) -> &mut Segment {
        match self.prefixes.segment {
            Some(segment) => self.cpu.get_segment(segment),
            None => &mut self.cpu.registers.ds,
        }
    }

    /// Picks the correct default segment for a ModR/M memory operand.
    /// BP-based addressing (rm=010, 011, or rm=110 with mod!=00) defaults to SS.
    /// All other modes default to DS. An explicit segment override prefix wins.
    pub fn effective_segment(&mut self, mod_val: u8, rm: u8) -> &mut Segment {
        if let Some(segment) = self.prefixes.segment {
            return self.cpu.get_segment(segment);
        }
        let bp_based = match rm {
            0b010 | 0b011 => true,
            0b110 => mod_val != 0b00,
            _ => false,
        };
        if bp_based {
            &mut self.cpu.registers.ss
        } else {
            &mut self.cpu.registers.ds
        }
    }
}

#[inline(always)]
fn show_flag(vm: &Runtime, flag: CpuFlag, c: char) -> char {
    if vm.cpu.check_flag(flag) {
        return c;
    }
    '-'
}

impl Debug for Runtime {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pc = self.cpu.registers.pc.word();
        write!(
            f,
            "{:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {}{}{}{}{} {:04x}:{:02x}{:02x} State: {}",
            self.cpu.registers.ax.word(),
            self.cpu.registers.bx.word(),
            self.cpu.registers.cx.word(),
            self.cpu.registers.dx.word(),
            self.cpu.registers.sp.word(),
            self.cpu.registers.bp.word(),
            self.cpu.registers.si.word(),
            self.cpu.registers.di.word(),
            show_flag(self, Overflow, 'O'),
            show_flag(self, Sign, 'S'),
            show_flag(self, Zero, 'Z'),
            show_flag(self, Carry, 'C'),
            show_flag(self, Interrupt, 'I'),
            pc,
            self.cpu.registers.cs.read_byte(pc),
            self.cpu.registers.cs.read_byte(pc.wrapping_add(1)),
            if self.cpu.halted { "Halted" } else if self.running { "Running" } else { "Stopped" }
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Regression: the fraction of a timer clock left over after each host
    /// clock sample was discarded, which made the timer run several percent
    /// slow while the guest was busy.
    #[test]
    fn timer_time_is_not_lost_to_rounding() {
        let mut rt = Runtime::new_test();
        rt.use_manual_clock();
        for _ in 0..1_000_000 {
            rt.credit_pit_time(100); // less than one timer clock
        }
        assert_eq!(rt.pit_debt, 119_318); // 0.1 s of a 1,193,182 Hz clock
    }

    #[test]
    fn timer_debt_is_capped_after_a_long_stall() {
        let mut rt = Runtime::new_test();
        rt.use_manual_clock();
        for _ in 0..3 {
            rt.credit_pit_time(NANOS_PER_SEC);
        }
        assert_eq!(rt.pit_debt, PIT_MAX_DEBT);
    }
}
