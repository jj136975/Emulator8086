//! The machine: the CPU, the devices on the port bus, and the run loop that
//! drives them. Each pass of `run` makes one `process` call (a prefix byte
//! takes a call of its own), hands the 8253 timer clocks it is owed, polls
//! the keyboard and takes a pending interrupt. Also here: the flag and
//! segment encodings and the prefix latches the instruction code shares,
//! interrupt entry, reset, the ROM loader, the devices it lends to whoever
//! reads them (`pit`, `keyboard`, `debug_console`), and the account of the
//! guest's time it keeps for whoever measures it (`guest_time`).
//!
//! The invariant: an interrupt is taken only between two whole instructions,
//! never after a prefix byte (`prefixes.pending`) or an instruction that sets
//! `interrupt_inhibit` (STI, a load of SS), and an IRQ goes before the
//! single-step trap, which is sampled before the instruction runs. While the
//! CPU runs, the 8253 gets only a few of the clocks it is owed (`pit_debt`)
//! per instruction, so the guest has time to service every timer interrupt.
//! The ROM socket, the top 64 KB of the first megabyte, holds one image and
//! nothing of the one before it.
//!
//! The machine reads no clock. Time passes for the guest only when the
//! owner of the machine says how much has passed on the host
//! (`credit_pit_time`): the app measures it, a test states it.

use crate::io::bus::IoBus;
use crate::io::console::DebugConsole;
use crate::io::disk::{BOOT_ORDER_PORT, DISK_TRAP_PORT, DiskController, DiskSpec, WallClock};
use crate::io::disk_spec::open_drives;
use crate::io::dma::{DmaController, DmaPageRegisters};
use crate::io::fdc::FdcStub;
use crate::io::hdc::HdcStub;
use crate::io::keyboard::KeyboardController;
use crate::io::pic::{Pic, PicDevice};
use crate::io::pit::{self, Pit};
use crate::io::vga::VgaDevice;
use crate::vm::cpu::Cpu;
use crate::vm::instructions::process;
use crate::vm::memory::{MEMORY_SIZE, Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::CpuFlag::{Carry, Interrupt, Overflow, Sign, Trap, Zero};
use log::{debug, warn};
use std::cell::{Cell, Ref, RefCell};
use std::fmt::{Debug, Formatter};
use std::ops::DerefMut;
use std::path::PathBuf;
use std::rc::Rc;

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

/// Instructions a test runs per `step_frame` call, at most.
#[cfg(test)]
const FRAME_INSTRUCTIONS: u64 = 50_000;

/// Size of the ROM socket, F000:0000-FFFF: the most a ROM image can be.
const ROM_MAX_SIZE: usize = 0x10000;
/// Where the ROM socket starts.
const ROM_START: usize = MEMORY_SIZE - ROM_MAX_SIZE;
/// The least a ROM image can be: the 16 bytes from the reset vector at
/// F000:FFF0, where the CPU fetches its first instruction, to the end of
/// the first megabyte. A shorter image would leave the CPU to start in the
/// empty socket below it.
const ROM_MIN_SIZE: usize = 16;
/// What the part of the socket an image leaves empty reads as: nothing
/// answers there, and a PC reads 0xFF from an address nothing answers.
const EMPTY_SOCKET: u8 = 0xFF;

/// Input clock of the 8253 timer, in Hz.
const PIT_CLOCK_HZ: u64 = pit::INPUT_HZ;
const NANOS_PER_SEC: u64 = 1_000_000_000;
/// Timer clocks handed to the 8253 per executed instruction while it is
/// behind: few enough that the guest gets to service every timer interrupt,
/// enough to catch up within a few thousand instructions.
const PIT_CLOCKS_PER_INSTRUCTION: u64 = 4;
/// Most timer clocks the guest can be owed (250 ms worth). After a longer
/// stall the excess is dropped rather than replayed as a burst of ticks.
const PIT_MAX_DEBT: u64 = PIT_CLOCK_HZ / 4;

/// Where the guest's time went since power-on, in clocks of the 8253's
/// input (`pit::INPUT_HZ`): handed over, owed, or dropped. A copy: holding
/// one changes nothing in the machine.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct GuestTime {
    /// Clocks handed to the 8253: the time that has passed for the guest.
    pub delivered: u64,
    /// Of those, the clocks handed over while the CPU was halted.
    pub halted: u64,
    /// Clocks owed for host time that has passed, not handed over yet.
    pub owed: u64,
    /// Clocks the guest will never be handed: host time it was told of
    /// beyond what it can be owed.
    pub dropped: u64,
}

pub struct Runtime {
    /// The CPU state. The app reads it (the frame is drawn from
    /// `cpu.memory`); whoever writes to it keeps the rule of `vm`: the
    /// `Memory` box is never replaced.
    pub cpu: Cpu,
    pub prefixes: Prefixes,
    pub instruction_count: u64,
    pub trace: bool,
    pub interrupt_inhibit: bool,

    io_bus: IoBus,
    pub disks: Rc<RefCell<DiskController>>,
    keyboard: Option<Rc<RefCell<KeyboardController>>>,
    pub vga: Option<Rc<RefCell<VgaDevice>>>,
    pit: Rc<RefCell<Pit>>,
    debug_console: Option<Rc<RefCell<DebugConsole>>>,
    /// What the user knows the image in the ROM socket by, once there is
    /// one (`load_rom_image`).
    rom: Option<String>,

    /// Timer clocks owed to the 8253 for host time that has already passed.
    pit_debt: u64,
    /// Remainder of the time-to-clocks conversion, in units of 1e-9 clock.
    pit_frac: u64,
    /// Timer clocks delivered to the 8253 so far; the VGA derives its
    /// retrace timing from this count.
    video_clock: Rc<Cell<u64>>,
    /// Of the clocks delivered, those delivered while the CPU was halted.
    halted_clocks: u64,
    /// Timer clocks the machine was told of and never owed: what a credit
    /// put over `PIT_MAX_DEBT`, and what was over a second in one credit.
    dropped_clocks: u64,
}

impl Runtime {
    #[cfg(test)]
    pub fn new_test() -> Self {
        let mut memory = Box::new(Memory::new());

        let registers = Registers::new(memory.deref_mut());
        Self {
            cpu: Cpu {
                registers,
                memory,
                flags: 0,
                halted: false,
                pic: Pic::new(),
            },
            prefixes: Prefixes::default(),
            io_bus: IoBus::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,

            disks: Rc::new(RefCell::new(DiskController::new())),
            keyboard: None,
            vga: None,
            pit: Rc::new(RefCell::new(Pit::new())),
            debug_console: None,
            rom: None,
            pit_debt: 0,
            pit_frac: 0,
            video_clock: Rc::new(Cell::new(0)),
            halted_clocks: 0,
            dropped_clocks: 0,
        }
    }

    /// Builds the machine with the disks the command line asked for. Fails,
    /// with a message for the user, if one of the images cannot be used.
    /// `keyboard` holds the receiving end of the channel the window sends
    /// its key events down. `clock` is the host's wall clock, for the disks
    /// that are formatted here.
    ///
    /// What the opening of the disks has to tell the user (`OpenedDrives`)
    /// goes to the log here. A caller with a better place for it opens the
    /// drives itself (`open_drives`) and builds the machine around them
    /// (`with_drives`).
    pub fn new(
        disk_specs: Vec<DiskSpec>,
        hd_specs: Vec<String>,
        boot_order: Option<Vec<u8>>,
        trace: bool,
        keyboard: KeyboardController,
        clock: &dyn WallClock,
    ) -> Result<Self, String> {
        let opened = open_drives(disk_specs, &hd_specs, clock)?;
        for note in &opened.notes {
            warn!("{}", note);
        }
        Ok(Self::with_drives(
            opened.controller,
            boot_order,
            trace,
            keyboard,
        ))
    }

    /// Builds the machine around drives that are open already: `drives`,
    /// which the bootstrap tries in `boot_order` if one is given.
    pub fn with_drives(
        mut drives: DiskController,
        boot_order: Option<Vec<u8>>,
        trace: bool,
        keyboard: KeyboardController,
    ) -> Self {
        if let Some(order) = boot_order {
            drives.set_boot_order(order);
        }

        let mut vm = Self::with_devices(drives, keyboard);
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
        let debug_console = Rc::new(RefCell::new(DebugConsole::new()));

        let video_clock = vga.borrow().timer_clock();

        keyboard.borrow_mut().set_pit(Rc::clone(&pit));

        io_bus.register(0x00, 0x0F, Box::new(DmaController::new()));
        io_bus.register(0x20, 0x21, Box::new(PicDevice));
        io_bus.register(0x40, 0x43, Box::new(Rc::clone(&pit)));
        io_bus.register(0x60, 0x63, Box::new(Rc::clone(&keyboard)));
        io_bus.register(0x81, 0x87, Box::new(DmaPageRegisters::new()));
        io_bus.register(
            DISK_TRAP_PORT,
            BOOT_ORDER_PORT,
            Box::new(Rc::clone(&disk_ctrl)),
        );
        io_bus.register(0xE9, 0xE9, Box::new(Rc::clone(&debug_console)));
        io_bus.register(0x320, 0x323, Box::new(HdcStub::new()));
        io_bus.register(0x3C0, 0x3DA, Box::new(Rc::clone(&vga)));
        io_bus.register(0x3F0, 0x3F7, Box::new(FdcStub::new()));

        Self {
            cpu: Cpu {
                registers,
                memory,
                flags: 0xF002, // 8086 reset state: interrupts disabled
                halted: false,
                pic: Pic::new(),
            },
            prefixes: Prefixes::default(),
            io_bus,
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,

            disks: disk_ctrl,
            keyboard: Some(keyboard),
            vga: Some(vga),
            pit,
            debug_console: Some(debug_console),
            rom: None,
            pit_debt: 0,
            pit_frac: 0,
            video_clock,
            halted_clocks: 0,
            dropped_clocks: 0,
        }
    }

    /// A fully wired machine for integration tests. The test types on its
    /// keyboard through the returned channel, as the window does.
    #[cfg(test)]
    pub fn new_test_machine(
        disk_ctrl: DiskController,
    ) -> (
        Self,
        std::sync::mpsc::Sender<crate::io::keyboard::EmulatorEvent>,
    ) {
        let (tx, rx) = std::sync::mpsc::channel();
        let vm = Self::with_devices(disk_ctrl, KeyboardController::with_receiver(rx));
        (vm, tx)
    }

    /// Loads the ROM image in the file `path` (`load_rom_image`), known by
    /// its path. Fails, with a message for the user that names the file, if
    /// the file cannot be read or holds no ROM image.
    pub fn load_rom(&mut self, path: PathBuf) -> Result<(), String> {
        let image = std::fs::read(&path)
            .map_err(|error| format!("cannot read ROM image '{}': {}", path.display(), error))?;
        self.load_rom_image(&image, path.display().to_string())
    }

    /// Puts `image` in the ROM socket, the top 64 KB of the first megabyte,
    /// and write-protects the socket. The image ends at F000:FFFF, so that
    /// the reset vector at F000:FFF0 lies in it, and the part of the socket
    /// below it reads as an empty socket does, whatever image was there
    /// before. `name` is what the user knows the image by (`rom`).
    ///
    /// Fails, with a message for the user that names the image, if it is
    /// not 16 bytes to 64 KB long; the machine is then left as it was.
    /// Nothing else is refused: the 8086 runs whatever it finds at the
    /// reset vector, and a ROM is not known by what it holds. Its checksum
    /// is a BIOS's test of itself, which a test ROM need not pass, and its
    /// first instruction need not be a far jump.
    ///
    /// The CPU is left as it is: whoever swaps the ROM of a machine that
    /// has run reboots it (`reset`).
    pub fn load_rom_image(&mut self, image: &[u8], name: impl Into<String>) -> Result<(), String> {
        let name = name.into();
        if !(ROM_MIN_SIZE..=ROM_MAX_SIZE).contains(&image.len()) {
            let size = match image.len() {
                0 => "empty".to_string(),
                bytes => format!("{bytes} bytes"),
            };
            return Err(format!(
                "{name} is {size}, and a ROM image is 16 bytes to 64 KB"
            ));
        }

        let mut socket = vec![EMPTY_SOCKET; ROM_MAX_SIZE];
        socket[ROM_MAX_SIZE - image.len()..].copy_from_slice(image);
        Memory::copy_data(&mut self.cpu.memory, ROM_START, &socket);
        self.cpu.memory.enable_rom_protection();
        self.rom = Some(name);
        Ok(())
    }

    /// What the user knows the image in the ROM socket by: the name it was
    /// loaded under (`load_rom_image`), or `None` before one was.
    pub fn rom(&self) -> Option<&str> {
        self.rom.as_deref()
    }

    /// Runs the guest for at most `instructions` passes: one `process` call
    /// each (a halted CPU makes none), then the timer clocks owed, the
    /// keyboard and the interrupts. Returns early when the CPU is halted
    /// and nothing woke it: until more time is credited or a key arrives,
    /// another call would find the same.
    ///
    /// No time passes in here. Whoever paces the guest by a clock calls
    /// this for a few instructions at a time, with `credit_pit_time`
    /// between the calls.
    pub fn run(&mut self, instructions: u64) {
        for _ in 0..instructions {
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

            if self.pit_debt > 0 {
                self.run_pit();
            }

            if (self.instruction_count.is_multiple_of(1000) || self.cpu.halted)
                && let Some(kb) = &self.keyboard
            {
                kb.borrow_mut().poll(&mut self.cpu.pic);
            }

            // Interrupts are not recognised between a prefix byte and the
            // instruction it belongs to, nor at the boundary right after STI
            // or a load of SS.
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
    }

    /// One batch of a test: `run` for `FRAME_INSTRUCTIONS`.
    #[cfg(test)]
    pub(crate) fn step_frame(&mut self) {
        self.run(FRAME_INSTRUCTIONS);
    }

    /// Reboots the guest: the CPU continues at the reset vector in its
    /// power-on state. Memory and devices are left as they are; the BIOS
    /// initialises what it uses.
    pub fn reset(&mut self) {
        let registers = &mut self.cpu.registers;
        registers.cs.reg_mut().set(0xFFFF);
        registers.pc.set(0x0000);
        registers.ds.reg_mut().set(0x0000);
        registers.es.reg_mut().set(0x0000);
        registers.ss.reg_mut().set(0x0000);
        self.cpu.flags = 0xF002; // interrupts disabled
        self.cpu.halted = false;
        self.prefixes = Prefixes::default();
        self.interrupt_inhibit = false;
    }

    /// The state of the display as text, for a diagnosis
    /// (`VgaDevice::dump_text`), or `None` on a machine without a VGA. The
    /// machine writes it nowhere: its owner decides where the text goes.
    pub fn vga_dump(&self) -> Option<String> {
        let vga = self.vga.as_ref()?;
        Some(vga.borrow().dump_text(&self.cpu.memory))
    }

    /// The 8253, lent to be read (`Pit::channels`). The loan borrows the
    /// machine: nothing runs while it is out.
    pub fn pit(&self) -> Ref<'_, Pit> {
        self.pit.borrow()
    }

    /// The keyboard interface, lent to be read (`KeyboardController::view`),
    /// or `None` on a machine without one. The loan borrows the machine:
    /// nothing runs while it is out.
    pub fn keyboard(&self) -> Option<Ref<'_, KeyboardController>> {
        self.keyboard.as_ref().map(|keyboard| keyboard.borrow())
    }

    /// The debug port E9h, lent to be read (`DebugConsole::lines`), or
    /// `None` on a machine without one. The loan borrows the machine:
    /// nothing runs while it is out.
    pub fn debug_console(&self) -> Option<Ref<'_, DebugConsole>> {
        self.debug_console.as_ref().map(|console| console.borrow())
    }

    /// Where the guest's time went since power-on (`GuestTime`). An account
    /// the machine keeps for whoever measures it: reading it changes
    /// nothing in the guest.
    pub fn guest_time(&self) -> GuestTime {
        GuestTime {
            delivered: self.video_clock.get(),
            halted: self.halted_clocks,
            owed: self.pit_debt,
            dropped: self.dropped_clocks,
        }
    }

    /// Tells the machine that `nanos` of host time have passed (at most one
    /// second per call; what is more is dropped). The time becomes timer
    /// clocks owed to the 8253, which `run` hands over as the guest
    /// executes: this is the only way time passes for the guest. The
    /// sub-clock remainder is carried over, so no time is lost to rounding
    /// however often this is called. What is dropped, here or at the cap on
    /// the debt, is counted (`GuestTime::dropped`).
    pub fn credit_pit_time(&mut self, nanos: u64) {
        let credited = nanos.min(NANOS_PER_SEC);
        if credited < nanos {
            self.dropped_clocks = self
                .dropped_clocks
                .saturating_add(clocks_in(nanos - credited));
        }
        self.pit_frac += credited * PIT_CLOCK_HZ;
        let owed = self.pit_debt + self.pit_frac / NANOS_PER_SEC;
        self.pit_debt = owed.min(PIT_MAX_DEBT);
        self.dropped_clocks = self.dropped_clocks.saturating_add(owed - self.pit_debt);
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
            self.video_clock
                .set(self.video_clock.get() + advance.clocks as u64);
            if self.cpu.halted {
                self.halted_clocks += advance.clocks as u64;
            }
            if advance.ch0_rising_edge {
                self.cpu.pic.request_interrupt(0);
            }
            let wakes_cpu = self.cpu.check_flag(Interrupt) && self.cpu.pic.has_interrupt();
            if !self.cpu.halted || self.pit_debt == 0 || wakes_cpu {
                break;
            }
        }
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

    pub(crate) fn handle_interrupt(&mut self, vector: u8) {
        let addr = (vector as usize) * 4;

        // The 8086 stores FLAGS bits 12-15 and 1 as ones, bits 3 and 5 as
        // zeros.
        let flags = (self.cpu.flags & 0x0FD5) | 0xF002;
        self.push_word(flags);
        self.push_word(self.cpu.registers.cs.reg().word());
        self.push_word(self.cpu.registers.pc.word());

        self.cpu.unset_flag(Interrupt);
        self.cpu.unset_flag(Trap);

        // The vector's entry in the table at 0000:0000 is the handler's IP,
        // then its CS.
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

/// The timer clocks `nanos` of host time come to, rounded down.
fn clocks_in(nanos: u64) -> u64 {
    let clocks = u128::from(nanos) * u128::from(PIT_CLOCK_HZ) / u128::from(NANOS_PER_SEC);
    u64::try_from(clocks).unwrap_or(u64::MAX)
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
            if self.cpu.halted { "Halted" } else { "Running" }
        )?;
        Ok(())
    }
}

#[cfg(test)]
#[path = "runtime_tests.rs"]
mod tests;
