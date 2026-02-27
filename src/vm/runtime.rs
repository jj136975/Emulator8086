use crate::io::bus::IoBus;
use crate::io::console::DebugConsole;
use crate::io::disk::{DiskController, DiskImage, DiskSource, DiskSpec, HD_DEFAULT_SIZE_MB};
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

pub enum Prefix {
    Rep(bool),
    Lock,
    Seg(SegmentType),
    Queued(Box<Prefix>),
}

const RENDER_INTERVAL: u64 = 50_000; // every ~50K instructions
const TRACE_RING_SIZE: usize = 32;
const CS_CHANGE_RING_SIZE: usize = 16;

#[derive(Clone, Copy, Default)]
pub struct TraceEntry {
    pub cs: u16,
    pub ip: u16,
    pub bytes: [u8; 8],
}

#[derive(Clone, Copy, Default)]
pub struct CsChangeEntry {
    pub instr_count: u64,
    pub old_cs: u16,
    pub old_ip: u16,
    pub new_cs: u16,
    pub new_ip: u16,
    pub ax: u16,
    pub bx: u16,
    pub cx: u16,
    pub dx: u16,
    pub sp: u16,
    pub ss: u16,
    pub ds: u16,
    pub es: u16,
    pub flags: u16,
    pub bytes_at_old: [u8; 8],
}

pub struct Runtime {
    pub(crate) cpu: Cpu,
    running: bool,
    status: u16,
    pub prefix: Option<Prefix>,
    pub segment_override: Option<SegmentType>,
    pub boot_order: Vec<u8>,
    pub instruction_count: u64,
    pub trace: bool,
    pub interrupt_inhibit: bool,
    pub trace_ring: [TraceEntry; TRACE_RING_SIZE],
    pub trace_ring_pos: usize,
    pub cs_change_ring: [CsChangeEntry; CS_CHANGE_RING_SIZE],
    pub cs_change_ring_pos: usize,
    pub last_cs: u16,

    // Devices
    io_bus: IoBus,
    pub disks: Rc<RefCell<DiskController>>,
    keyboard: Option<Rc<RefCell<KeyboardController>>>,
    pub vga: Option<Rc<RefCell<VgaDevice>>>,
    pit: Rc<RefCell<Pit>>,

    // Timing
    last_pit_tick: Instant,
}

impl Runtime {
    #[cfg(test)]
    pub fn new_test() -> Self {
        use std::ops::DerefMut;
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
            status: 0,
            prefix: None,
            segment_override: None,
            io_bus: IoBus::new(),
            boot_order: Vec::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,
            trace_ring: [TraceEntry::default(); TRACE_RING_SIZE],
            trace_ring_pos: 0,
            cs_change_ring: [CsChangeEntry::default(); CS_CHANGE_RING_SIZE],
            cs_change_ring_pos: 0,
            last_cs: 0,

            disks: Rc::new(RefCell::new(DiskController::new())),
            keyboard: None,
            vga: None,
            pit: Rc::new(RefCell::new(Pit::new())),
            last_pit_tick: Instant::now(),
        }
    }

    pub fn new(
        disk_specs: Vec<DiskSpec>,
        hd_specs: Vec<String>,
        boot_order: Option<Vec<u8>>,
        trace: bool,
    ) -> Self {
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

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

        // Create I/O bus and register devices
        let mut io_bus = IoBus::new();
        let keyboard = Rc::new(RefCell::new(KeyboardController::new()));
        let vga = Rc::new(RefCell::new(VgaDevice::new()));
        let disk_ctrl = Rc::new(RefCell::new(disk_ctrl));
        let pit = Rc::new(RefCell::new(Pit::new()));

        keyboard.borrow_mut().set_pit(Rc::clone(&pit));

        io_bus.register(0x00, 0x0F, Box::new(DmaController::new()));
        io_bus.register(0x20, 0x21, Box::new(PicDevice));
        io_bus.register(0x40, 0x43, Box::new(Rc::clone(&pit)));
        io_bus.register(0x60, 0x63, Box::new(Rc::clone(&keyboard)));
        io_bus.register(0x81, 0x87, Box::new(DmaPageRegisters::new()));
        io_bus.register(0xB0, 0xB0, Box::new(Rc::clone(&disk_ctrl)));
        io_bus.register(0xE9, 0xE9, Box::new(DebugConsole));
        io_bus.register(0x320, 0x323, Box::new(HdcStub::new()));
        io_bus.register(0x3C0, 0x3DA, Box::new(Rc::clone(&vga)));
        io_bus.register(0x3F0, 0x3F7, Box::new(FdcStub::new()));

        let mut vm = Self {
            cpu: Cpu {
                cpu_type: CpuType::Intel8086,
                registers,
                memory,
                flags: 0,
                halted: false,
                pic: Pic::new(),
            },
            running: true,
            status: 0,
            prefix: None,
            segment_override: None,
            io_bus,
            boot_order: boot_order.unwrap_or_else(|| vec![0x00, 0x80]),
            instruction_count: 0,
            trace,
            interrupt_inhibit: false,
            trace_ring: [TraceEntry::default(); TRACE_RING_SIZE],
            trace_ring_pos: 0,
            cs_change_ring: [CsChangeEntry::default(); CS_CHANGE_RING_SIZE],
            cs_change_ring_pos: 0,
            last_cs: 0xF000,

            disks: disk_ctrl,
            keyboard: Some(keyboard),
            vga: Some(vga),
            pit,
            last_pit_tick: Instant::now(),
        };

        vm.cpu.set_flag(Interrupt);
        vm
    }

    pub fn exit(&mut self, status: u16) {
        self.running = false;
        self.status = status;
    }

    pub fn record_trace(&mut self) {
        let cs = self.cpu.registers.cs.reg().word();
        let ip = self.cpu.registers.pc.word();
        let mut bytes = [0u8; 8];
        for i in 0..8 {
            bytes[i] = self.cpu.registers.cs.read_byte(ip.wrapping_add(i as u16));
        }
        self.trace_ring[self.trace_ring_pos] = TraceEntry { cs, ip, bytes };
        self.trace_ring_pos = (self.trace_ring_pos + 1) % TRACE_RING_SIZE;

        // Detect CS changes
        if cs != self.last_cs {
            let old_cs = self.last_cs;
            // Read bytes at old CS:IP (from previous trace entry)
            let prev_idx = (self.trace_ring_pos + TRACE_RING_SIZE - 2) % TRACE_RING_SIZE;
            let old_entry = &self.trace_ring[prev_idx];
            self.cs_change_ring[self.cs_change_ring_pos] = CsChangeEntry {
                instr_count: self.instruction_count,
                old_cs,
                old_ip: old_entry.ip,
                new_cs: cs,
                new_ip: ip,
                ax: self.cpu.registers.ax.word(),
                bx: self.cpu.registers.bx.word(),
                cx: self.cpu.registers.cx.word(),
                dx: self.cpu.registers.dx.word(),
                sp: self.cpu.registers.sp.word(),
                ss: self.cpu.registers.ss.reg().word(),
                ds: self.cpu.registers.ds.reg().word(),
                es: self.cpu.registers.es.reg().word(),
                flags: self.cpu.flags,
                bytes_at_old: old_entry.bytes,
            };
            self.cs_change_ring_pos = (self.cs_change_ring_pos + 1) % CS_CHANGE_RING_SIZE;
            self.last_cs = cs;
        }
    }

    pub fn dump_trace(&self) {
        eprintln!("=== Last {} CS changes ===", CS_CHANGE_RING_SIZE);
        for i in 0..CS_CHANGE_RING_SIZE {
            let idx = (self.cs_change_ring_pos + i) % CS_CHANGE_RING_SIZE;
            let e = &self.cs_change_ring[idx];
            if e.old_cs == 0 && e.new_cs == 0 && e.instr_count == 0 {
                continue;
            }
            eprintln!(
                "  #{:<10} {:04X}:{:04X} -> {:04X}:{:04X}  AX={:04X} BX={:04X} CX={:04X} DX={:04X} SP={:04X} SS={:04X} DS={:04X} ES={:04X} F={:04X}  [{:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X}]",
                e.instr_count, e.old_cs, e.old_ip, e.new_cs, e.new_ip,
                e.ax, e.bx, e.cx, e.dx, e.sp, e.ss, e.ds, e.es, e.flags,
                e.bytes_at_old[0], e.bytes_at_old[1], e.bytes_at_old[2], e.bytes_at_old[3],
                e.bytes_at_old[4], e.bytes_at_old[5], e.bytes_at_old[6], e.bytes_at_old[7],
            );
        }
        eprintln!("=== Last {} instructions before crash ===", TRACE_RING_SIZE);
        for i in 0..TRACE_RING_SIZE {
            let idx = (self.trace_ring_pos + i) % TRACE_RING_SIZE;
            let entry = &self.trace_ring[idx];
            if entry.cs == 0 && entry.ip == 0 && entry.bytes == [0; 8] {
                continue;
            }
            eprintln!(
                "  {:04X}:{:04X}  {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X}",
                entry.cs, entry.ip,
                entry.bytes[0], entry.bytes[1], entry.bytes[2], entry.bytes[3],
                entry.bytes[4], entry.bytes[5], entry.bytes[6], entry.bytes[7],
            );
        }
        eprintln!("=========================================");
    }

    pub(crate) fn load_rom(&mut self, path: PathBuf) {
        let rom_data = std::fs::read(path).expect("Failed to read ROM file");
        if rom_data.len() > 0x10000 {
            panic!("ROM file too large (max 64KB)");
        }

        let load_address = 0xF0000;
        for (i, byte) in rom_data.iter().enumerate() {
            self.cpu.memory.write_byte_unchecked(load_address + i, *byte);
        }
        self.cpu.memory.enable_rom_protection();
    }

    /// Execute one batch of CPU instructions, tick PIT, poll keyboard, and
    /// handle interrupts. Returns `false` when the emulator should stop.
    /// Called from the terminal `run()` loop and the GUI event loop.
    pub fn step_frame(&mut self) -> bool {
        const PIT_CLOCK_HZ: f64 = 1_193_182.0;

        for _ in 0..RENDER_INTERVAL {
            if !self.running {
                return false;
            }

            if self.trace {
                debug!("{:#?}", self);
            }

            if !self.cpu.halted {
                self.record_trace();
                process(self);
                self.instruction_count += 1;
            }

            // PIT timing
            let now = Instant::now();
            let elapsed = now.duration_since(self.last_pit_tick).as_secs_f64();
            let pit_ticks = ((elapsed * PIT_CLOCK_HZ) as u64).min(65535) as u16;
            if pit_ticks > 0 {
                self.last_pit_tick = now;
                let pit_result = self.pit.borrow_mut().tick(pit_ticks);
                if pit_result.ch0_rising_edge {
                    self.cpu.pic.request_interrupt(0);
                }
                if pit_result.ch0_falling_edge {
                    self.cpu.pic.clear_interrupt(0);
                }
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
                            enter_monitor(&mut *self.disks.borrow_mut());
                            if let Some(vga) = &self.vga {
                                vga.borrow_mut().queue_full_redraw();
                            }
                            self.last_pit_tick = Instant::now();
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

            // Interrupt handling
            if self.interrupt_inhibit || self.prefix.is_some() {
                self.interrupt_inhibit = false;
            } else if self.cpu.check_flag(Interrupt) && self.cpu.pic.has_interrupt() {
                let vector = self.cpu.pic.acknowledge();
                self.cpu.halted = false;
                self.handle_interrupt(vector);
            } else if self.cpu.halted {
                break; // exit batch early when halted
            }
        }

        self.running
    }

    /// Terminal-mode main loop. Uses crossterm for display and keyboard.
    pub fn run(&mut self) {
        debug!(
            "Starting emulator with boot order: {:02X?}",
            self.boot_order
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
        let _ = write!(std::io::stdout(), "\x1B[0m\x1B[?25h\n");
        let _ = std::io::stdout().flush();

        debug!(
            "Emulator exited with status {:04X} after {} instructions",
            self.status, self.instruction_count
        );
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
        if let Some(segment) = self.segment_override {
            return self.cpu.get_segment(segment);
        }
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.cpu.get_segment(*segment);
        }
        &mut self.cpu.registers.ds
    }

    /// Picks the correct default segment for a ModR/M memory operand.
    /// BP-based addressing (rm=010, 011, or rm=110 with mod!=00) defaults to SS.
    /// All other modes default to DS. An explicit segment override prefix wins.
    pub fn effective_segment(&mut self, mod_val: u8, rm: u8) -> &mut Segment {
        if let Some(segment) = self.segment_override {
            return self.cpu.get_segment(segment);
        }
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.cpu.get_segment(*segment);
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
