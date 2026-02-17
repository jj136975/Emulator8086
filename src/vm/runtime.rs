use crate::io::bus::IoBus;
use crate::io::console::{DebugConsole, PitStub};
use crate::io::disk::{DiskController, DiskImage, DiskSource, DiskSpec, HD_DEFAULT_SIZE_MB};
use crate::io::keyboard::KeyboardController;
use crate::io::pic::{Pic, PicDevice};
use crate::vm::cpu::{Cpu, CpuType};
use crate::vm::instructions::process;
use crate::vm::memory::{Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::CpuFlag::{Carry, Interrupt, Overflow, Sign, Trap, Zero};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::io::Write;
use std::ops::DerefMut;
use std::path::PathBuf;
use std::rc::Rc;
use log::debug;
use crate::io::vga::VgaTextMode;

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

pub struct Runtime {
    pub(crate) cpu: Cpu,
    running: bool,
    status: u16,
    pub prefix: Option<Prefix>,
    pub segment_override: Option<SegmentType>,
    io_bus: IoBus,
    pub(crate) hard_disks: Vec<Option<DiskImage>>,
    pub boot_order: Vec<u8>,
    pub instruction_count: u64,
    pub trace: bool,
    pub interrupt_inhibit: bool,

    // Devices
    keyboard: Option<Rc<RefCell<KeyboardController>>>,
    vga: Option<Rc<RefCell<VgaTextMode>>>,
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
            hard_disks: Vec::new(),
            boot_order: Vec::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,

            keyboard: None,
            vga: None,
        }
    }

    /// Test constructor with PIC, IoBus, and keyboard shared state.
    /// Does NOT start input thread, raw mode, or BIOS init.
    #[cfg(test)]
    pub fn new_bios_test() -> Self {
        use std::ops::DerefMut;
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

        // let keyboard = crate::io::keyboard::Keyboard::new();
        // let kb_data_avail = keyboard.shared_data_available();

        let mut io_bus = IoBus::new();
        io_bus.register(0x20, 0x21, Box::new(PicDevice));

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
            io_bus,
            hard_disks: Vec::new(),
            boot_order: Vec::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,

            keyboard: None,
            vga: None,
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
                        // Non-boot drive: create if doesn't exist
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
        let mut hard_disks: Vec<Option<DiskImage>> = Vec::new();
        for spec in hd_specs {
            let image = if spec.eq_ignore_ascii_case("memory") {
                DiskImage::new_in_memory_hard_disk(HD_DEFAULT_SIZE_MB)
            } else {
                let path = PathBuf::from(spec);
                DiskImage::open_or_create_hard_disk(&path, HD_DEFAULT_SIZE_MB)
                    .expect("Failed to open/create hard disk image")
            };
            hard_disks.push(Some(image));
        }

        // Create I/O bus and register devices
        let mut io_bus = IoBus::new();
        let keyboard = Rc::new(RefCell::new(KeyboardController::new()));
        let vga = Rc::new(RefCell::new(VgaTextMode::new()));

        io_bus.register(0x20, 0x21, Box::new(PicDevice));
        io_bus.register(0x40, 0x43, Box::new(PitStub));
        io_bus.register(0x60, 0x61, Box::new(Rc::clone(&keyboard)));
        io_bus.register(0x64, 0x64, Box::new(Rc::clone(&keyboard)));
        io_bus.register(0xB0, 0xB0, Box::new(disk_ctrl));
        io_bus.register(0xE9, 0xE9, Box::new(DebugConsole));
        io_bus.register(0x3D4, 0x3DA, Box::new(Rc::clone(&vga)));

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
            hard_disks,
            boot_order: boot_order.unwrap_or_else(|| vec![0x00, 0x80]),
            instruction_count: 0,
            trace,
            interrupt_inhibit: false,

            keyboard: Some(keyboard),
            vga: Some(vga),
        };

        vm.cpu.set_flag(Interrupt);
        // crate::bios::init::init_bios(&mut vm);
        vm
    }

    pub fn exit(&mut self, status: u16) {
        self.running = false;
        self.status = status;
    }

    pub(crate) fn load_rom(&mut self, path: PathBuf) {
        let rom_data = std::fs::read(path).expect("Failed to read ROM file");
        if rom_data.len() > 0x10000 {
            panic!("ROM file too large (max 64KB)");
        }

        let load_address = 0xF0000;
        for (i, byte) in rom_data.iter().enumerate() {
            self.cpu.memory.write_byte(load_address + i, *byte);
        }
    }

    pub fn run(&mut self) {
        debug!("Starting emulator with boot order: {:02X?}", self.boot_order);
        if self.trace {
            debug!("Tracing Enabled");
        }

        let _ = crossterm::terminal::enable_raw_mode();
        let _ = write!(std::io::stdout(), "\x1B[2J\x1B[H");
        let _ = std::io::stdout().flush();

        while self.running {
            if self.trace {
                debug!("{:#?}", self);
            }

            if !self.cpu.halted {
                process(self);
                self.instruction_count += 1;
            }

            if self.instruction_count % RENDER_INTERVAL == 0 || self.cpu.halted {
                if let Some(vga) = &self.vga {
                    vga.borrow_mut().render(&self.cpu.memory);
                }
            }

            // Poll keyboard every 1000 instructions
            if self.instruction_count % 1000 == 0 || self.cpu.halted {
                if let Some(kb) = &self.keyboard {
                    if kb.borrow_mut().poll(&mut self.cpu.pic) {
                        self.running = false; // Ctrl+C
                    }
                }
            }


            if self.interrupt_inhibit {
                self.interrupt_inhibit = false; // one-shot, cleared after next instruction
            } else if self.cpu.pic.has_interrupt() {
                let vector = self.cpu.pic.acknowledge();
                self.cpu.halted = false;
                self.handle_interrupt(vector);
            } else if self.cpu.halted {
                // If halted and no interrupts, just wait (don't burn CPU cycles)
                std::thread::sleep(std::time::Duration::from_millis(10));
            }
        }

        let _ = crossterm::terminal::disable_raw_mode();
        let _ = write!(std::io::stdout(), "\x1B[0m\x1B[?25h\n");
        let _ = std::io::stdout().flush();

        debug!("Emulator exited with status {:04X} after {} instructions", self.status, self.instruction_count);
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

    /// Check if the F12 monitor flag is set.
    // pub fn check_monitor_flag(&self) -> bool {
    //     self.monitor_flag
    //         .as_ref()
    //         .map_or(false, |f| f.load(Ordering::SeqCst))
    // }

    /// Look up a disk by BIOS drive number (DL < 0x80 = floppy, DL >= 0x80 = hard disk).
    // pub fn get_disk(&self, dl: u8) -> Option<&DiskImage> {
    //     if dl >= 0x80 {
    //         self.hard_disks
    //             .get((dl - 0x80) as usize)
    //             .and_then(|d| d.as_ref())
    //     } else {
    //         self.disks.get(dl as usize).and_then(|d| d.as_ref())
    //     }
    // }
    //
    // pub fn get_disk_mut(&mut self, dl: u8) -> Option<&mut DiskImage> {
    //     if dl >= 0x80 {
    //         self.hard_disks
    //             .get_mut((dl - 0x80) as usize)
    //             .and_then(|d| d.as_mut())
    //     } else {
    //         self.disks.get_mut(dl as usize).and_then(|d| d.as_mut())
    //     }
    // }

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
