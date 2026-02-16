use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::io::Write;
use std::ops::DerefMut;
use std::path::PathBuf;
use std::rc::Rc;
use crate::io::bus::IoBus;
use crate::io::console::{DebugConsole, DiskTrap, PitStub};
use crate::io::disk::{DiskImage, DiskSource, DiskSpec, HD_DEFAULT_SIZE_MB};
use crate::io::pic::{Pic, PicDevice};
use crate::vm::instructions::process;
use crate::vm::memory::{Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::CpuFlag::{Carry, Interrupt, Overflow, Sign, Trap, Zero};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CpuType {
    Intel8086,
    Intel80186,
}

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

pub struct Runtime {
    pub cpu_type: CpuType,
    pub registers: Registers,
    pub memory: Box<Memory>,
    pub flags: u16,
    pub halted: bool,
    running: bool,
    status: u16,
    pub prefix: Option<Prefix>,
    pub segment_override: Option<SegmentType>,
    pub io_bus: Option<IoBus>,
    pub disks: Vec<Option<DiskImage>>,
    pub hard_disks: Vec<Option<DiskImage>>,
    pub pic: Rc<RefCell<Pic>>,
    pub boot_order: Vec<u8>,
    pub instruction_count: u64,
    pub trace: bool,
    pub interrupt_inhibit: bool,
}

impl Runtime {
    #[cfg(test)]
    pub fn new_test() -> Self {
        use std::ops::DerefMut;
        let mut memory = Box::new(Memory::new());



        let registers = Registers::new(memory.deref_mut());
        Self {
            cpu_type: CpuType::Intel8086,
            registers,
            memory,
            flags: 0,
            halted: false,
            running: true,
            status: 0,
            prefix: None,
            segment_override: None,
            io_bus: None,
            disks: Vec::new(),
            hard_disks: Vec::new(),
            pic: Rc::new(RefCell::new(Pic::new())),
            boot_order: Vec::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,
        }
    }

    /// Test constructor with PIC, IoBus, and keyboard shared state.
    /// Does NOT start input thread, raw mode, or BIOS init.
    #[cfg(test)]
    pub fn new_bios_test() -> Self {
        use std::ops::DerefMut;
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

        let pic = Rc::new(RefCell::new(Pic::new()));

        // let keyboard = crate::io::keyboard::Keyboard::new();
        // let kb_data_avail = keyboard.shared_data_available();

        let mut io_bus = IoBus::new();
        io_bus.register(0x20, 0x21, Box::new(PicDevice::new(Rc::clone(&pic))));

        Self {
            cpu_type: CpuType::Intel8086,
            registers,
            memory,
            flags: 0,
            running: true,
            halted: false,
            status: 0,
            prefix: None,
            segment_override: None,
            io_bus: Some(io_bus),
            disks: Vec::new(),
            hard_disks: Vec::new(),
            pic,
            boot_order: Vec::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,
        }
    }

    pub fn new(disk_specs: Vec<DiskSpec>, hd_specs: Vec<String>, boot_order: Option<Vec<u8>>) -> Self {
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

        // Build disks vec — size to fit the highest requested drive index
        let max_drive = disk_specs.iter().map(|s| s.drive).max().unwrap_or(0) as usize;
        let mut disks: Vec<Option<DiskImage>> = (0..=max_drive).map(|_| None).collect();
        for spec in disk_specs {
            let image = match &spec.source {
                DiskSource::FilePath(path) => {
                    if spec.drive == 0 {
                        // Boot drive: must exist
                        DiskImage::open(path).expect("Failed to open disk image")
                    } else {
                        // Non-boot drive: create if doesn't exist
                        DiskImage::open_or_create(path).expect("Failed to open/create disk image")
                    }
                }
                DiskSource::Memory(size) => {
                    let mut img = DiskImage::new_in_memory_sized(*size);
                    if let Err(e) = img.format_fat() {
                        eprintln!("Warning: could not format floppy: {}", e);
                    }
                    img
                }
            };
            disks[spec.drive as usize] = Some(image);
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

        // Create shared PIC
        let pic = Rc::new(RefCell::new(Pic::new()));

        // Create I/O bus and register devices
        let mut io_bus = IoBus::new();

        io_bus.register(0x20, 0x21, Box::new(PicDevice::new(Rc::clone(&pic))));
        io_bus.register(0x40, 0x43, Box::new(PitStub));
        io_bus.register(0xB0, 0xB0, Box::new(DiskTrap));
        io_bus.register(0xE9, 0xE9, Box::new(DebugConsole));

        let mut vm = Self {
            cpu_type: CpuType::Intel80186,
            registers,
            memory,
            flags: 0,
            halted: false,
            running: true,
            status: 0,
            prefix: None,
            segment_override: None,
            io_bus: Some(io_bus),
            disks,
            hard_disks,
            pic,
            boot_order: boot_order.unwrap_or_else(|| vec![0x00, 0x80]),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,
        };

        vm.set_flag(Interrupt);
        // crate::bios::init::init_bios(&mut vm);
        vm
    }

    #[inline]
    pub fn is_186(&self) -> bool {
        self.cpu_type == CpuType::Intel80186
    }

    pub fn exit(&mut self, status: u16) {
        self.running = false;
        self.status = status;
    }

    #[inline]
    pub fn fetch_byte(&mut self) -> u8 {
        let res = self.registers.cs.read_byte(self.registers.pc.word());
        self.registers.pc.operation(1, u16::wrapping_add);
        res
    }

    #[inline]
    pub fn fetch_word(&mut self) -> u16 {
        let res = self.registers.cs.read_word(self.registers.pc.word());
        self.registers.pc.operation(2, u16::wrapping_add);
        res
    }

    pub fn data_segment(&mut self) -> &mut Segment {
        if let Some(segment) = self.segment_override {
            return self.get_segment(segment);
        }
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.get_segment(*segment);
        }
        &mut self.registers.ds
    }

    /// Picks the correct default segment for a ModR/M memory operand.
    /// BP-based addressing (rm=010, 011, or rm=110 with mod!=00) defaults to SS.
    /// All other modes default to DS. An explicit segment override prefix wins.
    pub fn effective_segment(&mut self, mod_val: u8, rm: u8) -> &mut Segment {
        if let Some(segment) = self.segment_override {
            return self.get_segment(segment);
        }
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.get_segment(*segment);
        }
        let bp_based = match rm {
            0b010 | 0b011 => true,
            0b110 => mod_val != 0b00,
            _ => false,
        };
        if bp_based {
            &mut self.registers.ss
        } else {
            &mut self.registers.ds
        }
    }

    #[inline(always)]
    pub fn set_flag(&mut self, flag: CpuFlag) {
        self.flags |= 1u16 << (flag as u8);
    }
    #[inline(always)]
    pub fn unset_flag(&mut self, flag: CpuFlag) {
        self.flags &= !(1u16 << (flag as u8));
    }

    #[inline(always)]
    pub fn update_flag(&mut self, flag: CpuFlag, active: bool) {
        if active {
            self.set_flag(flag);
        } else {
            self.unset_flag(flag);
        }
    }

    #[inline(always)]
    pub fn flip_flag(&mut self, flag: CpuFlag) {
        self.update_flag(flag, !self.check_flag(flag));
    }

    #[inline(always)]
    pub fn check_flag(&self, flag: CpuFlag) -> bool {
        (self.flags & 1u16 << (flag as u8)) != 0
    }

    pub(crate) fn load_rom(&mut self, path: PathBuf) {
        let rom_data = std::fs::read(path).expect("Failed to read ROM file");
        if rom_data.len() > 0x10000 {
            panic!("ROM file too large (max 64KB)");
        }

        let load_address = 0xF0000;
        for (i, byte) in rom_data.iter().enumerate() {
            self.memory.write_byte(load_address + i, *byte);
        }
    }

    pub fn run(&mut self) {
        // In BIOS mode: enable raw terminal and clear screen for VGA rendering
        let _ = crossterm::terminal::enable_raw_mode();

        let _ = write!(std::io::stdout(), "\x1B[2J\x1B[H");
        let _ = std::io::stdout().flush();

        while self.running {
            if self.trace {
                eprintln!("{:?}", self);
            }

            if !self.halted {
                process(self);
                self.instruction_count += 1;
            }

            if self.pic.borrow().has_interrupt() {
                let vector = self.pic.borrow_mut().acknowledge();
                self.halted = false;
                self.handle_interrupt(vector);
            }
        }
        let _ = crossterm::terminal::disable_raw_mode();

        let _ = write!(std::io::stdout(), "\x1B[0m\x1B[?25h\n");
        let _ = std::io::stdout().flush();
    }

    pub(crate) fn handle_interrupt(&mut self, vector: u8) {
        let addr = (vector as usize) * 4;

        // Push flags with 8086 fixed bits applied
        let flags = (self.flags & 0x0FD5) | 0xF002;
        self.push_word(flags);
        self.push_word(self.registers.cs.reg().word());
        self.push_word(self.registers.pc.word());

        // Clear IF and TF
        self.unset_flag(Interrupt);
        self.unset_flag(Trap);

        // Load new CS:IP from IVT
        self.registers.pc.set(self.memory.read_word(addr));
        self.registers
            .cs
            .reg_mut()
            .set(self.memory.read_word(addr + 2));
    }

    /// Check if the F12 monitor flag is set.
    // pub fn check_monitor_flag(&self) -> bool {
    //     self.monitor_flag
    //         .as_ref()
    //         .map_or(false, |f| f.load(Ordering::SeqCst))
    // }

    /// Look up a disk by BIOS drive number (DL < 0x80 = floppy, DL >= 0x80 = hard disk).
    pub fn get_disk(&self, dl: u8) -> Option<&DiskImage> {
        if dl >= 0x80 {
            self.hard_disks
                .get((dl - 0x80) as usize)
                .and_then(|d| d.as_ref())
        } else {
            self.disks.get(dl as usize).and_then(|d| d.as_ref())
        }
    }

    pub fn get_disk_mut(&mut self, dl: u8) -> Option<&mut DiskImage> {
        if dl >= 0x80 {
            self.hard_disks
                .get_mut((dl - 0x80) as usize)
                .and_then(|d| d.as_mut())
        } else {
            self.disks.get_mut(dl as usize).and_then(|d| d.as_mut())
        }
    }

    pub fn push_word(&mut self, word: u16) {
        let address = self.registers.sp.operation(2, u16::wrapping_sub);
        self.registers.ss.write_word(address, word);
    }

    pub fn pop_word(&mut self) -> u16 {
        let address = self.registers.sp.word();
        self.registers.sp.operation(2, u16::wrapping_add);
        self.registers.ss.read_word(address)
    }

    pub fn get_segment(&mut self, segment: SegmentType) -> &mut Segment {
        match segment {
            SegmentType::ES => &mut self.registers.es,
            SegmentType::CS => &mut self.registers.cs,
            SegmentType::SS => &mut self.registers.ss,
            SegmentType::DS => &mut self.registers.ds,
        }
    }
}

#[inline(always)]
fn show_flag(vm: &Runtime, flag: CpuFlag, c: char) -> char {
    if vm.check_flag(flag) {
        return c;
    }
    '-'
}

impl Debug for Runtime {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pc = self.registers.pc.word();
        write!(
            f,
            "{:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {}{}{}{} {:04x}:{:02x}{:02x}",
            self.registers.ax.word(),
            self.registers.bx.word(),
            self.registers.cx.word(),
            self.registers.dx.word(),
            self.registers.sp.word(),
            self.registers.bp.word(),
            self.registers.si.word(),
            self.registers.di.word(),
            show_flag(self, Overflow, 'O'),
            show_flag(self, Sign, 'S'),
            show_flag(self, Zero, 'Z'),
            show_flag(self, Carry, 'C'),
            pc,
            self.registers.cs.read_byte(pc),
            self.registers.cs.read_byte(pc.wrapping_add(1)),
        )?;
        Ok(())
    }
}
