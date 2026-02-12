use std::fmt::{Debug, Formatter};
use std::ops::{Add, DerefMut};
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use crate::a_out::executable::Executable;
use crate::io::bus::IoBus;
use crate::io::disk::DiskImage;
use crate::io::pic::Pic;
use crate::vm::instructions::process;
use crate::vm::memory::{Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::CpuFlag::{Carry, Interrupt, Overflow, Sign, Zero};
use crate::vm::runtime::Prefix::Queued;

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
            _ => panic!("Unknown segment: {}", value)
        }
    }
}

pub enum Prefix {
    Rep(bool),
    Lock,
    Seg(SegmentType),
    Queued(Box<Prefix>),
}

#[derive(PartialEq, Clone, Copy)]
pub enum ExecutionMode {
    MinixAout,
    BiosBoot,
}

pub struct Runtime {
    pub registers: Registers,
    pub memory: Box<Memory>,
    pub flags: u16,
    running: bool,
    status: u16,
    pub prefix: Option<Prefix>,
    pub mode: ExecutionMode,
    pub io_bus: Option<IoBus>,
    pub bios_handlers: [Option<fn(&mut Runtime)>; 256],
    pub disk: Option<DiskImage>,
    pub pic: Option<Arc<Mutex<Pic>>>,
    pub instruction_count: u64,
    pub trace: bool,
    last_tick: Option<Instant>,
    last_refresh: Option<Instant>,
}

impl Runtime {
    pub fn new(exe: &Executable, args: Vec<String>) -> Self {
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

        registers.cs.copy_data(0, exe.text_segment.as_slice());
        registers.ds.copy_data(0, exe.data_segment.as_slice());

        let mut vm = Self {
            registers,
            memory,
            flags: 0,
            running: true,
            status: 0,
            prefix: None,
            mode: ExecutionMode::MinixAout,
            io_bus: None,
            bios_handlers: [None; 256],
            disk: None,
            pic: None,
            instruction_count: 0,
            trace: false,
            last_tick: None,
            last_refresh: None,
        };
        vm.set_flag(Interrupt);
        vm.init_args(args);
        vm
    }

    pub fn new_bios(disk_path: &Path) -> Self {
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new_bios(memory.deref_mut());

        let disk = DiskImage::open(disk_path).expect("Failed to open disk image");

        // Create shared PIC
        let pic = Arc::new(Mutex::new(Pic::new()));

        // Create I/O bus and register devices
        let mut io_bus = IoBus::new();
        io_bus.register(0x20, 0x21, Box::new(crate::io::pic::SharedPic::new(pic.clone())));
        io_bus.register(0x40, 0x43, Box::new(crate::io::pit::Pit::new()));
        io_bus.register(0x60, 0x64, Box::new(crate::io::keyboard::Keyboard::new()));
        io_bus.register(0x3D4, 0x3D5, Box::new(crate::io::vga::Vga::new()));

        let mut vm = Self {
            registers,
            memory,
            flags: 0,
            running: true,
            status: 0,
            prefix: None,
            mode: ExecutionMode::BiosBoot,
            io_bus: Some(io_bus),
            bios_handlers: [None; 256],
            disk: Some(disk),
            pic: Some(pic),
            instruction_count: 0,
            trace: false,
            last_tick: Some(Instant::now()),
            last_refresh: Some(Instant::now()),
        };

        vm.set_flag(Interrupt);
        crate::bios::init::init_bios(&mut vm);
        vm
    }

    fn init_args(&mut self, args: Vec<String>) {
        let mut argv: Vec<u16> = Vec::with_capacity(args.len());
        let env = "PATH=/usr:/usr/bin";

        self.push_byte(0);
        for c in env.bytes().rev() {
            self.push_byte(c);
        }
        let env_addr = self.registers.sp.word();
        // Push strings (in reverse order so argv[0] ends up at lowest address)
        for arg in args.iter().rev() {
            self.push_byte(0);
            for c in arg.bytes().rev() {
                self.push_byte(c);
            }
            argv.push(self.registers.sp.word());
        }

        // Word-align the stack before pushing pointers
        if self.registers.sp.word() & 1 != 0 {
            self.push_byte(0);
        }
        // Push envp = [env_addr, NULL]
        self.push_word(0);
        self.push_word(env_addr);
        // Push NULL ptr of argv
        self.push_word(0);
        // Push argv addresses (argv is in reverse order from string push)
        for address in argv.iter() {
            self.push_word(*address);
        }
        // Push argc
        self.push_word(argv.len() as u16);
    }

    pub fn exit(&mut self, status: u16) {
        self.running = false;
        self.status = status;
    }

    #[inline]
    pub fn fetch_byte(&mut self) -> u8 {
        let res = self.registers.cs.read_byte(self.registers.pc.word());
        self.registers.pc.operation(1, u16::add);
        res
    }

    #[inline]
    pub fn fetch_word(&mut self) -> u16 {
        let res = self.registers.cs.read_word(self.registers.pc.word());
        self.registers.pc.operation(2, u16::add);
        res
    }

    pub fn data_segment(&mut self) -> &mut Segment {
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.get_segment(*segment);
        }
        &mut self.registers.ds
    }

    #[inline]
    pub fn set_prefix(&mut self, prefix: Prefix) {
        self.prefix = Some(Queued(Box::new(prefix)));
    }

    #[inline]
    pub fn peek_byte(&self) -> u8 {
        self.registers.cs.read_byte(self.registers.pc.word())
    }

    #[inline]
    pub fn peek_word(&self) -> u16 {
        self.registers.cs.read_word(self.registers.pc.word())
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
        self.update_flag(flag, self.check_flag(flag));
    }

    #[inline(always)]
    pub fn check_flag(&self, flag: CpuFlag) -> bool {
        (self.flags & 1u16 << (flag as u8)) != 0
    }

    pub fn run(&mut self) {
        while self.running {
            if self.trace {
                eprintln!("{:?}", self);
            }

            // In BIOS mode: check for pending hardware interrupts
            if self.mode == ExecutionMode::BiosBoot && self.check_flag(Interrupt) {
                self.check_hw_interrupts();
            }

            process(self);
            self.instruction_count += 1;

            // In BIOS mode: periodic timer tick and VGA refresh
            if self.mode == ExecutionMode::BiosBoot && self.instruction_count % 1000 == 0 {
                self.check_timer_tick();
                self.check_vga_refresh();
            }
        }
    }

    fn check_hw_interrupts(&mut self) {
        let vector = if let Some(ref pic) = self.pic {
            pic.lock().unwrap().acknowledge()
        } else {
            None
        };

        if let Some(vector) = vector {
            let ivt_offset = vector as usize * 4;
            let new_ip = self.memory.read_word(ivt_offset);
            let new_cs = self.memory.read_word(ivt_offset + 2);
            if new_cs != 0 || new_ip != 0 {
                self.push_word(self.flags);
                self.unset_flag(Interrupt);
                self.unset_flag(CpuFlag::Trap);
                self.push_word(self.registers.cs.reg().word());
                self.push_word(self.registers.pc.word());
                self.registers.cs.reg_mut().set(new_cs);
                self.registers.pc.set(new_ip);
            }
        }
    }

    fn check_timer_tick(&mut self) {
        let should_tick = if let Some(ref last_tick) = self.last_tick {
            last_tick.elapsed().as_millis() >= 55
        } else {
            false
        };

        if should_tick {
            // Increment BDA tick counter at 0x0040:006C
            let tick_addr = crate::vm::memory::BDA_BASE + 0x6C;
            let tick_lo = self.memory.read_word(tick_addr);
            let tick_hi = self.memory.read_word(tick_addr + 2);
            let tick = ((tick_hi as u32) << 16) | tick_lo as u32;
            let new_tick = tick.wrapping_add(1);
            self.memory.write_word(tick_addr, new_tick as u16);
            self.memory.write_word(tick_addr + 2, (new_tick >> 16) as u16);

            // Raise IRQ0 via shared PIC
            if let Some(ref pic) = self.pic {
                pic.lock().unwrap().raise_irq(0);
            }

            self.last_tick = Some(Instant::now());
        }
    }

    fn check_vga_refresh(&mut self) {
        let should_refresh = if let Some(ref last_refresh) = self.last_refresh {
            last_refresh.elapsed().as_millis() >= 50
        } else {
            false
        };

        if should_refresh {
            self.last_refresh = Some(Instant::now());
        }
    }

    pub fn push_word(&mut self, word: u16) {
        let address = self.registers.sp.operation(2, u16::wrapping_sub);
        self.registers.ss.write_word(address, word);
    }

    pub fn push_byte(&mut self, byte: u8) {
        let address = self.registers.sp.operation(1, u16::wrapping_sub);
        self.registers.ss.write_byte(address, byte);
    }

    pub fn pop_word(&mut self) -> u16 {
        let address = self.registers.sp.word();
        self.registers.sp.operation(2, u16::wrapping_add);
        self.registers.ss.read_word(address)
    }

    pub fn pop_byte(&mut self) -> u8 {
        let address = self.registers.sp.word();
        self.registers.sp.operation(1, u16::wrapping_add);
        self.registers.ss.read_byte(address)
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

// AX   BX   CX   DX   SP   BP   SI   DI  FLAGS IP
// 0000 0000 0000 0000 ffdc 0000 0000 0000 ---- 0000:31ed

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
        write!(f, "{:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {}{}{}{} {:04x}:{:02x}{:02x}",
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

