use std::fmt::{Debug, Formatter};
use std::ops::{Add, DerefMut};

use crate::a_out::executable::Executable;
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

pub struct Runtime {
    pub registers: Registers,
    pub memory: Box<Memory>,
    pub flags: u16,
    running: bool,
    status: u16,
    pub prefix: Option<Prefix>,
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
        };
        vm.set_flag(Interrupt);
        vm.init_args(args);
        vm
    }

    fn init_args(&mut self, args: Vec<String>) {
        let mut argv: Vec<u16> = Vec::with_capacity(args.len());
        let env = "PATH=/usr:/usr/bin";

        println!("{:?}", args);
        self.push_byte(0);
        for c in env.bytes().rev() {
            self.push_byte(c);
        }
        let env_addr = self.registers.sp.word();
        // Push strings
        for arg in args.iter() {
            self.push_byte(0);
            for c in arg.bytes().rev() {
                self.push_byte(c);
            }
            argv.push(self.registers.sp.word());
        }

        // Push empty env
        self.push_word(0);
        self.push_word(env_addr);
        // Push NULL ptr of argv
        self.push_word(0);
        // Push argv addresses
        for address in argv.iter().rev() {
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
            println!("{:?}", self);
            process(self);
        }
    }

    pub fn push_word(&mut self, word: u16) {
        let address = self.registers.sp.operation(2, u16::wrapping_add);
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
            SegmentType::CS => &mut self.registers.es,
            SegmentType::SS => &mut self.registers.es,
            SegmentType::DS => &mut self.registers.es,
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
        write!(f, "{:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {}{}{}{} {:04x}:{:02x}",
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
                 self.registers.pc.word(),
                 self.peek_byte(),
        )?;
        Ok(())
    }
}

