use std::fmt::{Debug, Formatter};
use std::ops::{Add, DerefMut};

use crate::a_out::executable::Executable;
use crate::vm::instructions::process;
use crate::vm::memory::{Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::CpuFlag::{AuxCarry, Carry, Directional, Interrupt, Overflow, Parity, Sign, Trap, Zero};
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
    Queued(Box<Prefix>)
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
    pub fn new(exe: &Executable) -> Self {
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
            prefix: None
        };
        vm.set_flag(Interrupt);
        vm
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
    pub fn peek_byte(&mut self) -> u8 {
        self.registers.cs.read_byte(self.registers.pc.word())
    }

    #[inline]
    pub fn peek_word(&mut self) -> u16 {
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
        self.registers.ss.write_word(self.registers.sp, word);
        self.registers.sp += 2;
    }

    pub fn push_byte(&mut self, byte: u8) {
        self.registers.ss.write_byte(self.registers.sp, byte);
        self.registers.sp += 1;
    }

    pub fn pop_word(&mut self) -> u16 {
        self.registers.sp -= 2;
        self.registers.ss.read_word(self.registers.sp)
    }

    pub fn pop_byte(&mut self) -> u8 {
        self.registers.sp -= 1;
        self.registers.ss.read_byte(self.registers.sp)
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

impl Debug for Runtime {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.registers)?;
        writeln!(f, "================ FLAGS ===================")?;
        writeln!(f, " C  | P  | AC | Z  | S  | T  | I  | D  | O")?;
        writeln!(f,
                 " {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}",
                 self.check_flag(Carry) as u8,
                 self.check_flag(Parity) as u8,
                 self.check_flag(AuxCarry) as u8,
                 self.check_flag(Zero) as u8,
                 self.check_flag(Sign) as u8,
                 self.check_flag(Trap) as u8,
                 self.check_flag(Interrupt) as u8,
                 self.check_flag(Directional) as u8,
                 self.check_flag(Overflow) as u8,
        )?;
        Ok(())
    }
}

