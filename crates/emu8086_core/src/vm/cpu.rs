//! The CPU state: the register file, the megabyte of guest memory, FLAGS,
//! the halt latch and the interrupt controller. It is a struct apart from
//! the `Runtime` because it is what the port bus hands a device: the bus is
//! borrowed while it dispatches, and the device gets the rest.
//!
//! The invariant: `registers` and `memory` are one unit. Each segment
//! register keeps a raw pointer to the `Memory` behind the box
//! (`Registers::new`), so the box is built first, handed to the registers,
//! and never replaced for as long as the `Cpu` exists.

use crate::io::pic::Pic;
use crate::vm::memory::{Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::{CpuFlag, SegmentType};

pub struct Cpu {
    pub registers: Registers,
    pub memory: Box<Memory>,
    pub flags: u16,
    pub halted: bool,
    pub pic: Pic,
}

impl Cpu {
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

    #[inline]
    pub fn get_segment(&mut self, segment: SegmentType) -> &mut Segment {
        match segment {
            SegmentType::ES => &mut self.registers.es,
            SegmentType::CS => &mut self.registers.cs,
            SegmentType::SS => &mut self.registers.ss,
            SegmentType::DS => &mut self.registers.ds,
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
}
