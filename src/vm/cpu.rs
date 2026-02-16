use crate::io::pic::Pic;
use crate::vm::memory::{Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::{CpuFlag, SegmentType};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CpuType {
    Intel8086,
    Intel80186,
}

pub struct Cpu {
    pub cpu_type: CpuType,
    pub registers: Registers,
    pub memory: Box<Memory>,
    pub flags: u16,
    pub halted: bool,
    pub pic: Pic,
}

impl Cpu {
    #[inline]
    pub fn is_186(&self) -> bool {
        self.cpu_type == CpuType::Intel80186
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