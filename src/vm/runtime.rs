use std::io::Cursor;
use byteorder::{LittleEndian, ReadBytesExt};
use crate::vm::memory::Memory;

const REGISTER_COUNT: usize = 14;

pub enum Register {
    AX = 0,
    CX = 1,
    DX = 2,
    BX = 3,
    SP = 4,
    BP = 5,
    SI = 6,
    DI = 7,
    ES = 8,
    CS = 9,
    SS = 10,
    DS = 11,
}

#[derive(Copy, Clone)]
pub struct Registers {
    pub ax: u16,
    pub bx: u16,
    pub cx: u16,
    pub dx: u16,
    pub si: u16,
    pub di: u16,
    pub sp: u16,
    pub bp: u16,
    pub ip: u16,
    pub cs: u16,
    pub ds: u16,
    pub es: u16,
    pub ss: u16,
    pub flags: u16
}

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

impl Registers {
    #[inline]
    pub fn set_flag(&mut self, flag: CpuFlag) {
        self.flags |= 1u16 << (flag as u8);
    }
    #[inline]
    pub fn unset_flag(&mut self, flag: CpuFlag) {
        self.flags &= !(1u16 << (flag as u8));
    }

    #[inline]
    pub fn update_flag(&mut self, flag: CpuFlag, active: bool) {
        if active {
            self.set_flag(flag);
        } else {
            self.unset_flag(flag);
        }
    }

    #[inline]
    pub fn check_flag(&self, flag: CpuFlag) -> bool {
        (self.flags & 1u16 << (flag as u8)) != 0
    }
}


pub struct Runtime<'vm> {
    pub registers: Registers,
    pub memory: Memory,
    cursor: Cursor<&'vm mut [u8]>
}

impl Runtime<'_> {
    pub fn fetch_byte(&mut self) -> u8 {
        self.cursor.read_u8().unwrap()
    }

    pub fn fetch_word(&mut self) -> u16 {
        self.cursor.read_u16::<LittleEndian>().unwrap()
    }
}