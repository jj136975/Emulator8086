use std::ops::Add;
use crate::vm::memory::{Memory, Segment, SEGMENT_SIZE};

#[repr(C)]
pub union Register {
    word: u16,
    byte: [u8; 2],
}

impl Register {
    pub fn new(word: u16) -> Self {
        Self {
            word
        }
    }
}

impl Default for Register {
    fn default() -> Self {
        Self {
            word: 0u16
        }
    }
}

pub struct ByteWrapper {
    inner: *mut u8,
}

impl ByteWrapper {
    pub fn new(byte: &mut u8) -> Self {
        Self {
            inner: byte
        }
    }

    #[inline(always)]
    pub fn byte(&self) -> u8 {
        unsafe { *self.inner }
    }

    #[inline(always)]
    pub fn set(&self, byte: u8) {
        unsafe { *self.inner = byte }
    }

    #[inline(always)]
    pub fn operation(&self, value: u8, operation: fn (u8, u8) -> u8) -> u8 {
        let res = operation( unsafe { *self.inner }, value);
        unsafe { *self.inner = res };
        res
    }

    #[inline(always)]
    pub fn apply<T>(&self, value: u8, operation: fn (u8, u8) -> T) -> T {
        operation( unsafe { *self.inner }, value)
    }

    #[inline]
    pub fn swap(&mut self, other: &mut ByteWrapper) {
        unsafe { core::ptr::swap(self.inner, other.inner) };
    }
}

pub struct WordWrapper {
    inner: *mut [u8; 2],
}

impl WordWrapper {
    pub fn from_register(register: &mut Register) -> Self {
        Self {
            inner: unsafe { &mut register.byte }
        }
    }

    pub fn from_slice(slice: &mut [u8; 2]) -> Self {
        Self {
            inner: slice
        }
    }

    #[inline(always)]
    pub fn word(&self) -> u16 {
        u16::from_le_bytes(unsafe { *self.inner })
    }

    #[inline(always)]
    pub fn low(&self) -> u8 {
        unsafe { (*self.inner)[LOW_IDX] }
    }

    #[inline(always)]
    pub fn high(&self) -> u8 {
        unsafe { (*self.inner)[HIGH_IDX] }
    }

    #[inline(always)]
    pub fn ref_mut_low(&mut self) -> &mut u8 {
        unsafe { &mut (*self.inner)[LOW_IDX] }
    }

    #[inline(always)]
    pub fn ref_mut_high(&mut self) -> &mut u8 {
        unsafe { &mut (*self.inner)[HIGH_IDX] }
    }

    #[inline(always)]
    pub fn set(&self, word: u16) {
        let bytes = word.to_le_bytes();
        unsafe { (*self.inner)[0] = bytes[0] };
        unsafe { (*self.inner)[1] = bytes[1] };
    }

    #[inline(always)]
    pub fn set_low(&self, byte: u8) {
        unsafe { (*self.inner)[LOW_IDX] = byte };
    }

    #[inline(always)]
    pub fn set_high(&self, byte: u8) {
        unsafe { (*self.inner)[HIGH_IDX] = byte };
    }

    #[inline(always)]
    pub fn operation<T>(&self, value: T, operation: fn (u16, T) -> u16) -> u16 {
        let res = operation(self.word(), value);
        self.set(res);
        res
    }

    #[inline(always)]
    pub fn apply<T, U>(&self, value: U, operation: fn (u16, U) -> T) -> T {
        operation(self.word(), value)
    }

    #[inline(always)]
    pub fn operation_low<T>(&self, value: T, operation: fn (u8, T) -> u8) -> u8 {
        let res = operation(self.low(), value);
        self.set_low(res);
        res
    }

    #[inline(always)]
    pub fn apply_low<T, U>(&self, value: U, operation: fn (u8, U) -> T) -> T {
        operation(self.low(), value)
    }

    #[inline(always)]
    pub fn operation_high<T>(&self, value: T, operation: fn (u8, T) -> u8) -> u8 {
        let res = operation(self.high(), value);
        self.set_high(res);
        res
    }

    #[inline(always)]
    pub fn apply_high<T, U>(&self, value: U, operation: fn (u8, U) -> T) -> T {
        operation(self.high(), value)
    }

    #[inline]
    pub fn swap(&mut self, other: &mut WordWrapper) {
        unsafe { core::ptr::swap(self.inner, other.inner) };
    }

    #[inline]
    pub fn swap_register(&mut self, register: &mut Register) {
        unsafe { core::ptr::swap(self.inner as *mut u16, &mut register.word) };
    }

    pub unsafe fn next(&self) -> u16 {
        unsafe { *(self.inner as *mut u16).add(2) }
    }
}

#[cfg(target_endian = "big")]
pub const LOW_IDX: usize = 1;
#[cfg(target_endian = "little")]
pub const LOW_IDX: usize = 0;

#[cfg(target_endian = "big")]
pub const HIGH_IDX: usize = 0;
#[cfg(target_endian = "little")]
pub const HIGH_IDX: usize = 1;

impl Register {
    #[inline(always)]
    pub fn word(&self) -> u16 {
        unsafe { self.word }
    }

    #[inline(always)]
    pub fn low(&self) -> u8 {
        unsafe { self.byte[LOW_IDX] }
    }

    #[inline(always)]
    pub fn high(&self) -> u8 {
        unsafe { self.byte[HIGH_IDX] }
    }

    #[inline(always)]
    pub fn ref_mut_low(&mut self) -> ByteWrapper {
        ByteWrapper::new(unsafe { &mut self.byte[LOW_IDX] })
    }

    #[inline(always)]
    pub fn ref_mut_high(&mut self) -> ByteWrapper {
        ByteWrapper::new(unsafe { &mut self.byte[HIGH_IDX] })
    }

    #[inline(always)]
    pub fn set(&mut self, word: u16) {
        self.word = word;
    }

    #[inline(always)]
    pub fn set_low(&mut self, byte: u8) {
        unsafe { self.byte[LOW_IDX] = byte };
    }

    #[inline(always)]
    pub fn set_high(&mut self, byte: u8) {
        unsafe { self.byte[HIGH_IDX] = byte };
    }

    #[inline]
    pub fn as_wrapper(&mut self) -> WordWrapper {
        WordWrapper::from_register(self)
    }

    #[inline(always)]
    pub fn operation<T>(&mut self, value: T, operation: fn (u16, T) -> u16) -> u16 {
        let res = operation(self.word(), value);
        self.set(res);
        res
    }

    #[inline(always)]
    pub fn apply<T, U>(&self, value: U, operation: fn (u16, U) -> T) -> T {
        operation(self.word(), value)
    }

    #[inline(always)]
    pub fn operation_low<T>(&mut self, value: T, operation: fn (u8, T) -> u8) -> u8 {
        let res = operation(self.low(), value);
        self.set_low(res);
        res
    }

    #[inline(always)]
    pub fn apply_low<T, U>(&self, value: U, operation: fn (u8, U) -> T) -> T {
        operation(self.low(), value)
    }

    #[inline(always)]
    pub fn operation_high<T>(&mut self, value: T, operation: fn (u8, T) -> u8) -> u8 {
        let res = operation(self.high(), value);
        self.set_high(res);
        res
    }

    #[inline(always)]
    pub fn apply_high<T, U>(&self, value: U, operation: fn (u8, U) -> T) -> T {
        operation(self.high(), value)
    }
}

pub struct Registers {
    pub ax: Register,
    pub bx: Register,
    pub cx: Register,
    pub dx: Register,
    pub si: Register,
    pub di: Register,
    pub sp: Register,
    pub bp: Register,
    pub cs: Segment,
    pub ds: Segment,
    pub es: Segment,
    pub ss: Segment,
    pub pc: Register,
    pub op_pc: u16
}

impl  Registers {
    pub fn new(memory: &mut Memory) -> Self {
        Self {
            ax: Register::default(),
            bx: Register::default(),
            cx: Register::default(),
            dx: Register::default(),
            si: Register::default(),
            di: Register::default(),
            sp: Register::new(0xFFFF),
            bp: Register::default(),
            cs: Segment::new((SEGMENT_SIZE >> 4) as u16, memory),
            ds: Segment::new(((SEGMENT_SIZE * 2) >> 4) as u16, memory),
            es: Segment::new(((SEGMENT_SIZE * 3) >> 4) as u16, memory),
            ss: Segment::new(((SEGMENT_SIZE * 2) >> 4) as u16, memory),  // SS == DS in MINIX separate I&D
            pc: Register::default(),
            op_pc: 0,
        }
    }

    #[inline(always)]
    pub fn ref_reg_byte(&mut self, reg: u8) -> ByteWrapper {
        match reg {
            0b000 => self.ax.ref_mut_low(),
            0b001 => self.cx.ref_mut_low(),
            0b010 => self.dx.ref_mut_low(),
            0b011 => self.bx.ref_mut_low(),
            0b100 => self.ax.ref_mut_high(),
            0b101 => self.cx.ref_mut_high(),
            0b110 => self.dx.ref_mut_high(),
            0b111 => self.bx.ref_mut_high(),
            _ => unreachable!()
        }
    }

    #[inline(always)]
    pub fn read_reg_byte(&self, reg: u8) -> u8 {
        match reg {
            0b000 => self.ax.low(),
            0b001 => self.cx.low(),
            0b010 => self.dx.low(),
            0b011 => self.bx.low(),
            0b100 => self.ax.high(),
            0b101 => self.cx.high(),
            0b110 => self.dx.high(),
            0b111 => self.bx.high(),
            _ => unreachable!()
        }
    }

    #[inline(always)]
    pub fn ref_reg_word(&mut self, reg: u8) -> WordWrapper {
        WordWrapper::from_register(match reg {
            0b000 => &mut self.ax,
            0b001 => &mut self.cx,
            0b010 => &mut self.dx,
            0b011 => &mut self.bx,
            0b100 => &mut self.sp,
            0b101 => &mut self.bp,
            0b110 => &mut self.si,
            0b111 => &mut self.di,
            _ => unreachable!()
        })
    }

    #[inline(always)]
    pub fn read_reg_word(&self, reg: u8) -> u16 {
        match reg {
            0b000 => self.ax.word(),
            0b001 => self.cx.word(),
            0b010 => self.dx.word(),
            0b011 => self.bx.word(),
            0b100 => self.sp.word(),
            0b101 => self.bp.word(),
            0b110 => self.si.word(),
            0b111 => self.di.word(),
            _ => unreachable!()
        }
    }
}
