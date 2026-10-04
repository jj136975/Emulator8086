//! The register file and the operand references the instructions work
//! through: `Register` is a 16-bit register with its two byte halves,
//! `Registers` the 8086's set of them, and `ByteWrapper`/`WordWrapper` name
//! an operand wherever it lives, in a register or in guest memory.
//!
//! The invariant: a wrapper is a raw pointer without a lifetime, so that an
//! instruction can take one for its destination, go on using `&mut Runtime`
//! (fetch an immediate, set the flags) and store through it last. Nothing
//! checks that. It holds because the bytes never move (registers are fields
//! of the `Runtime` that `process` borrows, memory is one boxed array), the
//! machine runs on one thread, and callers keep two rules: a wrapper is
//! never stored (an instruction makes it, uses it and drops it), and until
//! its last use the bytes behind it are written through wrappers only, never
//! through `vm`. Rust's aliasing rules for `&mut` do not sanction this
//! (Stacked Borrows rejects it): the tests hold it up, not the compiler.

use crate::vm::memory::{Memory, Segment};

/// A 16-bit register, readable as a word or as its two bytes. Both fields
/// are plain integers over the same two bytes and a `Register` is always
/// built from a whole word, so either view is initialized and valid at all
/// times: every `unsafe` access to a field below rests on that.
#[repr(C)]
pub union Register {
    word: u16,
    byte: [u8; 2],
}

impl Register {
    pub fn new(word: u16) -> Self {
        Self { word }
    }
}

impl Default for Register {
    fn default() -> Self {
        Self { word: 0u16 }
    }
}

/// An 8-bit operand: a pointer to a register half or to a byte of guest
/// memory, under the wrapper contract of the module header.
pub struct ByteWrapper {
    inner: *mut u8,
}

impl ByteWrapper {
    /// Forgets the borrow: from here on the wrapper contract, not the borrow
    /// checker, keeps the pointer valid.
    pub fn new(byte: &mut u8) -> Self {
        Self { inner: byte }
    }

    #[inline(always)]
    pub fn byte(&self) -> u8 {
        // SAFETY: wrapper contract: `inner` points at a `u8` of the register
        // file, of guest memory or of its ROM sink, which stays live for as
        // long as a wrapper may be in use.
        unsafe { *self.inner }
    }

    #[inline(always)]
    pub fn set(&self, byte: u8) {
        // SAFETY: wrapper contract, as for `byte`; nothing but wrappers
        // writes the pointee while this one is in use.
        unsafe { *self.inner = byte }
    }

    #[inline]
    pub fn swap(&mut self, other: &mut ByteWrapper) {
        // SAFETY: both pointers are valid under the wrapper contract, and
        // `ptr::swap` allows them to be the same byte (XCHG AL, AL).
        unsafe { core::ptr::swap(self.inner, other.inner) };
    }
}

/// A 16-bit operand addressed as two independent byte pointers, so that a
/// word whose second byte wraps (segment offset FFFFh -> 0000h, or physical
/// FFFFFh -> 00000h) reads and writes the right guest bytes.
pub struct WordWrapper {
    lo: *mut u8,
    hi: *mut u8,
}

impl WordWrapper {
    /// Forgets the borrow, like `ByteWrapper::new`.
    pub fn from_register(register: &mut Register) -> Self {
        // SAFETY: either view of the union is always valid (see `Register`).
        let bytes = unsafe { register.byte.as_mut_ptr() };
        Self {
            // SAFETY: `LOW_IDX` is 0 or 1, inside the two bytes of the array.
            lo: unsafe { bytes.add(LOW_IDX) },
            // SAFETY: `HIGH_IDX` is the other index of the same array.
            hi: unsafe { bytes.add(HIGH_IDX) },
        }
    }

    /// # Safety
    /// `lo` and `hi` must each point at a byte that stays valid for reads and
    /// writes for as long as the wrapper is used.
    pub unsafe fn from_parts(lo: *mut u8, hi: *mut u8) -> Self {
        Self { lo, hi }
    }

    #[inline(always)]
    pub fn word(&self) -> u16 {
        // SAFETY: wrapper contract: `lo` and `hi` each point at a `u8` of the
        // register file, of guest memory or of its ROM sink, which stays
        // live for as long as a wrapper may be in use.
        u16::from_le_bytes(unsafe { [*self.lo, *self.hi] })
    }

    #[inline(always)]
    pub fn set(&self, word: u16) {
        let bytes = word.to_le_bytes();
        // SAFETY: wrapper contract, as for `word`; nothing but wrappers
        // writes the pointees while this one is in use.
        unsafe { *self.lo = bytes[0] };
        // SAFETY: as for the low byte.
        unsafe { *self.hi = bytes[1] };
    }

    #[inline]
    pub fn swap(&mut self, other: &mut WordWrapper) {
        let mine = self.word();
        self.set(other.word());
        other.set(mine);
    }

    /// Exchanges the operand with `register`, which must not be the register
    /// this wrapper points at: it is written through its own reference.
    #[inline]
    pub fn swap_register(&mut self, register: &mut Register) {
        let mine = self.word();
        self.set(register.word());
        register.set(mine);
    }
}

/// Access to an operand of width `T` through its wrapper, for code that is
/// written once for both widths.
pub trait OperandRef<T> {
    fn get(&self) -> T;
    fn set(&self, value: T);
}

impl OperandRef<u8> for ByteWrapper {
    #[inline(always)]
    fn get(&self) -> u8 {
        self.byte()
    }

    #[inline(always)]
    fn set(&self, value: u8) {
        ByteWrapper::set(self, value)
    }
}

impl OperandRef<u16> for WordWrapper {
    #[inline(always)]
    fn get(&self) -> u16 {
        self.word()
    }

    #[inline(always)]
    fn set(&self, value: u16) {
        WordWrapper::set(self, value)
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
        // SAFETY: either view of the union is always valid (see `Register`).
        unsafe { self.word }
    }

    #[inline(always)]
    pub fn low(&self) -> u8 {
        // SAFETY: either view of the union is always valid (see `Register`).
        unsafe { self.byte[LOW_IDX] }
    }

    #[inline(always)]
    pub fn high(&self) -> u8 {
        // SAFETY: either view of the union is always valid (see `Register`).
        unsafe { self.byte[HIGH_IDX] }
    }

    #[inline(always)]
    pub fn ref_mut_low(&mut self) -> ByteWrapper {
        // SAFETY: either view of the union is always valid (see `Register`).
        ByteWrapper::new(unsafe { &mut self.byte[LOW_IDX] })
    }

    #[inline(always)]
    pub fn ref_mut_high(&mut self) -> ByteWrapper {
        // SAFETY: either view of the union is always valid (see `Register`).
        ByteWrapper::new(unsafe { &mut self.byte[HIGH_IDX] })
    }

    #[inline(always)]
    pub fn set(&mut self, word: u16) {
        self.word = word;
    }

    #[inline(always)]
    pub fn set_low(&mut self, byte: u8) {
        // SAFETY: either view of the union is always valid (see `Register`).
        unsafe { self.byte[LOW_IDX] = byte };
    }

    #[inline(always)]
    pub fn set_high(&mut self, byte: u8) {
        // SAFETY: either view of the union is always valid (see `Register`).
        unsafe { self.byte[HIGH_IDX] = byte };
    }

    #[inline(always)]
    pub fn operation<T>(&mut self, value: T, operation: fn(u16, T) -> u16) -> u16 {
        let res = operation(self.word(), value);
        self.set(res);
        res
    }

    #[inline(always)]
    pub fn operation_low<T>(&mut self, value: T, operation: fn(u8, T) -> u8) -> u8 {
        let res = operation(self.low(), value);
        self.set_low(res);
        res
    }

    #[inline(always)]
    pub fn apply_low<T, U>(&self, value: U, operation: fn(u8, U) -> T) -> T {
        operation(self.low(), value)
    }

    #[inline(always)]
    pub fn operation_high<T>(&mut self, value: T, operation: fn(u8, T) -> u8) -> u8 {
        let res = operation(self.high(), value);
        self.set_high(res);
        res
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
    /// IP of the first byte of the current instruction sequence, including
    /// any prefix bytes. Used by interruptible REP-string ops to rewind the
    /// PC when breaking out for a pending IRQ.
    pub op_start_pc: u16,
}

impl Registers {
    /// Creates registers in 8086 power-on reset state.
    /// CS=0xFFFF, IP=0x0000 -> execution starts at physical 0xFFFF0.
    /// All other registers are 0x0000.
    /// The BIOS is responsible for setting up the stack and segments.
    ///
    /// `memory` is what the four segment registers will address: it must
    /// point at a `Memory` that stays where it is for as long as these
    /// registers are used (see `Segment::new`).
    pub fn new(memory: *mut Memory) -> Self {
        Self {
            ax: Register::default(),
            bx: Register::default(),
            cx: Register::default(),
            dx: Register::default(),
            si: Register::default(),
            di: Register::default(),
            sp: Register::default(), // BIOS sets up stack
            bp: Register::default(),
            cs: Segment::new(0xFFFF, memory), // reset vector segment
            ds: Segment::new(0x0000, memory),
            es: Segment::new(0x0000, memory),
            ss: Segment::new(0x0000, memory),
            pc: Register::default(), // IP = 0x0000
            op_start_pc: 0,
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
            _ => unreachable!(),
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
            _ => unreachable!(),
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
            _ => unreachable!(),
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
            _ => unreachable!(),
        }
    }
}
