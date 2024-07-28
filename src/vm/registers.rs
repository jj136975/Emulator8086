use std::fmt::{Debug, Formatter};

pub enum WordRegister {
    AX = 0,
    CX = 1,
    DX = 2,
    BX = 3,
    SP = 4,
    BP = 5,
    SI = 6,
    DI = 7,
}

pub enum ByteRegister {
    AL = 0,
    CL = 1,
    DL = 2,
    BL = 3,
    AH = 4,
    CH = 5,
    DH = 6,
    BH = 7,
}

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

    pub fn byte(&self) -> u8 {
        unsafe { *self.inner }
    }

    pub fn set(&self, byte: u8) {
        unsafe { *self.inner = byte }
    }
}

pub struct WordWrapper {
    inner: *mut [u8; 2],
}

impl WordWrapper {
    pub fn from_register(register: &mut Register) -> Self {
        // Self {
        //     inner: unsafe { &mut register.byte }
        // }
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
    pub fn ref_low(&self) -> &u8 {
        unsafe { &(*self.inner)[LOW_IDX] }
    }

    #[inline(always)]
    pub fn ref_high(&self) -> &u8 {
        unsafe { &(*self.inner)[HIGH_IDX] }
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
}

#[cfg(target_endian = "big")]
const LOW_IDX: usize = 1;
#[cfg(target_endian = "little")]
const LOW_IDX: usize = 0;

#[cfg(target_endian = "big")]
const HIGH_IDX: usize = 0;
#[cfg(target_endian = "little")]
const HIGH_IDX: usize = 1;

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
    pub fn ref_low(&self) -> &u8 {
        unsafe { &self.byte[LOW_IDX] }
    }

    #[inline(always)]
    pub fn ref_high(&self) -> &u8 {
        unsafe { &self.byte[HIGH_IDX] }
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
}

#[derive(Default)]
pub struct Registers {
    pub ax: Register,
    pub bx: Register,
    pub cx: Register,
    pub dx: Register,
    pub si: Register,
    pub di: Register,
    pub sp: Register,
    pub bp: Register,
    pub ip: Register,
    pub cs: Register,
    pub ds: Register,
    pub es: Register,
    pub ss: Register,
}

impl Debug for Registers {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "============= Registers ===============")?;
        writeln!(f, " AX    | BX    | CX    | DX")?;
        unsafe {
            writeln!(f,
                     " {:#04X}  | {:#04X}  | {:#04X}  | {:#04X}",
                     self.ax.word,
                     self.bx.word,
                     self.cx.word,
                     self.dx.word,
            )?;
            writeln!(f, "============= Positional ==============")?;
            writeln!(f, " SI    | DI    | SP    | BP    | IP")?;
            writeln!(f,
                     " {:#04X}  | {:#04X}  | {:#04X}  | {:#04X}  | {:#04X}",
                     self.si.word,
                     self.di.word,
                     self.sp.word,
                     self.bp.word,
                     self.ip.word,
            )?;
            writeln!(f, "============= Segments ================")?;
            writeln!(f, " CS    | DS    | ES    | SS")?;
            writeln!(f,
                     " {:#04X}  | {:#04X}  | {:#04X}  | {:#04X}",
                     self.cs.word,
                     self.ds.word,
                     self.es.word,
                     self.ss.word,
            )?;
        }
        Ok(())
    }
}