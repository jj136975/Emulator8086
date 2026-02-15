use crate::vm::registers::{ByteWrapper, HIGH_IDX, LOW_IDX, Register, WordWrapper};

pub const MEMORY_SIZE: usize = 1 << 20; // 1MB address space

pub const IVT_BASE: usize = 0x00000;
pub const BDA_BASE: usize = 0x00400;
pub const BOOT_ADDR: usize = 0x07C00;
pub const CONV_MEM_END: usize = 0xA0000;
pub const VGA_TEXT_BASE: usize = 0xB8000;
pub const BIOS_ROM: usize = 0xF0000;

pub struct Memory {
    mem: Box<[u8]>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            mem: vec![0u8; MEMORY_SIZE].into_boxed_slice()
        }
    }
}

impl Memory {
    pub fn copy_data(memory: &mut Memory, offset: usize, data: &[u8]) {
        memory.mem[offset..offset+data.len()].copy_from_slice(data);
    }

    #[inline]
    pub fn ref_byte(&mut self, address: usize) -> ByteWrapper {
        ByteWrapper::new(&mut self.mem[address])
    }

    #[inline]
    pub fn ref_word(&mut self, address: usize) -> WordWrapper {
        let slice: &mut [u8; 2] = {
            let x = &mut self.mem[address..=address + 1];
            x.try_into()
                .unwrap_or_else(|_| panic!("Invalid Memory address: {}", address))
        };
        WordWrapper::from_slice(slice)
    }

    #[inline]
    pub fn read_byte(&self, address: usize) -> u8 {
        self.mem[address]
    }

    #[inline]
    pub fn read_word(&self, address: usize) -> u16 {
        let bytes: [u8; 2] = [self.mem[address], self.mem[address + 1]];
        u16::from_le_bytes(bytes)
    }

    #[inline]
    pub fn write_byte(&mut self, address: usize, byte: u8) {
        self.mem[address] = byte;
    }

    #[inline]
    pub fn write_word(&mut self, address: usize, word: u16) {
        let bytes = word.to_le_bytes();
        self.mem[address + LOW_IDX] = bytes[0];
        self.mem[address + HIGH_IDX] = bytes[1];
    }
}

pub struct Segment {
    offset: Register,
    mem: *mut Memory
}

impl Segment {
    pub fn new(offset: u16, memory: *mut Memory) -> Self {
        Self {
            offset: Register::new(offset),
            mem: memory
        }
    }

    #[inline(always)]
    pub fn phys_address(&self, address: u16) -> usize {
        (((self.offset.word() as usize) << 4) + address as usize) & 0xFFFFF
    }

    #[inline]
    pub fn ref_byte(&mut self, address: u16) -> ByteWrapper {
        let address = self.phys_address(address);
        unsafe { (*self.mem).ref_byte(address) }
    }

    #[inline]
    pub fn ref_word(&mut self, address: u16) -> WordWrapper {
        let address = self.phys_address(address);
        unsafe {(* self.mem).ref_word(address) }
    }

    #[inline]
    pub fn read_byte(&self, address: u16) -> u8 {
        let address = self.phys_address(address);
        unsafe { (*self.mem).read_byte(address) }
    }

    #[inline]
    pub fn read_word(&self, address: u16) -> u16 {
        // Read byte-by-byte so offset 0xFFFF wraps to 0x0000 within the segment
        let lo = self.read_byte(address);
        let hi = self.read_byte(address.wrapping_add(1));
        u16::from_le_bytes([lo, hi])
    }

    #[inline]
    pub fn write_byte(&mut self, address: u16, byte: u8) {
        let address = self.phys_address(address);
        unsafe { (*self.mem).write_byte(address, byte); }
    }

    #[inline]
    pub fn write_word(&mut self, address: u16, word: u16) {
        let bytes = word.to_le_bytes();
        self.write_byte(address, bytes[0]);
        self.write_byte(address.wrapping_add(1), bytes[1]);
    }

    #[inline]
    pub fn reg(&self) -> &Register {
        &self.offset
    }

    #[inline]
    pub fn reg_mut(&mut self) -> &mut Register {
        &mut self.offset
    }
}
