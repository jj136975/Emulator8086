use crate::vm::registers::{ByteWrapper, HIGH_IDX, LOW_IDX, Register, WordWrapper};

pub const SEGMENT_SIZE: usize = 1 << 16;
pub const MEMORY_SIZE: usize = SEGMENT_SIZE * 5;

pub struct Memory {
    mem: Box<[u8; MEMORY_SIZE]>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            mem: Box::new([0; MEMORY_SIZE])
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

    #[inline]
    pub unsafe fn as_ptr(&mut self) -> *mut u8 {
        self.mem.as_mut_ptr()
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
    fn phys_address(&self, address: u16) -> usize {
        ((self.offset.word() as usize) << 4) + address as usize
    }

    pub fn copy_data(&self, offset: u16, data: &[u8]) {
        let offset = self.phys_address(offset);
        unsafe { (*self.mem).mem[offset..offset+data.len()].copy_from_slice(data); }
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
        let address = self.phys_address(address);
        unsafe { (*self.mem).read_word(address) }
    }

    #[inline]
    pub fn write_byte(&mut self, address: u16, byte: u8) {
        let address = self.phys_address(address);
        unsafe { (*self.mem).write_byte(address, byte); }
    }

    #[inline]
    pub fn write_word(&mut self, address: u16, word: u16) {
        let address = self.phys_address(address);
        unsafe { (*self.mem).write_word(address, word); }
    }

    #[inline]
    pub fn reg(&self) -> &Register {
        &self.offset
    }

    #[inline]
    pub fn reg_mut(&mut self) -> &mut Register {
        &mut self.offset
    }

    #[inline]
    pub unsafe fn as_ptr(&mut self) -> *mut u8 {
        self.as_ptr_at(0)
    }

    #[inline]
    pub unsafe fn as_ptr_at(&mut self, address: u16) -> *mut u8 {
        (*self.mem).as_ptr().add(self.phys_address(address))
    }
}
