use crate::vm::registers::{ByteWrapper, Register, WordWrapper};

pub const MEMORY_SIZE: usize = 1 << 20; // 1MB address space


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
        ByteWrapper::new(&mut self.mem[address & 0xFFFFF])
    }

    #[inline]
    pub fn ref_word(&mut self, address: usize) -> WordWrapper {
        // NOTE: Does NOT wrap at 1MB boundary. Only use when address < 0xFFFFF.
        // For wrapping access, use read_word/write_word instead.
        let addr = address & 0xFFFFF;
        let slice: &mut [u8; 2] = {
            let x = &mut self.mem[addr..=addr + 1];
            x.try_into()
                .unwrap_or_else(|_| panic!("Invalid Memory address: {}", addr))
        };
        WordWrapper::from_slice(slice)
    }

    #[inline]
    pub fn read_byte(&self, address: usize) -> u8 {
        self.mem[address & 0xFFFFF]
    }

    #[inline]
    pub fn read_word(&self, address: usize) -> u16 {
        let lo = self.mem[address & 0xFFFFF];
        let hi = self.mem[(address + 1) & 0xFFFFF];
        u16::from_le_bytes([lo, hi])
    }

    #[inline]
    pub fn write_byte(&mut self, address: usize, byte: u8) {
        self.mem[address & 0xFFFFF] = byte;
    }

    #[inline]
    pub fn write_word(&mut self, address: usize, word: u16) {
        let bytes = word.to_le_bytes();
        self.mem[address & 0xFFFFF] = bytes[0];
        self.mem[(address + 1) & 0xFFFFF] = bytes[1];
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
        // NOTE: Does NOT wrap at 1MB. Only safe when access won't cross boundary.
        let address = self.phys_address(address);
        unsafe { (*self.mem).ref_word(address) }
    }

    #[inline]
    pub fn read_byte(&self, address: u16) -> u8 {
        let address = self.phys_address(address);
        unsafe { (*self.mem).read_byte(address) }
    }

    #[inline]
    pub fn read_word(&self, address: u16) -> u16 {
        // Byte-by-byte so offset 0xFFFF wraps correctly within segment
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