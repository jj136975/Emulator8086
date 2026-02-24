use crate::vm::registers::{ByteWrapper, Register, WordWrapper};

pub const MEMORY_SIZE: usize = 1 << 20; // 1MB address space


pub struct Memory {
    mem: Box<[u8]>,
    pub(crate) boundary_buf: [u8; 2],
    rom_start: usize,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            mem: vec![0u8; MEMORY_SIZE].into_boxed_slice(),
            boundary_buf: [0; 2],
            rom_start: MEMORY_SIZE, // above 1MB -- protection disabled until explicitly enabled
        }
    }

    /// Enable ROM write protection for the region starting at 0xF0000.
    /// Call this after loading the ROM image.
    pub fn enable_rom_protection(&mut self) {
        self.rom_start = 0xF0000;
    }

    /// Set a custom ROM protection boundary.
    pub fn set_rom_start(&mut self, addr: usize) {
        self.rom_start = addr;
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
        let addr = address & 0xFFFFF;
        if addr >= self.rom_start { return; }
        self.mem[addr] = byte;
    }

    #[inline]
    pub fn write_word(&mut self, address: usize, word: u16) {
        let bytes = word.to_le_bytes();
        let lo = address & 0xFFFFF;
        let hi = (address + 1) & 0xFFFFF;
        if lo < self.rom_start { self.mem[lo] = bytes[0]; }
        if hi < self.rom_start { self.mem[hi] = bytes[1]; }
    }

    /// Write a byte without ROM protection checks.
    /// Used for loading ROM images into memory.
    #[inline]
    pub fn write_byte_unchecked(&mut self, address: usize, byte: u8) {
        self.mem[address & 0xFFFFF] = byte;
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
        if address == 0xFFFF {
            // Offset wraps within segment: low byte from :FFFF, high byte from :0000
            let lo = self.phys_address(0xFFFF);
            let hi = self.phys_address(0x0000);
            unsafe {
                let mem = &mut *self.mem;
                mem.boundary_buf[0] = mem.mem[lo];
                mem.boundary_buf[1] = mem.mem[hi];
                WordWrapper::from_slice(&mut mem.boundary_buf)
            }
        } else {
            let address = self.phys_address(address);
            unsafe { (*self.mem).ref_word(address) }
        }
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