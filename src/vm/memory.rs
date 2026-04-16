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
        let addr = address & 0xFFFFF;
        if addr == 0xFFFFF {
            // Word straddles the 1MB wrap: low byte at 0xFFFFF, high byte
            // wraps to 0x00000. Route through boundary_buf so the caller
            // gets a usable WordWrapper. Writes through the wrapper land
            // in boundary_buf (consistent with Segment::ref_word at offset
            // 0xFFFF) — callers needing write-through at the wrap must use
            // byte-by-byte access via write_word.
            self.boundary_buf[0] = self.mem[0xFFFFF];
            self.boundary_buf[1] = self.mem[0x00000];
            return WordWrapper::from_slice(&mut self.boundary_buf);
        }
        let slice: &mut [u8; 2] = (&mut self.mem[addr..=addr + 1])
            .try_into()
            .expect("slice length 2 by construction");
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

// ---------------------------------------------------------------------------
// Inspection tests — Memory & Segment invariants flagged by the code review.
// ---------------------------------------------------------------------------
#[cfg(test)]
mod inspection_tests {
    use super::*;

    /// Regression: `ref_word(0xFFFFF)` must not panic. The word straddles
    /// the 1MB wrap, so the low byte comes from 0xFFFFF and the high byte
    /// wraps to 0x00000. The wrapper routes through `boundary_buf` so
    /// reads are correct; writes to the wrap slot are dropped (documented).
    #[test]
    fn ref_word_at_last_physical_address_does_not_panic_and_wraps_read() {
        let mut mem = Memory::new();
        mem.write_byte_unchecked(0xFFFFF, 0x11);
        mem.write_byte_unchecked(0x00000, 0x22);
        let w = mem.ref_word(0xFFFFF);
        assert_eq!(w.word(), 0x2211, "low from 0xFFFFF, high wraps to 0x00000");
    }

    /// FINDING (Phase 2): Segment::read_word at offset 0xFFFF must wrap
    /// within the segment (low byte from offset 0xFFFF, high byte from 0x0000
    /// of the SAME segment). Real 8086 does this.
    /// STATUS: PASSES → retracted. `Segment::read_word` uses byte-by-byte
    /// access with `address.wrapping_add(1)`.
    #[test]
    fn segment_read_word_wraps_within_segment_at_offset_ffff() {
        let mut mem = Box::new(Memory::new());
        // Pick segment 0x1000. Physical 0x10000..0x1FFFF.
        // Write 0xAA at segment:0xFFFF (physical 0x1FFFF)
        // Write 0xBB at segment:0x0000 (physical 0x10000)
        Memory::copy_data(&mut mem, 0x1FFFF, &[0xAA]);
        Memory::copy_data(&mut mem, 0x10000, &[0xBB]);
        let mut seg = Segment::new(0x1000, &mut *mem as *mut Memory);
        let word = seg.read_word(0xFFFF);
        assert_eq!(
            word, 0xBBAA,
            "At offset 0xFFFF, low byte should come from segment:FFFF (0xAA) \
             and high byte should wrap to segment:0000 (0xBB), giving 0xBBAA"
        );
    }

    /// FINDING (Phase 2): Memory::read_word at physical 0xFFFFF must wrap
    /// to read the high byte from physical 0. This is the real 8086 20-bit
    /// address bus wrap (no A20 line).
    /// STATUS: PASSES → retracted. `Memory::read_word` masks both addresses.
    #[test]
    fn memory_read_word_wraps_at_1mb_boundary() {
        let mut mem = Memory::new();
        mem.write_byte_unchecked(0xFFFFF, 0x11);
        mem.write_byte_unchecked(0x00000, 0x22);
        let word = mem.read_word(0xFFFFF);
        assert_eq!(
            word, 0x2211,
            "At physical 0xFFFFF, low byte is 0x11, high byte wraps to 0x00000 (0x22)"
        );
    }

    /// FINDING (Phase 2): ROM write protection must block writes above
    /// rom_start when enabled.
    /// STATUS: PASSES → retracted. `write_byte` / `write_word` check
    /// `addr >= self.rom_start`.
    #[test]
    fn rom_write_protection_blocks_writes() {
        let mut mem = Memory::new();
        // Seed a known byte via unchecked write
        mem.write_byte_unchecked(0xF1234, 0xAB);
        mem.enable_rom_protection();
        // Try to overwrite through the protected API
        mem.write_byte(0xF1234, 0xCD);
        assert_eq!(
            mem.read_byte(0xF1234),
            0xAB,
            "ROM write should have been dropped; value changed"
        );
        // write_byte_unchecked should still bypass
        mem.write_byte_unchecked(0xF1234, 0xCD);
        assert_eq!(mem.read_byte(0xF1234), 0xCD);
    }

    /// Sanity: writes below rom_start still succeed.
    #[test]
    fn writes_below_rom_start_still_work() {
        let mut mem = Memory::new();
        mem.enable_rom_protection();
        mem.write_byte(0x00100, 0xEF);
        assert_eq!(mem.read_byte(0x00100), 0xEF);
    }
}