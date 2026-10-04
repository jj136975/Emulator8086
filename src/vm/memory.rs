use crate::vm::registers::{ByteWrapper, Register, WordWrapper};

pub const MEMORY_SIZE: usize = 1 << 20; // 1MB address space


pub struct Memory {
    // A fixed-size array rather than a slice: every index is masked to 20
    // bits, so with the length known at compile time no bounds checks remain.
    mem: Box<[u8; MEMORY_SIZE]>,
    /// Scratch bytes that operand references into ROM point at, so stores
    /// through them are dropped like any other write to ROM.
    rom_sink: [u8; 2],
    rom_start: usize,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            mem: vec![0u8; MEMORY_SIZE]
                .into_boxed_slice()
                .try_into()
                .expect("MEMORY_SIZE bytes were allocated"),
            rom_sink: [0; 2],
            rom_start: MEMORY_SIZE, // above 1MB -- protection disabled until explicitly enabled
        }
    }

    /// Enable ROM write protection for the region starting at 0xF0000.
    /// Call this after loading the ROM image.
    pub fn enable_rom_protection(&mut self) {
        self.rom_start = 0xF0000;
    }
}

impl Memory {
    pub fn copy_data(memory: &mut Memory, offset: usize, data: &[u8]) {
        memory.mem[offset..offset+data.len()].copy_from_slice(data);
    }

    /// The byte an operand reference to `addr` reads and writes. A ROM byte is copied to scratch slot `slot` first,
    /// so the operand reads it normally but cannot change it.
    #[inline]
    fn operand_byte(&mut self, addr: usize, slot: usize) -> &mut u8 {
        let addr = addr & 0xFFFFF;
        if addr >= self.rom_start {
            self.rom_sink[slot] = self.mem[addr];
            &mut self.rom_sink[slot]
        } else {
            &mut self.mem[addr]
        }
    }

    #[inline]
    pub fn ref_byte(&mut self, address: usize) -> ByteWrapper {
        ByteWrapper::new(self.operand_byte(address & 0xFFFFF, 0))
    }

    /// Reference to the word whose low byte is at `lo` and high byte at `hi`
    /// (both already wrapped to 20 bits). The bytes are addressed separately,
    /// so a word that wraps around the end of a segment or of memory is read
    /// and written correctly.
    #[inline]
    pub fn ref_word_at(&mut self, lo: usize, hi: usize) -> WordWrapper {
        let lo: *mut u8 = self.operand_byte(lo, 0);
        let hi: *mut u8 = self.operand_byte(hi, 1);
        // SAFETY: both pointers address live bytes owned by `self`.
        unsafe { WordWrapper::from_parts(lo, hi) }
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

    #[cfg(test)]
    #[inline]
    pub fn write_word(&mut self, address: usize, word: u16) {
        let bytes = word.to_le_bytes();
        let lo = address & 0xFFFFF;
        let hi = (address + 1) & 0xFFFFF;
        if lo < self.rom_start { self.mem[lo] = bytes[0]; }
        if hi < self.rom_start { self.mem[hi] = bytes[1]; }
    }

    /// Write a byte without ROM protection checks.
    #[cfg(test)]
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
        // The high byte's offset wraps inside the segment (FFFFh -> 0000h).
        let lo = self.phys_address(address);
        let hi = self.phys_address(address.wrapping_add(1));
        unsafe { (*self.mem).ref_word_at(lo, hi) }
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
// Regression tests — Memory & Segment invariants.
// ---------------------------------------------------------------------------
#[cfg(test)]
mod inspection_tests {
    use super::*;

    /// A word at the last physical address has its high byte at address 0,
    /// for reads and for writes.
    #[test]
    fn word_reference_at_last_physical_address_wraps_to_zero() {
        let mut mem = Memory::new();
        mem.write_byte_unchecked(0xFFFFF, 0x11);
        mem.write_byte_unchecked(0x00000, 0x22);
        let w = mem.ref_word_at(0xFFFFF, 0x00000);
        assert_eq!(w.word(), 0x2211, "low from 0xFFFFF, high wraps to 0x00000");
        w.set(0xBBAA);
        assert_eq!(mem.read_byte(0xFFFFF), 0xAA);
        assert_eq!(mem.read_byte(0x00000), 0xBB);
    }

    /// Operand references into ROM read the ROM bytes but drop writes, byte
    /// by byte: a word straddling the RAM/ROM boundary stores only its RAM half.
    #[test]
    fn operand_references_cannot_modify_rom() {
        let mut mem = Memory::new();
        mem.write_byte_unchecked(0xEFFFF, 0x11);
        mem.write_byte_unchecked(0xF0000, 0x22);
        mem.write_byte_unchecked(0xF0001, 0x33);
        mem.enable_rom_protection();

        let b = mem.ref_byte(0xF0000);
        assert_eq!(b.byte(), 0x22);
        b.set(0x99);
        assert_eq!(mem.read_byte(0xF0000), 0x22);

        let w = mem.ref_word_at(0xF0000, 0xF0001);
        assert_eq!(w.word(), 0x3322);
        w.set(0x7788);
        assert_eq!(mem.read_word(0xF0000), 0x3322);

        let straddling = mem.ref_word_at(0xEFFFF, 0xF0000);
        assert_eq!(straddling.word(), 0x2211);
        straddling.set(0x5566);
        assert_eq!(mem.read_byte(0xEFFFF), 0x66, "the RAM byte is written");
        assert_eq!(mem.read_byte(0xF0000), 0x22, "the ROM byte is not");
    }

    /// `Segment::read_word` at offset 0xFFFF wraps within the segment, as on
    /// the 8086: low byte from offset 0xFFFF, high byte from offset 0x0000.
    #[test]
    fn segment_read_word_wraps_within_segment_at_offset_ffff() {
        let mut mem = Box::new(Memory::new());
        // Pick segment 0x1000. Physical 0x10000..0x1FFFF.
        // Write 0xAA at segment:0xFFFF (physical 0x1FFFF)
        // Write 0xBB at segment:0x0000 (physical 0x10000)
        Memory::copy_data(&mut mem, 0x1FFFF, &[0xAA]);
        Memory::copy_data(&mut mem, 0x10000, &[0xBB]);
        let seg = Segment::new(0x1000, &mut *mem as *mut Memory);
        let word = seg.read_word(0xFFFF);
        assert_eq!(
            word, 0xBBAA,
            "At offset 0xFFFF, low byte should come from segment:FFFF (0xAA) \
             and high byte should wrap to segment:0000 (0xBB), giving 0xBBAA"
        );
    }

    /// `Memory::read_word` at physical 0xFFFFF reads the high byte from
    /// physical 0, like the 8086's 20-bit address bus (no A20 line).
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

    /// With ROM protection enabled, `write_byte` drops writes at or above
    /// `rom_start`; `write_byte_unchecked` still writes.
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