//! Guest memory. `Memory` is the megabyte the 8086 addresses: every address
//! is taken modulo 2^20 and a word is two separately addressed bytes, so it
//! wraps at the end of a segment or of memory as on the chip. `Segment` is a
//! segment register with the accessors of its 64 KiB window.
//!
//! The invariant: nothing the guest does changes ROM (the top 64 KiB, once
//! protection is on). `write_byte` drops the store, and an operand reference
//! to a ROM byte points at a copy of it in `rom_sink`. The sink has one slot
//! per byte of one operand, so a reference into ROM is good only until the
//! next reference is taken. A `Segment` reaches its `Memory` through a raw
//! pointer: no reference to `cpu.memory` may be held across a call on a
//! segment.

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
    pub(crate) fn new() -> Self {
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
        memory.mem[offset..offset + data.len()].copy_from_slice(data);
    }

    /// The byte an operand reference to `addr` reads and writes. A ROM byte
    /// is copied to scratch slot `slot` first, so the operand reads it
    /// normally but cannot change it.
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
        // SAFETY: both pointers address bytes this `Memory` owns, in its
        // array or in `rom_sink`. They stay valid while it is neither moved
        // nor dropped, which outlasts any wrapper used as the wrapper
        // contract (`registers.rs`) requires.
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
        if addr >= self.rom_start {
            return;
        }
        self.mem[addr] = byte;
    }

    #[cfg(test)]
    #[inline]
    pub fn write_word(&mut self, address: usize, word: u16) {
        let bytes = word.to_le_bytes();
        let lo = address & 0xFFFFF;
        let hi = (address + 1) & 0xFFFFF;
        if lo < self.rom_start {
            self.mem[lo] = bytes[0];
        }
        if hi < self.rom_start {
            self.mem[hi] = bytes[1];
        }
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
    /// The `Memory` this segment addresses: the one its `Cpu` keeps boxed
    /// (`Registers::new`). That allocation outlives the registers and is
    /// never replaced, so the pointer always points at a live `Memory`. It
    /// aliases the box that owns that `Memory`, which the aliasing rules
    /// sanction no more than the operand wrappers of `registers.rs`: what
    /// keeps it working is that the two paths are never in use at once.
    mem: *mut Memory,
}

impl Segment {
    /// `memory` must point at a `Memory` that stays where it is for as long
    /// as the segment is used: in the machine, the box the `Cpu` owns.
    pub fn new(offset: u16, memory: *mut Memory) -> Self {
        Self {
            offset: Register::new(offset),
            mem: memory,
        }
    }

    #[inline(always)]
    pub fn phys_address(&self, address: u16) -> usize {
        (((self.offset.word() as usize) << 4) + address as usize) & 0xFFFFF
    }

    #[inline]
    pub fn ref_byte(&mut self, address: u16) -> ByteWrapper {
        let address = self.phys_address(address);
        // SAFETY: `mem` points at a live `Memory` (see the field), and the
        // caller holds no reference to it across this call.
        unsafe { (*self.mem).ref_byte(address) }
    }

    #[inline]
    pub fn ref_word(&mut self, address: u16) -> WordWrapper {
        // The high byte's offset wraps inside the segment (FFFFh -> 0000h).
        let lo = self.phys_address(address);
        let hi = self.phys_address(address.wrapping_add(1));
        // SAFETY: `mem` points at a live `Memory` (see the field), and the
        // caller holds no reference to it across this call.
        unsafe { (*self.mem).ref_word_at(lo, hi) }
    }

    #[inline]
    pub fn read_byte(&self, address: u16) -> u8 {
        let address = self.phys_address(address);
        // SAFETY: `mem` points at a live `Memory` (see the field), and the
        // caller holds no mutable reference to it across this call.
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
        // SAFETY: `mem` points at a live `Memory` (see the field), and the
        // caller holds no reference to it across this call.
        unsafe {
            (*self.mem).write_byte(address, byte);
        }
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

#[cfg(test)]
#[path = "memory_tests.rs"]
mod tests;
