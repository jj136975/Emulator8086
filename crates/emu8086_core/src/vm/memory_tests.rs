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
    // Segment 1000h is physical 10000h-1FFFFh: its offset FFFFh is 1FFFFh
    // and its offset 0000h is 10000h.
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
    mem.write_byte_unchecked(0xF1234, 0xAB);
    mem.enable_rom_protection();
    mem.write_byte(0xF1234, 0xCD);
    assert_eq!(
        mem.read_byte(0xF1234),
        0xAB,
        "ROM write should have been dropped; value changed"
    );
    mem.write_byte_unchecked(0xF1234, 0xCD);
    assert_eq!(mem.read_byte(0xF1234), 0xCD);
}

/// ROM protection leaves writes below `rom_start` alone.
#[test]
fn writes_below_rom_start_still_work() {
    let mut mem = Memory::new();
    mem.enable_rom_protection();
    mem.write_byte(0x00100, 0xEF);
    assert_eq!(mem.read_byte(0x00100), 0xEF);
}
