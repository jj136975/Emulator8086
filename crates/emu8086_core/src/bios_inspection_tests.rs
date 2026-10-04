//! Static checks of the BIOS ROM image (`assets/rom/bios.bin`): these tests
//! inspect the bytes the crate ships as `BIOS_ROM`, the image as build.rs
//! left it, without running it. The tests that run the BIOS are in
//! `bios_tests.rs`.
//!
//! The invariant: nothing here executes the ROM. A property that only shows
//! when the BIOS runs belongs in `bios_tests.rs`, on a powered-on machine.

use crate::BIOS_ROM;

/// The ROM image is exactly 64KB, so it fills 0xF0000..=0xFFFFF.
#[test]
fn rom_is_exactly_64kb() {
    assert_eq!(
        BIOS_ROM.len(),
        0x10000,
        "BIOS ROM must be exactly 64KB (0x10000 bytes) so it fills 0xF0000..=0xFFFFF"
    );
}

/// The reset vector at ROM offset 0xFFF0 (physical 0xFFFF0) is a far JMP
/// (opcode 0xEA) to the entry point: the CPU starts executing there, and
/// anything else would run data on reset.
#[test]
fn reset_vector_is_far_jump() {
    let opcode = BIOS_ROM[0xFFF0];
    assert_eq!(
        opcode, 0xEA,
        "Reset vector byte at ROM offset 0xFFF0 should be 0xEA (FAR JMP); got 0x{:02X}",
        opcode
    );
}

/// The 8-bit sum of the whole ROM is 0, as the IBM PC POST checks; build.rs
/// patches the checksum byte at offset 0xFFFF to make it so.
#[test]
fn rom_checksum_sums_to_zero() {
    let sum: u8 = BIOS_ROM.iter().fold(0u8, |acc, &b| acc.wrapping_add(b));
    assert_eq!(
        sum, 0,
        "8-bit sum of the BIOS ROM must be 0 for POST to pass. Got 0x{:02X}. \
         Fix: adjust the checksum byte at offset 0xFFFF so the total sum is 0.",
        sum
    );
}

/// BIOS init points every IVT entry that is still zero (INT 1Ch, 1Bh, 14h,
/// 17h, 05h, ...) at `iret_stub` in segment 0xF000, so no vector is left
/// pointing at garbage. Checked by looking for the fill loop's bytes.
///
/// Fill loop:
///   xor si, si             (31 F6)
///   mov cx, 256            (B9 00 01)
///   cmp word [si], 0       (83 3C 00)
///   jne ...                (75 xx)
///   mov word [si], iret_stub  (C7 04 xx xx)
///   mov word [si+2], 0xF000   (C7 44 02 00 F0)
#[test]
fn ivt_fill_loop_populates_all_unset_vectors() {
    // The distinctive pattern is the "[si+2] = 0xF000" store: C7 44 02 00 F0
    let pattern: &[u8] = &[0xC7, 0x44, 0x02, 0x00, 0xF0];
    assert!(
        BIOS_ROM.windows(pattern.len()).any(|w| w == pattern),
        "bios.bin should contain the IVT-fill instruction `mov word [si+2], 0xF000` \
         (C7 44 02 00 F0) that populates unset IVT entries with the IRET stub \
         segment. Missing this byte sequence means unset vectors could be garbage."
    );
}
