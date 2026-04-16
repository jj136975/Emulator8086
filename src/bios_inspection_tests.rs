//! Inspection tests for the BIOS ROM binary (assets/rom/bios.bin).
//!
//! These are static/structural tests — they inspect the compiled ROM bytes
//! without running the emulator. They complement the runtime tests in the
//! io/vm modules.

const BIOS_BIN: &[u8] = include_bytes!("../assets/rom/bios.bin");

/// FINDING (Phase 5): ROM sits in memory from 0xF0000..=0xFFFFF. The reset
/// vector is at physical 0xFFFF0 (segment F000, offset FFF0). Within a 64KB
/// ROM image loaded at 0xF0000, the reset vector byte offset is 0xFFF0.
/// STATUS: PASSES → retracted. The reset vector is present.
#[test]
fn rom_is_exactly_64kb() {
    assert_eq!(
        BIOS_BIN.len(),
        0x10000,
        "BIOS ROM must be exactly 64KB (0x10000 bytes) so it fills 0xF0000..=0xFFFFF"
    );
}

/// Reset vector at offset 0xFFF0 in the ROM (= physical 0xFFFF0) should be
/// a valid x86 instruction — almost always a FAR JMP (opcode 0xEA) to the
/// entry point. Anything else means the CPU would execute data on reset.
#[test]
fn reset_vector_is_far_jump() {
    let opcode = BIOS_BIN[0xFFF0];
    assert_eq!(
        opcode, 0xEA,
        "Reset vector byte at ROM offset 0xFFF0 should be 0xEA (FAR JMP); got 0x{:02X}",
        opcode
    );
}

/// FINDING (Phase 5): The ROM checksum byte at offset 0xFFFF is hardcoded
/// to 0x00, so the 8-bit sum of the entire ROM is NOT zero.
/// Real IBM PC POST verifies the sum-mod-256 is 0 across the ROM area.
/// STATUS: CONFIRMED BUG (or at least non-conformance).
#[test]
fn rom_checksum_sums_to_zero() {
    let sum: u8 = BIOS_BIN.iter().fold(0u8, |acc, &b| acc.wrapping_add(b));
    assert_eq!(
        sum, 0,
        "8-bit sum of the BIOS ROM must be 0 for POST to pass. Got 0x{:02X}. \
         Fix: adjust the checksum byte at offset 0xFFFF so the total sum is 0.",
        sum
    );
}

/// FINDING (Phase 5): INT 14h (serial), 17h (parallel), 05h (PrintScreen)
/// should be installed as IRET stubs. Without them, user code that chains
/// to these vectors will execute garbage.
///
/// This test is *structural* — we can't verify IVT content without running
/// the BIOS init code, but we can look for an 0xCF (IRET) opcode somewhere
/// in the ROM, which every BIOS should contain.
///
/// STATUS: weak test — just checks that IRET bytes appear in the image.
#[test]
fn bios_contains_iret_bytes() {
    assert!(
        BIOS_BIN.contains(&0xCF),
        "ROM must contain at least one IRET (0xCF) instruction"
    );
}

/// Running the full BIOS init to verify IVT entries would require loading
/// the ROM into a Runtime and stepping until POST completes. That's
/// integration-test territory and sensitive to BIOS internals. Marked
/// #[ignore] so it doesn't run by default; run with `cargo test --
/// --ignored` once the BIOS init is stable enough to assert against.
#[test]
#[ignore]
fn ivt_int_1c_is_initialized_after_bios_init() {
    // Stub — would load bios.bin, step N instructions, then check that
    // memory at 0x0070 / 0x0072 (INT 1Ch vector) is non-zero and points
    // at a plausible IRET stub.
}

/// FIX: INT 08h must re-enable interrupts (STI) between sending EOI and
/// chaining to INT 1Ch so the user hook can be pre-empted by higher-
/// priority IRQs like keyboard. Before the fix there was no STI, so
/// INT 1Ch ran with IF=0 and blocked all IRQs for the hook's duration.
///
/// Regression check: search the ROM for the specific byte sequence
///   B0 20           ; mov al, 0x20
///   E6 20           ; out 0x20, al
///   FB              ; sti
///   CD 1C           ; int 0x1C
/// If the STI gets removed again, this test fails.
#[test]
fn int08_handler_has_sti_between_eoi_and_int1c() {
    let pattern: &[u8] = &[0xB0, 0x20, 0xE6, 0x20, 0xFB, 0xCD, 0x1C];
    assert!(
        BIOS_BIN.windows(pattern.len()).any(|w| w == pattern),
        "INT 08h handler must contain `mov al,0x20; out 0x20,al; sti; int 0x1C` — \
         pattern not found in bios.bin. Missing STI means INT 1Ch runs with \
         interrupts disabled."
    );
}

/// Retract (Phase 5 finding): "INT 1Ch, 1Bh, 14h, 17h, 05h not installed".
/// The fill loop at bios.asm:66-75 sets every zero IVT entry to point at
/// `iret_stub` at segment 0xF000. So unset vectors get stub pointers, not
/// garbage. We verify the ROM contains the fill-loop byte pattern.
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
        BIOS_BIN.windows(pattern.len()).any(|w| w == pattern),
        "bios.bin should contain the IVT-fill instruction `mov word [si+2], 0xF000` \
         (C7 44 02 00 F0) that populates unset IVT entries with the IRET stub \
         segment. Missing this byte sequence means unset vectors could be garbage."
    );
}
