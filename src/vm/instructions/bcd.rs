//! The decimal adjust instructions: AAA, AAS, AAM and AAD work on unpacked
//! BCD (one decimal digit per byte, in AL and AH), DAA and DAS on packed BCD
//! (two digits in AL).
//!
//! The invariant: these follow the 8086 itself where it departs from the
//! algorithm Intel documents. DAA and DAS correct the high digit above 9Fh
//! instead of 99h when AF comes in set, DAS takes CF from the high-digit
//! correction alone, and AAM with a zero divisor sets ZF, SF and PF as for a
//! zero result before it raises INT 0. The hardware vectors pin each of
//! these: do not replace one with the textbook version.

use super::{BYTE_SIGN_FLAG, control};
use crate::utils::number::div_rem;
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Runtime;

/// AAA (37h): adjusts AL after an addition of unpacked BCD digits, carrying
/// into AH.
#[inline(always)]
pub(super) fn aaa(vm: &mut Runtime) {
    let al = vm.cpu.registers.ax.low();

    if (al & 0x0F) >= 0xA || vm.cpu.check_flag(AuxCarry) {
        vm.cpu.registers.ax.set_low(al.wrapping_add(6));
        vm.cpu
            .registers
            .ax
            .set_high(vm.cpu.registers.ax.high().wrapping_add(1));
        vm.cpu.set_flag(AuxCarry);
        vm.cpu.set_flag(Carry);
    } else {
        vm.cpu.unset_flag(AuxCarry);
        vm.cpu.unset_flag(Carry);
    }
    // The high nibble of AL is cleared whether or not the digit was adjusted.
    let new_al = vm.cpu.registers.ax.low() & 0x0F;
    vm.cpu.registers.ax.set_low(new_al);
}

/// AAD (D5h): folds the two unpacked digits into AL as AH * base + AL and
/// clears AH. The base is the immediate byte: 10 as assembled, but any value
/// is accepted.
#[inline(always)]
pub(super) fn aad(vm: &mut Runtime) {
    let factor = vm.cpu.fetch_byte();
    let ah = vm.cpu.registers.ax.operation_high(factor, u8::wrapping_mul);
    let al = vm.cpu.registers.ax.operation_low(ah, u8::wrapping_add);
    vm.cpu.registers.ax.set_high(0);
    vm.cpu.update_flag(Zero, al == 0);
    vm.cpu.update_flag(Sign, al & BYTE_SIGN_FLAG != 0);
    vm.cpu.update_flag(Parity, al.count_ones() & 1 == 0);
}

/// AAM (D4h): splits AL into AH = AL / base and AL = AL % base. The base is
/// the immediate byte, as for AAD; a base of zero raises the divide error.
#[inline(always)]
pub(super) fn aam(vm: &mut Runtime) {
    let factor = vm.cpu.fetch_byte();
    if factor == 0 {
        // The 8086 sets ZF, SF and PF as for a zero result before it takes
        // the divide error, so the FLAGS it stacks carry them.
        vm.cpu.update_flag(Zero, true);
        vm.cpu.update_flag(Sign, false);
        vm.cpu.update_flag(Parity, true);
        control::div_zero(vm);
    } else {
        let (ah, al) = vm.cpu.registers.ax.apply_low(factor, div_rem);
        vm.cpu.registers.ax.set_high(ah);
        vm.cpu.registers.ax.set_low(al);
        vm.cpu.update_flag(Zero, al == 0);
        vm.cpu.update_flag(Sign, al & BYTE_SIGN_FLAG != 0);
        vm.cpu.update_flag(Parity, al.count_ones() & 1 == 0);
    }
}

/// AAS (3Fh): adjusts AL after a subtraction of unpacked BCD digits,
/// borrowing from AH.
#[inline(always)]
pub(super) fn aas(vm: &mut Runtime) {
    let al = vm.cpu.registers.ax.low();

    if (al & 0x0F) >= 0xA || vm.cpu.check_flag(AuxCarry) {
        vm.cpu.registers.ax.set_low(al.wrapping_sub(6));
        vm.cpu
            .registers
            .ax
            .set_high(vm.cpu.registers.ax.high().wrapping_sub(1));
        vm.cpu.set_flag(AuxCarry);
        vm.cpu.set_flag(Carry);
    } else {
        vm.cpu.unset_flag(AuxCarry);
        vm.cpu.unset_flag(Carry);
    }
    // The high nibble of AL is cleared whether or not the digit was adjusted.
    let new_al = vm.cpu.registers.ax.low() & 0x0F;
    vm.cpu.registers.ax.set_low(new_al);
}

/// DAA (27h): adjusts AL after an addition of packed BCD numbers.
#[inline(always)]
pub(super) fn daa(vm: &mut Runtime) {
    let old_al = vm.cpu.registers.ax.low();
    let old_cf = vm.cpu.check_flag(Carry);
    let old_af = vm.cpu.check_flag(AuxCarry);
    let mut cf = false;

    if old_af || (old_al & 0x0F) >= 0x0A {
        let (new_al, carry) = vm.cpu.registers.ax.low().overflowing_add(0x06);
        vm.cpu.registers.ax.set_low(new_al);
        cf = old_cf || carry;
        vm.cpu.set_flag(AuxCarry);
    } else {
        vm.cpu.unset_flag(AuxCarry);
    }
    // 8086 undocumented: when initial AF is set, the upper nibble
    // threshold is 0x9F instead of 0x99 (righto.com silicon analysis).
    let upper_threshold = if old_af { 0x9F } else { 0x99 };
    if old_cf || old_al > upper_threshold {
        let new_al = vm.cpu.registers.ax.low().wrapping_add(0x60);
        vm.cpu.registers.ax.set_low(new_al);
        cf = true;
    }
    vm.cpu.update_flag(Carry, cf);
    let al = vm.cpu.registers.ax.low();
    vm.cpu.update_flag(Zero, al == 0);
    vm.cpu.update_flag(Sign, (al as i8) < 0);
    vm.cpu.update_flag(Parity, al.count_ones() & 1 == 0);
}

/// DAS (2Fh): adjusts AL after a subtraction of packed BCD numbers.
#[inline(always)]
pub(super) fn das(vm: &mut Runtime) {
    let old_al = vm.cpu.registers.ax.low();
    let old_cf = vm.cpu.check_flag(Carry);
    let old_af = vm.cpu.check_flag(AuxCarry);
    // On real 8086, CF is determined solely by the upper nibble
    // correction (step 2). The borrow from the lower nibble
    // correction (step 1) does NOT propagate to CF.
    let mut cf = old_cf;

    if old_af || (old_al & 0x0F) >= 0x0A {
        let new_al = vm.cpu.registers.ax.low().wrapping_sub(0x06);
        vm.cpu.registers.ax.set_low(new_al);
        vm.cpu.set_flag(AuxCarry);
    } else {
        vm.cpu.unset_flag(AuxCarry);
    }
    // 8086 undocumented: when initial AF is set, the upper nibble
    // threshold is 0x9F instead of 0x99 (righto.com silicon analysis).
    let upper_threshold = if old_af { 0x9F } else { 0x99 };
    if old_cf || old_al > upper_threshold {
        let new_al = vm.cpu.registers.ax.low().wrapping_sub(0x60);
        vm.cpu.registers.ax.set_low(new_al);
        cf = true;
    }
    vm.cpu.update_flag(Carry, cf);
    let al = vm.cpu.registers.ax.low();
    vm.cpu.update_flag(Zero, al == 0);
    vm.cpu.update_flag(Sign, (al as i8) < 0);
    vm.cpu.update_flag(Parity, al.count_ones() & 1 == 0);
}
