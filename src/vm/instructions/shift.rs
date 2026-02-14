use crate::utils::number::SpecialOps;
use crate::vm::modrm::ModRM;
use crate::vm::runtime::Runtime;
use crate::vm::runtime::CpuFlag::*;
use super::{update_arithmetic_flags_word, update_arithmetic_flags_byte};

/// 0xC0/0xC1: shift/rotate r/m8 or r/m16 by immediate byte count.
/// Used by MS-DOS 6.22 IO.SYS (186+ but required for compatibility).
pub(super) fn group_c0_c1(vm: &mut Runtime, is_word: bool) {
    if is_word {
        let (modrm, reg) = u16::mod_rm_single(vm);
        let count = vm.fetch_byte() as u32 & 0x1F;
        shift_word(vm, modrm, reg, count);
    } else {
        let (modrm, reg) = u8::mod_rm_single(vm);
        let count = vm.fetch_byte() as u32 & 0x1F;
        shift_byte(vm, modrm, reg, count);
    }
}

pub(super) fn group_d0_d3(vm: &mut Runtime, is_word: bool, directional: bool) {
    let count = if !directional { 1 } else { vm.registers.cx.low() as u32 };

    if is_word {
        let (modrm, reg) = u16::mod_rm_single(vm);
        shift_word(vm, modrm, reg, count);
    } else {
        let (modrm, reg) = u8::mod_rm_single(vm);
        shift_byte(vm, modrm, reg, count);
    }
}

fn shift_word(vm: &mut Runtime, modrm: <u16 as ModRM>::Target, reg: u8, count: u32) {
    if count == 0 { return; }
    let word = modrm.word();

    let (w, c) = match reg & 0b_111 {
        // ROL — CF = bit 0 of result (last bit rotated around)
        0b_000 => { let r = word.rotate_left(count); (r, r & 1 != 0) },
        // ROR — CF = MSB of result (last bit rotated around)
        0b_001 => { let r = word.rotate_right(count); (r, r & 0x8000 != 0) },
        // RCL
        0b_010 => word.rotate_carry_left(count, vm.check_flag(Carry)),
        // RCR
        0b_011 => word.rotate_carry_right(count, vm.check_flag(Carry)),
        // SHL | SAL
        0b_100 | 0b_110 => {
            let result = if count >= 16 { 0u16 } else { word.wrapping_shl(count) };
            let cf = if count <= 16 { word & (1u16 << (16u32 - count)) != 0 } else { false };
            (result, cf)
        },
        // SHR
        0b_101 => {
            let result = if count >= 16 { 0u16 } else { word.wrapping_shr(count) };
            let cf = if count <= 16 { word & (1u16.wrapping_shl(count - 1)) != 0 } else { false };
            (result, cf)
        },
        // SAR
        0b_111 => {
            let result = if count >= 16 { if (word as i16) < 0 { 0xFFFFu16 } else { 0u16 } }
                         else { (word as i16).wrapping_shr(count) as u16 };
            let cf = if count <= 16 { word & (1u16.wrapping_shl(count - 1)) != 0 }
                     else { (word as i16) < 0 };
            (result, cf)
        },
        _ => unreachable!()
    };

    if reg & 0b_100 != 0 {
        let overflow = if count == 1 {
            if reg & 0b_001 == 0 {
                ((w as i16) < 0) != c
            } else {
                ((word ^ w) as i16) < 0
            }
        } else {
            vm.check_flag(Overflow)
        };
        update_arithmetic_flags_word(vm, w, overflow, c);
    } else {
        vm.update_flag(Carry, c);
        if count == 1 {
            match reg & 0b_111 {
                0b_000 => vm.update_flag(Overflow, ((w as i16) < 0) != c),
                0b_001 => vm.update_flag(Overflow, ((w >> 15) ^ ((w >> 14) & 1)) != 0),
                0b_010 => vm.update_flag(Overflow, ((w as i16) < 0) != c),
                0b_011 => vm.update_flag(Overflow, ((w >> 15) ^ ((w >> 14) & 1)) != 0),
                _ => unreachable!()
            }
        }
    }
    modrm.set(w);
}

fn shift_byte(vm: &mut Runtime, modrm: <u8 as ModRM>::Target, reg: u8, count: u32) {
    if count == 0 { return; }
    let byte = modrm.byte();

    let (b, c) = match reg & 0b_111 {
        // ROL — CF = bit 0 of result (last bit rotated around)
        0b_000 => { let r = byte.rotate_left(count); (r, r & 1 != 0) },
        // ROR — CF = MSB of result (last bit rotated around)
        0b_001 => { let r = byte.rotate_right(count); (r, r & 0x80 != 0) },
        // RCL
        0b_010 => byte.rotate_carry_left(count, vm.check_flag(Carry)),
        // RCR
        0b_011 => byte.rotate_carry_right(count, vm.check_flag(Carry)),
        // SHL | SAL
        0b_100 | 0b_110 => {
            let result = if count >= 8 { 0u8 } else { byte.wrapping_shl(count) };
            let cf = if count <= 8 { byte & (1u8 << (8u32 - count)) != 0 } else { false };
            (result, cf)
        },
        // SHR
        0b_101 => {
            let result = if count >= 8 { 0u8 } else { byte.wrapping_shr(count) };
            let cf = if count <= 8 { byte & (1u8.wrapping_shl(count - 1)) != 0 } else { false };
            (result, cf)
        },
        // SAR
        0b_111 => {
            let result = if count >= 8 { if (byte as i8) < 0 { 0xFFu8 } else { 0u8 } }
                         else { (byte as i8).wrapping_shr(count) as u8 };
            let cf = if count <= 8 { byte & (1u8.wrapping_shl(count - 1)) != 0 }
                     else { (byte as i8) < 0 };
            (result, cf)
        },
        _ => unreachable!()
    };

    if reg & 0b_100 != 0 {
        let overflow = if count == 1 {
            if reg & 0b_001 == 0 {
                ((b as i8) < 0) != c
            } else {
                ((byte ^ b) as i8) < 0
            }
        } else {
            vm.check_flag(Overflow)
        };
        update_arithmetic_flags_byte(vm, b, overflow, c);
    } else {
        vm.update_flag(Carry, c);
        if count == 1 {
            match reg & 0b_111 {
                0b_000 => vm.update_flag(Overflow, ((b as i8) < 0) != c),
                0b_001 => vm.update_flag(Overflow, ((b >> 7) ^ ((b >> 6) & 1)) != 0),
                0b_010 => vm.update_flag(Overflow, ((b as i8) < 0) != c),
                0b_011 => vm.update_flag(Overflow, ((b >> 7) ^ ((b >> 6) & 1)) != 0),
                _ => unreachable!()
            }
        }
    }
    modrm.set(b);
}
