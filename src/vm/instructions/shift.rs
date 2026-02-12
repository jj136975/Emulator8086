use crate::utils::number::SpecialOps;
use crate::vm::modrm::ModRM;
use crate::vm::runtime::Runtime;
use crate::vm::runtime::CpuFlag::*;
use super::{update_arithmetic_flags_word, update_arithmetic_flags_byte};

pub(super) fn group_d0_d3(vm: &mut Runtime, is_word: bool, directional: bool) {
    let count = if !directional { 1 } else { vm.registers.cx.low() as u32 };

    if is_word {
        let (modrm, reg) = u16::mod_rm_single(vm);
        if count == 0 { return; }
        let word = modrm.word();

        let (w, c) = match reg & 0b_111 {
            // ROL
            0b_000 => (word.rotate_left(count), word & (1u16.wrapping_shl(u16::BITS - count)) != 0),
            // ROR
            0b_001 => (word.rotate_right(count), word & (1u16.wrapping_shl(count - 1)) != 0),
            // RCL
            0b_010 => word.rotate_carry_left(count, vm.check_flag(Carry)),
            // RCR
            0b_011 => word.rotate_carry_right(count, vm.check_flag(Carry)),
            // SHL | SAL - cast to u32 for large counts
            0b_100 | 0b_110 => {
                let result = (word as u32).wrapping_shl(count) as u16;
                let cf = word & (1u16.wrapping_shl(u16::BITS - count)) != 0;
                (result, cf)
            },
            // SHR - cast to u32 for large counts
            0b_101 => {
                let result = (word as u32).wrapping_shr(count) as u16;
                let cf = if count <= 16 { word & (1u16.wrapping_shl(count - 1)) != 0 } else { false };
                (result, cf)
            },
            // SAR
            0b_111 => ((word as i16).wrapping_shr(count) as u16, word & (1u16.wrapping_shl(count - 1)) != 0),
            _ => unreachable!()
        };

        if reg & 0b_100 != 0 {
            // SHL/SHR/SAR: update all arithmetic flags
            let overflow = if count == 1 {
                if reg & 0b_001 == 0 {
                    // SHL/SAL: OF = MSB(result) XOR CF
                    ((w as i16) < 0) != c
                } else {
                    // SHR/SAR: OF = MSB of original value changed
                    ((word ^ w) as i16) < 0
                }
            } else {
                // OF undefined for count > 1, leave unchanged
                vm.check_flag(Overflow)
            };
            update_arithmetic_flags_word(vm, w, overflow, c);
        } else {
            // ROL/ROR/RCL/RCR: only update CF and OF
            vm.update_flag(Carry, c);
            if count == 1 {
                match reg & 0b_111 {
                    // ROL: OF = MSB(result) XOR CF
                    0b_000 => vm.update_flag(Overflow, ((w as i16) < 0) != c),
                    // ROR: OF = MSB(result) XOR bit[n-2](result)
                    0b_001 => vm.update_flag(Overflow, ((w >> 15) ^ ((w >> 14) & 1)) != 0),
                    // RCL: OF = MSB(result) XOR CF
                    0b_010 => vm.update_flag(Overflow, ((w as i16) < 0) != c),
                    // RCR: OF = MSB(result) XOR bit[n-2](result)
                    0b_011 => vm.update_flag(Overflow, ((w >> 15) ^ ((w >> 14) & 1)) != 0),
                    _ => unreachable!()
                }
            }
            // OF unchanged for count > 1 on rotates
        }
        modrm.set(w);
    } else {
        let (modrm, reg) = u8::mod_rm_single(vm);
        if count == 0 { return; }
        let byte = modrm.byte();

        let (b, c) = match reg & 0b_111 {
            // ROL
            0b_000 => (byte.rotate_left(count), byte & (1u8.wrapping_shl(u8::BITS - count)) != 0),
            // ROR
            0b_001 => (byte.rotate_right(count), byte & (1u8.wrapping_shl(count - 1)) != 0),
            // RCL
            0b_010 => byte.rotate_carry_left(count, vm.check_flag(Carry)),
            // RCR
            0b_011 => byte.rotate_carry_right(count, vm.check_flag(Carry)),
            // SHL | SAL - cast to u16 for large counts
            0b_100 | 0b_110 => {
                let result = (byte as u16).wrapping_shl(count) as u8;
                let cf = byte & (1u8.wrapping_shl(u8::BITS - count)) != 0;
                (result, cf)
            },
            // SHR - cast to u16 for large counts
            0b_101 => {
                let result = (byte as u16).wrapping_shr(count) as u8;
                let cf = if count <= 8 { byte & (1u8.wrapping_shl(count - 1)) != 0 } else { false };
                (result, cf)
            },
            // SAR
            0b_111 => ((byte as i8).wrapping_shr(count) as u8, byte & (1u8.wrapping_shl(count - 1)) != 0),
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
                    // ROL: OF = MSB(result) XOR CF
                    0b_000 => vm.update_flag(Overflow, ((b as i8) < 0) != c),
                    // ROR: OF = MSB(result) XOR bit[n-2](result)
                    0b_001 => vm.update_flag(Overflow, ((b >> 7) ^ ((b >> 6) & 1)) != 0),
                    // RCL: OF = MSB(result) XOR CF
                    0b_010 => vm.update_flag(Overflow, ((b as i8) < 0) != c),
                    // RCR: OF = MSB(result) XOR bit[n-2](result)
                    0b_011 => vm.update_flag(Overflow, ((b >> 7) ^ ((b >> 6) & 1)) != 0),
                    _ => unreachable!()
                }
            }
        }
        modrm.set(b);
    }
}
