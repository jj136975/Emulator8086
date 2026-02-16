use std::ops::{BitAnd, BitOr, BitXor, Not};
use crate::utils::number::{extend_sign, SpecialOps};
use crate::vm::modrm::ModRM;
use crate::vm::runtime::{Prefix, Runtime};
use crate::vm::runtime::CpuFlag::*;
use super::{
    update_arithmetic_flags_word, update_arithmetic_flags_byte,
    update_logical_flags_word, update_logical_flags_byte,
    WORD_AUX_CARRY_MASK, WORD_AUX_CARRY_FLAG,
    BYTE_AUX_CARRY_MASK, BYTE_AUX_CARRY_FLAG,
};
use super::control::div_zero;

pub(super) fn group_80_83(vm: &mut Runtime, opcode: u8, is_word: bool, directional: bool) {
    // Opcode 0x82 is an alias for 0x80 (byte operations), so treat it as byte path
    if is_word || (directional && opcode != 0x82) {
        let (modrm, reg) = u16::mod_rm_single(vm);
        let word = if directional {
            extend_sign(vm.cpu.fetch_byte())
        } else {
            vm.cpu.fetch_word()
        };

        let prev = modrm.word();
        let (w, o, c) = match reg & 0b_111 {
            // ADD
            0b_000 => prev.oc_add(word),
            // OR
            0b_001 => (modrm.operation(word, u16::bitor), false, false),
            // ADC
            0b_010 => prev.oc_carry_add(word, vm.cpu.check_flag(Carry)),
            // SBB
            0b_011 => prev.oc_carry_sub(word, vm.cpu.check_flag(Carry)),
            // AND
            0b_100 => (modrm.operation(word, u16::bitand), false, false),
            // SUB
            0b_101 => prev.oc_sub(word),
            // XOR
            0b_110 => (modrm.operation(word, u16::bitxor), false, false),
            // CMP
            0b_111 => prev.oc_sub(word),
            _ => unreachable!()
        };

        match reg & 0b_111 {
            // OR, AND, XOR - logical flags
            0b_001 | 0b_100 | 0b_110 => {
                update_logical_flags_word(vm, w);
            }
            // ADD
            0b_000 => {
                update_arithmetic_flags_word(vm, w, o, c);
                vm.cpu.update_flag(AuxCarry, ((prev & WORD_AUX_CARRY_MASK) + (word & WORD_AUX_CARRY_MASK)) & WORD_AUX_CARRY_FLAG != 0);
                modrm.set(w);
            }
            // ADC
            0b_010 => {
                let cf = vm.cpu.check_flag(Carry);
                update_arithmetic_flags_word(vm, w, o, c);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) + (word & 0xF) + (cf as u16) > 0xF);
                modrm.set(w);
            }
            // SUB, CMP
            0b_101 | 0b_111 => {
                update_arithmetic_flags_word(vm, w, o, c);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (word & 0xF));
                if reg & 0b_111 != 0b_111 {
                    modrm.set(w);
                }
            }
            // SBB
            0b_011 => {
                let cf = vm.cpu.check_flag(Carry) as u16;
                update_arithmetic_flags_word(vm, w, o, c);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (word & 0xF) + cf);
                modrm.set(w);
            }
            _ => {}
        }
        // For ADD/ADC/SUB/SBB that already called modrm.set above, skip the generic set
        // For OR/AND/XOR, modrm.operation already wrote the result
        // For CMP, we don't write back
        return;
    } else {
        let (modrm, reg) = u8::mod_rm_single(vm);
        let byte = vm.cpu.fetch_byte();

        let prev = modrm.byte();
        let (b, o, c) = match reg & 0b_111 {
            // ADD
            0b_000 => prev.oc_add(byte),
            // OR
            0b_001 => (modrm.operation(byte, u8::bitor), false, false),
            // ADC
            0b_010 => prev.oc_carry_add(byte, vm.cpu.check_flag(Carry)),
            // SBB
            0b_011 => prev.oc_carry_sub(byte, vm.cpu.check_flag(Carry)),
            // AND
            0b_100 => (modrm.operation(byte, u8::bitand), false, false),
            // SUB
            0b_101 => prev.oc_sub(byte),
            // XOR
            0b_110 => (modrm.operation(byte, u8::bitxor), false, false),
            // CMP
            0b_111 => prev.oc_sub(byte),
            _ => unreachable!()
        };

        match reg & 0b_111 {
            0b_001 | 0b_100 | 0b_110 => {
                update_logical_flags_byte(vm, b);
            }
            0b_000 => {
                update_arithmetic_flags_byte(vm, b, o, c);
                vm.cpu.update_flag(AuxCarry, ((prev & BYTE_AUX_CARRY_MASK) + (byte & BYTE_AUX_CARRY_MASK)) & BYTE_AUX_CARRY_FLAG != 0);
                modrm.set(b);
            }
            0b_010 => {
                let cf = vm.cpu.check_flag(Carry);
                update_arithmetic_flags_byte(vm, b, o, c);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) + (byte & 0xF) + (cf as u8) > 0xF);
                modrm.set(b);
            }
            0b_101 | 0b_111 => {
                update_arithmetic_flags_byte(vm, b, o, c);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (byte & 0xF));
                if reg & 0b_111 != 0b_111 {
                    modrm.set(b);
                }
            }
            0b_011 => {
                let cf = vm.cpu.check_flag(Carry) as u8;
                update_arithmetic_flags_byte(vm, b, o, c);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (byte & 0xF) + cf);
                modrm.set(b);
            }
            _ => {}
        }
    }
}

pub(super) fn group_f6_f7(vm: &mut Runtime, is_word: bool) {
    if is_word {
        let (modrm, reg) = u16::mod_rm_single(vm);

        match reg & 0b_111 {
            // TEST (reg=1 is undocumented alias on 8086)
            0b_000 | 0b_001 => {
                let word = vm.cpu.fetch_word();
                let res = modrm.word().bitand(word);
                update_logical_flags_word(vm, res);
            },
            // NOT - no flags modified
            0b_010 => {
                let res = modrm.word().not();
                modrm.set(res);
            },
            // NEG
            0b_011 => {
                let operand = modrm.word();
                let (res, overflow, carry) = 0u16.oc_sub(operand);
                modrm.set(res);
                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (operand & 0xF) != 0);
            },
            // MUL - only CF/OF defined
            0b_100 => {
                let mut res: u32 = (vm.cpu.registers.ax.word() as u32) * (modrm.word() as u32);
                // 8086 undocumented: REPZ prefix negates result (F1 flag)
                if matches!(vm.prefix, Some(Prefix::Rep(_))) { res = (res as i32).wrapping_neg() as u32; }
                let dx: u16 = (res >> 16) as u16;
                vm.cpu.registers.dx.set(dx);
                vm.cpu.registers.ax.set(res as u16);
                vm.cpu.update_flag(Carry, dx != 0);
                vm.cpu.update_flag(Overflow, dx != 0);
            },
            // IMUL - sign-extend operands, only CF/OF defined
            0b_101 => {
                let mut res: i32 = (vm.cpu.registers.ax.word() as i16 as i32) * (modrm.word() as i16 as i32);
                // 8086 undocumented: REPZ prefix negates result (F1 flag)
                if matches!(vm.prefix, Some(Prefix::Rep(_))) { res = res.wrapping_neg(); }
                let dx: u16 = ((res as u32) >> 16) as u16;
                vm.cpu.registers.dx.set(dx);
                vm.cpu.registers.ax.set(res as u16);
                // CF/OF set if sign extension of AX != DX:AX
                let sign_ext_check = (res as i16 as i32) != res;
                vm.cpu.update_flag(Carry, sign_ext_check);
                vm.cpu.update_flag(Overflow, sign_ext_check);
            },
            // DIV - no flags defined
            0b_110 => {
                let numerator: u32 = (vm.cpu.registers.dx.word() as u32) << 16 | (vm.cpu.registers.ax.word() as u32);
                let denum = modrm.word() as u32;

                if denum == 0 {
                    div_zero(vm);
                } else {
                    let mut quot = numerator / denum;
                    let rem = numerator % denum;
                    // 8086 undocumented: REPZ prefix negates quotient (F1 flag)
                    if matches!(vm.prefix, Some(Prefix::Rep(_))) { quot = (quot as i32).wrapping_neg() as u32; }
                    if quot > u16::MAX as u32 {
                        div_zero(vm);
                    } else {
                        vm.cpu.registers.ax.set(quot as u16);
                        vm.cpu.registers.dx.set(rem as u16);
                    }
                }
            },
            // IDIV - sign-extend divisor, truncated division, no flags defined
            0b_111 => {
                let numerator: i32 = ((vm.cpu.registers.dx.word() as u32) << 16 | (vm.cpu.registers.ax.word() as u32)) as i32;
                let denum = modrm.word() as i16 as i32;

                if denum == 0 {
                    div_zero(vm);
                } else {
                    let mut quot = numerator / denum;
                    let rem = numerator % denum;
                    // 8086 undocumented: REPZ prefix sets internal F1 flag,
                    // which the multiply/divide microcode uses for sign tracking.
                    // This causes an extra negation of the quotient.
                    if matches!(vm.prefix, Some(Prefix::Rep(_))) { quot = -quot; }
                    if quot < -32768 || quot > 32767 {
                        div_zero(vm);
                    } else {
                        vm.cpu.registers.ax.set(quot as u16);
                        vm.cpu.registers.dx.set(rem as u16);
                    }
                }
            },
            _ => unreachable!(),
        };
    } else {
        let (modrm, reg) = u8::mod_rm_single(vm);

        match reg & 0b_111 {
            // TEST (reg=1 is undocumented alias on 8086)
            0b_000 | 0b_001 => {
                let byte = vm.cpu.fetch_byte();
                let res = modrm.apply(byte, u8::bitand);
                update_logical_flags_byte(vm, res);
            },
            // NOT - no flags modified
            0b_010 => {
                let res = modrm.byte().not();
                modrm.set(res);
            },
            // NEG
            0b_011 => {
                let operand = modrm.byte();
                let (res, overflow, carry) = 0u8.oc_sub(operand);
                modrm.set(res);
                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (operand & 0xF) != 0);
            },
            // MUL - only CF/OF defined
            0b_100 => {
                let mut res: u16 = (vm.cpu.registers.ax.low() as u16) * (modrm.byte() as u16);
                // 8086 undocumented: REPZ prefix negates result (F1 flag)
                if matches!(vm.prefix, Some(Prefix::Rep(_))) { res = (res as i16).wrapping_neg() as u16; }
                vm.cpu.registers.ax.set(res);
                let high_nonzero = res >> 8 != 0;
                vm.cpu.update_flag(Carry, high_nonzero);
                vm.cpu.update_flag(Overflow, high_nonzero);
            },
            // IMUL - only CF/OF defined
            0b_101 => {
                let mut res: i16 = (vm.cpu.registers.ax.low() as i8 as i16) * (modrm.byte() as i8 as i16);
                // 8086 undocumented: REPZ prefix negates result (F1 flag)
                if matches!(vm.prefix, Some(Prefix::Rep(_))) { res = res.wrapping_neg(); }
                vm.cpu.registers.ax.set(res as u16);
                // CF/OF set if sign extension of AL != AX
                let sign_ext_check = (res as i8 as i16) != res;
                vm.cpu.update_flag(Carry, sign_ext_check);
                vm.cpu.update_flag(Overflow, sign_ext_check);
            },
            // DIV - no flags defined
            0b_110 => {
                let numerator: u16 = vm.cpu.registers.ax.word();
                let denum = modrm.byte() as u16;

                if denum == 0 {
                    div_zero(vm);
                } else {
                    let mut quot = numerator / denum;
                    let rem = numerator % denum;
                    // 8086 undocumented: REPZ prefix negates quotient (F1 flag)
                    if matches!(vm.prefix, Some(Prefix::Rep(_))) { quot = (quot as i16).wrapping_neg() as u16; }
                    if quot > u8::MAX as u16 {
                        div_zero(vm);
                    } else {
                        vm.cpu.registers.ax.set_low(quot as u8);
                        vm.cpu.registers.ax.set_high(rem as u8);
                    }
                }
            },
            // IDIV - truncated division, no flags defined
            0b_111 => {
                let numerator: i16 = vm.cpu.registers.ax.word() as i16;
                let denum = modrm.byte() as i8 as i16;

                if denum == 0 {
                    div_zero(vm);
                } else {
                    let mut quot = numerator / denum;
                    let rem = numerator % denum;
                    // 8086 undocumented: REPZ prefix negates quotient (F1 flag)
                    if matches!(vm.prefix, Some(Prefix::Rep(_))) { quot = -quot; }
                    if quot < -128 || quot > 127 {
                        div_zero(vm);
                    } else {
                        vm.cpu.registers.ax.set_low(quot as u8);
                        vm.cpu.registers.ax.set_high(rem as u8);
                    }
                }
            },
            _ => unreachable!(),
        };
    }
}
