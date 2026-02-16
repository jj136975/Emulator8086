mod alu;
pub mod control;
mod shift;
mod string;

#[cfg(test)]
mod tests;

use crate::utils::number::{div_rem, SpecialOps};
use crate::vm::modrm::{direct_address, rm_address, ModRM};
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Prefix::{Queued, Seg};
use crate::vm::runtime::{Prefix, Runtime, SegmentType};
use std::ops::{BitAnd, BitOr, BitXor};

const WORD_MASK: u8 = 0b_00_00_00_01;
const DIRECTION_MASK: u8 = 0b_00_00_00_10;

const WORD_AUX_CARRY_MASK: u16 = 0b_0000_0000_0000_1111;
const WORD_AUX_CARRY_FLAG: u16 = 0b_0000_0000_0001_0000;
const WORD_SIGN_FLAG: u16 = 0b_1000_0000_0000_0000;

const BYTE_AUX_CARRY_MASK: u8 = 0b_0000_1111;
const BYTE_AUX_CARRY_FLAG: u8 = 0b_0001_0000;
const BYTE_SIGN_FLAG: u8 = 0b_1000_0000;

#[inline]
fn update_arithmetic_flags_word(vm: &mut Runtime, res: u16, overflow: bool, carry: bool) {
    vm.cpu.update_flag(Overflow, overflow);
    vm.cpu.update_flag(Carry, carry);
    vm.cpu.update_flag(Zero, res == 0);
    vm.cpu.update_flag(Sign, (res as i16) < 0);
    vm.cpu.update_flag(Parity, (res as u8).count_ones() & 1 == 0);
}

#[inline]
fn update_arithmetic_flags_byte(vm: &mut Runtime, res: u8, overflow: bool, carry: bool) {
    vm.cpu.update_flag(Overflow, overflow);
    vm.cpu.update_flag(Carry, carry);
    vm.cpu.update_flag(Zero, res == 0);
    vm.cpu.update_flag(Sign, (res as i8) < 0);
    vm.cpu.update_flag(Parity, res.count_ones() & 1 == 0);
}

#[inline]
fn update_logical_flags_word(vm: &mut Runtime, res: u16) {
    vm.cpu.unset_flag(Overflow);
    vm.cpu.unset_flag(Carry);
    vm.cpu.update_flag(Zero, res == 0);
    vm.cpu.update_flag(Sign, (res as i16) < 0);
    vm.cpu.update_flag(Parity, (res as u8).count_ones() & 1 == 0);
}

#[inline]
fn update_logical_flags_byte(vm: &mut Runtime, res: u8) {
    vm.cpu.unset_flag(Overflow);
    vm.cpu.unset_flag(Carry);
    vm.cpu.update_flag(Zero, res == 0);
    vm.cpu.update_flag(Sign, (res as i8) < 0);
    vm.cpu.update_flag(Parity, res.count_ones() & 1 == 0);
}

pub fn process(vm: &mut Runtime) {
    vm.cpu.registers.op_pc = vm.cpu.registers.pc.word();

    let opcode = vm.cpu.fetch_byte();
    let is_word: bool = opcode & WORD_MASK != 0;
    let directional: bool = opcode & DIRECTION_MASK != 0;
    if let Some(Queued(prefix)) = vm.prefix.take() {
        vm.prefix = Some(*prefix);
    } else {
        // Previous instruction's segment override has been consumed — clear it.
        vm.segment_override = None;
    }

    match opcode {
        // ADD ac,data
        0b_0000_0100 | 0b_0000_0101 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let ax = vm.cpu.registers.ax.word();
                let (res, overflow, carry) = ax.oc_add(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(
                    AuxCarry,
                    (((word & WORD_AUX_CARRY_MASK) + (ax & WORD_AUX_CARRY_MASK))
                        & WORD_AUX_CARRY_FLAG)
                        != 0,
                );

                vm.cpu.registers.ax.set(res);
            } else {
                let byte = vm.cpu.fetch_byte();
                let ah = vm.cpu.registers.ax.low();
                let (res, overflow, carry) = ah.oc_add(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(
                    AuxCarry,
                    (((byte & BYTE_AUX_CARRY_MASK) + (ah & BYTE_AUX_CARRY_MASK))
                        & BYTE_AUX_CARRY_FLAG)
                        != 0,
                );

                vm.cpu.registers.ax.set_low(res);
            }
        }
        // ADD MOD_R/M
        0b_0000_0000..=0b_0000_0011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };
                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_add(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(
                    AuxCarry,
                    (((word & WORD_AUX_CARRY_MASK) + (prev & WORD_AUX_CARRY_MASK))
                        & WORD_AUX_CARRY_FLAG)
                        != 0,
                );

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };
                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_add(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(
                    AuxCarry,
                    (((byte & BYTE_AUX_CARRY_MASK) + (prev & BYTE_AUX_CARRY_MASK))
                        & BYTE_AUX_CARRY_FLAG)
                        != 0,
                );

                modrm.set(res);
            }
        }
        // AAA
        0x37 => {
            let al = vm.cpu.registers.ax.low();

            if (al & 0x0F) >= 0xA || vm.cpu.check_flag(AuxCarry) {
                vm.cpu.registers.ax.set_low(al.wrapping_add(6));
                vm.cpu.registers
                    .ax
                    .set_high(vm.cpu.registers.ax.high().wrapping_add(1));
                vm.cpu.set_flag(AuxCarry);
                vm.cpu.set_flag(Carry);
            } else {
                vm.cpu.unset_flag(AuxCarry);
                vm.cpu.unset_flag(Carry);
            }
            // Re-read AL after adjustment, then mask low nibble
            let new_al = vm.cpu.registers.ax.low() & 0x0F;
            vm.cpu.registers.ax.set_low(new_al);
        }
        // AAD
        0xD5 => {
            let factor = vm.cpu.fetch_byte();
            let ah = vm.cpu.registers.ax.operation_high(factor, u8::wrapping_mul);
            let al = vm.cpu.registers.ax.operation_low(ah, u8::wrapping_add);
            vm.cpu.registers.ax.set_high(0);
            vm.cpu.update_flag(Zero, al == 0);
            vm.cpu.update_flag(Sign, al & BYTE_SIGN_FLAG != 0);
            vm.cpu.update_flag(Parity, al.count_ones() & 1 == 0);
        }

        // AAM
        0xD4 => {
            let factor = vm.cpu.fetch_byte();
            if factor == 0 {
                // Real 8086 updates PF/ZF/SF as if result were 0 before
                // the divide exception, so the stacked FLAGS reflect this.
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
        // AAS
        0x3F => {
            let al = vm.cpu.registers.ax.low();

            if (al & 0x0F) >= 0xA || vm.cpu.check_flag(AuxCarry) {
                vm.cpu.registers.ax.set_low(al.wrapping_sub(6));
                vm.cpu.registers
                    .ax
                    .set_high(vm.cpu.registers.ax.high().wrapping_sub(1));
                vm.cpu.set_flag(AuxCarry);
                vm.cpu.set_flag(Carry);
            } else {
                vm.cpu.unset_flag(AuxCarry);
                vm.cpu.unset_flag(Carry);
            }
            // Re-read AL after adjustment, then mask low nibble
            let new_al = vm.cpu.registers.ax.low() & 0x0F;
            vm.cpu.registers.ax.set_low(new_al);
        }
        // ADC ac,data
        0b_0001_0100 | 0b_0001_0101 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let ax = vm.cpu.registers.ax.word();
                let cf = vm.cpu.check_flag(Carry);
                let (res, overflow, carry) = ax.oc_carry_add(word, cf);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (ax & 0xF) + (word & 0xF) + (cf as u16) > 0xF);

                vm.cpu.registers.ax.set(res);
            } else {
                let byte = vm.cpu.fetch_byte();
                let al = vm.cpu.registers.ax.low();
                let cf = vm.cpu.check_flag(Carry);
                let (res, overflow, carry) = al.oc_carry_add(byte, cf);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (al & 0xF) + (byte & 0xF) + (cf as u8) > 0xF);

                vm.cpu.registers.ax.set_low(res);
            }
        }
        // ADC Mod R/M
        0b_0001_0000..=0b_0001_0011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };

                let prev = modrm.word();
                let cf = vm.cpu.check_flag(Carry);
                let (res, overflow, carry) = prev.oc_carry_add(word, cf);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) + (word & 0xF) + (cf as u16) > 0xF);

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };

                let prev = modrm.byte();
                let cf = vm.cpu.check_flag(Carry);
                let (res, overflow, carry) = prev.oc_carry_add(byte, cf);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) + (byte & 0xF) + (cf as u8) > 0xF);

                modrm.set(res);
            }
        }
        // AND ac,data
        0b_0010_0100 | 0b_0010_0101 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let word = vm.cpu.registers.ax.operation(word, u16::bitand);

                update_logical_flags_word(vm, word);
            } else {
                let byte = vm.cpu.fetch_byte();
                let byte = vm.cpu.registers.ax.operation_low(byte, u8::bitand);

                update_logical_flags_byte(vm, byte);
            }
        }
        // AND Mod R/M
        0b_0010_0000..=0b_0010_0011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };
                let word = modrm.operation(word, u16::bitand);

                update_logical_flags_word(vm, word);
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };
                let byte = modrm.operation(byte, u8::bitand);

                update_logical_flags_byte(vm, byte);
            }
        }
        // CALL addr
        0x9A => {
            let address = vm.cpu.fetch_word();
            let segment = vm.cpu.fetch_word();
            vm.push_word(vm.cpu.registers.cs.reg().word());
            vm.push_word(vm.cpu.registers.pc.word());
            vm.cpu.registers.cs.reg_mut().set(segment);
            vm.cpu.registers.pc.set(address);
        }
        // CALL | JMP disp16
        0xE8 | 0xE9 => {
            let address = vm.cpu.fetch_word().wrapping_add(vm.cpu.registers.pc.word());
            if !is_word {
                vm.push_word(vm.cpu.registers.pc.word());
            }
            vm.cpu.registers.pc.set(address);
        }
        // CBW
        0x98 => {
            let al = vm.cpu.registers.ax.low();
            if al & BYTE_SIGN_FLAG != 0 {
                vm.cpu.registers.ax.set_high(!0);
            } else {
                vm.cpu.registers.ax.set_high(0);
            }
        }
        // CLC / STC
        0xF8 | 0xF9 => {
            vm.cpu.update_flag(Carry, is_word);
        }
        // CLD / STD
        0xFC | 0xFD => {
            vm.cpu.update_flag(Directional, is_word);
        }
        // CLI
        0xFA => {
            vm.cpu.unset_flag(Interrupt);
        }
        // STI
        0xFB => {
            let was_clear = !vm.cpu.check_flag(Interrupt);
            vm.cpu.set_flag(Interrupt);
            if was_clear {
                vm.interrupt_inhibit = true;
            }
        }
        // CMC
        0xF5 => {
            vm.cpu.flip_flag(Carry);
        }
        // CMP ac,imm
        0b_0011_1100 | 0b_0011_1101 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let ax = vm.cpu.registers.ax.word();
                let (res, overflow, carry) = ax.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (ax & 0xF) < (word & 0xF));
            } else {
                let byte = vm.cpu.fetch_byte();
                let al = vm.cpu.registers.ax.low();
                let (res, overflow, carry) = al.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (al & 0xF) < (byte & 0xF));
            }
        }
        // CMP modrm
        0b_0011_1000..=0b_0011_1011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };
                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (word & 0xF));
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };
                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (byte & 0xF));
            }
        }
        // CMPS
        0b_1010_0110 | 0b_1010_0111 => {
            string::cmps(vm, is_word);
        }
        // CWD
        0x99 => {
            let ax = vm.cpu.registers.ax.word();
            if ax & WORD_SIGN_FLAG != 0 {
                vm.cpu.registers.dx.set(0xFFFF);
            } else {
                vm.cpu.registers.dx.set(0);
            }
        }
        // DAA
        0x27 => {
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
        // DAS
        0x2F => {
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
        // (GRP) DEC, INC, CALL, JMP, PUSH
        0b_1111_1110 | 0b_1111_1111 => {
            control::group_fe_ff(vm, is_word);
        }
        // DEC reg
        0b_0100_1000..=0b_0100_1111 => {
            let reg = opcode & 0b111;
            let rh = vm.cpu.registers.ref_reg_word(reg);
            let prev = rh.word();
            let (res, overflow, _) = prev.oc_sub(1);

            vm.cpu.update_flag(Overflow, overflow);
            vm.cpu.update_flag(Zero, res == 0);
            vm.cpu.update_flag(Sign, (res as i16) < 0);
            vm.cpu.update_flag(Parity, (res as u8).count_ones() & 1 == 0);
            vm.cpu.update_flag(AuxCarry, (prev & 0xF) < 1);

            rh.set(res);
        }
        // TEST, NOT, NEG, MUL, IMUL, DIV, IDIV
        0b_1111_0110 | 0b_1111_0111 => {
            alu::group_f6_f7(vm, is_word);
        }
        // ESC (FPU instructions - no 8087 coprocessor, consume ModR/M)
        0b_1101_1000..=0b_1101_1111 => {
            let _ = u8::mod_rm_single(vm);
        }
        // HLT
        0xF4 => {
            vm.cpu.halted = true;
        }
        // IN ac,DX
        0xEC | 0xED => {
            let port = vm.cpu.registers.dx.word();
            if is_word {
                let val = vm.port_in_word(port);
                vm.cpu.registers.ax.set(val);
            } else {
                let val = vm.port_in_byte(port);
                vm.cpu.registers.ax.set_low(val);
            }
        }
        // IN ac,imm8
        0xE4 | 0xE5 => {
            let port = vm.cpu.fetch_byte() as u16;
            if is_word {
                let val = vm.port_in_word(port);
                vm.cpu.registers.ax.set(val);
            } else {
                let val = vm.port_in_byte(port);
                vm.cpu.registers.ax.set_low(val);
            }
        }
        // INC reg
        0b_0100_0000..=0b_0100_0111 => {
            let reg = opcode & 0b111;
            let rh = vm.cpu.registers.ref_reg_word(reg);
            let prev = rh.word();
            let (res, overflow, _) = prev.oc_add(1);

            vm.cpu.update_flag(Overflow, overflow);
            vm.cpu.update_flag(Zero, res == 0);
            vm.cpu.update_flag(Sign, (res as i16) < 0);
            vm.cpu.update_flag(Parity, (res as u8).count_ones() & 1 == 0);
            vm.cpu.update_flag(AuxCarry, (prev & 0xF) + 1 > 0xF);

            rh.set(res);
        }
        // INT 3 (breakpoint)
        0xCC => {
            vm.handle_interrupt(0x03);
        }
        // INT imm8
        0xCD => {
            let vector = vm.cpu.fetch_byte();
            vm.handle_interrupt(vector);
        }
        // INTO (Interrupt on Overflow)
        0xCE => {
            if vm.cpu.check_flag(Overflow) {
                vm.handle_interrupt(0x04)
            }
        }
        // IRET
        0xCF => {
            let pc = vm.pop_word();
            let cs = vm.pop_word();
            let flags = vm.pop_word();
            vm.cpu.registers.pc.set(pc);
            vm.cpu.registers.cs.reg_mut().set(cs);
            // 8086: bits 12-15 always 1, bit 1 always 1, bits 3,5 always 0
            vm.cpu.flags = (flags & 0x0FD5) | 0xF002;
        }
        // JMP CONDITIONAL disp
        0x70..=0x7F => {
            let disp = vm.cpu.fetch_byte() as i8;
            if (match (opcode >> 1) & 0b_111 {
                // JO / JNO
                0b_000 => vm.cpu.check_flag(Overflow),
                // JB,JNEA / JAE,JNB
                0b_001 => vm.cpu.check_flag(Carry),
                // JE,JZ / JNE,JNZ
                0b_010 => vm.cpu.check_flag(Zero),
                // JBE,JNS / JA,JNBE
                0b_011 => vm.cpu.check_flag(Carry) || vm.cpu.check_flag(Zero),
                // JS / JNS
                0b_100 => vm.cpu.check_flag(Sign),
                // JP,JPE / JNP,JPO
                0b_101 => vm.cpu.check_flag(Parity),
                // JL,JNGE / JGE,JNL
                0b_110 => vm.cpu.check_flag(Sign) != vm.cpu.check_flag(Overflow),
                // JLE,JNG / JG,JNLE
                0b_111 => vm.cpu.check_flag(Zero) || vm.cpu.check_flag(Sign) != vm.cpu.check_flag(Overflow),
                _ => unreachable!(),
            }) != is_word
            {
                vm.cpu.registers
                    .pc
                    .operation(disp as i16, u16::wrapping_add_signed);
            }
        }
        // JCXZ disp
        0xE3 => {
            let disp = vm.cpu.fetch_byte() as i8;
            if vm.cpu.registers.cx.word() == 0 {
                vm.cpu.registers
                    .pc
                    .operation(disp as i16, u16::wrapping_add_signed);
            }
        }
        // JMP addr
        0xEA => {
            let pc = vm.cpu.fetch_word();
            let cs = vm.cpu.fetch_word();
            vm.cpu.registers.pc.set(pc);
            vm.cpu.registers.cs.reg_mut().set(cs);
        }
        // JMP disp
        0xEB => {
            let disp = vm.cpu.fetch_byte() as i8;
            vm.cpu.registers
                .pc
                .operation(disp as i16, u16::wrapping_add_signed);
        }
        // LAHF
        0x9F => {
            vm.cpu.registers.ax.set_high(vm.cpu.flags as u8);
        }
        // LDS reg, mem
        0xC5 => {
            let mod_rm = vm.cpu.fetch_byte();
            let rm = mod_rm & 0b111;
            let mod_val = (mod_rm >> 6) & 0b11;

            let address: u16 = match mod_val {
                0b00 => direct_address(vm, rm),
                0b01 => {
                    let displacement = vm.cpu.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    address.wrapping_add_signed(displacement)
                }
                0b10 => {
                    let displacement = vm.cpu.fetch_word();
                    let address = rm_address(vm, rm);
                    address.wrapping_add(displacement)
                }
                0b11 => vm.cpu.registers.read_reg_word(rm),
                _ => unreachable!(),
            };
            let offset = vm.effective_segment(mod_val, rm).read_word(address);
            let segment = vm
                .effective_segment(mod_val, rm)
                .read_word(address.wrapping_add(2));
            vm.cpu.registers.ref_reg_word((mod_rm >> 3) & 0b111).set(offset);
            vm.cpu.registers.ds.reg_mut().set(segment);
        }
        // LEA
        0x8D => {
            let mod_rm = vm.cpu.fetch_byte();
            let rm = mod_rm & 0b111;

            let address: u16 = match (mod_rm >> 6) & 0b11 {
                0b00 => direct_address(vm, rm),
                0b01 => {
                    let displacement = vm.cpu.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    address.wrapping_add_signed(displacement)
                }
                0b10 => {
                    let displacement = vm.cpu.fetch_word();
                    let address = rm_address(vm, rm);
                    address.wrapping_add(displacement)
                }
                0b11 => vm.cpu.registers.read_reg_word(rm),
                _ => unreachable!(),
            };
            vm.cpu.registers
                .ref_reg_word((mod_rm >> 3) & 0b111)
                .set(address);
        }
        // LES
        0xC4 => {
            let mod_rm = vm.cpu.fetch_byte();
            let rm = mod_rm & 0b111;
            let mod_val = (mod_rm >> 6) & 0b11;

            let address: u16 = match mod_val {
                0b00 => direct_address(vm, rm),
                0b01 => {
                    let displacement = vm.cpu.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    address.wrapping_add_signed(displacement)
                }
                0b10 => {
                    let displacement = vm.cpu.fetch_word();
                    let address = rm_address(vm, rm);
                    address.wrapping_add(displacement)
                }
                0b11 => vm.cpu.registers.read_reg_word(rm),
                _ => unreachable!(),
            };
            let word = vm.effective_segment(mod_val, rm).read_word(address);
            vm.cpu.registers.ref_reg_word((mod_rm >> 3) & 0b111).set(word);
            let word = vm
                .effective_segment(mod_val, rm)
                .read_word(address.wrapping_add(2));
            vm.cpu.registers.es.reg_mut().set(word);
        }
        // LOCK
        0xF0 => {
            vm.prefix = Some(Queued(Box::new(Prefix::Lock)));
        }
        // LODS
        0b_1010_1100 | 0b_1010_1101 => {
            string::lods(vm, is_word);
        }
        // LOOP
        0xE2 => {
            let disp = vm.cpu.fetch_byte() as i8 as i16;
            vm.cpu.registers.cx.operation(1, u16::wrapping_sub);

            if vm.cpu.registers.cx.word() != 0 {
                vm.cpu.registers.pc.operation(disp, u16::wrapping_add_signed);
            }
        }
        // LOOPZ disp, LOOPE disp
        0xE1 => {
            let disp = vm.cpu.fetch_byte() as i8 as i16;
            vm.cpu.registers.cx.operation(1, u16::wrapping_sub);

            if vm.cpu.registers.cx.word() != 0 && vm.cpu.check_flag(Zero) {
                vm.cpu.registers.pc.operation(disp, u16::wrapping_add_signed);
            }
        }
        // LOOPNZ disp, LOOPNE disp
        0xE0 => {
            let disp = vm.cpu.fetch_byte() as i8 as i16;
            vm.cpu.registers.cx.operation(1, u16::wrapping_sub);

            if vm.cpu.registers.cx.word() != 0 && !vm.cpu.check_flag(Zero) {
                vm.cpu.registers.pc.operation(disp, u16::wrapping_add_signed);
            }
        }
        // MOV Mod R/M
        0b_1000_1000..=0b_1000_1011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };
                modrm.set(word);
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };
                modrm.set(byte);
            }
        }
        // MOV data,reg
        0b_1011_0000..=0b_1011_1111 => {
            if opcode & 0b_0000_1000 != 0 {
                let reg = vm.cpu.registers.ref_reg_word(opcode & 0b111);
                let word = vm.cpu.fetch_word();
                reg.set(word);
            } else {
                let reg = vm.cpu.registers.ref_reg_byte(opcode & 0b111);
                let byte = vm.cpu.fetch_byte();
                reg.set(byte);
            }
        }
        // MOV ac,mem (moffs - always 16-bit address)
        0b_1010_0000 | 0b_1010_0001 => {
            let address = vm.cpu.fetch_word();
            if is_word {
                let word = vm.data_segment().read_word(address);
                vm.cpu.registers.ax.set(word);
            } else {
                let byte = vm.data_segment().read_byte(address);
                vm.cpu.registers.ax.set_low(byte);
            }
        }
        // MOV mem,ac
        0b_1010_0010 | 0b_1010_0011 => {
            let address = vm.cpu.fetch_word();
            if is_word {
                let word = vm.cpu.registers.ax.word();
                vm.data_segment().write_word(address, word);
            } else {
                let byte = vm.cpu.registers.ax.low();
                vm.data_segment().write_byte(address, byte);
            }
        }
        // MOV segreg,mem/reg
        0x8E => {
            let mod_rm = vm.cpu.fetch_byte();
            let rm = mod_rm & 0b111;
            let mod_val = (mod_rm >> 6) & 0b11;

            let word = match mod_val {
                0b00 => {
                    let address = direct_address(vm, rm);
                    vm.effective_segment(0b00, rm).read_word(address)
                }
                0b01 => {
                    let displacement = vm.cpu.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    vm.effective_segment(0b01, rm)
                        .read_word(address.wrapping_add_signed(displacement))
                }
                0b10 => {
                    let displacement = vm.cpu.fetch_word();
                    let address = rm_address(vm, rm);
                    vm.effective_segment(0b10, rm)
                        .read_word(address.wrapping_add(displacement))
                }
                0b11 => vm.cpu.registers.read_reg_word(rm),
                _ => unreachable!(),
            };
            let seg_field = (mod_rm >> 3) & 0b011;
            vm.cpu.get_segment(SegmentType::from(seg_field))
                .reg_mut()
                .set(word);
            // MOV SS inhibits interrupts for the next instruction
            if seg_field == 0b10 {
                vm.interrupt_inhibit = true;
            }
        }
        // MOV mem/reg,segreg
        0x8C => {
            let (modrm, reg) = u16::mod_rm_single(vm);
            let word = vm
                .cpu
                .get_segment(SegmentType::from((reg) & 0b011))
                .reg()
                .word();
            modrm.set(word);
        }
        // MOV mem/reg,data
        // Only reg=0 is documented, but on real 8086 all reg values behave as MOV.
        0b_1100_0110 | 0b_1100_0111 => {
            if is_word {
                let (modrm, _reg) = u16::mod_rm_single(vm);
                let word = vm.cpu.fetch_word();
                modrm.set(word);
            } else {
                let (modrm, _reg) = u8::mod_rm_single(vm);
                let byte = vm.cpu.fetch_byte();
                modrm.set(byte);
            }
        }
        // MOVS
        0b_1010_0100 | 0b_1010_0101 => {
            string::movs(vm, is_word);
        }
        // PUSHA (80186+)
        0x60 => {
            let ax = vm.cpu.registers.ax.word();
            let cx = vm.cpu.registers.cx.word();
            let dx = vm.cpu.registers.dx.word();
            let bx = vm.cpu.registers.bx.word();
            let sp = vm.cpu.registers.sp.word();
            let bp = vm.cpu.registers.bp.word();
            let si = vm.cpu.registers.si.word();
            let di = vm.cpu.registers.di.word();
            vm.push_word(ax);
            vm.push_word(cx);
            vm.push_word(dx);
            vm.push_word(bx);
            vm.push_word(sp); // original SP before PUSHA
            vm.push_word(bp);
            vm.push_word(si);
            vm.push_word(di);
        }
        // POPA (80186+)
        0x61 => {
            let di = vm.pop_word();
            let si = vm.pop_word();
            let bp = vm.pop_word();
            let _sp = vm.pop_word(); // discard saved SP
            let bx = vm.pop_word();
            let dx = vm.pop_word();
            let cx = vm.pop_word();
            let ax = vm.pop_word();
            vm.cpu.registers.di.set(di);
            vm.cpu.registers.si.set(si);
            vm.cpu.registers.bp.set(bp);
            vm.cpu.registers.bx.set(bx);
            vm.cpu.registers.dx.set(dx);
            vm.cpu.registers.cx.set(cx);
            vm.cpu.registers.ax.set(ax);
        }
        // BOUND (80186+) — check array index; just consume operands
        0x62 => {
            let _ = u16::mod_rm_single(vm);
        }
        // 8086 alias: JNB rel8 (same as 0x73)
        0x63 => {
            let disp = vm.cpu.fetch_byte() as i8;
            if !vm.cpu.check_flag(Carry) {
                vm.cpu.registers
                    .pc
                    .operation(disp as i16, u16::wrapping_add_signed);
            }
        }
        // FS/GS segment prefix (386+) — ignore in real mode (treat as DS)
        0x64 | 0x65 => {
            vm.prefix = Some(Queued(Box::new(Seg(SegmentType::DS))));
        }
        // Operand/Address size override (386+) — ignore in real mode (NOP prefix)
        0x66 | 0x67 => {}
        // PUSH imm16 (80186+)
        0x68 => {
            let imm = vm.cpu.fetch_word();
            vm.push_word(imm);
        }
        // IMUL reg, r/m16, imm16 (80186+)
        0x69 => {
            let (modrm, reg) = u16::mod_rm_single(vm);
            let src = modrm.word();
            let imm = vm.cpu.fetch_word();
            let result = (src as i16 as i32) * (imm as i16 as i32);
            vm.cpu.registers.ref_reg_word(reg).set(result as u16);
            let overflow = result < -32768 || result > 32767;
            vm.cpu.update_flag(Overflow, overflow);
            vm.cpu.update_flag(Carry, overflow);
        }
        // PUSH sign-extended imm8 (80186+)
        0x6A => {
            let imm = vm.cpu.fetch_byte() as i8 as u16;
            vm.push_word(imm);
        }
        // IMUL reg, r/m16, imm8 (80186+)
        0x6B => {
            let (modrm, reg) = u16::mod_rm_single(vm);
            let src = modrm.word();
            let imm = vm.cpu.fetch_byte() as i8 as i16;
            let result = (src as i16 as i32) * (imm as i32);
            vm.cpu.registers.ref_reg_word(reg).set(result as u16);
            let overflow = result < -32768 || result > 32767;
            vm.cpu.update_flag(Overflow, overflow);
            vm.cpu.update_flag(Carry, overflow);
        }
        // INSB/INSW (80186+) — port [DX] -> ES:[DI]
        0x6C | 0x6D => {
            string::ins(vm, is_word);
        }
        // OUTSB/OUTSW (80186+) — DS:[SI] -> port [DX]
        0x6E | 0x6F => {
            string::outs(vm, is_word);
        }
        // NOP
        0x90 => {}
        // OR ac,data
        0b_0000_1100 | 0b_0000_1101 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let word = vm.cpu.registers.ax.operation(word, u16::bitor);

                update_logical_flags_word(vm, word);
            } else {
                let byte = vm.cpu.fetch_byte();
                let byte = vm.cpu.registers.ax.ref_mut_low().operation(byte, u8::bitor);

                update_logical_flags_byte(vm, byte);
            }
        }
        // OR Mod/RM
        0b_0000_1000..=0b_0000_1011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };
                let word = modrm.operation(word, u16::bitor);

                update_logical_flags_word(vm, word);
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };
                let byte = modrm.operation(byte, u8::bitor);

                update_logical_flags_byte(vm, byte);
            }
        }
        // OUT DX,ac
        0xEE | 0xEF => {
            let port = vm.cpu.registers.dx.word();
            if is_word {
                let val = vm.cpu.registers.ax.word();
                vm.port_out_word(port, val);
            } else {
                let val = vm.cpu.registers.ax.low();
                vm.port_out_byte(port, val);
            }
        }
        // OUT imm8,ac
        0xE6 | 0xE7 => {
            let port = vm.cpu.fetch_byte() as u16;
            if is_word {
                let val = vm.cpu.registers.ax.word();
                vm.port_out_word(port, val);
            } else {
                let val = vm.cpu.registers.ax.low();
                vm.port_out_byte(port, val);
            }
        }
        // POP mem/reg
        0x8F => {
            let word = vm.pop_word();

            let mod_rm = vm.cpu.fetch_byte();
            let rm = mod_rm & 0b111;
            let mod_val = (mod_rm >> 6) & 0b11;

            match mod_val {
                0b00 => {
                    let address = direct_address(vm, rm);
                    vm.effective_segment(0b00, rm).ref_word(address)
                }
                0b01 => {
                    let displacement = vm.cpu.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    vm.effective_segment(0b01, rm)
                        .ref_word(address.wrapping_add_signed(displacement))
                }
                0b10 => {
                    let displacement = vm.cpu.fetch_word();
                    let address = rm_address(vm, rm);
                    vm.effective_segment(0b10, rm)
                        .ref_word(address.wrapping_add(displacement))
                }
                0b11 => vm.cpu.registers.ref_reg_word(rm),
                _ => unreachable!(),
            }
            .set(word);
        }
        // POP reg
        0b_0101_1000..=0b_0101_1111 => {
            let word = vm.pop_word();
            vm.cpu.registers.ref_reg_word(opcode & 0b111).set(word);
        }
        // POP sreg (ES=0x07, CS=0x0F, SS=0x17, DS=0x1F)
        0b_0000_0111 | 0b_0000_1111 | 0b_0001_0111 | 0b_0001_1111 => {
            let word = vm.pop_word();
            let seg_field = (opcode >> 3) & 0b11;
            match seg_field {
                0b00 => vm.cpu.registers.es.reg_mut(),
                0b01 => vm.cpu.registers.cs.reg_mut(),
                0b10 => vm.cpu.registers.ss.reg_mut(),
                0b11 => vm.cpu.registers.ds.reg_mut(),
                _ => unreachable!(),
            }
            .set(word);
            // POP SS inhibits interrupts for the next instruction
            if seg_field == 0b10 {
                vm.interrupt_inhibit = true;
            }
        }
        // POPF
        0x9D => {
            // 8086: bits 12-15 always 1, bit 1 always 1, bits 3,5 always 0
            let flags = vm.pop_word();
            vm.cpu.flags = (flags & 0x0FD5) | 0xF002;
        }
        // PUSH reg
        0b_0101_0000..=0b_0101_0111 => {
            let reg = opcode & 0b111;
            if reg == 4 && !vm.cpu.is_186() {
                // 8086 quirk: PUSH SP pushes the already-decremented value
                vm.cpu.registers.sp.operation(2, u16::wrapping_sub);
                let sp = vm.cpu.registers.sp.word();
                vm.cpu.registers.ss.write_word(sp, sp);
            } else {
                vm.push_word(vm.cpu.registers.read_reg_word(reg));
            }
        }
        // PUSH sreg
        0b_0000_0110 | 0b_0000_1110 | 0b_0001_0110 | 0b_0001_1110 => {
            let word = match (opcode >> 3) & 0b11 {
                0b00 => vm.cpu.registers.es.reg_mut(),
                0b01 => vm.cpu.registers.cs.reg_mut(),
                0b10 => vm.cpu.registers.ss.reg_mut(),
                0b11 => vm.cpu.registers.ds.reg_mut(),
                _ => unreachable!(),
            }
            .word();
            vm.push_word(word);
        }
        // PUSHF
        0x9C => {
            // 8086: bits 12-15 always 1, bit 1 always 1
            vm.push_word(vm.cpu.flags | 0xF002);
        }
        // ROL / ROR / RCL / RCR / SHL / SHR / SAL / SAR  r/m, imm8  (186+, needed by MS-DOS)
        0xC0 | 0xC1 => {
            shift::group_c0_c1(vm, is_word);
        }
        // ENTER imm16, imm8 (80186+)
        0xC8 => {
            let alloc_size = vm.cpu.fetch_word();
            let nesting = (vm.cpu.fetch_byte() & 0x1F) as u16;
            vm.push_word(vm.cpu.registers.bp.word());
            let frame = vm.cpu.registers.sp.word();
            if nesting > 1 {
                for _ in 1..nesting {
                    let bp = vm.cpu.registers.bp.word().wrapping_sub(2);
                    vm.cpu.registers.bp.set(bp);
                    let val = vm.cpu.registers.ss.read_word(bp);
                    vm.push_word(val);
                }
                vm.push_word(frame);
            } else if nesting == 1 {
                vm.push_word(frame);
            }
            vm.cpu.registers.bp.set(frame);
            vm.cpu.registers.sp.operation(alloc_size, u16::wrapping_sub);
        }
        // LEAVE (80186+)
        0xC9 => {
            vm.cpu.registers.sp.set(vm.cpu.registers.bp.word());
            let bp = vm.pop_word();
            vm.cpu.registers.bp.set(bp);
        }
        // ROL / ROR / RCL / RCR / SHL / SHR / SAL / SAR
        0b_1101_0000..=0b_1101_0011 => {
            shift::group_d0_d3(vm, is_word, directional);
        }
        // REP | REPE | REPNE | REPNZ | REPZ
        0b_1111_0010 | 0b_1111_0011 => {
            vm.prefix = Some(Queued(Box::new(Prefix::Rep(is_word))));
        }
        // RET (long)
        0xCB => {
            let pc = vm.pop_word();
            let cs = vm.pop_word();

            vm.cpu.registers.pc.set(pc);
            vm.cpu.registers.cs.reg_mut().set(cs);
        }
        // RET
        0xC3 => {
            let pc = vm.pop_word();
            vm.cpu.registers.pc.set(pc);
        }
        // RET disp16
        0xCA => {
            let disp = vm.cpu.fetch_word();
            let pc = vm.pop_word();
            let cs = vm.pop_word();

            vm.cpu.registers.pc.set(pc);
            vm.cpu.registers.cs.reg_mut().set(cs);
            vm.cpu.registers.sp.operation(disp, u16::wrapping_add);
        }
        // RET disp16
        0xC2 => {
            let disp = vm.cpu.fetch_word();
            let pc = vm.pop_word();

            vm.cpu.registers.pc.set(pc);
            vm.cpu.registers.sp.operation(disp, u16::wrapping_add);
        }
        // SAHF
        0x9E => {
            // Load AH into low byte of flags; 8086: bit 1=1, bits 3,5=0
            vm.cpu.flags = (vm.cpu.flags & 0xFF00) | ((vm.cpu.registers.ax.high() as u16) & 0x00D5) | 0x0002;
        }
        // SBB ac,imm
        0b_0001_1100 | 0b_0001_1101 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let ax = vm.cpu.registers.ax.word();
                let cf = vm.cpu.check_flag(Carry) as u16;
                let (res, overflow, carry) = ax.oc_carry_sub(word, cf != 0);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (ax & 0xF) < (word & 0xF) + cf);
                vm.cpu.registers.ax.set(res);
            } else {
                let byte = vm.cpu.fetch_byte();
                let al = vm.cpu.registers.ax.low();
                let cf = vm.cpu.check_flag(Carry) as u8;
                let (res, overflow, carry) = al.oc_carry_sub(byte, cf != 0);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (al & 0xF) < (byte & 0xF) + cf);
                vm.cpu.registers.ax.set_low(res);
            }
        }
        // Group 80-83: SBB, SUB, XOR, CMP, AND, ADC, ADD, OR
        0b_1000_0000..=0b_1000_0011 => {
            alu::group_80_83(vm, opcode, is_word, directional);
        }
        // SBB modrm
        0b_0001_1000..=0b_0001_1011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };
                let prev = modrm.word();
                let cf = vm.cpu.check_flag(Carry) as u16;
                let (res, overflow, carry) = prev.oc_carry_sub(word, cf != 0);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (word & 0xF) + cf);

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };
                let prev = modrm.byte();
                let cf = vm.cpu.check_flag(Carry) as u8;
                let (res, overflow, carry) = prev.oc_carry_sub(byte, cf != 0);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (byte & 0xF) + cf);

                modrm.set(res);
            }
        }
        // SCAS
        0b_1010_1110 | 0b_1010_1111 => {
            string::scas(vm, is_word);
        }
        // SEG
        0b_0010_0110 | 0b_0010_1110 | 0b_0011_0110 | 0b_0011_1110 => {
            let seg = SegmentType::from((opcode >> 3) & 0b_11);
            vm.segment_override = Some(seg);
            vm.prefix = Some(Queued(Box::new(Seg(seg))));
        }
        // STOS
        0b_1010_1010 | 0b_1010_1011 => {
            string::stos(vm, is_word);
        }
        // SUB AL/AX, imm
        0b_0010_1100 | 0b_0010_1101 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let ax = vm.cpu.registers.ax.word();
                let (res, overflow, carry) = ax.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (ax & 0xF) < (word & 0xF));
                vm.cpu.registers.ax.set(res);
            } else {
                let byte = vm.cpu.fetch_byte();
                let al = vm.cpu.registers.ax.low();
                let (res, overflow, carry) = al.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (al & 0xF) < (byte & 0xF));
                vm.cpu.registers.ax.set_low(res);
            }
        }
        // SUB modrm
        0b_0010_1000..=0b_0010_1011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };
                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (word & 0xF));

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };
                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.cpu.update_flag(AuxCarry, (prev & 0xF) < (byte & 0xF));

                modrm.set(res);
            }
        }
        // TEST ac,data
        0b_1010_1000 | 0b_1010_1001 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let word = vm.cpu.registers.ax.word().bitand(word);

                update_logical_flags_word(vm, word);
            } else {
                let byte = vm.cpu.fetch_byte();
                let byte = vm.cpu.registers.ax.low().bitand(byte);

                update_logical_flags_byte(vm, byte);
            }
        }
        // TEST Mod R/M
        0b_1000_0100 | 0b_1000_0101 => {
            if is_word {
                let (modrm, word) = u16::mod_rm_lhs(vm);
                let word = modrm.word().bitand(word);

                update_logical_flags_word(vm, word);
            } else {
                let (modrm, byte) = u8::mod_rm_lhs(vm);
                let byte = modrm.byte().bitand(byte);

                update_logical_flags_byte(vm, byte);
            }
        }
        // WAIT
        0x9B => {}
        // XCHNG reg
        0b_1001_0001..=0b_1001_0111 => {
            vm.cpu.registers
                .ref_reg_word(opcode & 0b_111)
                .swap_register(&mut vm.cpu.registers.ax);
        }
        // XCHNG Mod R/M
        0b_1000_0110 | 0b_1000_0111 => {
            if is_word {
                let (mut modrm, reg) = u16::mod_rm_single(vm);
                modrm.swap(&mut vm.cpu.registers.ref_reg_word(reg));
            } else {
                let (mut modrm, reg) = u8::mod_rm_single(vm);
                modrm.swap(&mut vm.cpu.registers.ref_reg_byte(reg));
            }
        }
        // SALC (undocumented) — set AL to 0xFF if CF, else 0x00
        0xD6 => {
            vm.cpu.registers
                .ax
                .set_low(if vm.cpu.check_flag(Carry) { 0xFF } else { 0x00 });
        }
        // XLAT
        0xD7 => {
            let address = (vm.cpu.registers.ax.low() as u16).wrapping_add(vm.cpu.registers.bx.word());
            let byte = vm.data_segment().read_byte(address);
            vm.cpu.registers.ax.set_low(byte);
        }
        // XOR
        0b_0011_0100 | 0b_0011_0101 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                let word = vm.cpu.registers.ax.operation(word, u16::bitxor);
                update_logical_flags_word(vm, word);
            } else {
                let byte = vm.cpu.fetch_byte();
                let byte = vm.cpu.registers.ax.operation_low(byte, u8::bitxor);
                update_logical_flags_byte(vm, byte);
            }
        }
        // XOR
        0b_0011_0000..=0b_0011_0011 => {
            if is_word {
                let (modrm, word) = if directional {
                    u16::mod_rm_lhs(vm)
                } else {
                    u16::mod_rm_rhs(vm)
                };
                let word = modrm.operation(word, u16::bitxor);
                update_logical_flags_word(vm, word);
            } else {
                let (modrm, byte) = if directional {
                    u8::mod_rm_lhs(vm)
                } else {
                    u8::mod_rm_rhs(vm)
                };
                let byte = modrm.operation(byte, u8::bitxor);
                update_logical_flags_byte(vm, byte);
            }
        }
        opcode => {
            eprintln!(
                "[ERROR] Unknown instruction: {:#04X} at {:04X}:{:04X}",
                opcode,
                vm.cpu.registers.cs.reg().word(),
                vm.cpu.registers.op_pc
            );
            vm.exit(1);
        }
    }
}
