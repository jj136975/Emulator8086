mod alu;
mod shift;
mod string;
pub(super) mod control;

use std::ops::{Add, BitAnd, BitOr, BitXor, Mul};
use crate::utils::number::{div_rem, SpecialOps};
use crate::vm::modrm::{direct_address, ModRM, rm_address};
use crate::vm::runtime::{ExecutionMode, Prefix, Runtime, SegmentType};
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Prefix::{Queued, Seg};

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
    vm.update_flag(Overflow, overflow);
    vm.update_flag(Carry, carry);
    vm.update_flag(Zero, res == 0);
    vm.update_flag(Sign, (res as i16) < 0);
    vm.update_flag(Parity, (res as u8).count_ones() & 1 == 0);
}

#[inline]
fn update_arithmetic_flags_byte(vm: &mut Runtime, res: u8, overflow: bool, carry: bool) {
    vm.update_flag(Overflow, overflow);
    vm.update_flag(Carry, carry);
    vm.update_flag(Zero, res == 0);
    vm.update_flag(Sign, (res as i8) < 0);
    vm.update_flag(Parity, res.count_ones() & 1 == 0);
}

#[inline]
fn update_logical_flags_word(vm: &mut Runtime, res: u16) {
    vm.unset_flag(Overflow);
    vm.unset_flag(Carry);
    vm.update_flag(Zero, res == 0);
    vm.update_flag(Sign, (res as i16) < 0);
    vm.update_flag(Parity, (res as u8).count_ones() & 1 == 0);
}

#[inline]
fn update_logical_flags_byte(vm: &mut Runtime, res: u8) {
    vm.unset_flag(Overflow);
    vm.unset_flag(Carry);
    vm.update_flag(Zero, res == 0);
    vm.update_flag(Sign, (res as i8) < 0);
    vm.update_flag(Parity, res.count_ones() & 1 == 0);
}

pub fn process(vm: &mut Runtime) {
    vm.registers.op_pc = vm.registers.pc.word();

    let opcode = vm.fetch_byte();
    let is_word: bool = opcode & WORD_MASK != 0;
    let directional: bool = opcode & DIRECTION_MASK != 0;
    if let Some(Queued(prefix)) = vm.prefix.take() {
        vm.prefix = Some(*prefix);
    }

    match opcode {
        // ADD ac,data
        0b_0000_0100 | 0b_0000_0101 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let (res, overflow, carry) = ax.oc_add(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (((word & WORD_AUX_CARRY_MASK) + (ax & WORD_AUX_CARRY_MASK)) & WORD_AUX_CARRY_FLAG) != 0);

                vm.registers.ax.set(res);
            } else {
                let byte = vm.fetch_byte();
                let ah = vm.registers.ax.low();
                let (res, overflow, carry) = ah.oc_add(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (((byte & BYTE_AUX_CARRY_MASK) + (ah & BYTE_AUX_CARRY_MASK)) & BYTE_AUX_CARRY_FLAG) != 0);

                vm.registers.ax.set_low(res);
            }
        }
        // ADD MOD_R/M
        0b_0000_0000..=0b_0000_0011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_add(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (((word & WORD_AUX_CARRY_MASK) + (prev & WORD_AUX_CARRY_MASK)) & WORD_AUX_CARRY_FLAG) != 0);

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_add(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (((byte & BYTE_AUX_CARRY_MASK) + (prev & BYTE_AUX_CARRY_MASK)) & BYTE_AUX_CARRY_FLAG) != 0);

                modrm.set(res);
            }
        }
        // AAA
        0x37 => {
            let al = vm.registers.ax.low();

            if (al & 0x0F) >= 0xA || vm.check_flag(AuxCarry) {
                vm.registers.ax.operation(0x106, u16::wrapping_add);
                vm.set_flag(AuxCarry);
                vm.set_flag(Carry);
            } else {
                vm.unset_flag(AuxCarry);
                vm.unset_flag(Carry);
            }
            // Re-read AL after adjustment, then mask low nibble
            let new_al = vm.registers.ax.low() & 0x0F;
            vm.registers.ax.set_low(new_al);
        }
        // AAD
        0xD5 => {
            let factor = vm.fetch_byte();
            let ah = vm.registers.ax.operation_high(factor, u8::mul);
            let al = vm.registers.ax.operation_low(ah, u8::add);
            vm.registers.ax.set_high(0);
            vm.update_flag(Zero, al == 0);
            vm.update_flag(Sign, al & BYTE_SIGN_FLAG != 0);
            vm.update_flag(Parity, al.count_ones() & 1 == 0);
        }

        // AAM
        0xD4 => {
            let factor = vm.fetch_byte();
            let (ah, al) = vm.registers.ax.apply_low(factor, div_rem);
            vm.registers.ax.set_high(ah);
            vm.registers.ax.set_low(al);
            vm.update_flag(Zero, al == 0);
            vm.update_flag(Sign, al & BYTE_SIGN_FLAG != 0);
            vm.update_flag(Parity, al.count_ones() & 1 == 0);
        }
        // AAS
        0x3F => {
            let al = vm.registers.ax.low();

            if (al & 0x0F) >= 0xA || vm.check_flag(AuxCarry) {
                vm.registers.ax.operation(0x106, u16::wrapping_sub);
                vm.set_flag(AuxCarry);
                vm.set_flag(Carry);
            } else {
                vm.unset_flag(AuxCarry);
                vm.unset_flag(Carry);
            }
            // Re-read AL after adjustment, then mask low nibble
            let new_al = vm.registers.ax.low() & 0x0F;
            vm.registers.ax.set_low(new_al);
        }
        // ADC ac,data
        0b_0001_0100 | 0b_0001_0101 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let cf = vm.check_flag(Carry);
                let (res, overflow, carry) = ax.oc_carry_add(word, cf);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (ax & 0xF) + (word & 0xF) + (cf as u16) > 0xF);

                vm.registers.ax.set(res);
            } else {
                let byte = vm.fetch_byte();
                let al = vm.registers.ax.low();
                let cf = vm.check_flag(Carry);
                let (res, overflow, carry) = al.oc_carry_add(byte, cf);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (al & 0xF) + (byte & 0xF) + (cf as u8) > 0xF);

                vm.registers.ax.set_low(res);
            }
        }
        // ADC Mod R/M
        0b_0001_0000..=0b_0001_0011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };

                let prev = modrm.word();
                let cf = vm.check_flag(Carry);
                let (res, overflow, carry) = prev.oc_carry_add(word, cf);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (prev & 0xF) + (word & 0xF) + (cf as u16) > 0xF);

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };

                let prev = modrm.byte();
                let cf = vm.check_flag(Carry);
                let (res, overflow, carry) = prev.oc_carry_add(byte, cf);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (prev & 0xF) + (byte & 0xF) + (cf as u8) > 0xF);

                modrm.set(res);
            }
        }
        // AND ac,data
        0b_0010_0100 | 0b_0010_0101 => {
            if is_word {
                let word = vm.fetch_word();
                let word = vm.registers.ax.operation(word, u16::bitand);

                update_logical_flags_word(vm, word);
            } else {
                let byte = vm.fetch_byte();
                let byte = vm.registers.ax.operation_low(byte, u8::bitand);

                update_logical_flags_byte(vm, byte);
            }
        }
        // AND Mod R/M
        0b_0010_0000..=0b_0010_0011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let word = modrm.operation(word, u16::bitand);

                update_logical_flags_word(vm, word);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let byte = modrm.operation(byte, u8::bitand);

                update_logical_flags_byte(vm, byte);
            }
        }
        // CALL addr
        0x9A => {
            let address = vm.fetch_word();
            let segment = vm.fetch_word();
            vm.push_word(vm.registers.cs.reg().word());
            vm.push_word(vm.registers.pc.word());
            vm.registers.cs.reg_mut().set(segment);
            vm.registers.pc.set(address);
        }
        // CALL | JMP disp16
        0xE8|0xE9 => {
            let address = vm.fetch_word().wrapping_add(vm.registers.pc.word());
            if !is_word {
                vm.push_word(vm.registers.pc.word());
            }
            vm.registers.pc.set(address);
        }
        // CBW
        0x98 => {
            let al = vm.registers.ax.low();
            if al & BYTE_SIGN_FLAG != 0 {
                vm.registers.ax.set_high(!0);
            } else {
                vm.registers.ax.set_high(0);
            }
        }
        // CLC / STC
        0xF8 | 0xF9 => {
            vm.update_flag(Carry, is_word);
        }
        // CLD / STD
        0xFC | 0xFD => {
            vm.update_flag(Directional, is_word);
        }
        // CLI / STI
        0xFA | 0xFB => {
            vm.update_flag(Interrupt, is_word);
        }
        // CMC
        0xF5 => {
            vm.flip_flag(Carry);
        }
        // CMP ac,imm
        0b_0011_1100 | 0b_0011_1101 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let (res, overflow, carry) = ax.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (ax & 0xF) < (word & 0xF));
            } else {
                let byte = vm.fetch_byte();
                let al = vm.registers.ax.low();
                let (res, overflow, carry) = al.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (al & 0xF) < (byte & 0xF));
            }
        }
        // CMP modrm
        0b_0011_1000..=0b_0011_1011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (prev & 0xF) < (word & 0xF));
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (prev & 0xF) < (byte & 0xF));
            }
        }
        // CMPS
        0b_1010_0110 | 0b_1010_0111 => {
            string::cmps(vm, is_word);
        }
        // CWD
        0x99 => {
            let ax = vm.registers.ax.word();
            if ax & WORD_SIGN_FLAG != 0 {
                vm.registers.dx.set(0xFFFF);
            } else {
                vm.registers.dx.set(0);
            }
        }
        // DAA
        0x27 => {
            let old_al = vm.registers.ax.low();
            let old_cf = vm.check_flag(Carry);
            let mut cf = false;

            if vm.check_flag(AuxCarry) || (old_al & 0x0F) >= 0x0A {
                let (new_al, carry) = vm.registers.ax.low().overflowing_add(0x06);
                vm.registers.ax.set_low(new_al);
                cf = old_cf || carry;
                vm.set_flag(AuxCarry);
            } else {
                vm.unset_flag(AuxCarry);
            }
            if old_cf || old_al > 0x99 {
                let new_al = vm.registers.ax.low().wrapping_add(0x60);
                vm.registers.ax.set_low(new_al);
                cf = true;
            }
            vm.update_flag(Carry, cf);
            let al = vm.registers.ax.low();
            vm.update_flag(Zero, al == 0);
            vm.update_flag(Sign, (al as i8) < 0);
            vm.update_flag(Parity, al.count_ones() & 1 == 0);
        }
        // DAS
        0x2F => {
            let old_al = vm.registers.ax.low();
            let old_cf = vm.check_flag(Carry);
            let mut cf = false;

            if vm.check_flag(AuxCarry) || (old_al & 0x0F) >= 0x0A {
                let (new_al, borrow) = vm.registers.ax.low().overflowing_sub(0x06);
                vm.registers.ax.set_low(new_al);
                cf = old_cf || borrow;
                vm.set_flag(AuxCarry);
            } else {
                vm.unset_flag(AuxCarry);
            }
            if old_cf || old_al > 0x99 {
                let new_al = vm.registers.ax.low().wrapping_sub(0x60);
                vm.registers.ax.set_low(new_al);
                cf = true;
            }
            vm.update_flag(Carry, cf);
            let al = vm.registers.ax.low();
            vm.update_flag(Zero, al == 0);
            vm.update_flag(Sign, (al as i8) < 0);
            vm.update_flag(Parity, al.count_ones() & 1 == 0);
        }
        // (GRP) DEC, INC, CALL, JMP, PUSH
        0b_1111_1110 | 0b_1111_1111 => {
            control::group_fe_ff(vm, is_word);
        }
        // DEC reg
        0b_0100_1000..=0b_0100_1111 => {
            let reg = opcode & 0b111;
            let rh = vm.registers.ref_reg_word(reg);
            let prev = rh.word();
            let (res, overflow, _) = prev.oc_sub(1);

            vm.update_flag(Overflow, overflow);
            vm.update_flag(Zero, res == 0);
            vm.update_flag(Sign, (res as i16) < 0);
            vm.update_flag(Parity, (res as u8).count_ones() & 1 == 0);
            vm.update_flag(AuxCarry, (prev & 0xF) < 1);

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
            if vm.mode == ExecutionMode::BiosBoot {
                // Rewind PC so the run loop can process interrupts before retrying.
                // When a hardware interrupt fires, its handler runs via IRET and
                // execution resumes at the instruction *after* HLT naturally.
                vm.registers.pc.set(vm.registers.op_pc);
                std::thread::sleep(std::time::Duration::from_millis(1));
            } else {
                vm.exit(0);
            }
        }
        // IN ac,DX
        0xEC | 0xED => {
            let port = vm.registers.dx.word();
            if is_word {
                let val = if let Some(ref mut io_bus) = vm.io_bus {
                    io_bus.port_in_word(port)
                } else { 0xFFFF };
                vm.registers.ax.set(val);
            } else {
                let val = if let Some(ref mut io_bus) = vm.io_bus {
                    io_bus.port_in_byte(port)
                } else { 0xFF };
                vm.registers.ax.set_low(val);
            }
        }
        // IN ac,imm8
        0xE4 | 0xE5 => {
            let port = vm.fetch_byte() as u16;
            if is_word {
                let val = if let Some(ref mut io_bus) = vm.io_bus {
                    io_bus.port_in_word(port)
                } else { 0xFFFF };
                vm.registers.ax.set(val);
            } else {
                let val = if let Some(ref mut io_bus) = vm.io_bus {
                    io_bus.port_in_byte(port)
                } else { 0xFF };
                vm.registers.ax.set_low(val);
            }
        }
        // INC reg
        0b_0100_0000..=0b_0100_0111 => {
            let reg = opcode & 0b111;
            let rh = vm.registers.ref_reg_word(reg);
            let prev = rh.word();
            let (res, overflow, _) = prev.oc_add(1);

            vm.update_flag(Overflow, overflow);
            vm.update_flag(Zero, res == 0);
            vm.update_flag(Sign, (res as i16) < 0);
            vm.update_flag(Parity, (res as u8).count_ones() & 1 == 0);
            vm.update_flag(AuxCarry, (prev & 0xF) + 1 > 0xF);

            rh.set(res);
        }
        // INT 3 (breakpoint)
        0xCC => {
            let vector: u8 = 3;
            control::dispatch_int(vm, vector);
        }
        // INT imm8
        0xCD => {
            let vector = vm.fetch_byte();
            control::dispatch_int(vm, vector);
        }
        // INTO (Interrupt on Overflow)
        0xCE => {
            if vm.check_flag(Overflow) {
                control::dispatch_int(vm, 4);
            }
        }
        // IRET
        0xCF => {
            let pc = vm.pop_word();
            let cs = vm.pop_word();
            let flags = vm.pop_word();
            vm.registers.pc.set(pc);
            vm.registers.cs.reg_mut().set(cs);
            vm.flags = flags;
        }
        // JMP CONDITIONAL disp
        0x70..=0x7F => {
            let disp = vm.fetch_byte() as i8;
            if (match (opcode >> 1) & 0b_111 {
                // JO / JNO
                0b_000 => vm.check_flag(Overflow),
                // JB,JNEA / JAE,JNB
                0b_001 => vm.check_flag(Carry),
                // JE,JZ / JNE,JNZ
                0b_010 => vm.check_flag(Zero),
                // JBE,JNS / JA,JNBE
                0b_011 => vm.check_flag(Carry) || vm.check_flag(Zero),
                // JS / JNS
                0b_100 => vm.check_flag(Sign),
                // JP,JPE / JNP,JPO
                0b_101 => vm.check_flag(Parity),
                // JL,JNGE / JGE,JNL
                0b_110 => vm.check_flag(Sign) != vm.check_flag(Overflow),
                // JLE,JNG / JG,JNLE
                0b_111 => vm.check_flag(Zero) || vm.check_flag(Sign) != vm.check_flag(Overflow),
                _ => unreachable!()
            }) != is_word {
                vm.registers.pc.operation(disp as i16, u16::wrapping_add_signed);
            }
        }
        // JCXZ disp
        0xE3 => {
            let disp = vm.fetch_byte() as i8;
            if vm.registers.cx.word() == 0 {
                vm.registers.pc.operation(disp as i16, u16::wrapping_add_signed);
            }
        }
        // JMP addr
        0xEA => {
            let pc = vm.fetch_word();
            let cs = vm.fetch_word();
            vm.registers.pc.set(pc);
            vm.registers.cs.reg_mut().set(cs);
        }
        // JMP disp
        0xEB => {
            let disp = vm.fetch_byte() as i8;
            vm.registers.pc.operation(disp as i16, u16::wrapping_add_signed);
        }
        // LAHF
        0x9F => {
            vm.registers.ax.set_high(vm.flags as u8);
        }
        // LDS reg, mem
        0xC5 => {
            let mod_rm = vm.fetch_byte();
            let rm = mod_rm & 0b111;
            let mod_val = (mod_rm >> 6) & 0b11;

            let address: u16 = match mod_val {
                0b00 => direct_address(vm, rm),
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    address.wrapping_add_signed(displacement)
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    address.wrapping_add(displacement)
                }
                0b11 => vm.registers.read_reg_word(rm),
                _ => unreachable!()
            };
            let offset = vm.effective_segment(mod_val, rm).read_word(address);
            let segment = vm.effective_segment(mod_val, rm).read_word(address.wrapping_add(2));
            vm.registers.ref_reg_word((mod_rm >> 3) & 0b111).set(offset);
            vm.registers.ds.reg_mut().set(segment);
        }
        // LEA
        0x8D => {
            let mod_rm = vm.fetch_byte();
            let rm = mod_rm & 0b111;

            let address: u16 = match (mod_rm >> 6) & 0b11 {
                0b00 => {
                    direct_address(vm, rm)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    address.wrapping_add_signed(displacement)
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    address.wrapping_add(displacement)
                }
                0b11 => vm.registers.read_reg_word(rm),
                _ => unreachable!()
            };
            vm.registers.ref_reg_word((mod_rm >> 3) & 0b111).set(address);
        }
        // LES
        0xC4 => {
            let mod_rm = vm.fetch_byte();
            let rm = mod_rm & 0b111;
            let mod_val = (mod_rm >> 6) & 0b11;

            let address: u16 = match mod_val {
                0b00 => {
                    direct_address(vm, rm)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    address.wrapping_add_signed(displacement)
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    address.wrapping_add(displacement)
                }
                0b11 => vm.registers.read_reg_word(rm),
                _ => unreachable!()
            };
            let word = vm.effective_segment(mod_val, rm).read_word(address);
            vm.registers.ref_reg_word((mod_rm >> 3) & 0b111).set(word);
            let word = vm.effective_segment(mod_val, rm).read_word(address.wrapping_add(2));
            vm.registers.es.reg_mut().set(word);
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
            let disp = vm.fetch_byte() as i8 as i16;
            vm.registers.cx.operation(1, u16::wrapping_sub);

            if vm.registers.cx.word() != 0 {
                vm.registers.pc.operation(disp, u16::wrapping_add_signed);
            }
        }
        // LOOPZ disp, LOOPE disp
        0xE1 => {
            let disp = vm.fetch_byte() as i8 as i16;
            vm.registers.cx.operation(1, u16::wrapping_sub);

            if vm.registers.cx.word() != 0 && vm.check_flag(Zero) {
                vm.registers.pc.operation(disp, u16::wrapping_add_signed);
            }
        }
        // LOOPNZ disp, LOOPNE disp
        0xE0 => {
            let disp = vm.fetch_byte() as i8 as i16;
            vm.registers.cx.operation(1, u16::wrapping_sub);

            if vm.registers.cx.word() != 0 && !vm.check_flag(Zero) {
                vm.registers.pc.operation(disp, u16::wrapping_add_signed);
            }
        }
        // MOV Mod R/M
        0b_1000_1000..=0b_1000_1011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                modrm.set(word);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                modrm.set(byte);
            }
        }
        // MOV data,reg
        0b_1011_0000..=0b_1011_1111 => {
            if opcode & 0b_0000_1000 != 0 {
                let reg = vm.registers.ref_reg_word(opcode & 0b111);
                let word = vm.fetch_word();
                reg.set(word);
            } else {
                let reg = vm.registers.ref_reg_byte(opcode & 0b111);
                let byte = vm.fetch_byte();
                reg.set(byte);
            }
        }
        // MOV ac,mem (moffs - always 16-bit address)
        0b_1010_0000 | 0b_1010_0001 => {
            let address = vm.fetch_word();
            if is_word {
                let word = vm.data_segment().read_word(address);
                vm.registers.ax.set(word);
            } else {
                let byte = vm.data_segment().read_byte(address);
                vm.registers.ax.set_low(byte);
            }
        }
        // MOV mem,ac
        0b_1010_0010 | 0b_1010_0011 => {
            let address = vm.fetch_word();
            if is_word {
                let word = vm.registers.ax.word();
                vm.data_segment().write_word(address, word);
            } else {
                let byte = vm.registers.ax.low();
                vm.data_segment().write_byte(address, byte);
            }
        }
        // MOV segreg,mem/reg
        0x8E => {
            let mod_rm = vm.fetch_byte();
            let rm = mod_rm & 0b111;
            let mod_val = (mod_rm >> 6) & 0b11;

            let word = match mod_val {
                0b00 => {
                    let address = direct_address(vm, rm);
                    vm.effective_segment(0b00, rm).read_word(address)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    vm.effective_segment(0b01, rm).read_word(address.wrapping_add_signed(displacement))
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    vm.effective_segment(0b10, rm).read_word(address.wrapping_add(displacement))
                }
                0b11 => vm.registers.read_reg_word(rm),
                _ => unreachable!()
            };
            vm.get_segment(SegmentType::from((mod_rm >> 3) & 0b011))
                .reg_mut()
                .set(word);
        }
        // MOV mem/reg,segreg
        0x8C => {
            let (modrm, reg) = u16::mod_rm_single(vm);
            let word = vm.get_segment(SegmentType::from((reg) & 0b011))
                .reg()
                .word();
            modrm.set(word);
        }
        // MOV mem/reg,data
        0b_1100_0110 | 0b_1100_0111 => {
            if is_word {
                let (modrm, reg) = u16::mod_rm_single(vm);
                let word = vm.fetch_word();

                match reg & 0b_111 {
                    0b_000 => modrm.set(word),
                    _ => unreachable!()
                }
            } else {
                let (modrm, reg) = u8::mod_rm_single(vm);
                let byte = vm.fetch_byte();

                match reg & 0b_111 {
                    0b_000 => modrm.set(byte),
                    _ => unreachable!()
                }
            }
        }
        // MOVS
        0b_1010_0100 | 0b_1010_0101 => {
            string::movs(vm, is_word);
        }
        // NOP
        0x90 => {}
        // OR ac,data
        0b_0000_1100 | 0b_0000_1101 => {
            if is_word {
                let word = vm.fetch_word();
                let word = vm.registers.ax.operation(word, u16::bitor);

                update_logical_flags_word(vm, word);
            } else {
                let byte = vm.fetch_byte();
                let byte = vm.registers.ax.ref_mut_low().operation(byte, u8::bitor);

                update_logical_flags_byte(vm, byte);
            }
        }
        // OR Mod/RM
        0b_0000_1000..=0b_0000_1011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let word = modrm.operation(word, u16::bitor);

                update_logical_flags_word(vm, word);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let byte = modrm.operation(byte, u8::bitor);

                update_logical_flags_byte(vm, byte);
            }
        }
        // OUT DX,ac
        0xEE | 0xEF => {
            let port = vm.registers.dx.word();
            if is_word {
                let val = vm.registers.ax.word();
                if let Some(ref mut io_bus) = vm.io_bus {
                    io_bus.port_out_word(port, val);
                }
            } else {
                let val = vm.registers.ax.low();
                if let Some(ref mut io_bus) = vm.io_bus {
                    io_bus.port_out_byte(port, val);
                }
            }
        }
        // OUT imm8,ac
        0xE6 | 0xE7 => {
            let port = vm.fetch_byte() as u16;
            if is_word {
                let val = vm.registers.ax.word();
                if let Some(ref mut io_bus) = vm.io_bus {
                    io_bus.port_out_word(port, val);
                }
            } else {
                let val = vm.registers.ax.low();
                if let Some(ref mut io_bus) = vm.io_bus {
                    io_bus.port_out_byte(port, val);
                }
            }
        }
        // POP mem/reg
        0x8F => {
            let word = vm.pop_word();

            let mod_rm = vm.fetch_byte();
            let rm = mod_rm & 0b111;
            let mod_val = (mod_rm >> 6) & 0b11;

            match mod_val {
                0b00 => {
                    let address = direct_address(vm, rm);
                    vm.effective_segment(0b00, rm).ref_word(address)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    vm.effective_segment(0b01, rm).ref_word(address.wrapping_add_signed(displacement))
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    vm.effective_segment(0b10, rm).ref_word(address.wrapping_add(displacement))
                }
                0b11 => vm.registers.ref_reg_word(rm),
                _ => unreachable!()
            }.set(word);
        }
        // POP reg
        0b_0101_1000..=0b_0101_1111 => {
            let word = vm.pop_word();
            vm.registers.ref_reg_word(opcode & 0b111).set(word);
        }
        // POP sreg
        0b_0000_0111 | 0b_0001_0111 | 0b_0001_1111 => {
            let word = vm.pop_word();
            match (opcode >> 3) & 0b11 {
                0b00 => vm.registers.es.reg_mut(),
                0b10 => vm.registers.ss.reg_mut(),
                0b11 => vm.registers.ds.reg_mut(),
                _ => unreachable!(),
            }.set(word);
        }
        // POPF
        0x9D => {
            vm.flags = vm.pop_word();
        }
        // PUSH reg
        0b_0101_0000..=0b_0101_0111 => {
            let reg = opcode & 0b111;
            vm.push_word(vm.registers.read_reg_word(reg));
        }
        // PUSH sreg
        0b_0000_0110 | 0b_0000_1110 | 0b_0001_0110 | 0b_0001_1110 => {
            let word = match (opcode >> 3) & 0b11 {
                0b00 => vm.registers.es.reg_mut(),
                0b01 => vm.registers.cs.reg_mut(),
                0b10 => vm.registers.ss.reg_mut(),
                0b11 => vm.registers.ds.reg_mut(),
                _ => unreachable!(),
            }.word();
            vm.push_word(word);
        }
        // PUSHF
        0x9C => {
            vm.push_word(vm.flags);
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

            vm.registers.pc.set(pc);
            vm.registers.cs.reg_mut().set(cs);
        }
        // RET
        0xC3 => {
            let pc = vm.pop_word();
            vm.registers.pc.set(pc);
        }
        // RET disp16
        0xCA => {
            let disp = vm.fetch_word();
            let pc = vm.pop_word();
            let cs = vm.pop_word();

            vm.registers.pc.set(pc);
            vm.registers.cs.reg_mut().set(cs);
            vm.registers.sp.operation(disp, u16::add);
        }
        // RET disp16
        0xC2 => {
            let disp = vm.fetch_word();
            let pc = vm.pop_word();

            vm.registers.pc.set(pc);
            vm.registers.sp.operation(disp, u16::add);
        }
        // SAHF
        0x9E => {
            vm.flags = (vm.flags & 0xFF00) | (vm.registers.ax.high() as u16);
        }
        // SBB ac,imm
        0b_0001_1100 | 0b_0001_1101 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let cf = vm.check_flag(Carry) as u16;
                let (res, overflow, carry) = ax.oc_carry_sub(word, cf != 0);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (ax & 0xF) < (word & 0xF) + cf);
                vm.registers.ax.set(res);
            } else {
                let byte = vm.fetch_byte();
                let al = vm.registers.ax.low();
                let cf = vm.check_flag(Carry) as u8;
                let (res, overflow, carry) = al.oc_carry_sub(byte, cf != 0);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (al & 0xF) < (byte & 0xF) + cf);
                vm.registers.ax.set_low(res);
            }
        }
        // Group 80-83: SBB, SUB, XOR, CMP, AND, ADC, ADD, OR
        0b_1000_0000..=0b_1000_0011 => {
            alu::group_80_83(vm, opcode, is_word, directional);
        }
        // SBB modrm
        0b_0001_1000..=0b_0001_1011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let prev = modrm.word();
                let cf = vm.check_flag(Carry) as u16;
                let (res, overflow, carry) = prev.oc_carry_sub(word, cf != 0);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (prev & 0xF) < (word & 0xF) + cf);

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let prev = modrm.byte();
                let cf = vm.check_flag(Carry) as u8;
                let (res, overflow, carry) = prev.oc_carry_sub(byte, cf != 0);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (prev & 0xF) < (byte & 0xF) + cf);

                modrm.set(res);
            }
        }
        // SCAS
        0b_1010_1110 | 0b_1010_1111 => {
            string::scas(vm, is_word);
        }
        // SEG
        0b_0010_0110 | 0b_0010_1110 | 0b_0011_0110 | 0b_0011_1110 => {
            vm.prefix = Some(Queued(Box::new(Seg(SegmentType::from((opcode >> 3) & 0b_11)))));
        }
        // STOS
        0b_1010_1010 | 0b_1010_1011 => {
            string::stos(vm, is_word);
        }
        // SUB AL/AX, imm
        0b_0010_1100 | 0b_0010_1101 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let (res, overflow, carry) = ax.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (ax & 0xF) < (word & 0xF));
                vm.registers.ax.set(res);
            } else {
                let byte = vm.fetch_byte();
                let al = vm.registers.ax.low();
                let (res, overflow, carry) = al.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (al & 0xF) < (byte & 0xF));
                vm.registers.ax.set_low(res);
            }
        }
        // SUB modrm
        0b_0010_1000..=0b_0010_1011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (prev & 0xF) < (word & 0xF));

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (prev & 0xF) < (byte & 0xF));

                modrm.set(res);
            }
        }
        // TEST ac,data
        0b_1010_1000 | 0b_1010_1001 => {
            if is_word {
                let word = vm.fetch_word();
                let word = vm.registers.ax.word().bitand(word);

                update_logical_flags_word(vm, word);
            } else {
                let byte = vm.fetch_byte();
                let byte = vm.registers.ax.low().bitand(byte);

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
        0x9B => {
        }
        // XCHNG reg
        0b_1001_0001..=0b_1001_0111 => {
            vm.registers.ref_reg_word(opcode & 0b_111).swap_register(&mut vm.registers.ax);
        }
        // XCHNG Mod R/M
        0b_1000_0110 | 0b_1000_0111 => {
            if is_word {
                let (mut modrm, reg) = u16::mod_rm_single(vm);
                modrm.swap(&mut vm.registers.ref_reg_word(reg));
            } else {
                let (mut modrm, reg) = u8::mod_rm_single(vm);
                modrm.swap(&mut vm.registers.ref_reg_byte(reg));
            }
        }
        // XLAT
        0xD7 => {
            let address = (vm.registers.ax.low() as u16).wrapping_add(vm.registers.bx.word());
            let byte = vm.data_segment().read_byte(address);
            vm.registers.ax.set_low(byte);
        }
        // XOR
        0b_0011_0100 | 0b_0011_0101 => {
            if is_word {
                let word = vm.fetch_word();
                let word = vm.registers.ax.operation(word, u16::bitxor);
                update_logical_flags_word(vm, word);
            } else {
                let byte = vm.fetch_byte();
                let byte = vm.registers.ax.operation_low(byte, u8::bitxor);
                update_logical_flags_byte(vm, byte);
            }
        }
        // XOR
        0b_0011_0000..=0b_0011_0011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let word = modrm.operation(word, u16::bitxor);
                update_logical_flags_word(vm, word);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let byte = modrm.operation(byte, u8::bitxor);
                update_logical_flags_byte(vm, byte);
            }
        }
        opcode => {
            eprintln!("[ERROR] Unknown instruction: {:#04X} at {:04X}:{:04X}",
                      opcode,
                      vm.registers.cs.reg().word(),
                      vm.registers.op_pc);
            vm.exit(1);
        }
    }
}
