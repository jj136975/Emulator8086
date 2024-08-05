use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Not, Sub};
use log::error;
use num_traits::Euclid;
use crate::minix2::interruption::Message;
use crate::minix2::syscall::handle_interrupt;
use crate::utils::number::{div_rem, extend_sign, SpecialOps};
use crate::vm::modrm::{direct_address, ModRM, rm_address};
use crate::vm::runtime::{Prefix, Runtime, SegmentType};
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Prefix::{Queued, Seg};

const WORD_MASK: u8 = 0b_00_00_00_01;
const DIRECTION_MASK: u8 = 0b_00_00_00_10;
// const OPCODE_MASK: u8 = !(WORD_MASK | DIRECTION_MASK);

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

fn div_zero(vm: &mut Runtime) {
    vm.push_word(vm.flags);

    vm.unset_flag(Interrupt);
    vm.unset_flag(Trap);

    vm.push_word(vm.registers.cs.reg().word());
    vm.push_word(vm.registers.pc.word());

    let cs = vm.memory.read_word(0x00002);
    let pc = vm.memory.read_word(0x00000);
    vm.registers.cs.reg_mut().set(cs);
    vm.registers.pc.set(pc);
}

pub fn process(vm: &mut Runtime) {
    vm.registers.op_pc = vm.registers.pc.word();

    let opcode = vm.fetch_byte();
    let is_word: bool = opcode & WORD_MASK != 0;
    let directional: bool = opcode & DIRECTION_MASK != 0;
    if let Some(Queued(prefix)) = vm.prefix.take() {
        vm.prefix = Some(*prefix);
    }

    //println!("{:#06X} | OP: {:#02X}", vm.registers.cs.phys_address(vm.registers.pc.word()), opcode);

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
                vm.registers.ax.operation(0x106, u16::add);
                vm.set_flag(AuxCarry);
                vm.set_flag(Carry);
            } else {
                vm.unset_flag(AuxCarry);
                vm.unset_flag(Carry);
            }
            vm.registers.ax.set_low(al & 0x0F);
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
                vm.registers.ax.operation(0x106, u16::sub);
                vm.set_flag(AuxCarry);
                vm.set_flag(Carry);
            } else {
                vm.unset_flag(AuxCarry);
                vm.unset_flag(Carry);
            }
            vm.registers.ax.set_low(al & 0x0F);
        }
        // ADC ac,data
        0b_0001_0100 | 0b_0001_0101 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let (res, overflow, carry) = ax.oc_carry_add(word, vm.check_flag(Carry));

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (((word & WORD_AUX_CARRY_MASK) + (ax & WORD_AUX_CARRY_MASK)) & WORD_AUX_CARRY_FLAG) != 0);

                vm.registers.ax.set(res);
            } else {
                let byte = vm.fetch_byte();
                let al = vm.registers.ax.low();
                let (res, overflow, carry) = al.oc_carry_add(byte, vm.check_flag(Carry));

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (((byte & BYTE_AUX_CARRY_MASK) + (al & BYTE_AUX_CARRY_MASK)) & BYTE_AUX_CARRY_FLAG) != 0);

                vm.registers.ax.set_low(res);
            }
        }
        // ADC Mod R/M
        0b_0001_0000..=0b_0001_0011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                // let word = vm.fetch_word();

                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_carry_add(word, vm.check_flag(Carry));

                update_arithmetic_flags_word(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (((word & WORD_AUX_CARRY_MASK) + (prev & WORD_AUX_CARRY_MASK)) & WORD_AUX_CARRY_FLAG) != 0);

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };

                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_carry_add(byte, vm.check_flag(Carry));

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                vm.update_flag(AuxCarry, (((byte & BYTE_AUX_CARRY_MASK) + (prev & BYTE_AUX_CARRY_MASK)) & BYTE_AUX_CARRY_FLAG) != 0);

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
            let address = vm.fetch_word() + vm.registers.pc.word();
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
        0xFC => {
            vm.update_flag(Directional, is_word);
        }
        // CLI / STI
        0xFA => {
            vm.update_flag(Interrupt, is_word);
        }
        // CMC
        0xF5 => {
            vm.flip_flag(Carry);
        }
        // CMP
        0b_0011_1100 | 0b_0011_1101 => {
            if is_word {
                let word = vm.fetch_word();
                let (res, overflow, carry) = vm.registers.ax.word().oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                // TODO: check aux carry
            } else {
                let byte = vm.fetch_byte();
                let (res, overflow, carry) = vm.registers.ax.low().oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                // TODO: check aux carry
            }
        }
        // CMP
        0b_0011_1000..=0b_0011_1011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let (res, overflow, carry) = modrm.word().oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                // TODO: check aux carry
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let (res, overflow, carry) = modrm.byte().oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                // TODO: check aux carry
            }
        }
        // CMPS
        0b_1010_0110 | 0b_1010_0111 => {
            if is_word {
                let address = vm.registers.si.word();
                let word1 = vm.data_segment().read_word(address);
                let word2 = vm.registers.es.read_word(vm.registers.di.word());
                let (res, overflow, carry) = word1.oc_sub(word2);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                // TODO: check aux carry

                if vm.check_flag(Directional) {
                    vm.registers.si.operation(2, u16::sub);
                    vm.registers.di.operation(2, u16::sub);
                } else {
                    vm.registers.si.operation(2, u16::add);
                    vm.registers.di.operation(2, u16::add);
                }
            } else {
                let address = vm.registers.si.word();
                let byte1 = vm.data_segment().read_byte(address);
                let byte2 = vm.registers.es.read_byte(vm.registers.di.word());
                let (res, overflow, carry) = byte1.oc_sub(byte2);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                // TODO: check aux carry

                if vm.check_flag(Directional) {
                    vm.registers.si.operation(1, u16::sub);
                    vm.registers.di.operation(1, u16::sub);
                } else {
                    vm.registers.si.operation(1, u16::add);
                    vm.registers.di.operation(1, u16::add);
                }
            }
        }
        // CWD
        0x99 => {
            let ax = vm.registers.ax.word();
            if ax & WORD_SIGN_FLAG != 0 {
                vm.registers.bx.set(0xFFFF);
            } else {
                vm.registers.bx.set(0);
            }
        }
        // DAA
        0x27 => {
            let mut al = vm.registers.ax.low();
            if vm.check_flag(AuxCarry) || al & 0x0F >= 0x0A {
                al += 0x06;
                vm.set_flag(AuxCarry);
            }
            if vm.check_flag(Carry) || al & 0xF0 >= 0xA0 {
                al += 0x60;
                vm.set_flag(Carry);
            }
            vm.registers.ax.set_low(al);
            vm.update_flag(Zero, al == 0);
            vm.update_flag(Sign, (al as i8) < 0);
            vm.update_flag(Parity, al.count_ones() & 1 == 0);
        }
        // DAS
        0x2F => {
            let mut al = vm.registers.ax.low();
            if vm.check_flag(AuxCarry) || al & 0x0F >= 0x0A {
                al -= 0x06;
                vm.set_flag(AuxCarry);
            }
            if vm.check_flag(Carry) || al & 0xF0 >= 0xA0 {
                al -= 0x60;
                vm.set_flag(Carry);
            }
            vm.registers.ax.set_low(al);
            vm.update_flag(Zero, al == 0);
            vm.update_flag(Sign, (al as i8) < 0);
            vm.update_flag(Parity, al.count_ones() & 1 == 0);
        }
        // (GRP) DEC, INC, CALL, JMP, PUSH
        0b_1111_1110 | 0b_1111_1111 => {
            if is_word {
                let (modrm, reg) = u16::mod_rm_single(vm);

                match reg & 0b_111 {
                    // INC
                    0b_000 => {
                        let (w, o, _) = modrm.word().oc_add(1);
                        modrm.set(w);
                        update_arithmetic_flags_word(vm, w, o, vm.check_flag(Carry));
                    }
                    // DEC
                    0b_001 => {
                        let (w, o, _) = modrm.word().oc_sub(1);
                        modrm.set(w);
                        update_arithmetic_flags_word(vm, w, o, vm.check_flag(Carry));
                    }
                    // CALL mem/reg
                    0b_010 => {
                        vm.push_word(vm.registers.pc.word());
                        vm.registers.pc.set(modrm.word());
                    },
                    // CALL mem
                    0b_011 => {
                        vm.push_word(vm.registers.cs.reg().word());
                        vm.push_word(vm.registers.pc.word());
                        vm.registers.cs.reg_mut().set(modrm.word());
                        unsafe { vm.registers.pc.set(modrm.next()); }
                    },
                    // JMP mem/reg
                    0b_100 => vm.registers.pc.set(modrm.word()),
                    // JMP mem
                    0b_101 => {
                        vm.registers.pc.set(modrm.word());
                        unsafe { vm.registers.cs.reg_mut().set(modrm.next()); }
                    },
                    // PUSH
                    0b_110 => vm.push_word(modrm.word()),
                    _ => unreachable!(),
                };
            } else {
                let (modrm, reg) = u8::mod_rm_single(vm);

                match reg & 0b_111 {
                    // INC
                    0b_000 => {
                        let (b, o, _) = modrm.byte().oc_add(1);
                        modrm.set(b);
                        update_arithmetic_flags_byte(vm, b, o, vm.check_flag(Carry));
                    }
                    // DEC
                    0b_001 => {
                        let (b, o, _) = modrm.byte().oc_sub(1);
                        modrm.set(b);
                        update_arithmetic_flags_byte(vm, b, o, vm.check_flag(Carry));
                    }
                    _ => unreachable!(),
                };
            }
        }
        // DEC
        0b_0100_1000..=0b_0100_1111 => {
            let reg = opcode & 0b111;
            let rh = vm.registers.ref_reg_word(reg);
            let (res, overflow, _) = rh.word().oc_sub(1);

            vm.update_flag(Overflow, overflow);
            vm.update_flag(Zero, res == 0);
            vm.update_flag(Sign, (res as i16) < 0);
            vm.update_flag(Parity, (res as u8).count_ones() & 1 == 0);

            rh.set(res);
        }
        // TEST, NOT, NEG, MUL, IMUL, DIV, IDIV
        0b_1111_0110 | 0b_1111_0111 => {
            if is_word {
                let (modrm, reg) = u16::mod_rm_single(vm);

                let (w, o, c) = match reg & 0b_111 {
                    // TEST
                    0b_000 => {
                        let word = vm.fetch_word();
                        (modrm.word().bitand(word), false, false)
                    },
                    // NOT
                    0b_010 => {
                        let res = modrm.word().not();
                        modrm.set(res);
                        (res, false, false)
                    },
                    // NEG
                    0b_011 => {
                        let res = 0u16.oc_sub(modrm.word());
                        modrm.set(res.0);
                        res
                    },
                    // MUL
                    0b_100 => {
                        let res: u32 = (vm.registers.ax.word() as u32) * (modrm.word() as u32);
                        let dx: u16 = (res >> 16) as u16;
                        vm.registers.dx.set(dx);
                        vm.registers.ax.set(res as u16);
                        (res as u16, dx != 0, dx != 0)
                    },
                    // IMUL
                    0b_101 => {
                        let res: i32 = (vm.registers.ax.word() as i32) * (modrm.word() as i32);
                        let dx: u16 = ((res as u32) >> 16) as u16;
                        vm.registers.dx.set(dx);
                        vm.registers.ax.set(res as u16);
                        (res as u16, dx != 0, dx != 0)
                    },
                    // DIV
                    0b_110 => {
                        let numerator: u32 = (vm.registers.dx.word() as u32) << 16 | (vm.registers.ax.word() as u32);
                        let denum = modrm.word() as u32;

                        if denum == 0 {
                            div_zero(vm);
                        } else {
                            let (quot, rem) = numerator.div_rem_euclid(&denum);
                            if quot > u16::MAX as u32 {
                                div_zero(vm);
                            } else {
                                vm.registers.ax.set(quot as u16);
                                vm.registers.dx.set(rem as u16);
                            }
                        }
                        (0, false, false)
                    },
                    // IDIV
                    0b_111 => {
                        let numerator: i32 = ((vm.registers.dx.word() as u32) << 16 | (vm.registers.ax.word() as u32)) as i32;
                        let denum = modrm.word() as u32 as i32;

                        if denum == 0 {
                            div_zero(vm);
                        } else {
                            let (quot, rem) = numerator.div_rem_euclid(&denum);
                            if quot > 0x7FFF {
                                div_zero(vm);
                            } else {
                                vm.registers.ax.set(quot as i16 as u16);
                                vm.registers.dx.set(rem as i16 as u16);
                            }
                        }
                        (0, false, false)
                    },
                    _ => unreachable!(),
                };
                update_arithmetic_flags_word(vm, w, o, c);
            } else {
                let (modrm, reg) = u8::mod_rm_single(vm);

                let (b, o, c) = match reg & 0b_111 {
                    // TEST
                    0b_000 => {
                        let byte = vm.fetch_byte();
                        (modrm.operation(byte, u8::bitand), false, false)
                    },
                    // NOT
                    0b_010 => {
                        let res = modrm.byte().not();
                        modrm.set(res);
                        (res, false, false)
                    },
                    // NEG
                    0b_011 => {
                        let res = 0u8.oc_sub(modrm.byte());
                        modrm.set(res.0);
                        res
                    },
                    // MUL
                    0b_100 => {
                        let res: u16 = (vm.registers.ax.low() as u16) * (modrm.byte() as u16);
                        vm.registers.ax.set(res);
                        (res as u8, res & 0xF0 != 0, res & 0xF0 != 0)
                    },
                    // IMUL
                    0b_101 => {
                        let res: i16 = (vm.registers.ax.low() as i8 as i16) * (modrm.byte() as i8 as i16);
                        vm.registers.ax.set(res as u16);
                        (res as u8, res & 0xF0 != 0, res & 0xF0 != 0)
                    },
                    // DIV
                    0b_110 => {
                        let numerator: u16 = vm.registers.ax.word();
                        let denum = modrm.byte() as u16;

                        if denum == 0 {
                            div_zero(vm);
                        } else {
                            let (quot, rem) = numerator.div_rem_euclid(&denum);
                            if quot > u8::MAX as u16 {
                                div_zero(vm);
                            } else {
                                vm.registers.ax.set_low(quot as u8);
                                vm.registers.ax.set_high(rem as u8);
                            }
                        }
                        (0, false, false)
                    },
                    // IDIV
                    0b_111 => {
                        let numerator: i16 = vm.registers.ax.word() as i16;
                        let denum = modrm.byte() as i8 as i16;

                        if denum == 0 {
                            div_zero(vm);
                        } else {
                            let (quot, rem) = numerator.div_rem_euclid(&denum);
                            if quot > 0x7F {
                                div_zero(vm);
                            } else {
                                vm.registers.ax.set_low(quot as i8 as u8);
                                vm.registers.ax.set_high(rem as i8 as u8);
                            }
                        }
                        (0, false, false)
                    },
                    _ => unreachable!(),
                };
                update_arithmetic_flags_byte(vm, b, o, c);
            }
        }
        // ESC
        0b_1101_1000..=0b_1101_1111 => {
            // TODO No BUS yet
        }
        // HLT
        0xF4 => {
            // TODO Should stop execution, but in our case, we'll exit
            vm.exit(0);
        }
        // IN ac,DX
        0b_11101100 | 0b_11101101 => {
            // TODO Need to read I/O
        }
        // IN ac,port
        0b_11100100 | 0b_11100101 => {
            // TODO Need to read I/O
        }
        // INC
        0b_0100_0000..=0b_0100_0111 => {
            let reg = opcode & 0b111;
            let rh = vm.registers.ref_reg_word(reg);
            let (res, overflow, _) = rh.word().oc_add(1);

            vm.update_flag(Overflow, overflow);
            vm.update_flag(Zero, res == 0);
            vm.update_flag(Sign, (res as i16) < 0);
            vm.update_flag(Parity, (res as u8).count_ones() & 1 == 0);

            rh.set(res);
        }
        // INT
        0b_1100_1100 | 0b_1100_1101 => {
            let _ = vm.fetch_byte();
            let message = Message::new(vm);
            unsafe {
                handle_interrupt(vm, &mut *message);
            }
            // vm.push_word(vm.flags);
            //
            // vm.unset_flag(Interrupt);
            // vm.unset_flag(Trap);
            //
            // vm.push_word(vm.registers.cs.reg().word());
            //
            // let cs_address: usize;
            // let pc_address: usize;
            //
            // if is_word {
            //     pc_address = vm.fetch_byte() as usize * 4;
            //     cs_address = pc_address + 2;
            // } else {
            //     pc_address = 0x0000C;
            //     cs_address = 0x0000E;
            // }
            //
            // let cs: u16 = vm.memory.read_word(cs_address);
            // vm.registers.cs.reg_mut().set(cs);
            // let pc: u16 = vm.memory.read_word(pc_address);
            // vm.registers.pc.set(pc);
        }
        // INT0
        0xCE => {
            vm.push_word(vm.flags);

            vm.unset_flag(Interrupt);
            vm.unset_flag(Trap);

            vm.push_word(vm.registers.cs.reg().word());

            let cs: u16 = vm.memory.read_word(0x00012);
            vm.registers.cs.reg_mut().set(cs);
            let pc: u16 = vm.memory.read_word(0x00010);
            vm.registers.pc.set(pc);
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
            vm.registers.ax.set_low(vm.flags as u8);
        }
        // LDS
        0xC5 => {
            let (modrm, word) = u16::mod_rm_lhs(vm);
            modrm.set(word);
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
                    address.checked_add_signed(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement))
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    address.checked_add(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement))
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

            let address: u16 = match (mod_rm >> 6) & 0b11 {
                0b00 => {
                    direct_address(vm, rm)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    address.checked_add_signed(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement))
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    address.checked_add(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement))
                }
                0b11 => vm.registers.read_reg_word(rm),
                _ => unreachable!()
            };
            let word = vm.data_segment().read_word(address);
            vm.registers.ref_reg_word((mod_rm >> 3) & 0b111).set(word);
            let word = vm.data_segment().read_word(address + 2);
            vm.registers.es.reg_mut().set(word);
        }
        // LOCK
        0xF0 => {
            vm.prefix = Some(Queued(Box::new(Prefix::Lock)));
            // TODO No BUS yet
        }
        // LODS
        0b_1010_1100 | 0b_1010_1101 => {
            let address = vm.registers.si.word();
            if is_word {
                let word = vm.data_segment().read_word(address);
                vm.registers.ax.set(word);
                if !vm.check_flag(Directional) {
                    vm.registers.si.operation(2, u16::add);
                } else {
                    vm.registers.si.operation(2, u16::sub);
                }
            } else {
                let byte = vm.data_segment().read_byte(address);
                vm.registers.ax.set_low(byte);
                if !vm.check_flag(Directional) {
                    vm.registers.si.operation(1, u16::add);
                } else {
                    vm.registers.si.operation(1, u16::sub);
                }
            }
        }
        // LOOP
        0xE2 => {
            let disp = extend_sign(vm.fetch_byte());
            vm.registers.cx.operation(1, u16::sub);

            if vm.registers.cx.word() != 0 {
                vm.registers.pc.set(vm.registers.op_pc + disp);
            }
        }
        // LOOPZ disp, LOOPE disp
        0xE1 => {
            let disp = extend_sign(vm.fetch_byte());
            vm.registers.cx.operation(1, u16::sub);

            if vm.registers.cx.word() != 0 && vm.check_flag(Zero) {
                vm.registers.pc.set(vm.registers.op_pc + disp);
            }
        }
        // LOOPNZ disp, LOOPNE disp
        0xE0 => {
            let disp = extend_sign(vm.fetch_byte());
            vm.registers.cx.operation(1, u16::sub);

            if vm.registers.cx.word() != 0 && !vm.check_flag(Zero) {
                vm.registers.pc.set(vm.registers.op_pc + disp);
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
        // MOV ac,mem
        0b_1010_0000 | 0b_1010_0001 => {
            if is_word {
                let word = vm.fetch_word();
                vm.registers.ax.set(word);
            } else {
                let byte = vm.fetch_byte();
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

            let word = match (mod_rm >> 6) & 0b11 {
                0b00 => {
                    let address = direct_address(vm, rm);
                    vm.data_segment().read_word(address)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    vm.data_segment().read_word(address.checked_add_signed(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    vm.data_segment().read_word(address.checked_add(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
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
            let address = vm.registers.si.word();
            if is_word {
                let word = vm.data_segment().read_word(address);
                vm.registers.es.write_word(vm.registers.di.word(), word);
                vm.registers.si.operation(2, u16::add);
                vm.registers.di.operation(2, u16::add);
            } else {
                let byte = vm.data_segment().read_byte(address);
                vm.registers.es.write_byte(vm.registers.di.word(), byte);
                vm.registers.si.operation(1, u16::add);
                vm.registers.di.operation(1, u16::add);
            }
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
        // OUT
        0b_1110_1110 | 0b_1110_1111 => {
            // TODO No Ports yet
        }
        // OUT
        0b_1110_0110 | 0b_1110_0111 => {
            let _ = vm.fetch_byte();
            // TODO No Ports yet
        }
        // POP mem/reg
        0x8F => {
            let word = vm.pop_word();

            let mod_rm = vm.fetch_byte();
            let rm = mod_rm & 0b111;

            match (mod_rm >> 6) & 0b11 {
                0b00 => {
                    let address = direct_address(vm, rm);
                    vm.data_segment().ref_word(address)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    vm.data_segment().ref_word(address.checked_add_signed(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    vm.data_segment().ref_word(address.checked_add(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
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
            let count = if !directional { 1 } else { vm.registers.cx.low() as u32 };
            if count != 0 {
                if is_word {
                    let (modrm, reg) = u16::mod_rm_single(vm);
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
                        // SHL | SAL
                        0b_100 | 0b_110 => (word.wrapping_shl(count), word & (1u16.wrapping_shl(u16::BITS - count)) != 0),
                        // SHR
                        0b_101 => (word.wrapping_shr(count), word & (1u16.wrapping_shl(count - 1)) != 0),
                        // SAR
                        0b_111 => ((word as i16).wrapping_shr(count) as u16, word & (1u16.wrapping_shl(count - 1)) != 0),
                        _ => unreachable!()
                    };
                    if reg & 0b_100 != 0 {
                        update_arithmetic_flags_word(vm, w, ((word ^ w) as i16) < 0, c);
                    } else {
                        vm.update_flag(Carry, c);
                        vm.update_flag(Overflow, ((word ^ w) as i16) < 0);
                    }
                    modrm.set(w);
                } else {
                    let (modrm, reg) = u8::mod_rm_single(vm);
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
                        // SHL | SAL
                        0b_100 | 0b_110 => (byte.wrapping_shl(count), byte & (1u8.wrapping_shl(u8::BITS - count)) != 0),
                        // SHR
                        0b_101 => (byte.wrapping_shr(count), byte & (1u8.wrapping_shl(count - 1)) != 0),
                        // SAR
                        0b_111 => ((byte as i8).wrapping_shr(count) as u8, byte & (1u8.wrapping_shl(count - 1)) != 0),
                        _ => unreachable!()
                    };
                    if reg & 0b_100 != 0 {
                        update_arithmetic_flags_byte(vm, b, ((byte ^ b) as i8) < 0, c);
                    } else {
                        vm.update_flag(Carry, c);
                        vm.update_flag(Overflow, ((byte ^ b) as i8) < 0);
                    }
                    modrm.set(b);
                }
            }
        }
        // REP | REPE | REPNE | REPNZ | REPZ
        0b_1111_0010 | 0b_1111_0011 => {
            // Do nothing, this is a prefix.
            // We'll fetch the previous instruction when
            // a string operation occurs.

            // no z impact: MOVS,LODS,STOS
            // z impact: CMPS,SCAS
            vm.prefix = Some(Queued(Box::new(Prefix::Rep(is_word))));
            // TODO
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
            vm.flags = (vm.flags & 0xFF00) | (vm.registers.ax.low() as u16);
        }
        // SBB
        0b_0001_1100 | 0b_0001_1101 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let (res, overflow, carry) = ax.oc_carry_sub(word, vm.check_flag(Carry));

                update_arithmetic_flags_word(vm, res, overflow, carry);
                // TODO: check aux carry
            } else {
                let byte = vm.fetch_byte();
                let al = vm.registers.ax.low();
                let (res, overflow, carry) = al.oc_carry_sub(byte, vm.check_flag(Carry));

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                // TODO: check aux carry
            }
        }
        // SBB, SUB, XOR, CMP, AND, ADC
        0b_1000_0000..=0b_1000_0011 => {
            if is_word || directional {
                let (modrm, reg) = u16::mod_rm_single(vm);
                let word = if directional {
                    extend_sign(vm.fetch_byte())
                } else {
                    vm.fetch_word()
                };

                let (w, o, c) = match reg & 0b_111 {
                    // ADD
                    0b_000 => modrm.word().oc_add(word),
                    // OR
                    0b_001 => (modrm.operation(word, u16::bitor), false, false),
                    // ADC
                    0b_010 => modrm.word().oc_carry_add(word, vm.check_flag(Carry)),
                    // SBB
                    0b_011 => modrm.word().oc_carry_sub(word, vm.check_flag(Carry)),
                    // AND
                    0b_100 => (modrm.operation(word, u16::bitand), false, false),
                    // SUB
                    0b_101 => modrm.word().oc_sub(word),
                    // XOR
                    0b_110 => (modrm.operation(word, u16::bitxor), false, false),
                    // CMP
                    0b_111 => modrm.word().oc_sub(word),

                    _ => unreachable!()
                };
                // TODO: check aux carry
                update_arithmetic_flags_word(vm, w, o, c);
                if reg != 0b_111 {
                    modrm.set(w);
                }
            } else {
                let (modrm, reg) = u8::mod_rm_single(vm);
                let byte = vm.fetch_byte();

                let (b, o, c) = match reg & 0b_111 {
                    // ADD
                    0b_000 => modrm.byte().oc_add(byte),
                    // OR
                    0b_001 => (modrm.operation(byte, u8::bitor), false, false),
                    // ADC
                    0b_010 => modrm.byte().oc_carry_add(byte, vm.check_flag(Carry)),
                    // SBB
                    0b_011 => modrm.byte().oc_carry_sub(byte, vm.check_flag(Carry)),
                    // AND
                    0b_100 => (modrm.operation(byte, u8::bitand), false, false),
                    // SUB
                    0b_101 => modrm.byte().oc_sub(byte),
                    // XOR
                    0b_110 => (modrm.operation(byte, u8::bitxor), false, false),
                    // CMP
                    0b_111 => modrm.byte().oc_sub(byte),
                    _ => unreachable!()
                };
                // TODO: check aux carry
                update_arithmetic_flags_byte(vm, b, o, c);
                if reg & 0b_111 != 0b_111 {
                    modrm.set(b);
                }
            }
        }
        // SBB
        0b_0001_1000..=0b_0001_1011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_carry_sub(word, vm.check_flag(Carry));

                update_arithmetic_flags_word(vm, res, overflow, carry);
                // TODO: check aux carry

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_carry_sub(byte, vm.check_flag(Carry));

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                // TODO: check aux carry

                modrm.set(res);
            }
        }
        // SCAS
        0b_1010_1110 | 0b_1010_1111 => {
            let address = vm.registers.si.word();
            if is_word {
                let word = vm.data_segment().read_word(address);
                let (res, overflow, carry) = vm.registers.ax.word().oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                // TODO: check aux carry

                if vm.check_flag(Directional) {
                    vm.registers.di.operation(2, u16::sub);
                } else {
                    vm.registers.di.operation(2, u16::add);
                }
            } else {
                let byte = vm.data_segment().read_byte(address);
                let (res, overflow, carry) = vm.registers.ax.low().oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                // TODO: check aux carry

                if vm.check_flag(Directional) {
                    vm.registers.di.operation(1, u16::sub);
                } else {
                    vm.registers.di.operation(1, u16::add);
                }
            }
        }
        // SEG
        0b_0010_0110 | 0b_0010_1110 | 0b_0011_0110 | 0b_0011_1110 => {
            vm.prefix = Some(Queued(Box::new(Seg(SegmentType::from((opcode >> 3) & 0b_11)))));
        }
        // STOS
        0b_1010_1010 | 0b_1010_1011 => {
            let address = vm.registers.si.word();
            if is_word {
                let word = vm.registers.ax.word();
                vm.data_segment().write_word(address, word);
                if !vm.check_flag(Directional) {
                    vm.registers.di.operation(2, u16::add);
                } else {
                    vm.registers.di.operation(2, u16::sub);
                }
            } else {
                let byte = vm.registers.ax.low();
                vm.data_segment().write_byte(address, byte);
                if !vm.check_flag(Directional) {
                    vm.registers.di.operation(1, u16::add);
                } else {
                    vm.registers.di.operation(1, u16::sub);
                }
            }
        }
        // SBB
        0b_0010_1100 | 0b_0010_1101 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let (res, overflow, carry) = ax.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                // TODO: check aux carry
            } else {
                let byte = vm.fetch_byte();
                let al = vm.registers.ax.low();
                let (res, overflow, carry) = al.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                // TODO: check aux carry
            }
        }
        // SBB
        0b_0010_1000..=0b_0010_1011 => {
            if is_word {
                let (modrm, word) = if directional { u16::mod_rm_lhs(vm) } else { u16::mod_rm_rhs(vm) };
                let prev = modrm.word();
                let (res, overflow, carry) = prev.oc_sub(word);

                update_arithmetic_flags_word(vm, res, overflow, carry);
                // TODO: check aux carry

                modrm.set(res);
            } else {
                let (modrm, byte) = if directional { u8::mod_rm_lhs(vm) } else { u8::mod_rm_rhs(vm) };
                let prev = modrm.byte();
                let (res, overflow, carry) = prev.oc_sub(byte);

                update_arithmetic_flags_byte(vm, res, overflow, carry);
                // TODO: check aux carry

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
            // TODO No signals yet
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
            let address = (vm.registers.ax.low() as u16) + vm.registers.bx.word();
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
        opcode =>  {
            error!("Unknown instruction: {:#02X}", opcode);
            vm.exit(1);
        }
    }
}