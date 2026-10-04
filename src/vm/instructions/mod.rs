mod alu;
pub mod control;
mod shift;
mod string;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod regression_tests;

use crate::utils::number::{div_rem, SpecialOps};
use crate::vm::modrm::{decode_rm, reg_field, ModRM};
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::{Prefixes, Runtime, SegmentType};

const WORD_MASK: u8 = 0b_00_00_00_01;
const DIRECTION_MASK: u8 = 0b_00_00_00_10;

const WORD_SIGN_FLAG: u16 = 0b_1000_0000_0000_0000;
const BYTE_SIGN_FLAG: u8 = 0b_1000_0000;

/// Sets ZF, SF and PF from a result.
#[inline]
fn update_result_flags<T: SpecialOps>(vm: &mut Runtime, res: T) {
    vm.cpu.update_flag(Zero, res == T::ZERO);
    vm.cpu.update_flag(Sign, res.is_negative());
    vm.cpu.update_flag(Parity, res.low_byte().count_ones() & 1 == 0);
}

/// Sets OF and CF as given, and ZF, SF and PF from the result.
#[inline]
fn update_arithmetic_flags<T: SpecialOps>(vm: &mut Runtime, res: T, overflow: bool, carry: bool) {
    vm.cpu.update_flag(Overflow, overflow);
    vm.cpu.update_flag(Carry, carry);
    update_result_flags(vm, res);
}

/// Clears OF and CF, and sets ZF, SF and PF from the result.
#[inline]
fn update_logical_flags<T: SpecialOps>(vm: &mut Runtime, res: T) {
    vm.cpu.unset_flag(Overflow);
    vm.cpu.unset_flag(Carry);
    update_result_flags(vm, res);
}

pub fn process(vm: &mut Runtime) {
    if vm.prefixes.pending {
        // The previous call consumed a prefix byte: this byte continues the
        // same instruction, so the latched prefixes stay in force.
        vm.prefixes.pending = false;
    } else {
        // A new instruction starts here. Remember where, including any
        // prefix bytes, so an interrupted REP string op can be restarted.
        vm.prefixes = Prefixes::default();
        vm.cpu.registers.op_start_pc = vm.cpu.registers.pc.word();
    }

    let opcode = vm.cpu.fetch_byte();
    let is_word: bool = opcode & WORD_MASK != 0;
    let directional: bool = opcode & DIRECTION_MASK != 0;

    match opcode {
        // ADD, OR, ADC, SBB, AND, SUB, XOR and CMP. Bits 3-5 of the opcode
        // select the operation; x0-x3 (x8-xB) take an r/m operand and a
        // register, x4-x5 (xC-xD) the accumulator and an immediate.
        0x00..=0x03 => alu::mod_rm(vm, alu::ADD, is_word, directional),
        0x04 | 0x05 => alu::accumulator_immediate(vm, alu::ADD, is_word),
        0x08..=0x0B => alu::mod_rm(vm, alu::OR, is_word, directional),
        0x0C | 0x0D => alu::accumulator_immediate(vm, alu::OR, is_word),
        0x10..=0x13 => alu::mod_rm(vm, alu::ADC, is_word, directional),
        0x14 | 0x15 => alu::accumulator_immediate(vm, alu::ADC, is_word),
        0x18..=0x1B => alu::mod_rm(vm, alu::SBB, is_word, directional),
        0x1C | 0x1D => alu::accumulator_immediate(vm, alu::SBB, is_word),
        0x20..=0x23 => alu::mod_rm(vm, alu::AND, is_word, directional),
        0x24 | 0x25 => alu::accumulator_immediate(vm, alu::AND, is_word),
        0x28..=0x2B => alu::mod_rm(vm, alu::SUB, is_word, directional),
        0x2C | 0x2D => alu::accumulator_immediate(vm, alu::SUB, is_word),
        0x30..=0x33 => alu::mod_rm(vm, alu::XOR, is_word, directional),
        0x34 | 0x35 => alu::accumulator_immediate(vm, alu::XOR, is_word),
        0x38..=0x3B => alu::mod_rm(vm, alu::CMP, is_word, directional),
        0x3C | 0x3D => alu::accumulator_immediate(vm, alu::CMP, is_word),
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
        // CMPS
        0xA6 => string::cmps::<u8>(vm),
        0xA7 => string::cmps::<u16>(vm),
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
            let reg = vm.cpu.registers.ref_reg_word(opcode & 0b111);
            alu::inc_dec(vm, &reg, true);
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
            let reg = vm.cpu.registers.ref_reg_word(opcode & 0b111);
            alu::inc_dec(vm, &reg, false);
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
                // JB,JNAE / JAE,JNB
                0b_001 => vm.cpu.check_flag(Carry),
                // JE,JZ / JNE,JNZ
                0b_010 => vm.cpu.check_flag(Zero),
                // JBE,JNA / JA,JNBE
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
            let (offset, segment) = decode_rm(vm, mod_rm).as_memory(vm).read_far_pointer(vm);
            vm.cpu.registers.ref_reg_word(reg_field(mod_rm)).set(offset);
            vm.cpu.registers.ds.reg_mut().set(segment);
        }
        // LEA
        0x8D => {
            let mod_rm = vm.cpu.fetch_byte();
            let address = decode_rm(vm, mod_rm).as_memory(vm).offset;
            vm.cpu.registers.ref_reg_word(reg_field(mod_rm)).set(address);
        }
        // LES reg, mem
        0xC4 => {
            let mod_rm = vm.cpu.fetch_byte();
            let (offset, segment) = decode_rm(vm, mod_rm).as_memory(vm).read_far_pointer(vm);
            vm.cpu.registers.ref_reg_word(reg_field(mod_rm)).set(offset);
            vm.cpu.registers.es.reg_mut().set(segment);
        }
        // LOCK (0xF1 is undocumented alias on 8086)
        0xF0 | 0xF1 => {
            vm.prefixes.pending = true;
        }
        // LODS
        0xAC => string::lods::<u8>(vm),
        0xAD => string::lods::<u16>(vm),
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
            let operand = decode_rm(vm, mod_rm);
            let word = u16::read_rm(vm, operand);
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
        0xA4 => string::movs::<u8>(vm),
        0xA5 => string::movs::<u16>(vm),
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
        // BOUND (80186+). The range check is deliberately not performed: its
        // exception is INT 5, which on a PC is the BIOS print-screen service,
        // so a failing BOUND would be re-executed forever. Only the operands
        // are consumed.
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
            vm.prefixes.segment = Some(SegmentType::DS);
            vm.prefixes.pending = true;
        }
        // Operand/Address size override (386+) — ignored: a prefix with no effect
        0x66 | 0x67 => {
            vm.prefixes.pending = true;
        }
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
            let overflow = !(-32768..=32767).contains(&result);
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
            let overflow = !(-32768..=32767).contains(&result);
            vm.cpu.update_flag(Overflow, overflow);
            vm.cpu.update_flag(Carry, overflow);
        }
        // INSB/INSW (80186+) — port [DX] -> ES:[DI]
        0x6C => string::ins::<u8>(vm),
        0x6D => string::ins::<u16>(vm),
        // OUTSB/OUTSW (80186+) — DS:[SI] -> port [DX]
        0x6E => string::outs::<u8>(vm),
        0x6F => string::outs::<u16>(vm),
        // NOP
        0x90 => {}
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
            // The word is popped before the operand is decoded.
            let word = vm.pop_word();
            let (operand, _reg) = u16::mod_rm_single(vm);
            operand.set(word);
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
            if reg == 4 {
                // PUSH SP stores the already-decremented value. This is the
                // 8086's behaviour, kept deliberately: the 80286 and later
                // store the value SP had before the push.
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
            vm.prefixes.rep = Some(is_word);
            vm.prefixes.pending = true;
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
        // RETF imm16
        0xCA => {
            let disp = vm.cpu.fetch_word();
            let pc = vm.pop_word();
            let cs = vm.pop_word();

            vm.cpu.registers.pc.set(pc);
            vm.cpu.registers.cs.reg_mut().set(cs);
            vm.cpu.registers.sp.operation(disp, u16::wrapping_add);
        }
        // RET imm16
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
        // Group 80-83: the same eight operations on an r/m operand and an
        // immediate, selected by the reg field
        0b_1000_0000..=0b_1000_0011 => {
            alu::group_80_83(vm, is_word, directional);
        }
        // SCAS
        0xAE => string::scas::<u8>(vm),
        0xAF => string::scas::<u16>(vm),
        // SEG
        0b_0010_0110 | 0b_0010_1110 | 0b_0011_0110 | 0b_0011_1110 => {
            vm.prefixes.segment = Some(SegmentType::from((opcode >> 3) & 0b_11));
            vm.prefixes.pending = true;
        }
        // STOS
        0xAA => string::stos::<u8>(vm),
        0xAB => string::stos::<u16>(vm),
        // TEST ac,data
        0b_1010_1000 | 0b_1010_1001 => {
            if is_word {
                let word = vm.cpu.fetch_word();
                update_logical_flags(vm, vm.cpu.registers.ax.word() & word);
            } else {
                let byte = vm.cpu.fetch_byte();
                update_logical_flags(vm, vm.cpu.registers.ax.low() & byte);
            }
        }
        // TEST Mod R/M
        0b_1000_0100 | 0b_1000_0101 => {
            if is_word {
                let (modrm, word) = u16::mod_rm_lhs(vm);
                update_logical_flags(vm, modrm.word() & word);
            } else {
                let (modrm, byte) = u8::mod_rm_lhs(vm);
                update_logical_flags(vm, modrm.byte() & byte);
            }
        }
        // WAIT
        0x9B => {}
        // XCHG reg
        0b_1001_0001..=0b_1001_0111 => {
            vm.cpu.registers
                .ref_reg_word(opcode & 0b_111)
                .swap_register(&mut vm.cpu.registers.ax);
        }
        // XCHG Mod R/M
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
    }
}
