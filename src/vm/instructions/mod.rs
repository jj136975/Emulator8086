//! The instruction interpreter. `process` fetches one byte and dispatches on
//! it: each arm of its `match` names the mnemonic and calls into the module
//! of that instruction's domain. Only the prefixes and a few one-line
//! instructions are handled in place. This is the hot path: a function that
//! implements one arm is `#[inline(always)]`, the opcode groups are left to
//! the compiler, and `string` says why its instructions stay out of line.
//!
//! The invariant: a call is one step of decoding, which is not always a
//! whole instruction. A prefix byte latches itself in `Runtime::prefixes`,
//! sets `pending` and returns; the next call continues the same instruction.
//! Whoever wants a whole instruction keeps calling while `prefixes.pending`
//! is set, and recognises no interrupt in between.

mod alu;
mod bcd;
pub mod control;
mod shift;
mod stack;
mod string;
mod transfer;

#[cfg(test)]
mod tests;

use crate::utils::number::SpecialOps;
use crate::vm::modrm::ModRM;
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
    vm.cpu
        .update_flag(Parity, res.low_byte().count_ones() & 1 == 0);
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

/// STI (FBh): sets IF. When that enables interrupts, they stay held off
/// until the instruction after STI has run.
#[inline(always)]
fn sti(vm: &mut Runtime) {
    let was_clear = !vm.cpu.check_flag(Interrupt);
    vm.cpu.set_flag(Interrupt);
    if was_clear {
        vm.interrupt_inhibit = true;
    }
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
        0x37 => bcd::aaa(vm),
        // AAD
        0xD5 => bcd::aad(vm),
        // AAM
        0xD4 => bcd::aam(vm),
        // AAS
        0x3F => bcd::aas(vm),
        // CALL addr
        0x9A => control::call_far(vm),
        // CALL | JMP disp16
        0xE8 | 0xE9 => control::call_or_jmp_near(vm, is_word),
        // CBW
        0x98 => alu::cbw(vm),
        // CLC / STC
        0xF8 | 0xF9 => vm.cpu.update_flag(Carry, is_word),
        // CLD / STD
        0xFC | 0xFD => vm.cpu.update_flag(Directional, is_word),
        // CLI
        0xFA => vm.cpu.unset_flag(Interrupt),
        // STI
        0xFB => sti(vm),
        // CMC
        0xF5 => vm.cpu.flip_flag(Carry),
        // CMPS
        0xA6 => string::cmps::<u8>(vm),
        0xA7 => string::cmps::<u16>(vm),
        // CWD
        0x99 => alu::cwd(vm),
        // DAA
        0x27 => bcd::daa(vm),
        // DAS
        0x2F => bcd::das(vm),
        // (GRP) DEC, INC, CALL, JMP, PUSH
        0b_1111_1110 | 0b_1111_1111 => control::group_fe_ff(vm, is_word),
        // DEC reg
        0b_0100_1000..=0b_0100_1111 => alu::inc_dec_reg(vm, opcode, true),
        // TEST, NOT, NEG, MUL, IMUL, DIV, IDIV
        0b_1111_0110 | 0b_1111_0111 => alu::group_f6_f7(vm, is_word),
        // ESC (FPU instructions - no 8087 coprocessor, consume ModR/M)
        0b_1101_1000..=0b_1101_1111 => {
            let _ = u8::mod_rm_single(vm);
        }
        // HLT
        0xF4 => vm.cpu.halted = true,
        // IN ac,DX
        0xEC | 0xED => transfer::in_dx(vm, is_word),
        // IN ac,imm8
        0xE4 | 0xE5 => transfer::in_imm8(vm, is_word),
        // INC reg
        0b_0100_0000..=0b_0100_0111 => alu::inc_dec_reg(vm, opcode, false),
        // INT 3 (breakpoint)
        0xCC => vm.handle_interrupt(0x03),
        // INT imm8
        0xCD => control::int_imm8(vm),
        // INTO (Interrupt on Overflow)
        0xCE => control::into(vm),
        // IRET
        0xCF => control::iret(vm),
        // JMP CONDITIONAL disp
        0x70..=0x7F => control::jcc(vm, opcode, is_word),
        // JCXZ disp
        0xE3 => control::jcxz(vm),
        // JMP addr
        0xEA => control::jmp_far(vm),
        // JMP disp
        0xEB => control::jmp_short(vm),
        // LAHF
        0x9F => transfer::lahf(vm),
        // LDS reg, mem
        0xC5 => transfer::lds(vm),
        // LEA
        0x8D => transfer::lea(vm),
        // LES reg, mem
        0xC4 => transfer::les(vm),
        // LOCK (0xF1 is undocumented alias on 8086)
        0xF0 | 0xF1 => vm.prefixes.pending = true,
        // LODS
        0xAC => string::lods::<u8>(vm),
        0xAD => string::lods::<u16>(vm),
        // LOOP
        0xE2 => control::loop_(vm),
        // LOOPZ disp, LOOPE disp
        0xE1 => control::loopz(vm),
        // LOOPNZ disp, LOOPNE disp
        0xE0 => control::loopnz(vm),
        // MOV Mod R/M
        0b_1000_1000..=0b_1000_1011 => transfer::mov_mod_rm(vm, is_word, directional),
        // MOV data,reg
        0b_1011_0000..=0b_1011_1111 => transfer::mov_reg_imm(vm, opcode),
        // MOV ac,mem (moffs - always 16-bit address)
        0b_1010_0000 | 0b_1010_0001 => transfer::mov_accumulator_mem(vm, is_word),
        // MOV mem,ac
        0b_1010_0010 | 0b_1010_0011 => transfer::mov_mem_accumulator(vm, is_word),
        // MOV segreg,mem/reg
        0x8E => transfer::mov_sreg_rm(vm),
        // MOV mem/reg,segreg
        0x8C => transfer::mov_rm_sreg(vm),
        // MOV mem/reg,data
        0b_1100_0110 | 0b_1100_0111 => transfer::mov_rm_imm(vm, is_word),
        // MOVS
        0xA4 => string::movs::<u8>(vm),
        0xA5 => string::movs::<u16>(vm),
        // PUSHA (80186+)
        0x60 => stack::pusha(vm),
        // POPA (80186+)
        0x61 => stack::popa(vm),
        // BOUND (80186+). The range check is deliberately not performed: its
        // exception is INT 5, which on a PC is the BIOS print-screen service,
        // so a failing BOUND would be re-executed forever. Only the operands
        // are consumed.
        0x62 => {
            let _ = u16::mod_rm_single(vm);
        }
        // 8086 alias: JNB rel8 (same as 0x73)
        0x63 => control::jnb_alias(vm),
        // FS/GS segment prefix (386+) — ignore in real mode (treat as DS)
        0x64 | 0x65 => {
            vm.prefixes.segment = Some(SegmentType::DS);
            vm.prefixes.pending = true;
        }
        // Operand/Address size override (386+) — ignored: a prefix with no effect
        0x66 | 0x67 => vm.prefixes.pending = true,
        // PUSH imm16 (80186+)
        0x68 => stack::push_imm16(vm),
        // IMUL reg, r/m16, imm16 (80186+)
        0x69 => alu::imul_imm16(vm),
        // PUSH sign-extended imm8 (80186+)
        0x6A => stack::push_imm8(vm),
        // IMUL reg, r/m16, imm8 (80186+)
        0x6B => alu::imul_imm8(vm),
        // INSB/INSW (80186+) — port [DX] -> ES:[DI]
        0x6C => string::ins::<u8>(vm),
        0x6D => string::ins::<u16>(vm),
        // OUTSB/OUTSW (80186+) — DS:[SI] -> port [DX]
        0x6E => string::outs::<u8>(vm),
        0x6F => string::outs::<u16>(vm),
        // NOP
        0x90 => {}
        // OUT DX,ac
        0xEE | 0xEF => transfer::out_dx(vm, is_word),
        // OUT imm8,ac
        0xE6 | 0xE7 => transfer::out_imm8(vm, is_word),
        // POP mem/reg
        0x8F => stack::pop_rm(vm),
        // POP reg
        0b_0101_1000..=0b_0101_1111 => stack::pop_reg(vm, opcode),
        // POP sreg (ES=0x07, CS=0x0F, SS=0x17, DS=0x1F)
        0b_0000_0111 | 0b_0000_1111 | 0b_0001_0111 | 0b_0001_1111 => stack::pop_sreg(vm, opcode),
        // POPF
        0x9D => stack::popf(vm),
        // PUSH reg
        0b_0101_0000..=0b_0101_0111 => stack::push_reg(vm, opcode),
        // PUSH sreg
        0b_0000_0110 | 0b_0000_1110 | 0b_0001_0110 | 0b_0001_1110 => stack::push_sreg(vm, opcode),
        // PUSHF
        0x9C => stack::pushf(vm),
        // ROL / ROR / RCL / RCR / SHL / SHR / SAL / SAR  r/m, imm8  (186+, needed by MS-DOS)
        0xC0 | 0xC1 => shift::group_c0_c1(vm, is_word),
        // ENTER imm16, imm8 (80186+)
        0xC8 => stack::enter(vm),
        // LEAVE (80186+)
        0xC9 => stack::leave(vm),
        // ROL / ROR / RCL / RCR / SHL / SHR / SAL / SAR
        0b_1101_0000..=0b_1101_0011 => shift::group_d0_d3(vm, is_word, directional),
        // REP | REPE | REPNE | REPNZ | REPZ
        0b_1111_0010 | 0b_1111_0011 => {
            vm.prefixes.rep = Some(is_word);
            vm.prefixes.pending = true;
        }
        // RET (long)
        0xCB => control::ret_far(vm),
        // RET
        0xC3 => control::ret_near(vm),
        // RETF imm16
        0xCA => control::ret_far_imm16(vm),
        // RET imm16
        0xC2 => control::ret_near_imm16(vm),
        // SAHF
        0x9E => transfer::sahf(vm),
        // Group 80-83: the same eight operations on an r/m operand and an
        // immediate, selected by the reg field
        0b_1000_0000..=0b_1000_0011 => alu::group_80_83(vm, is_word, directional),
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
        0b_1010_1000 | 0b_1010_1001 => alu::test_accumulator_immediate(vm, is_word),
        // TEST Mod R/M
        0b_1000_0100 | 0b_1000_0101 => alu::test_mod_rm(vm, is_word),
        // WAIT
        0x9B => {}
        // XCHG reg
        0b_1001_0001..=0b_1001_0111 => transfer::xchg_accumulator(vm, opcode),
        // XCHG Mod R/M
        0b_1000_0110 | 0b_1000_0111 => transfer::xchg_mod_rm(vm, is_word),
        // SALC (undocumented) — set AL to 0xFF if CF, else 0x00
        0xD6 => transfer::salc(vm),
        // XLAT
        0xD7 => transfer::xlat(vm),
    }
}
