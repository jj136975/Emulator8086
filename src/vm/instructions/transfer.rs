//! The data transfer instructions: MOV in its seven encodings, XCHG, LEA,
//! LDS and LES, XLAT, LAHF, SAHF and SALC, and the port instructions IN and
//! OUT.
//!
//! The invariant: none of these changes a flag, SAHF excepted, whose
//! destination the flags are. MOV to SS also holds interrupts off until the
//! next instruction has run, so that a guest can load SS and SP as a pair.

use crate::vm::modrm::{ModRM, decode_rm, reg_field};
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::{Runtime, SegmentType};

/// MOV between an r/m operand and a register (88h-8Bh). With `directional`
/// set the register is the destination.
#[inline(always)]
pub(super) fn mov_mod_rm(vm: &mut Runtime, is_word: bool, directional: bool) {
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

/// MOV reg, imm (B0h-BFh): bit 3 of the opcode selects the word registers,
/// bits 0-2 the register.
#[inline(always)]
pub(super) fn mov_reg_imm(vm: &mut Runtime, opcode: u8) {
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

/// MOV accumulator, mem (A0h, A1h). The operand is addressed by a 16-bit
/// offset that follows the opcode; there is no ModR/M byte.
#[inline(always)]
pub(super) fn mov_accumulator_mem(vm: &mut Runtime, is_word: bool) {
    let address = vm.cpu.fetch_word();
    if is_word {
        let word = vm.data_segment().read_word(address);
        vm.cpu.registers.ax.set(word);
    } else {
        let byte = vm.data_segment().read_byte(address);
        vm.cpu.registers.ax.set_low(byte);
    }
}

/// MOV mem, accumulator (A2h, A3h), addressed like `mov_accumulator_mem`.
#[inline(always)]
pub(super) fn mov_mem_accumulator(vm: &mut Runtime, is_word: bool) {
    let address = vm.cpu.fetch_word();
    if is_word {
        let word = vm.cpu.registers.ax.word();
        vm.data_segment().write_word(address, word);
    } else {
        let byte = vm.cpu.registers.ax.low();
        vm.data_segment().write_byte(address, byte);
    }
}

/// MOV sreg, r/m (8Eh).
#[inline(always)]
pub(super) fn mov_sreg_rm(vm: &mut Runtime) {
    let mod_rm = vm.cpu.fetch_byte();
    let operand = decode_rm(vm, mod_rm);
    let word = u16::read_rm(vm, operand);
    let seg_field = (mod_rm >> 3) & 0b011;
    vm.cpu
        .get_segment(SegmentType::from(seg_field))
        .reg_mut()
        .set(word);
    // MOV SS inhibits interrupts for the next instruction
    if seg_field == 0b10 {
        vm.interrupt_inhibit = true;
    }
}

/// MOV r/m, sreg (8Ch).
#[inline(always)]
pub(super) fn mov_rm_sreg(vm: &mut Runtime) {
    let (modrm, reg) = u16::mod_rm_single(vm);
    let word = vm
        .cpu
        .get_segment(SegmentType::from((reg) & 0b011))
        .reg()
        .word();
    modrm.set(word);
}

/// MOV r/m, imm (C6h, C7h). Only reg = 0 is documented; the 8086 executes
/// every reg value as MOV.
#[inline(always)]
pub(super) fn mov_rm_imm(vm: &mut Runtime, is_word: bool) {
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

/// XCHG AX, reg (91h-97h); the register is in bits 0-2 of the opcode. It is
/// never AX itself, as `swap_register` requires: 90h is NOP and has an arm
/// of its own.
#[inline(always)]
pub(super) fn xchg_accumulator(vm: &mut Runtime, opcode: u8) {
    vm.cpu
        .registers
        .ref_reg_word(opcode & 0b_111)
        .swap_register(&mut vm.cpu.registers.ax);
}

/// XCHG between an r/m operand and a register (86h, 87h).
#[inline(always)]
pub(super) fn xchg_mod_rm(vm: &mut Runtime, is_word: bool) {
    if is_word {
        let (mut modrm, reg) = u16::mod_rm_single(vm);
        modrm.swap(&mut vm.cpu.registers.ref_reg_word(reg));
    } else {
        let (mut modrm, reg) = u8::mod_rm_single(vm);
        modrm.swap(&mut vm.cpu.registers.ref_reg_byte(reg));
    }
}

/// LEA (8Dh): loads the offset of the memory operand into a register.
#[inline(always)]
pub(super) fn lea(vm: &mut Runtime) {
    let mod_rm = vm.cpu.fetch_byte();
    let address = decode_rm(vm, mod_rm).as_memory(vm).offset;
    vm.cpu
        .registers
        .ref_reg_word(reg_field(mod_rm))
        .set(address);
}

/// LDS (C5h): loads a register and DS from the far pointer stored at the
/// memory operand.
#[inline(always)]
pub(super) fn lds(vm: &mut Runtime) {
    let mod_rm = vm.cpu.fetch_byte();
    let (offset, segment) = decode_rm(vm, mod_rm).as_memory(vm).read_far_pointer(vm);
    vm.cpu.registers.ref_reg_word(reg_field(mod_rm)).set(offset);
    vm.cpu.registers.ds.reg_mut().set(segment);
}

/// LES (C4h): loads a register and ES from the far pointer stored at the
/// memory operand.
#[inline(always)]
pub(super) fn les(vm: &mut Runtime) {
    let mod_rm = vm.cpu.fetch_byte();
    let (offset, segment) = decode_rm(vm, mod_rm).as_memory(vm).read_far_pointer(vm);
    vm.cpu.registers.ref_reg_word(reg_field(mod_rm)).set(offset);
    vm.cpu.registers.es.reg_mut().set(segment);
}

/// XLAT (D7h): replaces AL with the byte at offset BX + AL of the data
/// segment.
#[inline(always)]
pub(super) fn xlat(vm: &mut Runtime) {
    let address = (vm.cpu.registers.ax.low() as u16).wrapping_add(vm.cpu.registers.bx.word());
    let byte = vm.data_segment().read_byte(address);
    vm.cpu.registers.ax.set_low(byte);
}

/// LAHF (9Fh): copies the low byte of FLAGS to AH.
#[inline(always)]
pub(super) fn lahf(vm: &mut Runtime) {
    vm.cpu.registers.ax.set_high(vm.cpu.flags as u8);
}

/// SAHF (9Eh): copies AH to SF, ZF, AF, PF and CF.
#[inline(always)]
pub(super) fn sahf(vm: &mut Runtime) {
    // Whatever AH holds, bit 1 of FLAGS stays 1 and bits 3 and 5 stay 0.
    vm.cpu.flags =
        (vm.cpu.flags & 0xFF00) | ((vm.cpu.registers.ax.high() as u16) & 0x00D5) | 0x0002;
}

/// SALC (D6h, undocumented): AL = FFh if CF is set, 00h if it is clear.
#[inline(always)]
pub(super) fn salc(vm: &mut Runtime) {
    vm.cpu
        .registers
        .ax
        .set_low(if vm.cpu.check_flag(Carry) { 0xFF } else { 0x00 });
}

/// IN accumulator, DX (ECh, EDh).
#[inline(always)]
pub(super) fn in_dx(vm: &mut Runtime, is_word: bool) {
    let port = vm.cpu.registers.dx.word();
    if is_word {
        let val = vm.port_in_word(port);
        vm.cpu.registers.ax.set(val);
    } else {
        let val = vm.port_in_byte(port);
        vm.cpu.registers.ax.set_low(val);
    }
}

/// IN accumulator, imm8 (E4h, E5h).
#[inline(always)]
pub(super) fn in_imm8(vm: &mut Runtime, is_word: bool) {
    let port = vm.cpu.fetch_byte() as u16;
    if is_word {
        let val = vm.port_in_word(port);
        vm.cpu.registers.ax.set(val);
    } else {
        let val = vm.port_in_byte(port);
        vm.cpu.registers.ax.set_low(val);
    }
}

/// OUT DX, accumulator (EEh, EFh).
#[inline(always)]
pub(super) fn out_dx(vm: &mut Runtime, is_word: bool) {
    let port = vm.cpu.registers.dx.word();
    if is_word {
        let val = vm.cpu.registers.ax.word();
        vm.port_out_word(port, val);
    } else {
        let val = vm.cpu.registers.ax.low();
        vm.port_out_byte(port, val);
    }
}

/// OUT imm8, accumulator (E6h, E7h).
#[inline(always)]
pub(super) fn out_imm8(vm: &mut Runtime, is_word: bool) {
    let port = vm.cpu.fetch_byte() as u16;
    if is_word {
        let val = vm.cpu.registers.ax.word();
        vm.port_out_word(port, val);
    } else {
        let val = vm.cpu.registers.ax.low();
        vm.port_out_byte(port, val);
    }
}
