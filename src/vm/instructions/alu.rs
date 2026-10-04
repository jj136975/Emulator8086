use std::ops::Not;
use crate::utils::number::{extend_sign, SpecialOps};
use crate::vm::modrm::ModRM;
use crate::vm::registers::OperandRef;
use crate::vm::runtime::Runtime;
use crate::vm::runtime::CpuFlag::*;
use super::{update_arithmetic_flags, update_logical_flags, update_result_flags};
use super::control::div_zero;

// The eight two-operand ALU operations, numbered as in bits 3-5 of opcodes
// 00h-3Fh and in the reg field of group 80h-83h.
pub(super) const ADD: u8 = 0b_000;
pub(super) const OR: u8 = 0b_001;
pub(super) const ADC: u8 = 0b_010;
pub(super) const SBB: u8 = 0b_011;
pub(super) const AND: u8 = 0b_100;
pub(super) const SUB: u8 = 0b_101;
pub(super) const XOR: u8 = 0b_110;
pub(super) const CMP: u8 = 0b_111;

/// Performs ALU operation `op` on destination `a` and source `b`, sets the
/// flags and returns the result. For CMP that is the difference, which the
/// caller does not store.
#[inline(always)]
pub(super) fn alu<T: SpecialOps>(vm: &mut Runtime, op: u8, a: T, b: T) -> T {
    let (res, overflow, carry) = match op {
        ADD => a.oc_add(b),
        ADC => a.oc_carry_add(b, vm.cpu.check_flag(Carry)),
        SBB => a.oc_carry_sub(b, vm.cpu.check_flag(Carry)),
        SUB | CMP => a.oc_sub(b),
        // OR, AND and XOR clear OF and CF and leave AF alone
        _ => {
            let res = match op {
                OR => a | b,
                AND => a & b,
                XOR => a ^ b,
                _ => unreachable!(),
            };
            update_logical_flags(vm, res);
            return res;
        }
    };
    update_arithmetic_flags(vm, res, overflow, carry);
    vm.cpu.update_flag(AuxCarry, auxiliary_carry(a, b, res));
    res
}

/// AF for `res = a + b` or `a - b` (with or without carry in): the carry or
/// borrow between bits 3 and 4, which is what makes bit 4 of the result
/// differ from the XOR of the operands' bit 4.
#[inline(always)]
fn auxiliary_carry<T: SpecialOps>(a: T, b: T, res: T) -> bool {
    (a ^ b ^ res).low_byte() & 0x10 != 0
}

/// Applies `op` to the operand behind `dest` and `src`, storing the result
/// unless the operation is CMP.
#[inline(always)]
fn apply<T: SpecialOps>(vm: &mut Runtime, op: u8, dest: &impl OperandRef<T>, src: T) {
    let res = alu(vm, op, dest.get(), src);
    if op != CMP {
        dest.set(res);
    }
}

/// INC or DEC of the operand behind `dest`. The flags are those of adding or
/// subtracting one, except that CF is not touched.
#[inline(always)]
pub(super) fn inc_dec<T: SpecialOps>(vm: &mut Runtime, dest: &impl OperandRef<T>, decrement: bool) {
    let prev = dest.get();
    let (res, overflow, _) = if decrement {
        prev.oc_sub(T::ONE)
    } else {
        prev.oc_add(T::ONE)
    };
    vm.cpu.update_flag(Overflow, overflow);
    update_result_flags(vm, res);
    vm.cpu.update_flag(AuxCarry, auxiliary_carry(prev, T::ONE, res));
    dest.set(res);
}

/// `op` between the accumulator and an immediate (opcodes x4h, x5h, xCh, xDh).
#[inline(always)]
pub(super) fn accumulator_immediate(vm: &mut Runtime, op: u8, is_word: bool) {
    if is_word {
        let imm = vm.cpu.fetch_word();
        let res = alu(vm, op, vm.cpu.registers.ax.word(), imm);
        if op != CMP {
            vm.cpu.registers.ax.set(res);
        }
    } else {
        let imm = vm.cpu.fetch_byte();
        let res = alu(vm, op, vm.cpu.registers.ax.low(), imm);
        if op != CMP {
            vm.cpu.registers.ax.set_low(res);
        }
    }
}

/// `op` between an r/m operand and a register (opcodes x0h-x3h, x8h-xBh).
/// With `directional` set the register is the destination.
#[inline(always)]
pub(super) fn mod_rm(vm: &mut Runtime, op: u8, is_word: bool, directional: bool) {
    if is_word {
        mod_rm_sized::<u16>(vm, op, directional)
    } else {
        mod_rm_sized::<u8>(vm, op, directional)
    }
}

#[inline(always)]
fn mod_rm_sized<T: SpecialOps + ModRM>(vm: &mut Runtime, op: u8, directional: bool) {
    let (dest, src) = if directional {
        T::mod_rm_lhs(vm)
    } else {
        T::mod_rm_rhs(vm)
    };
    apply(vm, op, &dest, src);
}

/// Opcodes 80h-83h: the operation in the reg field, between an r/m operand
/// and an immediate. 81h takes a word immediate, 83h (`sign_extended`) a
/// byte that is sign-extended to a word; 80h and its alias 82h are the byte
/// forms.
pub(super) fn group_80_83(vm: &mut Runtime, is_word: bool, sign_extended: bool) {
    if is_word {
        let (dest, op) = u16::mod_rm_single(vm);
        let imm = if sign_extended {
            extend_sign(vm.cpu.fetch_byte())
        } else {
            vm.cpu.fetch_word()
        };
        apply(vm, op, &dest, imm);
    } else {
        let (dest, op) = u8::mod_rm_single(vm);
        let imm = vm.cpu.fetch_byte();
        apply(vm, op, &dest, imm);
    }
}

pub(super) fn group_f6_f7(vm: &mut Runtime, is_word: bool) {
    if is_word {
        let (modrm, reg) = u16::mod_rm_single(vm);

        match reg & 0b_111 {
            // TEST (reg=1 is undocumented alias on 8086)
            0b_000 | 0b_001 => {
                let word = vm.cpu.fetch_word();
                update_logical_flags(vm, modrm.word() & word);
            },
            // NOT - no flags modified
            0b_010 => {
                let res = modrm.word().not();
                modrm.set(res);
            },
            // NEG: 0 - operand
            0b_011 => {
                let res = alu(vm, SUB, 0, modrm.word());
                modrm.set(res);
            },
            // MUL - only CF/OF defined
            0b_100 => {
                let mut res: u32 = (vm.cpu.registers.ax.word() as u32) * (modrm.word() as u32);
                // 8086 undocumented: REPZ prefix negates result (F1 flag)
                if vm.prefixes.rep.is_some() { res = (res as i32).wrapping_neg() as u32; }
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
                if vm.prefixes.rep.is_some() { res = res.wrapping_neg(); }
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

                // A REP prefix has no effect here: the 8086 only consults
                // its F1 flag when fixing up signs for IDIV.
                match numerator.checked_div(denum) {
                    Some(quot) if quot <= u16::MAX as u32 => {
                        vm.cpu.registers.ax.set(quot as u16);
                        vm.cpu.registers.dx.set((numerator % denum) as u16);
                    }
                    // Division by zero, or a quotient that does not fit in AX
                    _ => div_zero(vm),
                }
            },
            // IDIV - sign-extend divisor, truncated division, no flags defined
            0b_111 => {
                // Computed in i64 so that 8000:0000h / -1 fails the range
                // check below instead of overflowing the host's division.
                let numerator = (((vm.cpu.registers.dx.word() as u32) << 16 | (vm.cpu.registers.ax.word() as u32)) as i32) as i64;
                let denum = modrm.word() as i16 as i64;

                if denum == 0 {
                    div_zero(vm);
                } else {
                    let mut quot = numerator / denum;
                    let rem = numerator % denum;
                    // 8086 undocumented: REPZ prefix sets internal F1 flag,
                    // which the multiply/divide microcode uses for sign tracking.
                    // This causes an extra negation of the quotient.
                    if vm.prefixes.rep.is_some() { quot = -quot; }
                    // 8086 checks unsigned quotient > 0x7FFF before sign application
                    if !(-32767..=32767).contains(&quot) {
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
                update_logical_flags(vm, modrm.byte() & byte);
            },
            // NOT - no flags modified
            0b_010 => {
                let res = modrm.byte().not();
                modrm.set(res);
            },
            // NEG: 0 - operand
            0b_011 => {
                let res = alu(vm, SUB, 0, modrm.byte());
                modrm.set(res);
            },
            // MUL - only CF/OF defined
            0b_100 => {
                let mut res: u16 = (vm.cpu.registers.ax.low() as u16) * (modrm.byte() as u16);
                // 8086 undocumented: REPZ prefix negates result (F1 flag)
                if vm.prefixes.rep.is_some() { res = (res as i16).wrapping_neg() as u16; }
                vm.cpu.registers.ax.set(res);
                let high_nonzero = res >> 8 != 0;
                vm.cpu.update_flag(Carry, high_nonzero);
                vm.cpu.update_flag(Overflow, high_nonzero);
            },
            // IMUL - only CF/OF defined
            0b_101 => {
                let mut res: i16 = (vm.cpu.registers.ax.low() as i8 as i16) * (modrm.byte() as i8 as i16);
                // 8086 undocumented: REPZ prefix negates result (F1 flag)
                if vm.prefixes.rep.is_some() { res = res.wrapping_neg(); }
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

                // A REP prefix has no effect here (see the word form).
                match numerator.checked_div(denum) {
                    Some(quot) if quot <= u8::MAX as u16 => {
                        vm.cpu.registers.ax.set_low(quot as u8);
                        vm.cpu.registers.ax.set_high((numerator % denum) as u8);
                    }
                    // Division by zero, or a quotient that does not fit in AL
                    _ => div_zero(vm),
                }
            },
            // IDIV - truncated division, no flags defined
            0b_111 => {
                // Computed in i32 so that 8000h / -1 fails the range check
                // below instead of overflowing the host's division.
                let numerator = vm.cpu.registers.ax.word() as i16 as i32;
                let denum = modrm.byte() as i8 as i32;

                if denum == 0 {
                    div_zero(vm);
                } else {
                    let mut quot = numerator / denum;
                    let rem = numerator % denum;
                    // 8086 undocumented: REPZ prefix negates quotient (F1 flag)
                    if vm.prefixes.rep.is_some() { quot = -quot; }
                    // 8086 checks unsigned quotient > 0x7F before sign application
                    if !(-127..=127).contains(&quot) {
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
