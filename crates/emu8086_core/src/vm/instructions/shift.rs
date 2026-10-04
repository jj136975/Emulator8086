//! The shift and rotate instructions: ROL, ROR, RCL, RCR, SHL, SHR, SAR and
//! the undocumented SETMO, by 1 or by CL (D0h-D3h) and, from the 80186, by
//! an immediate (C0h, C1h).
//!
//! The invariant: a count taken from CL is not masked. All eight bits count,
//! as on the 8086; the 80186 and later reduce the count to five bits, and
//! only the immediate form, which is theirs, does so here. A count of zero
//! changes nothing, flags included.

use super::update_arithmetic_flags;
use crate::utils::number::SpecialOps;
use crate::vm::modrm::ModRM;
use crate::vm::registers::OperandRef;
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Runtime;

/// Opcodes C0h and C1h (80186+): shift or rotate an r/m operand by an
/// immediate count, of which five bits are used. MS-DOS 6.22's IO.SYS needs
/// them.
pub(super) fn group_c0_c1(vm: &mut Runtime, is_word: bool) {
    if is_word {
        let (operand, op) = u16::mod_rm_single(vm);
        let count = vm.cpu.fetch_byte() as u32 & 0x1F;
        shift(vm, &operand, op, count);
    } else {
        let (operand, op) = u8::mod_rm_single(vm);
        let count = vm.cpu.fetch_byte() as u32 & 0x1F;
        shift(vm, &operand, op, count);
    }
}

/// Opcodes D0h-D3h: shift or rotate an r/m operand by 1 or, with bit 1 of
/// the opcode set (`by_cl`), by CL.
pub(super) fn group_d0_d3(vm: &mut Runtime, is_word: bool, by_cl: bool) {
    // All eight bits of CL count: the 8086's behaviour. The 80186 and later
    // mask the count to 5 bits.
    let count = if !by_cl {
        1
    } else {
        vm.cpu.registers.cx.low() as u32
    };

    if is_word {
        let (operand, op) = u16::mod_rm_single(vm);
        shift(vm, &operand, op, count);
    } else {
        let (operand, op) = u8::mod_rm_single(vm);
        shift(vm, &operand, op, count);
    }
}

/// Shifts or rotates the operand behind `dest` by `count`. `op` is the reg
/// field of the ModR/M byte. A count of 0 changes nothing, flags included.
fn shift<T: SpecialOps>(vm: &mut Runtime, dest: &impl OperandRef<T>, op: u8, count: u32) {
    if count == 0 {
        return;
    }
    let value = dest.get();
    let bits = T::BITS;

    let (res, carry) = match op & 0b_111 {
        // ROL — CF = bit 0 of result (last bit rotated around)
        0b_000 => {
            let res = value.rotl(count);
            (res, res.bit(0))
        }
        // ROR — CF = MSB of result (last bit rotated around)
        0b_001 => {
            let res = value.rotr(count);
            (res, res.is_negative())
        }
        // RCL
        0b_010 => value.rotate_carry_left(count, vm.cpu.check_flag(Carry)),
        // RCR
        0b_011 => value.rotate_carry_right(count, vm.cpu.check_flag(Carry)),
        // SHL | SAL — CF = last bit shifted out, if the count reaches that far
        0b_100 => {
            let res = if count >= bits {
                T::ZERO
            } else {
                value.logical_shl(count)
            };
            (res, count <= bits && value.bit(bits - count))
        }
        // SETMO (undocumented: result is always all-1s on real 8086)
        0b_110 => (T::ALL_ONES, count <= bits && value.bit(bits - count)),
        // SHR
        0b_101 => {
            let res = if count >= bits {
                T::ZERO
            } else {
                value.logical_shr(count)
            };
            (res, count <= bits && value.bit(count - 1))
        }
        // SAR — past the operand width, result and CF are copies of the sign
        0b_111 => {
            let res = if count < bits {
                value.arithmetic_shr(count)
            } else if value.is_negative() {
                T::ALL_ONES
            } else {
                T::ZERO
            };
            let carry = if count <= bits {
                value.bit(count - 1)
            } else {
                value.is_negative()
            };
            (res, carry)
        }
        _ => unreachable!(),
    };

    if op & 0b_100 != 0 {
        // Shifts set CF, ZF, SF and PF; OF only for a count of 1.
        let overflow = if count == 1 {
            if op & 0b_001 == 0 {
                res.is_negative() != carry
            } else {
                (value ^ res).is_negative()
            }
        } else {
            vm.cpu.check_flag(Overflow)
        };
        update_arithmetic_flags(vm, res, overflow, carry);
    } else {
        // Rotates set CF and, for a count of 1, OF.
        vm.cpu.update_flag(Carry, carry);
        if count == 1 {
            let overflow = if op & 0b_001 == 0 {
                // ROL, RCL: the sign changed
                res.is_negative() != carry
            } else {
                // ROR, RCR: the top two bits differ
                res.is_negative() != res.bit(bits - 2)
            };
            vm.cpu.update_flag(Overflow, overflow);
        }
    }
    dest.set(res);
}
