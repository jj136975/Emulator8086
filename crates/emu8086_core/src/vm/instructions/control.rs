//! The control-transfer instructions: CALL, JMP and RET, near and far, the
//! conditional jumps and the loops, INT n, INTO and IRET with the divide
//! error, and the FE/FF group, which reaches CALL, JMP, PUSH, INC and DEC
//! through an r/m operand.
//!
//! The invariant: a displacement counts from the instruction that follows,
//! and that instruction is where a call returns, so every byte of the
//! instruction is fetched before IP is read, pushed or changed. Where the
//! 8086 differs from its successors it wins: a divide error returns to the
//! next instruction, not to the one that failed.

use super::alu::inc_dec;
use crate::vm::modrm::{ModRM, RmOperand, decode_rm, reg_field};
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Runtime;

/// CALL far (9Ah): pushes CS and IP and continues at the segment:offset that
/// follows the opcode.
#[inline(always)]
pub(super) fn call_far(vm: &mut Runtime) {
    let address = vm.cpu.fetch_word();
    let segment = vm.cpu.fetch_word();
    vm.push_word(vm.cpu.registers.cs.reg().word());
    vm.push_word(vm.cpu.registers.pc.word());
    vm.cpu.registers.cs.reg_mut().set(segment);
    vm.cpu.registers.pc.set(address);
}

/// CALL (E8h) and JMP (E9h) to a 16-bit displacement from the next
/// instruction. `jump` is bit 0 of the opcode: CALL has it clear and pushes
/// the return address.
#[inline(always)]
pub(super) fn call_or_jmp_near(vm: &mut Runtime, jump: bool) {
    let address = vm.cpu.fetch_word().wrapping_add(vm.cpu.registers.pc.word());
    if !jump {
        vm.push_word(vm.cpu.registers.pc.word());
    }
    vm.cpu.registers.pc.set(address);
}

/// JMP far (EAh): continues at the segment:offset that follows the opcode.
#[inline(always)]
pub(super) fn jmp_far(vm: &mut Runtime) {
    let pc = vm.cpu.fetch_word();
    let cs = vm.cpu.fetch_word();
    vm.cpu.registers.pc.set(pc);
    vm.cpu.registers.cs.reg_mut().set(cs);
}

/// JMP short (EBh): adds a signed 8-bit displacement to IP.
#[inline(always)]
pub(super) fn jmp_short(vm: &mut Runtime) {
    let disp = vm.cpu.fetch_byte() as i8;
    vm.cpu
        .registers
        .pc
        .operation(disp as i16, u16::wrapping_add_signed);
}

/// Jcc (70h-7Fh): a short jump taken if the condition holds. Bits 1-3 of the
/// opcode select the condition and bit 0 (`negated`) inverts it.
#[inline(always)]
pub(super) fn jcc(vm: &mut Runtime, opcode: u8, negated: bool) {
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
    }) != negated
    {
        vm.cpu
            .registers
            .pc
            .operation(disp as i16, u16::wrapping_add_signed);
    }
}

/// 63h: on the 8086 an alias of JNB (73h), a short jump taken if CF is
/// clear.
#[inline(always)]
pub(super) fn jnb_alias(vm: &mut Runtime) {
    let disp = vm.cpu.fetch_byte() as i8;
    if !vm.cpu.check_flag(Carry) {
        vm.cpu
            .registers
            .pc
            .operation(disp as i16, u16::wrapping_add_signed);
    }
}

/// JCXZ (E3h): a short jump taken if CX is zero.
#[inline(always)]
pub(super) fn jcxz(vm: &mut Runtime) {
    let disp = vm.cpu.fetch_byte() as i8;
    if vm.cpu.registers.cx.word() == 0 {
        vm.cpu
            .registers
            .pc
            .operation(disp as i16, u16::wrapping_add_signed);
    }
}

/// LOOP (E2h): decrements CX and jumps short while it is not zero. The
/// flags are not touched.
#[inline(always)]
pub(super) fn loop_(vm: &mut Runtime) {
    let disp = vm.cpu.fetch_byte() as i8 as i16;
    vm.cpu.registers.cx.operation(1, u16::wrapping_sub);

    if vm.cpu.registers.cx.word() != 0 {
        vm.cpu
            .registers
            .pc
            .operation(disp, u16::wrapping_add_signed);
    }
}

/// LOOPE/LOOPZ (E1h): as LOOP, and the jump also requires ZF set.
#[inline(always)]
pub(super) fn loopz(vm: &mut Runtime) {
    let disp = vm.cpu.fetch_byte() as i8 as i16;
    vm.cpu.registers.cx.operation(1, u16::wrapping_sub);

    if vm.cpu.registers.cx.word() != 0 && vm.cpu.check_flag(Zero) {
        vm.cpu
            .registers
            .pc
            .operation(disp, u16::wrapping_add_signed);
    }
}

/// LOOPNE/LOOPNZ (E0h): as LOOP, and the jump also requires ZF clear.
#[inline(always)]
pub(super) fn loopnz(vm: &mut Runtime) {
    let disp = vm.cpu.fetch_byte() as i8 as i16;
    vm.cpu.registers.cx.operation(1, u16::wrapping_sub);

    if vm.cpu.registers.cx.word() != 0 && !vm.cpu.check_flag(Zero) {
        vm.cpu
            .registers
            .pc
            .operation(disp, u16::wrapping_add_signed);
    }
}

/// RET near (C3h): pops IP.
#[inline(always)]
pub(super) fn ret_near(vm: &mut Runtime) {
    let pc = vm.pop_word();
    vm.cpu.registers.pc.set(pc);
}

/// RET far (CBh): pops IP, then CS.
#[inline(always)]
pub(super) fn ret_far(vm: &mut Runtime) {
    let pc = vm.pop_word();
    let cs = vm.pop_word();

    vm.cpu.registers.pc.set(pc);
    vm.cpu.registers.cs.reg_mut().set(cs);
}

/// RET near imm16 (C2h): pops IP, then releases imm16 bytes of stack.
#[inline(always)]
pub(super) fn ret_near_imm16(vm: &mut Runtime) {
    let disp = vm.cpu.fetch_word();
    let pc = vm.pop_word();

    vm.cpu.registers.pc.set(pc);
    vm.cpu.registers.sp.operation(disp, u16::wrapping_add);
}

/// RET far imm16 (CAh): pops IP and CS, then releases imm16 bytes of stack.
#[inline(always)]
pub(super) fn ret_far_imm16(vm: &mut Runtime) {
    let disp = vm.cpu.fetch_word();
    let pc = vm.pop_word();
    let cs = vm.pop_word();

    vm.cpu.registers.pc.set(pc);
    vm.cpu.registers.cs.reg_mut().set(cs);
    vm.cpu.registers.sp.operation(disp, u16::wrapping_add);
}

/// Opcodes FE and FF: INC, DEC, CALL, JMP or PUSH of the r/m operand,
/// selected by the reg field.
pub(super) fn group_fe_ff(vm: &mut Runtime, is_word: bool) {
    let mod_rm = vm.cpu.fetch_byte();
    let operand = decode_rm(vm, mod_rm);

    match reg_field(mod_rm) {
        // INC r/m (000) / DEC r/m (001)
        reg @ (0b_000 | 0b_001) => {
            let decrement = reg == 0b_001;
            if is_word {
                let dest = u16::ref_rm(vm, operand);
                inc_dec(vm, &dest, decrement);
            } else {
                let dest = u8::ref_rm(vm, operand);
                inc_dec(vm, &dest, decrement);
            }
        }
        // The remaining operations take a word operand. Only FF defines
        // them; FE with these reg values is undefined on the 8086 and is
        // executed like FF here.
        // CALL near
        0b_010 => {
            let target = u16::read_rm(vm, operand);
            vm.push_word(vm.cpu.registers.pc.word());
            vm.cpu.registers.pc.set(target);
        }
        // CALL far / JMP far through the pointer stored at the operand
        reg @ (0b_011 | 0b_101) => {
            let (new_ip, new_cs) = match operand {
                // A register operand has no second word; this form is
                // undefined on the 8086. Keep CS.
                RmOperand::Register(rm) => (
                    vm.cpu.registers.read_reg_word(rm),
                    vm.cpu.registers.cs.reg().word(),
                ),
                RmOperand::Memory(memory) => memory.read_far_pointer(vm),
            };
            if reg == 0b_011 {
                vm.push_word(vm.cpu.registers.cs.reg().word());
                vm.push_word(vm.cpu.registers.pc.word());
            }
            vm.cpu.registers.pc.set(new_ip);
            vm.cpu.registers.cs.reg_mut().set(new_cs);
        }
        // JMP near
        0b_100 => {
            let target = u16::read_rm(vm, operand);
            vm.cpu.registers.pc.set(target);
        }
        // PUSH r/m (110, and its undocumented alias 111)
        _ => {
            if matches!(operand, RmOperand::Register(0b100)) {
                // PUSH SP stores the already-decremented value, as opcode
                // 54h does: the 8086's behaviour.
                vm.cpu.registers.sp.operation(2, u16::wrapping_sub);
                let sp = vm.cpu.registers.sp.word();
                vm.cpu.registers.ss.write_word(sp, sp);
            } else {
                let word = u16::read_rm(vm, operand);
                vm.push_word(word);
            }
        }
    }
}

/// INT n (CDh): the software interrupt whose vector is the immediate byte.
#[inline(always)]
pub(super) fn int_imm8(vm: &mut Runtime) {
    let vector = vm.cpu.fetch_byte();
    vm.handle_interrupt(vector);
}

/// INTO (CEh): INT 4 if OF is set.
#[inline(always)]
pub(super) fn into(vm: &mut Runtime) {
    if vm.cpu.check_flag(Overflow) {
        vm.handle_interrupt(0x04)
    }
}

/// IRET (CFh): pops IP, CS and FLAGS.
#[inline(always)]
pub(super) fn iret(vm: &mut Runtime) {
    let pc = vm.pop_word();
    let cs = vm.pop_word();
    let flags = vm.pop_word();
    vm.cpu.registers.pc.set(pc);
    vm.cpu.registers.cs.reg_mut().set(cs);
    // 8086: bits 12-15 always 1, bit 1 always 1, bits 3,5 always 0
    vm.cpu.flags = (flags & 0x0FD5) | 0xF002;
}

/// Divide error (INT 0). The return address pushed is that of the next
/// instruction: the 8086's behaviour. The 80286 and later push the address
/// of the failing instruction instead.
pub(super) fn div_zero(vm: &mut Runtime) {
    vm.handle_interrupt(0x00);
}
