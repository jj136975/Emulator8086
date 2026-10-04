//! The stack instructions: PUSH and POP of a register, a segment register,
//! an r/m operand or an immediate, PUSHF and POPF, and the 80186's PUSHA,
//! POPA, ENTER and LEAVE.
//!
//! The invariant: where the 8086 and its successors disagree, the 8086 wins,
//! because the hardware vectors were captured on one. PUSH SP stores the SP
//! it has just decremented (the 80286 and later store the old value), and a
//! FLAGS image always has bits 12-15 and bit 1 set and bits 3 and 5 clear.

use crate::vm::modrm::ModRM;
use crate::vm::runtime::Runtime;

/// PUSH reg (50h-57h); the register is in bits 0-2 of the opcode.
#[inline(always)]
pub(super) fn push_reg(vm: &mut Runtime, opcode: u8) {
    let reg = opcode & 0b111;
    if reg == 4 {
        // PUSH SP stores the already-decremented value: the 8086's
        // behaviour. The 80286 and later store the value SP had before
        // the push.
        vm.cpu.registers.sp.operation(2, u16::wrapping_sub);
        let sp = vm.cpu.registers.sp.word();
        vm.cpu.registers.ss.write_word(sp, sp);
    } else {
        vm.push_word(vm.cpu.registers.read_reg_word(reg));
    }
}

/// PUSH sreg (06h, 0Eh, 16h, 1Eh); the segment register is in bits 3-4 of
/// the opcode.
#[inline(always)]
pub(super) fn push_sreg(vm: &mut Runtime, opcode: u8) {
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

/// PUSH imm16 (68h, 80186+).
#[inline(always)]
pub(super) fn push_imm16(vm: &mut Runtime) {
    let imm = vm.cpu.fetch_word();
    vm.push_word(imm);
}

/// PUSH imm8 (6Ah, 80186+): the byte is sign-extended to a word.
#[inline(always)]
pub(super) fn push_imm8(vm: &mut Runtime) {
    let imm = vm.cpu.fetch_byte() as i8 as u16;
    vm.push_word(imm);
}

/// PUSHF (9Ch).
#[inline(always)]
pub(super) fn pushf(vm: &mut Runtime) {
    // 8086: bits 12-15 always 1, bit 1 always 1
    vm.push_word(vm.cpu.flags | 0xF002);
}

/// PUSHA (60h, 80186+): pushes AX, CX, DX, BX, the SP from before the
/// instruction, BP, SI and DI.
#[inline(always)]
pub(super) fn pusha(vm: &mut Runtime) {
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

/// POP reg (58h-5Fh); the register is in bits 0-2 of the opcode.
#[inline(always)]
pub(super) fn pop_reg(vm: &mut Runtime, opcode: u8) {
    let word = vm.pop_word();
    vm.cpu.registers.ref_reg_word(opcode & 0b111).set(word);
}

/// POP sreg (ES = 07h, CS = 0Fh, SS = 17h, DS = 1Fh); the segment register
/// is in bits 3-4 of the opcode.
#[inline(always)]
pub(super) fn pop_sreg(vm: &mut Runtime, opcode: u8) {
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

/// POP r/m (8Fh).
#[inline(always)]
pub(super) fn pop_rm(vm: &mut Runtime) {
    // Pop first: `pop_word` moves SP through `vm`, which must not happen
    // while an operand reference that may point at SP (POP SP) is alive.
    let word = vm.pop_word();
    let (operand, _reg) = u16::mod_rm_single(vm);
    operand.set(word);
}

/// POPF (9Dh).
#[inline(always)]
pub(super) fn popf(vm: &mut Runtime) {
    // 8086: bits 12-15 always 1, bit 1 always 1, bits 3,5 always 0
    let flags = vm.pop_word();
    vm.cpu.flags = (flags & 0x0FD5) | 0xF002;
}

/// POPA (61h, 80186+): pops DI, SI and BP, drops the word PUSHA stored for
/// SP, then pops BX, DX, CX and AX.
#[inline(always)]
pub(super) fn popa(vm: &mut Runtime) {
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

/// ENTER imm16, imm8 (C8h, 80186+): pushes BP, copies the frame pointers of
/// the enclosing procedures for a nesting level (imm8 modulo 32) above 1,
/// makes the new frame the value of BP and reserves imm16 bytes below it.
#[inline(always)]
pub(super) fn enter(vm: &mut Runtime) {
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

/// LEAVE (C9h, 80186+): releases the frame (SP = BP), then pops BP.
#[inline(always)]
pub(super) fn leave(vm: &mut Runtime) {
    vm.cpu.registers.sp.set(vm.cpu.registers.bp.word());
    let bp = vm.pop_word();
    vm.cpu.registers.bp.set(bp);
}
