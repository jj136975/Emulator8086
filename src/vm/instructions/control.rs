use super::alu::inc_dec;
use crate::vm::modrm::{decode_rm, reg_field, ModRM, RmOperand};
use crate::vm::runtime::Runtime;

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
                // 54h does: the 8086's behaviour, kept deliberately.
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

/// Divide error (INT 0). The return address pushed is that of the next
/// instruction. This is the 8086's behaviour, kept deliberately: the 80286
/// and later push the address of the failing instruction instead.
pub(super) fn div_zero(vm: &mut Runtime) {
    vm.handle_interrupt(0x00);
}
