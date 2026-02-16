use super::{update_arithmetic_flags_byte, update_arithmetic_flags_word};
use crate::utils::number::SpecialOps;
use crate::vm::modrm::ModRM;
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Runtime;
pub(super) fn group_fe_ff(vm: &mut Runtime, is_word: bool) {
    if !is_word {
        let modrm_byte = vm.registers.cs.read_byte(vm.registers.pc.word());
        let reg = (modrm_byte >> 3) & 0b_111;
        if reg <= 1 {
            let (modrm, _) = u8::mod_rm_single(vm);
            match reg {
                0b_000 => {
                    let prev = modrm.byte();
                    let (b, o, _) = prev.oc_add(1);
                    modrm.set(b);
                    update_arithmetic_flags_byte(vm, b, o, vm.check_flag(Carry));
                    vm.update_flag(AuxCarry, (prev & 0xF) + 1 > 0xF);
                }
                0b_001 => {
                    let prev = modrm.byte();
                    let (b, o, _) = prev.oc_sub(1);
                    modrm.set(b);
                    update_arithmetic_flags_byte(vm, b, o, vm.check_flag(Carry));
                    vm.update_flag(AuxCarry, (prev & 0xF) < 1);
                }
                _ => unreachable!(),
            };
            return;
        }
    }

    let modrm_byte = vm.registers.cs.read_byte(vm.registers.pc.word());
    let (modrm, reg) = u16::mod_rm_single(vm);

    match reg & 0b_111 {
        0b_000 => {
            let prev = modrm.word();
            let (w, o, _) = prev.oc_add(1);
            modrm.set(w);
            update_arithmetic_flags_word(vm, w, o, vm.check_flag(Carry));
            vm.update_flag(AuxCarry, (prev & 0xF) + 1 > 0xF);
        }
        0b_001 => {
            let prev = modrm.word();
            let (w, o, _) = prev.oc_sub(1);
            modrm.set(w);
            update_arithmetic_flags_word(vm, w, o, vm.check_flag(Carry));
            vm.update_flag(AuxCarry, (prev & 0xF) < 1);
        }
        0b_010 => {
            let target = modrm.word();
            vm.push_word(vm.registers.pc.word());
            vm.registers.pc.set(target);
        }
        // CALL far indirect — read IP and CS from memory
        0b_011 => {
            let new_ip = modrm.word();
            let new_cs = unsafe { modrm.read_next_word() };
            vm.push_word(vm.registers.cs.reg().word());
            vm.push_word(vm.registers.pc.word());
            vm.registers.pc.set(new_ip);
            vm.registers.cs.reg_mut().set(new_cs);
        }
        0b_100 => vm.registers.pc.set(modrm.word()),
        // JMP far indirect — read IP and CS from memory
        0b_101 => {
            let new_ip = modrm.word();
            let new_cs = unsafe { modrm.read_next_word() };
            vm.registers.pc.set(new_ip);
            vm.registers.cs.reg_mut().set(new_cs);
        }
        0b_110 | 0b_111 => {
            let is_sp_reg = !vm.is_186() && (modrm_byte >> 6) == 0b11 && (modrm_byte & 0b111) == 4;
            if is_sp_reg {
                vm.registers.sp.operation(2, u16::wrapping_sub);
                let sp = vm.registers.sp.word();
                vm.registers.ss.write_word(sp, sp);
            } else {
                vm.push_word(modrm.word());
            }
        }
        _ => unreachable!()
    };
}

pub(super) fn div_zero(vm: &mut Runtime) {
    // 80186+: return address points to the faulting DIV/IDIV instruction (allows retry)
    // 8086: return address points past the instruction (already advanced)
    if vm.is_186() {
        vm.registers.pc.set(vm.registers.op_pc);
    }
    vm.handle_interrupt(0x00);
}
