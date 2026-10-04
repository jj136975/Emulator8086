use super::{update_arithmetic_flags_byte, update_arithmetic_flags_word};
use crate::utils::number::SpecialOps;
use crate::vm::modrm::{direct_address, rm_address, ModRM};
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Runtime;
pub(super) fn group_fe_ff(vm: &mut Runtime, is_word: bool) {
    if !is_word {
        let modrm_byte = vm.cpu.registers.cs.read_byte(vm.cpu.registers.pc.word());
        let reg = (modrm_byte >> 3) & 0b_111;
        if reg <= 1 {
            let (modrm, _) = u8::mod_rm_single(vm);
            match reg {
                0b_000 => {
                    let prev = modrm.byte();
                    let (b, o, _) = prev.oc_add(1);
                    modrm.set(b);
                    update_arithmetic_flags_byte(vm, b, o, vm.cpu.check_flag(Carry));
                    vm.cpu.update_flag(AuxCarry, (prev & 0xF) + 1 > 0xF);
                }
                0b_001 => {
                    let prev = modrm.byte();
                    let (b, o, _) = prev.oc_sub(1);
                    modrm.set(b);
                    update_arithmetic_flags_byte(vm, b, o, vm.cpu.check_flag(Carry));
                    vm.cpu.update_flag(AuxCarry, (prev & 0xF) < 1);
                }
                _ => unreachable!(),
            };
            return;
        }
    }

    let modrm_byte = vm.cpu.registers.cs.read_byte(vm.cpu.registers.pc.word());

    // CALL FAR / JMP FAR through a memory pointer: offset word at EA, segment
    // word at EA+2, with the +2 wrapping inside the segment as for LES/LDS.
    let far_reg = (modrm_byte >> 3) & 0b_111;
    if far_reg == 0b_011 || far_reg == 0b_101 {
        vm.cpu.fetch_byte();
        let rm = modrm_byte & 0b_111;
        let mod_val = modrm_byte >> 6;
        let (new_ip, new_cs) = if mod_val == 0b11 {
            // A register operand has no second word; this form is undefined
            // on the 8086. Keep CS.
            (
                vm.cpu.registers.read_reg_word(rm),
                vm.cpu.registers.cs.reg().word(),
            )
        } else {
            let address = match mod_val {
                0b00 => direct_address(vm, rm),
                0b01 => {
                    let displacement = vm.cpu.fetch_byte() as i8 as i16;
                    rm_address(vm, rm).wrapping_add_signed(displacement)
                }
                _ => {
                    let displacement = vm.cpu.fetch_word();
                    rm_address(vm, rm).wrapping_add(displacement)
                }
            };
            let segment = vm.effective_segment(mod_val, rm);
            (
                segment.read_word(address),
                segment.read_word(address.wrapping_add(2)),
            )
        };
        if far_reg == 0b_011 {
            vm.push_word(vm.cpu.registers.cs.reg().word());
            vm.push_word(vm.cpu.registers.pc.word());
        }
        vm.cpu.registers.pc.set(new_ip);
        vm.cpu.registers.cs.reg_mut().set(new_cs);
        return;
    }

    let (modrm, reg) = u16::mod_rm_single(vm);

    match reg & 0b_111 {
        0b_000 => {
            let prev = modrm.word();
            let (w, o, _) = prev.oc_add(1);
            modrm.set(w);
            update_arithmetic_flags_word(vm, w, o, vm.cpu.check_flag(Carry));
            vm.cpu.update_flag(AuxCarry, (prev & 0xF) + 1 > 0xF);
        }
        0b_001 => {
            let prev = modrm.word();
            let (w, o, _) = prev.oc_sub(1);
            modrm.set(w);
            update_arithmetic_flags_word(vm, w, o, vm.cpu.check_flag(Carry));
            vm.cpu.update_flag(AuxCarry, (prev & 0xF) < 1);
        }
        0b_010 => {
            let target = modrm.word();
            vm.push_word(vm.cpu.registers.pc.word());
            vm.cpu.registers.pc.set(target);
        }
        0b_100 => vm.cpu.registers.pc.set(modrm.word()),
        // Far CALL / JMP were handled above.
        0b_011 | 0b_101 => unreachable!(),
        0b_110 | 0b_111 => {
            let is_sp_reg = !vm.cpu.is_186() && (modrm_byte >> 6) == 0b11 && (modrm_byte & 0b111) == 4;
            if is_sp_reg {
                vm.cpu.registers.sp.operation(2, u16::wrapping_sub);
                let sp = vm.cpu.registers.sp.word();
                vm.cpu.registers.ss.write_word(sp, sp);
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
    if vm.cpu.is_186() {
        vm.cpu.registers.pc.set(vm.cpu.registers.op_pc);
    }
    vm.handle_interrupt(0x00);
}
