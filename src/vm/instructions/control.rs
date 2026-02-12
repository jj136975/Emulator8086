use crate::minix2::interruption::Message;
use crate::minix2::syscall::handle_interrupt;
use crate::utils::number::SpecialOps;
use crate::vm::modrm::ModRM;
use crate::vm::runtime::{ExecutionMode, Runtime};
use crate::vm::runtime::CpuFlag::*;
use super::{update_arithmetic_flags_word, update_arithmetic_flags_byte};

pub(super) fn group_fe_ff(vm: &mut Runtime, is_word: bool) {
    // For 0xFE (byte): only INC/DEC (reg 0-1) are byte operations.
    // On the real 8086, reg 2-7 with 0xFE behave as word operations
    // (CALL/JMP/PUSH), ignoring the operand-size bit. Some DOS code relies on this.
    if !is_word {
        // Peek at the ModR/M to check reg field before consuming it
        let modrm_byte = vm.registers.cs.read_byte(vm.registers.pc.word());
        let reg = (modrm_byte >> 3) & 0b_111;
        if reg <= 1 {
            let (modrm, _) = u8::mod_rm_single(vm);
            match reg {
                // INC
                0b_000 => {
                    let prev = modrm.byte();
                    let (b, o, _) = prev.oc_add(1);
                    modrm.set(b);
                    update_arithmetic_flags_byte(vm, b, o, vm.check_flag(Carry));
                    vm.update_flag(AuxCarry, (prev & 0xF) + 1 > 0xF);
                }
                // DEC
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
        // reg >= 2: fall through to word path (8086 quirk)
    }

    let (modrm, reg) = u16::mod_rm_single(vm);

    match reg & 0b_111 {
        // INC
        0b_000 => {
            let prev = modrm.word();
            let (w, o, _) = prev.oc_add(1);
            modrm.set(w);
            update_arithmetic_flags_word(vm, w, o, vm.check_flag(Carry));
            vm.update_flag(AuxCarry, (prev & 0xF) + 1 > 0xF);
        }
        // DEC
        0b_001 => {
            let prev = modrm.word();
            let (w, o, _) = prev.oc_sub(1);
            modrm.set(w);
            update_arithmetic_flags_word(vm, w, o, vm.check_flag(Carry));
            vm.update_flag(AuxCarry, (prev & 0xF) < 1);
        }
        // CALL mem/reg
        0b_010 => {
            vm.push_word(vm.registers.pc.word());
            vm.registers.pc.set(modrm.word());
        },
        // CALL far indirect
        0b_011 => {
            let new_ip = modrm.word();
            let new_cs = unsafe { modrm.next() };
            vm.push_word(vm.registers.cs.reg().word());
            vm.push_word(vm.registers.pc.word());
            vm.registers.pc.set(new_ip);
            vm.registers.cs.reg_mut().set(new_cs);
        },
        // JMP mem/reg
        0b_100 => vm.registers.pc.set(modrm.word()),
        // JMP mem
        0b_101 => {
            vm.registers.pc.set(modrm.word());
            unsafe { vm.registers.cs.reg_mut().set(modrm.next()); }
        },
        // PUSH
        0b_110 => vm.push_word(modrm.word()),
        _ => unreachable!(),
    };
}

pub(super) fn dispatch_int(vm: &mut Runtime, vector: u8) {
    if vm.mode == ExecutionMode::MinixAout {
        let message = Message::new(vm);
        unsafe {
            handle_interrupt(vm, &mut *message);
        }
        vm.registers.ax.set(0);
    } else {
        if let Some(handler) = vm.bios_handlers[vector as usize] {
            vm.push_word(vm.flags);
            vm.unset_flag(Interrupt);
            vm.unset_flag(Trap);
            vm.push_word(vm.registers.cs.reg().word());
            vm.push_word(vm.registers.pc.word());

            handler(vm);

            let ip = vm.pop_word();
            let cs = vm.pop_word();
            let flags = vm.pop_word();
            vm.registers.pc.set(ip);
            vm.registers.cs.reg_mut().set(cs);
            vm.flags = flags;
        } else {
            let ivt_offset = vector as usize * 4;
            let new_ip = vm.memory.read_word(ivt_offset);
            let new_cs = vm.memory.read_word(ivt_offset + 2);

            vm.push_word(vm.flags);
            vm.unset_flag(Interrupt);
            vm.unset_flag(Trap);
            vm.push_word(vm.registers.cs.reg().word());
            vm.push_word(vm.registers.pc.word());

            vm.registers.cs.reg_mut().set(new_cs);
            vm.registers.pc.set(new_ip);
        }
    }
}

pub(super) fn div_zero(vm: &mut Runtime) {
    dispatch_int(vm, 0);
}
