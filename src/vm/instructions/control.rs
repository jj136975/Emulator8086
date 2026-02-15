use super::{update_arithmetic_flags_byte, update_arithmetic_flags_word};
use crate::utils::number::SpecialOps;
use crate::vm::memory::BIOS_ROM;
use crate::vm::modrm::ModRM;
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Runtime;
use log::debug;

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

    let modrm_byte = vm.registers.cs.read_byte(vm.registers.pc.word());
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
        // CALL mem/reg (read target before push so CALL SP works correctly)
        0b_010 => {
            let target = modrm.word();
            vm.push_word(vm.registers.pc.word());
            vm.registers.pc.set(target);
        }
        // CALL far indirect
        0b_011 => {
            let new_ip = modrm.word();
            let new_cs = unsafe { modrm.next() };
            vm.push_word(vm.registers.cs.reg().word());
            vm.push_word(vm.registers.pc.word());
            vm.registers.pc.set(new_ip);
            vm.registers.cs.reg_mut().set(new_cs);
        }
        // JMP mem/reg
        0b_100 => vm.registers.pc.set(modrm.word()),
        // JMP mem
        0b_101 => {
            vm.registers.pc.set(modrm.word());
            unsafe {
                vm.registers.cs.reg_mut().set(modrm.next());
            }
        }
        // PUSH (reg=7 is undocumented alias on 8086)
        0b_110 | 0b_111 => {
            // 8086 quirk: PUSH SP pushes the already-decremented value
            let is_sp_reg = (modrm_byte >> 6) == 0b11 && (modrm_byte & 0b111) == 4;
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

pub(super) fn dispatch_int(vm: &mut Runtime, vector: u8) {
    if let Some(handler) = vm.bios_handlers[vector as usize] {
        handler(vm);

        // Software BIOS trap vectors (0xF1-0xF8) are reached via ROM stubs:
        //     INT Fxh ; IRET
        // The stub's IRET will pop the FLAGS that the *outer* IVT dispatch
        // pushed, overwriting any flag changes the handler made (CF, ZF, etc.).
        // Fix: propagate the handler's status flags into the stacked FLAGS
        // word, preserving IF and TF from the original caller.
        //
        // Skip 0xF0 (hardware IRQ handler for INT 09h) — the stacked flags
        // there belong to the interrupted code and must not be modified.
        if vector >= 0xF1 {
            let sp = vm.registers.sp.word();
            let stacked = vm.registers.ss.read_word(sp.wrapping_add(4));
            // Preserve IF (bit 9) and TF (bit 8) from original flags
            let preserved = stacked & 0x0300;
            let handler_flags = vm.flags & !0x0300;
            vm.registers.ss.write_word(sp.wrapping_add(4), preserved | handler_flags);
        }
    } else {
        // Look up IVT target
        let ivt_offset = vector as usize * 4;
        let new_ip = vm.memory.read_word(ivt_offset);
        let new_cs = vm.memory.read_word(ivt_offset + 2);
        let phys = (((new_cs as usize) << 4) + new_ip as usize) & 0xFFFFF;

        // Tier 2: ROM stub fast path — IVT points to our ROM code
        if phys >= BIOS_ROM {
            let b0 = vm.memory.read_byte(phys);

            // Pattern: INT xx; IRET (CD xx CF) — call trap handler directly
            if b0 == 0xCD {
                let trap_vec = vm.memory.read_byte((phys + 1) & 0xFFFFF);
                if vm.memory.read_byte((phys + 2) & 0xFFFFF) == 0xCF {
                    if let Some(handler) = vm.bios_handlers[trap_vec as usize] {
                        let saved_if_tf = vm.flags & 0x0300;
                        handler(vm);
                        // Restore IF/TF from caller, keep handler's status flags
                        vm.flags = (vm.flags & !0x0300) | saved_if_tf;
                        return;
                    }
                }
            }

        }

        // Tier 3: Standard IVT dispatch (hooked vectors, DOS interrupts, etc.)

        // Use trace level for high-frequency DOS interrupts (21h, 28h, 29h,
        // 2Ah, 2Fh, 33h) to avoid flooding the debug log.  INT 2Ah AH=84
        // alone can generate 35,000+ entries/sec during keyboard idle.
        match vector {
            0x21 | 0x28 | 0x29 | 0x2A | 0x2F | 0x33 => {
                log::trace!(
                    "[IVT] INT {:02X}h -> {:04X}:{:04X} at {:04X}:{:04X} AH={:02X}",
                    vector, new_cs, new_ip,
                    vm.registers.cs.reg().word(), vm.registers.op_pc,
                    vm.registers.ax.high()
                );
            }
            _ => {
                debug!(
                    "[IVT] INT {:02X}h -> {:04X}:{:04X} at {:04X}:{:04X} AH={:02X}",
                    vector, new_cs, new_ip,
                    vm.registers.cs.reg().word(), vm.registers.op_pc,
                    vm.registers.ax.high()
                );
            }
        }

        vm.push_word(vm.flags);
        vm.unset_flag(Interrupt);
        vm.unset_flag(Trap);
        vm.push_word(vm.registers.cs.reg().word());
        vm.push_word(vm.registers.pc.word());

        vm.registers.cs.reg_mut().set(new_cs);
        vm.registers.pc.set(new_ip);
    }
}

pub(super) fn div_zero(vm: &mut Runtime) {
    dispatch_int(vm, 0);
}
