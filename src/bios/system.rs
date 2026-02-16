use crate::bios::bda;
use crate::vm::memory::BOOT_ADDR;
use crate::vm::runtime::{CpuFlag, Runtime};

/// INT 11h — Equipment determination
pub fn int11h(vm: &mut Runtime) {
    let equip = vm.memory.read_word(bda::EQUIP_WORD);
    vm.registers.ax.set(equip);
}

/// INT 12h — Memory size
pub fn int12h(vm: &mut Runtime) {
    let mem_kb = vm.memory.read_word(bda::MEMORY_SIZE_KB);
    vm.registers.ax.set(mem_kb);
}

/// INT 15h — System services
pub fn int15h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // Keyboard intercept: set CF to pass through, AL unchanged
        0x4F => {
            vm.set_flag(CpuFlag::Carry);
        }
        // Wait (microseconds in CX:DX) — no-op
        0x86 => {
            vm.unset_flag(CpuFlag::Carry);
        }
        // Extended memory size: AX=0 (no extended memory on 8086)
        0x88 => {
            vm.registers.ax.set(0);
            vm.unset_flag(CpuFlag::Carry);
        }
        // Device POST complete — no-op
        0x91 => {}
        // System configuration table — not supported
        0xC0 => {
            vm.set_flag(CpuFlag::Carry);
            vm.registers.ax.set_high(0x86);
        }
        // Get EBDA segment — not supported
        0xC1 => {
            vm.set_flag(CpuFlag::Carry);
        }
        // Default: unsupported function
        _ => {
            vm.set_flag(CpuFlag::Carry);
            vm.registers.ax.set_high(0x86);
        }
    }
}

/// INT 19h — Bootstrap loader
pub fn int19h(vm: &mut Runtime) {
    let boot_order = vm.boot_order.clone();

    for &drive in &boot_order {
        let data = if let Some(disk) = vm.get_disk_mut(drive) {
            match disk.read_sectors(0, 0, 1, 1) {
                Ok(data) => data,
                Err(_) => continue,
            }
        } else {
            continue;
        };

        // Check boot signature (0x55AA at offset 510-511)
        if data.len() < 512 || data[510] != 0x55 || data[511] != 0xAA {
            continue;
        }

        // Copy boot sector to 0x7C00
        for (i, &byte) in data.iter().take(512).enumerate() {
            vm.memory.write_byte(BOOT_ADDR + i, byte);
        }

        // Set DL = boot drive number, jump to boot sector
        vm.registers.dx.set_low(drive);
        vm.registers.cs.reg_mut().set(0x0000);
        vm.registers.pc.set(BOOT_ADDR as u16);

        log::info!("[BIOS] Booting from drive 0x{:02X}", drive);
        return;
    }

    // No bootable disk found
    log::error!("[BIOS] No bootable disk found");
    eprintln!("No bootable disk found");
    vm.exit(1);
}
