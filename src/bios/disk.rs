use crate::bios::bda;
use crate::vm::runtime::{CpuFlag, Runtime};

/// INT 13h — Disk services
pub fn int13h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    let dl = vm.registers.dx.low();

    // Log non-read/write calls (reads/writes have their own detailed logging)
    if !matches!(ah, 0x02 | 0x03 | 0x04) {
        log::debug!(
            "[INT13h] AH={:02X} DL={:02X} at {:04X}:{:04X}",
            ah, dl, vm.registers.cs.reg().word(), vm.registers.op_pc
        );
    }

    match ah {
        0x00 => reset(vm, dl),
        0x01 => get_status(vm, dl),
        0x02 => read_sectors(vm, dl),
        0x03 => write_sectors(vm, dl),
        0x04 => verify_sectors(vm, dl),
        0x05 => format_track(vm, dl),
        0x08 => get_drive_params(vm, dl),
        0x09 | 0x0C | 0x0D | 0x11 => {
            // Init HD tables / Seek / Alt reset / Recalibrate — no-op success
            disk_ok(vm, dl);
        }
        0x10 => {
            // Test ready
            if vm.get_disk(dl).is_some() {
                disk_ok(vm, dl);
            } else {
                disk_err(vm, dl, 0xAA); // Drive not ready
            }
        }
        0x15 => get_disk_type(vm, dl),
        0x41 => {
            // Extensions check — not supported
            vm.set_flag(CpuFlag::Carry);
            vm.registers.ax.set_high(0x01);
        }
        _ => {
            disk_err(vm, dl, 0x01); // Invalid function
        }
    }
}

fn status_addr(dl: u8) -> usize {
    if dl >= 0x80 { bda::HD_STATUS } else { bda::FLOPPY_STATUS }
}

fn disk_ok(vm: &mut Runtime, dl: u8) {
    vm.unset_flag(CpuFlag::Carry);
    vm.registers.ax.set_high(0);
    vm.memory.write_byte(status_addr(dl), 0);
}

fn disk_err(vm: &mut Runtime, dl: u8, code: u8) {
    vm.set_flag(CpuFlag::Carry);
    vm.registers.ax.set_high(code);
    vm.memory.write_byte(status_addr(dl), code);
}

/// Decode CHS from registers. Returns (cylinder, head, sector, count).
fn decode_chs(vm: &Runtime) -> (u16, u8, u8, u8) {
    let cl = vm.registers.cx.low();
    let ch = vm.registers.cx.high();
    let cylinder = ((cl as u16 & 0xC0) << 2) | ch as u16;
    let sector = cl & 0x3F;
    let head = vm.registers.dx.high();
    let count = vm.registers.ax.low();
    (cylinder, head, sector, count)
}

fn reset(vm: &mut Runtime, dl: u8) {
    disk_ok(vm, dl);
}

fn get_status(vm: &mut Runtime, dl: u8) {
    let status = vm.memory.read_byte(status_addr(dl));
    vm.registers.ax.set_high(status);
    if status != 0 {
        vm.set_flag(CpuFlag::Carry);
    } else {
        vm.unset_flag(CpuFlag::Carry);
    }
}

fn read_sectors(vm: &mut Runtime, dl: u8) {
    let (cyl, head, sector, count) = decode_chs(vm);
    let es = vm.registers.es.reg().word();
    let bx = vm.registers.bx.word();

    log::debug!(
        "[INT13h] AH=02 READ  DL={:02X} CHS=({},{},{}) count={} -> {:04X}:{:04X}",
        dl, cyl, head, sector, count, es, bx
    );

    if count == 0 {
        disk_ok(vm, dl);
        vm.registers.ax.set_low(0);
        return;
    }

    let data = if let Some(disk) = vm.get_disk_mut(dl) {
        match disk.read_sectors(cyl, head, sector, count) {
            Ok(data) => data,
            Err(e) => {
                log::warn!(
                    "[INT13h] READ FAILED DL={:02X} CHS=({},{},{}) count={}: {}",
                    dl, cyl, head, sector, count, e
                );
                disk_err(vm, dl, 0x04); // Sector not found
                vm.registers.ax.set_low(0);
                return;
            }
        }
    } else {
        log::warn!("[INT13h] READ FAILED DL={:02X}: drive not ready", dl);
        disk_err(vm, dl, 0xAA); // Drive not ready
        vm.registers.ax.set_low(0);
        return;
    };

    // Copy data to ES:BX
    let base = (((es as usize) << 4) + bx as usize) & 0xFFFFF;
    for (i, &byte) in data.iter().enumerate() {
        vm.memory.write_byte((base + i) & 0xFFFFF, byte);
    }

    disk_ok(vm, dl);
    vm.registers.ax.set_low(count);
}

fn write_sectors(vm: &mut Runtime, dl: u8) {
    let (cyl, head, sector, count) = decode_chs(vm);
    let es = vm.registers.es.reg().word();
    let bx = vm.registers.bx.word();

    log::debug!(
        "[INT13h] AH=03 WRITE DL={:02X} CHS=({},{},{}) count={} <- {:04X}:{:04X}",
        dl, cyl, head, sector, count, es, bx
    );

    if count == 0 {
        disk_ok(vm, dl);
        vm.registers.ax.set_low(0);
        return;
    }

    // Read data from ES:BX
    let base = (((es as usize) << 4) + bx as usize) & 0xFFFFF;
    let total_bytes = count as usize * 512;
    let mut data = vec![0u8; total_bytes];
    for (i, byte) in data.iter_mut().enumerate() {
        *byte = vm.memory.read_byte((base + i) & 0xFFFFF);
    }

    if let Some(disk) = vm.get_disk_mut(dl) {
        match disk.write_sectors(cyl, head, sector, count, &data) {
            Ok(()) => {
                disk_ok(vm, dl);
                vm.registers.ax.set_low(count);
            }
            Err(e) => {
                log::warn!(
                    "[INT13h] WRITE FAILED DL={:02X} CHS=({},{},{}) count={}: {}",
                    dl, cyl, head, sector, count, e
                );
                disk_err(vm, dl, 0x04);
                vm.registers.ax.set_low(0);
            }
        }
    } else {
        log::warn!("[INT13h] WRITE FAILED DL={:02X}: drive not ready", dl);
        disk_err(vm, dl, 0xAA);
        vm.registers.ax.set_low(0);
    }
}

fn verify_sectors(vm: &mut Runtime, dl: u8) {
    let (cyl, head, sector, count) = decode_chs(vm);

    log::debug!(
        "[INT13h] AH=04 VERIFY DL={:02X} CHS=({},{},{}) count={}",
        dl, cyl, head, sector, count
    );

    if let Some(disk) = vm.get_disk(dl) {
        if disk.verify_sectors(cyl, head, sector, count) {
            disk_ok(vm, dl);
            vm.registers.ax.set_low(count);
        } else {
            log::warn!(
                "[INT13h] VERIFY FAILED DL={:02X} CHS=({},{},{}) count={}",
                dl, cyl, head, sector, count
            );
            disk_err(vm, dl, 0x04);
            vm.registers.ax.set_low(0);
        }
    } else {
        log::warn!("[INT13h] VERIFY FAILED DL={:02X}: drive not ready", dl);
        disk_err(vm, dl, 0xAA);
        vm.registers.ax.set_low(0);
    }
}

fn format_track(vm: &mut Runtime, dl: u8) {
    let (cyl, head, _, _) = decode_chs(vm);

    if let Some(disk) = vm.get_disk_mut(dl) {
        match disk.format_track(cyl, head) {
            Ok(()) => disk_ok(vm, dl),
            Err(_) => disk_err(vm, dl, 0x04),
        }
    } else {
        disk_err(vm, dl, 0xAA);
    }
}

fn get_drive_params(vm: &mut Runtime, dl: u8) {
    if dl >= 0x80 {
        // Hard disk
        let idx = (dl - 0x80) as usize;
        if let Some(Some(disk)) = vm.hard_disks.get(idx) {
            let max_cyl = disk.cylinders.saturating_sub(1);
            let max_head = disk.heads.saturating_sub(1);
            let spt = disk.sectors_per_track;
            let num_hd = vm.hard_disks.iter().filter(|d| d.is_some()).count() as u8;

            vm.registers.cx.set_high((max_cyl & 0xFF) as u8);
            vm.registers.cx.set_low(
                (((max_cyl >> 8) as u8 & 0x03) << 6) | (spt & 0x3F),
            );
            vm.registers.dx.set_high(max_head);
            vm.registers.dx.set_low(num_hd);
            vm.registers.ax.set_high(0);
            vm.unset_flag(CpuFlag::Carry);
        } else {
            disk_err(vm, dl, 0x07); // Drive parameter activity failed
        }
    } else {
        // Floppy disk
        let idx = dl as usize;
        if let Some(Some(disk)) = vm.disks.get(idx) {
            let max_cyl = disk.cylinders.saturating_sub(1);
            let max_head = disk.heads.saturating_sub(1);
            let spt = disk.sectors_per_track;
            let num_floppy = vm.disks.iter().filter(|d| d.is_some()).count() as u8;

            // Determine floppy type from geometry
            let floppy_type: u8 = match (disk.cylinders, disk.heads, disk.sectors_per_track) {
                (40, 1, 8) | (40, 1, 9) => 0x01, // 360K 5.25"
                (80, 2, 9) => 0x03,               // 720K 3.5"
                (80, 2, 15) => 0x02,              // 1.2M 5.25"
                (80, 2, 18) => 0x04,              // 1.44M 3.5"
                (80, 2, 36) => 0x06,              // 2.88M 3.5"
                _ => 0x04,                         // Default to 1.44M
            };

            vm.registers.ax.set_high(0);
            vm.registers.bx.set_low(floppy_type);
            vm.registers.cx.set_high((max_cyl & 0xFF) as u8);
            vm.registers.cx.set_low(
                (((max_cyl >> 8) as u8 & 0x03) << 6) | (spt & 0x3F),
            );
            vm.registers.dx.set_high(max_head);
            vm.registers.dx.set_low(num_floppy);
            vm.unset_flag(CpuFlag::Carry);

            // Set ES:DI to point to diskette parameter table (INT 1Eh vector)
            let dpt_ip = vm.memory.read_word(0x1E * 4);
            let dpt_cs = vm.memory.read_word(0x1E * 4 + 2);
            vm.registers.es.reg_mut().set(dpt_cs);
            vm.registers.di.set(dpt_ip);
        } else {
            disk_err(vm, dl, 0x07);
        }
    }
}

fn get_disk_type(vm: &mut Runtime, dl: u8) {
    if dl >= 0x80 {
        // Hard disk
        if let Some(Some(disk)) = vm.hard_disks.get((dl - 0x80) as usize) {
            let total_sectors = disk.cylinders as u32
                * disk.heads as u32
                * disk.sectors_per_track as u32;
            vm.registers.ax.set_high(0x03); // Fixed disk
            vm.registers.cx.set((total_sectors >> 16) as u16);
            vm.registers.dx.set(total_sectors as u16);
            vm.unset_flag(CpuFlag::Carry);
        } else {
            vm.registers.ax.set_high(0x00); // No disk
            vm.set_flag(CpuFlag::Carry);
        }
    } else {
        // Floppy
        if vm.get_disk(dl).is_some() {
            vm.registers.ax.set_high(0x01); // Floppy without change-line
            vm.unset_flag(CpuFlag::Carry);
        } else {
            vm.registers.ax.set_high(0x00);
            vm.set_flag(CpuFlag::Carry);
        }
    }
}
