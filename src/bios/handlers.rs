use log::debug;
use crate::vm::memory::{BDA_BASE, VGA_TEXT_BASE};
use crate::vm::runtime::{CpuFlag, Runtime};

// INT 10h - Video Services
pub fn int10h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // Set video mode
        0x00 => {
            let mode = vm.registers.ax.low();
            vm.memory.write_byte(BDA_BASE + 0x49, mode);
            // Clear VGA buffer
            for i in 0..4000 {
                vm.memory.write_byte(VGA_TEXT_BASE + i * 2, b' ');
                vm.memory.write_byte(VGA_TEXT_BASE + i * 2 + 1, 0x07);
            }
        }
        // Set cursor position
        0x02 => {
            let page = vm.registers.bx.high();
            let row = vm.registers.dx.high();
            let col = vm.registers.dx.low();
            // Store in BDA (page 0 cursor at 0x450)
            let cursor_offset = BDA_BASE + 0x50 + (page as usize) * 2;
            vm.memory.write_byte(cursor_offset, col);
            vm.memory.write_byte(cursor_offset + 1, row);
            // Update VGA CRT registers
            let pos = row as u16 * 80 + col as u16;
            if let Some(io_bus) = &mut vm.io_bus {
                io_bus.port_out_byte(0x3D4, 0x0E);
                io_bus.port_out_byte(0x3D5, (pos >> 8) as u8);
                io_bus.port_out_byte(0x3D4, 0x0F);
                io_bus.port_out_byte(0x3D5, pos as u8);
            }
        }
        // Get cursor position
        0x03 => {
            let page = vm.registers.bx.high();
            let cursor_offset = BDA_BASE + 0x50 + (page as usize) * 2;
            let col = vm.memory.read_byte(cursor_offset);
            let row = vm.memory.read_byte(cursor_offset + 1);
            vm.registers.dx.set_high(row);
            vm.registers.dx.set_low(col);
            vm.registers.cx.set(0x0607); // Cursor shape
        }
        // Scroll up
        0x06 => {
            let lines = vm.registers.ax.low();
            let attr = vm.registers.bx.high();
            let top_row = vm.registers.cx.high() as usize;
            let left_col = vm.registers.cx.low() as usize;
            let bottom_row = vm.registers.dx.high() as usize;
            let right_col = vm.registers.dx.low() as usize;
            scroll_up(vm, lines, attr, top_row, left_col, bottom_row, right_col);
        }
        // Scroll down
        0x07 => {
            let lines = vm.registers.ax.low();
            let attr = vm.registers.bx.high();
            let top_row = vm.registers.cx.high() as usize;
            let left_col = vm.registers.cx.low() as usize;
            let bottom_row = vm.registers.dx.high() as usize;
            let right_col = vm.registers.dx.low() as usize;
            scroll_down(vm, lines, attr, top_row, left_col, bottom_row, right_col);
        }
        // Read char+attr at cursor
        0x08 => {
            let page = vm.registers.bx.high();
            let cursor_offset = BDA_BASE + 0x50 + (page as usize) * 2;
            let col = vm.memory.read_byte(cursor_offset) as usize;
            let row = vm.memory.read_byte(cursor_offset + 1) as usize;
            let pos = (row * 80 + col) * 2;
            let ch = vm.memory.read_byte(VGA_TEXT_BASE + pos);
            let attr = vm.memory.read_byte(VGA_TEXT_BASE + pos + 1);
            vm.registers.ax.set_low(ch);
            vm.registers.ax.set_high(attr);
        }
        // Write char+attr at cursor
        0x09 => {
            let ch = vm.registers.ax.low();
            let page = vm.registers.bx.high();
            let attr = vm.registers.bx.low();
            let count = vm.registers.cx.word();
            let cursor_offset = BDA_BASE + 0x50 + (page as usize) * 2;
            let col = vm.memory.read_byte(cursor_offset) as usize;
            let row = vm.memory.read_byte(cursor_offset + 1) as usize;
            for i in 0..count as usize {
                let pos = (row * 80 + col + i) * 2;
                if pos + 1 < 4000 * 2 {
                    vm.memory.write_byte(VGA_TEXT_BASE + pos, ch);
                    vm.memory.write_byte(VGA_TEXT_BASE + pos + 1, attr);
                }
            }
        }
        // Write character only (no attribute change)
        0x0A => {
            let ch = vm.registers.ax.low();
            let page = vm.registers.bx.high();
            let count = vm.registers.cx.word();
            let cursor_offset = BDA_BASE + 0x50 + (page as usize) * 2;
            let col = vm.memory.read_byte(cursor_offset) as usize;
            let row = vm.memory.read_byte(cursor_offset + 1) as usize;
            for i in 0..count as usize {
                let pos = (row * 80 + col + i) * 2;
                if pos < 4000 * 2 {
                    vm.memory.write_byte(VGA_TEXT_BASE + pos, ch);
                }
            }
        }
        // Teletype output
        0x0E => {
            let ch = vm.registers.ax.low();
            teletype_output(vm, ch);
        }
        // Get video mode
        0x0F => {
            let mode = vm.memory.read_byte(BDA_BASE + 0x49);
            vm.registers.ax.set_low(mode);
            vm.registers.ax.set_high(80); // Columns
            vm.registers.bx.set_high(0);  // Active page
        }
        _ => {}
    }
}

// INT 11h - Equipment list (read from BDA)
pub fn int11h(vm: &mut Runtime) {
    let equip = vm.memory.read_word(BDA_BASE + 0x10);
    vm.registers.ax.set(equip);
}

// INT 12h - Memory size
pub fn int12h(vm: &mut Runtime) {
    vm.registers.ax.set(640); // 640KB conventional memory
}

// INT 13h - Disk Services
pub fn int13h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    let dl = vm.registers.dx.low();

    match ah {
        // Reset disk
        0x00 => {
            vm.registers.ax.set_high(0);
            vm.unset_flag(CpuFlag::Carry);
        }
        // Get status of last operation
        0x01 => {
            // Return success (last operation was OK)
            vm.registers.ax.set_high(0);
            vm.unset_flag(CpuFlag::Carry);
        }
        // Read sectors
        0x02 => {
            let count = vm.registers.ax.low();
            let ch = vm.registers.cx.high();
            let cl = vm.registers.cx.low();
            let cylinder = ((cl as u16 & 0xC0) << 2) | ch as u16;
            let sector = cl & 0x3F;
            let head = vm.registers.dx.high();
            let es = vm.registers.es.reg().word();
            let bx = vm.registers.bx.word();

            debug!("INT 13h READ: drive={:02X} C={} H={} S={} count={} -> {:04X}:{:04X}",
                   dl, cylinder, head, sector, count, es, bx);

            if let Some(disk) = vm.get_disk_mut(dl) {
                match disk.read_sectors(cylinder, head, sector, count) {
                    Ok(data) => {
                        let base = ((es as usize) << 4) + bx as usize;
                        for (i, &byte) in data.iter().enumerate() {
                            let addr = (base + i) & 0xFFFFF;
                            vm.memory.write_byte(addr, byte);
                        }
                        vm.registers.ax.set_high(0);
                        vm.registers.ax.set_low(count);
                        vm.unset_flag(CpuFlag::Carry);
                        debug!("INT 13h READ: OK, {} bytes", data.len());
                    }
                    Err(e) => {
                        debug!("INT 13h READ: FAILED: {}", e);
                        vm.registers.ax.set_high(0x04);
                        vm.set_flag(CpuFlag::Carry);
                    }
                }
            } else {
                debug!("INT 13h READ: No disk for drive {:02X}!", dl);
                vm.registers.ax.set_high(0x80);
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Write sectors
        0x03 => {
            let count = vm.registers.ax.low();
            let ch = vm.registers.cx.high();
            let cl = vm.registers.cx.low();
            let cylinder = ((cl as u16 & 0xC0) << 2) | ch as u16;
            let sector = cl & 0x3F;
            let head = vm.registers.dx.high();
            let es = vm.registers.es.reg().word();
            let bx = vm.registers.bx.word();

            debug!("INT 13h WRITE: drive={:02X} C={} H={} S={} count={} <- {:04X}:{:04X}",
                   dl, cylinder, head, sector, count, es, bx);

            // Collect guest memory into local buffer to avoid borrow conflict
            let total_bytes = count as usize * 512;
            let base = ((es as usize) << 4) + bx as usize;
            let mut buf = vec![0u8; total_bytes];
            for (i, byte) in buf.iter_mut().enumerate() {
                *byte = vm.memory.read_byte((base + i) & 0xFFFFF);
            }

            if let Some(disk) = vm.get_disk_mut(dl) {
                match disk.write_sectors(cylinder, head, sector, count, &buf) {
                    Ok(()) => {
                        vm.registers.ax.set_high(0);
                        vm.registers.ax.set_low(count);
                        vm.unset_flag(CpuFlag::Carry);
                        debug!("INT 13h WRITE: OK, {} bytes", total_bytes);
                    }
                    Err(e) => {
                        debug!("INT 13h WRITE: FAILED: {}", e);
                        vm.registers.ax.set_high(0x04);
                        vm.set_flag(CpuFlag::Carry);
                    }
                }
            } else {
                debug!("INT 13h WRITE: No disk for drive {:02X}!", dl);
                vm.registers.ax.set_high(0x80);
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Verify sectors
        0x04 => {
            let count = vm.registers.ax.low();
            let ch = vm.registers.cx.high();
            let cl = vm.registers.cx.low();
            let cylinder = ((cl as u16 & 0xC0) << 2) | ch as u16;
            let sector = cl & 0x3F;
            let head = vm.registers.dx.high();

            if let Some(disk) = vm.get_disk(dl) {
                if disk.verify_sectors(cylinder, head, sector, count) {
                    vm.registers.ax.set_high(0);
                    vm.registers.ax.set_low(count);
                    vm.unset_flag(CpuFlag::Carry);
                } else {
                    vm.registers.ax.set_high(0x04);
                    vm.set_flag(CpuFlag::Carry);
                }
            } else {
                vm.registers.ax.set_high(0x80);
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Format track
        0x05 => {
            let ch = vm.registers.cx.high();
            let cl = vm.registers.cx.low();
            let cylinder = ((cl as u16 & 0xC0) << 2) | ch as u16;
            let head = vm.registers.dx.high();

            debug!("INT 13h FORMAT: drive={:02X} C={} H={}", dl, cylinder, head);

            if let Some(disk) = vm.get_disk_mut(dl) {
                match disk.format_track(cylinder, head) {
                    Ok(()) => {
                        vm.registers.ax.set_high(0);
                        vm.unset_flag(CpuFlag::Carry);
                    }
                    Err(e) => {
                        debug!("INT 13h FORMAT: FAILED: {}", e);
                        vm.registers.ax.set_high(0x04);
                        vm.set_flag(CpuFlag::Carry);
                    }
                }
            } else {
                vm.registers.ax.set_high(0x80);
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Get drive parameters
        0x08 => {
            if dl >= 0x80 {
                // Hard disk
                let hd_idx = (dl - 0x80) as usize;
                if let Some(disk) = vm.hard_disks.get(hd_idx).and_then(|d| d.as_ref()) {
                    let max_cyl = disk.cylinders - 1;
                    let max_head = disk.heads - 1;
                    let spt = disk.sectors_per_track;

                    vm.registers.ax.set_high(0);
                    vm.registers.bx.set_low(0); // Not used for HD
                    vm.registers.cx.set_high(max_cyl as u8); // low 8 bits of cylinder
                    vm.registers.cx.set_low((((max_cyl >> 8) as u8 & 0x03) << 6) | spt); // high 2 bits of cyl in bits 6-7
                    vm.registers.dx.set_high(max_head);
                    let hd_count = vm.hard_disks.iter().filter(|d| d.is_some()).count() as u8;
                    vm.registers.dx.set_low(hd_count);

                    debug!("INT 13h GET PARAMS HD{}: C={} H={} S={} count={}",
                           hd_idx, disk.cylinders, disk.heads, spt, hd_count);
                    vm.unset_flag(CpuFlag::Carry);
                } else {
                    vm.registers.ax.set_high(0x07);
                    vm.set_flag(CpuFlag::Carry);
                }
            } else {
                // Floppy
                if let Some(disk) = vm.disks.get(dl as usize).and_then(|d| d.as_ref()) {
                    let max_cyl = disk.cylinders - 1;
                    let max_head = disk.heads - 1;
                    let spt = disk.sectors_per_track;

                    vm.registers.ax.set_high(0);
                    vm.registers.bx.set_low(0x04); // Drive type: 1.44MB
                    vm.registers.cx.set_high(max_cyl as u8);
                    vm.registers.cx.set_low(((max_cyl as u16 >> 2) as u8 & 0xC0) | spt);
                    vm.registers.dx.set_high(max_head);
                    let drive_count = vm.disks.iter().filter(|d| d.is_some()).count() as u8;
                    vm.registers.dx.set_low(drive_count);

                    let dpt_ip = vm.memory.read_word(0x1E * 4);
                    let dpt_cs = vm.memory.read_word(0x1E * 4 + 2);
                    vm.registers.es.reg_mut().set(dpt_cs);
                    vm.registers.di.set(dpt_ip);

                    vm.unset_flag(CpuFlag::Carry);
                } else {
                    vm.registers.ax.set_high(0x07);
                    vm.set_flag(CpuFlag::Carry);
                }
            }
        }
        // Get disk type
        0x15 => {
            if dl >= 0x80 {
                // Hard disk
                let hd_idx = (dl - 0x80) as usize;
                if let Some(disk) = vm.hard_disks.get(hd_idx).and_then(|d| d.as_ref()) {
                    let total_sectors = disk.cylinders as u32
                        * disk.heads as u32
                        * disk.sectors_per_track as u32;
                    vm.registers.ax.set_high(0x03); // Fixed disk
                    vm.registers.cx.set((total_sectors >> 16) as u16);
                    vm.registers.dx.set(total_sectors as u16);
                    vm.unset_flag(CpuFlag::Carry);
                } else {
                    vm.registers.ax.set_high(0x00); // No such drive
                    vm.set_flag(CpuFlag::Carry);
                }
            } else {
                // Floppy
                if vm.disks.get(dl as usize).and_then(|d| d.as_ref()).is_some() {
                    vm.registers.ax.set_high(0x02); // Floppy with change-line
                    vm.unset_flag(CpuFlag::Carry);
                } else {
                    vm.registers.ax.set_high(0x00); // No such drive
                    vm.set_flag(CpuFlag::Carry);
                }
            }
        }
        // Initialize HD controller parameters
        0x09 => {
            if dl >= 0x80 {
                vm.registers.ax.set_high(0);
                vm.unset_flag(CpuFlag::Carry);
            } else {
                vm.registers.ax.set_high(0x01);
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Seek to cylinder (no-op - our virtual disk doesn't need seeking)
        0x0C => {
            if vm.get_disk(dl).is_some() {
                vm.registers.ax.set_high(0);
                vm.unset_flag(CpuFlag::Carry);
            } else {
                vm.registers.ax.set_high(0x80);
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Alternate disk reset
        0x0D => {
            vm.registers.ax.set_high(0);
            vm.unset_flag(CpuFlag::Carry);
        }
        // Test drive ready
        0x10 => {
            if vm.get_disk(dl).is_some() {
                vm.registers.ax.set_high(0);
                vm.unset_flag(CpuFlag::Carry);
            } else {
                vm.registers.ax.set_high(0x80); // Not ready
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Recalibrate
        0x11 => {
            if vm.get_disk(dl).is_some() {
                vm.registers.ax.set_high(0);
                vm.unset_flag(CpuFlag::Carry);
            } else {
                vm.registers.ax.set_high(0x80);
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Check for INT 13h extensions (IBM/MS INT 13h Extensions)
        0x41 => {
            // Not supported - return error so caller falls back to CHS
            vm.registers.ax.set_high(0x01);
            vm.set_flag(CpuFlag::Carry);
        }
        _ => {
            debug!("INT 13h: unsupported function AH={:02X} DL={:02X}", ah, dl);
            vm.registers.ax.set_high(0x01);
            vm.set_flag(CpuFlag::Carry);
        }
    }
}

// INT 15h - System Services
pub fn int15h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // Wait (microsecond delay) - return immediately
        0x86 => {
            vm.unset_flag(CpuFlag::Carry);
        }
        // Get extended memory size - no extended memory in 8086
        0x88 => {
            vm.registers.ax.set(0); // 0 KB extended memory
            vm.unset_flag(CpuFlag::Carry);
        }
        // Function not supported - set CF to indicate error
        _ => {
            debug!("INT 15h: unsupported function AH={:02X}", ah);
            vm.registers.ax.set_high(0x86); // Unsupported function
            vm.set_flag(CpuFlag::Carry);
        }
    }
}

// BIOS INT 09h handler — called via INT F0h trap from ROM stub.
// Reads the latched scancode from port 0x60, populates the BDA keyboard
// buffer, and sends EOI.  This is the end of the IRQ 1 chain: real programs
// can hook IVT[9] and chain to this stub.
pub fn int09h_bios(vm: &mut Runtime) {
    // Read scancode from port 0x60 (latch set by deliver_keyboard_irq)
    let scancode = if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_in_byte(0x60)
    } else {
        return;
    };

    debug!("[INT09-BIOS] scancode={:02X} pending_ascii={:02X}", scancode, vm.keyboard_pending_ascii);

    // BDA 0x496 bit 1 = "last scancode was E0 prefix"
    let kb_status3_addr = BDA_BASE + 0x96;
    let kb_status3 = vm.memory.read_byte(kb_status3_addr);

    // 0xE0 prefix byte: set the E0 flag and return (no BDA buffer entry).
    // The next IRQ will deliver the actual scancode.
    if scancode == 0xE0 {
        debug!("[INT09-BIOS] E0 prefix, setting flag");
        vm.memory.write_byte(kb_status3_addr, kb_status3 | 0x02);
        // Send EOI
        if let Some(ref mut io_bus) = vm.io_bus {
            io_bus.port_out_byte(0x20, 0x61);
        }
        return;
    }

    let e0_prefix = kb_status3 & 0x02 != 0;

    // Clear the E0 flag now that we've consumed it
    if e0_prefix {
        vm.memory.write_byte(kb_status3_addr, kb_status3 & !0x02);
    }

    // Break codes (bit 7 set): just send EOI and return.
    // TODO: track shift/ctrl/alt state for break codes in BDA 0x417
    if scancode & 0x80 != 0 {
        debug!("[INT09-BIOS] break code {:02X}, EOI", scancode);
        if let Some(ref mut io_bus) = vm.io_bus {
            io_bus.port_out_byte(0x20, 0x61);
        }
        return;
    }

    // Determine ASCII value for this scancode
    let ascii = if e0_prefix {
        // Enhanced key (arrow/nav): ASCII = 0xE0 for enhanced keyboard,
        // INT 16h AH=00 will convert to 0x00 for backward compat
        0xE0u8
    } else {
        // Regular key: use the pre-computed ASCII from deliver_keyboard_irq
        vm.keyboard_pending_ascii
    };

    // Make code: write (ascii, scancode) to BDA keyboard circular buffer
    let bda = BDA_BASE;
    let head = vm.memory.read_word(bda + 0x1A) as usize;
    let tail = vm.memory.read_word(bda + 0x1C) as usize;
    let mut next_tail = tail + 2;
    if next_tail >= 0x3E { next_tail = 0x1E; }

    if next_tail != head {
        vm.memory.write_byte(0x400 + tail, ascii);
        vm.memory.write_byte(0x400 + tail + 1, scancode);
        vm.memory.write_word(bda + 0x1C, next_tail as u16);
        debug!("[INT09-BIOS] BDA write ascii={:02X} sc={:02X} e0={} head={:02X} tail={:02X}->next={:02X}",
               ascii, scancode, e0_prefix, head, tail, next_tail);
    } else {
        debug!("[INT09-BIOS] BDA FULL! head={:02X} tail={:02X}", head, tail);
    }

    // Send specific EOI for IRQ 1
    if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_out_byte(0x20, 0x61);
    }
}

// INT 16h - Keyboard Services
pub fn int16h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    let bda = BDA_BASE;

    match ah {
        // Wait for key (AH=00) / Extended wait for key (AH=10)
        0x00 | 0x10 => {
            let head = vm.memory.read_word(bda + 0x1A) as usize;
            let tail = vm.memory.read_word(bda + 0x1C) as usize;

            if head != tail {
                // Read key from BDA buffer at 0x400 + head
                let mut ascii = vm.memory.read_byte(0x400 + head);
                let scancode = vm.memory.read_byte(0x400 + head + 1);

                // AH=00: convert enhanced prefix 0xE0 to 0x00 for backward compat
                if ah == 0x00 && ascii == 0xE0 {
                    ascii = 0x00;
                }

                vm.registers.ax.set_high(scancode);
                vm.registers.ax.set_low(ascii);

                // Advance head
                let mut new_head = head + 2;
                if new_head >= 0x3E {
                    new_head = 0x1E;
                }
                vm.memory.write_word(bda + 0x1A, new_head as u16);
                return;
            }

            // No key available: run periodic tasks (VGA refresh, timer) then retry.
            // Without this, the 1ms sleep × 1000 instruction threshold = ~1s VGA lag.
            vm.idle_tick();
            vm.registers.pc.set(vm.registers.op_pc);
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
        // Check key (AH=01, non-blocking) / Extended check (AH=11)
        0x01 | 0x11 => {
            let head = vm.memory.read_word(bda + 0x1A) as usize;
            let tail = vm.memory.read_word(bda + 0x1C) as usize;

            if head != tail {
                let mut ascii = vm.memory.read_byte(0x400 + head);
                let scancode = vm.memory.read_byte(0x400 + head + 1);

                // AH=01: convert enhanced prefix 0xE0 to 0x00 for backward compat
                if ah == 0x01 && ascii == 0xE0 {
                    ascii = 0x00;
                }

                vm.registers.ax.set_high(scancode);
                vm.registers.ax.set_low(ascii);
                vm.unset_flag(CpuFlag::Zero); // ZF=0 means key available
            } else {
                vm.set_flag(CpuFlag::Zero); // ZF=1 means no key
            }
        }
        // Get shift flags
        0x02 | 0x12 => {
            vm.registers.ax.set_low(0);
        }
        // Set typematic rate and delay — ignore, return success
        0x03 => {}
        // Write to keyboard buffer
        0x05 => {
            let scancode = vm.registers.cx.high();
            let ascii = vm.registers.cx.low();
            let head = vm.memory.read_word(bda + 0x1A) as usize;
            let tail = vm.memory.read_word(bda + 0x1C) as usize;
            let mut next_tail = tail + 2;
            if next_tail >= 0x3E {
                next_tail = 0x1E;
            }
            if next_tail == head {
                vm.registers.ax.set_low(1); // Buffer full
            } else {
                vm.memory.write_byte(0x400 + tail, ascii);
                vm.memory.write_byte(0x400 + tail + 1, scancode);
                vm.memory.write_word(bda + 0x1C, next_tail as u16);
                vm.registers.ax.set_low(0); // Success
            }
        }
        _ => {}
    }
}

// INT 19h - Bootstrap loader
pub fn int19h(vm: &mut Runtime) {
    let boot_order = vm.boot_order.clone();

    for &drive in &boot_order {
        let name = if drive >= 0x80 {
            format!("hard disk {}", drive - 0x80)
        } else {
            format!("floppy {}:", (b'A' + drive) as char)
        };
        let disk = if drive >= 0x80 {
            vm.hard_disks.get_mut((drive - 0x80) as usize).and_then(|d| d.as_mut())
        } else {
            vm.disks.get_mut(drive as usize).and_then(|d| d.as_mut())
        };

        if let Some(disk) = disk {
            match disk.read_sectors(0, 0, 1, 1) {
                Ok(data) => {
                    if data.len() >= 512 && data[510] == 0x55 && data[511] == 0xAA {
                        use crate::vm::memory::BOOT_ADDR;
                        for (i, &byte) in data.iter().enumerate() {
                            vm.memory.write_byte(BOOT_ADDR + i, byte);
                        }
                        debug!("BIOS: Booting from {} (DL={:02X})", name, drive);
                        vm.registers.dx.set_low(drive);
                        vm.registers.cs.reg_mut().set(0x0000);
                        vm.registers.pc.set(0x7C00);
                        return;
                    }
                }
                Err(e) => {
                    debug!("BIOS: Failed to read boot sector from {}: {}", name, e);
                }
            }
        }
    }

    eprintln!("BIOS: No bootable disk found");
    vm.exit(1);
}

// INT 1Ah - Time Services
pub fn int1ah(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // Get tick count
        0x00 => {
            let tick_lo = vm.memory.read_word(BDA_BASE + 0x6C);
            let tick_hi = vm.memory.read_word(BDA_BASE + 0x6E);
            vm.registers.cx.set(tick_hi);
            vm.registers.dx.set(tick_lo);
            vm.registers.ax.set_low(0); // Midnight flag
        }
        // Get RTC time
        0x02 => {
            // Return a fixed time: 12:00:00
            vm.registers.cx.set_high(0x12); // Hours (BCD)
            vm.registers.cx.set_low(0x00);  // Minutes (BCD)
            vm.registers.dx.set_high(0x00); // Seconds (BCD)
            vm.registers.dx.set_low(0x00);  // DST flag
            vm.unset_flag(CpuFlag::Carry);
        }
        // Get RTC date
        0x04 => {
            // Return a fixed date: 01/01/2000
            vm.registers.cx.set(0x2000); // Century (20) + Year (00) in BCD
            vm.registers.dx.set_high(0x01); // Month (BCD)
            vm.registers.dx.set_low(0x01);  // Day (BCD)
            vm.unset_flag(CpuFlag::Carry);
        }
        _ => {}
    }
}

fn teletype_output(vm: &mut Runtime, ch: u8) {
    let page: usize = 0;
    let cursor_offset = BDA_BASE + 0x50 + page * 2;
    let mut col = vm.memory.read_byte(cursor_offset) as usize;
    let mut row = vm.memory.read_byte(cursor_offset + 1) as usize;

    match ch {
        0x0D => { // CR
            col = 0;
        }
        0x0A => { // LF
            row += 1;
        }
        0x08 => { // BS
            if col > 0 {
                col -= 1;
            }
        }
        0x07 => { // BEL
            // Ring bell - just ignore
        }
        _ => {
            let pos = (row * 80 + col) * 2;
            vm.memory.write_byte(VGA_TEXT_BASE + pos, ch);
            vm.memory.write_byte(VGA_TEXT_BASE + pos + 1, 0x07);
            col += 1;
            if col >= 80 {
                col = 0;
                row += 1;
            }
        }
    }

    if row >= 25 {
        scroll_up(vm, 1, 0x07, 0, 0, 24, 79);
        row = 24;
    }

    vm.memory.write_byte(cursor_offset, col as u8);
    vm.memory.write_byte(cursor_offset + 1, row as u8);

    // Update CRT cursor
    let pos = row as u16 * 80 + col as u16;
    if let Some(io_bus) = &mut vm.io_bus {
        io_bus.port_out_byte(0x3D4, 0x0E);
        io_bus.port_out_byte(0x3D5, (pos >> 8) as u8);
        io_bus.port_out_byte(0x3D4, 0x0F);
        io_bus.port_out_byte(0x3D5, pos as u8);
    }

}

fn scroll_up(vm: &mut Runtime, lines: u8, attr: u8, top_row: usize, left_col: usize, bottom_row: usize, right_col: usize) {
    let lines = if lines == 0 { (bottom_row - top_row + 1) as u8 } else { lines };

    for row in top_row..=bottom_row {
        for col in left_col..=right_col {
            let src_row = row + lines as usize;
            if src_row <= bottom_row {
                let src = (src_row * 80 + col) * 2;
                let dst = (row * 80 + col) * 2;
                let ch = vm.memory.read_byte(VGA_TEXT_BASE + src);
                let a = vm.memory.read_byte(VGA_TEXT_BASE + src + 1);
                vm.memory.write_byte(VGA_TEXT_BASE + dst, ch);
                vm.memory.write_byte(VGA_TEXT_BASE + dst + 1, a);
            } else {
                let dst = (row * 80 + col) * 2;
                vm.memory.write_byte(VGA_TEXT_BASE + dst, b' ');
                vm.memory.write_byte(VGA_TEXT_BASE + dst + 1, attr);
            }
        }
    }
}

fn scroll_down(vm: &mut Runtime, lines: u8, attr: u8, top_row: usize, left_col: usize, bottom_row: usize, right_col: usize) {
    let lines = if lines == 0 { (bottom_row - top_row + 1) as u8 } else { lines };

    for row in (top_row..=bottom_row).rev() {
        for col in left_col..=right_col {
            if row >= lines as usize + top_row {
                let src_row = row - lines as usize;
                let src = (src_row * 80 + col) * 2;
                let dst = (row * 80 + col) * 2;
                let ch = vm.memory.read_byte(VGA_TEXT_BASE + src);
                let a = vm.memory.read_byte(VGA_TEXT_BASE + src + 1);
                vm.memory.write_byte(VGA_TEXT_BASE + dst, ch);
                vm.memory.write_byte(VGA_TEXT_BASE + dst + 1, a);
            } else {
                let dst = (row * 80 + col) * 2;
                vm.memory.write_byte(VGA_TEXT_BASE + dst, b' ');
                vm.memory.write_byte(VGA_TEXT_BASE + dst + 1, attr);
            }
        }
    }
}
