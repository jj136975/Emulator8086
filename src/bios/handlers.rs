use std::io::Write;
use log::debug;
use crate::io::keyboard::scancode_to_ascii;
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

// INT 11h - Equipment list
pub fn int11h(vm: &mut Runtime) {
    vm.registers.ax.set(0x0021); // 1 floppy, 80x25 color
}

// INT 12h - Memory size
pub fn int12h(vm: &mut Runtime) {
    vm.registers.ax.set(640); // 640KB conventional memory
}

// INT 13h - Disk Services
pub fn int13h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // Reset disk
        0x00 => {
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
            let dl = vm.registers.dx.low();
            let es = vm.registers.es.reg().word();
            let bx = vm.registers.bx.word();

            debug!("INT 13h READ: drive={:02X} C={} H={} S={} count={} -> {:04X}:{:04X}",
                   dl, cylinder, head, sector, count, es, bx);

            if let Some(ref mut disk) = vm.disk {
                match disk.read_sectors(cylinder, head, sector, count) {
                    Ok(data) => {
                        // Write data to ES:BX
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
                        vm.registers.ax.set_high(0x04); // Sector not found
                        vm.set_flag(CpuFlag::Carry);
                    }
                }
            } else {
                debug!("INT 13h READ: No disk!");
                vm.registers.ax.set_high(0x80); // Timeout
                vm.set_flag(CpuFlag::Carry);
            }
        }
        // Get drive parameters
        0x08 => {
            let dl = vm.registers.dx.low();
            if dl == 0x00 {
                if let Some(ref disk) = vm.disk {
                    let max_cyl = disk.cylinders - 1;
                    let max_head = disk.heads - 1;
                    let spt = disk.sectors_per_track;

                    vm.registers.ax.set_high(0);
                    vm.registers.bx.set_low(0x04); // Drive type: 1.44MB
                    vm.registers.cx.set_high(((max_cyl >> 2) as u8 & 0xC0) | (max_cyl as u8));
                    vm.registers.cx.set_low(((max_cyl >> 2) as u8 & 0xC0) | spt);
                    vm.registers.dx.set_high(max_head);
                    vm.registers.dx.set_low(1); // Number of drives
                    vm.unset_flag(CpuFlag::Carry);
                } else {
                    vm.registers.ax.set_high(0x07);
                    vm.set_flag(CpuFlag::Carry);
                }
            } else {
                vm.registers.ax.set_high(0x01);
                vm.set_flag(CpuFlag::Carry);
            }
        }
        _ => {
            vm.registers.ax.set_high(0x01); // Invalid function
            vm.set_flag(CpuFlag::Carry);
        }
    }
}

// INT 15h - System Services
pub fn int15h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // Function not supported - set CF to indicate error
        _ => {
            debug!("INT 15h: unsupported function AH={:02X}", ah);
            vm.registers.ax.set_high(0x86); // Unsupported function
            vm.set_flag(CpuFlag::Carry);
        }
    }
}

// INT 16h - Keyboard Services
pub fn int16h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // Wait for key
        0x00 => {
            // Block until a key is available
            loop {
                if let Some(ref mut io_bus) = vm.io_bus {
                    let status = io_bus.port_in_byte(0x64);
                    if status & 0x01 != 0 {
                        let scancode = io_bus.port_in_byte(0x60);
                        // Skip break codes
                        if scancode & 0x80 == 0 {
                            let ascii = scancode_to_ascii(scancode);
                            vm.registers.ax.set_high(scancode);
                            vm.registers.ax.set_low(ascii);
                            return;
                        }
                    }
                }
                std::thread::sleep(std::time::Duration::from_millis(10));
            }
        }
        // Check key (non-blocking)
        0x01 => {
            if let Some(ref mut io_bus) = vm.io_bus {
                let status = io_bus.port_in_byte(0x64);
                if status & 0x01 != 0 {
                    // Peek - don't consume
                    let scancode = io_bus.port_in_byte(0x60);
                    if scancode & 0x80 == 0 {
                        let ascii = scancode_to_ascii(scancode);
                        vm.registers.ax.set_high(scancode);
                        vm.registers.ax.set_low(ascii);
                        vm.unset_flag(CpuFlag::Zero); // ZF=0 means key available
                    } else {
                        vm.set_flag(CpuFlag::Zero); // ZF=1 means no key
                    }
                } else {
                    vm.set_flag(CpuFlag::Zero);
                }
            } else {
                vm.set_flag(CpuFlag::Zero);
            }
        }
        // Get shift flags
        0x02 => {
            vm.registers.ax.set_low(0);
        }
        _ => {}
    }
}

// INT 19h - Bootstrap loader
pub fn int19h(vm: &mut Runtime) {
    if let Some(ref mut disk) = vm.disk {
        match disk.read_sectors(0, 0, 1, 1) {
            Ok(data) => {
                use crate::vm::memory::BOOT_ADDR;
                for (i, &byte) in data.iter().enumerate() {
                    vm.memory.write_byte(BOOT_ADDR + i, byte);
                }
                // Check boot signature
                if data.len() >= 512 && data[510] == 0x55 && data[511] == 0xAA {
                    vm.registers.dx.set_low(0x00); // DL = boot drive (floppy)
                    vm.registers.cs.reg_mut().set(0x0000);
                    vm.registers.pc.set(0x7C00);
                } else {
                    eprintln!("BIOS: No bootable disk (missing 0x55AA signature)");
                    vm.exit(1);
                }
            }
            Err(e) => {
                eprintln!("BIOS: Failed to read boot sector: {}", e);
                vm.exit(1);
            }
        }
    } else {
        eprintln!("BIOS: No disk image loaded");
        vm.exit(1);
    }
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

    // Also print to host stdout
    let stdout = std::io::stdout();
    let mut lock = stdout.lock();
    let _ = lock.write_all(&[ch]);
    let _ = lock.flush();
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
