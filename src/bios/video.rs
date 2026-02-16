use crate::bios::bda;
use crate::vm::memory::VGA_TEXT_BASE;
use crate::vm::runtime::Runtime;

/// INT 10h — Video services
pub fn int10h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        0x00 => set_video_mode(vm),
        0x01 => set_cursor_shape(vm),
        0x02 => set_cursor_pos(vm),
        0x03 => get_cursor_pos(vm),
        0x05 => select_active_page(vm),
        0x06 => scroll_up(vm),
        0x07 => scroll_down(vm),
        0x08 => read_char_attr(vm),
        0x09 => write_char_attr(vm),
        0x0A => write_char_only(vm),
        0x0E => teletype_output(vm),
        0x0F => get_video_mode(vm),
        0x13 => write_string(vm),
        0x10 => {
            // AX=1003: toggle blinking/intensity
            let al = vm.registers.ax.low();
            if al == 0x03 {
                // Store BL, no-op for rendering
            }
        }
        0x11 => {
            // Font services — return character height info
            let al = vm.registers.ax.low();
            if al == 0x30 {
                // Get font info: return rows in DL, char height in CX
                let rows = vm.memory.read_byte(bda::VIDEO_ROWS);
                let height = vm.memory.read_word(bda::CHAR_HEIGHT);
                vm.registers.cx.set(height);
                vm.registers.dx.set_low(rows);
            }
        }
        0x12 => {
            // Alternate select (BL=10: get EGA info)
            let bl = vm.registers.bx.low();
            if bl == 0x10 {
                vm.registers.bx.set_high(0x00); // Color mode
                vm.registers.bx.set_low(0x03);  // 256K EGA memory
                vm.registers.cx.set(0x0000);     // Feature bits
            }
        }
        0x1A => {
            // Get/Set Display Combination Code
            let al = vm.registers.ax.low();
            if al == 0x00 {
                vm.registers.ax.set_low(0x1A); // Function supported
                vm.registers.bx.set_low(0x08); // VGA color
                vm.registers.bx.set_high(0x00); // No secondary
            }
        }
        _ => {}
    }
}

#[inline]
fn vga_addr(page: u8, row: u8, col: u8) -> usize {
    VGA_TEXT_BASE + (page as usize * 4000) + (row as usize * 160) + (col as usize * 2)
}

fn set_video_mode(vm: &mut Runtime) {
    let al = vm.registers.ax.low();
    let mode = al & 0x7F; // Bit 7 = don't clear screen
    let clear = al & 0x80 == 0;

    // Only support mode 3 (80x25 color) and 7 (mono, treat as 3)
    let effective_mode = if mode == 0x07 { 0x03 } else { mode };

    if clear {
        // Clear VGA text memory: space (0x20) + light gray on black (0x07)
        for i in 0..4000 {
            vm.memory.write_byte(VGA_TEXT_BASE + i * 2, 0x20);
            vm.memory.write_byte(VGA_TEXT_BASE + i * 2 + 1, 0x07);
        }
    }

    vm.memory.write_byte(bda::VIDEO_MODE, effective_mode);
    vm.memory.write_word(bda::VIDEO_COLS, 80);
    vm.memory.write_byte(bda::VIDEO_ROWS, 24);
    vm.memory.write_byte(bda::ACTIVE_PAGE, 0);

    // Reset all cursor positions
    for i in 0..16 {
        vm.memory.write_byte(bda::CURSOR_POS + i, 0);
    }
}

fn set_cursor_shape(vm: &mut Runtime) {
    let ch = vm.registers.cx.high();
    let cl = vm.registers.cx.low();
    vm.memory.write_byte(bda::CURSOR_SHAPE, ch);
    vm.memory.write_byte(bda::CURSOR_SHAPE + 1, cl);
}

fn set_cursor_pos(vm: &mut Runtime) {
    let page = vm.registers.bx.high();
    let row = vm.registers.dx.high();
    let col = vm.registers.dx.low();
    let offset = bda::CURSOR_POS + (page as usize & 0x07) * 2;
    vm.memory.write_byte(offset, col);
    vm.memory.write_byte(offset + 1, row);
}

fn get_cursor_pos(vm: &mut Runtime) {
    let page = vm.registers.bx.high();
    let offset = bda::CURSOR_POS + (page as usize & 0x07) * 2;
    let col = vm.memory.read_byte(offset);
    let row = vm.memory.read_byte(offset + 1);
    vm.registers.dx.set_high(row);
    vm.registers.dx.set_low(col);
    let shape_start = vm.memory.read_byte(bda::CURSOR_SHAPE);
    let shape_end = vm.memory.read_byte(bda::CURSOR_SHAPE + 1);
    vm.registers.cx.set_high(shape_start);
    vm.registers.cx.set_low(shape_end);
}

fn select_active_page(vm: &mut Runtime) {
    let al = vm.registers.ax.low();
    vm.memory.write_byte(bda::ACTIVE_PAGE, al);
}

fn scroll_up(vm: &mut Runtime) {
    let lines = vm.registers.ax.low();
    let attr = vm.registers.bx.high();
    let top_row = vm.registers.cx.high();
    let left_col = vm.registers.cx.low();
    let bottom_row = vm.registers.dx.high();
    let right_col = vm.registers.dx.low();
    let page = vm.memory.read_byte(bda::ACTIVE_PAGE);

    let lines = if lines == 0 {
        bottom_row - top_row + 1
    } else {
        lines
    };

    scroll(vm, page, top_row, left_col, bottom_row, right_col, attr, lines as i8);
}

fn scroll_down(vm: &mut Runtime) {
    let lines = vm.registers.ax.low();
    let attr = vm.registers.bx.high();
    let top_row = vm.registers.cx.high();
    let left_col = vm.registers.cx.low();
    let bottom_row = vm.registers.dx.high();
    let right_col = vm.registers.dx.low();
    let page = vm.memory.read_byte(bda::ACTIVE_PAGE);

    let lines = if lines == 0 {
        bottom_row - top_row + 1
    } else {
        lines
    };

    scroll(vm, page, top_row, left_col, bottom_row, right_col, attr, -(lines as i8));
}

fn scroll(vm: &mut Runtime, page: u8, top: u8, left: u8, bottom: u8, right: u8, attr: u8, lines: i8) {
    if lines > 0 {
        // Scroll up
        let n = lines as u8;
        for row in top..=bottom {
            for col in left..=right {
                if row + n <= bottom {
                    let src = vga_addr(page, row + n, col);
                    let dst = vga_addr(page, row, col);
                    let ch = vm.memory.read_byte(src);
                    let at = vm.memory.read_byte(src + 1);
                    vm.memory.write_byte(dst, ch);
                    vm.memory.write_byte(dst + 1, at);
                } else {
                    let dst = vga_addr(page, row, col);
                    vm.memory.write_byte(dst, 0x20);
                    vm.memory.write_byte(dst + 1, attr);
                }
            }
        }
    } else if lines < 0 {
        // Scroll down
        let n = (-lines) as u8;
        for row in (top..=bottom).rev() {
            for col in left..=right {
                if row >= top + n {
                    let src = vga_addr(page, row - n, col);
                    let dst = vga_addr(page, row, col);
                    let ch = vm.memory.read_byte(src);
                    let at = vm.memory.read_byte(src + 1);
                    vm.memory.write_byte(dst, ch);
                    vm.memory.write_byte(dst + 1, at);
                } else {
                    let dst = vga_addr(page, row, col);
                    vm.memory.write_byte(dst, 0x20);
                    vm.memory.write_byte(dst + 1, attr);
                }
            }
        }
    }
}

fn read_char_attr(vm: &mut Runtime) {
    let page = vm.registers.bx.high();
    let offset = bda::CURSOR_POS + (page as usize & 0x07) * 2;
    let col = vm.memory.read_byte(offset);
    let row = vm.memory.read_byte(offset + 1);
    let addr = vga_addr(page, row, col);
    let ch = vm.memory.read_byte(addr);
    let attr = vm.memory.read_byte(addr + 1);
    vm.registers.ax.set_low(ch);
    vm.registers.ax.set_high(attr);
}

fn write_char_attr(vm: &mut Runtime) {
    let ch = vm.registers.ax.low();
    let page = vm.registers.bx.high();
    let attr = vm.registers.bx.low();
    let count = vm.registers.cx.word();

    let offset = bda::CURSOR_POS + (page as usize & 0x07) * 2;
    let col = vm.memory.read_byte(offset);
    let row = vm.memory.read_byte(offset + 1);

    let mut pos = vga_addr(page, row, col);
    for _ in 0..count {
        vm.memory.write_byte(pos, ch);
        vm.memory.write_byte(pos + 1, attr);
        pos += 2;
    }
    // Does NOT advance cursor
}

fn write_char_only(vm: &mut Runtime) {
    let ch = vm.registers.ax.low();
    let page = vm.registers.bx.high();
    let count = vm.registers.cx.word();

    let offset = bda::CURSOR_POS + (page as usize & 0x07) * 2;
    let col = vm.memory.read_byte(offset);
    let row = vm.memory.read_byte(offset + 1);

    let mut pos = vga_addr(page, row, col);
    for _ in 0..count {
        vm.memory.write_byte(pos, ch);
        pos += 2; // Skip attribute byte (preserve existing)
    }
    // Does NOT advance cursor
}

fn teletype_output(vm: &mut Runtime) {
    let ch = vm.registers.ax.low();
    // INT 10h/0Eh always operates on the current active page
    let page = vm.memory.read_byte(bda::ACTIVE_PAGE);

    let offset = bda::CURSOR_POS + (page as usize & 0x07) * 2;
    let mut col = vm.memory.read_byte(offset);
    let mut row = vm.memory.read_byte(offset + 1);

    match ch {
        0x0D => {
            // Carriage return
            col = 0;
        }
        0x0A => {
            // Line feed
            row += 1;
        }
        0x08 => {
            // Backspace
            if col > 0 {
                col -= 1;
            }
        }
        0x07 => {
            // Bell — ignore
        }
        _ => {
            // Normal character: write with current attribute
            let addr = vga_addr(page, row, col);
            let attr = vm.memory.read_byte(addr + 1);
            vm.memory.write_byte(addr, ch);
            // Preserve existing attribute if it's non-zero, otherwise use 0x07
            if attr == 0 {
                vm.memory.write_byte(addr + 1, 0x07);
            }
            col += 1;
        }
    }

    // Handle line wrap
    if col >= 80 {
        col = 0;
        row += 1;
    }

    // Handle scrolling
    if row >= 25 {
        scroll(vm, page, 0, 0, 24, 79, 0x07, 1);
        row = 24;
    }

    // Update cursor position in BDA
    vm.memory.write_byte(offset, col);
    vm.memory.write_byte(offset + 1, row);
}

fn get_video_mode(vm: &mut Runtime) {
    let mode = vm.memory.read_byte(bda::VIDEO_MODE);
    let cols = vm.memory.read_word(bda::VIDEO_COLS);
    let page = vm.memory.read_byte(bda::ACTIVE_PAGE);
    vm.registers.ax.set_low(mode);
    vm.registers.ax.set_high(cols as u8);
    vm.registers.bx.set_high(page);
}

fn write_string(vm: &mut Runtime) {
    let mode = vm.registers.ax.low();
    let attr = vm.registers.bx.low();
    let page = vm.registers.bx.high();
    let length = vm.registers.cx.word();
    let row = vm.registers.dx.high();
    let col = vm.registers.dx.low();

    // Set cursor position for writing
    let cursor_offset = bda::CURSOR_POS + (page as usize & 0x07) * 2;
    let save_col = vm.memory.read_byte(cursor_offset);
    let save_row = vm.memory.read_byte(cursor_offset + 1);

    vm.memory.write_byte(cursor_offset, col);
    vm.memory.write_byte(cursor_offset + 1, row);

    // Get string address from ES:BP
    let es = vm.registers.es.reg().word() as usize;
    let bp = vm.registers.bp.word() as usize;
    let base = ((es << 4) + bp) & 0xFFFFF;

    let interleaved = mode & 0x02 != 0; // Bit 1: char+attr pairs
    let update_cursor = mode & 0x01 != 0; // Bit 0: update cursor after

    let mut str_offset = 0usize;
    for _ in 0..length {
        let ch = vm.memory.read_byte((base + str_offset) & 0xFFFFF);
        str_offset += 1;

        if interleaved {
            let char_attr = vm.memory.read_byte((base + str_offset) & 0xFFFFF);
            str_offset += 1;
            // Write char with specified attribute
            let cur_off = bda::CURSOR_POS + (page as usize & 0x07) * 2;
            let c = vm.memory.read_byte(cur_off);
            let r = vm.memory.read_byte(cur_off + 1);
            let addr = vga_addr(page, r, c);
            vm.memory.write_byte(addr, ch);
            vm.memory.write_byte(addr + 1, char_attr);
            // Advance cursor manually
            advance_cursor(vm, page);
        } else {
            // Use teletype-style output with fixed attribute
            // Set BL attribute for write
            let cur_off = bda::CURSOR_POS + (page as usize & 0x07) * 2;
            let c = vm.memory.read_byte(cur_off);
            let r = vm.memory.read_byte(cur_off + 1);

            match ch {
                0x0D | 0x0A | 0x08 | 0x07 => {
                    // Control chars: use teletype logic
                    vm.registers.ax.set_low(ch);
                    vm.registers.bx.set_high(page);
                    teletype_output(vm);
                }
                _ => {
                    let addr = vga_addr(page, r, c);
                    vm.memory.write_byte(addr, ch);
                    vm.memory.write_byte(addr + 1, attr);
                    advance_cursor(vm, page);
                }
            }
        }
    }

    if !update_cursor {
        // Restore original cursor position
        vm.memory.write_byte(cursor_offset, save_col);
        vm.memory.write_byte(cursor_offset + 1, save_row);
    }
}

fn advance_cursor(vm: &mut Runtime, page: u8) {
    let offset = bda::CURSOR_POS + (page as usize & 0x07) * 2;
    let mut col = vm.memory.read_byte(offset);
    let mut row = vm.memory.read_byte(offset + 1);

    col += 1;
    if col >= 80 {
        col = 0;
        row += 1;
    }
    if row >= 25 {
        scroll(vm, page, 0, 0, 24, 79, 0x07, 1);
        row = 24;
    }
    vm.memory.write_byte(offset, col);
    vm.memory.write_byte(offset + 1, row);
}
