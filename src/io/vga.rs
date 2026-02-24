use std::io::{self, Write};

use crossterm::{cursor::MoveTo, style::{Color, SetBackgroundColor, SetForegroundColor}, QueueableCommand};
use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use crate::vm::memory::Memory;

/// CGA/VGA text mode — handles CRTC ports and renders B800:0000 to terminal.
///
/// Ports:
///   0x3D0/0x3D2/0x3D4 — CRTC index register (mirrors)
///   0x3D1/0x3D3/0x3D5 — CRTC data register (mirrors)
///   0x3D8 — Mode control register
///   0x3D9 — Color select register
///   0x3DA — Status register (cycle-based retrace timing)
pub struct VgaTextMode {
    // CRTC state
    crtc_index: u8,
    crtc_regs: [u8; 256],

    // Mode control register (port 0x3D8)
    mode_register: u8,

    // Color select register (port 0x3D9)
    color_register: u8,

    // Status register — cycle counter for realistic retrace timing
    cycle_counter: u64,

    // Rendering state
    prev_buffer: [u16; 80 * 25],   // max size buffer for dirty detection
    cursor_prev: (u8, u8),          // previous cursor position (col, row)
    needs_full_redraw: bool,
    blink_mode: bool,
}

const VIDEO_BASE: usize = 0xB8000;
const ROWS: usize = 25;

impl VgaTextMode {
    pub fn new() -> Self {
        Self {
            crtc_index: 0,
            crtc_regs: [0; 256],
            mode_register: 0x29, // 80-col text, display enabled, blink
            color_register: 0,
            cycle_counter: 0,
            prev_buffer: [0xFFFF; 80 * 25], // force full draw on first frame
            cursor_prev: (0xFF, 0xFF),
            needs_full_redraw: true,
            blink_mode: true, // matches mode_register bit 5
        }
    }

    /// Advance the cycle counter externally (called from runtime).
    pub fn tick(&mut self, cycles: u64) {
        self.cycle_counter += cycles;
    }

    /// Dynamic column count from CRTC register 1 (Horizontal Displayed).
    fn cols(&self) -> usize {
        let crtc_r1 = self.crtc_regs[0x01] as usize;
        if crtc_r1 > 0 && crtc_r1 <= 80 { crtc_r1 } else { 80 }
    }

    /// Get cursor position from CRTC registers 0x0E (high) and 0x0F (low).
    fn cursor_offset(&self) -> u16 {
        ((self.crtc_regs[0x0E] as u16) << 8) | self.crtc_regs[0x0F] as u16
    }

    /// Return the display start offset from CRTC registers 0x0C/0x0D.
    /// The CRTC stores the offset in character cells (not bytes).
    fn start_address(&self) -> usize {
        let addr = ((self.crtc_regs[0x0C] as u16) << 8) | self.crtc_regs[0x0D] as u16;
        addr as usize
    }

    pub fn queue_full_redraw(&mut self) {
        self.needs_full_redraw = true;
    }

    /// Call periodically from main loop to render video RAM to terminal.
    /// Only redraws cells that changed since last call.
    pub fn render(&mut self, memory: &Memory) {
        let mut stdout = io::stdout();

        // Note: MODE_ENABLE bit (bit 3 of port 0x3D8) is ignored for rendering.
        // Programs like EDIT/QBasic disable the CGA display during video memory
        // writes to prevent "snow" artifacts on real hardware. Since we render to
        // a terminal, there is no snow — always render the video buffer contents.

        let start = self.start_address();
        let cols = self.cols();
        let cells = cols * ROWS;

        for i in 0..cells {
            // Apply start address offset — wraps within 16KB text page
            let cell_index = (start + i) & 0x1FFF; // wrap within 8K character cells (16KB bytes)
            let addr = VIDEO_BASE + cell_index * 2;
            let ch = memory.read_byte(addr);
            let attr = memory.read_byte(addr + 1);
            let cell = (attr as u16) << 8 | ch as u16;

            if !self.needs_full_redraw && self.prev_buffer[i] == cell {
                continue;
            }
            self.prev_buffer[i] = cell;

            let row = (i / cols) as u16;
            let col = (i % cols) as u16;

            let (fg_index, bg_index) = if self.blink_mode {
                (attr & 0x0F, (attr >> 4) & 0x07)  // 8 background colors, bit 7 = blink
            } else {
                (attr & 0x0F, (attr >> 4) & 0x0F)  // 16 background colors
            };
            let fg = cga_to_color(fg_index);
            let bg = cga_to_color(bg_index);

            let display_char = if ch >= 0x20 && ch < 0x7F {
                ch as char
            } else if ch == 0x00 {
                ' '
            } else {
                cp437_to_unicode(ch)
            };

            let _ = stdout.queue(MoveTo(col, row));
            let _ = stdout.queue(SetForegroundColor(fg));
            let _ = stdout.queue(SetBackgroundColor(bg));
            let _ = write!(stdout, "{}", display_char);
        }

        self.needs_full_redraw = false;

        // Update cursor position (clamped to visible area)
        let offset = self.cursor_offset();
        let cursor_rel = offset.wrapping_sub(start as u16);
        let col = (cursor_rel % cols as u16).min(cols as u16 - 1);
        let row = (cursor_rel / cols as u16).min(ROWS as u16 - 1);

        if (col as u8, row as u8) != self.cursor_prev {
            self.cursor_prev = (col as u8, row as u8);
            let _ = stdout.queue(MoveTo(col, row));
        }

        let _ = stdout.flush();
    }

    /// Dump VGA text buffer to a file for debugging.
    /// Shows each row as: row number, ASCII text, then hex dump of char+attr pairs.
    pub fn dump(&self, memory: &Memory, path: &str) {
        use std::fs::File;
        let start = self.start_address();
        let cols = self.cols();
        let mut f = match File::create(path) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("VGA dump failed: {}", e);
                return;
            }
        };
        let offset = self.cursor_offset();
        let _ = writeln!(f, "=== VGA Diagnostic Dump ===");
        let _ = writeln!(f, "CRTC start_address={} cursor_offset={} (row={}, col={})",
            start, offset, offset / cols as u16, offset % cols as u16);
        let _ = writeln!(f, "CRTC regs: 0x0C={:02X} 0x0D={:02X} 0x0E={:02X} 0x0F={:02X}",
            self.crtc_regs[0x0C], self.crtc_regs[0x0D], self.crtc_regs[0x0E], self.crtc_regs[0x0F]);
        // BDA fields (segment 0x0040, physical 0x400+offset)
        let _ = writeln!(f, "BDA video_mode={:02X} video_cols={} active_page={:02X}",
            memory.read_byte(0x0449), // BDA_VIDEO_MODE
            memory.read_word(0x044A), // BDA_VIDEO_COLS
            memory.read_byte(0x0462)); // BDA_ACTIVE_PAGE
        let _ = writeln!(f, "BDA cursor_pos page0: col={:02X} row={:02X}",
            memory.read_byte(0x0450), memory.read_byte(0x0451));
        // Show all 8 page cursor positions
        let _ = write!(f, "BDA cursor_pos all pages:");
        for p in 0..8 {
            let addr = 0x0450 + p * 2;
            let _ = write!(f, " p{}=[{:02X},{:02X}]", p,
                memory.read_byte(addr), memory.read_byte(addr + 1));
        }
        let _ = writeln!(f);

        // Mode register
        let _ = writeln!(f, "Mode register: {:02X} (display {}abled, blink={})",
            self.mode_register,
            if self.mode_register & 0x08 != 0 { "en" } else { "dis" },
            self.blink_mode);

        // IVT entries (to detect if program replaced handlers)
        let _ = writeln!(f, "IVT entries:");
        for &(vec, name) in &[
            (0x08u8, "INT 08h (Timer)"),
            (0x09, "INT 09h (Keyboard)"),
            (0x10, "INT 10h (Video)"),
            (0x15, "INT 15h (System)"),
            (0x16, "INT 16h (Kbd Svc)"),
            (0x1C, "INT 1Ch (Timer Hook)"),
        ] {
            let addr = (vec as usize) * 4;
            let off = memory.read_word(addr);
            let seg = memory.read_word(addr + 2);
            let _ = writeln!(f, "  {} = {:04X}:{:04X}", name, seg, off);
        }

        // BDA keyboard buffer state
        let kbd_head = memory.read_word(0x041A);
        let kbd_tail = memory.read_word(0x041C);
        let kbd_start = memory.read_word(0x0480);
        let kbd_end = memory.read_word(0x0482);
        let kbd_flags1 = memory.read_byte(0x0417);
        let kbd_flags2 = memory.read_byte(0x0418);
        let _ = writeln!(f, "Keyboard buffer: head={:04X} tail={:04X} start={:04X} end={:04X}",
            kbd_head, kbd_tail, kbd_start, kbd_end);
        let _ = writeln!(f, "Keyboard flags: flags1={:02X} flags2={:02X}", kbd_flags1, kbd_flags2);
        // Dump buffer contents
        let _ = write!(f, "Buffer contents:");
        let mut ptr = kbd_start;
        while ptr < kbd_end {
            let w = memory.read_word(0x0400 + ptr as usize);
            if ptr == kbd_head { let _ = write!(f, " [H"); }
            if ptr == kbd_tail { let _ = write!(f, " [T"); }
            let _ = write!(f, " {:04X}", w);
            if ptr == kbd_head || ptr == kbd_tail { let _ = write!(f, "]"); }
            ptr += 2;
        }
        let _ = writeln!(f);

        // PIC state from BDA (IMR from port 0x21 isn't stored in BDA, but we can show IRQ mask)
        let _ = writeln!(f);
        for row in 0..ROWS {
            // ASCII text line
            let _ = write!(f, "Row {:02}: |", row);
            for col in 0..cols {
                let cell_index = (start + row * cols + col) & 0x1FFF;
                let ch = memory.read_byte(VIDEO_BASE + cell_index * 2);
                if ch >= 0x20 && ch < 0x7F {
                    let _ = write!(f, "{}", ch as char);
                } else {
                    let _ = write!(f, ".");
                }
            }
            let _ = writeln!(f, "|");
            // Hex dump line
            let _ = write!(f, "        ");
            for col in 0..cols {
                let cell_index = (start + row * cols + col) & 0x1FFF;
                let addr = VIDEO_BASE + cell_index * 2;
                let ch = memory.read_byte(addr);
                let attr = memory.read_byte(addr + 1);
                let _ = write!(f, "{:02X}{:02X} ", ch, attr);
            }
            let _ = writeln!(f);
        }
        eprintln!("VGA dumped to {}", path);
    }
}

impl IoDevice for VgaTextMode {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            // CRTC index (with mirrors)
            0x3D0 | 0x3D2 | 0x3D4 => self.crtc_index,

            // CRTC data (with mirrors) — only readable for registers 0x0A-0x0F
            0x3D1 | 0x3D3 | 0x3D5 => {
                match self.crtc_index {
                    0x0A..=0x0F => self.crtc_regs[self.crtc_index as usize],
                    _ => 0x00,
                }
            }

            // Mode control register
            0x3D8 => self.mode_register,

            // Color select register
            0x3D9 => self.color_register,

            // Status register — programs poll this for retrace timing.
            // Reading 0x3DA also resets the attribute controller address flip-flop
            // (relevant for EGA/VGA, noted here for future compatibility).
            0x3DA => {
                self.cycle_counter += 1;

                // Simulate hblank: CGA has ~76 character clocks per scanline,
                // display active for ~40 chars, blanking for ~36 chars.
                let bit0 = (self.cycle_counter % 76) >= 40;

                // Simulate vblank: ~262 scanlines per frame,
                // vblank starts around scanline 225.
                let scanline = (self.cycle_counter / 76) % 262;
                let bit3 = scanline >= 225;

                // Upper nibble = 0xF0 (floating bits on CGA hardware)
                0xF0 | ((bit3 as u8) << 3) | (bit0 as u8)
            }

            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            // CRTC index register (with mirrors)
            0x3D0 | 0x3D2 | 0x3D4 => self.crtc_index = value,

            // CRTC data register (with mirrors) — validated writes
            0x3D1 | 0x3D3 | 0x3D5 => {
                if self.crtc_index <= 17 {
                    let masked = match self.crtc_index {
                        0x04 => value & 0x7F,  // Vertical Total
                        0x05 => value & 0x1F,  // VT Adjust
                        0x06 => value & 0x7F,  // Vertical Displayed
                        0x07 => value & 0x7F,  // Vertical Sync Position
                        0x09 => value & 0x1F,  // Max Scan Line
                        0x0A => value & 0x7F,  // Cursor Start
                        0x0B => value & 0x1F,  // Cursor End
                        0x0C => value & 0x3F,  // Start Address High
                        _ => value,
                    };
                    self.crtc_regs[self.crtc_index as usize] = masked;
                }
            }

            // Mode control register
            0x3D8 => {
                self.mode_register = value;
                self.blink_mode = value & 0x20 != 0;
                // Bit 3 (MODE_ENABLE) is checked in render()
                // Bit 0 (HIRES_TEXT) affects 80 vs 40 column mode
            }

            // Color select register
            0x3D9 => {
                self.color_register = value;
            }

            _ => {}
        }
    }

    fn name(&self) -> &'static str {
        "VGA Text Mode"
    }
}

/// Map CGA 4-bit color index to crossterm Color.
fn cga_to_color(index: u8) -> Color {
    match index & 0x0F {
        0x00 => Color::Rgb { r: 0, g: 0, b: 0 },          // Black
        0x01 => Color::Rgb { r: 0, g: 0, b: 170 },        // Blue
        0x02 => Color::Rgb { r: 0, g: 170, b: 0 },        // Green
        0x03 => Color::Rgb { r: 0, g: 170, b: 170 },      // Cyan
        0x04 => Color::Rgb { r: 170, g: 0, b: 0 },        // Red
        0x05 => Color::Rgb { r: 170, g: 0, b: 170 },      // Magenta
        0x06 => Color::Rgb { r: 170, g: 85, b: 0 },       // Brown
        0x07 => Color::Rgb { r: 170, g: 170, b: 170 },    // Light gray
        0x08 => Color::Rgb { r: 85, g: 85, b: 85 },       // Dark gray
        0x09 => Color::Rgb { r: 85, g: 85, b: 255 },      // Light blue
        0x0A => Color::Rgb { r: 85, g: 255, b: 85 },      // Light green
        0x0B => Color::Rgb { r: 85, g: 255, b: 255 },     // Light cyan
        0x0C => Color::Rgb { r: 255, g: 85, b: 85 },      // Light red
        0x0D => Color::Rgb { r: 255, g: 85, b: 255 },     // Light magenta
        0x0E => Color::Rgb { r: 255, g: 255, b: 85 },     // Yellow
        0x0F => Color::Rgb { r: 255, g: 255, b: 255 },    // White
        _ => Color::Rgb { r: 170, g: 170, b: 170 },
    }
}

/// Map CP437 byte to Unicode for common box drawing and special characters.
fn cp437_to_unicode(byte: u8) -> char {
    const TABLE: [char; 256] = {
        let mut t = [' '; 256];

        // Control characters (0x00-0x1F) — display as spaces or symbols
        t[0x01] = '\u{263A}'; // smiley
        t[0x02] = '\u{263B}'; // dark smiley
        t[0x03] = '\u{2665}'; // heart
        t[0x04] = '\u{2666}'; // diamond
        t[0x05] = '\u{2663}'; // club
        t[0x06] = '\u{2660}'; // spade
        t[0x07] = '\u{2022}'; // bullet
        t[0x08] = '\u{25D8}'; // inverse bullet
        t[0x09] = '\u{25CB}'; // circle
        t[0x0A] = '\u{25D9}'; // inverse circle
        t[0x0B] = '\u{2642}'; // male
        t[0x0C] = '\u{2640}'; // female
        t[0x0D] = '\u{266A}'; // note
        t[0x0E] = '\u{266B}'; // double note
        t[0x0F] = '\u{263C}'; // sun
        t[0x10] = '\u{25BA}'; // right triangle
        t[0x11] = '\u{25C4}'; // left triangle
        t[0x12] = '\u{2195}'; // up-down arrow
        t[0x13] = '\u{203C}'; // double exclaim
        t[0x14] = '\u{00B6}'; // pilcrow
        t[0x15] = '\u{00A7}'; // section
        t[0x16] = '\u{25AC}'; // black rectangle
        t[0x17] = '\u{21A8}'; // up-down arrow w/ base
        t[0x18] = '\u{2191}'; // up arrow
        t[0x19] = '\u{2193}'; // down arrow
        t[0x1A] = '\u{2192}'; // right arrow
        t[0x1B] = '\u{2190}'; // left arrow
        t[0x1C] = '\u{221F}'; // right angle
        t[0x1D] = '\u{2194}'; // left-right arrow
        t[0x1E] = '\u{25B2}'; // up triangle
        t[0x1F] = '\u{25BC}'; // down triangle

        // Printable ASCII (0x20-0x7E) — identity mapping
        let mut i = 0x20u16;
        while i <= 0x7E {
            t[i as usize] = i as u8 as char;
            i += 1;
        }
        t[0x7F] = '\u{2302}'; // house

        // Extended characters (0x80-0xFF)
        t[0x80] = '\u{00C7}'; t[0x81] = '\u{00FC}'; t[0x82] = '\u{00E9}';
        t[0x83] = '\u{00E2}'; t[0x84] = '\u{00E4}'; t[0x85] = '\u{00E0}';
        t[0x86] = '\u{00E5}'; t[0x87] = '\u{00E7}'; t[0x88] = '\u{00EA}';
        t[0x89] = '\u{00EB}'; t[0x8A] = '\u{00E8}'; t[0x8B] = '\u{00EF}';
        t[0x8C] = '\u{00EE}'; t[0x8D] = '\u{00EC}'; t[0x8E] = '\u{00C4}';
        t[0x8F] = '\u{00C5}'; t[0x90] = '\u{00C9}'; t[0x91] = '\u{00E6}';
        t[0x92] = '\u{00C6}'; t[0x93] = '\u{00F4}'; t[0x94] = '\u{00F6}';
        t[0x95] = '\u{00F2}'; t[0x96] = '\u{00FB}'; t[0x97] = '\u{00F9}';
        t[0x98] = '\u{00FF}'; t[0x99] = '\u{00D6}'; t[0x9A] = '\u{00DC}';
        t[0x9B] = '\u{00A2}'; t[0x9C] = '\u{00A3}'; t[0x9D] = '\u{00A5}';
        t[0x9E] = '\u{20A7}'; t[0x9F] = '\u{0192}';
        t[0xA0] = '\u{00E1}'; t[0xA1] = '\u{00ED}'; t[0xA2] = '\u{00F3}';
        t[0xA3] = '\u{00FA}'; t[0xA4] = '\u{00F1}'; t[0xA5] = '\u{00D1}';
        t[0xA6] = '\u{00AA}'; t[0xA7] = '\u{00BA}'; t[0xA8] = '\u{00BF}';
        t[0xA9] = '\u{2310}'; t[0xAA] = '\u{00AC}'; t[0xAB] = '\u{00BD}';
        t[0xAC] = '\u{00BC}'; t[0xAD] = '\u{00A1}'; t[0xAE] = '\u{00AB}';
        t[0xAF] = '\u{00BB}';

        // Box drawing (0xB0-0xDF)
        t[0xB0] = '\u{2591}'; t[0xB1] = '\u{2592}'; t[0xB2] = '\u{2593}';
        t[0xB3] = '\u{2502}'; t[0xB4] = '\u{2524}'; t[0xB5] = '\u{2561}';
        t[0xB6] = '\u{2562}'; t[0xB7] = '\u{2556}'; t[0xB8] = '\u{2555}';
        t[0xB9] = '\u{2563}'; t[0xBA] = '\u{2551}'; t[0xBB] = '\u{2557}';
        t[0xBC] = '\u{255D}'; t[0xBD] = '\u{255C}'; t[0xBE] = '\u{255B}';
        t[0xBF] = '\u{2510}'; t[0xC0] = '\u{2514}'; t[0xC1] = '\u{2534}';
        t[0xC2] = '\u{252C}'; t[0xC3] = '\u{251C}'; t[0xC4] = '\u{2500}';
        t[0xC5] = '\u{253C}'; t[0xC6] = '\u{255E}'; t[0xC7] = '\u{255F}';
        t[0xC8] = '\u{255A}'; t[0xC9] = '\u{2554}'; t[0xCA] = '\u{2569}';
        t[0xCB] = '\u{2566}'; t[0xCC] = '\u{2560}'; t[0xCD] = '\u{2550}';
        t[0xCE] = '\u{256C}'; t[0xCF] = '\u{2567}'; t[0xD0] = '\u{2568}';
        t[0xD1] = '\u{2564}'; t[0xD2] = '\u{2565}'; t[0xD3] = '\u{2559}';
        t[0xD4] = '\u{2558}'; t[0xD5] = '\u{2552}'; t[0xD6] = '\u{2553}';
        t[0xD7] = '\u{256B}'; t[0xD8] = '\u{256A}'; t[0xD9] = '\u{2518}';
        t[0xDA] = '\u{250C}'; t[0xDB] = '\u{2588}'; t[0xDC] = '\u{2584}';
        t[0xDD] = '\u{258C}'; t[0xDE] = '\u{2590}'; t[0xDF] = '\u{2580}';

        // Greek/math (0xE0-0xFF)
        t[0xE0] = '\u{03B1}'; t[0xE1] = '\u{00DF}'; t[0xE2] = '\u{0393}';
        t[0xE3] = '\u{03C0}'; t[0xE4] = '\u{03A3}'; t[0xE5] = '\u{03C3}';
        t[0xE6] = '\u{00B5}'; t[0xE7] = '\u{03C4}'; t[0xE8] = '\u{03A6}';
        t[0xE9] = '\u{0398}'; t[0xEA] = '\u{03A9}'; t[0xEB] = '\u{03B4}';
        t[0xEC] = '\u{221E}'; t[0xED] = '\u{03C6}'; t[0xEE] = '\u{03B5}';
        t[0xEF] = '\u{2229}'; t[0xF0] = '\u{2261}'; t[0xF1] = '\u{00B1}';
        t[0xF2] = '\u{2265}'; t[0xF3] = '\u{2264}'; t[0xF4] = '\u{2320}';
        t[0xF5] = '\u{2321}'; t[0xF6] = '\u{00F7}'; t[0xF7] = '\u{2248}';
        t[0xF8] = '\u{00B0}'; t[0xF9] = '\u{2219}'; t[0xFA] = '\u{00B7}';
        t[0xFB] = '\u{221A}'; t[0xFC] = '\u{207F}'; t[0xFD] = '\u{00B2}';
        t[0xFE] = '\u{25A0}'; t[0xFF] = '\u{00A0}';

        t
    };

    TABLE[byte as usize]
}
