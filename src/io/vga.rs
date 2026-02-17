use std::io::{self, Write};

use crossterm::{cursor::MoveTo, style::{Attribute, Color, SetAttribute, SetBackgroundColor, SetForegroundColor}, QueueableCommand};
use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use crate::vm::memory::Memory;

/// CGA/VGA text mode — handles CRTC ports and renders B800:0000 to terminal.
///
/// Ports:
///   0x3D4 — CRTC index register
///   0x3D5 — CRTC data register
///   0x3D8 — Mode control register (ignored)
///   0x3D9 — Color select register (ignored)
///   0x3DA — Status register (vertical retrace toggle)
pub struct VgaTextMode {
    // CRTC state
    crtc_index: u8,
    crtc_regs: [u8; 256],

    // Status register — bit 0 and bit 3 toggle on each read
    status_toggle: bool,

    // Rendering state
    prev_buffer: [u16; 2000],   // previous frame for dirty detection
    cursor_prev: (u8, u8),       // previous cursor position (col, row)
    needs_full_redraw: bool,
}

const VIDEO_BASE: usize = 0xB8000;
const COLS: usize = 80;
const ROWS: usize = 25;
const CELLS: usize = COLS * ROWS;

impl VgaTextMode {
    pub fn new() -> Self {
        Self {
            crtc_index: 0,
            crtc_regs: [0; 256],
            status_toggle: false,
            prev_buffer: [0xFFFF; CELLS], // force full draw on first frame
            cursor_prev: (0xFF, 0xFF),
            needs_full_redraw: true,
        }
    }

    /// Get cursor position from CRTC registers 0x0E (high) and 0x0F (low).
    fn cursor_offset(&self) -> u16 {
        ((self.crtc_regs[0x0E] as u16) << 8) | self.crtc_regs[0x0F] as u16
    }

    /// Call periodically from main loop to render video RAM to terminal.
    /// Only redraws cells that changed since last call.
    pub fn render(&mut self, memory: &Memory) {
        let mut stdout = io::stdout();

        for i in 0..CELLS {
            let addr = VIDEO_BASE + i * 2;
            let ch = memory.read_byte(addr);
            let attr = memory.read_byte(addr + 1);
            let cell = (attr as u16) << 8 | ch as u16;

            if !self.needs_full_redraw && self.prev_buffer[i] == cell {
                continue;
            }
            self.prev_buffer[i] = cell;

            let row = (i / COLS) as u16;
            let col = (i % COLS) as u16;

            let fg = cga_to_color(attr & 0x0F);
            let bg = cga_to_color((attr >> 4) & 0x07);
            let bright = attr & 0x08 != 0;

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
            if bright {
                let _ = stdout.queue(SetAttribute(Attribute::Bold));
            } else {
                let _ = stdout.queue(SetAttribute(Attribute::NoBold));
            }
            let _ = write!(stdout, "{}", display_char);
        }

        self.needs_full_redraw = false;

        // Update cursor position
        let offset = self.cursor_offset();
        let col = (offset % COLS as u16) as u8;
        let row = (offset / COLS as u16) as u8;

        if (col, row) != self.cursor_prev {
            self.cursor_prev = (col, row);
            let _ = stdout.queue(MoveTo(col as u16, row as u16));
        }

        let _ = stdout.flush();
    }
}

impl IoDevice for VgaTextMode {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            // CRTC index
            0x3D4 => self.crtc_index,

            // CRTC data
            0x3D5 => self.crtc_regs[self.crtc_index as usize],

            // Status register — programs poll this for retrace timing
            0x3DA => {
                self.status_toggle = !self.status_toggle;
                if self.status_toggle {
                    0x09 // bit 0 (retrace) + bit 3 (vertical retrace)
                } else {
                    0x00
                }
            }

            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            // CRTC index register
            0x3D4 => self.crtc_index = value,

            // CRTC data register
            0x3D5 => {
                self.crtc_regs[self.crtc_index as usize] = value;
            }

            // Mode control register — ignore for text mode
            0x3D8 => {}

            // Color select register — ignore
            0x3D9 => {}

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