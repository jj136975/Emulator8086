//! Terminal frontend: draws the 80x25 text screen with ANSI escape
//! sequences. Mode 13h needs the GUI frontend; while it is active the
//! terminal is left as it is.

use crossterm::cursor::{Hide, MoveTo, Show};
use crossterm::style::{Color, SetBackgroundColor, SetForegroundColor};
use crossterm::Command;

use crate::io::vga::{VgaDevice, VideoMode};
use crate::vm::memory::Memory;

const ROWS: usize = 25;
const MAX_COLS: usize = 80;

/// Draws only what changed since the previous frame and keeps the host
/// cursor on the emulated one.
pub struct TermRenderer {
    /// Character and attribute of every cell as last drawn.
    drawn: [u16; MAX_COLS * ROWS],
    /// Column count the cells were drawn with.
    drawn_cols: usize,
    /// Column and row the host cursor was left at, if known.
    host_cursor: Option<(usize, usize)>,
    /// Whether the host cursor is shown, if known.
    cursor_shown: Option<bool>,
    /// Foreground and background colour index the terminal was left with.
    colours: Option<(u8, u8)>,
    /// Escape sequences and text of the frame being built.
    frame: String,
}

impl TermRenderer {
    pub fn new() -> Self {
        Self {
            drawn: [0xFFFF; MAX_COLS * ROWS],
            drawn_cols: 0,
            host_cursor: None,
            cursor_shown: None,
            colours: None,
            frame: String::new(),
        }
    }

    /// Brings the terminal up to date with one write.
    #[cfg(not(feature = "gui"))]
    pub fn render(&mut self, vga: &mut VgaDevice, memory: &Memory) {
        let frame = self.frame(vga, memory);
        if frame.is_empty() {
            return;
        }
        use std::io::Write;
        let mut stdout = std::io::stdout().lock();
        let _ = stdout.write_all(frame.as_bytes());
        let _ = stdout.flush();
    }

    /// Builds the output that turns what the terminal shows into the
    /// current screen: changed cells, then the cursor. A cursor move is
    /// emitted only where the output does not continue at the host cursor,
    /// and a colour only where it differs from the one in effect.
    fn frame(&mut self, vga: &mut VgaDevice, memory: &Memory) -> &str {
        self.frame.clear();
        if vga.video_mode != VideoMode::Text80x25 {
            return &self.frame;
        }

        let cols = vga.cols();
        let repaint = vga.take_full_redraw() || cols != self.drawn_cols;
        if repaint {
            // Something else may have written to the terminal.
            self.host_cursor = None;
            self.colours = None;
        }
        self.drawn_cols = cols;

        for index in 0..cols * ROWS {
            let addr = vga.text_cell_address(index);
            let ch = memory.read_byte(addr);
            let attr = memory.read_byte(addr + 1);
            let cell = (attr as u16) << 8 | ch as u16;
            if !repaint && self.drawn[index] == cell {
                continue;
            }
            self.drawn[index] = cell;

            let at = (index % cols, index / cols);
            self.move_to(at);
            self.set_colours(attr & 0x0F, background(vga.blink_mode, attr));
            self.frame.push(cp437_to_unicode(ch));
            // In the last column the terminal defers the wrap; the position
            // recorded here then matches no cell, which forces a move.
            self.host_cursor = Some((at.0 + 1, at.1));
        }

        match vga.visible_cursor_cell() {
            Some(cell) => {
                if self.cursor_shown != Some(true) {
                    let _ = Show.write_ansi(&mut self.frame);
                    self.cursor_shown = Some(true);
                }
                self.move_to((cell % cols, cell / cols));
            }
            None => {
                if self.cursor_shown != Some(false) {
                    let _ = Hide.write_ansi(&mut self.frame);
                    self.cursor_shown = Some(false);
                }
            }
        }

        &self.frame
    }

    fn move_to(&mut self, at: (usize, usize)) {
        if self.host_cursor != Some(at) {
            let _ = MoveTo(at.0 as u16, at.1 as u16).write_ansi(&mut self.frame);
            self.host_cursor = Some(at);
        }
    }

    fn set_colours(&mut self, fg: u8, bg: u8) {
        let (old_fg, old_bg) = match self.colours {
            Some((fg, bg)) => (Some(fg), Some(bg)),
            None => (None, None),
        };
        if old_fg != Some(fg) {
            let _ = SetForegroundColor(cga_to_color(fg)).write_ansi(&mut self.frame);
        }
        if old_bg != Some(bg) {
            let _ = SetBackgroundColor(cga_to_color(bg)).write_ansi(&mut self.frame);
        }
        self.colours = Some((fg, bg));
    }
}

/// Background colour index of an attribute byte. With blinking enabled,
/// bit 7 selects blink rather than a bright background.
fn background(blink_mode: bool, attr: u8) -> u8 {
    if blink_mode {
        (attr >> 4) & 0x07
    } else {
        attr >> 4
    }
}

/// Map CGA 4-bit color index to crossterm Color.
fn cga_to_color(index: u8) -> Color {
    match index & 0x0F {
        0x00 => Color::Rgb { r: 0, g: 0, b: 0 },
        0x01 => Color::Rgb { r: 0, g: 0, b: 170 },
        0x02 => Color::Rgb { r: 0, g: 170, b: 0 },
        0x03 => Color::Rgb { r: 0, g: 170, b: 170 },
        0x04 => Color::Rgb { r: 170, g: 0, b: 0 },
        0x05 => Color::Rgb { r: 170, g: 0, b: 170 },
        0x06 => Color::Rgb { r: 170, g: 85, b: 0 },
        0x07 => Color::Rgb { r: 170, g: 170, b: 170 },
        0x08 => Color::Rgb { r: 85, g: 85, b: 85 },
        0x09 => Color::Rgb { r: 85, g: 85, b: 255 },
        0x0A => Color::Rgb { r: 85, g: 255, b: 85 },
        0x0B => Color::Rgb { r: 85, g: 255, b: 255 },
        0x0C => Color::Rgb { r: 255, g: 85, b: 85 },
        0x0D => Color::Rgb { r: 255, g: 85, b: 255 },
        0x0E => Color::Rgb { r: 255, g: 255, b: 85 },
        _ => Color::Rgb { r: 255, g: 255, b: 255 },
    }
}

/// Map a CP437 byte to the Unicode character with the same glyph.
fn cp437_to_unicode(byte: u8) -> char {
    const TABLE: [char; 256] = {
        let mut t = [' '; 256];

        t[0x01] = '\u{263A}'; t[0x02] = '\u{263B}'; t[0x03] = '\u{2665}';
        t[0x04] = '\u{2666}'; t[0x05] = '\u{2663}'; t[0x06] = '\u{2660}';
        t[0x07] = '\u{2022}'; t[0x08] = '\u{25D8}'; t[0x09] = '\u{25CB}';
        t[0x0A] = '\u{25D9}'; t[0x0B] = '\u{2642}'; t[0x0C] = '\u{2640}';
        t[0x0D] = '\u{266A}'; t[0x0E] = '\u{266B}'; t[0x0F] = '\u{263C}';
        t[0x10] = '\u{25BA}'; t[0x11] = '\u{25C4}'; t[0x12] = '\u{2195}';
        t[0x13] = '\u{203C}'; t[0x14] = '\u{00B6}'; t[0x15] = '\u{00A7}';
        t[0x16] = '\u{25AC}'; t[0x17] = '\u{21A8}'; t[0x18] = '\u{2191}';
        t[0x19] = '\u{2193}'; t[0x1A] = '\u{2192}'; t[0x1B] = '\u{2190}';
        t[0x1C] = '\u{221F}'; t[0x1D] = '\u{2194}'; t[0x1E] = '\u{25B2}';
        t[0x1F] = '\u{25BC}';

        let mut i = 0x20u16;
        while i <= 0x7E {
            t[i as usize] = i as u8 as char;
            i += 1;
        }
        t[0x7F] = '\u{2302}';

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::bus::IoDevice;
    use crate::vm::runtime::Runtime;

    const GREY_ON_BLACK: &str = "\x1b[38;2;170;170;170m\x1b[48;2;0;0;0m";

    /// A blank screen (spaces, attribute 07h) with the cursor at row 5,
    /// column 3, already drawn once.
    fn drawn_screen() -> (Runtime, VgaDevice, TermRenderer) {
        let mut rt = Runtime::new_test();
        for cell in 0..MAX_COLS * ROWS {
            rt.cpu.memory.write_byte(0xB8000 + cell * 2, b' ');
            rt.cpu.memory.write_byte(0xB8000 + cell * 2 + 1, 0x07);
        }
        let mut vga = VgaDevice::new();
        set_cursor(&mut vga, &mut rt, 5, 3);
        let mut renderer = TermRenderer::new();
        renderer.frame(&mut vga, &rt.cpu.memory);
        (rt, vga, renderer)
    }

    fn write_crtc(vga: &mut VgaDevice, rt: &mut Runtime, index: u8, value: u8) {
        vga.port_out_byte(0x3D4, index, &mut rt.cpu);
        vga.port_out_byte(0x3D5, value, &mut rt.cpu);
    }

    fn set_cursor(vga: &mut VgaDevice, rt: &mut Runtime, row: u16, col: u16) {
        let location = row * 80 + col;
        write_crtc(vga, rt, 0x0E, (location >> 8) as u8);
        write_crtc(vga, rt, 0x0F, location as u8);
    }

    /// The frame without its escape sequences.
    fn text_of(frame: &str) -> String {
        let mut text = String::new();
        let mut in_escape = false;
        for ch in frame.chars() {
            match (in_escape, ch) {
                (false, '\x1b') => in_escape = true,
                (false, _) => text.push(ch),
                (true, _) => in_escape = !ch.is_ascii_alphabetic(),
            }
        }
        text
    }

    fn put(rt: &mut Runtime, row: usize, col: usize, ch: u8, attr: u8) {
        let addr = 0xB8000 + (row * 80 + col) * 2;
        rt.cpu.memory.write_byte(addr, ch);
        rt.cpu.memory.write_byte(addr + 1, attr);
    }

    #[test]
    fn first_frame_paints_every_cell_with_one_move_per_row_and_one_colour_pair() {
        let mut rt = Runtime::new_test();
        for cell in 0..MAX_COLS * ROWS {
            rt.cpu.memory.write_byte(0xB8000 + cell * 2, b'a' + (cell % 26) as u8);
            rt.cpu.memory.write_byte(0xB8000 + cell * 2 + 1, 0x07);
        }
        let mut vga = VgaDevice::new();
        set_cursor(&mut vga, &mut rt, 5, 3);
        let mut renderer = TermRenderer::new();
        let frame = renderer.frame(&mut vga, &rt.cpu.memory);

        assert!(frame.starts_with(&format!("\x1b[1;1H{GREY_ON_BLACK}abc")), "{:?}", &frame[..60]);
        assert_eq!(frame.matches("\x1b[38;2;").count(), 1, "foreground changes");
        assert_eq!(frame.matches("\x1b[48;2;").count(), 1, "background changes");
        // One move per row, and one to put the cursor back.
        assert_eq!(frame.matches('H').count(), ROWS + 1);
        assert_eq!(text_of(frame).len(), 2000);
        assert!(frame.ends_with("\x1b[?25h\x1b[6;4H"), "cursor shown at row 5, column 3");
        assert!(frame.len() < 2400, "{} bytes", frame.len());
    }

    #[test]
    fn unchanged_screen_produces_no_output() {
        let (rt, mut vga, mut renderer) = drawn_screen();
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "");
    }

    /// Regression: the host cursor was moved back only when the emulated
    /// cursor had moved, so after any other screen update it stayed behind
    /// the last cell drawn.
    #[test]
    fn host_cursor_returns_to_the_emulated_cursor_after_an_update() {
        let (mut rt, mut vga, mut renderer) = drawn_screen();
        put(&mut rt, 0, 70, b'X', 0x07);
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "\x1b[1;71HX\x1b[6;4H");
    }

    #[test]
    fn output_at_the_host_cursor_needs_no_cursor_move() {
        let (mut rt, mut vga, mut renderer) = drawn_screen();
        // A character is typed at the cursor, which advances by one.
        put(&mut rt, 5, 3, b'a', 0x07);
        set_cursor(&mut vga, &mut rt, 5, 4);
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "a");
    }

    #[test]
    fn colours_are_sent_only_when_they_change() {
        let (mut rt, mut vga, mut renderer) = drawn_screen();
        put(&mut rt, 0, 0, b'A', 0x1E); // yellow on blue
        put(&mut rt, 0, 1, b'B', 0x1E);
        put(&mut rt, 0, 2, b'C', 0x17); // grey on blue
        assert_eq!(
            renderer.frame(&mut vga, &rt.cpu.memory),
            "\x1b[1;1H\x1b[38;2;255;255;85m\x1b[48;2;0;0;170mAB\x1b[38;2;170;170;170mC\x1b[6;4H"
        );
    }

    /// Regression: a cursor the guest had switched off, or parked below the
    /// last row, was shown on row 24.
    #[test]
    fn host_cursor_is_hidden_while_the_emulated_one_is_not_visible() {
        let (mut rt, mut vga, mut renderer) = drawn_screen();

        write_crtc(&mut vga, &mut rt, 0x0A, 0x20); // cursor disable bit
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "\x1b[?25l");
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "");

        write_crtc(&mut vga, &mut rt, 0x0A, 0x06);
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "\x1b[?25h");

        set_cursor(&mut vga, &mut rt, 25, 0); // below the last row
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "\x1b[?25l");

        set_cursor(&mut vga, &mut rt, 2, 0);
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "\x1b[?25h\x1b[3;1H");
    }

    /// Regression: after a queued full redraw the host cursor was left
    /// behind the bottom right cell.
    #[test]
    fn queued_full_redraw_repaints_and_restores_the_cursor() {
        let (rt, mut vga, mut renderer) = drawn_screen();
        vga.queue_full_redraw();
        let frame = renderer.frame(&mut vga, &rt.cpu.memory);
        assert!(frame.starts_with(&format!("\x1b[1;1H{GREY_ON_BLACK} ")));
        assert_eq!(text_of(frame).len(), 2000);
        assert!(frame.ends_with("\x1b[6;4H"));
    }

    #[test]
    fn graphics_mode_leaves_the_terminal_alone_and_text_mode_repaints() {
        let (mut rt, mut vga, mut renderer) = drawn_screen();

        vga.port_out_byte(0x3CE, 0x05, &mut rt.cpu);
        vga.port_out_byte(0x3CF, 0x40, &mut rt.cpu); // 256-colour mode
        assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "");

        vga.port_out_byte(0x3CF, 0x00, &mut rt.cpu);
        let frame = renderer.frame(&mut vga, &rt.cpu.memory);
        assert_eq!(text_of(frame).len(), 2000);
    }

    #[test]
    fn cp437_bytes_map_to_their_unicode_glyphs() {
        assert_eq!(cp437_to_unicode(0x00), ' ');
        assert_eq!(cp437_to_unicode(b'A'), 'A');
        assert_eq!(cp437_to_unicode(0xC9), '\u{2554}'); // double box corner
        assert_eq!(cp437_to_unicode(0xDB), '\u{2588}'); // full block
        assert_eq!(cp437_to_unicode(0x1A), '\u{2192}'); // right arrow
    }
}
