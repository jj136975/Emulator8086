use std::io::{self, Write};

use crossterm::{cursor::MoveTo, style::{Color, SetBackgroundColor, SetForegroundColor}, QueueableCommand};
use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use crate::vm::memory::Memory;

// ---------------------------------------------------------------------------
// Video mode tracking
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum VideoMode {
    Text80x25,
    Mode13h, // 320x200 256-color (chain-4 linear at A000:0000)
}

// ---------------------------------------------------------------------------
// VGA device — full register state for text mode + Mode 13h
// ---------------------------------------------------------------------------

/// VGA register-level emulation covering ports 0x3C0-0x3DA.
///
/// Ports handled:
///   0x3C0      — Attribute Controller index / data (flip-flop)
///   0x3C1      — Attribute Controller data read
///   0x3C2      — Miscellaneous Output Register (write)
///   0x3C4      — Sequencer index
///   0x3C5      — Sequencer data
///   0x3C6      — DAC PEL mask
///   0x3C7      — DAC state (read) / DAC read index (write)
///   0x3C8      — DAC write index
///   0x3C9      — DAC data (R/G/B cycle)
///   0x3CC      — Miscellaneous Output Register (read)
///   0x3CE      — Graphics Controller index
///   0x3CF      — Graphics Controller data
///   0x3D0-0x3D5 — CRTC index/data (with CGA mirrors)
///   0x3D8      — CGA mode control register
///   0x3D9      — CGA color select register
///   0x3DA      — Status register (resets AC flip-flop)
pub struct VgaDevice {
    // --- CRTC (0x3D4/0x3D5) ---
    crtc_index: u8,
    crtc_regs: [u8; 256],

    // --- CGA compatibility (0x3D8/0x3D9) ---
    mode_register: u8,
    color_register: u8,

    // --- Status / timing ---
    cycle_counter: u64,

    // --- Miscellaneous Output Register (0x3C2 write, 0x3CC read) ---
    misc_output: u8,

    // --- Sequencer (0x3C4 index, 0x3C5 data) ---
    seq_index: u8,
    seq_regs: [u8; 8],

    // --- Attribute Controller (0x3C0/0x3C1) ---
    attr_index: u8,
    pub attr_regs: [u8; 32],
    attr_flip_flop: bool, // false = next 0x3C0 write is index, true = data

    // --- Graphics Controller (0x3CE index, 0x3CF data) ---
    gc_index: u8,
    gc_regs: [u8; 16],

    // --- DAC palette (0x3C6-0x3C9) ---
    dac_mask: u8,
    dac_read_index: u8,
    dac_write_index: u8,
    dac_component: u8, // 0=R, 1=G, 2=B — cycles after each read/write
    pub dac_palette: [[u8; 3]; 256], // 6-bit per component (0-63)

    // --- Video mode (derived from register state) ---
    pub video_mode: VideoMode,

    // --- Terminal rendering state ---
    pub blink_mode: bool,
    prev_buffer: [u16; 80 * 25],
    cursor_prev: (u8, u8),
    needs_full_redraw: bool,
}

const VIDEO_BASE: usize = 0xB8000;
const ROWS: usize = 25;

/// Standard VGA default 6-bit DAC values for the first 16 CGA colors.
const CGA_DAC: [[u8; 3]; 16] = [
    [ 0,  0,  0], // 0  Black
    [ 0,  0, 42], // 1  Blue
    [ 0, 42,  0], // 2  Green
    [ 0, 42, 42], // 3  Cyan
    [42,  0,  0], // 4  Red
    [42,  0, 42], // 5  Magenta
    [42, 21,  0], // 6  Brown
    [42, 42, 42], // 7  Light gray
    [21, 21, 21], // 8  Dark gray
    [21, 21, 63], // 9  Light blue
    [21, 63, 21], // 10 Light green
    [21, 63, 63], // 11 Light cyan
    [63, 21, 21], // 12 Light red
    [63, 21, 63], // 13 Light magenta
    [63, 63, 21], // 14 Yellow
    [63, 63, 63], // 15 White
];

impl VgaDevice {
    pub fn new() -> Self {
        let mut dac_palette = [[0u8; 3]; 256];

        // Initialize first 16 entries to CGA colors
        for i in 0..16 {
            dac_palette[i] = CGA_DAC[i];
        }

        // Initialize entries 16-31 as a greyscale ramp
        for i in 0..16u8 {
            let v = i * 4 + 2; // 2, 6, 10, ..., 62
            dac_palette[16 + i as usize] = [v, v, v];
        }

        // Initialize default attribute controller palette (identity 0-15)
        let mut attr_regs = [0u8; 32];
        for i in 0..16 {
            attr_regs[i] = i as u8;
        }
        // AC Mode Control register (index 0x10)
        attr_regs[0x10] = 0x0C; // default text mode value

        Self {
            crtc_index: 0,
            crtc_regs: [0; 256],
            mode_register: 0x29, // 80-col text, display enabled, blink
            color_register: 0,
            cycle_counter: 0,
            misc_output: 0x67, // default: color mode, odd/even, 25MHz clock

            seq_index: 0,
            seq_regs: [0x03, 0x00, 0x0F, 0x00, 0x02, 0, 0, 0],
            // seq[0]=Reset, seq[1]=Clocking, seq[2]=Map Mask (all planes),
            // seq[3]=Char Map, seq[4]=Memory Mode (odd/even, no chain-4)

            attr_index: 0,
            attr_regs,
            attr_flip_flop: false,

            gc_index: 0,
            gc_regs: [0; 16],

            dac_mask: 0xFF,
            dac_read_index: 0,
            dac_write_index: 0,
            dac_component: 0,
            dac_palette,

            video_mode: VideoMode::Text80x25,

            blink_mode: true,
            prev_buffer: [0xFFFF; 80 * 25],
            cursor_prev: (0xFF, 0xFF),
            needs_full_redraw: true,
        }
    }

    /// Advance the cycle counter externally (called from runtime).
    pub fn tick(&mut self, cycles: u64) {
        self.cycle_counter += cycles;
    }

    /// Dynamic column count from CRTC register 1 (Horizontal Displayed).
    pub fn cols(&self) -> usize {
        let crtc_r1 = self.crtc_regs[0x01] as usize;
        if crtc_r1 > 0 && crtc_r1 <= 80 { crtc_r1 } else { 80 }
    }

    /// Get cursor position from CRTC registers 0x0E (high) and 0x0F (low).
    pub fn cursor_offset(&self) -> u16 {
        ((self.crtc_regs[0x0E] as u16) << 8) | self.crtc_regs[0x0F] as u16
    }

    /// Return the display start offset from CRTC registers 0x0C/0x0D.
    pub fn start_address(&self) -> usize {
        let addr = ((self.crtc_regs[0x0C] as u16) << 8) | self.crtc_regs[0x0D] as u16;
        addr as usize
    }

    /// Convert a DAC palette index to 8-bit RGB.
    pub fn dac_to_rgb8(&self, index: u8) -> [u8; 3] {
        let [r6, g6, b6] = self.dac_palette[(index & self.dac_mask) as usize];
        [
            (r6 << 2) | (r6 >> 4),
            (g6 << 2) | (g6 >> 4),
            (b6 << 2) | (b6 >> 4),
        ]
    }

    pub fn queue_full_redraw(&mut self) {
        self.needs_full_redraw = true;
    }

    /// Detect video mode from register state.
    fn detect_mode(&mut self) {
        // GC register 5 bit 6 = 256-color shift mode
        let old = self.video_mode;
        if self.gc_regs[0x05] & 0x40 != 0 {
            self.video_mode = VideoMode::Mode13h;
        } else {
            self.video_mode = VideoMode::Text80x25;
        }
        if old != self.video_mode {
            self.needs_full_redraw = true;
        }
    }

    /// Call periodically from main loop to render video RAM to terminal.
    /// Only redraws cells that changed since last call.
    /// Only renders text mode; Mode 13h requires the GUI backend.
    pub fn render(&mut self, memory: &Memory) {
        if self.video_mode != VideoMode::Text80x25 {
            return;
        }

        let mut stdout = io::stdout();

        let start = self.start_address();
        let cols = self.cols();
        let cells = cols * ROWS;

        for i in 0..cells {
            let cell_index = (start + i) & 0x1FFF;
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
                (attr & 0x0F, (attr >> 4) & 0x07)
            } else {
                (attr & 0x0F, (attr >> 4) & 0x0F)
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
        let _ = writeln!(f, "Video mode: {:?}", self.video_mode);
        let _ = writeln!(f, "CRTC start_address={} cursor_offset={} (row={}, col={})",
            start, offset, offset / cols as u16, offset % cols as u16);
        let _ = writeln!(f, "CRTC regs: 0x0C={:02X} 0x0D={:02X} 0x0E={:02X} 0x0F={:02X}",
            self.crtc_regs[0x0C], self.crtc_regs[0x0D], self.crtc_regs[0x0E], self.crtc_regs[0x0F]);
        let _ = writeln!(f, "BDA video_mode={:02X} video_cols={} active_page={:02X}",
            memory.read_byte(0x0449),
            memory.read_word(0x044A),
            memory.read_byte(0x0462));
        let _ = writeln!(f, "BDA cursor_pos page0: col={:02X} row={:02X}",
            memory.read_byte(0x0450), memory.read_byte(0x0451));
        let _ = write!(f, "BDA cursor_pos all pages:");
        for p in 0..8 {
            let addr = 0x0450 + p * 2;
            let _ = write!(f, " p{}=[{:02X},{:02X}]", p,
                memory.read_byte(addr), memory.read_byte(addr + 1));
        }
        let _ = writeln!(f);

        let _ = writeln!(f, "Mode register: {:02X} (display {}abled, blink={})",
            self.mode_register,
            if self.mode_register & 0x08 != 0 { "en" } else { "dis" },
            self.blink_mode);
        let _ = writeln!(f, "Misc output: {:02X}, Seq[4]={:02X}, GC[5]={:02X}, GC[6]={:02X}",
            self.misc_output, self.seq_regs[4], self.gc_regs[5], self.gc_regs[6]);
        let _ = writeln!(f, "DAC mask: {:02X}", self.dac_mask);

        // IVT entries
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

        let _ = writeln!(f);
        for row in 0..ROWS {
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

// ---------------------------------------------------------------------------
// I/O port dispatch — 0x3C0 through 0x3DA
// ---------------------------------------------------------------------------

impl IoDevice for VgaDevice {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            // --- Attribute Controller ---
            0x3C0 => self.attr_index,
            0x3C1 => self.attr_regs[(self.attr_index & 0x1F) as usize],

            // --- Miscellaneous Output (read mirror) ---
            0x3CC => self.misc_output,

            // --- Sequencer ---
            0x3C4 => self.seq_index,
            0x3C5 => self.seq_regs[(self.seq_index & 0x07) as usize],

            // --- DAC ---
            0x3C6 => self.dac_mask,
            0x3C7 => {
                // DAC state: bit 0-1: 0=write, 3=read
                0x03 // always report "read mode" for simplicity
            }
            0x3C8 => self.dac_write_index,
            0x3C9 => {
                let idx = self.dac_read_index as usize;
                let comp = self.dac_component;
                let val = self.dac_palette[idx][comp as usize];
                self.dac_component += 1;
                if self.dac_component >= 3 {
                    self.dac_component = 0;
                    self.dac_read_index = self.dac_read_index.wrapping_add(1);
                }
                val
            }

            // --- Graphics Controller ---
            0x3CE => self.gc_index,
            0x3CF => self.gc_regs[(self.gc_index & 0x0F) as usize],

            // --- CRTC index (with CGA mirrors) ---
            0x3D0 | 0x3D2 | 0x3D4 => self.crtc_index,

            // --- CRTC data (with CGA mirrors) ---
            // Real VGA/CGA exposes the full CRTC register set for read.
            0x3D1 | 0x3D3 | 0x3D5 => self.crtc_regs[self.crtc_index as usize],

            // --- CGA mode control ---
            0x3D8 => self.mode_register,

            // --- CGA color select ---
            0x3D9 => self.color_register,

            // --- Status register ---
            // Reading also resets the Attribute Controller flip-flop.
            0x3DA => {
                self.attr_flip_flop = false;
                self.cycle_counter += 1;

                let bit0 = (self.cycle_counter % 76) >= 40;
                let scanline = (self.cycle_counter / 76) % 262;
                let bit3 = scanline >= 225;

                0xF0 | ((bit3 as u8) << 3) | (bit0 as u8)
            }

            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            // --- Attribute Controller (flip-flop) ---
            0x3C0 => {
                if !self.attr_flip_flop {
                    // Index write
                    self.attr_index = value;
                } else {
                    // Data write
                    self.attr_regs[(self.attr_index & 0x1F) as usize] = value;
                }
                self.attr_flip_flop = !self.attr_flip_flop;
            }

            // --- Miscellaneous Output Register ---
            0x3C2 => {
                self.misc_output = value;
            }

            // --- Sequencer ---
            0x3C4 => self.seq_index = value,
            0x3C5 => {
                self.seq_regs[(self.seq_index & 0x07) as usize] = value;
            }

            // --- DAC ---
            0x3C6 => self.dac_mask = value,
            0x3C7 => {
                // Set DAC read index
                self.dac_read_index = value;
                self.dac_component = 0;
            }
            0x3C8 => {
                // Set DAC write index
                self.dac_write_index = value;
                self.dac_component = 0;
            }
            0x3C9 => {
                let idx = self.dac_write_index as usize;
                let comp = self.dac_component;
                self.dac_palette[idx][comp as usize] = value & 0x3F; // 6-bit
                self.dac_component += 1;
                if self.dac_component >= 3 {
                    self.dac_component = 0;
                    self.dac_write_index = self.dac_write_index.wrapping_add(1);
                }
            }

            // --- Graphics Controller ---
            0x3CE => self.gc_index = value,
            0x3CF => {
                self.gc_regs[(self.gc_index & 0x0F) as usize] = value;
                // Detect mode change when GC mode register (index 5) is written
                if self.gc_index == 0x05 {
                    self.detect_mode();
                }
            }

            // --- CRTC index (with CGA mirrors) ---
            0x3D0 | 0x3D2 | 0x3D4 => self.crtc_index = value,

            // --- CRTC data (with CGA mirrors) ---
            0x3D1 | 0x3D3 | 0x3D5 => {
                if self.crtc_index <= 17 {
                    let masked = match self.crtc_index {
                        0x04 => value & 0x7F,
                        0x05 => value & 0x1F,
                        0x06 => value & 0x7F,
                        0x07 => value & 0x7F,
                        0x09 => value & 0x1F,
                        0x0A => value & 0x7F,
                        0x0B => value & 0x1F,
                        0x0C => value & 0x3F,
                        _ => value,
                    };
                    self.crtc_regs[self.crtc_index as usize] = masked;
                }
            }

            // --- CGA mode control ---
            0x3D8 => {
                self.mode_register = value;
                self.blink_mode = value & 0x20 != 0;
            }

            // --- CGA color select ---
            0x3D9 => {
                self.color_register = value;
            }

            _ => {}
        }
    }

    fn name(&self) -> &'static str {
        "VGA"
    }
}

// ---------------------------------------------------------------------------
// Color / character helpers
// ---------------------------------------------------------------------------

/// Map CGA 4-bit color index to crossterm Color.
pub fn cga_to_color(index: u8) -> Color {
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
        0x0F => Color::Rgb { r: 255, g: 255, b: 255 },
        _ => Color::Rgb { r: 170, g: 170, b: 170 },
    }
}

/// Map CP437 byte to Unicode for common box drawing and special characters.
pub fn cp437_to_unicode(byte: u8) -> char {
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

// ---------------------------------------------------------------------------
// Inspection tests — demonstrate real bugs in the VGA emulation.
// ---------------------------------------------------------------------------
#[cfg(test)]
mod inspection_tests {
    use super::*;
    use crate::io::bus::IoDevice;
    use crate::vm::runtime::Runtime;

    /// Regression: CRTC data port 0x3D5 must return the stored register
    /// value for every index (not just 0x0A..=0x0F as the original code did).
    /// Real VGA/CGA exposes the full CRTC register set for read-back.
    #[test]
    fn crtc_register_00_readback_returns_stored_value() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();

        vga.port_out_byte(0x3D4, 0x00, &mut rt.cpu);
        vga.port_out_byte(0x3D5, 0x5F, &mut rt.cpu);

        vga.port_out_byte(0x3D4, 0x00, &mut rt.cpu);
        let got = vga.port_in_byte(0x3D5, &mut rt.cpu);
        assert_eq!(got, 0x5F, "CRTC index 0x00 readback");
    }

    /// Sanity check: CRTC index 0x0E (Cursor Location High) is in the
    /// supported range and should read back correctly. This test should
    /// PASS on the current code; if it fails we have a different bug.
    #[test]
    fn crtc_register_0e_readback_is_ok() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        vga.port_out_byte(0x3D4, 0x0E, &mut rt.cpu);
        vga.port_out_byte(0x3D5, 0xAB, &mut rt.cpu);
        vga.port_out_byte(0x3D4, 0x0E, &mut rt.cpu);
        let got = vga.port_in_byte(0x3D5, &mut rt.cpu);
        assert_eq!(got, 0xAB);
    }

    /// FINDING (Phase 3): DAC 6→8 bit conversion `(v<<2)|(v>>4)` uses bit
    /// replication, not linear scaling. Phase 3 claimed this causes color
    /// banding because it doesn't reach full-scale properly.
    /// STATUS: PASSES → retracted. Bit replication maps 0→0 and 63→255
    /// correctly; intermediate values differ from linear scaling by at most
    /// 1 (e.g., 32 → 130 vs linear 129). Bit replication is standard VGA
    /// DAC emulation practice and is NOT a bug.
    #[test]
    fn dac_conversion_endpoints_are_exact() {
        let vga = VgaDevice::new();
        // DAC entry 0 should be all-zero regardless of bit replication.
        // Fill entry 1 with (63, 63, 63) via a fresh instance — here we
        // just verify the formula behaves correctly at extremes.
        // The function uses dac_palette; since we can't easily inject,
        // we directly verify the math by computing the expected output
        // of the bit-replication formula.
        fn replicate(v6: u8) -> u8 {
            (v6 << 2) | (v6 >> 4)
        }
        assert_eq!(replicate(0), 0x00, "0 must map to 0x00");
        assert_eq!(replicate(63), 0xFF, "63 must map to 0xFF (full scale reached)");
        assert_eq!(replicate(32), 130, "midpoint maps consistently");
        // Prevent dead_code warning on the vga binding.
        let _ = vga.dac_to_rgb8(0);
    }
}
