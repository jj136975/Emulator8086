use std::cell::Cell;
use std::io::Write;
use std::rc::Rc;

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
    /// Clocks of the 1,193,182 Hz timer input the machine has run for.
    /// Shared with the runtime, which advances it as it clocks the 8253.
    timer_clock: Rc<Cell<u64>>,

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
    /// What port 0x3C7 reads: 3 after the read index was set, 0 after the
    /// write index was set.
    dac_state: u8,
    pub dac_palette: [[u8; 3]; 256], // 6-bit per component (0-63)

    // --- Video mode (derived from register state) ---
    pub video_mode: VideoMode,

    /// Attribute bit 7 means "blink" (true) or "bright background" (false).
    pub blink_mode: bool,
    /// A renderer that only draws changes must repaint the whole screen.
    needs_full_redraw: bool,
}

const VIDEO_BASE: usize = 0xB8000;
const ROWS: usize = 25;
/// Text cell indexes wrap at this mask when the display start is added:
/// the 32 KB text buffer at B8000h holds 16,384 cells (eight 80x25 pages).
const TEXT_CELL_MASK: usize = 0x3FFF;

/// Timer clocks per displayed frame: 70 frames per second.
const FRAME_CLOCKS: u64 = 17_045;
/// Vertical retrace is reported for the last 8 % of each frame. While the
/// timer catches up after a pause it gets 4 clocks per instruction, so the
/// window still spans a few hundred instructions of a polling loop.
const VRETRACE_CLOCKS: u64 = 1_364;
/// One scanline with its horizontal blank. Coarser than the real 31.5 kHz
/// line, for the same reason: a loop that polls every few instructions
/// must not be able to step over the blank.
const LINE_CLOCKS: u64 = 76;
const HBLANK_CLOCKS: u64 = 19;

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
        dac_palette[..16].copy_from_slice(&CGA_DAC);

        // Initialize entries 16-31 as a greyscale ramp
        for i in 0..16u8 {
            let v = i * 4 + 2; // 2, 6, 10, ..., 62
            dac_palette[16 + i as usize] = [v, v, v];
        }

        // Initialize default attribute controller palette (identity 0-15)
        let mut attr_regs = [0u8; 32];
        for (i, reg) in attr_regs.iter_mut().enumerate().take(16) {
            *reg = i as u8;
        }
        // AC Mode Control register (index 0x10)
        attr_regs[0x10] = 0x0C; // default text mode value

        // Cursor shape the BIOS programs: scanlines 6-7 of an 8-line cell.
        let mut crtc_regs = [0u8; 256];
        crtc_regs[0x0A] = 0x06;
        crtc_regs[0x0B] = 0x07;

        Self {
            crtc_index: 0,
            crtc_regs,
            mode_register: 0x29, // 80-col text, display enabled, blink
            color_register: 0,
            timer_clock: Rc::new(Cell::new(0)),
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
            dac_state: 0,
            dac_palette,

            video_mode: VideoMode::Text80x25,

            blink_mode: true,
            needs_full_redraw: true,
        }
    }

    /// The time base of the retrace bits in Input Status 1. The runtime
    /// adds every clock it hands to the 8253, so display time advances
    /// only while the guest runs and catches up after a pause exactly as
    /// the timer does.
    pub fn timer_clock(&self) -> Rc<Cell<u64>> {
        Rc::clone(&self.timer_clock)
    }

    /// Input Status 1: bit 3 during vertical retrace, bit 0 whenever the
    /// display is off (horizontal blank or vertical retrace).
    fn input_status_1(&self) -> u8 {
        let in_frame = self.timer_clock.get() % FRAME_CLOCKS;
        let vertical_retrace = in_frame >= FRAME_CLOCKS - VRETRACE_CLOCKS;
        let display_off =
            vertical_retrace || in_frame % LINE_CLOCKS >= LINE_CLOCKS - HBLANK_CLOCKS;
        0xF0 | (vertical_retrace as u8) << 3 | display_off as u8
    }

    /// Column count from CRTC register 1. VGA software writes the count
    /// minus one (4Fh, 27h), 6845 software the count itself; both are
    /// accepted. Anything else, including the power-on zero, means 80.
    pub fn cols(&self) -> usize {
        match self.crtc_regs[0x01] as usize {
            79 => 80,
            39 => 40,
            count @ 1..=80 => count,
            _ => 80,
        }
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

    /// Cursor start and end scanline from CRTC registers 0x0A/0x0B, or
    /// `None` while the cursor is switched off (register 0x0A bit 5).
    pub fn cursor_shape(&self) -> Option<(u8, u8)> {
        let start = self.crtc_regs[0x0A];
        if start & 0x20 != 0 {
            return None;
        }
        Some((start & 0x1F, self.crtc_regs[0x0B] & 0x1F))
    }

    /// Index of the displayed text cell the cursor is in, counted from the
    /// top left, or `None` when the cursor is switched off or lies outside
    /// the displayed page.
    pub fn visible_cursor_cell(&self) -> Option<usize> {
        self.cursor_shape()?;
        let cell = (self.cursor_offset() as usize).wrapping_sub(self.start_address())
            & TEXT_CELL_MASK;
        (cell < self.cols() * ROWS).then_some(cell)
    }

    /// Memory address of the `index`-th displayed text cell: its character
    /// byte, followed by its attribute byte.
    pub fn text_cell_address(&self, index: usize) -> usize {
        VIDEO_BASE + ((self.start_address() + index) & TEXT_CELL_MASK) * 2
    }

    /// Convert a DAC palette index to 8-bit RGB.
    #[cfg(any(feature = "gui", test))]
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

    /// Switches attribute bit 7 between "blink" and "bright background".
    /// Cells with that bit set change colour, so a change needs a repaint.
    fn set_blink_mode(&mut self, blink: bool) {
        if blink != self.blink_mode {
            self.blink_mode = blink;
            self.needs_full_redraw = true;
        }
    }

    /// Returns whether a full repaint was requested since the last call.
    #[cfg(any(not(feature = "gui"), test))]
    pub fn take_full_redraw(&mut self) -> bool {
        std::mem::take(&mut self.needs_full_redraw)
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
                let ch = memory.read_byte(self.text_cell_address(row * cols + col));
                if ch >= 0x20 && ch < 0x7F {
                    let _ = write!(f, "{}", ch as char);
                } else {
                    let _ = write!(f, ".");
                }
            }
            let _ = writeln!(f, "|");
            let _ = write!(f, "        ");
            for col in 0..cols {
                let addr = self.text_cell_address(row * cols + col);
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
            0x3C7 => self.dac_state,
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
                self.input_status_1()
            }

            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            // --- Attribute Controller (flip-flop) ---
            0x3C0 => {
                if !self.attr_flip_flop {
                    // Index write: five index bits and the palette address
                    // source bit.
                    self.attr_index = value & 0x3F;
                } else {
                    // Data write
                    let index = (self.attr_index & 0x1F) as usize;
                    self.attr_regs[index] = value;
                    if index == 0x10 {
                        // Mode control bit 3: attribute bit 7 blinks the
                        // character (1) or brightens the background (0).
                        self.set_blink_mode(value & 0x08 != 0);
                    }
                }
                self.attr_flip_flop = !self.attr_flip_flop;
            }

            // --- Miscellaneous Output Register ---
            0x3C2 => {
                self.misc_output = value;
            }

            // --- Sequencer ---
            0x3C4 => self.seq_index = value & 0x07,
            0x3C5 => {
                self.seq_regs[(self.seq_index & 0x07) as usize] = value;
            }

            // --- DAC ---
            0x3C6 => self.dac_mask = value,
            0x3C7 => {
                // Set DAC read index
                self.dac_read_index = value;
                self.dac_component = 0;
                self.dac_state = 0x03;
            }
            0x3C8 => {
                // Set DAC write index
                self.dac_write_index = value;
                self.dac_component = 0;
                self.dac_state = 0x00;
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
            0x3CE => self.gc_index = value & 0x0F,
            0x3CF => {
                self.gc_regs[(self.gc_index & 0x0F) as usize] = value;
                // Detect mode change when GC mode register (index 5) is written
                if self.gc_index == 0x05 {
                    self.detect_mode();
                }
            }

            // --- CRTC index (with CGA mirrors) ---
            0x3D0 | 0x3D2 | 0x3D4 => self.crtc_index = value & 0x3F,

            // --- CRTC data (with CGA mirrors) ---
            // The VGA has CRTC registers 00h-18h, all eight bits wide.
            0x3D1 | 0x3D3 | 0x3D5 => {
                if self.crtc_index <= 0x18 {
                    self.crtc_regs[self.crtc_index as usize] = value;
                }
            }

            // --- CGA mode control ---
            0x3D8 => {
                self.mode_register = value;
                self.set_blink_mode(value & 0x20 != 0);
            }

            // --- CGA color select ---
            0x3D9 => {
                self.color_register = value;
            }

            _ => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Regression tests — VGA register and DAC behaviour.
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

    /// CRTC index 0x0E (Cursor Location High) reads back the stored value.
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

    /// Regression: the retrace bits were driven by a count of status reads,
    /// so their timing depended on how often the guest looked. They follow
    /// the timer clock: 70 frames per second, vertical retrace at the end
    /// of each frame.
    #[test]
    fn input_status_1_follows_the_timer_clock_not_the_reads() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        let clock = vga.timer_clock();
        let mut status = |at: u64| {
            clock.set(at);
            vga.port_in_byte(0x3DA, &mut rt.cpu)
        };

        // Reading does not move time.
        for _ in 0..100_000 {
            assert_eq!(status(0) & 0x08, 0);
        }

        let retrace_start = FRAME_CLOCKS - VRETRACE_CLOCKS;
        assert_eq!(status(retrace_start - 1) & 0x08, 0);
        assert_eq!(status(retrace_start) & 0x08, 0x08);
        assert_eq!(status(FRAME_CLOCKS - 1) & 0x08, 0x08);
        assert_eq!(status(FRAME_CLOCKS) & 0x08, 0, "next frame");
        assert_eq!(status(70 * FRAME_CLOCKS + retrace_start) & 0x08, 0x08);
    }

    /// Regression: bit 0 kept toggling during vertical retrace. It reports
    /// "display off", which covers the horizontal blank and the retrace.
    #[test]
    fn display_off_bit_covers_horizontal_blank_and_vertical_retrace() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        let clock = vga.timer_clock();
        let mut blank_clocks = 0;
        for at in 0..FRAME_CLOCKS {
            clock.set(at);
            let status = vga.port_in_byte(0x3DA, &mut rt.cpu);
            if status & 0x08 != 0 {
                assert_eq!(status & 0x01, 0x01, "display off during retrace, clock {at}");
            } else if status & 0x01 != 0 {
                blank_clocks += 1;
            }
        }
        // Outside the retrace, every line ends with a blank.
        let lines = (FRAME_CLOCKS - VRETRACE_CLOCKS) / LINE_CLOCKS;
        assert_eq!(blank_clocks, lines * HBLANK_CLOCKS);

        clock.set(0);
        assert_eq!(vga.port_in_byte(0x3DA, &mut rt.cpu) & 0x01, 0, "start of a line");
        clock.set(LINE_CLOCKS - 1);
        assert_eq!(vga.port_in_byte(0x3DA, &mut rt.cpu) & 0x01, 0x01, "end of a line");
    }

    fn write_crtc(vga: &mut VgaDevice, rt: &mut Runtime, index: u8, value: u8) {
        vga.port_out_byte(0x3D4, index, &mut rt.cpu);
        vga.port_out_byte(0x3D5, value, &mut rt.cpu);
    }

    fn read_crtc(vga: &mut VgaDevice, rt: &mut Runtime, index: u8) -> u8 {
        vga.port_out_byte(0x3D4, index, &mut rt.cpu);
        vga.port_in_byte(0x3D5, &mut rt.cpu)
    }

    /// Regression: displayed cells wrapped at 16 KB, the CGA limit, so
    /// display pages 4-7 of the 32 KB text buffer showed pages 0-3.
    #[test]
    fn display_pages_4_to_7_are_addressable() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        // Page 4 starts 4 * 2048 cells into the buffer.
        write_crtc(&mut vga, &mut rt, 0x0C, 0x20);
        write_crtc(&mut vga, &mut rt, 0x0D, 0x00);
        assert_eq!(vga.text_cell_address(0), 0xBC000);
        assert_eq!(vga.text_cell_address(1999), 0xBC000 + 1999 * 2);

        // Cursor at row 1, column 2 of that page.
        write_crtc(&mut vga, &mut rt, 0x0E, 0x20);
        write_crtc(&mut vga, &mut rt, 0x0F, 82);
        assert_eq!(vga.visible_cursor_cell(), Some(82));

        // Page 7 ends at the end of the buffer; the next cell wraps.
        write_crtc(&mut vga, &mut rt, 0x0C, 0x38);
        assert_eq!(vga.text_cell_address(2047), 0xBFFFE);
        assert_eq!(vga.text_cell_address(2048), 0xB8000);
    }

    /// Regression: CRTC register 01h was taken as a column count. The VGA
    /// value is the count minus one, so the standard 4Fh gave 79 columns.
    #[test]
    fn column_count_accepts_vga_and_6845_values() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        assert_eq!(vga.cols(), 80, "power-on");
        for (value, cols) in [(0x4F, 80), (0x27, 40), (80, 80), (40, 40), (0xFF, 80)] {
            write_crtc(&mut vga, &mut rt, 0x01, value);
            assert_eq!(vga.cols(), cols, "CRTC 01h = {value:#04X}");
        }
    }

    /// Regression: CRTC registers 12h-18h ignored writes, and several
    /// others were cut to their 6845 widths.
    #[test]
    fn crtc_registers_up_to_18h_read_back_what_was_written() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        for index in 0x00..=0x18 {
            write_crtc(&mut vga, &mut rt, index, 0xA5 ^ index);
        }
        for index in 0x00..=0x18 {
            assert_eq!(read_crtc(&mut vga, &mut rt, index), 0xA5 ^ index, "index {index:#04X}");
        }
        // There is no register 19h.
        write_crtc(&mut vga, &mut rt, 0x19, 0x77);
        assert_eq!(read_crtc(&mut vga, &mut rt, 0x19), 0x00);
    }

    /// Regression: index registers kept all eight bits, and a graphics
    /// controller write through an index with high bits set changed the
    /// mode register without the video mode following.
    #[test]
    fn index_registers_keep_only_their_implemented_bits() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        for port in [0x3C4, 0x3CE, 0x3D4] {
            vga.port_out_byte(port, 0xFF, &mut rt.cpu);
        }
        assert_eq!(vga.port_in_byte(0x3C4, &mut rt.cpu), 0x07, "sequencer");
        assert_eq!(vga.port_in_byte(0x3CE, &mut rt.cpu), 0x0F, "graphics controller");
        assert_eq!(vga.port_in_byte(0x3D4, &mut rt.cpu), 0x3F, "CRTC");

        vga.port_in_byte(0x3DA, &mut rt.cpu); // attribute flip-flop to "index"
        vga.port_out_byte(0x3C0, 0xFF, &mut rt.cpu);
        assert_eq!(vga.port_in_byte(0x3C0, &mut rt.cpu), 0x3F, "attribute controller");

        vga.port_out_byte(0x3CE, 0x15, &mut rt.cpu); // register 5
        vga.port_out_byte(0x3CF, 0x40, &mut rt.cpu);
        assert_eq!(vga.video_mode, VideoMode::Mode13h);
    }

    /// Regression: only the CGA mode port 3D8h switched between blinking
    /// and bright backgrounds. On the VGA it is bit 3 of attribute
    /// controller register 10h.
    #[test]
    fn attribute_mode_bit_3_selects_blink_or_bright_background() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        assert!(vga.blink_mode, "power-on");
        assert!(vga.take_full_redraw());

        let mut write_mode = |vga: &mut VgaDevice, value: u8| {
            vga.port_in_byte(0x3DA, &mut rt.cpu);
            vga.port_out_byte(0x3C0, 0x30, &mut rt.cpu);
            vga.port_out_byte(0x3C0, value, &mut rt.cpu);
        };

        write_mode(&mut vga, 0x04);
        assert!(!vga.blink_mode);
        assert!(vga.take_full_redraw(), "cells with bit 7 set change colour");

        write_mode(&mut vga, 0x04);
        assert!(!vga.take_full_redraw(), "no change, no repaint");

        write_mode(&mut vga, 0x0C);
        assert!(vga.blink_mode);
        assert!(vga.take_full_redraw());
    }

    /// Regression: the DAC state port always read 03h. It tells whether
    /// the read index (03h) or the write index (00h) was set last.
    #[test]
    fn dac_state_reports_the_last_index_port_written() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        vga.port_out_byte(0x3C8, 0x10, &mut rt.cpu);
        assert_eq!(vga.port_in_byte(0x3C7, &mut rt.cpu) & 0x03, 0x00, "write mode");
        vga.port_out_byte(0x3C7, 0x10, &mut rt.cpu);
        assert_eq!(vga.port_in_byte(0x3C7, &mut rt.cpu) & 0x03, 0x03, "read mode");
        vga.port_out_byte(0x3C8, 0x10, &mut rt.cpu);
        assert_eq!(vga.port_in_byte(0x3C7, &mut rt.cpu) & 0x03, 0x00, "write mode again");
    }

    /// `dac_to_rgb8` widens the 6-bit DAC components by bit replication
    /// `(v << 2) | (v >> 4)`, so 0 maps to 0x00 and 63 to 0xFF (full scale).
    #[test]
    fn dac_conversion_endpoints_are_exact() {
        let mut vga = VgaDevice::new();
        let mut rt = Runtime::new_test();
        // Program DAC entry 1 to (63, 32, 0) through the write-index and
        // data ports, as software does.
        vga.port_out_byte(0x3C8, 1, &mut rt.cpu);
        vga.port_out_byte(0x3C9, 63, &mut rt.cpu);
        vga.port_out_byte(0x3C9, 32, &mut rt.cpu);
        vga.port_out_byte(0x3C9, 0, &mut rt.cpu);
        assert_eq!(
            vga.dac_to_rgb8(1),
            [0xFF, 130, 0x00],
            "63 must map to 0xFF (full scale), 32 to 130 and 0 to 0x00"
        );
    }
}
