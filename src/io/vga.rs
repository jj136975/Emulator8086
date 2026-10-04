//! The VGA's registers, ports 3C0h-3DAh: CRTC, sequencer, attribute and
//! graphics controllers, DAC, the CGA mode and colour registers, and Input
//! Status 1, whose retrace bits run on the timer clock the runtime
//! advances. The renderers read the screen's state through it: the video
//! mode, the colours, the cursor, and where each displayed text cell lies.
//!
//! The invariant: a write that changes how the terminal shows unchanged
//! cell bytes (the video mode, or what attribute bit 7 means) requests a
//! full redraw. The terminal renderer repaints only cells whose bytes
//! changed, so a missed request leaves the old picture on screen.

use std::cell::Cell;
use std::io::Write;
use std::rc::Rc;

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use crate::vm::memory::Memory;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum VideoMode {
    Text80x25,
    Mode13h, // 320x200 256-color (chain-4 linear at A000:0000)
}

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
    [0, 0, 0],    // 0  Black
    [0, 0, 42],   // 1  Blue
    [0, 42, 0],   // 2  Green
    [0, 42, 42],  // 3  Cyan
    [42, 0, 0],   // 4  Red
    [42, 0, 42],  // 5  Magenta
    [42, 21, 0],  // 6  Brown
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
        // Power-on contents, until a BIOS mode set loads the mode's own
        // palette: the CGA colours, then a grey ramp.
        let mut dac_palette = [[0u8; 3]; 256];
        dac_palette[..16].copy_from_slice(&CGA_DAC);
        for i in 0..16u8 {
            let v = i * 4 + 2;
            dac_palette[16 + i as usize] = [v, v, v];
        }

        // Attribute controller registers 00h-0Fh are the palette, identity
        // at power-on.
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
            misc_output: 0x67, // default: color mode, odd/even, 28 MHz clock

            seq_index: 0,
            // seq[0]=Reset, seq[1]=Clocking, seq[2]=Map Mask (all planes),
            // seq[3]=Char Map, seq[4]=Memory Mode (odd/even, no chain-4)
            seq_regs: [0x03, 0x00, 0x0F, 0x00, 0x02, 0, 0, 0],
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
        let display_off = vertical_retrace || in_frame % LINE_CLOCKS >= LINE_CLOCKS - HBLANK_CLOCKS;
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
        let cell =
            (self.cursor_offset() as usize).wrapping_sub(self.start_address()) & TEXT_CELL_MASK;
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
        let _ = writeln!(
            f,
            "CRTC start_address={} cursor_offset={} (row={}, col={})",
            start,
            offset,
            offset / cols as u16,
            offset % cols as u16
        );
        let _ = writeln!(
            f,
            "CRTC regs: 0x0C={:02X} 0x0D={:02X} 0x0E={:02X} 0x0F={:02X}",
            self.crtc_regs[0x0C], self.crtc_regs[0x0D], self.crtc_regs[0x0E], self.crtc_regs[0x0F]
        );
        let _ = writeln!(
            f,
            "BDA video_mode={:02X} video_cols={} active_page={:02X}",
            memory.read_byte(0x0449),
            memory.read_word(0x044A),
            memory.read_byte(0x0462)
        );
        let _ = writeln!(
            f,
            "BDA cursor_pos page0: col={:02X} row={:02X}",
            memory.read_byte(0x0450),
            memory.read_byte(0x0451)
        );
        let _ = write!(f, "BDA cursor_pos all pages:");
        for p in 0..8 {
            let addr = 0x0450 + p * 2;
            let _ = write!(
                f,
                " p{}=[{:02X},{:02X}]",
                p,
                memory.read_byte(addr),
                memory.read_byte(addr + 1)
            );
        }
        let _ = writeln!(f);

        let _ = writeln!(
            f,
            "Mode register: {:02X} (display {}abled, blink={})",
            self.mode_register,
            if self.mode_register & 0x08 != 0 {
                "en"
            } else {
                "dis"
            },
            self.blink_mode
        );
        let _ = writeln!(
            f,
            "Misc output: {:02X}, Seq[4]={:02X}, GC[5]={:02X}, GC[6]={:02X}",
            self.misc_output, self.seq_regs[4], self.gc_regs[5], self.gc_regs[6]
        );
        let _ = writeln!(f, "DAC mask: {:02X}", self.dac_mask);

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
        let _ = writeln!(
            f,
            "Keyboard buffer: head={:04X} tail={:04X} start={:04X} end={:04X}",
            kbd_head, kbd_tail, kbd_start, kbd_end
        );
        let _ = writeln!(
            f,
            "Keyboard flags: flags1={:02X} flags2={:02X}",
            kbd_flags1, kbd_flags2
        );
        let _ = write!(f, "Buffer contents:");
        let mut ptr = kbd_start;
        while ptr < kbd_end {
            let w = memory.read_word(0x0400 + ptr as usize);
            if ptr == kbd_head {
                let _ = write!(f, " [H");
            }
            if ptr == kbd_tail {
                let _ = write!(f, " [T");
            }
            let _ = write!(f, " {:04X}", w);
            if ptr == kbd_head || ptr == kbd_tail {
                let _ = write!(f, "]");
            }
            ptr += 2;
        }
        let _ = writeln!(f);

        let _ = writeln!(f);
        for row in 0..ROWS {
            let _ = write!(f, "Row {:02}: |", row);
            for col in 0..cols {
                let ch = memory.read_byte(self.text_cell_address(row * cols + col));
                if (0x20..0x7F).contains(&ch) {
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
            // The VGA reads back every CRTC register.
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
                self.dac_read_index = value;
                self.dac_component = 0;
                self.dac_state = 0x03;
            }
            0x3C8 => {
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
                // Only register 5, Graphics Mode, selects the video mode.
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

#[cfg(test)]
#[path = "vga_tests.rs"]
mod tests;
