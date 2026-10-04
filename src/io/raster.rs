//! CPU-side rasterisers: VGA register state and video memory to an RGBA
//! frame. Text mode draws each cell's glyph from the 8x16 font in its
//! attribute colours, then the cursor, both blinking by `BlinkPhase`;
//! mode 13h sends its 320x200 bytes through the DAC.
//!
//! The invariant: nothing here touches the GPU. The window renderer only
//! uploads and scales these frames, so every pixel of them can be checked
//! by a unit test.

use std::ops::Range;
use std::time::Duration;

use crate::io::font_cp437::VGA_FONT_8X16;
use crate::io::vga::VgaDevice;
use crate::vm::memory::Memory;

/// Text mode frame: 80 columns of 8 pixels, 25 rows of 16 scanlines.
pub const TEXT_WIDTH: usize = 640;
pub const TEXT_HEIGHT: usize = 400;
/// Mode 13h frame.
pub const GFX_WIDTH: usize = 320;
pub const GFX_HEIGHT: usize = 200;

const CELL_WIDTH: usize = 8;
const CELL_HEIGHT: usize = 16;
const TEXT_ROWS: usize = 25;
const BYTES_PER_PIXEL: usize = 4;
const GFX_BASE: usize = 0xA0000;

/// The cursor is shown for this long, then hidden for as long: 16 frames
/// of a 70 Hz display, as on the VGA. Blinking characters switch at half
/// that rate.
const CURSOR_BLINK_MS: u128 = 229;

/// Blink state of the text screen at one moment.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BlinkPhase {
    /// The cursor is in the visible half of its blink.
    pub cursor_on: bool,
    /// Characters with the blink attribute are in their visible half.
    pub text_on: bool,
}

impl BlinkPhase {
    /// The phase `elapsed` after an arbitrary starting point.
    pub fn at(elapsed: Duration) -> Self {
        let half_periods = elapsed.as_millis() / CURSOR_BLINK_MS;
        Self {
            cursor_on: half_periods.is_multiple_of(2),
            text_on: half_periods % 4 < 2,
        }
    }
}

fn put_pixel(frame: &mut [u8], width: usize, x: usize, y: usize, rgb: [u8; 3]) {
    let offset = (y * width + x) * BYTES_PER_PIXEL;
    frame[offset..offset + 3].copy_from_slice(&rgb);
    frame[offset + 3] = 255;
}

/// Foreground and background colour of a text cell: attribute nibble ->
/// attribute controller palette register -> DAC entry.
fn cell_colours(vga: &VgaDevice, attr: u8) -> ([u8; 3], [u8; 3]) {
    let fg_index = attr & 0x0F;
    let bg_index = if vga.blink_mode {
        (attr >> 4) & 0x07
    } else {
        attr >> 4
    };
    let colour = |index: u8| vga.dac_to_rgb8(vga.attr_regs[index as usize] & 0x3F);
    (colour(fg_index), colour(bg_index))
}

/// Scanlines of the 16-line character cell covered by a cursor programmed
/// as `start..=end` in CRTC registers 0Ah/0Bh. A shape that ends above
/// line 8 was written for the 8-line CGA cell (the BIOS uses 6-7) and is
/// doubled; anything else is taken as real scanlines. A start beyond the
/// end shows no cursor.
pub fn cursor_scanlines(start: u8, end: u8) -> Range<usize> {
    let (start, end) = (start as usize, end as usize);
    if start > end {
        0..0
    } else if end < 8 {
        start * 2..(end + 1) * 2
    } else {
        start.min(CELL_HEIGHT)..(end + 1).min(CELL_HEIGHT)
    }
}

/// Rasterises the 80x25 text screen into `frame` (640x400 RGBA) as it
/// looks in blink phase `phase`.
pub fn rasterize_text(frame: &mut [u8], vga: &VgaDevice, memory: &Memory, phase: BlinkPhase) {
    let cols = vga.cols();

    for row in 0..TEXT_ROWS {
        for col in 0..cols {
            let addr = vga.text_cell_address(row * cols + col);
            let ch = memory.read_byte(addr) as usize;
            let attr = memory.read_byte(addr + 1);
            let (fg, bg) = cell_colours(vga, attr);
            // A blinking character shows only its background half the time.
            let hidden = vga.blink_mode && attr & 0x80 != 0 && !phase.text_on;

            for scanline in 0..CELL_HEIGHT {
                let bits = VGA_FONT_8X16[ch * CELL_HEIGHT + scanline];
                let y = row * CELL_HEIGHT + scanline;
                for bit in 0..CELL_WIDTH {
                    let lit = !hidden && bits & (0x80 >> bit) != 0;
                    put_pixel(
                        frame,
                        TEXT_WIDTH,
                        col * CELL_WIDTH + bit,
                        y,
                        if lit { fg } else { bg },
                    );
                }
            }
        }
    }

    if phase.cursor_on {
        draw_cursor(frame, vga, memory);
    }
}

/// Fills the cursor's scanlines in the cell at the CRTC cursor location
/// with that cell's foreground colour. Draws nothing while the cursor is
/// disabled or outside the displayed page.
fn draw_cursor(frame: &mut [u8], vga: &VgaDevice, memory: &Memory) {
    let (Some(cell), Some((start, end))) = (vga.visible_cursor_cell(), vga.cursor_shape()) else {
        return;
    };
    let cols = vga.cols();
    let (row, col) = (cell / cols, cell % cols);
    let attr = memory.read_byte(vga.text_cell_address(cell) + 1);
    let (fg, _) = cell_colours(vga, attr);

    for scanline in cursor_scanlines(start, end) {
        for bit in 0..CELL_WIDTH {
            put_pixel(
                frame,
                TEXT_WIDTH,
                col * CELL_WIDTH + bit,
                row * CELL_HEIGHT + scanline,
                fg,
            );
        }
    }
}

/// Rasterises mode 13h (320x200, one byte per pixel at A000:0000) into
/// `frame` (320x200 RGBA).
pub fn rasterize_mode13h(frame: &mut [u8], vga: &VgaDevice, memory: &Memory) {
    for y in 0..GFX_HEIGHT {
        for x in 0..GFX_WIDTH {
            let index = memory.read_byte(GFX_BASE + y * GFX_WIDTH + x);
            put_pixel(frame, GFX_WIDTH, x, y, vga.dac_to_rgb8(index));
        }
    }
}

#[cfg(test)]
#[path = "raster_tests.rs"]
mod tests;
