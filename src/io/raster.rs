//! CPU-side rasterisers: VGA register state and video memory to an RGBA
//! frame. Nothing here touches the GPU, so the output can be unit-tested.

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
        Self { cursor_on: half_periods % 2 == 0, text_on: half_periods % 4 < 2 }
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
    let bg_index = if vga.blink_mode { (attr >> 4) & 0x07 } else { attr >> 4 };
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
                    put_pixel(frame, TEXT_WIDTH, col * CELL_WIDTH + bit, y, if lit { fg } else { bg });
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
            put_pixel(frame, TEXT_WIDTH, col * CELL_WIDTH + bit, row * CELL_HEIGHT + scanline, fg);
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
mod tests {
    use super::*;
    use crate::io::bus::IoDevice;
    use crate::vm::runtime::Runtime;

    const BLACK: [u8; 3] = [0, 0, 0];
    const BLUE: [u8; 3] = [0, 0, 170];
    const LIGHT_GREY: [u8; 3] = [170, 170, 170];
    const YELLOW: [u8; 3] = [255, 255, 85];
    const WHITE: [u8; 3] = [255, 255, 255];
    const BRIGHT_BLUE: [u8; 3] = [85, 85, 255];

    const ALL_ON: BlinkPhase = BlinkPhase { cursor_on: true, text_on: true };
    const CURSOR_OFF: BlinkPhase = BlinkPhase { cursor_on: false, text_on: true };
    const TEXT_OFF: BlinkPhase = BlinkPhase { cursor_on: false, text_on: false };

    fn text_frame() -> Vec<u8> {
        vec![0; TEXT_WIDTH * TEXT_HEIGHT * BYTES_PER_PIXEL]
    }

    fn pixel(frame: &[u8], width: usize, x: usize, y: usize) -> [u8; 3] {
        let offset = (y * width + x) * BYTES_PER_PIXEL;
        assert_eq!(frame[offset + 3], 255, "alpha at {x},{y}");
        [frame[offset], frame[offset + 1], frame[offset + 2]]
    }

    /// A machine whose text screen is blank (spaces, attribute 07h).
    fn blank_screen() -> (Runtime, VgaDevice) {
        let mut rt = Runtime::new_test();
        for cell in 0..80 * 25 {
            rt.cpu.memory.write_byte(0xB8000 + cell * 2, b' ');
            rt.cpu.memory.write_byte(0xB8000 + cell * 2 + 1, 0x07);
        }
        (rt, VgaDevice::new())
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

    /// Rows of the cell at `row`,`col` whose eight pixels all have `rgb`.
    fn filled_scanlines(frame: &[u8], row: usize, col: usize, rgb: [u8; 3]) -> Vec<usize> {
        (0..CELL_HEIGHT)
            .filter(|scanline| {
                (0..CELL_WIDTH).all(|bit| {
                    pixel(frame, TEXT_WIDTH, col * 8 + bit, row * 16 + scanline) == rgb
                })
            })
            .collect()
    }

    #[test]
    fn text_cells_are_drawn_in_their_attribute_colours() {
        let (mut rt, vga) = blank_screen();
        // Row 0, column 1: a full block, yellow on blue.
        rt.cpu.memory.write_byte(0xB8002, 0xDB);
        rt.cpu.memory.write_byte(0xB8003, 0x1E);
        // Row 0, column 2: a space with the same attribute.
        rt.cpu.memory.write_byte(0xB8005, 0x1E);

        let mut frame = text_frame();
        rasterize_text(&mut frame, &vga, &rt.cpu.memory, CURSOR_OFF);

        assert_eq!(filled_scanlines(&frame, 0, 1, YELLOW).len(), 16);
        assert_eq!(filled_scanlines(&frame, 0, 2, BLUE).len(), 16);
        assert_eq!(filled_scanlines(&frame, 0, 3, BLACK).len(), 16);
    }

    /// Regression: the text rasteriser drew glyphs only, so the GUI showed
    /// no cursor at all.
    #[test]
    fn cursor_is_drawn_at_the_crtc_location() {
        let (mut rt, mut vga) = blank_screen();
        set_cursor(&mut vga, &mut rt, 1, 2);

        let mut frame = text_frame();
        rasterize_text(&mut frame, &vga, &rt.cpu.memory, ALL_ON);

        // The power-on shape is the BIOS one, lines 6-7 of an 8-line cell:
        // the bottom quarter of the 16-line cell.
        assert_eq!(filled_scanlines(&frame, 1, 2, LIGHT_GREY), vec![12, 13, 14, 15]);
        assert_eq!(filled_scanlines(&frame, 1, 2, BLACK).len(), 12);
        // Neighbouring cells are untouched.
        assert_eq!(filled_scanlines(&frame, 1, 1, BLACK).len(), 16);
        assert_eq!(filled_scanlines(&frame, 1, 3, BLACK).len(), 16);
        assert_eq!(filled_scanlines(&frame, 0, 2, BLACK).len(), 16);
    }

    #[test]
    fn cursor_shape_follows_crtc_registers_0a_and_0b() {
        let (mut rt, mut vga) = blank_screen();
        set_cursor(&mut vga, &mut rt, 0, 0);
        write_crtc(&mut vga, &mut rt, 0x0A, 0x00);
        write_crtc(&mut vga, &mut rt, 0x0B, 0x0F); // block cursor, real scanlines

        let mut frame = text_frame();
        rasterize_text(&mut frame, &vga, &rt.cpu.memory, ALL_ON);
        assert_eq!(filled_scanlines(&frame, 0, 0, LIGHT_GREY).len(), 16);
    }

    #[test]
    fn cursor_is_not_drawn_when_disabled_off_screen_or_in_the_off_phase() {
        let (mut rt, mut vga) = blank_screen();
        let lit = |vga: &VgaDevice, rt: &Runtime, phase: BlinkPhase| {
            let mut frame = text_frame();
            rasterize_text(&mut frame, vga, &rt.cpu.memory, phase);
            frame.chunks(BYTES_PER_PIXEL).filter(|p| p[..3] != BLACK).count()
        };

        set_cursor(&mut vga, &mut rt, 1, 2);
        assert_eq!(lit(&vga, &rt, ALL_ON), 4 * 8, "visible cursor");
        assert_eq!(lit(&vga, &rt, CURSOR_OFF), 0, "off phase of the blink");

        write_crtc(&mut vga, &mut rt, 0x0A, 0x20); // cursor disable bit
        assert_eq!(lit(&vga, &rt, ALL_ON), 0, "disabled through CRTC 0Ah bit 5");

        write_crtc(&mut vga, &mut rt, 0x0A, 0x06);
        set_cursor(&mut vga, &mut rt, 25, 0); // parked below the last row
        assert_eq!(lit(&vga, &rt, ALL_ON), 0, "outside the displayed page");
    }

    /// Regression: with blinking enabled, attribute bit 7 only lost the
    /// bright background; the character never blinked.
    #[test]
    fn blinking_characters_show_only_their_background_in_the_off_phase() {
        let (mut rt, mut vga) = blank_screen();
        // A full block, white on blue, attribute bit 7 set.
        rt.cpu.memory.write_byte(0xB8000, 0xDB);
        rt.cpu.memory.write_byte(0xB8001, 0x9F);
        let cell = |vga: &VgaDevice, rt: &Runtime, phase: BlinkPhase| {
            let mut frame = text_frame();
            rasterize_text(&mut frame, vga, &rt.cpu.memory, phase);
            pixel(&frame, TEXT_WIDTH, 0, 0)
        };

        assert_eq!(cell(&vga, &rt, CURSOR_OFF), WHITE, "visible half of the blink");
        assert_eq!(cell(&vga, &rt, TEXT_OFF), BLUE, "hidden half: background only");

        // Mode control bit 3 clear: bit 7 selects a bright background.
        vga.port_in_byte(0x3DA, &mut rt.cpu);
        vga.port_out_byte(0x3C0, 0x30, &mut rt.cpu);
        vga.port_out_byte(0x3C0, 0x04, &mut rt.cpu);
        assert_eq!(cell(&vga, &rt, TEXT_OFF), WHITE, "no blinking");
        rt.cpu.memory.write_byte(0xB8000, b' ');
        assert_eq!(cell(&vga, &rt, TEXT_OFF), BRIGHT_BLUE);
    }

    #[test]
    fn blink_phase_follows_elapsed_time() {
        let phase = |ms| BlinkPhase::at(Duration::from_millis(ms));
        assert_eq!(phase(0), BlinkPhase { cursor_on: true, text_on: true });
        assert_eq!(phase(229), BlinkPhase { cursor_on: false, text_on: true });
        assert_eq!(phase(458), BlinkPhase { cursor_on: true, text_on: false });
        assert_eq!(phase(687), BlinkPhase { cursor_on: false, text_on: false });
        assert_eq!(phase(916), BlinkPhase { cursor_on: true, text_on: true });
    }

    #[test]
    fn cga_cursor_shapes_are_scaled_to_the_16_line_cell() {
        assert_eq!(cursor_scanlines(6, 7), 12..16); // BIOS underline
        assert_eq!(cursor_scanlines(0, 7), 0..16); // CGA block
        assert_eq!(cursor_scanlines(4, 7), 8..16); // CGA half block
        assert_eq!(cursor_scanlines(13, 14), 13..15); // VGA underline, as is
        assert_eq!(cursor_scanlines(0, 31), 0..16); // clipped to the cell
        assert!(cursor_scanlines(7, 6).is_empty()); // start beyond end
    }

    #[test]
    fn mode_13h_pixels_go_through_the_dac() {
        let mut rt = Runtime::new_test();
        let mut vga = VgaDevice::new();
        vga.port_out_byte(0x3C8, 200, &mut rt.cpu);
        for component in [63, 0, 32] {
            vga.port_out_byte(0x3C9, component, &mut rt.cpu);
        }
        rt.cpu.memory.write_byte(GFX_BASE + GFX_WIDTH + 1, 200); // pixel 1,1

        let mut frame = vec![0; GFX_WIDTH * GFX_HEIGHT * BYTES_PER_PIXEL];
        rasterize_mode13h(&mut frame, &vga, &rt.cpu.memory);

        assert_eq!(pixel(&frame, GFX_WIDTH, 1, 1), [255, 0, 130]);
        assert_eq!(pixel(&frame, GFX_WIDTH, 0, 0), BLACK);
        assert_eq!(pixel(&frame, GFX_WIDTH, 319, 199), BLACK);
    }
}
