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
        rt.cpu
            .memory
            .write_byte(0xB8000 + cell * 2, b'a' + (cell % 26) as u8);
        rt.cpu.memory.write_byte(0xB8000 + cell * 2 + 1, 0x07);
    }
    let mut vga = VgaDevice::new();
    set_cursor(&mut vga, &mut rt, 5, 3);
    let mut renderer = TermRenderer::new();
    let frame = renderer.frame(&mut vga, &rt.cpu.memory);

    assert!(
        frame.starts_with(&format!("\x1b[1;1H{GREY_ON_BLACK}abc")),
        "{:?}",
        &frame[..60]
    );
    assert_eq!(frame.matches("\x1b[38;2;").count(), 1, "foreground changes");
    assert_eq!(frame.matches("\x1b[48;2;").count(), 1, "background changes");
    // One move per row, and one to put the cursor back.
    assert_eq!(frame.matches('H').count(), ROWS + 1);
    assert_eq!(text_of(frame).len(), 2000);
    assert!(
        frame.ends_with("\x1b[?25h\x1b[6;4H"),
        "cursor shown at row 5, column 3"
    );
    assert!(frame.len() < 2400, "{} bytes", frame.len());
}

#[test]
fn unchanged_screen_produces_no_output() {
    let (rt, mut vga, mut renderer) = drawn_screen();
    assert_eq!(renderer.frame(&mut vga, &rt.cpu.memory), "");
}

/// After any screen update the host cursor goes back to the emulated
/// cursor, whether or not the emulated cursor moved.
#[test]
fn host_cursor_returns_to_the_emulated_cursor_after_an_update() {
    let (mut rt, mut vga, mut renderer) = drawn_screen();
    put(&mut rt, 0, 70, b'X', 0x07);
    assert_eq!(
        renderer.frame(&mut vga, &rt.cpu.memory),
        "\x1b[1;71HX\x1b[6;4H"
    );
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

/// The host cursor is hidden while the emulated one is switched off or
/// parked outside the displayed page, and shown again where it returns.
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
    assert_eq!(
        renderer.frame(&mut vga, &rt.cpu.memory),
        "\x1b[?25h\x1b[3;1H"
    );
}

/// A queued full redraw repaints every cell, then puts the host cursor
/// back on the emulated one.
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
