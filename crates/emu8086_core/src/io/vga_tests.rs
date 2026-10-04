use super::*;
use crate::io::bus::IoDevice;
use crate::vm::runtime::Runtime;

/// The CRTC data port 3D5h reads back the register the index port selects,
/// whatever its index: the VGA's CRTC registers are all readable.
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

/// The retrace bits follow the timer clock, not the number of status
/// reads, so their timing does not depend on how often the guest looks:
/// 70 frames per second, vertical retrace at the end of each frame.
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

/// Bit 0 reports "display off": it is set through every horizontal blank
/// and stays set for the whole vertical retrace.
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
            assert_eq!(
                status & 0x01,
                0x01,
                "display off during retrace, clock {at}"
            );
        } else if status & 0x01 != 0 {
            blank_clocks += 1;
        }
    }
    // Outside the retrace, every line ends with a blank.
    let lines = (FRAME_CLOCKS - VRETRACE_CLOCKS) / LINE_CLOCKS;
    assert_eq!(blank_clocks, lines * HBLANK_CLOCKS);

    clock.set(0);
    assert_eq!(
        vga.port_in_byte(0x3DA, &mut rt.cpu) & 0x01,
        0,
        "start of a line"
    );
    clock.set(LINE_CLOCKS - 1);
    assert_eq!(
        vga.port_in_byte(0x3DA, &mut rt.cpu) & 0x01,
        0x01,
        "end of a line"
    );
}

fn write_crtc(vga: &mut VgaDevice, rt: &mut Runtime, index: u8, value: u8) {
    vga.port_out_byte(0x3D4, index, &mut rt.cpu);
    vga.port_out_byte(0x3D5, value, &mut rt.cpu);
}

fn read_crtc(vga: &mut VgaDevice, rt: &mut Runtime, index: u8) -> u8 {
    vga.port_out_byte(0x3D4, index, &mut rt.cpu);
    vga.port_in_byte(0x3D5, &mut rt.cpu)
}

/// Displayed cells wrap at the end of the 32 KB text buffer, not at the
/// CGA's 16 KB, so display pages 4-7 are addressable.
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

/// The dump is text: the mode, the registers that place the screen and the
/// cursor, and the page that is displayed, each of its 25 rows once as
/// characters and once as character and attribute in hexadecimal. It shows
/// the displayed page, not the start of the buffer.
#[test]
fn dump_text_describes_the_page_that_is_displayed() {
    let mut vga = VgaDevice::new();
    let mut rt = Runtime::new_test();
    // Page 1 starts 2048 cells into the buffer; the cursor is on its
    // second row.
    write_crtc(&mut vga, &mut rt, 0x0C, 0x08);
    write_crtc(&mut vga, &mut rt, 0x0D, 0x00);
    write_crtc(&mut vga, &mut rt, 0x0E, 0x08);
    write_crtc(&mut vga, &mut rt, 0x0F, 82);
    for (cell, character) in b"HELLO".iter().enumerate() {
        let address = vga.text_cell_address(cell);
        rt.cpu.memory.write_byte(address, *character);
        rt.cpu.memory.write_byte(address + 1, 0x1F);
    }
    rt.cpu.memory.write_byte(0xB8000, b'X'); // page 0, not displayed

    let dump = vga.dump_text(&rt.cpu.memory);
    assert!(dump.starts_with("=== VGA Diagnostic Dump ===\n"), "{dump}");
    assert!(dump.contains("Video mode: Text80x25\n"), "{dump}");
    assert!(
        dump.contains("CRTC start_address=2048 cursor_offset=2130 "),
        "{dump}"
    );
    assert!(
        dump.contains("CRTC regs: 0x0C=08 0x0D=00 0x0E=08 0x0F=52\n"),
        "{dump}"
    );

    let rows: Vec<&str> = dump
        .lines()
        .filter(|line| line.starts_with("Row "))
        .collect();
    assert_eq!(rows.len(), 25);
    // Characters outside the printable ASCII range show as dots.
    assert_eq!(rows[0], format!("Row 00: |HELLO{}|", ".".repeat(75)));
    assert!(
        dump.contains("\n        481F 451F 4C1F 4C1F 4F1F 0000 "),
        "{dump}"
    );
    assert!(!dump.contains('X'), "page 0 is not on the screen:\n{dump}");
}

/// CRTC register 01h holds the column count minus one on the VGA (4Fh for
/// 80) and the count itself on the 6845; both read as the count.
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

/// Every CRTC register from 00h to 18h keeps all eight bits written to it,
/// even where the 6845's register of the same index is narrower.
#[test]
fn crtc_registers_up_to_18h_read_back_what_was_written() {
    let mut vga = VgaDevice::new();
    let mut rt = Runtime::new_test();
    for index in 0x00..=0x18 {
        write_crtc(&mut vga, &mut rt, index, 0xA5 ^ index);
    }
    for index in 0x00..=0x18 {
        assert_eq!(
            read_crtc(&mut vga, &mut rt, index),
            0xA5 ^ index,
            "index {index:#04X}"
        );
    }
    // There is no register 19h.
    write_crtc(&mut vga, &mut rt, 0x19, 0x77);
    assert_eq!(read_crtc(&mut vga, &mut rt, 0x19), 0x00);
}

/// Index registers keep only their implemented bits, so a graphics
/// controller index written with high bits set still selects the mode
/// register, and the video mode follows the data written to it.
#[test]
fn index_registers_keep_only_their_implemented_bits() {
    let mut vga = VgaDevice::new();
    let mut rt = Runtime::new_test();
    for port in [0x3C4, 0x3CE, 0x3D4] {
        vga.port_out_byte(port, 0xFF, &mut rt.cpu);
    }
    assert_eq!(vga.port_in_byte(0x3C4, &mut rt.cpu), 0x07, "sequencer");
    assert_eq!(
        vga.port_in_byte(0x3CE, &mut rt.cpu),
        0x0F,
        "graphics controller"
    );
    assert_eq!(vga.port_in_byte(0x3D4, &mut rt.cpu), 0x3F, "CRTC");

    vga.port_in_byte(0x3DA, &mut rt.cpu); // attribute flip-flop to "index"
    vga.port_out_byte(0x3C0, 0xFF, &mut rt.cpu);
    assert_eq!(
        vga.port_in_byte(0x3C0, &mut rt.cpu),
        0x3F,
        "attribute controller"
    );

    vga.port_out_byte(0x3CE, 0x15, &mut rt.cpu); // register 5
    vga.port_out_byte(0x3CF, 0x40, &mut rt.cpu);
    assert_eq!(vga.video_mode, VideoMode::Mode13h);
}

/// Bit 3 of attribute controller register 10h selects blinking or bright
/// backgrounds, as bit 5 of the CGA mode port 3D8h does: whichever of the
/// two was written last decides.
#[test]
fn attribute_mode_bit_3_selects_blink_or_bright_background() {
    let mut vga = VgaDevice::new();
    let mut rt = Runtime::new_test();
    assert!(vga.blink_mode, "power-on");

    let write_mode = |vga: &mut VgaDevice, rt: &mut Runtime, value: u8| {
        vga.port_in_byte(0x3DA, &mut rt.cpu);
        vga.port_out_byte(0x3C0, 0x30, &mut rt.cpu);
        vga.port_out_byte(0x3C0, value, &mut rt.cpu);
    };

    write_mode(&mut vga, &mut rt, 0x04);
    assert!(!vga.blink_mode);

    write_mode(&mut vga, &mut rt, 0x0C);
    assert!(vga.blink_mode);

    vga.port_out_byte(0x3D8, 0x09, &mut rt.cpu); // CGA mode port, bit 5 clear
    assert!(!vga.blink_mode);
    vga.port_out_byte(0x3D8, 0x29, &mut rt.cpu);
    assert!(vga.blink_mode);
}

/// The DAC state port 3C7h tells whether the read index (03h) or the
/// write index (00h) was set last.
#[test]
fn dac_state_reports_the_last_index_port_written() {
    let mut vga = VgaDevice::new();
    let mut rt = Runtime::new_test();
    vga.port_out_byte(0x3C8, 0x10, &mut rt.cpu);
    assert_eq!(
        vga.port_in_byte(0x3C7, &mut rt.cpu) & 0x03,
        0x00,
        "write mode"
    );
    vga.port_out_byte(0x3C7, 0x10, &mut rt.cpu);
    assert_eq!(
        vga.port_in_byte(0x3C7, &mut rt.cpu) & 0x03,
        0x03,
        "read mode"
    );
    vga.port_out_byte(0x3C8, 0x10, &mut rt.cpu);
    assert_eq!(
        vga.port_in_byte(0x3C7, &mut rt.cpu) & 0x03,
        0x00,
        "write mode again"
    );
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

/// A DAC entry's colour is that entry's, whatever the pixel mask; what a
/// pixel shows goes through the mask first. Reading the colour does not
/// move the guest's read index.
#[test]
fn a_dac_entry_has_its_colour_whatever_the_mask() {
    let mut vga = VgaDevice::new();
    let mut rt = Runtime::new_test();
    vga.port_out_byte(0x3C8, 0x81, &mut rt.cpu);
    for component in [0x3F, 0x15, 0x00] {
        vga.port_out_byte(0x3C9, component, &mut rt.cpu);
    }
    vga.port_out_byte(0x3C6, 0x0F, &mut rt.cpu); // pixel mask
    vga.port_out_byte(0x3C7, 0x81, &mut rt.cpu); // the guest reads entry 81h

    assert_eq!(vga.dac_colour(0x81), [0xFF, 0x55, 0x00]);
    assert_eq!(
        vga.dac_to_rgb8(0x81),
        vga.dac_colour(0x01),
        "a pixel of 81h shows entry 01h"
    );
    assert_eq!(vga.dac_colour(0x01), [0x00, 0x00, 0xAA], "CGA blue");
    assert_eq!(
        vga.port_in_byte(0x3C9, &mut rt.cpu),
        0x3F,
        "the guest's read"
    );
}

/// The register view holds what the guest wrote through each pair of
/// index and data ports, at the index written to, the attribute
/// controller's through its flip-flop; and the power-on cursor shape.
/// Taking the view leaves the flip-flop where it was.
#[test]
fn the_register_view_holds_what_was_written() {
    let mut vga = VgaDevice::new();
    let mut rt = Runtime::new_test();
    for (port, value) in [
        (0x3C2, 0x63),
        (0x3C4, 0x04),
        (0x3C5, 0x0E),
        (0x3CE, 0x05),
        (0x3CF, 0x40),
        (0x3C6, 0x0F),
        (0x3D8, 0x09),
        (0x3D9, 0x30),
        (0x3C0, 0x10),
        (0x3C0, 0x41),
        (0x3C0, 0x14), // an index: the flip-flop expects data next
    ] {
        vga.port_out_byte(port, value, &mut rt.cpu);
    }
    write_crtc(&mut vga, &mut rt, 0x13, 0x28);

    let registers = vga.registers();
    assert_eq!(registers.misc_output, 0x63);
    assert_eq!(registers.sequencer[4], 0x0E);
    assert_eq!(registers.graphics[5], 0x40);
    assert_eq!(registers.attribute[0x10], 0x41);
    assert_eq!(registers.crtc[0x13], 0x28);
    assert_eq!((registers.crtc[0x0A], registers.crtc[0x0B]), (0x06, 0x07));
    assert_eq!(
        (registers.dac_mask, registers.cga_mode, registers.cga_colour),
        (0x0F, 0x09, 0x30)
    );

    vga.port_out_byte(0x3C0, 0x05, &mut rt.cpu);
    assert_eq!(
        vga.registers().attribute[0x14],
        0x05,
        "the view did not reset the flip-flop"
    );
}
