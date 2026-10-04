//! The Memory panel: the addresses its field takes, the rows it reads, and
//! what it shows of a machine the BIOS has booted, driven as a user drives
//! it.

use std::sync::mpsc::channel;

use egui::accesskit::Role;

use super::*;
use crate::gui::overlay::testing::{Driver, Screen, Shell, waiting_for_a_key};
use crate::wall_clock::HostClock;
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::vm::runtime::Runtime;

/// A machine without disks or ROM, which never runs: its memory is what
/// the test writes there.
fn machine() -> Runtime {
    let (_keys, key_events) = channel();
    Runtime::new(
        vec![],
        vec![],
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .expect("a machine without disks has nothing to open")
}

/// The Memory panel open over a machine whose BIOS found nothing to boot
/// and waits for a key.
fn panel() -> Driver<Shell> {
    let (runtime, _keys) = waiting_for_a_key();
    Shell::with_panel(runtime, "Memory")
}

/// The row on the screen that starts at `address`.
fn row_at<'a>(screen: &'a Screen, address: &str) -> &'a str {
    let start = format!("{address}  ");
    screen
        .texts()
        .into_iter()
        .find(|text| text.starts_with(&start))
        .unwrap_or_else(|| panic!("no row {address} in {:?}", screen.texts()))
}

/// The field takes a physical address of up to five digits, or a segment
/// and an offset of up to four, in hexadecimal digits of either case, with
/// spaces around them.
#[test]
fn the_field_takes_a_physical_or_a_segmented_address() {
    let segmented = |segment, offset| Ok(Address::Segmented { segment, offset });
    assert_eq!(parse_address("B8000"), Ok(Address::Physical(0xB8000)));
    assert_eq!(parse_address(" fffff "), Ok(Address::Physical(0xFFFFF)));
    assert_eq!(parse_address("0"), Ok(Address::Physical(0x00000)));
    assert_eq!(parse_address("b800:0000"), segmented(0xB800, 0x0000));
    assert_eq!(parse_address("40 : 1a"), segmented(0x0040, 0x001A));
    assert_eq!(parse_address("FFFF:FFFF"), segmented(0xFFFF, 0xFFFF));
}

/// What is not an address is refused, and the message says why.
#[test]
fn what_is_not_an_address_is_refused_with_the_reason() {
    for (text, reason) in [
        ("", "Type an address: B8000 or B800:0000"),
        ("  ", "Type an address: B8000 or B800:0000"),
        (
            "100000",
            "100000 is not an address: the physical address has more than 5 digits",
        ),
        (
            "B8000:0",
            "B8000:0 is not an address: the segment has more than 4 digits",
        ),
        (
            "B800:10000",
            "B800:10000 is not an address: the offset has more than 4 digits",
        ),
        (":10", ":10 is not an address: the segment is missing"),
        ("B800:", "B800: is not an address: the offset is missing"),
        (
            "B800:0:0",
            "B800:0:0 is not an address: : is not a hexadecimal digit",
        ),
        (
            "0xB8000",
            "0xB8000 is not an address: x is not a hexadecimal digit",
        ),
        ("+10", "+10 is not an address: + is not a hexadecimal digit"),
    ] {
        assert_eq!(parse_address(text), Err(reason.to_string()), "{text:?}");
    }
}

/// A segment and an offset add up to a physical address that wraps at the
/// end of the megabyte, as on the 8086, which has no 21st address line.
/// Each kind of address is written the way it is typed.
#[test]
fn a_segmented_address_wraps_at_the_end_of_the_megabyte() {
    let at = |segment, offset| Address::Segmented { segment, offset };
    assert_eq!(at(0xB800, 0x0000).physical(), 0xB8000);
    assert_eq!(at(0x0040, 0x0010).physical(), 0x00410);
    assert_eq!(at(0xF000, 0xFFFF).physical(), 0xFFFFF);
    assert_eq!(at(0xFFFF, 0x0010).physical(), 0x00000);
    assert_eq!(at(0xFFFF, 0xFFFF).physical(), 0x0FFEF);
    assert_eq!(at(0x0040, 0x001A).to_string(), "0040:001A");
    assert_eq!(Address::Physical(0xB8000).to_string(), "B8000");
    assert_eq!(Address::Physical(0x00410).to_string(), "00410");
}

/// A row shows its address, its sixteen bytes with a dash between the
/// halves, and their characters, a dot for each byte that is not printable
/// ASCII. A row of a segment that runs past the end of the megabyte reads
/// its start.
#[test]
fn a_row_shows_its_address_its_bytes_and_their_characters() {
    let mut runtime = machine();
    let memory = &mut runtime.cpu.memory;
    for (index, byte) in b"Hi!\x00\x07 ~\x7F\x80\xFFabcdef".iter().enumerate() {
        memory.write_byte(0x12340 + index, *byte);
        memory.write_byte(index, *byte);
    }
    let bytes = "48 69 21 00 07 20 7E 7F-80 FF 61 62 63 64 65 66  Hi!.. ~...abcdef";
    let segmented = Address::Segmented {
        segment: 0x1234,
        offset: 0x0000,
    };
    let memory = &runtime.cpu.memory;
    assert_eq!(row_text(memory, segmented), format!("1234:0000  {bytes}"));
    assert_eq!(
        row_text(memory, Address::Physical(0x12340)),
        format!("12340  {bytes}")
    );
    let wrapped = Address::Segmented {
        segment: 0xFFFF,
        offset: 0x0010,
    };
    assert_eq!(row_text(memory, wrapped), format!("FFFF:0010  {bytes}"));
}

/// The marked byte is drawn as a selection, its two digits and its
/// character, wherever it is in the row; the rest of the row, and a row
/// without the mark, is drawn as it comes.
#[test]
fn the_marked_byte_is_drawn_as_a_selection() {
    let text = "0040:0010  21 00 00 00 00 00 00 00-00 00 00 00 00 00 00 7A  !..............z";
    let selected = |marked| {
        let job = row_job(text, 9, Some(marked), TextFormat::default());
        assert_eq!(job.text, text);
        job.sections
            .iter()
            .filter(|section| section.format.background == PALETTE.accent)
            .map(|section| {
                let bytes = &section.byte_range;
                job.text[bytes.start.0..bytes.end.0].to_string()
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(selected(0), ["21", "!"]);
    assert_eq!(selected(15), ["7A", "z"]);

    let job = row_job(text, 9, None, TextFormat::default());
    assert_eq!((job.text.as_str(), job.sections.len()), (text, 1));
}

/// A jump brings the row of its address to the top of the view and marks
/// its byte. A segmented address counts the rows in its segment, 4096 of
/// them; a physical one in the megabyte, 65536.
#[test]
fn a_jump_brings_the_row_of_its_address_to_the_top() {
    let mut view = View::default();
    assert_eq!(view.row_address(0).to_string(), "0000:0000", "the vectors");

    view.go(Address::Segmented {
        segment: 0xB800,
        offset: 0x00A5,
    });
    assert_eq!(
        (view.segment, view.scroll_to, view.mark, view.rows()),
        (Some(0xB800), Some(0x0A), Some(0xB80A5), 4096)
    );
    assert_eq!(view.row_address(0x0A).to_string(), "B800:00A0");

    view.go(Address::Physical(0xB80A5));
    assert_eq!(
        (view.segment, view.scroll_to, view.mark, view.rows()),
        (None, Some(0xB80A), Some(0xB80A5), 65536)
    );
    assert_eq!(view.row_address(0xB80A).to_string(), "B80A0");
}

/// Typed into the field, `0040:0000` shows the BIOS data area: its row at
/// 0040:0010 starts with the equipment word, low byte first. The BIOS sets
/// 0061h: 80x25 colour video, and two floppy drives, A: and B:, which a
/// machine always has, with or without a disk in them.
#[test]
fn the_bios_data_area_shows_the_equipment_word() {
    let mut shell = panel();
    assert_eq!(shell.target.runtime.cpu.memory.read_word(0x0410), 0x0061);
    let screen = shell.fill("Address", "0040:0000");
    row_at(screen, "0040:0000");
    let equipment = row_at(screen, "0040:0010");
    assert!(equipment.starts_with("0040:0010  61 00 "), "{equipment}");
}

/// A physical address shows physical rows: at B8000, the message the BIOS
/// printed on the first row of the screen, each character followed by its
/// attribute, 07h.
#[test]
fn a_physical_address_shows_the_rows_of_the_megabyte() {
    let mut shell = panel();
    let screen = shell.fill("Address", "B8000");
    assert_eq!(
        row_at(screen, "B8000"),
        "B8000  4E 07 6F 07 20 07 62 07-6F 07 6F 07 74 07 61 07  N.o. .b.o.o.t.a."
    );
}

/// The rows are read on every frame: a byte the guest writes shows on the
/// next one.
#[test]
fn the_rows_show_memory_as_it_is_on_each_frame() {
    let mut shell = panel();
    shell.fill("Address", "0000:0500");
    assert!(row_at(shell.screen(), "0000:0500").starts_with("0000:0500  00 "));

    shell.target.runtime.cpu.memory.write_byte(0x0500, 0xAB);
    let screen = shell.frame(Vec::new());
    assert!(row_at(screen, "0000:0500").starts_with("0000:0500  AB "));
}

/// Each register pair is shown with its value, beside the button that
/// jumps to where it points: CS:IP to the code of the BIOS the CPU halted
/// in.
#[test]
fn the_register_pairs_show_their_values_and_jump_where_they_point() {
    let mut shell = panel();
    for (name, address) in pointers(&shell.target.runtime.cpu.registers) {
        let value = address.to_string();
        assert!(shell.screen().shows(&value), "{name} {value}");
    }

    let registers = &shell.target.runtime.cpu.registers;
    let (cs, ip) = (registers.cs.reg().word(), registers.pc.word());
    assert_eq!(cs, 0xF000, "the CPU waits in the BIOS");
    let row = Address::Segmented {
        segment: cs,
        offset: ip & !0x000F,
    };
    let expected = row_text(&shell.target.runtime.cpu.memory, row);
    let screen = shell.click(Role::Button, "CS:IP");
    assert_eq!(row_at(screen, &row.to_string()), expected);
}

/// The landmarks jump to the places they name: the vector table, where the
/// vector of INT 08h points into the BIOS, at F000, and the ROM itself.
#[test]
fn the_landmarks_jump_to_the_vector_table_and_the_rom() {
    let mut shell = panel();
    shell.fill("Address", "B8000");
    let screen = shell.click(Role::Button, "IVT");
    let vectors = row_at(screen, "0000:0020");
    assert_eq!(&vectors[17..22], "00 F0", "INT 08h at F000: {vectors}");

    let expected = row_text(
        &shell.target.runtime.cpu.memory,
        Address::Segmented {
            segment: 0xF000,
            offset: 0x0000,
        },
    );
    let screen = shell.click(Role::Button, "ROM");
    assert_eq!(row_at(screen, "F000:0000"), expected);
}

/// What is not an address is reported, and the view stays where it was.
#[test]
fn an_address_that_is_none_is_reported() {
    let mut shell = panel();
    let screen = shell.fill("Address", "B8000:0");
    assert!(
        screen.shows("B8000:0 is not an address: the segment has more than 4 digits"),
        "{:?}",
        screen.texts()
    );
    row_at(screen, "0000:0000");
}

/// The view is a window on memory, not the megabyte: the rows on the
/// screen are the ones that fit, and at most two more, which the scroll
/// area lays out at its edge.
#[test]
fn the_view_shows_a_window_of_rows() {
    let mut shell = panel();
    let screen = shell.fill("Address", "0040:0000");
    let rows = screen
        .texts()
        .iter()
        .filter(|text| text.starts_with("0040:"))
        .count();
    assert!(
        (ROWS_SHOWN..=ROWS_SHOWN + 2).contains(&rows),
        "{rows} rows of 0040 on the screen"
    );
    assert!(
        !screen
            .texts()
            .iter()
            .any(|text| text.starts_with("0040:0200")),
        "a row far below is not read"
    );
}
