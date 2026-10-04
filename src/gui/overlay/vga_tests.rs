//! The VGA panel: the state it reads out of the adapter's registers, the
//! registers it names, what it shows after the BIOS set mode 3, and the
//! DAC's colours in what it paints.

use std::sync::mpsc::channel;

use egui::epaint::Primitive;

use super::*;
use crate::gui::overlay::testing::{Driver, Shell, waiting_for_a_key};
use crate::wall_clock::HostClock;
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::vm::runtime::Runtime;

/// The panel's title, which the View menu and its window show.
const TITLE: &str = "Video (VGA)";

/// A machine without disks or ROM, its VGA as it powers on, programmed
/// only through the ports the test writes.
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

/// Writes `value` to register `index` of the group at `index_port`.
fn write(runtime: &mut Runtime, index_port: u16, index: u8, value: u8) {
    runtime.port_out_byte(index_port, index);
    runtime.port_out_byte(index_port + 1, value);
}

/// The state of `runtime`'s display, as the panel words it.
fn state_of(runtime: &Runtime) -> Vec<(&'static str, String)> {
    let vga = runtime
        .vga
        .as_ref()
        .expect("the machine has a VGA")
        .borrow();
    state(&vga, &runtime.cpu.memory)
}

/// The setting of `name` in `state`.
fn setting<'a>(state: &'a [(&'static str, String)], name: &str) -> &'a str {
    let (_, setting) = state
        .iter()
        .find(|(shown, _)| *shown == name)
        .unwrap_or_else(|| panic!("no {name} in {state:?}"));
    setting
}

/// The state is read out of the registers: the mode from the graphics
/// mode register, the cursor from its location and shape, counted from
/// the display start, and what attribute bit 7 does from the attribute
/// controller.
#[test]
fn the_state_is_read_out_of_the_registers() {
    let mut runtime = machine();
    assert_eq!(
        state_of(&runtime),
        vec![
            ("Mode", "text, 80 columns".to_string()),
            ("BIOS mode", "00h".to_string()),
            ("Display start", "0000h".to_string()),
            ("Cursor", "row 0, column 0".to_string()),
            ("Cursor shape", "lines 6 to 7".to_string()),
            ("Attribute bit 7", "blinks the character".to_string()),
        ],
        "as the VGA powers on"
    );

    let cursor: u16 = 80 * 3 + 7 + 80; // a row below a display start of 80
    write(&mut runtime, 0x3D4, 0x0C, 0x00);
    write(&mut runtime, 0x3D4, 0x0D, 80);
    write(&mut runtime, 0x3D4, 0x0E, (cursor >> 8) as u8);
    write(&mut runtime, 0x3D4, 0x0F, cursor as u8);
    write(&mut runtime, 0x3D4, 0x0A, 0x0D);
    write(&mut runtime, 0x3D4, 0x0B, 0x0E);
    runtime.port_in_byte(0x3DA); // the attribute controller takes an index
    runtime.port_out_byte(0x3C0, 0x10);
    runtime.port_out_byte(0x3C0, 0x04); // AR10 without the blink bit
    let state = state_of(&runtime);
    assert_eq!(setting(&state, "Display start"), "0050h");
    assert_eq!(setting(&state, "Cursor"), "row 3, column 7");
    assert_eq!(setting(&state, "Cursor shape"), "lines 13 to 14");
    assert_eq!(
        setting(&state, "Attribute bit 7"),
        "brightens the background"
    );

    write(&mut runtime, 0x3D4, 0x0E, 0x10); // far below the page
    assert_eq!(setting(&state_of(&runtime), "Cursor"), "off the page");
    write(&mut runtime, 0x3D4, 0x0A, 0x20); // cursor off
    let state = state_of(&runtime);
    assert_eq!(setting(&state, "Cursor"), "hidden");
    assert_eq!(setting(&state, "Cursor shape"), "hidden");

    write(&mut runtime, 0x3CE, 0x05, 0x40); // 256-colour shift
    let state = state_of(&runtime);
    assert_eq!(setting(&state, "Mode"), "graphics, 320x200, 256 colours");
    assert_eq!(setting(&state, "Cursor"), "not drawn in graphics");
}

/// Each register is shown with where it is and its name, holding what the
/// guest wrote to it.
#[test]
fn the_registers_are_named_with_where_they_are() {
    let mut runtime = machine();
    write(&mut runtime, 0x3CE, 0x05, 0x40);
    write(&mut runtime, 0x3D4, 0x13, 0x28);
    runtime.port_out_byte(0x3C6, 0x0F);
    let vga = runtime
        .vga
        .as_ref()
        .expect("the machine has a VGA")
        .borrow();
    let shown = registers(&vga.registers());
    for expected in [
        ("GR05", "Graphics mode", 0x40),
        ("CR13", "Offset", 0x28),
        ("3C6h", "DAC pixel mask", 0x0F),
        ("SR02", "Map mask", 0x0F),
    ] {
        assert!(shown.contains(&expected), "{expected:?} in {shown:?}");
    }
    let places: Vec<_> = shown.iter().map(|(place, _, _)| *place).collect();
    for (index, place) in places.iter().enumerate() {
        assert!(!places[..index].contains(place), "{place} is shown twice");
    }
}

/// The panel open over a machine whose BIOS set mode 3 and printed that it
/// found nothing to boot.
fn panel() -> Driver<Shell> {
    let (runtime, _keys) = waiting_for_a_key();
    Shell::with_panel(runtime, TITLE)
}

/// After the BIOS: 80-column text, mode 03h, page 0 displayed, the cursor
/// on the line below the BIOS's message, the shape the BIOS gives it, and
/// attribute bit 7 blinking; the registers as the mode set left them.
#[test]
fn after_the_bios_the_panel_shows_mode_3() {
    let shell = panel();
    let screen = shell.screen();
    for (name, value) in [
        ("Mode", "text, 80 columns"),
        ("BIOS mode", "03h"),
        ("Display start", "0000h"),
        ("Cursor", "row 1, column 0"),
        ("Cursor shape", "lines 6 to 7"),
        ("Attribute bit 7", "blinks the character"),
    ] {
        let row = screen.row(name);
        assert_eq!(row.get(..2), Some(&[name, value][..]), "{row:?}");
    }
    // The registers stand right of the state: a row of theirs may have a
    // row of the state on its left.
    let row = screen.row("CGA mode control");
    assert!(
        row.ends_with(&["3D8h", "CGA mode control", "29h"]),
        "the mode set's value, which the BDA keeps a copy of: {row:?}"
    );
}

/// The opaque colours of every vertex the last pass of `shell` painted.
fn painted_colours(shell: &mut Driver<Shell>) -> Vec<egui::Color32> {
    let mut paint = shell.target.overlay.take_paint();
    // Nothing draws here: the texture changes are dropped on purpose.
    paint.textures.clear();
    paint
        .primitives
        .iter()
        .filter_map(|clipped| match &clipped.primitive {
            Primitive::Mesh(mesh) => Some(mesh),
            Primitive::Callback(_) => None,
        })
        .flat_map(|mesh| mesh.vertices.iter().map(|vertex| vertex.color))
        .filter(|colour| colour.a() == 255)
        .collect()
}

/// The DAC is painted as the guest programmed it: an entry written through
/// ports 3C8h and 3C9h is painted in its colour, widened to eight bits, and
/// so is entry 0Fh, the BIOS's white.
#[test]
fn the_dac_is_painted_in_its_colours() {
    let (mut runtime, _keys) = waiting_for_a_key();
    runtime.port_out_byte(0x3C8, 0x81);
    for component in [0x21, 0x0B, 0x37] {
        runtime.port_out_byte(0x3C9, component);
    }
    let mut shell = Shell::with_panel(runtime, TITLE);
    shell.frame(Vec::new());
    let colours = painted_colours(&mut shell);
    for (entry, colour) in [
        ("81h", egui::Color32::from_rgb(134, 44, 223)),
        ("0Fh", egui::Color32::from_rgb(255, 255, 255)),
    ] {
        assert!(colours.contains(&colour), "entry {entry} in {colour:?}");
    }
}
