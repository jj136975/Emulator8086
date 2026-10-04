//! The 8259A panel: what it makes of the controller's view, and what it
//! shows of a machine the BIOS booted, and of one taking a key.

use super::*;
use crate::gui::overlay::testing::{Driver, Shell, waiting_for_a_key};
use emu8086_core::io::keyboard::EmulatorEvent;

/// The panel's title, which the View menu and its window show.
const TITLE: &str = "Interrupts (8259A)";

/// The controller as the BIOS leaves it: ICW1 11h (edge, cascade, ICW4
/// to come), ICW2 08h, ICW3 04h, ICW4 01h, then the mask FCh.
fn as_the_bios_leaves_it() -> PicView {
    PicView {
        request: 0x00,
        in_service: 0x00,
        mask: 0xFC,
        vector_base: 0x08,
        level_triggered: false,
        single: false,
        auto_eoi: false,
        rotate_on_auto_eoi: false,
        special_mask: false,
        highest_priority: 0,
        reads_in_service: false,
        poll: false,
        awaiting_icw: None,
    }
}

/// The panel open over a machine whose BIOS waits for a key.
fn panel() -> (Driver<Shell>, std::sync::mpsc::Sender<EmulatorEvent>) {
    let (runtime, keys) = waiting_for_a_key();
    (Shell::with_panel(runtime, TITLE), keys)
}

/// Each line is named with what the PC/XT wires to it, the vector it is
/// served by, and its bit in each of the three registers.
#[test]
fn each_line_has_its_device_its_vector_and_its_bits() {
    let view = PicView {
        request: 0x01,
        in_service: 0x02,
        ..as_the_bios_leaves_it()
    };
    let line = |name: &str, device, vector: &str, bits: [bool; 3]| Line {
        name: name.to_string(),
        device,
        vector: vector.to_string(),
        requested: bits[0],
        in_service: bits[1],
        masked: bits[2],
    };
    let listed = lines(&view);
    assert_eq!(listed.len(), 8);
    assert_eq!(
        listed[0],
        line("IRQ0", "timer", "08h", [true, false, false])
    );
    assert_eq!(
        listed[1],
        line("IRQ1", "keyboard", "09h", [false, true, false])
    );
    assert_eq!(
        listed[6],
        line("IRQ6", "floppy", "0Eh", [false, false, true])
    );
    assert_eq!(
        listed[7],
        line("IRQ7", "printer", "0Fh", [false, false, true])
    );

    let relocated = lines(&PicView {
        vector_base: 0x70,
        ..view
    });
    assert_eq!(relocated[7].vector, "77h");
}

/// The modes are set out in words: as the BIOS leaves them, and as other
/// command words would set them, with the word an initialisation still
/// waits for and a poll waiting for its read.
#[test]
fn the_modes_are_set_out_in_words() {
    let settings = |view: PicView| -> Vec<(&'static str, String)> { modes(&view) };
    let text = |pairs: &[(&'static str, &str)]| -> Vec<(&'static str, String)> {
        pairs
            .iter()
            .map(|(name, setting)| (*name, setting.to_string()))
            .collect()
    };
    assert_eq!(
        settings(as_the_bios_leaves_it()),
        text(&[
            ("Vector base", "08h"),
            ("Triggering", "edge"),
            ("End of interrupt", "by command"),
            ("Nesting", "fully nested"),
            ("Priority", "IRQ0 highest"),
            ("Port 20h reads", "IRR"),
            ("Set up for", "a cascade, without slave"),
        ])
    );
    assert_eq!(
        settings(PicView {
            vector_base: 0x70,
            level_triggered: true,
            single: true,
            auto_eoi: true,
            rotate_on_auto_eoi: true,
            special_mask: true,
            highest_priority: 5,
            reads_in_service: true,
            poll: true,
            awaiting_icw: Some(4),
            ..as_the_bios_leaves_it()
        }),
        text(&[
            ("Vector base", "70h"),
            ("Triggering", "level"),
            ("End of interrupt", "automatic, rotating"),
            ("Nesting", "special mask"),
            ("Priority", "IRQ5 highest"),
            ("Port 20h reads", "ISR"),
            ("Set up for", "a single controller"),
            ("Initialisation", "waiting for ICW4"),
            ("Poll", "waiting for its read"),
        ])
    );
}

/// After the BIOS has booted, the panel shows the vector base 08h, the
/// timer and keyboard lines unmasked and every other line masked, with no
/// request and nothing in service: the mask FCh.
#[test]
fn after_the_bios_the_timer_and_the_keyboard_are_unmasked() {
    let (shell, _keys) = panel();
    let screen = shell.screen();
    assert_eq!(
        screen.row("Vector base"),
        ["Vector base", "08h", "Triggering", "edge"]
    );
    assert_eq!(
        screen.row("IRQ0"),
        ["IRQ0", "timer", "08h", CLEAR, CLEAR, "unmasked"]
    );
    assert_eq!(
        screen.row("IRQ1"),
        ["IRQ1", "keyboard", "09h", CLEAR, CLEAR, "unmasked"]
    );
    for irq in 2..8 {
        let row = screen.row(&format!("IRQ{irq}"));
        assert_eq!(row.last(), Some(&"masked"), "{row:?}");
    }
    assert_eq!(
        screen.row("Line"),
        ["Line", "Device", "Vector", "IRR 00h", "ISR 00h", "IMR FCh"]
    );
}

/// A key the guest is taking shows on the keyboard's line: its request
/// was acknowledged, and IRQ1 is in service while INT 09h runs.
#[test]
fn a_key_being_taken_puts_the_keyboard_line_in_service() {
    let (mut shell, keys) = panel();
    keys.send(EmulatorEvent::Scancode(0x1E)).unwrap();
    // The keyboard is polled, IRQ1 raised and taken, in one pass.
    shell.target.runtime.run(1);
    let screen = shell.frame(Vec::new());
    assert_eq!(
        screen.row("IRQ1"),
        ["IRQ1", "keyboard", "09h", CLEAR, "in service", "unmasked"]
    );
    assert_eq!(
        screen.row("Line"),
        ["Line", "Device", "Vector", "IRR 00h", "ISR 02h", "IMR FCh"]
    );
}
