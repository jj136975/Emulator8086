//! The Keyboard panel: what it reads in port A, how it words the state of
//! the interface, and what it shows as the BIOS leaves the keyboard and
//! while keys wait for the guest.

use super::*;
use crate::gui::overlay::testing::{Shell, waiting_for_a_key};
use emu8086_core::io::keyboard::EmulatorEvent;

/// The panel's title, which the View menu and its window show.
const TITLE: &str = "Keyboard (8255)";

/// The interface as POST leaves it: port B 48h, nothing in port A.
fn after_post() -> KeyboardView {
    KeyboardView {
        port_a: 0x00,
        port_b: 0x48,
        awaiting_acknowledge: false,
        held_clear: false,
        clock_held_low: false,
        reset_pending: false,
        queued: 0,
    }
}

/// The words of each part of `view`'s state, without their colours.
fn words(view: &KeyboardView) -> Vec<(&'static str, String)> {
    state(view)
        .into_iter()
        .map(|(name, value)| (name, value.text().to_string()))
        .collect()
}

/// Port A is read as a scancode of set 1: a make code, a break code with
/// the key it releases, a prefix; a zero that waits for no acknowledge is
/// an empty port.
#[test]
fn port_a_is_read_as_a_scancode() {
    assert_eq!(scancode(0x1E, true), "1Eh, make");
    assert_eq!(scancode(0x9E, true), "9Eh, break of 1Eh");
    assert_eq!(scancode(0xE0, true), "E0h, prefix");
    assert_eq!(scancode(0x00, false), "00h, empty");
}

/// The state is worded part by part: as POST leaves it, and while a key
/// waits for its acknowledge behind a held clock with a reset to come.
#[test]
fn the_state_is_worded_part_by_part() {
    let text = |pairs: &[(&'static str, &str)]| -> Vec<(&'static str, String)> {
        pairs
            .iter()
            .map(|(name, value)| (*name, value.to_string()))
            .collect()
    };
    assert_eq!(
        words(&after_post()),
        text(&[
            ("Port A (60h)", "00h, empty"),
            ("Acknowledge", "not awaited"),
            ("Queued", "nothing"),
            ("Keyboard", "enabled"),
            ("Clock", "running"),
            ("Port B (61h)", "48h"),
        ])
    );
    let busy = KeyboardView {
        port_a: 0x1E,
        port_b: 0xC8,
        awaiting_acknowledge: true,
        held_clear: true,
        clock_held_low: true,
        reset_pending: true,
        queued: 3,
    };
    assert_eq!(
        words(&busy),
        text(&[
            ("Port A (60h)", "1Eh, make"),
            ("Acknowledge", "awaited"),
            ("Queued", "3 scancodes"),
            ("Keyboard", "held clear"),
            ("Clock", "held low"),
            ("Port B (61h)", "C8h"),
            ("Reset", "AAh due at the next poll"),
        ])
    );
    let one = KeyboardView {
        queued: 1,
        ..after_post()
    };
    assert_eq!(words(&one)[2], ("Queued", "1 scancode".to_string()));
}

/// As POST leaves it, port B holds 48h: the keyboard clock enabled (PB6)
/// and the high switches selected (PB3), every other bit clear; nothing
/// waits in port A.
#[test]
fn after_post_port_b_holds_48h() {
    let (runtime, _keys) = waiting_for_a_key();
    let shell = Shell::with_panel(runtime, TITLE);
    let screen = shell.screen();
    let row = screen.row("Port A (60h)");
    assert_eq!(row.get(..2), Some(&["Port A (60h)", "00h, empty"][..]));
    for (bit, name, value) in [
        ("PB0", "timer 2 gate", "0"),
        ("PB3", "read high switches", "1"),
        ("PB6", "keyboard clock enabled", "1"),
        ("PB7", "keyboard held clear", "0"),
    ] {
        let row = screen.row(bit);
        assert!(row.ends_with(&[bit, name, value]), "{row:?}");
    }
}

/// Keys typed while the guest waits: the machine's next poll loads the
/// first into port A, where it waits for INT 09h's acknowledge, and the
/// others queue behind it.
#[test]
fn a_key_waits_in_port_a_and_the_others_queue() {
    let (runtime, keys) = waiting_for_a_key();
    let mut shell = Shell::with_panel(runtime, TITLE);
    for code in [0x1E, 0x30, 0x2E] {
        keys.send(EmulatorEvent::Scancode(code)).unwrap();
    }
    shell.target.runtime.run(1);
    let screen = shell.frame(Vec::new());
    for (name, value) in [
        ("Port A (60h)", "1Eh, make"),
        ("Acknowledge", "awaited"),
        ("Queued", "2 scancodes"),
    ] {
        let row = screen.row(name);
        assert_eq!(row.get(..2), Some(&[name, value][..]), "{row:?}");
    }
}
