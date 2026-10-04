//! The key map, and what `GuestKeys` sends down its channel: the tests read
//! the receiving end, as the machine's keyboard controller would.

use std::sync::mpsc::{Receiver, channel};

use super::*;
use EmulatorEvent::{ScanRelease, Scancode};

fn scancode(code: KeyCode) -> Option<u8> {
    physical_key_to_scancode(PhysicalKey::Code(code))
}

/// Every key of the numeric keypad, Enter and `/` included, sends its XT
/// scancode, and so does the 102nd key of ISO keyboards.
#[test]
fn keypad_and_102nd_key_send_their_scancodes() {
    let expected = [
        (KeyCode::Numpad7, 0x47),
        (KeyCode::Numpad8, 0x48),
        (KeyCode::Numpad9, 0x49),
        (KeyCode::NumpadSubtract, 0x4A),
        (KeyCode::Numpad4, 0x4B),
        (KeyCode::Numpad5, 0x4C),
        (KeyCode::Numpad6, 0x4D),
        (KeyCode::NumpadAdd, 0x4E),
        (KeyCode::Numpad1, 0x4F),
        (KeyCode::Numpad2, 0x50),
        (KeyCode::Numpad3, 0x51),
        (KeyCode::Numpad0, 0x52),
        (KeyCode::NumpadDecimal, 0x53),
        (KeyCode::NumpadMultiply, 0x37),
        (KeyCode::NumpadEnter, 0x1C),
        (KeyCode::NumpadDivide, 0x35),
        (KeyCode::IntlBackslash, 0x56),
    ];
    for (code, xt) in expected {
        assert_eq!(scancode(code), Some(xt), "{code:?}");
    }
}

#[test]
fn cursor_keys_share_the_keypad_scancodes() {
    assert_eq!(scancode(KeyCode::ArrowUp), scancode(KeyCode::Numpad8));
    assert_eq!(scancode(KeyCode::Home), scancode(KeyCode::Numpad7));
    assert_eq!(scancode(KeyCode::Delete), scancode(KeyCode::NumpadDecimal));
}

#[test]
fn keys_the_83_key_keyboard_lacks_send_nothing() {
    for code in [
        KeyCode::F11,
        KeyCode::F12,
        KeyCode::PrintScreen,
        KeyCode::Pause,
    ] {
        assert_eq!(scancode(code), None, "{code:?}");
    }
}

/// A guest keyboard, and the end of its channel the machine would read.
fn guest_keys() -> (GuestKeys, Receiver<EmulatorEvent>) {
    let (events, sent) = channel();
    (GuestKeys::new(events), sent)
}

fn press(keys: &mut GuestKeys, code: KeyCode) {
    keys.key(PhysicalKey::Code(code), ElementState::Pressed);
}

fn release(keys: &mut GuestKeys, code: KeyCode) {
    keys.key(PhysicalKey::Code(code), ElementState::Released);
}

/// What was sent since the last look.
fn sent(events: &Receiver<EmulatorEvent>) -> Vec<EmulatorEvent> {
    events.try_iter().collect()
}

const NOTHING: [EmulatorEvent; 0] = [];

/// A key goes to the guest as its make code when it goes down and as the
/// make with bit 7 set when it comes up. Held down, it is sent again with
/// every repeat of the host, and released once.
#[test]
fn a_key_is_sent_as_make_then_break() {
    let (mut keys, events) = guest_keys();
    press(&mut keys, KeyCode::KeyA);
    press(&mut keys, KeyCode::KeyA);
    release(&mut keys, KeyCode::KeyA);
    assert_eq!(
        sent(&events),
        [Scancode(0x1E), Scancode(0x1E), ScanRelease(0x9E)]
    );

    release(&mut keys, KeyCode::KeyA);
    assert_eq!(sent(&events), NOTHING, "it is up already");
}

/// The release of a key the guest never saw go down is not passed on, and
/// neither is a key the XT keyboard does not have.
#[test]
fn a_release_without_its_press_and_an_unknown_key_send_nothing() {
    let (mut keys, events) = guest_keys();
    release(&mut keys, KeyCode::KeyA);
    press(&mut keys, KeyCode::F11);
    release(&mut keys, KeyCode::F11);
    assert_eq!(sent(&events), NOTHING);
}

/// `release_all` lets go of every key the guest holds, each once: the
/// releases of the host that arrive afterwards find nothing left to release.
#[test]
fn release_all_lets_go_of_every_held_key_once() {
    let (mut keys, events) = guest_keys();
    press(&mut keys, KeyCode::ShiftLeft);
    press(&mut keys, KeyCode::KeyA);
    press(&mut keys, KeyCode::KeyB);
    release(&mut keys, KeyCode::KeyB);
    assert_eq!(
        sent(&events),
        [
            Scancode(0x2A),
            Scancode(0x1E),
            Scancode(0x30),
            ScanRelease(0xB0)
        ]
    );

    keys.release_all();
    assert_eq!(sent(&events), [ScanRelease(0xAA), ScanRelease(0x9E)]);

    release(&mut keys, KeyCode::KeyA);
    release(&mut keys, KeyCode::ShiftLeft);
    keys.release_all();
    assert_eq!(sent(&events), NOTHING);

    // The keyboard goes on working afterwards.
    press(&mut keys, KeyCode::KeyA);
    assert_eq!(sent(&events), [Scancode(0x1E)]);
}

/// Two keys of the host that share an XT code are held and released each
/// for itself: the guest gets as many breaks as it got keys.
#[test]
fn two_keys_with_one_code_are_released_one_by_one() {
    let (mut keys, events) = guest_keys();
    press(&mut keys, KeyCode::ControlLeft);
    press(&mut keys, KeyCode::ControlRight);
    release(&mut keys, KeyCode::ControlLeft);
    assert_eq!(
        sent(&events),
        [Scancode(0x1D), Scancode(0x1D), ScanRelease(0x9D)]
    );
    keys.release_all();
    assert_eq!(sent(&events), [ScanRelease(0x9D)]);
}
