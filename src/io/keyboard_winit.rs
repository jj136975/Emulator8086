//! The window frontend's key map (`--features gui`): a winit physical key
//! to the make code of the key in the same place on the PC/XT keyboard.
//! The guest's keyboard driver turns that code into a character, so the
//! host's keyboard layout plays no part.
//!
//! The invariant: a key maps to one make code below 80h, or to nothing.
//! The window loop sends the break as the make with bit 7 set, and an XT
//! keyboard knows no E0 prefix: a key that later keyboards added beside an
//! XT key (right Ctrl, the cursor block, keypad Enter) shares its code.

use winit::keyboard::{KeyCode, PhysicalKey};

/// Map a winit `PhysicalKey` to an IBM PC/XT scancode.
/// Returns `None` for unmapped keys.
pub fn physical_key_to_scancode(key: PhysicalKey) -> Option<u8> {
    match key {
        PhysicalKey::Code(code) => keycode_to_scancode(code),
        _ => None,
    }
}

fn keycode_to_scancode(code: KeyCode) -> Option<u8> {
    match code {
        KeyCode::Escape => Some(0x01),
        KeyCode::Digit1 => Some(0x02),
        KeyCode::Digit2 => Some(0x03),
        KeyCode::Digit3 => Some(0x04),
        KeyCode::Digit4 => Some(0x05),
        KeyCode::Digit5 => Some(0x06),
        KeyCode::Digit6 => Some(0x07),
        KeyCode::Digit7 => Some(0x08),
        KeyCode::Digit8 => Some(0x09),
        KeyCode::Digit9 => Some(0x0A),
        KeyCode::Digit0 => Some(0x0B),
        KeyCode::Minus => Some(0x0C),
        KeyCode::Equal => Some(0x0D),
        KeyCode::Backspace => Some(0x0E),
        KeyCode::Tab => Some(0x0F),
        KeyCode::KeyQ => Some(0x10),
        KeyCode::KeyW => Some(0x11),
        KeyCode::KeyE => Some(0x12),
        KeyCode::KeyR => Some(0x13),
        KeyCode::KeyT => Some(0x14),
        KeyCode::KeyY => Some(0x15),
        KeyCode::KeyU => Some(0x16),
        KeyCode::KeyI => Some(0x17),
        KeyCode::KeyO => Some(0x18),
        KeyCode::KeyP => Some(0x19),
        KeyCode::BracketLeft => Some(0x1A),
        KeyCode::BracketRight => Some(0x1B),
        KeyCode::Enter | KeyCode::NumpadEnter => Some(0x1C),
        KeyCode::ControlLeft | KeyCode::ControlRight => Some(0x1D),
        KeyCode::KeyA => Some(0x1E),
        KeyCode::KeyS => Some(0x1F),
        KeyCode::KeyD => Some(0x20),
        KeyCode::KeyF => Some(0x21),
        KeyCode::KeyG => Some(0x22),
        KeyCode::KeyH => Some(0x23),
        KeyCode::KeyJ => Some(0x24),
        KeyCode::KeyK => Some(0x25),
        KeyCode::KeyL => Some(0x26),
        KeyCode::Semicolon => Some(0x27),
        KeyCode::Quote => Some(0x28),
        KeyCode::Backquote => Some(0x29),
        KeyCode::ShiftLeft => Some(0x2A),
        KeyCode::Backslash => Some(0x2B),
        KeyCode::KeyZ => Some(0x2C),
        KeyCode::KeyX => Some(0x2D),
        KeyCode::KeyC => Some(0x2E),
        KeyCode::KeyV => Some(0x2F),
        KeyCode::KeyB => Some(0x30),
        KeyCode::KeyN => Some(0x31),
        KeyCode::KeyM => Some(0x32),
        KeyCode::Comma => Some(0x33),
        KeyCode::Period => Some(0x34),
        KeyCode::Slash | KeyCode::NumpadDivide => Some(0x35),
        KeyCode::ShiftRight => Some(0x36),
        KeyCode::NumpadMultiply => Some(0x37),
        KeyCode::AltLeft | KeyCode::AltRight => Some(0x38),
        KeyCode::Space => Some(0x39),
        KeyCode::CapsLock => Some(0x3A),
        KeyCode::F1 => Some(0x3B),
        KeyCode::F2 => Some(0x3C),
        KeyCode::F3 => Some(0x3D),
        KeyCode::F4 => Some(0x3E),
        KeyCode::F5 => Some(0x3F),
        KeyCode::F6 => Some(0x40),
        KeyCode::F7 => Some(0x41),
        KeyCode::F8 => Some(0x42),
        KeyCode::F9 => Some(0x43),
        KeyCode::F10 => Some(0x44),
        KeyCode::NumLock => Some(0x45),
        KeyCode::ScrollLock => Some(0x46),
        // The 83-key keyboard has one cursor/number pad: the dedicated
        // cursor keys of later keyboards send the same codes as the keypad.
        KeyCode::Home | KeyCode::Numpad7 => Some(0x47),
        KeyCode::ArrowUp | KeyCode::Numpad8 => Some(0x48),
        KeyCode::PageUp | KeyCode::Numpad9 => Some(0x49),
        KeyCode::NumpadSubtract => Some(0x4A),
        KeyCode::ArrowLeft | KeyCode::Numpad4 => Some(0x4B),
        KeyCode::Numpad5 => Some(0x4C),
        KeyCode::ArrowRight | KeyCode::Numpad6 => Some(0x4D),
        KeyCode::NumpadAdd => Some(0x4E),
        KeyCode::End | KeyCode::Numpad1 => Some(0x4F),
        KeyCode::ArrowDown | KeyCode::Numpad2 => Some(0x50),
        KeyCode::PageDown | KeyCode::Numpad3 => Some(0x51),
        KeyCode::Insert | KeyCode::Numpad0 => Some(0x52),
        KeyCode::Delete | KeyCode::NumpadDecimal => Some(0x53),
        // The extra key of 102-key (ISO) keyboards, left of Z.
        KeyCode::IntlBackslash => Some(0x56),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
