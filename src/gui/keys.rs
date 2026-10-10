//! The guest's keyboard as the window and the on-screen keyboard press it.
//! `physical_key_to_scancode` is the key map: a winit physical key to the
//! make code of the key in the same place on the PC/XT keyboard. The
//! guest's keyboard driver turns that code into a character, so the host's
//! keyboard layout plays no part. A key of the overlay's on-screen keyboard
//! comes as its make code already. `GuestKeys` sends the codes to the
//! machine and remembers which keys the guest holds, and who holds each:
//! the host's keyboard, or the on-screen one.
//!
//! The invariants: a key maps to one make code below 80h, or to nothing.
//! The break is the make with bit 7 set, and an XT keyboard knows no E0
//! prefix: a key that later keyboards added beside an XT key (right Ctrl,
//! the cursor block, keypad Enter) shares its code. And the guest gets the
//! break of a key if and only if it got its make, whichever keyboard
//! pressed it: a key it holds is let go when its keyboard releases it, or
//! by `release_screen` (the on-screen keys) or `release_all` (every key),
//! whichever comes first, and a release of a key it does not hold is not
//! passed on.

use std::sync::mpsc::Sender;

use winit::event::ElementState;
use winit::keyboard::{KeyCode, PhysicalKey};

use emu8086_core::io::keyboard::EmulatorEvent;

/// Who holds a key down in the guest.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Holder {
    /// A key of the host's keyboard.
    Host(PhysicalKey),
    /// A key of the on-screen keyboard, which has each make code once.
    Screen,
}

/// Sends the guest its key presses and releases, down the channel the
/// machine's keyboard controller reads.
pub struct GuestKeys {
    events: Sender<EmulatorEvent>,
    /// The keys the guest holds, in the order they went down, each with
    /// who holds it and the make code it was sent. Two of them can share a
    /// code (the two Ctrl keys of the host, a key of the host and the same
    /// key on the screen): each is pressed and released for itself.
    held: Vec<(Holder, u8)>,
}

impl GuestKeys {
    /// `events` is the sender of the channel the machine was built with.
    pub fn new(events: Sender<EmulatorEvent>) -> Self {
        Self {
            events,
            held: Vec::new(),
        }
    }

    /// A key of the host went down or came up. `taken` says that the
    /// overlay kept the event for itself: a press it kept is not the
    /// guest's, while a release reaches the guest whoever kept it, if the
    /// guest holds the key. A key without a place on the XT keyboard is
    /// not the guest's either. A key held down arrives pressed again and
    /// again, and is sent again: an XT keyboard repeats the same way.
    pub fn key(&mut self, key: PhysicalKey, state: ElementState, taken: bool) {
        let Some(make) = physical_key_to_scancode(key) else {
            return;
        };
        self.press_or_release(Holder::Host(key), make, state, taken);
    }

    /// A key of the on-screen keyboard went down or came up: `make` is its
    /// make code. The rules are a host key's: a press the overlay took,
    /// because it has the keyboard, is not the guest's; a release reaches
    /// the guest if it holds the key; a press of a key held is sent again,
    /// as a repeat.
    pub fn screen_key(&mut self, make: u8, state: ElementState, taken: bool) {
        self.press_or_release(Holder::Screen, make, state, taken);
    }

    fn press_or_release(&mut self, holder: Holder, make: u8, state: ElementState, taken: bool) {
        let held = self.held.iter().position(|held| *held == (holder, make));
        match state {
            ElementState::Pressed if taken => {}
            ElementState::Pressed => {
                if held.is_none() {
                    self.held.push((holder, make));
                }
                self.send(EmulatorEvent::Scancode(make));
            }
            ElementState::Released => {
                if let Some(held) = held {
                    self.held.remove(held);
                    self.send(EmulatorEvent::ScanRelease(make | 0x80));
                }
            }
        }
    }

    /// Lets go of the keys of the on-screen keyboard the guest holds. For
    /// when that keyboard is gone from the screen: nothing there holds a
    /// key anymore, and its releases will not come.
    pub fn release_screen(&mut self) {
        self.release_where(|holder| holder == Holder::Screen);
    }

    /// Lets go of every key the guest holds, the host's and the on-screen
    /// keyboard's. For when the releases of the keys will not reach the
    /// guest anymore, as when the window loses the keyboard or the overlay
    /// takes it: a key held then would stay down in the guest.
    pub fn release_all(&mut self) {
        self.release_where(|_| true);
    }

    /// Lets go of the keys held by a holder `which` picks, in the order
    /// they went down.
    fn release_where(&mut self, which: impl Fn(Holder) -> bool) {
        let (released, kept): (Vec<_>, Vec<_>) = std::mem::take(&mut self.held)
            .into_iter()
            .partition(|(holder, _)| which(*holder));
        self.held = kept;
        for (_, make) in released {
            self.send(EmulatorEvent::ScanRelease(make | 0x80));
        }
    }

    fn send(&self, event: EmulatorEvent) {
        // The other end is the machine's keyboard, which lives as long as
        // the window does: there is nobody to tell of a failure.
        let _ = self.events.send(event);
    }
}

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
#[path = "keys_tests.rs"]
mod tests;
