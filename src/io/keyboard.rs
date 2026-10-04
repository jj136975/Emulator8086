//! The PC/XT keyboard interface on the 8255 PPI (ports 60h-63h) and the
//! terminal frontend's key reader. A frontend sends `EmulatorEvent`s down
//! a channel; `KeyboardController::poll` loads their scancodes into port A
//! one at a time and raises IRQ1 for each. Port B carries the acknowledge,
//! the keyboard clock and timer 2's gate and speaker bits; port C reads
//! timer 2's output back. `KeyTranslator` turns the terminal's key events,
//! which name characters rather than keys, into US keyboard scancodes.
//!
//! The invariant: port A holds a byte until the guest acknowledges it by
//! taking port B bit 7 high and back low, as the BIOS's INT 09h handler
//! does. The acknowledge withdraws IRQ1, and only then is the next byte
//! loaded and IRQ1 raised again: no byte is replaced before the guest has
//! acknowledged it.

use crate::io::bus::IoDevice;
use crate::io::pic::Pic;
use crate::io::pit::Pit;
use crate::vm::cpu::Cpu;
// The terminal frontend reads keys through crossterm; the GUI frontend gets
// them from its window, so none of that is compiled into a GUI build.
#[cfg(not(feature = "gui"))]
use crossterm::event::{self, Event};
#[cfg(any(not(feature = "gui"), test))]
use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use log::debug;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use std::sync::mpsc;
#[cfg(not(feature = "gui"))]
use std::thread;

// `EnterCLI` and `Quit` are only produced by the terminal frontend.
#[cfg_attr(feature = "gui", allow(dead_code))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmulatorEvent {
    Scancode(u8),    // make code
    ScanRelease(u8), // break code (scancode | 0x80)
    EnterCLI,        // open the F12 monitor
    DumpVga,         // dump VGA memory to file (F11 in terminal mode, F12 in GUI mode)
    Quit,
}

/// IBM PC/XT 8255 PPI keyboard controller.
///
/// Models the 8255 Programmable Peripheral Interface as used in the original
/// IBM PC and PC/XT for keyboard input, speaker control, and system
/// configuration switches.
///
/// Port mapping:
///   0x60 — PPI Port A: keyboard scancode byte (read-only)
///   0x61 — PPI Port B: control register (read/write)
///   0x62 — PPI Port C: system status (read-only)
///   0x63 — PPI command port (ignored, returns 0xFF)
pub struct KeyboardController {
    /// Keyboard shift register — holds one scancode byte, like real hardware.
    kb_byte: u8,
    /// Whether the keyboard can accept new scancodes into the shift register.
    kb_enabled: bool,
    /// Shift register was cleared via Port B acknowledge; ready for next byte.
    ksr_cleared: bool,

    /// PPI Port B state register.
    port_b: u8,

    /// Events from the frontend: its window, or the terminal reader thread.
    rx: mpsc::Receiver<EmulatorEvent>,
    /// Queue of scancodes waiting to be loaded into the shift register.
    pending: VecDeque<u8>,

    /// Reference to the PIT for channel 2 gate control and output reading.
    pit: Option<Rc<RefCell<Pit>>>,

    /// Port B bit 6 pulled keyboard clock low (inhibiting keyboard).
    kb_clock_low: bool,
    /// A keyboard reset sequence was detected; need to send 0xAA response.
    kb_reset_pending: bool,
}

impl KeyboardController {
    /// Create with the receiving end of an event channel. The frontend sends
    /// `EmulatorEvent` values through the other end: from its window in GUI
    /// builds, from `spawn_terminal_reader` otherwise.
    pub fn with_receiver(rx: mpsc::Receiver<EmulatorEvent>) -> Self {
        Self {
            kb_byte: 0,
            kb_enabled: true,
            ksr_cleared: true,
            port_b: 0,
            rx,
            pending: VecDeque::new(),
            pit: None,
            kb_clock_low: false,
            kb_reset_pending: false,
        }
    }

    /// Starts the thread that reads keys from the terminal and sends them
    /// through `tx` to the controller holding the receiving end.
    #[cfg(not(feature = "gui"))]
    pub fn spawn_terminal_reader(tx: mpsc::Sender<EmulatorEvent>) {
        thread::spawn(move || {
            let mut translator = KeyTranslator::new(cfg!(windows));
            loop {
                let events = match event::read() {
                    Ok(Event::Key(key)) => translator.translate(&key),
                    // Keys let go while the window is out of focus are
                    // never reported.
                    Ok(Event::FocusLost) => translator.release_all(),
                    _ => continue,
                };
                let quit = events.contains(&EmulatorEvent::Quit);
                for event in events {
                    let _ = tx.send(event);
                }
                if quit {
                    break;
                }
            }
        });
    }

    pub fn set_pit(&mut self, pit: Rc<RefCell<Pit>>) {
        self.pit = Some(pit);
    }

    /// Call this periodically from the main loop (e.g. every ~1000 instructions).
    /// Drains the event channel and loads the next scancode into the shift
    /// register when it is clear and the keyboard is enabled.
    /// Returns Some(event) for control events (Quit, EnterCLI, DumpVga).
    pub fn poll(&mut self, pic: &mut Pic) -> Option<EmulatorEvent> {
        while let Ok(event) = self.rx.try_recv() {
            match event {
                EmulatorEvent::Quit | EmulatorEvent::EnterCLI | EmulatorEvent::DumpVga => {
                    return Some(event);
                }
                EmulatorEvent::Scancode(sc) | EmulatorEvent::ScanRelease(sc) => {
                    debug!("Received scancode: 0x{:02X}", sc);
                    self.pending.push_back(sc);
                }
            }
        }

        if self.kb_reset_pending {
            self.kb_reset_pending = false;
            self.pending.push_front(0xAA); // Self-test passed
        }

        if self.kb_enabled
            && self.ksr_cleared
            && let Some(sc) = self.pending.pop_front()
        {
            debug!(
                "Loading scancode 0x{:02X} into shift register, raising IRQ1",
                sc
            );
            self.kb_byte = sc;
            self.ksr_cleared = false;
            pic.request_interrupt(1);
        }

        None
    }
}

impl IoDevice for KeyboardController {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            // Port A — keyboard scancode byte
            0x60 => self.kb_byte,

            // Port B — control register with dynamic status bits
            0x61 => {
                let mut val = self.port_b;

                // Bit 4: refresh toggle (flip on each read, used for RAM
                // timing loops and POST diagnostics)
                self.port_b ^= 0x10;
                val = (val & !0x10) | (self.port_b & 0x10);

                // Bit 5: PIT channel 2 output
                if let Some(pit) = &self.pit {
                    if pit.borrow().channel2_output() {
                        val |= 0x20;
                    } else {
                        val &= !0x20;
                    }
                }

                val
            }

            // Port C — system status (read-only)
            0x62 => {
                let mut val: u8 = 0;
                let timer_output = self
                    .pit
                    .as_ref()
                    .is_some_and(|pit| pit.borrow().channel2_output());

                // Bit 5: PIT channel 2 output (mirrors port B bit 5)
                if timer_output {
                    val |= 0x20;
                }

                // Bit 4: speaker output = PIT ch2 output AND port_b bit 1
                if timer_output && self.port_b & 0x02 != 0 {
                    val |= 0x10;
                }

                // Bits 0-3: DIP switch settings (return 0 — no special config)
                val
            }

            // PPI command port, not used
            0x63 => 0xFF,

            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, cpu: &mut Cpu) {
        match port {
            // Port A is read-only on the PPI in the PC/XT
            0x60 => { /* writes ignored */ }

            // Port B — control register
            0x61 => {
                let old_b = self.port_b;
                self.port_b = value;

                // Bit 7: keyboard clear/acknowledge
                if value & 0x80 != 0 {
                    // Set high: the keyboard is held off
                    self.kb_enabled = false;
                } else if old_b & 0x80 != 0 {
                    // Bit 7 went from high to low — the shift register and
                    // IRQ1 are cleared and the keyboard is enabled again
                    self.kb_enabled = true;
                    self.kb_byte = 0;
                    self.ksr_cleared = true;
                    cpu.pic.clear_interrupt(1);
                }

                // Bit 0: PIT channel 2 gate
                if let Some(pit) = &self.pit {
                    pit.borrow_mut().set_gate(2, value & 0x01 != 0);
                }

                // Bit 6: keyboard clock line
                if value & 0x40 == 0 {
                    // Clock pulled low — inhibit keyboard
                    self.kb_clock_low = true;
                } else if self.kb_clock_low {
                    // Clock was low, now released high — keyboard reset
                    self.kb_clock_low = false;
                    self.kb_reset_pending = true;
                }
            }

            // Port C is read-only
            0x62 => { /* writes ignored */ }

            // PPI command port — not implemented
            0x63 => { /* writes ignored */ }

            _ => {}
        }
    }
}

/// Turns the terminal's key events into XT scancodes and emulator commands.
///
/// A terminal reports Shift, Ctrl and Alt only as flags on other keys, so
/// the translator keeps the emulated keyboard's modifier keys in step with
/// those flags: before each key it presses or releases them as needed.
///
/// Characters are the exception for Shift. The terminal delivers the
/// character that was typed, and which key and Shift state produced it
/// depends on the host's keyboard layout. The emulated keyboard has the US
/// layout, so Shift is held according to where the character sits there.
///
/// The Windows console also reports when a key goes up, and repeats the
/// press of a key that is held. There a key stays down on the emulated
/// keyboard until its release arrives. Other terminals report presses
/// only, so every make is followed at once by its break.
#[cfg(any(not(feature = "gui"), test))]
pub(crate) struct KeyTranslator {
    /// The host is the Windows console.
    windows_console: bool,
    shift: bool,
    ctrl: bool,
    alt: bool,
    /// Scancodes of the keys that are down, oldest first. Only filled when
    /// the host reports releases.
    held: Vec<u8>,
}

#[cfg(any(not(feature = "gui"), test))]
impl KeyTranslator {
    pub(crate) fn new(windows_console: bool) -> Self {
        Self {
            windows_console,
            shift: false,
            ctrl: false,
            alt: false,
            held: Vec::new(),
        }
    }

    /// The events one host key event stands for, in the order to send them.
    pub(crate) fn translate(&mut self, key: &KeyEvent) -> Vec<EmulatorEvent> {
        let pressed = match key.kind {
            KeyEventKind::Press => true,
            KeyEventKind::Release if self.windows_console => false,
            _ => return Vec::new(),
        };

        // Keys the emulator keeps for itself.
        if pressed {
            if key.code == KeyCode::F(12) {
                return vec![EmulatorEvent::EnterCLI];
            }
            if key.code == KeyCode::F(11) {
                return vec![EmulatorEvent::DumpVga];
            }
            if key.modifiers.contains(KeyModifiers::CONTROL) && key.code == KeyCode::Char('c') {
                return vec![EmulatorEvent::Quit];
            }
        }

        let host_shift = key.modifiers.contains(KeyModifiers::SHIFT);
        let mut ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
        let mut alt = key.modifiers.contains(KeyModifiers::ALT);

        // The Windows console reports AltGr as Ctrl+Alt next to the
        // character AltGr produced. A symbol that arrives with both is
        // taken to be such a character and typed without them. It cannot
        // be told apart from a real Ctrl+Alt+symbol, which is the rarer of
        // the two; letters and digits stay Ctrl+Alt combinations.
        if let KeyCode::Char(c) = key.code
            && self.windows_console
            && ctrl
            && alt
            && c != ' '
            && !c.is_ascii_alphanumeric()
        {
            ctrl = false;
            alt = false;
        }

        // The key to press and whether Shift is down while it is pressed.
        let (scancode, shift) = match key.code {
            KeyCode::Char(c) => match char_key(c) {
                Some((scancode, needs_shift)) => {
                    let shift = if c == ' ' {
                        // The same character either way.
                        host_shift
                    } else {
                        // With Ctrl or Alt held the host reports the key's
                        // unshifted character, so its Shift flag counts too.
                        needs_shift || ((ctrl || alt) && host_shift)
                    };
                    (Some(scancode), shift)
                }
                None => (None, host_shift),
            },
            // Shift+Tab has its own key code.
            KeyCode::BackTab => (Some(0x0F), true),
            code => (special_key(code), host_shift),
        };

        let mut events = Vec::new();
        if !pressed {
            // Between keys the emulated Shift is where the host has it.
            self.set_modifiers(&mut events, host_shift, ctrl, alt);
            if let Some(released) = scancode.and_then(|scancode| self.take_held(scancode)) {
                events.push(EmulatorEvent::ScanRelease(released | 0x80));
            }
            return events;
        }

        self.set_modifiers(&mut events, shift, ctrl, alt);
        if let Some(scancode) = scancode {
            events.push(EmulatorEvent::Scancode(scancode));
            if self.windows_console {
                // Down until its release arrives. A repeated press of a key
                // that is already down is a typematic make.
                if !self.held.contains(&scancode) {
                    self.held.push(scancode);
                }
            } else {
                // The terminal does not report releases, so the break
                // follows the make at once.
                events.push(EmulatorEvent::ScanRelease(scancode | 0x80));
                // Shift held only for this character does not stay down.
                self.set_modifiers(&mut events, host_shift, ctrl, alt);
            }
        }
        events
    }

    /// Lets go of every key and modifier that is down on the emulated
    /// keyboard.
    pub(crate) fn release_all(&mut self) -> Vec<EmulatorEvent> {
        let mut events: Vec<EmulatorEvent> = self
            .held
            .drain(..)
            .map(|scancode| EmulatorEvent::ScanRelease(scancode | 0x80))
            .collect();
        self.set_modifiers(&mut events, false, false, false);
        events
    }

    /// Takes the key a release event refers to out of the held keys.
    ///
    /// The character a key types can change while the key is down: on a
    /// layout where Shift moves it to a different US key, letting go of
    /// Shift first makes the release name a key that was never pressed.
    /// The newest key still down is taken in that case; should that be the
    /// wrong one, it is the key the host goes on repeating, which presses
    /// it again.
    fn take_held(&mut self, scancode: u8) -> Option<u8> {
        let index = self
            .held
            .iter()
            .rposition(|&held| held == scancode)
            .or(self.held.len().checked_sub(1))?;
        Some(self.held.remove(index))
    }

    /// Presses or releases Shift, Ctrl and Alt on the emulated keyboard so
    /// that they match the wanted state.
    fn set_modifiers(
        &mut self,
        events: &mut Vec<EmulatorEvent>,
        shift: bool,
        ctrl: bool,
        alt: bool,
    ) {
        for (held, wanted, make) in [
            (&mut self.shift, shift, 0x2A),
            (&mut self.ctrl, ctrl, 0x1D),
            (&mut self.alt, alt, 0x38),
        ] {
            if *held != wanted {
                *held = wanted;
                events.push(if wanted {
                    EmulatorEvent::Scancode(make)
                } else {
                    EmulatorEvent::ScanRelease(make | 0x80)
                });
            }
        }
    }
}

/// Where a printable character sits on the US keyboard: the scancode of
/// its key and whether Shift is held to type it.
#[cfg(any(not(feature = "gui"), test))]
fn char_key(c: char) -> Option<(u8, bool)> {
    // Scancode of the first key of each row, then the characters its keys
    // type without and with Shift.
    const ROWS: [(u8, &str, &str); 4] = [
        (0x02, "1234567890-=", "!@#$%^&*()_+"),
        (0x10, "qwertyuiop[]", "QWERTYUIOP{}"),
        (0x1E, "asdfghjkl;'`", "ASDFGHJKL:\"~"),
        (0x2B, "\\zxcvbnm,./", "|ZXCVBNM<>?"),
    ];

    if c == ' ' {
        return Some((0x39, false));
    }
    for (first, plain, shifted) in ROWS {
        if let Some(index) = plain.find(c) {
            return Some((first + index as u8, false));
        }
        if let Some(index) = shifted.find(c) {
            return Some((first + index as u8, true));
        }
    }
    None
}

/// Scancode of a key that does not type a character.
///
/// The lock keys are not here. Terminals do not report them, and Caps Lock
/// has already been applied to the characters the host delivers.
#[cfg(any(not(feature = "gui"), test))]
fn special_key(code: KeyCode) -> Option<u8> {
    Some(match code {
        KeyCode::Esc => 0x01,
        KeyCode::Backspace => 0x0E,
        KeyCode::Tab => 0x0F,
        KeyCode::Enter => 0x1C,
        KeyCode::F(n @ 1..=10) => 0x3A + n,
        KeyCode::Home => 0x47,
        KeyCode::Up => 0x48,
        KeyCode::PageUp => 0x49,
        KeyCode::Left => 0x4B,
        KeyCode::Right => 0x4D,
        KeyCode::End => 0x4F,
        KeyCode::Down => 0x50,
        KeyCode::PageDown => 0x51,
        KeyCode::Insert => 0x52,
        KeyCode::Delete => 0x53,
        _ => return None,
    })
}

#[cfg(test)]
#[path = "keyboard_tests.rs"]
mod tests;
