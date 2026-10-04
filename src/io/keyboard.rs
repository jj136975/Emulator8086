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
    EnterCLI,        // signal to enter command line interface
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

    /// Receiver for events from the background keyboard thread.
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
    /// Create with an externally-provided event channel.
    /// The caller is responsible for sending `EmulatorEvent` values.
    /// Used by the GUI backend where winit provides keyboard events.
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

    /// Create with built-in crossterm keyboard thread (terminal mode).
    #[cfg(not(feature = "gui"))]
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();

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

        Self::with_receiver(rx)
    }

    pub fn set_pit(&mut self, pit: Rc<RefCell<Pit>>) {
        self.pit = Some(pit);
    }

    /// Call this periodically from the main loop (e.g. every ~1000 instructions).
    /// Drains the event channel and loads the next scancode into the shift
    /// register when it is clear and the keyboard is enabled.
    /// Returns Some(event) for control events (Quit, EnterCLI, DumpVga).
    pub fn poll(&mut self, pic: &mut Pic) -> Option<EmulatorEvent> {
        // Drain channel into pending queue
        while let Ok(event) = self.rx.try_recv() {
            match event {
                EmulatorEvent::Quit | EmulatorEvent::EnterCLI | EmulatorEvent::DumpVga => {
                    return Some(event)
                }
                EmulatorEvent::Scancode(sc) | EmulatorEvent::ScanRelease(sc) => {
                    debug!("Received scancode: 0x{:02X}", sc);
                    self.pending.push_back(sc);
                }
            }
        }

        // Handle keyboard reset response
        if self.kb_reset_pending {
            self.kb_reset_pending = false;
            self.pending.push_front(0xAA); // Self-test passed
        }

        // Load next scancode if shift register is clear and keyboard is enabled
        if self.kb_enabled && self.ksr_cleared {
            if let Some(sc) = self.pending.pop_front() {
                debug!(
                    "Loading scancode 0x{:02X} into shift register, raising IRQ1",
                    sc
                );
                self.kb_byte = sc;
                self.ksr_cleared = false;
                pic.request_interrupt(1); // Pulse IRQ1
            }
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

            // PPI command port — not used, return 0xFF
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
        if let KeyCode::Char(c) = key.code {
            if self.windows_console && ctrl && alt && c != ' ' && !c.is_ascii_alphanumeric() {
                ctrl = false;
                alt = false;
            }
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
    fn set_modifiers(&mut self, events: &mut Vec<EmulatorEvent>, shift: bool, ctrl: bool, alt: bool) {
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
mod tests {
    use super::*;
    use crate::io::pic::PicDevice;
    use crate::vm::runtime::Runtime;

    /// A controller fed through a channel, with the machine its port
    /// handlers and its poll are handed.
    struct Bench {
        keyboard: KeyboardController,
        keys: mpsc::Sender<EmulatorEvent>,
        rt: Runtime,
    }

    impl Bench {
        fn new() -> Self {
            let (keys, rx) = mpsc::channel();
            Self {
                keyboard: KeyboardController::with_receiver(rx),
                keys,
                rt: Runtime::new_test(),
            }
        }

        fn poll(&mut self) {
            self.keyboard.poll(&mut self.rt.cpu.pic);
        }

        fn out(&mut self, port: u16, value: u8) {
            self.keyboard.port_out_byte(port, value, &mut self.rt.cpu);
        }

        fn inp(&mut self, port: u16) -> u8 {
            self.keyboard.port_in_byte(port, &mut self.rt.cpu)
        }

        /// IRQ1 is waiting in the interrupt controller's request register.
        fn irq1_requested(&mut self) -> bool {
            PicDevice.port_in_byte(0x20, &mut self.rt.cpu) & 0x02 != 0
        }
    }

    #[test]
    fn scancode_is_held_until_port_b_bit_7_is_pulsed() {
        let mut bench = Bench::new();
        bench.keys.send(EmulatorEvent::Scancode(0x1E)).unwrap();
        bench.keys.send(EmulatorEvent::ScanRelease(0x9E)).unwrap();

        bench.poll();
        assert_eq!(bench.inp(0x60), 0x1E);
        assert!(bench.irq1_requested());
        bench.poll();
        assert_eq!(bench.inp(0x60), 0x1E, "the next byte waits for the acknowledge");

        bench.out(0x61, 0x80);
        bench.poll();
        assert_eq!(bench.inp(0x60), 0x1E, "nothing is loaded while bit 7 is high");
        bench.out(0x61, 0x00);
        assert_eq!(bench.inp(0x60), 0x00);
        assert!(!bench.irq1_requested());

        bench.poll();
        assert_eq!(bench.inp(0x60), 0x9E);
        assert!(bench.irq1_requested());
    }

    #[test]
    fn releasing_the_clock_line_makes_the_keyboard_answer_aa() {
        let mut bench = Bench::new();
        bench.out(0x61, 0x08); // clock line low
        bench.out(0x61, 0xC8); // clock released, keyboard held clear
        bench.out(0x61, 0x48); // keyboard enabled
        bench.poll();
        assert_eq!(bench.inp(0x60), 0xAA);
        assert!(bench.irq1_requested());
    }

    #[test]
    fn ports_b_and_c_report_timer_channel_2() {
        let mut bench = Bench::new();
        let pit = Rc::new(RefCell::new(Pit::new()));
        bench.keyboard.set_pit(Rc::clone(&pit));
        // Channel 2 in mode 3: the output is high while the gate is low.
        pit.borrow_mut().port_out_byte(0x43, 0xB6, &mut bench.rt.cpu);

        assert_eq!(bench.inp(0x61) & 0x20, 0x20);
        assert_eq!(bench.inp(0x62), 0x20, "speaker data is gated off");
        bench.out(0x61, 0x02); // speaker data enabled
        assert_eq!(bench.inp(0x62), 0x30);
    }

    // --- terminal key translation ---

    /// A translator for a terminal that reports key presses only.
    fn terminal() -> KeyTranslator {
        KeyTranslator::new(false)
    }

    fn press(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
        KeyEvent::new(code, modifiers)
    }

    fn release(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
        KeyEvent::new_with_kind(code, modifiers, KeyEventKind::Release)
    }

    /// The make and break codes in `events`, as the bytes the guest reads.
    fn scancodes(events: Vec<EmulatorEvent>) -> Vec<u8> {
        events
            .into_iter()
            .map(|event| match event {
                EmulatorEvent::Scancode(code) | EmulatorEvent::ScanRelease(code) => code,
                other => panic!("not a scancode: {other:?}"),
            })
            .collect()
    }

    #[test]
    fn plain_key_sends_its_make_and_break() {
        let mut keys = terminal();
        assert_eq!(
            keys.translate(&press(KeyCode::Char('a'), KeyModifiers::NONE)),
            [EmulatorEvent::Scancode(0x1E), EmulatorEvent::ScanRelease(0x9E)]
        );
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Enter, KeyModifiers::NONE))), [0x1C, 0x9C]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::F(1), KeyModifiers::NONE))), [0x3B, 0xBB]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Left, KeyModifiers::NONE))), [0x4B, 0xCB]);
    }

    #[test]
    fn modifier_keys_follow_the_flags_on_each_key() {
        let mut keys = terminal();
        // Shift goes down with the first shifted key and stays down ...
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Up, KeyModifiers::SHIFT))), [0x2A, 0x48, 0xC8]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Up, KeyModifiers::SHIFT))), [0x48, 0xC8]);
        // ... until a key arrives without it.
        assert_eq!(
            scancodes(keys.translate(&press(KeyCode::Char('s'), KeyModifiers::CONTROL))),
            [0xAA, 0x1D, 0x1F, 0x9F]
        );
        assert_eq!(
            scancodes(keys.translate(&press(KeyCode::Char('x'), KeyModifiers::ALT))),
            [0x9D, 0x38, 0x2D, 0xAD]
        );
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Enter, KeyModifiers::NONE))), [0xB8, 0x1C, 0x9C]);
    }

    #[test]
    fn emulator_keys_are_not_passed_to_the_guest() {
        let mut keys = terminal();
        assert_eq!(keys.translate(&press(KeyCode::F(12), KeyModifiers::NONE)), [EmulatorEvent::EnterCLI]);
        assert_eq!(keys.translate(&press(KeyCode::F(11), KeyModifiers::SHIFT)), [EmulatorEvent::DumpVga]);
        assert_eq!(keys.translate(&press(KeyCode::Char('c'), KeyModifiers::CONTROL)), [EmulatorEvent::Quit]);
        // None of them touched the emulated modifier keys.
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('a'), KeyModifiers::NONE))), [0x1E, 0x9E]);
    }

    #[test]
    fn releases_and_unmapped_keys_send_no_scancode() {
        let mut keys = terminal();
        assert_eq!(keys.translate(&release(KeyCode::Char('a'), KeyModifiers::NONE)), []);
        assert_eq!(keys.translate(&press(KeyCode::Pause, KeyModifiers::NONE)), []);
        assert_eq!(keys.translate(&press(KeyCode::Char('\u{e9}'), KeyModifiers::NONE)), []);
    }

    /// Regression: Shift came from the host's modifier flag, so a character
    /// that the host typed with a different Shift state than the US layout
    /// needs came out as the other character on its key.
    #[test]
    fn shift_follows_the_character_typed() {
        // Typed with Shift on the host (digits on an AZERTY keyboard) but
        // without it on the US layout. Shift is released for the key and
        // goes back down afterwards, because the host still holds it.
        let mut keys = terminal();
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('1'), KeyModifiers::SHIFT))), [0x02, 0x82, 0x2A]);
        assert_eq!(
            scancodes(keys.translate(&press(KeyCode::Char('.'), KeyModifiers::SHIFT))),
            [0xAA, 0x34, 0xB4, 0x2A]
        );

        // Typed without Shift on the host (keypad keys, Caps Lock, terminals
        // that only flag Shift for capital letters) but with it on the US
        // layout. Shift is pressed for the key only.
        let mut keys = terminal();
        for (c, scancode) in [('*', 0x09), ('+', 0x0D), (':', 0x27), ('!', 0x02), ('A', 0x1E), ('~', 0x29)] {
            assert_eq!(
                scancodes(keys.translate(&press(KeyCode::Char(c), KeyModifiers::NONE))),
                [0x2A, scancode, scancode | 0x80, 0xAA],
                "{c:?}"
            );
        }
    }

    #[test]
    fn shift_matching_the_host_is_left_alone() {
        let mut keys = terminal();
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('!'), KeyModifiers::SHIFT))), [0x2A, 0x02, 0x82]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('Q'), KeyModifiers::SHIFT))), [0x10, 0x90]);
        // A space is the same character either way: the host decides.
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char(' '), KeyModifiers::SHIFT))), [0x39, 0xB9]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('q'), KeyModifiers::NONE))), [0xAA, 0x10, 0x90]);
    }

    /// With Ctrl or Alt held the host reports the key's unshifted character,
    /// so its Shift flag counts as well.
    #[test]
    fn shift_flag_is_honoured_in_ctrl_and_alt_combinations() {
        let mut keys = terminal();
        assert_eq!(
            scancodes(keys.translate(&press(KeyCode::Char('2'), KeyModifiers::CONTROL | KeyModifiers::SHIFT))),
            [0x2A, 0x1D, 0x03, 0x83]
        );
        assert_eq!(
            scancodes(keys.translate(&press(KeyCode::Char('x'), KeyModifiers::ALT))),
            [0xAA, 0x9D, 0x38, 0x2D, 0xAD]
        );
    }

    /// Regression: Shift+Tab arrives as its own key code and was dropped.
    #[test]
    fn back_tab_is_shift_tab() {
        let mut keys = terminal();
        assert_eq!(scancodes(keys.translate(&press(KeyCode::BackTab, KeyModifiers::SHIFT))), [0x2A, 0x0F, 0x8F]);
        let mut keys = terminal();
        assert_eq!(
            scancodes(keys.translate(&press(KeyCode::BackTab, KeyModifiers::NONE))),
            [0x2A, 0x0F, 0x8F, 0xAA]
        );
    }

    /// A translator for the Windows console, which reports key releases.
    fn windows_console() -> KeyTranslator {
        KeyTranslator::new(true)
    }

    /// Regression: the key releases the Windows console reports were thrown
    /// away and every make was followed at once by its break, so the guest
    /// never saw a key held down.
    #[test]
    fn windows_console_key_stays_down_until_it_is_released() {
        let mut keys = windows_console();
        assert_eq!(keys.translate(&press(KeyCode::Up, KeyModifiers::NONE)), [EmulatorEvent::Scancode(0x48)]);
        // The console repeats the press while the key is held, like the
        // typematic makes of a real keyboard.
        assert_eq!(keys.translate(&press(KeyCode::Up, KeyModifiers::NONE)), [EmulatorEvent::Scancode(0x48)]);
        assert_eq!(keys.translate(&release(KeyCode::Up, KeyModifiers::NONE)), [EmulatorEvent::ScanRelease(0xC8)]);

        // The release of a key that went down before the emulator was
        // listening has nothing to let go of.
        assert_eq!(keys.translate(&release(KeyCode::Enter, KeyModifiers::NONE)), []);
    }

    #[test]
    fn windows_console_modifiers_follow_presses_and_releases() {
        let mut keys = windows_console();
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Up, KeyModifiers::SHIFT))), [0x2A, 0x48]);
        // Shift was let go before the key: its release no longer carries the flag.
        assert_eq!(scancodes(keys.translate(&release(KeyCode::Up, KeyModifiers::NONE))), [0xAA, 0xC8]);

        // Shift held only to type a character goes up with its key.
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('*'), KeyModifiers::NONE))), [0x2A, 0x09]);
        assert_eq!(scancodes(keys.translate(&release(KeyCode::Char('*'), KeyModifiers::NONE))), [0xAA, 0x89]);
    }

    /// On a layout where Shift moves a key to another US key (AZERTY digits),
    /// letting go of Shift first makes the release name a key that is not
    /// down.
    #[test]
    fn release_naming_another_character_lets_go_of_the_newest_key() {
        let mut keys = windows_console();
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Up, KeyModifiers::NONE))), [0x48]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('1'), KeyModifiers::SHIFT))), [0x02]);
        assert_eq!(scancodes(keys.translate(&release(KeyCode::Char('&'), KeyModifiers::NONE))), [0x82]);
        assert_eq!(scancodes(keys.translate(&release(KeyCode::Up, KeyModifiers::NONE))), [0xC8]);
    }

    #[test]
    fn losing_focus_releases_everything() {
        let mut keys = windows_console();
        keys.translate(&press(KeyCode::Tab, KeyModifiers::ALT)); // Alt+Tab away
        assert_eq!(scancodes(keys.release_all()), [0x8F, 0xB8]);
        assert_eq!(keys.release_all(), []);
    }

    #[test]
    fn windows_console_emulator_keys_act_on_the_press_only() {
        let mut keys = windows_console();
        assert_eq!(keys.translate(&press(KeyCode::F(12), KeyModifiers::NONE)), [EmulatorEvent::EnterCLI]);
        assert_eq!(keys.translate(&release(KeyCode::F(12), KeyModifiers::NONE)), []);
        assert_eq!(keys.translate(&press(KeyCode::Char('c'), KeyModifiers::CONTROL)), [EmulatorEvent::Quit]);
    }

    /// Regression: the Windows console reports AltGr as Ctrl+Alt next to the
    /// character it produced, and the guest received Ctrl+Alt+key.
    #[test]
    fn altgr_character_is_typed_without_ctrl_and_alt() {
        let altgr = KeyModifiers::CONTROL | KeyModifiers::ALT;
        let mut keys = windows_console();
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('\\'), altgr))), [0x2B]);
        assert_eq!(scancodes(keys.translate(&release(KeyCode::Char('\\'), altgr))), [0xAB]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('@'), altgr))), [0x2A, 0x03]);
        assert_eq!(scancodes(keys.translate(&release(KeyCode::Char('@'), altgr))), [0xAA, 0x83]);
        // A character with no key on the XT keyboard presses nothing at all.
        assert_eq!(keys.translate(&press(KeyCode::Char('\u{20ac}'), altgr)), []);
    }

    #[test]
    fn ctrl_alt_with_a_letter_digit_or_special_key_is_passed_on() {
        let ctrl_alt = KeyModifiers::CONTROL | KeyModifiers::ALT;
        let mut keys = windows_console();
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('q'), ctrl_alt))), [0x1D, 0x38, 0x10]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('7'), ctrl_alt))), [0x08]);
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Delete, ctrl_alt))), [0x53]);

        // Only the Windows console reports AltGr this way; elsewhere a
        // symbol with Ctrl+Alt is the real combination.
        let mut keys = terminal();
        assert_eq!(scancodes(keys.translate(&press(KeyCode::Char('['), ctrl_alt))), [0x1D, 0x38, 0x1A, 0x9A]);
    }
}
