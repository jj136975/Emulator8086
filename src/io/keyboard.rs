use crate::io::bus::IoDevice;
use crate::io::pic::Pic;
use crate::io::pit::Pit;
use crate::vm::cpu::Cpu;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, ModifierKeyCode};
use log::debug;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use std::sync::mpsc;
use std::thread;

#[derive(Debug)]
pub enum EmulatorEvent {
    Scancode(u8),    // make code
    ScanRelease(u8), // break code (scancode | 0x80)
    EnterCLI,        // signal to enter command line interface
    DumpVga,         // F11: dump VGA memory to file
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
    /// Port B bit 7 was set high; need to process clear on falling edge.
    kb_clear_pending: bool,

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
            kb_clear_pending: false,
            port_b: 0,
            rx,
            pending: VecDeque::new(),
            pit: None,
            kb_clock_low: false,
            kb_reset_pending: false,
        }
    }

    /// Create with built-in crossterm keyboard thread (terminal mode).
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            // Track modifier state so we can sync from crossterm's key.modifiers
            // on every press event. This ensures modifiers are released even when
            // the terminal doesn't report KeyEventKind::Release events.
            let mut active_shift = false;
            let mut active_ctrl = false;
            let mut active_alt = false;

            loop {
                if let Ok(Event::Key(key)) = event::read() {
                    if key.kind != event::KeyEventKind::Press {
                        continue;
                    }

                    // F12: enter CLI
                    if key.code == KeyCode::F(12) {
                        let _ = tx.send(EmulatorEvent::EnterCLI);
                        continue;
                    }

                    // F11: dump VGA
                    if key.code == KeyCode::F(11) {
                        let _ = tx.send(EmulatorEvent::DumpVga);
                        continue;
                    }

                    // Ctrl+C: quit
                    if key.modifiers.contains(KeyModifiers::CONTROL)
                        && key.code == KeyCode::Char('c')
                    {
                        let _ = tx.send(EmulatorEvent::Quit);
                        break;
                    }

                    // Sync modifier state from crossterm's key.modifiers.
                    // Send make/break codes to match the actual modifier state.
                    let want_shift = key.modifiers.contains(KeyModifiers::SHIFT);
                    if want_shift && !active_shift {
                        let _ = tx.send(EmulatorEvent::Scancode(0x2A));
                        active_shift = true;
                    } else if !want_shift && active_shift {
                        let _ = tx.send(EmulatorEvent::ScanRelease(0xAA));
                        active_shift = false;
                    }

                    let want_ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
                    if want_ctrl && !active_ctrl {
                        let _ = tx.send(EmulatorEvent::Scancode(0x1D));
                        active_ctrl = true;
                    } else if !want_ctrl && active_ctrl {
                        let _ = tx.send(EmulatorEvent::ScanRelease(0x9D));
                        active_ctrl = false;
                    }

                    let want_alt = key.modifiers.contains(KeyModifiers::ALT);
                    if want_alt && !active_alt {
                        let _ = tx.send(EmulatorEvent::Scancode(0x38));
                        active_alt = true;
                    } else if !want_alt && active_alt {
                        let _ = tx.send(EmulatorEvent::ScanRelease(0xB8));
                        active_alt = false;
                    }

                    // Standalone modifier key events are fully handled by sync above
                    if matches!(key.code, KeyCode::Modifier(_)) {
                        continue;
                    }

                    if let Some(scancode) = key_to_scancode(&key) {
                        let _ = tx.send(EmulatorEvent::Scancode(scancode));
                        // Immediate break for all non-modifier keys (including
                        // toggle keys like CapsLock — BIOS toggles on make)
                        let _ = tx.send(EmulatorEvent::ScanRelease(scancode | 0x80));
                    }
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

                // Bit 5: PIT channel 2 output (mirrors port B bit 5)
                if let Some(pit) = &self.pit {
                    if pit.borrow().channel2_output() {
                        val |= 0x20;
                    }
                }

                // Bit 4: speaker output = PIT ch2 output AND port_b bit 1
                if let Some(pit) = &self.pit {
                    if pit.borrow().channel2_output() && (self.port_b & 0x02 != 0) {
                        val |= 0x10;
                    }
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
                    // Set high: clear keyboard shift register, suppress IRQ1
                    self.kb_clear_pending = true;
                    self.kb_enabled = false;
                } else if old_b & 0x80 != 0 {
                    // Bit 7 went from high to low — re-enable keyboard
                    self.kb_enabled = true;
                    // Process the pending clear
                    if self.kb_clear_pending {
                        self.kb_clear_pending = false;
                        self.kb_byte = 0;
                        self.ksr_cleared = true;
                        cpu.pic.clear_interrupt(1); // Clear IRQ1
                    }
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

    fn name(&self) -> &'static str {
        "8255 PPI / Keyboard"
    }
}

fn key_to_scancode(key: &KeyEvent) -> Option<u8> {
    match key.code {
        KeyCode::Esc => Some(0x01),
        KeyCode::Char('1') | KeyCode::Char('!') => Some(0x02),
        KeyCode::Char('2') | KeyCode::Char('@') => Some(0x03),
        KeyCode::Char('3') | KeyCode::Char('#') => Some(0x04),
        KeyCode::Char('4') | KeyCode::Char('$') => Some(0x05),
        KeyCode::Char('5') | KeyCode::Char('%') => Some(0x06),
        KeyCode::Char('6') | KeyCode::Char('^') => Some(0x07),
        KeyCode::Char('7') | KeyCode::Char('&') => Some(0x08),
        KeyCode::Char('8') | KeyCode::Char('*') => Some(0x09),
        KeyCode::Char('9') | KeyCode::Char('(') => Some(0x0A),
        KeyCode::Char('0') | KeyCode::Char(')') => Some(0x0B),
        KeyCode::Char('-') | KeyCode::Char('_') => Some(0x0C),
        KeyCode::Char('=') | KeyCode::Char('+') => Some(0x0D),
        KeyCode::Backspace => Some(0x0E),
        KeyCode::Tab => Some(0x0F),
        KeyCode::Char('q') | KeyCode::Char('Q') => Some(0x10),
        KeyCode::Char('w') | KeyCode::Char('W') => Some(0x11),
        KeyCode::Char('e') | KeyCode::Char('E') => Some(0x12),
        KeyCode::Char('r') | KeyCode::Char('R') => Some(0x13),
        KeyCode::Char('t') | KeyCode::Char('T') => Some(0x14),
        KeyCode::Char('y') | KeyCode::Char('Y') => Some(0x15),
        KeyCode::Char('u') | KeyCode::Char('U') => Some(0x16),
        KeyCode::Char('i') | KeyCode::Char('I') => Some(0x17),
        KeyCode::Char('o') | KeyCode::Char('O') => Some(0x18),
        KeyCode::Char('p') | KeyCode::Char('P') => Some(0x19),
        KeyCode::Char('[') | KeyCode::Char('{') => Some(0x1A),
        KeyCode::Char(']') | KeyCode::Char('}') => Some(0x1B),
        KeyCode::Enter => Some(0x1C),
        // 0x1D = Left Ctrl (see Modifier match below)
        KeyCode::Char('a') | KeyCode::Char('A') => Some(0x1E),
        KeyCode::Char('s') | KeyCode::Char('S') => Some(0x1F),
        KeyCode::Char('d') | KeyCode::Char('D') => Some(0x20),
        KeyCode::Char('f') | KeyCode::Char('F') => Some(0x21),
        KeyCode::Char('g') | KeyCode::Char('G') => Some(0x22),
        KeyCode::Char('h') | KeyCode::Char('H') => Some(0x23),
        KeyCode::Char('j') | KeyCode::Char('J') => Some(0x24),
        KeyCode::Char('k') | KeyCode::Char('K') => Some(0x25),
        KeyCode::Char('l') | KeyCode::Char('L') => Some(0x26),
        KeyCode::Char(';') | KeyCode::Char(':') => Some(0x27),
        KeyCode::Char('\'') | KeyCode::Char('"') => Some(0x28),
        KeyCode::Char('`') | KeyCode::Char('~') => Some(0x29),
        // 0x2A = Left Shift (see Modifier match below)
        KeyCode::Char('\\') | KeyCode::Char('|') => Some(0x2B),
        KeyCode::Char('z') | KeyCode::Char('Z') => Some(0x2C),
        KeyCode::Char('x') | KeyCode::Char('X') => Some(0x2D),
        KeyCode::Char('c') | KeyCode::Char('C') => Some(0x2E),
        KeyCode::Char('v') | KeyCode::Char('V') => Some(0x2F),
        KeyCode::Char('b') | KeyCode::Char('B') => Some(0x30),
        KeyCode::Char('n') | KeyCode::Char('N') => Some(0x31),
        KeyCode::Char('m') | KeyCode::Char('M') => Some(0x32),
        KeyCode::Char(',') | KeyCode::Char('<') => Some(0x33),
        KeyCode::Char('.') | KeyCode::Char('>') => Some(0x34),
        KeyCode::Char('/') | KeyCode::Char('?') => Some(0x35),
        // 0x36 = Right Shift (see Modifier match below)
        // 0x37 = Keypad * (not mapped)
        // 0x38 = Left Alt (see Modifier match below)
        KeyCode::Char(' ') => Some(0x39),
        // 0x3A = Caps Lock (see below)
        KeyCode::CapsLock => Some(0x3A),
        KeyCode::F(1) => Some(0x3B),
        KeyCode::F(2) => Some(0x3C),
        KeyCode::F(3) => Some(0x3D),
        KeyCode::F(4) => Some(0x3E),
        KeyCode::F(5) => Some(0x3F),
        KeyCode::F(6) => Some(0x40),
        KeyCode::F(7) => Some(0x41),
        KeyCode::F(8) => Some(0x42),
        KeyCode::F(9) => Some(0x43),
        KeyCode::F(10) => Some(0x44),
        // 0x45 = Num Lock, 0x46 = Scroll Lock (see below)
        KeyCode::NumLock => Some(0x45),
        KeyCode::ScrollLock => Some(0x46),
        KeyCode::Home => Some(0x47),
        KeyCode::Up => Some(0x48),
        KeyCode::PageUp => Some(0x49),
        KeyCode::Left => Some(0x4B),
        KeyCode::Right => Some(0x4D),
        KeyCode::End => Some(0x4F),
        KeyCode::Down => Some(0x50),
        KeyCode::PageDown => Some(0x51),
        KeyCode::Insert => Some(0x52),
        KeyCode::Delete => Some(0x53),

        // Modifier keys
        KeyCode::Modifier(ModifierKeyCode::LeftShift) => Some(0x2A),
        KeyCode::Modifier(ModifierKeyCode::RightShift) => Some(0x36),
        KeyCode::Modifier(ModifierKeyCode::LeftControl) => Some(0x1D),
        KeyCode::Modifier(ModifierKeyCode::RightControl) => Some(0x1D), // XT has only one ctrl
        KeyCode::Modifier(ModifierKeyCode::LeftAlt) => Some(0x38),
        KeyCode::Modifier(ModifierKeyCode::RightAlt) => Some(0x38), // XT has only one alt

        _ => None,
    }
}
