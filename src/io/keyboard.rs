//! The PC/XT keyboard interface on the 8255 PPI (ports 60h-63h). The window
//! sends `EmulatorEvent`s down a channel; `KeyboardController::poll` loads
//! their scancodes into port A one at a time and raises IRQ1 for each. Port
//! B carries the acknowledge, the keyboard clock and timer 2's gate and
//! speaker bits; port C reads timer 2's output back.
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
use log::debug;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;
use std::sync::mpsc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmulatorEvent {
    Scancode(u8),    // make code
    ScanRelease(u8), // break code (scancode | 0x80)
    DumpVga,         // dump the VGA state to a file (F12)
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

    /// Events from the window.
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
    /// Create with the receiving end of an event channel. The window sends
    /// `EmulatorEvent` values through the other end.
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

    pub fn set_pit(&mut self, pit: Rc<RefCell<Pit>>) {
        self.pit = Some(pit);
    }

    /// Call this periodically from the main loop (e.g. every ~1000 instructions).
    /// Drains the event channel and loads the next scancode into the shift
    /// register when it is clear and the keyboard is enabled.
    /// Returns Some(event) for an event that is not a key (DumpVga).
    pub fn poll(&mut self, pic: &mut Pic) -> Option<EmulatorEvent> {
        while let Ok(event) = self.rx.try_recv() {
            match event {
                EmulatorEvent::DumpVga => {
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

#[cfg(test)]
#[path = "keyboard_tests.rs"]
mod tests;
