use std::collections::VecDeque;
use std::sync::mpsc;
use std::thread;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use log::debug;
use crate::io::bus::IoDevice;
use crate::io::pic::Pic;
use crate::vm::cpu::Cpu;

#[derive(Debug)]
pub enum EmulatorEvent {
    Scancode(u8),       // make code
    ScanRelease(u8),    // break code (scancode | 0x80)
    Quit,
}

pub struct KeyboardController {
    status: u8,
    data: u8,
    port_b: u8,
    rx: mpsc::Receiver<EmulatorEvent>,
    pending: VecDeque<u8>,  // scancode queue
}

impl KeyboardController {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            loop {
                if let Ok(Event::Key(key)) = event::read() {
                    // Only handle key press, not release
                    if key.kind != event::KeyEventKind::Press {
                        continue;
                    }

                    if key.modifiers.contains(KeyModifiers::CONTROL)
                        && key.code == KeyCode::Char('c')
                    {
                        let _ = tx.send(EmulatorEvent::Quit);
                        break;
                    }

                    if let Some(scancode) = key_to_scancode(&key) {
                        let _ = tx.send(EmulatorEvent::Scancode(scancode));
                        let _ = tx.send(EmulatorEvent::ScanRelease(scancode | 0x80));
                    }
                }
            }
        });

        Self {
            status: 0x00,
            data: 0x00,
            port_b: 0x00,
            rx,
            pending: VecDeque::new(),
        }
    }

    /// Call this periodically from the main loop (e.g. every N instructions).
    /// Drains the channel and queues scancodes.
    /// Returns true if Quit was received.
    pub fn poll(&mut self, pic: &mut Pic) -> bool {
        while let Ok(event) = self.rx.try_recv() {
            match event {
                EmulatorEvent::Quit => return true,
                EmulatorEvent::Scancode(sc) | EmulatorEvent::ScanRelease(sc) => {
                    debug!("Received scancode: 0x{:02X}", sc);
                    self.pending.push_back(sc);
                }
            }
        }

        // If controller has no pending byte and queue has data, load next
        debug!("KeyboardController: status={:02X}, data={:02X}, pending={:?}",
            self.status, self.data, self.pending);
        if self.status & 0x01 == 0 {
            if let Some(sc) = self.pending.pop_front() {
                debug!("Raising IRQ for scancode: 0x{:02X}", sc);
                self.data = sc;
                self.status |= 0x01;
                pic.raise_irq(1);
            }
        }

        if self.status & 0x01 != 0 {
            pic.raise_irq(1);
        } else {
            pic.lower_irq(1);
        }
        false
    }
}

impl IoDevice for KeyboardController {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            0x60 => {
                self.status &= !0x01;
                self.data
            }
            0x61 => self.port_b,
            0x64 => self.status,
            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            0x60 => { /* keyboard commands — ignore for now */ }
            0x61 => {
                self.port_b = (value & 0x0F) | (self.port_b & 0xF0);
            }
            0x64 => {
                match value {
                    0xAA => { self.data = 0x55; self.status |= 0x01; }
                    0xAB => { self.data = 0x00; self.status |= 0x01; }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    fn name(&self) -> &'static str {
        "8042 Keyboard Controller"
    }
}

fn key_to_scancode(key: &KeyEvent) -> Option<u8> {
    match key.code {
        KeyCode::Esc       => Some(0x01),
        KeyCode::Char('1') => Some(0x02),
        KeyCode::Char('2') => Some(0x03),
        KeyCode::Char('3') => Some(0x04),
        KeyCode::Char('4') => Some(0x05),
        KeyCode::Char('5') => Some(0x06),
        KeyCode::Char('6') => Some(0x07),
        KeyCode::Char('7') => Some(0x08),
        KeyCode::Char('8') => Some(0x09),
        KeyCode::Char('9') => Some(0x0A),
        KeyCode::Char('0') => Some(0x0B),
        KeyCode::Char('-') => Some(0x0C),
        KeyCode::Char('=') => Some(0x0D),
        KeyCode::Backspace => Some(0x0E),
        KeyCode::Tab       => Some(0x0F),
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
        KeyCode::Char('[') => Some(0x1A),
        KeyCode::Char(']') => Some(0x1B),
        KeyCode::Enter     => Some(0x1C),
        KeyCode::Char('a') | KeyCode::Char('A') => Some(0x1E),
        KeyCode::Char('s') | KeyCode::Char('S') => Some(0x1F),
        KeyCode::Char('d') | KeyCode::Char('D') => Some(0x20),
        KeyCode::Char('f') | KeyCode::Char('F') => Some(0x21),
        KeyCode::Char('g') | KeyCode::Char('G') => Some(0x22),
        KeyCode::Char('h') | KeyCode::Char('H') => Some(0x23),
        KeyCode::Char('j') | KeyCode::Char('J') => Some(0x24),
        KeyCode::Char('k') | KeyCode::Char('K') => Some(0x25),
        KeyCode::Char('l') | KeyCode::Char('L') => Some(0x26),
        KeyCode::Char(';') => Some(0x27),
        KeyCode::Char('\'') => Some(0x28),
        KeyCode::Char('`') => Some(0x29),
        KeyCode::Char('\\') => Some(0x2B),
        KeyCode::Char('z') | KeyCode::Char('Z') => Some(0x2C),
        KeyCode::Char('x') | KeyCode::Char('X') => Some(0x2D),
        KeyCode::Char('c') | KeyCode::Char('C') => Some(0x2E),
        KeyCode::Char('v') | KeyCode::Char('V') => Some(0x2F),
        KeyCode::Char('b') | KeyCode::Char('B') => Some(0x30),
        KeyCode::Char('n') | KeyCode::Char('N') => Some(0x31),
        KeyCode::Char('m') | KeyCode::Char('M') => Some(0x32),
        KeyCode::Char(',') => Some(0x33),
        KeyCode::Char('.') => Some(0x34),
        KeyCode::Char('/') => Some(0x35),
        KeyCode::Char(' ') => Some(0x39),
        KeyCode::F(1)      => Some(0x3B),
        KeyCode::F(2)      => Some(0x3C),
        KeyCode::F(3)      => Some(0x3D),
        KeyCode::F(4)      => Some(0x3E),
        KeyCode::F(5)      => Some(0x3F),
        KeyCode::F(6)      => Some(0x40),
        KeyCode::F(7)      => Some(0x41),
        KeyCode::F(8)      => Some(0x42),
        KeyCode::F(9)      => Some(0x43),
        KeyCode::F(10)     => Some(0x44),
        KeyCode::Home      => Some(0x47),
        KeyCode::Up        => Some(0x48),
        KeyCode::PageUp    => Some(0x49),
        KeyCode::Left      => Some(0x4B),
        KeyCode::Right      => Some(0x4D),
        KeyCode::End       => Some(0x4F),
        KeyCode::Down      => Some(0x50),
        KeyCode::PageDown  => Some(0x51),
        KeyCode::Insert    => Some(0x52),
        KeyCode::Delete    => Some(0x53),
        _ => None,
    }
}