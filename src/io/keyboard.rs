use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use crate::io::bus::IoDevice;

pub struct Keyboard {
    scancode_buffer: Arc<Mutex<VecDeque<u8>>>,
    pub irq_pending: Arc<Mutex<bool>>,
}

impl Keyboard {
    pub fn new() -> Self {
        Self {
            scancode_buffer: Arc::new(Mutex::new(VecDeque::new())),
            irq_pending: Arc::new(Mutex::new(false)),
        }
    }

    pub fn start_input_thread(&self) {
        let buffer = self.scancode_buffer.clone();
        let irq_pending = self.irq_pending.clone();

        std::thread::spawn(move || {
            use std::io::Read;
            let stdin = std::io::stdin();
            let mut buf = [0u8; 1];
            loop {
                if stdin.lock().read_exact(&mut buf).is_ok() {
                    let scancode = ascii_to_scancode(buf[0]);
                    if let Some((make, brk)) = scancode {
                        let mut q = buffer.lock().unwrap();
                        q.push_back(make);
                        q.push_back(brk);
                        *irq_pending.lock().unwrap() = true;
                    }
                }
            }
        });
    }

    pub fn has_scancode(&self) -> bool {
        !self.scancode_buffer.lock().unwrap().is_empty()
    }

    pub fn peek_scancode(&self) -> Option<u8> {
        self.scancode_buffer.lock().unwrap().front().copied()
    }

    pub fn dequeue_scancode(&self) -> Option<u8> {
        self.scancode_buffer.lock().unwrap().pop_front()
    }

    pub fn check_and_clear_irq(&self) -> bool {
        let mut pending = self.irq_pending.lock().unwrap();
        let was_pending = *pending;
        *pending = false;
        was_pending
    }
}

impl IoDevice for Keyboard {
    fn port_in_byte(&mut self, port: u16) -> u8 {
        match port {
            0x60 => {
                self.scancode_buffer.lock().unwrap()
                    .pop_front()
                    .unwrap_or(0)
            }
            0x64 => {
                let has_data = !self.scancode_buffer.lock().unwrap().is_empty();
                if has_data { 0x01 } else { 0x00 }
            }
            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, _port: u16, _value: u8) {
        // Controller commands - acknowledge and ignore
    }

    fn name(&self) -> &'static str {
        "8042 Keyboard"
    }
}

fn ascii_to_scancode(ascii: u8) -> Option<(u8, u8)> {
    let make = match ascii {
        b'1' | b'!' => 0x02,
        b'2' | b'@' => 0x03,
        b'3' | b'#' => 0x04,
        b'4' | b'$' => 0x05,
        b'5' | b'%' => 0x06,
        b'6' | b'^' => 0x07,
        b'7' | b'&' => 0x08,
        b'8' | b'*' => 0x09,
        b'9' | b'(' => 0x0A,
        b'0' | b')' => 0x0B,
        b'-' | b'_' => 0x0C,
        b'=' | b'+' => 0x0D,
        0x08 => 0x0E, // Backspace
        0x09 => 0x0F, // Tab
        b'q' | b'Q' => 0x10,
        b'w' | b'W' => 0x11,
        b'e' | b'E' => 0x12,
        b'r' | b'R' => 0x13,
        b't' | b'T' => 0x14,
        b'y' | b'Y' => 0x15,
        b'u' | b'U' => 0x16,
        b'i' | b'I' => 0x17,
        b'o' | b'O' => 0x18,
        b'p' | b'P' => 0x19,
        b'[' | b'{' => 0x1A,
        b']' | b'}' => 0x1B,
        0x0D | 0x0A => 0x1C, // Enter
        b'a' | b'A' => 0x1E,
        b's' | b'S' => 0x1F,
        b'd' | b'D' => 0x20,
        b'f' | b'F' => 0x21,
        b'g' | b'G' => 0x22,
        b'h' | b'H' => 0x23,
        b'j' | b'J' => 0x24,
        b'k' | b'K' => 0x25,
        b'l' | b'L' => 0x26,
        b';' | b':' => 0x27,
        b'\'' | b'"' => 0x28,
        b'`' | b'~' => 0x29,
        b'\\' | b'|' => 0x2B,
        b'z' | b'Z' => 0x2C,
        b'x' | b'X' => 0x2D,
        b'c' | b'C' => 0x2E,
        b'v' | b'V' => 0x2F,
        b'b' | b'B' => 0x30,
        b'n' | b'N' => 0x31,
        b'm' | b'M' => 0x32,
        b',' | b'<' => 0x33,
        b'.' | b'>' => 0x34,
        b'/' | b'?' => 0x35,
        b' ' => 0x39,
        0x1B => 0x01, // Escape
        _ => return None,
    };
    Some((make, make | 0x80)) // make code, break code
}

pub fn scancode_to_ascii(scancode: u8) -> u8 {
    match scancode {
        0x02 => b'1',
        0x03 => b'2',
        0x04 => b'3',
        0x05 => b'4',
        0x06 => b'5',
        0x07 => b'6',
        0x08 => b'7',
        0x09 => b'8',
        0x0A => b'9',
        0x0B => b'0',
        0x0C => b'-',
        0x0D => b'=',
        0x0E => 0x08, // Backspace
        0x0F => 0x09, // Tab
        0x10 => b'q',
        0x11 => b'w',
        0x12 => b'e',
        0x13 => b'r',
        0x14 => b't',
        0x15 => b'y',
        0x16 => b'u',
        0x17 => b'i',
        0x18 => b'o',
        0x19 => b'p',
        0x1A => b'[',
        0x1B => b']',
        0x1C => 0x0D, // Enter
        0x1E => b'a',
        0x1F => b's',
        0x20 => b'd',
        0x21 => b'f',
        0x22 => b'g',
        0x23 => b'h',
        0x24 => b'j',
        0x25 => b'k',
        0x26 => b'l',
        0x27 => b';',
        0x28 => b'\'',
        0x29 => b'`',
        0x2B => b'\\',
        0x2C => b'z',
        0x2D => b'x',
        0x2E => b'c',
        0x2F => b'v',
        0x30 => b'b',
        0x31 => b'n',
        0x32 => b'm',
        0x33 => b',',
        0x34 => b'.',
        0x35 => b'/',
        0x39 => b' ',
        0x01 => 0x1B, // Escape
        _ => 0,
    }
}
