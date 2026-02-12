use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use crate::io::bus::IoDevice;

/// Each entry is (scancode, ascii). Break codes use (scancode|0x80, 0).
pub type KeyBuffer = VecDeque<(u8, u8)>;

pub struct Keyboard {
    scancode_buffer: Arc<Mutex<KeyBuffer>>,
    pub irq_pending: Arc<Mutex<bool>>,
    monitor_flag: Arc<AtomicBool>,
}

impl Keyboard {
    pub fn new() -> Self {
        Self {
            scancode_buffer: Arc::new(Mutex::new(VecDeque::new())),
            irq_pending: Arc::new(Mutex::new(false)),
            monitor_flag: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn shared_buffer(&self) -> Arc<Mutex<KeyBuffer>> {
        self.scancode_buffer.clone()
    }

    pub fn shared_irq(&self) -> Arc<Mutex<bool>> {
        self.irq_pending.clone()
    }

    pub fn shared_monitor_flag(&self) -> Arc<AtomicBool> {
        self.monitor_flag.clone()
    }

    pub fn start_input_thread(&self) {
        let buffer = self.scancode_buffer.clone();
        let irq_pending = self.irq_pending.clone();
        let monitor_flag = self.monitor_flag.clone();

        std::thread::spawn(move || {
            use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, KeyEventKind};

            loop {
                let ev = match event::read() {
                    Ok(ev) => ev,
                    Err(_) => continue,
                };

                let Event::Key(KeyEvent { code, modifiers, kind, .. }) = ev else {
                    continue;
                };

                // Only handle key press events (ignore release/repeat)
                if kind != KeyEventKind::Press {
                    continue;
                }

                // Ctrl+C exits the emulator
                if code == KeyCode::Char('c') && modifiers.contains(KeyModifiers::CONTROL) {
                    let _ = crossterm::terminal::disable_raw_mode();
                    use std::io::Write;
                    let _ = write!(std::io::stdout(), "\x1B[0m\x1B[?25h\n");
                    let _ = std::io::stdout().flush();
                    std::process::exit(0);
                }

                // F12 opens the monitor — set flag and sleep until main loop clears it
                if code == KeyCode::F(12) {
                    monitor_flag.store(true, Ordering::SeqCst);
                    while monitor_flag.load(Ordering::SeqCst) {
                        std::thread::sleep(std::time::Duration::from_millis(50));
                    }
                    continue;
                }

                if let Some((scancode, ascii)) = keycode_to_scancode(code, modifiers) {
                    let mut q = buffer.lock().unwrap();
                    q.push_back((scancode, ascii));          // make code + correct ASCII
                    q.push_back((scancode | 0x80, 0));       // break code
                    *irq_pending.lock().unwrap() = true;
                }
            }
        });
    }
}

impl IoDevice for Keyboard {
    fn port_in_byte(&mut self, _port: u16) -> u8 {
        // Port 0x60 returns raw scancodes only
        self.scancode_buffer.lock().unwrap()
            .pop_front()
            .map(|(sc, _)| sc)
            .unwrap_or(0)
    }

    fn port_out_byte(&mut self, _port: u16, _value: u8) {
        // Controller commands - acknowledge and ignore
    }

    fn name(&self) -> &'static str {
        "8042 Keyboard Data"
    }
}

/// Separate IoDevice for port 0x64 (keyboard status register).
/// Shares the scancode buffer with the Keyboard device.
pub struct KeyboardStatus {
    scancode_buffer: Arc<Mutex<KeyBuffer>>,
}

impl KeyboardStatus {
    pub fn new(buffer: Arc<Mutex<KeyBuffer>>) -> Self {
        Self { scancode_buffer: buffer }
    }
}

impl IoDevice for KeyboardStatus {
    fn port_in_byte(&mut self, _port: u16) -> u8 {
        let has_data = !self.scancode_buffer.lock().unwrap().is_empty();
        if has_data { 0x01 } else { 0x00 }
    }

    fn port_out_byte(&mut self, _port: u16, _value: u8) {}

    fn name(&self) -> &'static str {
        "8042 Keyboard Status"
    }
}

/// Map a crossterm KeyCode + modifiers to (PC AT scancode, ASCII byte).
fn keycode_to_scancode(code: crossterm::event::KeyCode, modifiers: crossterm::event::KeyModifiers) -> Option<(u8, u8)> {
    use crossterm::event::KeyCode;

    match code {
        // Regular characters — crossterm already gives us the right char
        // (uppercase if Shift is held, symbols like ! @ # etc.)
        KeyCode::Char(ch) => {
            let scancode = char_to_scancode(ch)?;
            let ascii = if ch.is_ascii() { ch as u8 } else { 0 };
            Some((scancode, ascii))
        }
        KeyCode::Enter => Some((0x1C, 0x0D)),
        KeyCode::Backspace => Some((0x0E, 0x08)),
        KeyCode::Tab => Some((0x0F, 0x09)),
        KeyCode::Esc => Some((0x01, 0x1B)),

        // Arrow keys (extended: ascii = 0)
        KeyCode::Up => Some((0x48, 0x00)),
        KeyCode::Down => Some((0x50, 0x00)),
        KeyCode::Left => Some((0x4B, 0x00)),
        KeyCode::Right => Some((0x4D, 0x00)),

        // Navigation keys (extended: ascii = 0)
        KeyCode::Home => Some((0x47, 0x00)),
        KeyCode::End => Some((0x4F, 0x00)),
        KeyCode::PageUp => Some((0x49, 0x00)),
        KeyCode::PageDown => Some((0x51, 0x00)),
        KeyCode::Insert => Some((0x52, 0x00)),
        KeyCode::Delete => Some((0x53, 0x00)),

        // Function keys (extended: ascii = 0)
        KeyCode::F(1) => Some((0x3B, 0x00)),
        KeyCode::F(2) => Some((0x3C, 0x00)),
        KeyCode::F(3) => Some((0x3D, 0x00)),
        KeyCode::F(4) => Some((0x3E, 0x00)),
        KeyCode::F(5) => Some((0x3F, 0x00)),
        KeyCode::F(6) => Some((0x40, 0x00)),
        KeyCode::F(7) => Some((0x41, 0x00)),
        KeyCode::F(8) => Some((0x42, 0x00)),
        KeyCode::F(9) => Some((0x43, 0x00)),
        KeyCode::F(10) => Some((0x44, 0x00)),
        KeyCode::F(11) => Some((0x57, 0x00)),
        KeyCode::F(12) => Some((0x58, 0x00)),

        _ => None,
    }
}

/// Map an ASCII/Unicode character to its PC AT scancode.
fn char_to_scancode(ch: char) -> Option<u8> {
    // Convert to lowercase for lookup, scancodes are the same regardless of shift
    let scancode = match ch.to_ascii_lowercase() {
        '1' | '!' => 0x02,
        '2' | '@' => 0x03,
        '3' | '#' => 0x04,
        '4' | '$' => 0x05,
        '5' | '%' => 0x06,
        '6' | '^' => 0x07,
        '7' | '&' => 0x08,
        '8' | '*' => 0x09,
        '9' | '(' => 0x0A,
        '0' | ')' => 0x0B,
        '-' | '_' => 0x0C,
        '=' | '+' => 0x0D,
        'q' => 0x10,
        'w' => 0x11,
        'e' => 0x12,
        'r' => 0x13,
        't' => 0x14,
        'y' => 0x15,
        'u' => 0x16,
        'i' => 0x17,
        'o' => 0x18,
        'p' => 0x19,
        '[' | '{' => 0x1A,
        ']' | '}' => 0x1B,
        'a' => 0x1E,
        's' => 0x1F,
        'd' => 0x20,
        'f' => 0x21,
        'g' => 0x22,
        'h' => 0x23,
        'j' => 0x24,
        'k' => 0x25,
        'l' => 0x26,
        ';' | ':' => 0x27,
        '\'' | '"' => 0x28,
        '`' | '~' => 0x29,
        '\\' | '|' => 0x2B,
        'z' => 0x2C,
        'x' => 0x2D,
        'c' => 0x2E,
        'v' => 0x2F,
        'b' => 0x30,
        'n' => 0x31,
        'm' => 0x32,
        ',' | '<' => 0x33,
        '.' | '>' => 0x34,
        '/' | '?' => 0x35,
        ' ' => 0x39,
        _ => return None,
    };
    Some(scancode)
}
