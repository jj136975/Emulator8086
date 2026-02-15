use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use crate::io::bus::IoDevice;

/// Each entry is (scancode, ascii). Break codes use (scancode|0x80, 0).
pub type KeyBuffer = VecDeque<(u8, u8)>;

pub struct Keyboard {
    scancode_buffer: Arc<Mutex<KeyBuffer>>,
    scancode_latch: Arc<Mutex<u8>>,
    pub irq_pending: Arc<Mutex<bool>>,
    monitor_flag: Arc<AtomicBool>,
    /// Set by deliver_keyboard_irq when a scancode is latched, cleared when
    /// port 0x60 is read.  Port 0x64 bit 0 (OBF) reflects this flag.
    data_available: Arc<AtomicBool>,
}

impl Keyboard {
    pub fn new() -> Self {
        Self {
            scancode_buffer: Arc::new(Mutex::new(VecDeque::new())),
            scancode_latch: Arc::new(Mutex::new(0)),
            irq_pending: Arc::new(Mutex::new(false)),
            monitor_flag: Arc::new(AtomicBool::new(false)),
            data_available: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn shared_buffer(&self) -> Arc<Mutex<KeyBuffer>> {
        self.scancode_buffer.clone()
    }

    pub fn shared_irq(&self) -> Arc<Mutex<bool>> {
        self.irq_pending.clone()
    }

    pub fn shared_latch(&self) -> Arc<Mutex<u8>> {
        self.scancode_latch.clone()
    }

    pub fn shared_monitor_flag(&self) -> Arc<AtomicBool> {
        self.monitor_flag.clone()
    }

    pub fn shared_data_available(&self) -> Arc<AtomicBool> {
        self.data_available.clone()
    }
}

impl IoDevice for Keyboard {
    fn port_in_byte(&mut self, _port: u16) -> u8 {
        // Port 0x60: return latched scancode (set by deliver_keyboard_irq)
        // Clear data_available so port 0x64 stops reporting OBF until next delivery.
        self.data_available.store(false, Ordering::SeqCst);
        *self.scancode_latch.lock().unwrap()
    }

    fn port_out_byte(&mut self, _port: u16, _value: u8) {
        // Controller commands - acknowledge and ignore
    }

    fn name(&self) -> &'static str {
        "8042 Keyboard Data"
    }
}

/// Separate IoDevice for port 0x64 (keyboard status register).
/// Uses a shared `data_available` flag that is set by `deliver_keyboard_irq`
/// when a scancode is latched to port 0x60, and cleared when port 0x60 is read.
pub struct KeyboardStatus {
    data_available: Arc<AtomicBool>,
}

impl KeyboardStatus {
    pub fn new(data_available: Arc<AtomicBool>) -> Self {
        Self { data_available }
    }
}

impl IoDevice for KeyboardStatus {
    fn port_in_byte(&mut self, _port: u16) -> u8 {
        if self.data_available.load(Ordering::SeqCst) { 0x01 } else { 0x00 }
    }

    fn port_out_byte(&mut self, _port: u16, _value: u8) {}

    fn name(&self) -> &'static str {
        "8042 Keyboard Status"
    }
}

/// Start the keyboard input thread using shared state.
/// Can be called after the Keyboard has been moved into the IoBus.
pub fn start_keyboard_thread(
    buffer: Arc<Mutex<KeyBuffer>>,
    irq_pending: Arc<Mutex<bool>>,
    monitor_flag: Arc<AtomicBool>,
) {
    std::thread::spawn(move || {
        use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers, KeyEventKind};
        use std::time::Duration;

        log::debug!("[KB-THREAD] keyboard input thread started");

        loop {
            // Use poll() with timeout instead of blocking read() to prevent
            // the thread from getting stuck indefinitely if the console state
            // changes (e.g. on Windows when VGA output reconfigures the terminal).
            match event::poll(Duration::from_millis(100)) {
                Ok(true) => {} // event available, fall through to read()
                Ok(false) => continue, // timeout, loop again
                Err(e) => {
                    log::debug!("[KB-THREAD] poll error: {:?}", e);
                    std::thread::sleep(Duration::from_millis(10));
                    continue;
                }
            }

            let ev = match event::read() {
                Ok(ev) => ev,
                Err(e) => {
                    log::debug!("[KB-THREAD] read error: {:?}", e);
                    continue;
                }
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

            if let Some((scancode, ascii, enhanced)) = keycode_to_scancode(code, modifiers) {
                let mut q = buffer.lock().unwrap();
                if enhanced {
                    q.push_back((0xE0, 0x00));               // E0 prefix (make)
                }
                q.push_back((scancode, ascii));              // make code + correct ASCII
                if enhanced {
                    q.push_back((0xE0, 0x00));               // E0 prefix (break)
                }
                q.push_back((scancode | 0x80, 0));           // break code
                log::debug!("[KB-THREAD] enqueued sc={:02X} ascii={:02X} enhanced={} buf_len={}",
                           scancode, ascii, enhanced, q.len());
                *irq_pending.lock().unwrap() = true;
            } else {
                log::debug!("[KB-THREAD] unmapped key: {:?}", code);
            }
        }
    });
}

/// Map a crossterm KeyCode + modifiers to (PC AT scancode, ASCII byte, enhanced).
/// `enhanced` = true means the key needs an 0xE0 prefix byte (arrow/nav keys).
fn keycode_to_scancode(code: crossterm::event::KeyCode, modifiers: crossterm::event::KeyModifiers) -> Option<(u8, u8, bool)> {
    use crossterm::event::KeyCode;

    match code {
        // Regular characters — crossterm already gives us the right char
        // (uppercase if Shift is held, symbols like ! @ # etc.)
        KeyCode::Char(ch) => {
            let scancode = char_to_scancode(ch)?;
            let ascii = if modifiers.contains(crossterm::event::KeyModifiers::CONTROL) {
                // Ctrl+letter → control code (A=0x01 .. Z=0x1A)
                if ch.is_ascii_alphabetic() {
                    (ch.to_ascii_uppercase() as u8) & 0x1F
                } else {
                    0
                }
            } else if ch.is_ascii() {
                ch as u8
            } else {
                0
            };
            Some((scancode, ascii, false))
        }
        KeyCode::Enter => Some((0x1C, 0x0D, false)),
        KeyCode::Backspace => Some((0x0E, 0x08, false)),
        KeyCode::Tab => Some((0x0F, 0x09, false)),
        KeyCode::Esc => Some((0x01, 0x1B, false)),

        // Arrow keys — enhanced keyboard: ascii = 0xE0, needs E0 prefix
        KeyCode::Up => Some((0x48, 0xE0, true)),
        KeyCode::Down => Some((0x50, 0xE0, true)),
        KeyCode::Left => Some((0x4B, 0xE0, true)),
        KeyCode::Right => Some((0x4D, 0xE0, true)),

        // Navigation keys — enhanced keyboard: ascii = 0xE0, needs E0 prefix
        KeyCode::Home => Some((0x47, 0xE0, true)),
        KeyCode::End => Some((0x4F, 0xE0, true)),
        KeyCode::PageUp => Some((0x49, 0xE0, true)),
        KeyCode::PageDown => Some((0x51, 0xE0, true)),
        KeyCode::Insert => Some((0x52, 0xE0, true)),
        KeyCode::Delete => Some((0x53, 0xE0, true)),

        // Function keys (extended: ascii = 0, no E0 prefix)
        KeyCode::F(1) => Some((0x3B, 0x00, false)),
        KeyCode::F(2) => Some((0x3C, 0x00, false)),
        KeyCode::F(3) => Some((0x3D, 0x00, false)),
        KeyCode::F(4) => Some((0x3E, 0x00, false)),
        KeyCode::F(5) => Some((0x3F, 0x00, false)),
        KeyCode::F(6) => Some((0x40, 0x00, false)),
        KeyCode::F(7) => Some((0x41, 0x00, false)),
        KeyCode::F(8) => Some((0x42, 0x00, false)),
        KeyCode::F(9) => Some((0x43, 0x00, false)),
        KeyCode::F(10) => Some((0x44, 0x00, false)),
        KeyCode::F(11) => Some((0x57, 0x00, false)),
        KeyCode::F(12) => Some((0x58, 0x00, false)),

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
