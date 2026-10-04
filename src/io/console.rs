//! The debug port E9h. The BIOS writes its progress markers there, and a
//! copy of everything it prints through INT 10h teletype.
//!
//! The invariant: nothing written to the port reaches the terminal, which
//! the terminal frontend draws the guest's screen on. The bytes are
//! collected into lines and logged at debug level (`RUST_LOG=debug`), and
//! the log is the only place they go.

use log::debug;

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

/// A line longer than this is logged without waiting for its end.
const MAX_LINE: usize = 256;

/// Collects the bytes written to port E9h into lines and logs each one.
#[derive(Default)]
pub struct DebugConsole {
    line: String,
}

impl DebugConsole {
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds one byte and returns the line it completes, if any.
    fn push(&mut self, byte: u8) -> Option<String> {
        match byte {
            b'\n' => Some(std::mem::take(&mut self.line)),
            b'\r' => None,
            _ => {
                let printable = byte == b' ' || byte.is_ascii_graphic();
                self.line.push(if printable { byte as char } else { '.' });
                (self.line.len() >= MAX_LINE).then(|| std::mem::take(&mut self.line))
            }
        }
    }
}

impl IoDevice for DebugConsole {
    fn port_in_byte(&mut self, _port: u16, _cpu: &mut Cpu) -> u8 {
        0xFF
    }
    fn port_out_byte(&mut self, _port: u16, value: u8, _cpu: &mut Cpu) {
        if let Some(line) = self.push(value) {
            debug!("port E9h: {}", line);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn feed(console: &mut DebugConsole, bytes: &[u8]) -> Vec<String> {
        bytes
            .iter()
            .filter_map(|byte| console.push(*byte))
            .collect()
    }

    /// Bytes are held until a line feed ends their line, which is then
    /// passed on whole; a carriage return is dropped.
    #[test]
    fn output_is_collected_into_lines() {
        let mut console = DebugConsole::new();
        assert_eq!(feed(&mut console, b"BIL!"), Vec::<String>::new());
        assert_eq!(
            feed(&mut console, b"Starting MS-DOS...\r\n\r\nC:\\>"),
            vec!["BIL!Starting MS-DOS...".to_string(), String::new()]
        );
        assert_eq!(console.line, "C:\\>", "kept until its line ends");
    }

    #[test]
    fn control_characters_are_not_passed_on() {
        let mut console = DebugConsole::new();
        assert_eq!(
            feed(&mut console, b"a\x07\x08\x1Bb\n"),
            vec!["a...b".to_string()]
        );
    }

    #[test]
    fn a_line_without_an_end_is_logged_when_it_gets_long() {
        let mut console = DebugConsole::new();
        let lines = feed(&mut console, &[b'x'; MAX_LINE + 10]);
        assert_eq!(lines, vec!["x".repeat(MAX_LINE)]);
        assert_eq!(console.line.len(), 10);
    }
}
