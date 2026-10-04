//! The debug port E9h. The BIOS writes its progress markers there, and a
//! copy of everything it prints through INT 10h teletype.
//!
//! The bytes are collected into lines. Each finished line is logged at
//! debug level (`RUST_LOG=debug`) and kept for whoever reads the console,
//! the app's panel of the port; nothing written to the port is printed or
//! shown on the guest's screen.
//!
//! The invariant: what is kept is bounded. Past `KEPT_LINES` the oldest
//! line goes as a new one comes, as lines scroll off a screen, while
//! `lines_finished` counts every line the guest finished.

use std::collections::VecDeque;

use log::debug;

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

/// A line longer than this is logged without waiting for its end.
const MAX_LINE: usize = 256;

/// The finished lines kept for a reader, the newest last.
pub const KEPT_LINES: usize = 500;

/// Collects the bytes written to port E9h into lines, logs each one and
/// keeps the last of them.
#[derive(Default)]
pub struct DebugConsole {
    /// The line being written.
    line: String,
    /// The last lines finished, oldest first.
    kept: VecDeque<String>,
    /// Lines finished since the machine was built, kept or not.
    finished: u64,
}

impl DebugConsole {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// The lines kept, oldest first: the last `KEPT_LINES` the guest
    /// finished at most.
    pub fn lines(&self) -> impl ExactSizeIterator<Item = &str> + DoubleEndedIterator {
        self.kept.iter().map(String::as_str)
    }

    /// What the guest wrote since its last finished line.
    pub fn line_in_progress(&self) -> &str {
        &self.line
    }

    /// Every line the guest finished since the machine was built, those no
    /// longer kept included.
    pub fn lines_finished(&self) -> u64 {
        self.finished
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

    /// Takes a byte the guest wrote: the line it finishes, if it finishes
    /// one, is logged and kept.
    fn write(&mut self, byte: u8) {
        if let Some(line) = self.push(byte) {
            debug!("port E9h: {}", line);
            if self.kept.len() == KEPT_LINES {
                self.kept.pop_front();
            }
            self.kept.push_back(line);
            self.finished += 1;
        }
    }
}

impl IoDevice for DebugConsole {
    fn port_in_byte(&mut self, _port: u16, _cpu: &mut Cpu) -> u8 {
        0xFF
    }
    fn port_out_byte(&mut self, _port: u16, value: u8, _cpu: &mut Cpu) {
        self.write(value);
    }
}

#[cfg(test)]
#[path = "console_tests.rs"]
mod tests;
