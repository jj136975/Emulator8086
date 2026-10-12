//! The machine as the tool runs it: the emulator's PC with its built-in
//! BIOS and the disks a stage made, run in batches of `BATCH`
//! instructions, each credited with the time its instructions take a guest
//! of `NANOS_PER_INSTRUCTION`. A stage reads what the guest tells the port
//! E9h, what its screen shows (text, or the picture of Mode 13h) and, once
//! the guest is done, its disks.
//!
//! The invariant: guest time comes from the instructions run, never from a
//! clock. The same disks make the guest run the same instructions on every
//! host and on every run, so the build makes the same files, a budget of
//! guest time is the same everywhere, and the guest shows the same picture
//! after the same instructions. A budget is of guest time, not of
//! instructions: a guest that waits for a key runs none, and its time
//! still passes.

use emu8086_core::io::disk::{DiskController, DiskImage};
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::io::vga::VideoMode;
use emu8086_core::vm::runtime::Runtime;
use emu8086_core::{BIOS_ROM, BIOS_ROM_NAME};
use std::fmt;
use std::sync::mpsc;
use std::time::Duration;

/// Instructions in a batch. The guest is credited its time, and a stage
/// looks at it, after each.
const BATCH: u64 = 1_000_000;
/// Guest time an instruction takes: a guest of 10 million instructions a
/// second.
const NANOS_PER_INSTRUCTION: u64 = 100;
/// Guest time a batch that ends halted takes on top of its instructions:
/// the CPU waits for an interrupt, and the timer's is the next one due.
const IDLE_NANOS: u64 = 1_000_000;
/// How many of the last lines of the port a failure shows.
const PORT_TAIL: usize = 40;
/// The text screen: 80 columns by 25 rows of a character and its
/// attribute, at B8000h.
const TEXT_SCREEN: usize = 0xB8000;
/// The picture of Mode 13h: 320 by 200 pixels of a byte, at A0000h.
const PICTURE: usize = 0xA0000;
const PICTURE_BYTES: usize = 320 * 200;

/// The 64-bit FNV-1a hash of `bytes`.
fn fnv1a(bytes: impl Iterator<Item = u8>) -> u64 {
    bytes.fold(0xCBF2_9CE4_8422_2325, |hash, byte| {
        (hash ^ u64::from(byte)).wrapping_mul(0x0100_0000_01B3)
    })
}

/// A machine the tool runs.
pub(crate) struct Guest {
    runtime: Runtime,
    /// The guest time credited since power-on, in nanoseconds.
    nanos: u64,
    /// The lines of the port E9h the guest had finished when they were
    /// last read.
    read: u64,
}

/// The lines the guest finished on the port E9h between two reads.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct PortLines {
    /// How many of them the port no longer kept: the first ones.
    pub lost: u64,
    /// The rest, which it kept, in their order.
    pub kept: Vec<String>,
}

/// What a stage saw of the guest when it gave up on it.
#[derive(Debug)]
pub(crate) struct Stopped {
    pub instructions: u64,
    pub time: Duration,
    pub cs: u16,
    pub ip: u16,
    /// Whether the VGA showed a picture, Mode 13h, or the text `screen`
    /// holds.
    pub in_mode_13h: bool,
    pub screen: String,
    /// The last lines of the port E9h, the one being written last.
    pub port: Vec<String>,
}

impl fmt::Display for Stopped {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "The guest ran {} instructions in {:.1} s of its time; CS:IP {:04X}:{:04X}.",
            self.instructions,
            self.time.as_secs_f64(),
            self.cs,
            self.ip
        )?;
        if self.in_mode_13h {
            writeln!(f, "--- the screen is in Mode 13h; its text memory holds")?;
        } else {
            writeln!(f, "--- the screen")?;
        }
        write!(f, "{}", self.screen)?;
        writeln!(f, "--- the last lines of port E9h")?;
        for line in &self.port {
            writeln!(f, "{line}")?;
        }
        Ok(())
    }
}

impl Guest {
    /// A machine with the built-in BIOS, powered on, with each disk of
    /// `disks` in the drive its BIOS drive number names. No key is ever
    /// pressed on its keyboard: the sender of its keys is gone at once.
    pub(crate) fn power_on(disks: Vec<(u8, DiskImage)>) -> Self {
        let mut drives = DiskController::new();
        for (drive, image) in disks {
            drives.attach(drive, image);
        }
        let (_, keys) = mpsc::channel();
        let mut runtime =
            Runtime::with_drives(drives, None, false, KeyboardController::with_receiver(keys));
        runtime
            .load_rom_image(BIOS_ROM, BIOS_ROM_NAME)
            .expect("the built-in BIOS fits the ROM socket");
        Self {
            runtime,
            nanos: 0,
            read: 0,
        }
    }

    /// Runs a batch, and credits the guest with the time its instructions
    /// took, and with the wait of a guest that halted.
    pub(crate) fn step(&mut self) {
        let before = self.runtime.instruction_count;
        self.runtime.run(BATCH);
        let ran = self.runtime.instruction_count - before;
        let idle = if self.runtime.cpu.halted {
            IDLE_NANOS
        } else {
            0
        };
        let nanos = ran * NANOS_PER_INSTRUCTION + idle;
        self.runtime.credit_pit_time(nanos);
        self.nanos += nanos;
    }

    /// The instructions the guest has run since it was powered on.
    pub(crate) fn instructions(&self) -> u64 {
        self.runtime.instruction_count
    }

    /// The guest time that has passed since power-on.
    pub(crate) fn time(&self) -> Duration {
        Duration::from_nanos(self.nanos)
    }

    /// The lines the guest finished on the port E9h since the last call:
    /// those the port kept, and how many before them it no longer kept, if
    /// the guest finished more of them than the port keeps.
    pub(crate) fn new_lines(&mut self) -> PortLines {
        let console = self
            .runtime
            .debug_console()
            .expect("the machine has the port E9h");
        let finished = console.lines_finished();
        let fresh = finished - self.read;
        self.read = finished;
        let held = console.lines().len();
        let lost = fresh.saturating_sub(held as u64);
        let kept = console
            .lines()
            .skip(held - (fresh - lost) as usize)
            .map(str::to_string)
            .collect();
        PortLines { lost, kept }
    }

    /// The text screen, a line a row, each without the blanks at its end.
    pub(crate) fn screen(&self) -> String {
        let memory = &self.runtime.cpu.memory;
        let mut screen = String::new();
        for row in 0..25 {
            let line: String = (0..80)
                .map(|column| {
                    let byte = memory.read_byte(TEXT_SCREEN + (row * 80 + column) * 2);
                    if byte == b' ' || byte.is_ascii_graphic() {
                        byte as char
                    } else {
                        ' '
                    }
                })
                .collect();
            screen.push_str(line.trim_end());
            screen.push('\n');
        }
        screen
    }

    /// Whether the VGA is in Mode 13h.
    pub(crate) fn in_mode_13h(&self) -> bool {
        let vga = self.runtime.vga.as_ref().expect("the machine has a VGA");
        vga.borrow().video_mode == VideoMode::Mode13h
    }

    /// The FNV-1a hash of the picture of Mode 13h: its 64,000 bytes.
    pub(crate) fn picture_hash(&self) -> u64 {
        let memory = &self.runtime.cpu.memory;
        fnv1a((PICTURE..PICTURE + PICTURE_BYTES).map(|address| memory.read_byte(address)))
    }

    /// The hash of a picture of Mode 13h all of colour 0 (`picture_hash`).
    pub(crate) fn blank_picture_hash() -> u64 {
        fnv1a(std::iter::repeat_n(0, PICTURE_BYTES))
    }

    /// The contents of the disk in `drive`, as the guest left them.
    pub(crate) fn disk(&self, drive: u8) -> Option<Vec<u8>> {
        let mut disks = self.runtime.disks.borrow_mut();
        let (_, image) = disks
            .drives
            .iter_mut()
            .find(|(number, _)| *number == drive)?;
        image.contents().ok()
    }

    /// What the guest shows now, for a failure to show.
    pub(crate) fn stopped(&self) -> Box<Stopped> {
        let console = self
            .runtime
            .debug_console()
            .expect("the machine has the port E9h");
        let skipped = console.lines().len().saturating_sub(PORT_TAIL);
        let mut port: Vec<String> = console.lines().skip(skipped).map(str::to_string).collect();
        if !console.line_in_progress().is_empty() {
            port.push(console.line_in_progress().to_string());
        }
        let registers = &self.runtime.cpu.registers;
        Box::new(Stopped {
            instructions: self.instructions(),
            time: self.time(),
            cs: registers.cs.reg().word(),
            ip: registers.pc.word(),
            in_mode_13h: self.in_mode_13h(),
            screen: self.screen(),
            port,
        })
    }

    /// Runs batches until `done` says the guest is where a stage wants it,
    /// or until `budget` more of its time has passed. `done` is shown the
    /// guest and the lines it finished on the port in the batch: those the
    /// port kept, after a line that says how many were lost before them,
    /// if the port did not keep them all. Returns the instructions the
    /// guest had run when it was done, or what it showed when its time was
    /// up.
    pub(crate) fn run_until(
        &mut self,
        budget: Duration,
        mut done: impl FnMut(&Guest, &[String]) -> bool,
    ) -> Result<u64, Box<Stopped>> {
        let end = self.time() + budget;
        loop {
            self.step();
            let PortLines { lost, kept } = self.new_lines();
            let lines: Vec<String> = (lost > 0)
                .then(|| format!("[{lost} lines of port E9h lost]"))
                .into_iter()
                .chain(kept)
                .collect();
            if done(self, &lines) {
                return Ok(self.instructions());
            }
            if self.time() >= end {
                return Err(self.stopped());
            }
        }
    }
}

#[cfg(test)]
#[path = "guest_tests.rs"]
mod tests;
