//! The app: start-up around the machine, which is the `emu8086_core` crate.
//! `Options` is what a start asks of the machine, and the host says what
//! that is (`platform::options`: on a desktop, the command line).
//! `machine` builds the machine the options describe and loads its ROM (the
//! image they name, or the core's built-in BIOS), and `run` hands that
//! machine to the window (`gui`). The machine reads no clock: the window's
//! pacer tells it the time that passes, and `wall_clock` is the clock its
//! disks are formatted by. What depends on the host is in `platform`, and
//! `site` is the table of the files the site publishes beside the page.
//!
//! The invariant: whatever the user can fix (a bad option value, a disk
//! image or ROM that cannot be used) is an `Err` from `run` carrying a
//! message for the user, which `main` hands to the host (`platform::fail`),
//! never a panic. What the opening of the disks did without being asked in
//! so many words (an image created, a disk cut short) is told the same way
//! (`platform::notice`): the machine prints nothing itself.

use std::path::PathBuf;
use std::sync::mpsc::Sender;

use emu8086_core::io::disk_spec::{open_drives, parse_boot_order, parse_disk_specs};
use emu8086_core::io::keyboard::{EmulatorEvent, KeyboardController};
use emu8086_core::vm::runtime::Runtime;
use emu8086_core::{BIOS_ROM, BIOS_ROM_NAME};

use crate::wall_clock::HostClock;

mod gui;
mod platform;
mod site;
mod wall_clock;

/// What a start asks of the machine. The default asks for nothing: the
/// built-in BIOS, no disk, the default boot order.
#[derive(Debug, Default)]
pub struct Options {
    /// A BIOS ROM image to run in place of the built-in BIOS.
    pub rom: Option<PathBuf>,
    /// The floppy disks, each as `[DRIVE:]SOURCE[:ro]`
    /// (`io::disk_spec::parse_disk_specs`).
    pub disk: Vec<String>,
    /// The hard disk images, in the order of their drive numbers.
    pub hd: Vec<String>,
    /// The boot order, as a comma-separated list of drives
    /// (`io::disk_spec::parse_boot_order`).
    pub boot: Option<String>,
    /// Whether every instruction is traced to the debug log.
    pub trace: bool,
}

/// Builds the machine `options` describe, with its ROM loaded: the image
/// they name, or the built-in BIOS. The sender feeds the machine's
/// keyboard. An error is something the user can fix: a bad option value or
/// a file that cannot be used.
fn machine(options: Options) -> Result<(Runtime, Sender<EmulatorEvent>), String> {
    let specs = parse_disk_specs(&options.disk)?;
    let boot_order = options.boot.as_deref().map(parse_boot_order).transpose()?;
    let drives = open_drives(specs, &options.hd, &HostClock)?;
    for note in &drives.notes {
        platform::notice(note);
    }

    // The window is the source of key events and feeds the keyboard
    // controller through this channel.
    let (keys, key_events) = std::sync::mpsc::channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    let mut runtime = Runtime::with_drives(drives.controller, boot_order, options.trace, keyboard);
    match options.rom {
        Some(path) => runtime.load_rom(path)?,
        None => runtime.load_rom_image(BIOS_ROM, BIOS_ROM_NAME)?,
    }
    Ok((runtime, keys))
}

/// Builds the machine `options` describe and runs it in the window.
fn run(options: Options) -> Result<(), String> {
    let (runtime, keys) = machine(options)?;
    gui::run(runtime, keys);
    Ok(())
}

fn main() {
    platform::init();
    if let Err(message) = run(platform::options()) {
        platform::fail(&message);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A start that asks for nothing gets the built-in BIOS, no disk in any
    /// drive and the default boot order: what a host without a command
    /// line starts.
    #[test]
    fn the_default_options_are_the_built_in_bios_and_no_disk() {
        let (rt, _keys) = machine(Options::default()).unwrap();
        let rom: Vec<u8> = (0xF0000..0x100000)
            .map(|address| rt.cpu.memory.read_byte(address))
            .collect();
        assert!(rom == BIOS_ROM, "the built-in BIOS is loaded");
        assert_eq!(rt.rom(), Some(BIOS_ROM_NAME));
        let disks = rt.disks.borrow();
        assert!(disks.drives.is_empty(), "no drive holds a disk");
        assert_eq!(disks.boot_order(), [0x00, 0x80], "A:, then HD0");
    }
}
