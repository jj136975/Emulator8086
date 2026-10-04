//! The app: the command line and start-up around the machine, which is the
//! `emu8086_core` crate. `Cli` (its doc comments are the `--help` text),
//! `machine`, which builds the machine the options describe and loads its
//! ROM (the image the command line names, or the core's built-in BIOS), and
//! `run`, which hands that machine to the window (`gui`). The machine reads
//! no clock: the window's pacer tells it the time that passes, and
//! `wall_clock` is the clock its disks are formatted by.
//!
//! The invariant: whatever the user can fix (a bad option value, a disk
//! image or ROM that cannot be used) is an `Err` from `run` carrying a
//! message for the user, which `main` prints before exiting with status 1,
//! never a panic.

use std::path::PathBuf;
use std::sync::mpsc::Sender;

use clap::Parser;

use emu8086_core::BIOS_ROM;
use emu8086_core::io::disk_spec::{parse_boot_order, parse_disk_specs};
use emu8086_core::io::keyboard::{EmulatorEvent, KeyboardController};
use emu8086_core::vm::runtime::Runtime;

use crate::wall_clock::HostClock;

mod gui;
mod wall_clock;

#[derive(Parser, Debug)]
#[command(name = "emulator8086", about = "Intel 8086 emulator")]
struct Cli {
    /// Path to a BIOS ROM image to run in place of the built-in BIOS
    rom: Option<PathBuf>,
    /// Floppy disk: SOURCE or DRIVE:SOURCE, with DRIVE a-d (default a) and SOURCE an image file,
    /// `memory` or `memory:KB` (e.g. a:boot.img, b:memory, a:C:\path\to\image.img).
    /// Append :ro to write-protect the disk.
    #[arg(long, action = clap::ArgAction::Append)]
    disk: Vec<String>,
    /// Hard disk image (e.g. --hd hd0.img). Multiple --hd flags for multiple drives.
    /// Creates a 32MB image if the file doesn't exist.
    #[arg(long, action = clap::ArgAction::Append)]
    hd: Vec<String>,
    /// Boot order: comma-separated list of drives (e.g. --boot hd0,a or --boot a,hd0).
    /// Default: a,hd0 (floppy first, then hard disk).
    #[arg(long)]
    boot: Option<String>,
    /// Enable register trace output
    #[arg(long)]
    trace: bool,
}

/// Builds the machine the command line describes, with its ROM loaded: the
/// image the command line names, or the built-in BIOS. The sender feeds the
/// machine's keyboard. An error is something the user can fix: a bad option
/// value or a file that cannot be used.
fn machine(args: Cli) -> Result<(Runtime, Sender<EmulatorEvent>), String> {
    let specs = parse_disk_specs(&args.disk)?;
    let boot_order = args.boot.as_deref().map(parse_boot_order).transpose()?;

    // The window is the source of key events and feeds the keyboard
    // controller through this channel.
    let (keys, key_events) = std::sync::mpsc::channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    let mut runtime = Runtime::new(specs, args.hd, boot_order, args.trace, keyboard, &HostClock)?;
    match args.rom {
        Some(path) => runtime.load_rom(path)?,
        None => runtime.load_rom_image(BIOS_ROM),
    }
    Ok((runtime, keys))
}

/// Builds the machine the command line describes and runs it in the window.
fn run(args: Cli) -> Result<(), String> {
    let (runtime, keys) = machine(args)?;
    gui::run(runtime, keys);
    Ok(())
}

fn main() {
    env_logger::init();
    if let Err(message) = run(Cli::parse()) {
        eprintln!("error: {}", message);
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::CommandFactory;
    use std::ffi::OsString;

    fn args(line: &[&str]) -> Cli {
        Cli::try_parse_from(line).unwrap()
    }

    /// The ROM is an optional positional argument: there is no `--rom`
    /// option, and the help must not mention one.
    #[test]
    fn help_describes_the_options_that_exist() {
        let help = Cli::command().render_long_help().to_string();
        assert!(!help.contains("--rom"), "{}", help);
        assert!(help.contains("[ROM]"), "{}", help);
        assert!(help.contains(":ro"), "{}", help);
        assert!(Cli::try_parse_from(["emulator8086", "--rom", "bios.bin"]).is_err());
    }

    #[test]
    fn bad_option_values_are_reported_not_guessed() {
        for bad in [
            &["emulator8086", "--disk", "e:foo.img"][..],
            &["emulator8086", "--disk", "a:one.img", "--disk", "a:two.img"],
            &["emulator8086", "--disk", "b:memory:1000"],
            &["emulator8086", "--boot", "hd200"],
            &["emulator8086", "--boot", "x"],
            &["emulator8086", "bios.bin", "--boot", "x"],
        ] {
            let error = machine(args(bad)).unwrap_err();
            assert!(!error.is_empty(), "{:?}", bad);
        }
    }

    /// A missing image is an error the user can read, not a panic.
    #[test]
    fn missing_boot_disk_is_reported_with_its_path() {
        let line = ["emulator8086", "--disk", "a:no-such-disk.img"];
        let error = machine(args(&line)).unwrap_err();
        assert!(error.contains("no-such-disk.img"), "{}", error);
    }

    /// Without a ROM argument the machine runs the BIOS built into the
    /// binary; with one, it runs the image in that file instead.
    #[test]
    fn rom_argument_replaces_the_built_in_bios() {
        let rom_region = |rt: &Runtime| -> Vec<u8> {
            (0xF0000..0x100000)
                .map(|address| rt.cpu.memory.read_byte(address))
                .collect()
        };

        let (rt, _keys) = machine(args(&["emulator8086"])).unwrap();
        assert!(rom_region(&rt) == BIOS_ROM, "the built-in BIOS is loaded");

        // An 8 KB image: it ends at the top of the region, nothing below it.
        let path = std::env::temp_dir().join(format!("emu8086-cli-rom-{}.bin", std::process::id()));
        std::fs::write(&path, [0x5A; 0x2000]).unwrap();
        let line = [
            OsString::from("emulator8086"),
            path.clone().into_os_string(),
        ];
        let (rt, _keys) = machine(Cli::try_parse_from(line).unwrap()).unwrap();
        let _ = std::fs::remove_file(&path);
        let region = rom_region(&rt);
        assert!(region[..0xE000].iter().all(|&byte| byte == 0x00));
        assert!(region[0xE000..].iter().all(|&byte| byte == 0x5A));
    }

    /// A ROM file that cannot be used is an error the user can read.
    #[test]
    fn unusable_rom_file_is_reported_with_its_path() {
        let error = machine(args(&["emulator8086", "no-such-rom.bin"])).unwrap_err();
        assert!(error.contains("no-such-rom.bin"), "{}", error);
    }
}
