use std::path::PathBuf;

use clap::Parser;

use crate::io::disk_spec::{parse_boot_order, parse_disk_specs};
use crate::io::keyboard::KeyboardController;
use crate::vm::runtime::Runtime;

mod io;
mod utils;
mod vm;

#[cfg(test)]
mod cpu_tests;

#[cfg(test)]
mod bios_inspection_tests;

#[cfg(test)]
mod bios_tests;

/// The build script, compiled into the test binary so that its tests run
/// with the others.
#[cfg(test)]
#[allow(dead_code)]
#[path = "../build.rs"]
mod build_script;

#[derive(Parser, Debug)]
#[command(name = "emulator8086", about = "Intel 8086 emulator")]
struct Cli {
    /// Path to the BIOS ROM image (e.g. assets/rom/bios.bin)
    rom: PathBuf,
    /// Floppy disk: [DRIVE:]SOURCE with DRIVE a-d (default a) and SOURCE an image file,
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

/// Builds the machine the command line describes and runs it. An error is
/// something the user can fix: a bad option value or a file that cannot be
/// used.
fn run(args: Cli) -> Result<(), String> {
    let specs = parse_disk_specs(&args.disk)?;
    let boot_order = args.boot.as_deref().map(parse_boot_order).transpose()?;

    // The frontend owns the source of key events — the window in GUI builds,
    // a terminal reader thread otherwise — and feeds the keyboard controller
    // through this channel.
    let (keys, key_events) = std::sync::mpsc::channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    let mut runtime = Runtime::new(specs, args.hd, boot_order, args.trace, keyboard)?;
    runtime.load_rom(args.rom)?;

    #[cfg(feature = "gui")]
    {
        runtime.run_gui(keys);
    }
    #[cfg(not(feature = "gui"))]
    {
        KeyboardController::spawn_terminal_reader(keys);
        runtime.run();
    }
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

    fn args(line: &[&str]) -> Cli {
        Cli::try_parse_from(line).unwrap()
    }

    /// The ROM is a positional argument: there is no `--rom` option, and the
    /// help must not mention one.
    #[test]
    fn help_describes_the_options_that_exist() {
        let help = Cli::command().render_long_help().to_string();
        assert!(!help.contains("--rom"), "{}", help);
        assert!(help.contains(":ro"), "{}", help);
        assert!(Cli::try_parse_from(["emulator8086", "--rom", "bios.bin"]).is_err());
    }

    #[test]
    fn bad_option_values_are_reported_not_guessed() {
        for bad in [
            &["emulator8086", "bios.bin", "--disk", "e:foo.img"][..],
            &[
                "emulator8086",
                "bios.bin",
                "--disk",
                "a:one.img",
                "--disk",
                "a:two.img",
            ],
            &["emulator8086", "bios.bin", "--disk", "b:memory:1000"],
            &["emulator8086", "bios.bin", "--boot", "hd200"],
            &["emulator8086", "bios.bin", "--boot", "x"],
        ] {
            let error = run(args(bad)).unwrap_err();
            assert!(!error.is_empty(), "{:?}", bad);
        }
    }

    /// A missing image is an error the user can read, not a panic.
    #[test]
    fn missing_boot_disk_is_reported_with_its_path() {
        let line = ["emulator8086", "bios.bin", "--disk", "a:no-such-disk.img"];
        let error = run(args(&line)).unwrap_err();
        assert!(error.contains("no-such-disk.img"), "{}", error);
    }
}
