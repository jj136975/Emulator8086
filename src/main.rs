use std::path::PathBuf;

use clap::Parser;
use clap_derive::Parser;

use crate::io::disk::{DiskSource, DiskSpec, FLOPPY_144_SIZE};
use crate::vm::runtime::Runtime;

mod io;
mod utils;
mod vm;

#[cfg(test)]
mod cpu_tests;

#[derive(Parser, Debug)]
#[command(name = "emulator8086", about = "Intel 8086 emulator")]
struct CLI {
    /// Path to boot ROM image (e.g. --rom boot.bin)
    rom: PathBuf,
    /// Disk specification: DRIVE:SOURCE (e.g. a:boot.img, b:memory, a:C:\path\to\image.img)
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
    /// Enable register trace output (MINIX mode)
    #[arg(long)]
    trace: bool
    // /// Run in headless mode (no terminal UI)
    // #[arg(long)]
    // headless: bool,
    // /// Path to write VGA screen dump (default: screen.txt when headless)
    // #[arg(long)]
    // screen_dump: Option<PathBuf>,
    // /// Path to read keystroke commands from (default: keys.txt when headless)
    // #[arg(long)]
    // key_input: Option<PathBuf>,
}

fn parse_disk_spec(spec: &str) -> DiskSpec {
    // Heuristic to distinguish "a:path" (emulator drive) from "C:\path" (Windows path):
    // If first char is a letter, position 1 is ':', and position 2 is NOT '\' or '/',
    // treat as emulator drive letter + source. Otherwise, it's a Windows absolute path.
    let (drive_letter, rest) = if spec.len() >= 2
        && spec.as_bytes()[0].is_ascii_alphabetic()
        && spec.as_bytes()[1] == b':'
        && (spec.len() == 2 || (spec.as_bytes()[2] != b'\\' && spec.as_bytes()[2] != b'/'))
    {
        (spec.as_bytes()[0], &spec[2..])
    } else {
        (b'a', spec)
    };

    let drive = match drive_letter.to_ascii_lowercase() {
        b'a' => 0,
        b'b' => 1,
        b'c' => 2,
        b'd' => 3,
        other => {
            eprintln!(
                "Unsupported drive letter '{}', defaulting to A:",
                other as char
            );
            0
        }
    };

    // Check for :ro or :rw suffix
    let (source_str, readonly) = if rest.ends_with(":ro") {
        (&rest[..rest.len() - 3], true)
    } else if rest.ends_with(":rw") {
        (&rest[..rest.len() - 3], false)
    } else {
        (rest, false)
    };

    let source = if source_str.eq_ignore_ascii_case("memory") {
        DiskSource::Memory(FLOPPY_144_SIZE)
    } else if source_str.len() > 7 && source_str[..7].eq_ignore_ascii_case("memory:") {
        let size_str = &source_str[7..];
        let kb: u64 = size_str.parse().unwrap_or_else(|_| {
            eprintln!("Invalid memory size '{}', using 1440KB", size_str);
            1440
        });
        DiskSource::Memory(kb * 1024)
    } else {
        DiskSource::FilePath(PathBuf::from(source_str))
    };

    DiskSpec {
        drive,
        source,
        readonly,
    }
}

/// Parse a boot order string like "hd0,a,hd1" into BIOS drive numbers.
fn parse_boot_order(spec: &str) -> Vec<u8> {
    spec.split(',')
        .filter_map(|s| {
            let s = s.trim().to_ascii_lowercase();
            if let Some(rest) = s.strip_prefix("hd") {
                let idx: u8 = rest.parse().unwrap_or_else(|_| {
                    eprintln!("Invalid HD index in boot order: '{}', using hd0", s);
                    0
                });
                Some(0x80 + idx)
            } else if s.len() == 1 && s.as_bytes()[0].is_ascii_alphabetic() {
                Some(s.as_bytes()[0] - b'a')
            } else {
                eprintln!("Unknown boot device: '{}'", s);
                None
            }
        })
        .collect()
}

fn main() {
    env_logger::init();
    let args = CLI::parse();

    let specs: Vec<DiskSpec> = args.disk.iter().map(|s| parse_disk_spec(s)).collect();
    let boot_order = args.boot.as_deref().map(parse_boot_order);

    // let headless_config = if args.headless {
    //     Some(HeadlessConfig {
    //         screen_dump_path: args.screen_dump.unwrap_or_else(|| PathBuf::from("screen.txt")),
    //         key_input_path: args.key_input.unwrap_or_else(|| PathBuf::from("keys.txt")),
    //     })
    // } else {
    //     None
    // };

    let mut runtime = Runtime::new(specs, args.hd, boot_order);
    runtime.load_rom(args.rom);
    runtime.run();
}
