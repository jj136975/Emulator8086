use std::env;
use std::fs::File;
use std::path::PathBuf;

use clap::Parser;
use clap_derive::Parser;

use crate::a_out::executable::Executable;
use crate::io::disk::{DiskSource, DiskSpec};
use crate::vm::runtime::Runtime;

mod a_out;
mod vm;
mod utils;
mod minix2;
mod io;
mod bios;

#[derive(Parser, Debug)]
#[command(name = "emulator8086", about = "Intel 8086 emulator")]
struct CLI {
    /// Path to a.out executable (for MINIX mode)
    path: Option<std::path::PathBuf>,
    /// Boot in BIOS mode
    #[arg(long)]
    bios: bool,
    /// Disk specification: DRIVE:SOURCE (e.g. a:boot.img, b:memory, a:C:\path\to\image.img)
    #[arg(long, action = clap::ArgAction::Append)]
    disk: Vec<String>,
    /// Hard disk image (e.g. --hd hd0.img). Multiple --hd flags for multiple drives.
    /// Creates a 32MB image if the file doesn't exist.
    #[arg(long, action = clap::ArgAction::Append)]
    hd: Vec<String>,
    /// Enable register trace output (MINIX mode)
    #[arg(long)]
    trace: bool,
    /// Arguments to pass to the a.out executable
    args: Vec<String>,
}

fn parse_disk_spec(spec: &str) -> DiskSpec {
    // Heuristic to distinguish "a:path" (emulator drive) from "C:\path" (Windows path):
    // If first char is a letter, position 1 is ':', and position 2 is NOT '\' or '/',
    // treat as emulator drive letter + source. Otherwise it's a Windows absolute path.
    let (drive_letter, source_str) = if spec.len() >= 2
        && spec.as_bytes()[0].is_ascii_alphabetic()
        && spec.as_bytes()[1] == b':'
        && (spec.len() == 2 || (spec.as_bytes()[2] != b'\\' && spec.as_bytes()[2] != b'/'))
    {
        (spec.as_bytes()[0], &spec[2..])
    } else {
        // Windows absolute path or no drive prefix — default to drive A:
        (b'a', spec)
    };

    let drive = match drive_letter.to_ascii_lowercase() {
        b'a' => 0,
        b'b' => 1,
        b'c' => 2,
        b'd' => 3,
        other => {
            eprintln!("Unsupported drive letter '{}', defaulting to A:", other as char);
            0
        }
    };

    let source = if source_str.eq_ignore_ascii_case("memory") {
        DiskSource::Memory
    } else {
        DiskSource::FilePath(PathBuf::from(source_str))
    };

    DiskSpec { drive, source }
}

fn main() {
    env_logger::init();
    let mut args = CLI::parse();

    if args.bios {
        if args.disk.is_empty() && args.hd.is_empty() {
            eprintln!("Error: at least one --disk or --hd is required for BIOS mode");
            std::process::exit(1);
        }
        let specs: Vec<DiskSpec> = args.disk.iter().map(|s| parse_disk_spec(s)).collect();
        let mut runtime = Runtime::new_bios(&specs, &args.hd);
        runtime.run();
    } else {
        let path = args.path.expect("Path to a.out executable is required");
        args.args.insert(0, env::args().collect::<Vec<String>>()[1].clone());
        match File::open(&path) {
            Ok(mut file) => match Executable::from_reader(&mut file) {
                Ok(exe) => {
                    let mut runtime = Runtime::new(&exe, args.args);
                    runtime.trace = args.trace;
                    if args.trace {
                        eprintln!(" AX   BX   CX   DX   SP   BP   SI   DI  FLAGS IP");
                    }
                    runtime.run();
                },
                Err(error) => eprintln!("{:?}", error)
            },
            Err(error) => eprintln!("{:?}", error),
        }
    }
}
