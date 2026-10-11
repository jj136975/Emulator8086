//! `dos_images`: builds MS-DOS 4.00 from its source inside the emulator,
//! and makes of what it built the two disks the site publishes: an MS-DOS
//! 4.00 boot floppy, and a hard disk on which MS-DOS 4.00 starts Doom8088.
//!
//! ```text
//! dos_images --freedos <dir> --msdos <dir> --doom8088 <dir> --out <dir>
//! ```
//!
//! The inputs are plain directories, which `fetch-inputs.sh` fills with
//! what `pins.env` pins: FreeDOS 1.4's boot floppy, the build host, which
//! is never published; the directory `v4.0` of MS-DOS 4.00's restored
//! source; and Doom8088's files, with its GPL text. Into the output
//! directory go `disks/msdos400.img` and `disks/doom8088.img`, the 62
//! files the build makes (`bin/`) and `report.txt`, in the place of any
//! files of those names.
//!
//! The stages run in order (`stages`). A stage that fails says why on
//! stdout, in an error annotation for GitHub Actions followed by what the
//! guest showed (`failure`), and the tool exits with 1.
//!
//! The invariant: what the tool makes is the same, byte for byte, on every
//! run and every host. Guest time comes from the instructions run, never
//! from a clock (`guest`); what the host writes onto a disk is stamped
//! with fixed times, and its volumes have fixed serials (`stamp`). The
//! host's clock measures how long the host took, for the report, and
//! nothing else.

mod failure;
mod fat;
mod guest;
mod layout;
mod log_scan;
mod makefiles;
mod markers;
mod notices;
mod pins;
mod report;
mod stages;
mod stamp;

use stages::Inputs;
use std::path::PathBuf;
use std::process::ExitCode;

const USAGE: &str = "\
Usage: dos_images --freedos <dir> --msdos <dir> --doom8088 <dir> --out <dir>

Builds MS-DOS 4.00 from its source in the emulator, and makes the site's
MS-DOS 4.00 floppy and its hard disk with Doom8088 of what it built.

  --freedos <dir>   the directory of FreeDOS 1.4's boot floppy, x86BOOT.img
  --msdos <dir>     the directory v4.0 of MS-DOS 4.00's restored source
  --doom8088 <dir>  the directory of Doom8088's D8M13H.EXE, DOOM1.WAD,
                    README.TXT and COPYING
  --out <dir>       where disks/msdos400.img, disks/doom8088.img, bin/ and
                    report.txt go

crates/dos_images/fetch-inputs.sh fetches the inputs, each checked against
its pin in crates/dos_images/pins.env.";

/// What the command line asks for.
#[derive(Debug, PartialEq, Eq)]
enum Asked {
    /// The usage.
    Help,
    Run {
        inputs: Inputs,
        out: PathBuf,
    },
}

/// What `args`, the command line's arguments, ask for. Each option is
/// given once, with its directory.
fn asked(args: impl IntoIterator<Item = String>) -> Result<Asked, String> {
    let (mut freedos, mut msdos, mut doom8088, mut out) = (None, None, None, None);
    let mut args = args.into_iter();
    while let Some(option) = args.next() {
        let slot = match option.as_str() {
            "-h" | "--help" => return Ok(Asked::Help),
            "--freedos" => &mut freedos,
            "--msdos" => &mut msdos,
            "--doom8088" => &mut doom8088,
            "--out" => &mut out,
            _ => return Err(format!("{option} is no option of the tool")),
        };
        let directory = args
            .next()
            .ok_or_else(|| format!("{option} names no directory"))?;
        if slot.replace(PathBuf::from(directory)).is_some() {
            return Err(format!("{option} is given twice"));
        }
    }
    let given = |slot: Option<PathBuf>, option: &str| slot.ok_or(format!("{option} is not given"));
    Ok(Asked::Run {
        inputs: Inputs {
            freedos: given(freedos, "--freedos")?,
            msdos: given(msdos, "--msdos")?,
            doom8088: given(doom8088, "--doom8088")?,
        },
        out: given(out, "--out")?,
    })
}

fn main() -> ExitCode {
    let (inputs, out) = match asked(std::env::args().skip(1)) {
        Ok(Asked::Run { inputs, out }) => (inputs, out),
        Ok(Asked::Help) => {
            println!("{USAGE}");
            return ExitCode::SUCCESS;
        }
        Err(error) => {
            eprintln!("{error}\n\n{USAGE}");
            return ExitCode::from(2);
        }
    };
    match stages::run(&inputs, &out) {
        Ok(report) => {
            println!();
            print!("{report}");
            ExitCode::SUCCESS
        }
        Err(failure) => {
            failure.print(&out);
            ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
#[path = "main_tests.rs"]
mod tests;
