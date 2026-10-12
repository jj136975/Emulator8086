//! The stages of the tool, in the order `run` runs them, each a module of
//! its own with its own error type:
//!
//! 1. `host`: FreeDOS's boot floppy, the build host.
//! 2. `build_disk`: the build disk, the source on it with BUILD.BAT.
//! 3. `build`: FreeDOS builds MS-DOS 4.00 on it in the emulator.
//! 4. `collect`: what the build made, checked, and its files into `bin`.
//! 5. `floppy`: the MS-DOS 4.00 floppy, booted to its prompt and checked.
//! 6. `hard_disk`: the hard disk that starts Doom8088, booted to its demo
//!    and checked.
//! 7. the report (`crate::report`).
//!
//! `msdos` puts MS-DOS 4.00 on a volume for the floppy and the hard disk,
//! and `check` has DOS 4's CHKDSK check each.
//!
//! The invariant: a stage takes what the stages before it made, and the
//! inputs, and hands what it made to those after it; only a stage that
//! makes an output of the tool writes into the output directory, and only
//! once the output has passed every check of the stage: a run writes no
//! output it has not checked, and it begins by removing those of an earlier
//! run, so that what it leaves there is what it made and checked.

pub(crate) mod build;
pub(crate) mod build_disk;
mod check;
pub(crate) mod collect;
pub(crate) mod floppy;
pub(crate) mod hard_disk;
pub(crate) mod host;
mod msdos;

pub(crate) use build::Build;
pub(crate) use collect::{Built, build_log};
pub(crate) use floppy::Floppy;
pub(crate) use hard_disk::DoomDisk;

use crate::failure::Failure;
use crate::pins::Pins;
use crate::report::Report;
use std::path::{Path, PathBuf};
use std::time::Instant;

/// The directories the inputs were put in (`fetch-inputs.sh`).
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Inputs {
    /// FreeDOS 1.4's boot floppy, `x86BOOT.img`.
    pub freedos: PathBuf,
    /// The directory `v4.0` of MS-DOS 4.00's restored source.
    pub msdos: PathBuf,
    /// D8M13H.EXE, DOOM1.WAD and README.TXT of Doom8088's release, and its
    /// GPL text, COPYING.
    pub doom8088: PathBuf,
}

/// Where the outputs go, below the output directory.
pub(crate) const FLOPPY_IMAGE: &str = "disks/msdos400.img";
pub(crate) const HARD_DISK_IMAGE: &str = "disks/doom8088.img";
pub(crate) const BUILT_FILES: &str = "bin";
pub(crate) const REPORT: &str = "report.txt";
/// The build disk as a failed build left it.
pub(crate) const FAILED_BUILD_DISK: &str = "failed-build.img";

/// Removes from `out` the outputs an earlier run may have left there: the
/// disks, the files of `bin`, the report and a failed build's disk. A run
/// that fails then leaves none of them, where a check by hand of the disks
/// would pass on the earlier run's.
fn clear_outputs(out: &Path) -> Result<(), Failure> {
    let bin = out.join(BUILT_FILES);
    let mut stale: Vec<PathBuf> = [FLOPPY_IMAGE, HARD_DISK_IMAGE, REPORT, FAILED_BUILD_DISK]
        .iter()
        .map(|name| out.join(name))
        .collect();
    for entry in std::fs::read_dir(&bin).map_err(|error| Failure::Output(bin.clone(), error))? {
        stale.push(
            entry
                .map_err(|error| Failure::Output(bin.clone(), error))?
                .path(),
        );
    }
    for path in stale {
        if let Err(error) = std::fs::remove_file(&path)
            && error.kind() != std::io::ErrorKind::NotFound
        {
            return Err(Failure::Output(path, error));
        }
    }
    Ok(())
}

/// Runs the stages on `inputs`, into the directory `out`, and returns the
/// report it wrote there.
pub(crate) fn run(inputs: &Inputs, out: &Path) -> Result<Report, Failure> {
    let started = Instant::now();
    let pins = Pins::compiled_in();
    for directory in [out.join("disks"), out.join(BUILT_FILES)] {
        std::fs::create_dir_all(&directory).map_err(|error| Failure::Output(directory, error))?;
    }
    clear_outputs(out)?;

    println!("Stage 1: the FreeDOS host floppy");
    let host = host::floppy(&inputs.freedos).map_err(Failure::Host)?;
    println!("Stage 2: the build disk");
    let disk = build_disk::make(&inputs.msdos).map_err(Failure::BuildDisk)?;
    println!(
        "  {} modules, {} files of source",
        disk.modules.len(),
        disk.sources
    );
    println!("Stage 3: the build");
    let (build, build_disk) = build::run(host, disk).map_err(Failure::Build)?;
    println!("Stage 4: what the build made");
    let built = collect::collect(&build_disk, out).map_err(|error| Failure::Collect {
        error,
        disk: build_disk,
    })?;
    println!("Stage 5: the MS-DOS 4.00 floppy");
    let floppy = floppy::make(&built, inputs, &pins, out).map_err(Failure::Floppy)?;
    println!("Stage 6: the hard disk with Doom8088");
    let doom_disk =
        hard_disk::make(&built, &floppy, inputs, &pins, out).map_err(Failure::HardDisk)?;

    println!("Stage 7: the report");
    let report = Report {
        pins,
        build,
        built,
        floppy,
        doom_disk,
        seconds: started.elapsed().as_secs_f64(),
    };
    let path = out.join(REPORT);
    std::fs::write(&path, report.to_string()).map_err(|error| Failure::Output(path, error))?;
    Ok(report)
}

#[cfg(test)]
mod tests;
