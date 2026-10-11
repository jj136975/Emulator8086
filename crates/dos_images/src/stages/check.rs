//! The check of a disk the tool made, by MS-DOS 4.00's own CHKDSK. The
//! checking floppy is a copy of the MS-DOS 4.00 floppy whose AUTOEXEC.BAT
//! runs CHKDSK on the disk, into a file, then tells the host on the port
//! E9h (E9.COM). The disk checked is in its drive write-protected, as it
//! is published, byte for byte.
//!
//! The invariant: a disk is clean when every line CHKDSK reports is one of
//! its summary's (`unclean_lines`): the size of the volume, what its files
//! take, its allocation units and the memory. Whatever else CHKDSK says is
//! an error it found.

use crate::fat;
use crate::guest::{Guest, Stopped};
use crate::markers::{E9_COM, Marker};
use crate::stamp;
use emu8086_core::io::disk::DiskImage;
use std::fmt;
use std::io;
use std::time::Duration;

/// MS-DOS 4.00 boots from the floppy and CHKDSK checks a volume in a
/// tenth of a second of guest time.
const CHECK_BUDGET: Duration = Duration::from_secs(30);
/// Where CHKDSK reports, on the checking floppy.
const REPORT: &str = "CHKDSK.TXT";

/// Why a check could not tell that a disk is clean.
#[derive(Debug)]
pub(crate) enum CheckError {
    /// The checking floppy could not be made.
    Floppy(io::Error),
    /// The guest did not end the check in its time.
    Unfinished(Box<Stopped>),
    /// CHKDSK left no report.
    NoReport(io::Error),
    /// CHKDSK reported what is not its summary of a clean volume.
    Unclean { report: String, lines: Vec<String> },
}

impl CheckError {
    /// What CHKDSK reported, if it reported what a clean volume has not.
    pub(crate) fn report(&self) -> Option<&str> {
        match self {
            Self::Unclean { report, .. } => Some(report),
            _ => None,
        }
    }
}

impl fmt::Display for CheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Floppy(error) => write!(f, "the checking floppy cannot be made: {error}"),
            Self::Unfinished(_) => write!(
                f,
                "CHKDSK did not end in {} s of guest time",
                CHECK_BUDGET.as_secs()
            ),
            Self::NoReport(error) => write!(f, "CHKDSK left no report: {error}"),
            Self::Unclean { lines, .. } => {
                write!(
                    f,
                    "CHKDSK reports what a clean disk has not: {}",
                    lines.join(" | ")
                )
            }
        }
    }
}

/// The lines of CHKDSK's `report` that are not of its summary of a clean
/// volume. A report without the volume's size is no summary at all.
pub(crate) fn unclean_lines(report: &str) -> Vec<String> {
    const SUMMARY: [&str; 7] = [
        "bytes total disk space",
        "bytes available on disk",
        "bytes in each allocation unit",
        "total allocation units on disk",
        "available allocation units on disk",
        "total bytes memory",
        "bytes free",
    ];
    const FILES: [&str; 3] = ["hidden files", "directories", "user files"];
    let is_number = |word: &str| !word.is_empty() && word.bytes().all(|b| b.is_ascii_digit());
    let mut unclean = Vec::new();
    for line in report
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
    {
        let summary = line.starts_with("Volume Serial Number is ")
            || (line.starts_with("Volume ") && line.contains(" created "))
            || line.split_once(' ').is_some_and(|(number, rest)| {
                is_number(number)
                    && (SUMMARY.contains(&rest.trim())
                        || rest.trim().strip_prefix("bytes in ").is_some_and(|files| {
                            files.split_once(' ').is_some_and(|(count, kind)| {
                                is_number(count) && FILES.contains(&kind)
                            })
                        }))
            });
        if !summary {
            unclean.push(line.to_string());
        }
    }
    if !report.contains(SUMMARY[0]) {
        unclean.push(format!("(no line of \"{}\")", SUMMARY[0]));
    }
    unclean
}

/// A disk to check, its bytes as published.
pub(crate) enum Checked<'a> {
    /// A floppy, which is checked in drive B:.
    Floppy(&'a [u8]),
    /// A hard disk, which is checked as C:.
    HardDisk(&'a [u8]),
}

/// Boots MS-DOS 4.00 from a copy of `floppy`, the MS-DOS 4.00 floppy, has
/// CHKDSK check the volume of `checked`, which is in its drive
/// write-protected, and returns what CHKDSK reported of a clean volume.
pub(crate) fn chkdsk(floppy: &[u8], checked: Checked<'_>) -> Result<String, CheckError> {
    let (drive, unit, disk) = match checked {
        Checked::Floppy(bytes) => (
            "B:",
            0x01,
            DiskImage::from_bytes(bytes.to_vec(), "checked.img", true),
        ),
        Checked::HardDisk(bytes) => (
            "C:",
            0x80,
            DiskImage::hard_disk_from_bytes(bytes.to_vec(), "checked.img", true),
        ),
    };
    let mut checking = floppy.to_vec();
    {
        let volume = fat::mount(&mut checking, &stamp::MSDOS).map_err(CheckError::Floppy)?;
        {
            let root = volume.root_dir();
            fat::put(&root, "E9.COM", &E9_COM).map_err(CheckError::Floppy)?;
            let autoexec = format!(
                "@ECHO OFF\nCHKDSK {drive} > A:\\{REPORT}\nA:\\E9 {}\n",
                Marker::ChecksDone.words()
            );
            let autoexec = fat::dos_text(autoexec.as_bytes());
            fat::put(&root, "AUTOEXEC.BAT", &autoexec).map_err(CheckError::Floppy)?;
        }
        volume.unmount().map_err(CheckError::Floppy)?;
    }
    let checking = DiskImage::from_bytes(checking, "checking.img", false);
    let mut guest = Guest::power_on(vec![(0x00, checking), (unit, disk)]);
    guest
        .run_until(CHECK_BUDGET, |_, lines| {
            lines
                .iter()
                .any(|line| Marker::parse(line) == Some(Marker::ChecksDone))
        })
        .map_err(CheckError::Unfinished)?;

    let mut checking = guest.disk(0x00).unwrap_or_default();
    let volume = fat::mount(&mut checking, &stamp::MSDOS).map_err(CheckError::NoReport)?;
    let report = fat::get(&volume.root_dir(), REPORT).map_err(CheckError::NoReport)?;
    let report = String::from_utf8_lossy(&report).into_owned();
    let lines = unclean_lines(&report);
    if lines.is_empty() {
        Ok(report)
    } else {
        Err(CheckError::Unclean { report, lines })
    }
}

#[cfg(test)]
#[path = "check_tests.rs"]
mod tests;
