//! MS-DOS 4.00 put on a volume, for the floppy and the hard disk alike:
//! the volume formatted, the build's boot sector put on it, the system
//! files written first and the rest of what the disk holds after them, and
//! the system files sealed where the boot sector looks for them
//! (`layout`).
//!
//! The invariant: a volume made here boots MS-DOS 4.00 and holds every file
//! it was given. A file that does not fit is an error, never a file left
//! out.

use super::{Built, InputError};
use crate::fat;
use crate::layout::{self, LayoutError, SECTOR};
use crate::stamp::Stamp;
use std::fmt;
use std::io;
use std::path::Path;

/// The files of MS-DOS that go first onto a volume, in this order: the two
/// the boot sector loads, and the shell.
pub(crate) const SYSTEM: [&str; 3] = ["IO.SYS", "MSDOS.SYS", "COMMAND.COM"];

/// Why MS-DOS could not be put on a volume.
#[derive(Debug)]
pub(crate) enum PutError {
    Format(io::Error),
    /// A file or a directory could not be written, or did not fit.
    Write(String, io::Error),
    /// The build made no file of that name.
    NotBuilt(String),
    Layout(LayoutError),
}

impl fmt::Display for PutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Format(error) => write!(f, "the volume cannot be formatted: {error}"),
            Self::Write(name, error) => write!(f, "{name} cannot be written: {error}"),
            Self::NotBuilt(name) => write!(f, "the build made no {name}"),
            Self::Layout(error) => write!(f, "{error}"),
        }
    }
}

/// A file for a volume: its name and its bytes.
pub(crate) type File<'a> = (String, &'a [u8]);

/// The license of the source in the directory `msdos` (`v4.0/LICENSE`, the
/// MIT License), as DOS reads text: the disks' LICENSE.TXT.
pub(crate) fn license(msdos: &Path) -> Result<Vec<u8>, InputError> {
    let path = msdos.join("LICENSE");
    std::fs::read(&path)
        .map(|text| fat::dos_text(&text))
        .map_err(|error| (path, error))
}

/// The files of `built` named `names`.
pub(crate) fn built_files<'a>(built: &'a Built, names: &[&str]) -> Result<Vec<File<'a>>, PutError> {
    names
        .iter()
        .map(|name| {
            built
                .file(name)
                .map(|data| (name.to_string(), data))
                .ok_or_else(|| PutError::NotBuilt(name.to_string()))
        })
        .collect()
}

/// Writes `files` in their order into the directory `directory` of the
/// volume, which is made first, or into its root (`None`); each is stamped
/// with `stamp`, and so is the directory.
pub(crate) fn write(
    volume: &mut [u8],
    stamp: &'static Stamp,
    directory: Option<&str>,
    files: &[File<'_>],
) -> Result<(), PutError> {
    let mounted = fat::mount(volume, stamp).map_err(PutError::Format)?;
    {
        let root = mounted.root_dir();
        let into = match directory {
            Some(name) => root
                .create_dir(name)
                .map_err(|error| PutError::Write(name.to_string(), error))?,
            None => root,
        };
        for (name, data) in files {
            fat::put(&into, name, data).map_err(|error| PutError::Write(name.clone(), error))?;
        }
    }
    mounted
        .unmount()
        .map_err(|error| PutError::Write("the volume".to_string(), error))
}

/// Puts the boot sector `boot` on `volume`, a volume formatted already,
/// with `drive` as its BIOS drive; has `fill` write what the volume holds
/// (the system files first, with `write`); and seals the system files.
/// Returns the bytes left free on the volume.
pub(crate) fn put_on(
    volume: &mut [u8],
    boot: &[u8; SECTOR],
    drive: u8,
    fill: impl FnOnce(&mut [u8]) -> Result<(), PutError>,
) -> Result<u64, PutError> {
    layout::install_boot_sector(volume, boot, drive);
    fill(volume)?;
    layout::seal_system_files(volume).map_err(PutError::Layout)?;
    let mounted = fat::mount(volume, &crate::stamp::MSDOS).map_err(PutError::Format)?;
    let stats = mounted.stats().map_err(PutError::Format)?;
    Ok(u64::from(stats.free_clusters()) * u64::from(stats.cluster_size()))
}
