//! Stage 4: what the build made, taken off the build disk: the files
//! CPY.BAT copied into C:\BIN, which go into the output's `bin`; the boot
//! sector the BOOT module assembled and FDISK's master boot record; and
//! BUILD.LOG, which must tell of no error.
//!
//! The invariant: what goes on to the disks is exactly what CPY.BAT
//! copies, every file of it and nothing else, from a build whose log is
//! clean. Nothing goes into `bin` before all of it has passed its checks,
//! the boot sector and the master boot record too: what fails one writes
//! no file there.

use crate::fat;
use crate::layout::{self, LayoutError, SECTOR};
use crate::log_scan::{self, Finding};
use crate::makefiles;
use crate::stamp;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};

/// What the build made.
pub(crate) struct Built {
    /// The files of CPY.BAT, by name, in its order.
    pub files: Vec<(String, Vec<u8>)>,
    /// The boot sector of the BOOT module, and FDISK's master boot record.
    pub boot_sector: [u8; SECTOR],
    pub master_boot_record: [u8; SECTOR],
    /// How many bytes BUILD.LOG holds, as the guest wrote them, and the
    /// warnings of the tools in it.
    pub log_bytes: usize,
    pub warnings: usize,
}

impl Built {
    /// The file of that name the build made, if CPY.BAT copies one.
    pub(crate) fn file(&self, name: &str) -> Option<&[u8]> {
        self.files
            .iter()
            .find(|(file, _)| file == name)
            .map(|(_, data)| data.as_slice())
    }
}

/// Why what the build made is not to be taken.
#[derive(Debug)]
pub(crate) enum Error {
    /// The build disk, or a file on it, cannot be read.
    Unreadable(&'static str, io::Error),
    /// BUILD.LOG tells of errors.
    LogErrors(Vec<Finding>),
    /// Files of CPY.BAT that are not in C:\BIN.
    Missing(Vec<String>),
    /// Files in C:\BIN that CPY.BAT does not copy.
    Unexpected(Vec<String>),
    /// The boot sector or the master boot record is not what it should be.
    Layout(LayoutError),
    Output(PathBuf, io::Error),
}

impl Error {
    /// The module the first error of BUILD.LOG is in, for the failure to
    /// show its part of the log.
    pub(crate) fn module(&self) -> Option<&str> {
        match self {
            Self::LogErrors(findings) => findings.first().map(|finding| finding.module.as_str()),
            _ => None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unreadable(what, error) => write!(f, "{what} cannot be read: {error}"),
            Self::LogErrors(findings) => {
                write!(f, "BUILD.LOG tells of {} errors:", findings.len())?;
                for finding in findings {
                    write!(f, " [{}] {}", finding.module, finding.line)?;
                }
                Ok(())
            }
            Self::Missing(files) => write!(f, "the build did not make {}", files.join(", ")),
            Self::Unexpected(files) => {
                write!(
                    f,
                    "C:\\BIN holds what CPY.BAT does not copy: {}",
                    files.join(", ")
                )
            }
            Self::Layout(error) => write!(f, "{error}"),
            Self::Output(path, error) => write!(f, "{} cannot be written: {error}", path.display()),
        }
    }
}

/// BUILD.LOG of the build disk `disk`, if it has one.
pub(crate) fn build_log(disk: &[u8]) -> Option<String> {
    let volume = fat::mount_to_read(layout::volume_of(disk), &stamp::MSDOS).ok()?;
    let log = fat::get(&volume.root_dir(), "BUILD.LOG").ok()?;
    Some(String::from_utf8_lossy(&log).into_owned())
}

/// What the build made on the build disk `disk`, checked; its files then
/// go into the directory `bin` of `out`.
pub(crate) fn collect(disk: &[u8], out: &Path) -> Result<Built, Error> {
    let volume = fat::mount_to_read(layout::volume_of(disk), &stamp::MSDOS)
        .map_err(|error| Error::Unreadable("the build disk", error))?;
    let root = volume.root_dir();
    let read =
        |path: &'static str| fat::get(&root, path).map_err(|error| Error::Unreadable(path, error));

    let raw_log = read("BUILD.LOG")?;
    let log = String::from_utf8_lossy(&raw_log);
    let findings = log_scan::errors(&log);
    if !findings.is_empty() {
        return Err(Error::LogErrors(findings));
    }
    let warnings = log_scan::warnings(&log);

    let copied = makefiles::copied_files(&String::from_utf8_lossy(&read("SRC/CPY.BAT")?));
    let bin = root
        .open_dir("BIN")
        .map_err(|error| Error::Unreadable("BIN", error))?;
    let present = fat::file_names(&bin).map_err(|error| Error::Unreadable("BIN", error))?;
    let missing: Vec<String> = copied
        .iter()
        .filter(|name| !present.contains(name))
        .cloned()
        .collect();
    if !missing.is_empty() {
        return Err(Error::Missing(missing));
    }
    let unexpected: Vec<String> = present
        .iter()
        .filter(|name| !copied.contains(name))
        .cloned()
        .collect();
    if !unexpected.is_empty() {
        return Err(Error::Unexpected(unexpected));
    }

    let mut files = Vec::new();
    for name in copied {
        let data = fat::get(&bin, &name).map_err(|error| Error::Unreadable("BIN", error))?;
        files.push((name, data));
    }
    let boot_sector = layout::boot_sector(&read("SRC/BOOT/MSBOOT.BIN")?).map_err(Error::Layout)?;
    let master_boot_record =
        layout::master_boot_record(&read("SRC/CMD/FDISK/FDBOOT.BIN")?).map_err(Error::Layout)?;

    let directory = out.join(super::BUILT_FILES);
    for (name, data) in &files {
        let path = directory.join(name);
        std::fs::write(&path, data).map_err(|error| Error::Output(path, error))?;
    }
    println!("  {}/: {} files", super::BUILT_FILES, files.len());
    Ok(Built {
        files,
        boot_sector,
        master_boot_record,
        log_bytes: raw_log.len(),
        warnings,
    })
}
