//! Stage 1: the build host. FreeDOS 1.4's boot floppy, as its release has
//! it, but for two files of its own: FDCONFIG.SYS, which starts the shell
//! without the menu and the pause of the boot, and FDAUTO.BAT, which sets
//! the date and runs C:\BUILD.BAT.
//!
//! The invariant: FreeDOS is the build's host and nothing more. Nothing of
//! it reaches the disks the tool makes.

use crate::fat;
use crate::stamp;
use emu8086_core::io::disk::DiskImage;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};

/// The file of the inputs' directory the floppy is in.
pub(crate) const BOOT_FLOPPY: &str = "x86BOOT.img";
/// The bytes of a 1.44 MB floppy.
const FLOPPY_BYTES: usize = 1_474_560;

/// No F5 or F8 prompt, and no pause for one (`SWITCHES=/N /F`); the files
/// and buffers NMAKE and the tools it runs need; the shell, with an
/// environment of 4 KB, running FDAUTO.BAT.
const FDCONFIG: &str = "SWITCHES=/N /F
FILES=60
BUFFERS=20
LASTDRIVE=Z
SHELL=A:\\FREEDOS\\BIN\\COMMAND.COM A:\\FREEDOS\\BIN /E:4096 /P=A:\\FDAUTO.BAT
";

/// The emulator has no clock the guest can read the date of: FDAUTO.BAT
/// sets it, to the day after the sources' (`stamp::MSDOS`), so that what
/// the build makes is newer than what it is made of.
const FDAUTO: &str = "@ECHO OFF
SET DOSDIR=A:\\FREEDOS
PATH A:\\FREEDOS\\BIN
DATE 10-07-1988
TIME 00:00:00
C:
CALL C:\\BUILD.BAT
";

/// What FreeDOS boots from and runs its shell from.
const NEEDED: [&str; 2] = ["KERNEL.SYS", "FREEDOS/BIN/COMMAND.COM"];

/// Why the host floppy could not be made.
#[derive(Debug)]
pub(crate) enum Error {
    Unreadable(PathBuf, io::Error),
    /// The image is not of a 1.44 MB floppy: its size.
    NotAFloppy(usize),
    /// Its volume is not one of FAT12.
    NotFat12(Option<io::Error>),
    /// It lacks a file FreeDOS boots with.
    Lacks(&'static str),
    Write(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unreadable(path, error) => {
                write!(f, "{} cannot be read: {error}", path.display())
            }
            Self::NotAFloppy(size) => write!(
                f,
                "{BOOT_FLOPPY} is {size} bytes long, not {FLOPPY_BYTES}: no 1.44 MB floppy"
            ),
            Self::NotFat12(Some(error)) => write!(f, "{BOOT_FLOPPY} holds no FAT volume: {error}"),
            Self::NotFat12(None) => write!(f, "{BOOT_FLOPPY} holds no FAT12 volume"),
            Self::Lacks(file) => write!(f, "{BOOT_FLOPPY} has no {file}"),
            Self::Write(error) => write!(f, "{BOOT_FLOPPY} cannot be written to: {error}"),
        }
    }
}

/// The host floppy, made of the boot floppy in the directory `freedos`.
pub(crate) fn floppy(freedos: &Path) -> Result<DiskImage, Error> {
    let path = freedos.join(BOOT_FLOPPY);
    let mut bytes = std::fs::read(&path).map_err(|error| Error::Unreadable(path, error))?;
    if bytes.len() != FLOPPY_BYTES {
        return Err(Error::NotAFloppy(bytes.len()));
    }
    let volume =
        fat::mount(&mut bytes, &stamp::MSDOS).map_err(|error| Error::NotFat12(Some(error)))?;
    if volume.fat_type() != fatfs::FatType::Fat12 {
        return Err(Error::NotFat12(None));
    }
    {
        let root = volume.root_dir();
        for file in NEEDED {
            root.open_file(file).map_err(|_| Error::Lacks(file))?;
        }
        let config = fat::dos_text(FDCONFIG.as_bytes());
        fat::put(&root, "FDCONFIG.SYS", &config).map_err(Error::Write)?;
        let autoexec = fat::dos_text(FDAUTO.as_bytes());
        fat::put(&root, "FDAUTO.BAT", &autoexec).map_err(Error::Write)?;
    }
    volume.unmount().map_err(Error::Write)?;
    Ok(DiskImage::from_bytes(bytes, BOOT_FLOPPY, false))
}
