//! Stage 5: the MS-DOS 4.00 floppy, `disks/msdos400.img`. A 1.44 MB floppy
//! formatted as MS-DOS 4.00's FORMAT formats one, with the build's boot
//! sector; on it MS-DOS 4.00's system, an AUTOEXEC.BAT that sets the
//! prompt, its notices (README.TXT, LICENSE.TXT) and every other file the
//! build makes but the installer, SELECT, which is made for the release's
//! own disks. It is then booted to its prompt, and checked by CHKDSK.
//!
//! The invariant: the floppy written to the output is the one that booted
//! and that CHKDSK checked.

use super::check::{self, CheckError, Checked};
use super::msdos::{self, File, PutError, SYSTEM};
use super::{Built, FLOPPY_IMAGE, Inputs};
use crate::fat;
use crate::guest::{Guest, Stopped};
use crate::notices::{self, Disk};
use crate::pins::Pins;
use crate::report::grouped;
use crate::stamp;
use emu8086_core::io::disk::DiskImage;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// The bytes of a 1.44 MB floppy: 80 cylinders of 2 heads of 18 sectors.
const FLOPPY_BYTES: usize = 1_474_560;
/// The AUTOEXEC.BAT of the floppy. A disk without one has DOS ask for the
/// date and the time first.
const AUTOEXEC: &str = "@ECHO OFF\nPROMPT $P$G\nVER\n";
/// The files of the installer, which the floppy leaves out.
const INSTALLER: &str = "SELECT.";
/// MS-DOS 4.00 is at its prompt after 0.28 million instructions.
const PROMPT_BUDGET: Duration = Duration::from_secs(5);
/// What VER shows, as AUTOEXEC.BAT runs it, and the prompt then, at the
/// root of A:.
const VERSION: &str = "MS-DOS Version 4.00";
const PROMPT: &str = "A:\\>";

/// The floppy, and how it booted.
pub(crate) struct Floppy {
    pub bytes: Vec<u8>,
    /// The files on it, and the bytes left free.
    pub files: usize,
    pub free: u64,
    /// The instructions MS-DOS 4.00 took to show its prompt.
    pub prompt_at: u64,
    /// What CHKDSK reported of it.
    pub chkdsk: String,
}

/// Why the floppy could not be made, or did not boot.
#[derive(Debug)]
pub(crate) enum Error {
    License(PathBuf, io::Error),
    Put(PutError),
    Output(PathBuf, io::Error),
    /// MS-DOS 4.00 did not show its prompt in its time.
    NoPrompt(Box<Stopped>),
    Check(CheckError),
}

impl Error {
    /// What the guest showed when the stage gave up on it, if one ran.
    pub(crate) fn guest(&self) -> Option<&Stopped> {
        match self {
            Self::NoPrompt(stopped) | Self::Check(CheckError::Unfinished(stopped)) => Some(stopped),
            _ => None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::License(path, error) => write!(f, "{} cannot be read: {error}", path.display()),
            Self::Put(error) => write!(f, "MS-DOS cannot be put on the floppy: {error}"),
            Self::Output(path, error) => write!(f, "{} cannot be written: {error}", path.display()),
            Self::NoPrompt(_) => write!(
                f,
                "the floppy did not show {VERSION}, then {PROMPT}, in {} s of guest time",
                PROMPT_BUDGET.as_secs()
            ),
            Self::Check(error) => write!(f, "the floppy: {error}"),
        }
    }
}

/// Makes the floppy of what the build made, writes it into `out`, boots it
/// and checks it.
pub(crate) fn make(
    built: &Built,
    inputs: &Inputs,
    pins: &Pins,
    out: &Path,
) -> Result<Floppy, Error> {
    let license =
        msdos::license(&inputs.msdos).map_err(|(path, error)| Error::License(path, error))?;
    let autoexec = fat::dos_text(AUTOEXEC.as_bytes());
    let readme = fat::dos_text(notices::readme(pins, Disk::Floppy).as_bytes());
    let mut files: Vec<File<'_>> = msdos::built_files(built, &SYSTEM).map_err(Error::Put)?;
    files.push(("AUTOEXEC.BAT".to_string(), &autoexec));
    files.push(("README.TXT".to_string(), &readme));
    files.push(("LICENSE.TXT".to_string(), &license));
    let mut others: Vec<File<'_>> = built
        .files
        .iter()
        .filter(|(name, _)| !SYSTEM.contains(&name.as_str()) && !name.starts_with(INSTALLER))
        .map(|(name, data)| (name.clone(), data.as_slice()))
        .collect();
    others.sort();
    files.extend(others);

    let mut bytes = vec![0; FLOPPY_BYTES];
    let options = fatfs::FormatVolumeOptions::new()
        .fat_type(fatfs::FatType::Fat12)
        .bytes_per_cluster(512)
        .total_sectors((FLOPPY_BYTES / crate::layout::SECTOR) as u32)
        .max_root_dir_entries(224)
        .media(0xF0)
        .sectors_per_track(18)
        .heads(2)
        .drive_num(0x00)
        .volume_id(stamp::FLOPPY_SERIAL);
    fatfs::format_volume(io::Cursor::new(&mut bytes[..]), options)
        .map_err(|error| Error::Put(PutError::Format(error)))?;
    let free = msdos::put_on(&mut bytes, &built.boot_sector, 0x00, |volume| {
        msdos::write(volume, &stamp::MSDOS, None, &files)
    })
    .map_err(Error::Put)?;
    let path = out.join(FLOPPY_IMAGE);
    std::fs::write(&path, &bytes).map_err(|error| Error::Output(path, error))?;
    println!(
        "  {FLOPPY_IMAGE}: {} files, {} bytes free",
        files.len(),
        grouped(free)
    );

    let image = DiskImage::from_bytes(bytes.clone(), "msdos400.img", false);
    let mut guest = Guest::power_on(vec![(0x00, image)]);
    let prompt_at = guest
        .run_until(PROMPT_BUDGET, |guest, _| {
            let screen = guest.screen();
            let mut lines = screen.lines().skip_while(|line| *line != VERSION);
            lines.any(|line| line == PROMPT)
        })
        .map_err(Error::NoPrompt)?;
    println!(
        "  booted: {PROMPT} after {} instructions",
        grouped(prompt_at)
    );
    let chkdsk = check::chkdsk(&bytes, Checked::Floppy(&bytes)).map_err(Error::Check)?;
    println!("  CHKDSK finds it clean");
    Ok(Floppy {
        bytes,
        files: files.len(),
        free,
        prompt_at,
        chkdsk,
    })
}
