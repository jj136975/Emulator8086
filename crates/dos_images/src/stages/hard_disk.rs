//! Stage 6: the hard disk that starts Doom8088, `disks/doom8088.img`. Five
//! cylinders, 2.5 MB, with FDISK's master boot record and one active FAT12
//! partition, which holds MS-DOS 4.00's system, COMMAND.COM, MEM.EXE and
//! CHKDSK.COM, a CONFIG.SYS and an AUTOEXEC.BAT that starts Doom8088, the
//! notices of MS-DOS, and in \DOOM Doom8088's Mode 13h build, its WAD, its
//! README, its GPL text (COPYING) and a note of its source (SOURCE.TXT). It
//! is then booted until Doom8088's demo plays, and checked by CHKDSK.
//!
//! The invariant: the disk written to the output is the one that booted,
//! showed Doom8088's title picture as it always has, and that CHKDSK
//! checked, and it is written only once all of that has passed: a disk
//! that fails any of it is never written.

use super::check::{self, CheckError, Checked};
use super::msdos::{self, File, PutError, SYSTEM};
use super::{Built, Floppy, HARD_DISK_IMAGE, Inputs};
use crate::fat;
use crate::guest::{Guest, Stopped};
use crate::layout::{self, HardDisk};
use crate::notices::{self, Disk};
use crate::pins::Pins;
use crate::report::grouped;
use crate::stamp;
use emu8086_core::io::disk::DiskImage;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// Five cylinders of 16 heads of 63 sectors: 2,580,480 bytes, which hold
/// all the disk holds with 600 KB to spare.
const CYLINDERS: usize = 5;
/// A primary partition of FAT12.
const FAT12: u8 = 0x01;
/// The programs of MS-DOS on the disk beside its system.
const PROGRAMS: [&str; 2] = ["MEM.EXE", "CHKDSK.COM"];
/// Fewer files and buffers than DOS's defaults leave Doom8088 595,168
/// bytes.
const CONFIG: &str = "FILES=15\nBUFFERS=10\n";
const AUTOEXEC: &str = "@ECHO OFF\nPROMPT $P$G\nCD \\DOOM\nD8M13H\n";
/// The files of Doom8088's release on the disk, as the inputs hold them;
/// the texts among them are given DOS's line ends.
const DOOM8088: [(&str, bool); 4] = [
    ("D8M13H.EXE", false),
    ("DOOM1.WAD", false),
    ("README.TXT", true),
    ("COPYING", true),
];

/// Doom8088 is in Mode 13h after 2.6 million instructions.
const MODE_13H_BUDGET: Duration = Duration::from_secs(2);
/// Its title picture comes 0.4 million instructions later.
const TITLE_BUDGET: Duration = Duration::from_secs(10);
/// Batches the title picture stays the same in before it is taken for the
/// title: 2 s of guest time.
const TITLE_BATCHES: usize = 20;
/// The FNV-1a hash of the title picture, the same since the first time
/// Doom8088 ran in the emulator, under MS-DOS 6.22.
const TITLE_HASH: u64 = 0xB66A_5DAC_EBB5_EDAE;
/// The demo starts 331.6 million instructions after power-on.
const DEMO_BUDGET: Duration = Duration::from_secs(60);

/// The hard disk, and how it booted.
pub(crate) struct DoomDisk {
    pub bytes: Vec<u8>,
    pub free: u64,
    /// The instructions after which Doom8088 was in Mode 13h, showed its
    /// title picture, and began its demo.
    pub mode_13h_at: u64,
    pub title_at: u64,
    pub demo_at: u64,
    /// What CHKDSK reported of it.
    pub chkdsk: String,
}

/// Why the hard disk could not be made, or did not start Doom8088.
#[derive(Debug)]
pub(crate) enum Error {
    Input(PathBuf, io::Error),
    Format(io::Error),
    Put(PutError),
    Output(PathBuf, io::Error),
    /// Doom8088 did not set Mode 13h in its time.
    NoMode13h(Box<Stopped>),
    /// No picture stayed the same for long enough in its time.
    NoTitle(Box<Stopped>),
    /// The title picture is not Doom8088's: its hash.
    Title(u64, Box<Stopped>),
    /// The demo did not start in its time.
    NoDemo(Box<Stopped>),
    Check(CheckError),
}

impl Error {
    /// What the guest showed when the stage gave up on it, if one ran.
    pub(crate) fn guest(&self) -> Option<&Stopped> {
        match self {
            Self::NoMode13h(stopped)
            | Self::NoTitle(stopped)
            | Self::Title(_, stopped)
            | Self::NoDemo(stopped)
            | Self::Check(CheckError::Unfinished(stopped)) => Some(stopped),
            _ => None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Input(path, error) => write!(f, "{} cannot be read: {error}", path.display()),
            Self::Format(error) => write!(f, "the hard disk cannot be formatted: {error}"),
            Self::Put(error) => write!(f, "MS-DOS cannot be put on the hard disk: {error}"),
            Self::Output(path, error) => write!(f, "{} cannot be written: {error}", path.display()),
            Self::NoMode13h(_) => write!(
                f,
                "Doom8088 did not set Mode 13h in {} s of guest time",
                MODE_13H_BUDGET.as_secs()
            ),
            Self::NoTitle(_) => write!(
                f,
                "Doom8088 showed no title picture in {} s of guest time",
                TITLE_BUDGET.as_secs()
            ),
            Self::Title(hash, _) => write!(
                f,
                "Doom8088's title picture hashes to {hash:016X}, not {TITLE_HASH:016X}"
            ),
            Self::NoDemo(_) => write!(
                f,
                "Doom8088's demo did not start in {} s of guest time",
                DEMO_BUDGET.as_secs()
            ),
            Self::Check(error) => write!(f, "the hard disk: {error}"),
        }
    }
}

/// The files of Doom8088 in the directory `doom8088`, as the disk takes
/// them.
fn doom8088_files(doom8088: &Path) -> Result<Vec<(String, Vec<u8>)>, Error> {
    DOOM8088
        .iter()
        .map(|(name, text)| {
            let path = doom8088.join(name);
            let data = std::fs::read(&path).map_err(|error| Error::Input(path, error))?;
            Ok((
                name.to_string(),
                if *text { fat::dos_text(&data) } else { data },
            ))
        })
        .collect()
}

/// Makes the hard disk of what the build made and of Doom8088, boots it and
/// checks it from `floppy`, then writes it into `out`.
pub(crate) fn make(
    built: &Built,
    floppy: &Floppy,
    inputs: &Inputs,
    pins: &Pins,
    out: &Path,
) -> Result<DoomDisk, Error> {
    let license =
        msdos::license(&inputs.msdos).map_err(|(path, error)| Error::Input(path, error))?;
    let config = fat::dos_text(CONFIG.as_bytes());
    let autoexec = fat::dos_text(AUTOEXEC.as_bytes());
    let readme = fat::dos_text(notices::readme(pins, Disk::Doom8088).as_bytes());
    let mut files: Vec<File<'_>> = msdos::built_files(built, &SYSTEM).map_err(Error::Put)?;
    files.push(("CONFIG.SYS".to_string(), &config));
    files.push(("AUTOEXEC.BAT".to_string(), &autoexec));
    files.push(("README.TXT".to_string(), &readme));
    files.push(("LICENSE.TXT".to_string(), &license));
    files.extend(msdos::built_files(built, &PROGRAMS).map_err(Error::Put)?);
    let doom8088 = doom8088_files(&inputs.doom8088)?;
    let source = fat::dos_text(notices::doom8088_source(pins).as_bytes());
    let mut doom: Vec<File<'_>> = doom8088
        .iter()
        .map(|(name, data)| (name.clone(), data.as_slice()))
        .collect();
    doom.push(("SOURCE.TXT".to_string(), &source));

    let mut disk = HardDisk::new(CYLINDERS, FAT12, Some(&built.master_boot_record));
    let volume_sectors = disk.volume().len() / layout::SECTOR;
    let options = fatfs::FormatVolumeOptions::new()
        .fat_type(fatfs::FatType::Fat12)
        .bytes_per_cluster(2048)
        .total_sectors(volume_sectors as u32)
        .max_root_dir_entries(512)
        .media(0xF8)
        .sectors_per_track(layout::SECTORS_PER_TRACK as u16)
        .heads(layout::HEADS as u16)
        .drive_num(0x80)
        .volume_id(stamp::HARD_DISK_SERIAL);
    disk.format(options).map_err(Error::Format)?;
    let free = msdos::put_on(disk.volume(), &built.boot_sector, 0x80, |volume| {
        msdos::write(volume, &stamp::MSDOS, None, &files)?;
        msdos::write(volume, &stamp::DOOM8088, Some("DOOM"), &doom)
    })
    .map_err(Error::Put)?;
    let bytes = disk.into_bytes();
    println!(
        "  {HARD_DISK_IMAGE}: {} files, {} bytes free",
        files.len() + doom.len(),
        grouped(free)
    );

    let image = DiskImage::hard_disk_from_bytes(bytes.clone(), "doom8088.img", false);
    let mut guest = Guest::power_on(vec![(0x80, image)]);
    let mode_13h_at = guest
        .run_until(MODE_13H_BUDGET, |guest, _| guest.in_mode_13h())
        .map_err(Error::NoMode13h)?;
    println!(
        "  booted: Mode 13h after {} instructions",
        grouped(mode_13h_at)
    );

    let blank = Guest::blank_picture_hash();
    let (mut last, mut still) = (guest.picture_hash(), 0);
    let title_at = guest
        .run_until(TITLE_BUDGET, |guest, _| {
            let hash = guest.picture_hash();
            if hash == last && hash != blank {
                still += 1;
            } else {
                (last, still) = (hash, 0);
            }
            still == TITLE_BATCHES
        })
        .map_err(Error::NoTitle)?;
    if last != TITLE_HASH {
        return Err(Error::Title(last, guest.stopped()));
    }
    println!(
        "  the title picture, {last:016X}, after {} instructions",
        grouped(title_at)
    );

    let demo_at = guest
        .run_until(DEMO_BUDGET, |guest, _| guest.picture_hash() != TITLE_HASH)
        .map_err(Error::NoDemo)?;
    println!("  the demo after {} instructions", grouped(demo_at));

    let chkdsk = check::chkdsk(&floppy.bytes, Checked::HardDisk(&bytes)).map_err(Error::Check)?;
    println!("  CHKDSK finds it clean");

    let path = out.join(HARD_DISK_IMAGE);
    std::fs::write(&path, &bytes).map_err(|error| Error::Output(path, error))?;
    Ok(DoomDisk {
        bytes,
        free,
        mode_13h_at,
        title_at,
        demo_at,
        chkdsk,
    })
}
