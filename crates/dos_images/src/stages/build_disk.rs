//! Stage 2: the build disk. A hard disk of 100 MB with one FAT16 volume,
//! C: to the guest: the source's `src` as C:\SRC, every file stamped with
//! the day of the release (`stamp::MSDOS`), its SETENV.BAT pointed at C:,
//! and E9.COM and BUILD.BAT at the root.
//!
//! The invariant: the disk holds the source as it was fetched, but for the
//! one line its build notes say to change: SETENV.BAT's `set BAKROOT=d:`,
//! the drive the source lives on.

use crate::fat::{self, TreeError};
use crate::layout::HardDisk;
use crate::makefiles::{self, MakefileError};
use crate::markers::E9_COM;
use crate::stamp;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};

/// 200 cylinders of 16 heads of 63 sectors, 100.8 MB: the geometry the
/// core gives an image of that size. The build leaves 38,013 of its 50,277
/// clusters free.
const CYLINDERS: usize = 200;
/// A primary partition of FAT16 over 32 MB.
const FAT16: u8 = 0x06;
/// The modules the makefiles of the pinned source lead to.
pub(crate) const MODULES: usize = 57;
/// What SETENV.BAT says of the drive the source lives on, and what the
/// build disk needs it to say.
const BAKROOT: &str = "set BAKROOT=d:";
const BAKROOT_HERE: &str = "set BAKROOT=c:";

/// The build disk, and what is on it.
pub(crate) struct BuildDisk {
    pub disk: HardDisk,
    /// The modules BUILD.BAT builds, in its order.
    pub modules: Vec<String>,
    /// The files of the source copied onto it.
    pub sources: usize,
}

/// Why the build disk could not be made.
#[derive(Debug)]
pub(crate) enum Error {
    Makefiles(MakefileError),
    /// The makefiles lead to another number of modules than they do in the
    /// pinned source.
    Modules(usize),
    Source(TreeError),
    /// SETENV.BAT does not name the drive of the source once, as `BAKROOT`.
    NoBakroot,
    Disk(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Makefiles(error) => write!(f, "{error}"),
            Self::Modules(found) => write!(
                f,
                "the makefiles lead to {found} modules, where the pinned source has {MODULES}"
            ),
            Self::Source(error) => write!(f, "the source cannot be copied: {error}"),
            Self::NoBakroot => write!(f, "SETENV.BAT does not say `{BAKROOT}` once"),
            Self::Disk(error) => write!(f, "the build disk cannot be written: {error}"),
        }
    }
}

/// The directory `module`, a DOS path below the source's root, below `src`.
fn host_directory(src: &Path, module: &str) -> PathBuf {
    module
        .split('\\')
        .filter(|part| !part.is_empty())
        .fold(src.to_path_buf(), |path, part| path.join(part))
}

/// The build disk for the source in the directory `msdos` (`v4.0`).
pub(crate) fn make(msdos: &Path) -> Result<BuildDisk, Error> {
    let src = msdos.join("src");
    let mut makefile = |module: &str| {
        let bytes = std::fs::read(host_directory(&src, module).join("MAKEFILE"))?;
        Ok(String::from_utf8_lossy(&bytes).into_owned())
    };
    let modules = makefiles::modules(&mut makefile).map_err(Error::Makefiles)?;
    if modules.len() != MODULES {
        return Err(Error::Modules(modules.len()));
    }
    let build_bat = makefiles::build_bat(&modules);

    let mut disk = HardDisk::new(CYLINDERS, FAT16, None);
    let volume_sectors = disk.volume().len() / crate::layout::SECTOR;
    let options = fatfs::FormatVolumeOptions::new()
        .fat_type(fatfs::FatType::Fat16)
        .bytes_per_cluster(2048)
        .total_sectors(volume_sectors as u32)
        .max_root_dir_entries(512)
        .media(0xF8)
        .sectors_per_track(crate::layout::SECTORS_PER_TRACK as u16)
        .heads(crate::layout::HEADS as u16)
        .drive_num(0x80)
        .volume_id(stamp::BUILD_DISK_SERIAL);
    disk.format(options).map_err(Error::Disk)?;

    let volume = fat::mount(disk.volume(), &stamp::MSDOS).map_err(Error::Disk)?;
    let sources = {
        let root = volume.root_dir();
        fat::put(&root, "E9.COM", &E9_COM).map_err(Error::Disk)?;
        fat::put(&root, "BUILD.BAT", &fat::dos_text(build_bat.as_bytes())).map_err(Error::Disk)?;
        let tree = root.create_dir("SRC").map_err(Error::Disk)?;
        let sources = fat::put_tree(&tree, &src).map_err(Error::Source)?;
        let setenv = fat::get(&tree, "SETENV.BAT").map_err(Error::Disk)?;
        let setenv = String::from_utf8_lossy(&setenv);
        if setenv.matches(BAKROOT).count() != 1 {
            return Err(Error::NoBakroot);
        }
        let here = setenv.replace(BAKROOT, BAKROOT_HERE);
        fat::put(&tree, "SETENV.BAT", here.as_bytes()).map_err(Error::Disk)?;
        sources
    };
    volume.unmount().map_err(Error::Disk)?;
    Ok(BuildDisk {
        disk,
        modules,
        sources,
    })
}
