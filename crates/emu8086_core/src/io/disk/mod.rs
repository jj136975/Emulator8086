//! The guest's disks: the images its drives hold and the controller through
//! which the BIOS reaches them. This file holds what the parts share: the
//! sector size, and what the command line asks a drive to hold (`DiskSpec`).
//!
//! - `image`: one disk, in a host file or in memory, addressed by CHS.
//! - `geometry`: the cylinders, heads and sectors an image is given.
//! - `fat`: the FAT volume on an image, formatted and written from the host.
//! - `clock`: the host's wall clock, which the caller of `fat` passes in.
//! - `controller`: the drives, their change lines and the boot order.
//! - `int13`, `int13_ext`: the INT 13h services behind the BIOS trap.
//!
//! The invariant: a disk ends where its geometry ends. Guest transfers and
//! host-side FAT writes alike stop at the last sector its cylinders, heads
//! and sectors per track address, so an image file is never grown beyond
//! its disk: a floppy image of another size would be opened with another
//! geometry the next time.

mod clock;
mod controller;
mod fat;
mod geometry;
mod image;
mod int13;
mod int13_ext;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub(crate) use clock::FixedClock;
pub use clock::WallClock;
pub use controller::{BOOT_ORDER_END, BOOT_ORDER_PORT, DISK_TRAP_PORT, DiskController};
pub use geometry::{FLOPPY_144_SIZE, HD_DEFAULT_SIZE_MB};
pub use image::{DiskImage, SaveOutcome};

use std::path::PathBuf;

const SECTOR_SIZE: usize = 512;

/// What the user asked for on the CLI.
#[derive(Debug, PartialEq, Eq)]
pub enum DiskSource {
    FilePath(PathBuf),
    Memory(u64), // size in bytes
}

/// Parsed `--disk` argument: drive letter + source.
#[derive(Debug)]
pub struct DiskSpec {
    pub drive: u8, // floppy drive: 0 = A:, 1 = B:, 2 = C:, 3 = D:
    pub source: DiskSource,
    pub readonly: bool,
}
