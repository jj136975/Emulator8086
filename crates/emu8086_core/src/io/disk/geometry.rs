//! The geometry of a disk image: cylinders, heads and sectors per track,
//! derived from the size of the image or from the BIOS parameter block in
//! its boot sector.
//!
//! The invariant: an image gets the geometry the guest computes its CHS
//! addresses with, or every sector the guest asks for comes from the wrong
//! place. The size decides for the floppy formats; a FAT boot sector is
//! taken at its word only when what it states is credible; anything else is
//! a hard disk of 16 heads and 63 sectors per track. No geometry has more
//! than 1024 cylinders, the most a CHS address can express.

use super::SECTOR_SIZE;

pub const FLOPPY_144_SIZE: u64 = 1_474_560; // 80 * 2 * 18 * 512

pub const HD_DEFAULT_SIZE_MB: u64 = 32;

/// Cylinders, heads and sectors per track.
pub(super) type Geometry = (u16, u8, u8);

/// Size in bytes of a disk with the given geometry.
pub(super) fn geometry_bytes((cylinders, heads, sectors_per_track): Geometry) -> u64 {
    cylinders as u64 * heads as u64 * sectors_per_track as u64 * SECTOR_SIZE as u64
}

/// Geometry of the floppy format that has `size` bytes, if there is one.
pub(super) fn floppy_geometry(size: u64) -> Option<Geometry> {
    match size {
        163_840 => Some((40, 1, 8)),
        184_320 => Some((40, 1, 9)),
        327_680 => Some((40, 2, 8)),
        368_640 => Some((40, 2, 9)),
        737_280 => Some((80, 2, 9)),
        1_228_800 => Some((80, 2, 15)),
        1_474_560 => Some((80, 2, 18)),
        2_949_120 => Some((80, 2, 36)),
        _ => None,
    }
}

/// Geometry of a blank floppy image of `size` bytes. A size that is not one
/// of the floppy formats gets the geometry of a hard disk.
pub(super) fn geometry_from_size(size: u64) -> Geometry {
    floppy_geometry(size).unwrap_or_else(|| hd_geometry_from_size(size))
}

/// The geometry a FAT boot sector states in its BIOS parameter block, if
/// `sector` is such a boot sector and what it states is credible.
///
/// DOS computes its CHS addresses from these values, so they are the right
/// geometry for an image whose size alone does not tell it.
pub(super) fn geometry_from_boot_sector(sector: &[u8]) -> Option<Geometry> {
    if sector.len() < SECTOR_SIZE {
        return None;
    }
    let word = |offset: usize| u16::from_le_bytes([sector[offset], sector[offset + 1]]);

    let starts_with_a_jump = matches!(sector[0], 0xEB | 0xE9);
    let bytes_per_sector = word(0x0B);
    let sectors_per_cluster = sector[0x0D];
    let fats = sector[0x10];
    let media = sector[0x15];
    let sectors_per_track = word(0x18);
    let heads = word(0x1A);
    // The 16-bit sector count is 0 on volumes too large for it; those give
    // their size in the 32-bit field.
    let sectors = match word(0x13) {
        0 => u32::from_le_bytes([sector[0x20], sector[0x21], sector[0x22], sector[0x23]]) as u64,
        count => count as u64,
    };

    let credible = starts_with_a_jump
        && bytes_per_sector as usize == SECTOR_SIZE
        && sectors_per_cluster.is_power_of_two()
        && (1..=2).contains(&fats)
        && media >= 0xF0
        && (1..=63).contains(&sectors_per_track)
        && (1..=255).contains(&heads)
        && sectors > 0;
    if !credible {
        return None;
    }

    // A volume that ends inside a cylinder still needs that cylinder.
    let cylinders = sectors.div_ceil(heads as u64 * sectors_per_track as u64);
    (cylinders <= MAX_CYLINDERS).then_some((cylinders as u16, heads as u8, sectors_per_track as u8))
}

/// Hard disks have 16 heads and 63 sectors per track.
const HD_HEADS: u8 = 16;
const HD_SECTORS_PER_TRACK: u8 = 63;

/// The number of cylinders a CHS address can express.
pub(super) const MAX_CYLINDERS: u64 = 1024;

/// Geometry of a hard disk of `size` bytes: as many whole cylinders as fit,
/// at least one and at most `MAX_CYLINDERS`.
pub(super) fn hd_geometry_from_size(size: u64) -> Geometry {
    let cylinder_bytes = geometry_bytes((1, HD_HEADS, HD_SECTORS_PER_TRACK));
    let cylinders = (size / cylinder_bytes).clamp(1, MAX_CYLINDERS) as u16;
    (cylinders, HD_HEADS, HD_SECTORS_PER_TRACK)
}
