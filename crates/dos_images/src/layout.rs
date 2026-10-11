//! The disks below their file systems: a hard disk of whole cylinders with
//! one primary partition (the partition entry of its master boot record),
//! the boot sector the build made put on a volume with the volume's own
//! BIOS parameter block, and IO.SYS and MSDOS.SYS where MS-DOS 4.00's boot
//! sector looks for them.
//!
//! The invariant: a volume the tool seals boots MS-DOS 4.00. Its boot
//! sector reads the first two entries of the root and compares them with
//! IO.SYS and MSDOS.SYS, then reads the first sectors of IO.SYS from the
//! first data cluster, and the loader there reads the rest of IO.SYS as one
//! run of clusters: `seal_system_files` refuses a volume that is not so.

use std::fmt;
use std::io::{self, Cursor};

/// Bytes in a sector.
pub(crate) const SECTOR: usize = 512;
/// The geometry the core gives a hard disk image of whole cylinders: 16
/// heads of 63 sectors.
pub(crate) const HEADS: usize = 16;
pub(crate) const SECTORS_PER_TRACK: usize = 63;

/// Where the partition table of a master boot record starts: the code
/// before it is the record's.
pub(crate) const PARTITION_TABLE: usize = 0x1BE;
/// The boot signature at the end of a boot sector, 55h AAh.
const SIGNATURE: usize = 0x1FE;

/// The BIOS parameter block and the extended one of a FAT12 or FAT16 boot
/// sector, from the bytes per sector to the file system's name.
const BPB: std::ops::Range<usize> = 0x0B..0x3E;
/// Where MS-DOS 4.00's boot sector keeps the BIOS drive it reads from.
const DRIVE: usize = 0x24;
/// The hidden sectors of the BPB: those of the disk before the volume.
const HIDDEN_SECTORS: usize = 0x1C;

/// The attributes SYS gives IO.SYS and MSDOS.SYS: read-only, hidden,
/// system and archive.
const SYSTEM_FILE: u8 = 0x27;

/// Why a volume, or a file the build made, is not what a boot needs.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum LayoutError {
    /// A file the build made is not the image its makefile describes.
    Image {
        name: &'static str,
        length: usize,
        expected: usize,
    },
    /// A boot sector or a master boot record has no boot signature.
    NoSignature(&'static str),
    /// The root does not begin with IO.SYS and MSDOS.SYS, but with these.
    RootOrder { first: String, second: String },
    /// IO.SYS does not begin at the first data cluster, but at this one.
    NotFirst(u16),
    /// IO.SYS is not one run of clusters: this cluster is not followed by
    /// the next.
    Fragmented(usize),
}

impl fmt::Display for LayoutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Image {
                name,
                length,
                expected,
            } => write!(f, "{name} is {length} bytes long, not {expected}"),
            Self::NoSignature(name) => write!(f, "{name} has no boot signature"),
            Self::RootOrder { first, second } => write!(
                f,
                "the root begins with {first} and {second}, not with IO.SYS and MSDOS.SYS"
            ),
            Self::NotFirst(cluster) => {
                write!(f, "IO.SYS begins at cluster {cluster}, not at the first, 2")
            }
            Self::Fragmented(cluster) => write!(
                f,
                "IO.SYS is not one run of clusters: cluster {cluster} is not followed by the next"
            ),
        }
    }
}

/// The bytes of the last sector of `image`, a file the build made, after
/// checking that it is `expected` bytes long and that the sector ends in
/// the boot signature.
fn last_sector(
    image: &[u8],
    name: &'static str,
    expected: usize,
) -> Result<[u8; SECTOR], LayoutError> {
    if image.len() != expected {
        return Err(LayoutError::Image {
            name,
            length: image.len(),
            expected,
        });
    }
    let sector: [u8; SECTOR] = image[expected - SECTOR..].try_into().unwrap();
    if sector[SIGNATURE..] != [0x55, 0xAA] {
        return Err(LayoutError::NoSignature(name));
    }
    Ok(sector)
}

/// The boot sector the BOOT module assembled. MSBOOT.BIN is the image of
/// 0000-7DFFh, the sector assembled at 7C00h: the makefile takes it with
/// `dbof msboot.bin boot.inc 7c00 200`.
pub(crate) fn boot_sector(msboot_bin: &[u8]) -> Result<[u8; SECTOR], LayoutError> {
    last_sector(msboot_bin, "BOOT\\MSBOOT.BIN", 0x7E00)
}

/// The master boot record FDISK writes. FDBOOT.BIN is the image of
/// 0000-07FFh, the sector assembled at 600h: the makefile takes it with
/// `dbof fdboot.bin fdboot.inc 600 200`.
pub(crate) fn master_boot_record(fdboot_bin: &[u8]) -> Result<[u8; SECTOR], LayoutError> {
    last_sector(fdboot_bin, "CMD\\FDISK\\FDBOOT.BIN", 0x800)
}

/// The CHS address of the sector `lba` as a partition entry gives it: the
/// head, the sector with bits 8-9 of the cylinder above it, and bits 0-7
/// of the cylinder. A cylinder past 1023 is given as 1023, the last that
/// INT 13h addresses.
pub(crate) fn chs(lba: usize) -> [u8; 3] {
    let cylinder = (lba / (HEADS * SECTORS_PER_TRACK)).min(1023);
    let head = lba / SECTORS_PER_TRACK % HEADS;
    let sector = lba % SECTORS_PER_TRACK + 1;
    [
        head as u8,
        sector as u8 | (((cylinder >> 8) as u8) << 6),
        cylinder as u8,
    ]
}

/// The entry of an active primary partition of the type `kind`, of the
/// `sectors` sectors from the sector `start`.
pub(crate) fn partition_entry(kind: u8, start: usize, sectors: usize) -> [u8; 16] {
    let mut entry = [0; 16];
    entry[0] = 0x80;
    entry[1..4].copy_from_slice(&chs(start));
    entry[4] = kind;
    entry[5..8].copy_from_slice(&chs(start + sectors - 1));
    entry[8..12].copy_from_slice(&(start as u32).to_le_bytes());
    entry[12..16].copy_from_slice(&(sectors as u32).to_le_bytes());
    entry
}

/// A hard disk of whole cylinders, whose master boot record holds one
/// partition: active, primary, from the second track, which is where FDISK
/// starts the first, to the end of the disk.
pub(crate) struct HardDisk {
    bytes: Vec<u8>,
}

impl HardDisk {
    /// A disk of `cylinders` holding nothing but its master boot record:
    /// `boot_code` (the record's code, before its partition table, or none:
    /// zeros), the entry of a partition of the type `kind`, and the boot
    /// signature.
    pub(crate) fn new(cylinders: usize, kind: u8, boot_code: Option<&[u8; SECTOR]>) -> Self {
        let sectors = cylinders * HEADS * SECTORS_PER_TRACK;
        let mut bytes = vec![0; sectors * SECTOR];
        if let Some(code) = boot_code {
            bytes[..PARTITION_TABLE].copy_from_slice(&code[..PARTITION_TABLE]);
        }
        let entry = partition_entry(kind, SECTORS_PER_TRACK, sectors - SECTORS_PER_TRACK);
        bytes[PARTITION_TABLE..PARTITION_TABLE + 16].copy_from_slice(&entry);
        bytes[SIGNATURE..SECTOR].copy_from_slice(&[0x55, 0xAA]);
        Self { bytes }
    }

    /// A disk as the guest left it, the partition where its first entry
    /// says.
    pub(crate) fn from_bytes(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

    fn entry_word(&self, at: usize) -> usize {
        let at = PARTITION_TABLE + at;
        u32::from_le_bytes(self.bytes[at..at + 4].try_into().unwrap()) as usize
    }

    /// The bytes of the partition: the volume. Of a disk whose entry says
    /// more than it holds, as much as it holds.
    pub(crate) fn volume(&mut self) -> &mut [u8] {
        let length = self.bytes.len();
        let start = (self.entry_word(8) * SECTOR).min(length);
        let end = (start + self.entry_word(12) * SECTOR).min(length);
        &mut self.bytes[start..end]
    }

    /// Formats the partition with `options`. fatfs counts no hidden
    /// sectors, and DOS finds the volume's sectors on the disk by adding
    /// them: they are set to the sectors before the partition.
    pub(crate) fn format(&mut self, options: fatfs::FormatVolumeOptions) -> io::Result<()> {
        let hidden = self.entry_word(8) as u32;
        let volume = self.volume();
        fatfs::format_volume(Cursor::new(&mut *volume), options)?;
        volume[HIDDEN_SECTORS..HIDDEN_SECTORS + 4].copy_from_slice(&hidden.to_le_bytes());
        Ok(())
    }

    pub(crate) fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}

/// Puts the boot sector `boot` on the volume that begins `volume`, keeping
/// the volume's BIOS parameter block and extended one, with `drive` as the
/// BIOS drive its code reads from: MS-DOS 4.00's boot sector takes the
/// drive from there, not from what the BIOS hands it in DL.
pub(crate) fn install_boot_sector(volume: &mut [u8], boot: &[u8; SECTOR], drive: u8) {
    let mut sector = *boot;
    sector[BPB].copy_from_slice(&volume[BPB]);
    sector[DRIVE] = drive;
    sector[DRIVE + 1] = 0;
    volume[..SECTOR].copy_from_slice(&sector);
}

/// What the BIOS parameter block of a FAT12 or FAT16 volume says of where
/// its parts lie.
struct Bpb {
    cluster_bytes: usize,
    /// Where the first FAT, the root and the first data cluster begin.
    fat: usize,
    root: usize,
    fat16: bool,
}

impl Bpb {
    fn of(volume: &[u8]) -> Self {
        let word = |at: usize| u16::from_le_bytes([volume[at], volume[at + 1]]) as usize;
        let sector_bytes = word(0x0B);
        let cluster_sectors = volume[0x0D] as usize;
        let reserved = word(0x0E);
        let fats = volume[0x10] as usize;
        let root_entries = word(0x11);
        let total = match word(0x13) {
            0 => u32::from_le_bytes(volume[0x20..0x24].try_into().unwrap()) as usize,
            sectors => sectors,
        };
        let fat_sectors = word(0x16);
        let root = reserved + fats * fat_sectors;
        let data = root + (root_entries * 32).div_ceil(sector_bytes);
        // The type of a FAT is told by its number of clusters alone.
        let clusters = (total - data) / cluster_sectors;
        Self {
            cluster_bytes: cluster_sectors * sector_bytes,
            fat: reserved * sector_bytes,
            root: root * sector_bytes,
            fat16: clusters >= 4085,
        }
    }

    /// The cluster that follows `cluster` in the first FAT of `volume`.
    fn next(&self, volume: &[u8], cluster: usize) -> usize {
        if self.fat16 {
            let at = self.fat + cluster * 2;
            u16::from_le_bytes([volume[at], volume[at + 1]]) as usize
        } else {
            let at = self.fat + cluster * 3 / 2;
            let pair = u16::from_le_bytes([volume[at], volume[at + 1]]);
            (if cluster & 1 == 1 {
                pair >> 4
            } else {
                pair & 0xFFF
            }) as usize
        }
    }
}

/// The name a root entry holds, spelled as DOS shows it.
fn entry_name(entry: &[u8]) -> String {
    let part = |bytes: &[u8]| String::from_utf8_lossy(bytes).trim_end().to_string();
    let (base, extension) = (part(&entry[..8]), part(&entry[8..11]));
    if extension.is_empty() {
        base
    } else {
        format!("{base}.{extension}")
    }
}

/// Gives IO.SYS and MSDOS.SYS the attributes SYS gives them, after
/// checking that they are the first two entries of the root of `volume`,
/// and that IO.SYS begins at the first data cluster, as one run of
/// clusters.
pub(crate) fn seal_system_files(volume: &mut [u8]) -> Result<(), LayoutError> {
    let bpb = Bpb::of(volume);
    let root = bpb.root;
    let (first, second) = (&volume[root..root + 32], &volume[root + 32..root + 64]);
    if &first[..11] != b"IO      SYS" || &second[..11] != b"MSDOS   SYS" {
        return Err(LayoutError::RootOrder {
            first: entry_name(first),
            second: entry_name(second),
        });
    }
    let start = u16::from_le_bytes([first[26], first[27]]);
    if start != 2 {
        return Err(LayoutError::NotFirst(start));
    }
    let length = u32::from_le_bytes(first[28..32].try_into().unwrap()) as usize;
    let clusters = length.div_ceil(bpb.cluster_bytes);
    for cluster in 2..1 + clusters {
        if bpb.next(volume, cluster) != cluster + 1 {
            return Err(LayoutError::Fragmented(cluster));
        }
    }
    volume[root + 11] = SYSTEM_FILE;
    volume[root + 32 + 11] = SYSTEM_FILE;
    Ok(())
}

#[cfg(test)]
#[path = "layout_tests.rs"]
mod tests;
