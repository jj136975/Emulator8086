//! The FAT volume on a disk image, as the host reaches it: formatting a
//! blank image, finding the volume (the whole disk, or the first FAT
//! partition of a partitioned one) and copying host files into it.
//!
//! The invariant: DOS is the reader of everything written here. Only 8.3
//! directory entries are written (fatfs is built without long names, which
//! MS-DOS 6 shows as the label of a volume that has none), a name DOS
//! resolves to a device is refused, a floppy is FAT12, and every formatted
//! disk gets a volume serial of its own. Writes stay inside the volume:
//! `PartitionSlice` ends where the partition ends.
//!
//! A copy changes the disk behind the guest's back, so it has to go through
//! `DiskController::put_file` or `put_dir`, which raise the change line of
//! the drive.
//!
//! The time a file is stamped with and the time a serial comes from are
//! those of the `WallClock` the caller passes. Nothing here reads a clock,
//! and a volume is mounted with the options that clock gives, never with
//! `FsOptions::new()`: fatfs's own options may come with a clock of its own.

use super::SECTOR_SIZE;
use super::clock::WallClock;
use super::image::{DiskImage, DiskStorage};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::Path;

/// What a FAT volume can live on: an image file or an image held in memory.
trait ImageIo: Read + Write + Seek {}

impl<T: Read + Write + Seek> ImageIo for T {}

type FatVolume<'a> = fatfs::FileSystem<PartitionSlice<Box<dyn ImageIo + 'a>>>;

/// A partition of a disk image as a stream of its own: positions count
/// from the start of the partition, and nothing outside it can be read or
/// written.
pub(super) struct PartitionSlice<T> {
    pub(super) inner: T,
    /// Byte offset of the partition in the image.
    start: u64,
    /// Size of the partition in bytes.
    length: u64,
    /// Position within the partition, at most `length`.
    position: u64,
}

impl<T: Seek> PartitionSlice<T> {
    pub(super) fn new(mut inner: T, start: u64, length: u64) -> std::io::Result<Self> {
        inner.seek(SeekFrom::Start(start))?;
        Ok(Self {
            inner,
            start,
            length,
            position: 0,
        })
    }
}

impl<T> PartitionSlice<T> {
    /// How many of `wanted` bytes lie between the position and the end of
    /// the partition.
    fn within(&self, wanted: usize) -> usize {
        (self.length - self.position).min(wanted as u64) as usize
    }
}

impl<T: Read> Read for PartitionSlice<T> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let wanted = self.within(buf.len());
        let read = self.inner.read(&mut buf[..wanted])?;
        self.position += read as u64;
        Ok(read)
    }
}

impl<T: Write> Write for PartitionSlice<T> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let wanted = self.within(buf.len());
        let written = self.inner.write(&buf[..wanted])?;
        self.position += written as u64;
        Ok(written)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

impl<T: Seek> Seek for PartitionSlice<T> {
    fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        let target = match pos {
            SeekFrom::Start(offset) => Some(offset),
            SeekFrom::Current(offset) => self.position.checked_add_signed(offset),
            SeekFrom::End(offset) => self.length.checked_add_signed(offset),
        };
        match target {
            Some(position) if position <= self.length => {
                self.inner.seek(SeekFrom::Start(self.start + position))?;
                self.position = position;
                Ok(position)
            }
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "seek outside the partition",
            )),
        }
    }
}

/// Partition types that hold a FAT file system: FAT12, FAT16 below 32 MB,
/// FAT16, FAT32, and FAT32 and FAT16 addressed by LBA.
const FAT_PARTITION_TYPES: [u8; 6] = [0x01, 0x04, 0x06, 0x0B, 0x0C, 0x0E];

impl DiskImage {
    /// Format this disk with a FAT filesystem using the fatfs crate.
    /// Sets BPB geometry, media descriptor, cluster size, and root directory entries
    /// to match standard DOS floppy formats. Forces FAT12 for floppy-sized disks.
    /// The volume serial comes from the time `clock` shows.
    pub fn format_fat(&mut self, clock: &dyn WallClock) -> std::io::Result<()> {
        let total_sectors = self.total_sectors() as u32;

        // Standard DOS floppy format parameters: (media_byte, sectors_per_cluster, root_dir_entries)
        let (media_byte, spc, root_entries) = match self.total_bytes() {
            163_840 => (0xFEu8, 1u8, 64u16), // 160K
            184_320 => (0xFC, 1, 64),        // 180K
            327_680 => (0xFF, 2, 112),       // 320K
            368_640 => (0xFD, 2, 112),       // 360K
            737_280 => (0xF9, 2, 112),       // 720K
            1_228_800 => (0xF9, 1, 224),     // 1.2M
            1_474_560 => (0xF0, 1, 224),     // 1.44M
            2_949_120 => (0xF0, 2, 240),     // 2.88M
            _ => (0xF0, 1, 224),             // other
        };

        let mut options = fatfs::FormatVolumeOptions::new()
            .media(media_byte)
            .total_sectors(total_sectors)
            .bytes_per_cluster(spc as u32 * SECTOR_SIZE as u32)
            .max_root_dir_entries(root_entries)
            .sectors_per_track(self.sectors_per_track as u16)
            .heads(self.heads as u16)
            .volume_id(new_volume_serial(clock));

        // Force FAT12 for floppy-sized disks — DOS expects FAT12 on floppies
        if self.heads <= 2 {
            options = options.fat_type(fatfs::FatType::Fat12);
        }

        let mut io = self.image_io();
        io.seek(SeekFrom::Start(0))?;
        fatfs::format_volume(&mut io, options)
    }

    /// The image as a seekable stream of bytes, whatever it is stored in.
    fn image_io(&mut self) -> Box<dyn ImageIo + '_> {
        match &mut self.storage {
            DiskStorage::FileBacked { file, .. } => Box::new(file),
            DiskStorage::InMemory(data) => Box::new(std::io::Cursor::new(data.as_mut_slice())),
        }
    }

    /// Copy all files from a host directory into the FAT filesystem on this
    /// disk, replacing files that already exist there. Returns the number of
    /// files copied.
    pub(super) fn copy_dir_to_fat(
        &mut self,
        host_dir: &Path,
        clock: &dyn WallClock,
    ) -> std::io::Result<usize> {
        self.require_writable()?;
        let volume = self.fat_volume(clock)?;
        put_dir(&volume.root_dir(), host_dir)
    }

    /// Mounts the FAT volume on this disk. What is written to it is stamped
    /// with the time `clock` shows.
    fn fat_volume(&mut self, clock: &dyn WallClock) -> std::io::Result<FatVolume<'_>> {
        let (start, length) = self.fat_volume_extent()?;
        let io = self.image_io();
        fatfs::FileSystem::new(PartitionSlice::new(io, start, length)?, clock.fat_options())
    }

    /// Where the FAT volume of this disk lies, as a byte offset and a
    /// length: the whole disk if its first sector is a FAT boot sector or
    /// no boot sector at all (a floppy, an unpartitioned disk), and the
    /// first FAT partition if the first sector is a master boot record.
    fn fat_volume_extent(&mut self) -> std::io::Result<(u64, u64)> {
        let whole_disk = (0, self.total_bytes());
        let Ok(first_sector) = self.read_sectors(0, 0, 1, 1) else {
            return Ok(whole_disk);
        };
        if first_sector[510] != 0x55 || first_sector[511] != 0xAA {
            return Ok(whole_disk);
        }
        // A FAT boot sector starts with a jump (EB xx 90 or E9 xx xx) and
        // states 512 bytes per sector. Anything else that carries the
        // signature is a master boot record with a partition table.
        let has_jump = matches!(first_sector[0], 0xEB | 0xE9);
        let bytes_per_sector = u16::from_le_bytes([first_sector[11], first_sector[12]]);
        if has_jump && bytes_per_sector as usize == SECTOR_SIZE {
            return Ok(whole_disk);
        }

        // Partition entries: type at 4, first sector at 8, sector count at 12.
        let disk_sectors = self.total_sectors();
        first_sector[0x1BE..0x1FE]
            .chunks(16)
            .filter(|entry| FAT_PARTITION_TYPES.contains(&entry[4]))
            .map(|entry| {
                let first = u32::from_le_bytes([entry[8], entry[9], entry[10], entry[11]]);
                let count = u32::from_le_bytes([entry[12], entry[13], entry[14], entry[15]]);
                (first as u64, count as u64)
            })
            .find(|(first, count)| *first > 0 && *count > 0 && *first < disk_sectors)
            .map(|(first, count)| {
                let count = count.min(disk_sectors - first);
                (first * SECTOR_SIZE as u64, count * SECTOR_SIZE as u64)
            })
            .ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "the disk has no FAT partition",
                )
            })
    }

    /// Copy host file data into the FAT filesystem on this disk, replacing
    /// the file if it exists. `dos_path` can be a filename ("GAME.EXE") or a
    /// path ("\DOS\GAME.EXE") whose directories exist. Returns the path under
    /// which DOS sees the file: a name that is not a valid 8.3 name is stored
    /// under a generated one.
    pub(super) fn copy_file_to_fat(
        &mut self,
        host_data: &[u8],
        dos_path: &str,
        clock: &dyn WallClock,
    ) -> std::io::Result<String> {
        self.require_writable()?;
        let volume = self.fat_volume(clock)?;
        put_path(&volume.root_dir(), dos_path, host_data)
    }
}

/// A volume serial number for a freshly formatted disk, derived from the
/// clock like the one DOS writes. DOS tells disks apart by this number when
/// the change line fires, so two formatted disks must not share one.
fn new_volume_serial(clock: &dyn WallClock) -> u32 {
    use std::sync::atomic::{AtomicU32, Ordering};

    static SEQUENCE: AtomicU32 = AtomicU32::new(0);
    volume_serial(clock.unix_nanos(), SEQUENCE.fetch_add(1, Ordering::Relaxed))
}

/// The serial of the `sequence`-th volume this process formats, at the time
/// `unix_nanos`. The time tells apart disks formatted in different runs,
/// the sequence number those formatted within one tick of the clock.
pub(super) fn volume_serial(unix_nanos: u64, sequence: u32) -> u32 {
    ((unix_nanos >> 32) as u32 ^ unix_nanos as u32).wrapping_add(sequence.wrapping_mul(0x9E37_79B9))
}

/// Names DOS resolves to a device in every directory and with any
/// extension. A file stored under one of them could never be opened or
/// deleted from DOS.
const DOS_DEVICE_NAMES: [&str; 12] = [
    "CON", "PRN", "AUX", "NUL", "CLOCK$", "COM1", "COM2", "COM3", "COM4", "LPT1", "LPT2", "LPT3",
];

fn reject_device_name(name: &str) -> std::io::Result<()> {
    let base = name.split('.').next().unwrap_or("").trim_end();
    if DOS_DEVICE_NAMES
        .iter()
        .any(|device| device.eq_ignore_ascii_case(base))
    {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("'{}' is the name of a DOS device", name),
        ));
    }
    Ok(())
}

/// The 8.3 names in a FAT directory, as DOS shows them.
fn short_names<T: Read + Write + Seek>(dir: &fatfs::Dir<T>) -> std::io::Result<Vec<String>> {
    dir.iter()
        .map(|entry| {
            let entry = entry?;
            Ok(String::from_utf8_lossy(entry.short_file_name_as_bytes()).into_owned())
        })
        .collect()
}

/// Writes `data` to the file `name` in `dir`, replacing an existing file,
/// and returns the name under which DOS sees it. Only 8.3 entries are
/// written (fatfs is built without long-name support, see Cargo.toml), so a
/// name that is not a valid 8.3 name ends up under a generated one.
fn put_file<T: Read + Write + Seek>(
    dir: &fatfs::Dir<T>,
    name: &str,
    data: &[u8],
) -> std::io::Result<String> {
    reject_device_name(name)?;
    let before = short_names(dir)?;

    let mut file = dir.create_file(name)?;
    // create_file opens an existing file as it is: cut it off before writing.
    file.truncate()?;
    file.write_all(data)?;
    file.flush()?;
    drop(file);

    let created = short_names(dir)?
        .into_iter()
        .find(|short| !before.contains(short));
    Ok(created.unwrap_or_else(|| name.to_ascii_uppercase()))
}

/// Writes `data` to `dos_path` below `root`. The directories on the path
/// must exist. Returns the path as DOS shows it.
fn put_path<T: Read + Write + Seek>(
    root: &fatfs::Dir<T>,
    dos_path: &str,
    data: &[u8],
) -> std::io::Result<String> {
    let components: Vec<&str> = dos_path
        .split(['\\', '/'])
        .filter(|c| !c.is_empty())
        .collect();
    let Some((name, directories)) = components.split_last() else {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "empty filename",
        ));
    };

    let mut dir = root.clone();
    let mut stored = String::new();
    for directory in directories {
        dir = dir.open_dir(directory)?;
        stored.push_str(&directory.to_ascii_uppercase());
        stored.push('\\');
    }
    stored.push_str(&put_file(&dir, name, data)?);
    Ok(stored)
}

/// Recursively copies the files of a host directory into a FAT directory.
fn put_dir<T: Read + Write + Seek>(
    fat_dir: &fatfs::Dir<T>,
    host_dir: &Path,
) -> std::io::Result<usize> {
    let mut count = 0;
    for entry in std::fs::read_dir(host_dir)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let name = entry.file_name().to_string_lossy().into_owned();

        if file_type.is_file() {
            put_file(fat_dir, &name, &std::fs::read(entry.path())?)?;
            count += 1;
        } else if file_type.is_dir() {
            reject_device_name(&name)?;
            let sub_dir = fat_dir.create_dir(&name)?;
            count += put_dir(&sub_dir, &entry.path())?;
        }
    }
    Ok(count)
}
