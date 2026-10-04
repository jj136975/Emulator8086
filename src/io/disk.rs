use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use crate::vm::runtime::CpuFlag::Carry;
use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use log::debug;

const SECTOR_SIZE: usize = 512;
pub const FLOPPY_144_SIZE: u64 = 1_474_560; // 80 * 2 * 18 * 512

/// Where the disk data lives.
enum DiskStorage {
    /// An image file on the host. `path` is its canonical path, kept so the
    /// image can tell when it is asked to save onto itself.
    FileBacked { file: File, path: PathBuf },
    InMemory(Vec<u8>),
}

impl DiskStorage {
    fn file_backed(file: File, path: &Path) -> Self {
        let path = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        DiskStorage::FileBacked { file, path }
    }
}

/// What `DiskImage::save_to_file` did.
#[derive(Debug, PartialEq, Eq)]
pub enum SaveOutcome {
    /// The image was copied to the target file.
    Written,
    /// The target is the file the image already lives in; it was flushed.
    AlreadyBacking,
}

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

pub struct DiskImage {
    storage: DiskStorage,
    pub cylinders: u16,
    pub heads: u8,
    pub sectors_per_track: u8,
    pub readonly: bool,
}

impl DiskImage {
    /// An image that lives in a host file.
    fn on_file(file: File, path: &Path, geometry: Geometry, readonly: bool) -> Self {
        let (cylinders, heads, sectors_per_track) = geometry;
        Self {
            storage: DiskStorage::file_backed(file, path),
            cylinders,
            heads,
            sectors_per_track,
            readonly,
        }
    }

    /// A blank image held in memory.
    fn in_memory(geometry: Geometry, readonly: bool) -> Self {
        let (cylinders, heads, sectors_per_track) = geometry;
        Self {
            storage: DiskStorage::InMemory(vec![0u8; geometry_bytes(geometry) as usize]),
            cylinders,
            heads,
            sectors_per_track,
            readonly,
        }
    }

    /// Opens an existing floppy image file.
    ///
    /// The geometry follows from the size if it is that of a floppy format.
    /// Any other image is taken at the word of its boot sector, if it has
    /// one, and as a disk with the geometry of a hard disk otherwise.
    pub fn open(path: &Path, readonly: bool) -> std::io::Result<Self> {
        let mut file = OpenOptions::new().read(true).write(!readonly).open(path)?;
        let size = file.metadata()?.len();
        let geometry = match floppy_geometry(size) {
            Some(geometry) => geometry,
            None => {
                let mut boot_sector = Vec::with_capacity(SECTOR_SIZE);
                (&mut file).take(SECTOR_SIZE as u64).read_to_end(&mut boot_sector)?;
                geometry_from_boot_sector(&boot_sector)
                    .unwrap_or_else(|| hd_geometry_from_size(size))
            }
        };
        Ok(Self::on_file(file, path, geometry, readonly))
    }

    /// Creates a new image file of `size` bytes, filled with zeros. An
    /// existing file is never replaced.
    pub fn create(path: &Path, size: u64) -> std::io::Result<Self> {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(path)?;
        file.set_len(size)?;
        Ok(Self::on_file(file, path, geometry_from_size(size), false))
    }

    /// Create a blank in-memory floppy of the given size in bytes.
    /// Uses standard floppy geometry for known sizes (720K, 1.2M, 1.44M, 2.88M).
    pub fn new_in_memory_sized(size: u64, readonly: bool) -> Self {
        Self::in_memory(geometry_from_size(size), readonly)
    }

    /// Open an existing hard disk image file.
    pub fn open_hard_disk(path: &Path, readonly: bool) -> std::io::Result<Self> {
        let file = OpenOptions::new().read(true).write(!readonly).open(path)?;
        let size = file.metadata()?.len();
        Ok(Self::on_file(file, path, hd_geometry_from_size(size), readonly))
    }

    /// Create a new hard disk image of the given size in MB. An existing
    /// file is never replaced.
    pub fn create_hard_disk(path: &Path, size_mb: u64) -> std::io::Result<Self> {
        let geometry = hd_geometry_from_size(size_mb * 1024 * 1024);
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(path)?;
        file.set_len(geometry_bytes(geometry))?;
        Ok(Self::on_file(file, path, geometry, false))
    }

    /// Create a blank in-memory hard disk of the given size in MB.
    pub fn new_in_memory_hard_disk(size_mb: u64) -> Self {
        Self::in_memory(hd_geometry_from_size(size_mb * 1024 * 1024), false)
    }

    /// Number of sectors the geometry addresses.
    pub fn total_sectors(&self) -> u64 {
        self.cylinders as u64 * self.heads as u64 * self.sectors_per_track as u64
    }

    /// Total size in bytes.
    pub fn total_bytes(&self) -> u64 {
        self.total_sectors() * SECTOR_SIZE as u64
    }

    /// What the user should be told when the guest cannot reach all of the
    /// image file: CHS addresses end at 1024 cylinders, and whatever a
    /// larger image holds beyond them is out of reach.
    pub fn capacity_note(&self) -> Option<String> {
        let DiskStorage::FileBacked { file, .. } = &self.storage else {
            return None;
        };
        let file_size = file.metadata().ok()?.len();
        let cylinder = geometry_bytes((1, self.heads, self.sectors_per_track));
        let cut_off =
            self.cylinders as u64 == MAX_CYLINDERS && file_size >= self.total_bytes() + cylinder;
        cut_off.then(|| {
            format!(
                "the image is {} MB, but a disk has at most {} cylinders: the guest sees only \
                 the first {} MB",
                file_size >> 20,
                MAX_CYLINDERS,
                self.total_bytes() >> 20
            )
        })
    }

    fn outside_the_disk() -> std::io::Error {
        std::io::Error::new(
            std::io::ErrorKind::UnexpectedEof,
            "sector outside the disk geometry",
        )
    }

    /// Byte offset of `count` sectors starting at a CHS address, or an error
    /// if any of them lies outside the geometry. Both kinds of storage are
    /// bounded this way: a file-backed image must not be read past, or grown
    /// beyond, the disk it represents.
    fn checked_offset(&self, c: u16, h: u8, s: u8, count: u8) -> std::io::Result<u64> {
        if !self.verify_sectors(c, h, s, count) {
            return Err(Self::outside_the_disk());
        }
        Ok(self.chs_to_offset(c, h, s))
    }

    pub fn read_sectors(&mut self, c: u16, h: u8, s: u8, count: u8) -> std::io::Result<Vec<u8>> {
        let byte_offset = self.checked_offset(c, h, s, count)?;
        let total_bytes = count as usize * SECTOR_SIZE;
        let mut buf = vec![0u8; total_bytes];

        match &mut self.storage {
            DiskStorage::FileBacked { file, .. } => {
                file.seek(SeekFrom::Start(byte_offset))?;
                file.read_exact(&mut buf)?;
            }
            DiskStorage::InMemory(data) => {
                let start = byte_offset as usize;
                let sectors = data
                    .get(start..start + total_bytes)
                    .ok_or_else(Self::outside_the_disk)?;
                buf.copy_from_slice(sectors);
            }
        }
        Ok(buf)
    }

    pub fn write_sectors(
        &mut self,
        c: u16,
        h: u8,
        s: u8,
        count: u8,
        data: &[u8],
    ) -> std::io::Result<()> {
        self.require_writable()?;

        let byte_offset = self.checked_offset(c, h, s, count)?;
        let total_bytes = count as usize * SECTOR_SIZE;
        if data.len() < total_bytes {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "data buffer too small",
            ));
        }

        match &mut self.storage {
            DiskStorage::FileBacked { file, .. } => {
                file.seek(SeekFrom::Start(byte_offset))?;
                file.write_all(&data[..total_bytes])?;
                file.flush()?;
            }
            DiskStorage::InMemory(mem) => {
                let start = byte_offset as usize;
                let sectors = mem
                    .get_mut(start..start + total_bytes)
                    .ok_or_else(Self::outside_the_disk)?;
                sectors.copy_from_slice(&data[..total_bytes]);
            }
        }
        Ok(())
    }

    /// Validate that the CHS range is within disk geometry.
    pub fn verify_sectors(&self, c: u16, h: u8, s: u8, count: u8) -> bool {
        if s == 0 || s > self.sectors_per_track {
            return false;
        }
        if h >= self.heads {
            return false;
        }
        if c >= self.cylinders {
            return false;
        }
        self.chs_to_lba(c, h, s) + count as u64 <= self.total_sectors()
    }

    /// Fill a track with the standard DOS format fill byte (0xF6).
    pub fn format_track(&mut self, c: u16, h: u8) -> std::io::Result<()> {
        let fill = vec![0xF6u8; self.sectors_per_track as usize * SECTOR_SIZE];
        self.write_sectors(c, h, 1, self.sectors_per_track, &fill)
    }

    /// The host file this image lives in, if it is file-backed.
    pub fn backing_path(&self) -> Option<&Path> {
        match &self.storage {
            DiskStorage::FileBacked { path, .. } => Some(path),
            DiskStorage::InMemory(_) => None,
        }
    }

    /// Whether `path` names the host file this image lives in.
    pub fn is_backed_by(&self, path: &Path) -> bool {
        match (self.backing_path(), std::fs::canonicalize(path)) {
            (Some(own), Ok(target)) => own == target,
            _ => false,
        }
    }

    /// Save the entire disk contents to a file.
    ///
    /// A file-backed image saved onto its own file is only flushed: guest
    /// writes already go straight to that file. Any other target is written
    /// to a temporary file beside it and renamed into place, so an existing
    /// file is replaced only by a complete copy.
    pub fn save_to_file(&mut self, path: &Path) -> std::io::Result<SaveOutcome> {
        if self.is_backed_by(path) {
            if let DiskStorage::FileBacked { file, .. } = &mut self.storage {
                file.sync_all()?;
            }
            return Ok(SaveOutcome::AlreadyBacking);
        }

        let mut temp_name = path
            .file_name()
            .ok_or_else(|| {
                std::io::Error::new(std::io::ErrorKind::InvalidInput, "not a file name")
            })?
            .to_os_string();
        temp_name.push(format!(".{}.tmp", std::process::id()));
        let temp_path = path.with_file_name(temp_name);

        let written = self
            .write_copy(&temp_path)
            .and_then(|()| std::fs::rename(&temp_path, path));
        if written.is_err() {
            let _ = std::fs::remove_file(&temp_path);
        }
        written.map(|()| SaveOutcome::Written)
    }

    /// Writes a complete copy of the image to a new file at `target`.
    fn write_copy(&mut self, target: &Path) -> std::io::Result<()> {
        let mut out = File::create(target)?;
        match &mut self.storage {
            DiskStorage::InMemory(data) => out.write_all(data)?,
            DiskStorage::FileBacked { file, .. } => {
                file.seek(SeekFrom::Start(0))?;
                std::io::copy(file, &mut out)?;
            }
        }
        out.sync_all()
    }

    fn chs_to_lba(&self, c: u16, h: u8, s: u8) -> u64 {
        (c as u64 * self.heads as u64 + h as u64) * self.sectors_per_track as u64 + (s as u64 - 1)
    }

    /// The CHS address of the sector with the number `lba`, or `None` if
    /// the disk has no such sector.
    pub fn lba_to_chs(&self, lba: u64) -> Option<(u16, u8, u8)> {
        if lba >= self.total_sectors() {
            return None;
        }
        let sectors_per_track = self.sectors_per_track as u64;
        let track = lba / sectors_per_track;
        let heads = self.heads as u64;
        Some((
            (track / heads) as u16,
            (track % heads) as u8,
            (lba % sectors_per_track) as u8 + 1,
        ))
    }

    fn chs_to_offset(&self, c: u16, h: u8, s: u8) -> u64 {
        self.chs_to_lba(c, h, s) * SECTOR_SIZE as u64
    }

    /// Format this disk with a FAT filesystem using the fatfs crate.
    /// Sets BPB geometry, media descriptor, cluster size, and root directory entries
    /// to match standard DOS floppy formats. Forces FAT12 for floppy-sized disks.
    pub fn format_fat(&mut self) -> std::io::Result<()> {
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
            .volume_id(new_volume_serial());

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
    pub fn copy_dir_to_fat(&mut self, host_dir: &Path) -> std::io::Result<usize> {
        self.require_writable()?;
        let volume = self.fat_volume()?;
        let copied = put_dir(&volume.root_dir(), host_dir);
        copied
    }

    fn require_writable(&self) -> std::io::Result<()> {
        if self.readonly {
            return Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "disk is write-protected",
            ));
        }
        Ok(())
    }

    /// Mounts the FAT volume on this disk.
    fn fat_volume(&mut self) -> std::io::Result<FatVolume<'_>> {
        let (start, length) = self.fat_volume_extent()?;
        let io = self.image_io();
        fatfs::FileSystem::new(PartitionSlice::new(io, start, length)?, fatfs::FsOptions::new())
    }
}

// --- FAT filesystem access for the `put` command ---

/// What a FAT volume can live on: an image file or an image held in memory.
trait ImageIo: Read + Write + Seek {}

impl<T: Read + Write + Seek> ImageIo for T {}

type FatVolume<'a> = fatfs::FileSystem<PartitionSlice<Box<dyn ImageIo + 'a>>>;

/// A partition of a disk image as a stream of its own: positions count
/// from the start of the partition, and nothing outside it can be read or
/// written.
struct PartitionSlice<T> {
    inner: T,
    /// Byte offset of the partition in the image.
    start: u64,
    /// Size of the partition in bytes.
    length: u64,
    /// Position within the partition, at most `length`.
    position: u64,
}

impl<T: Seek> PartitionSlice<T> {
    fn new(mut inner: T, start: u64, length: u64) -> std::io::Result<Self> {
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
    pub fn copy_file_to_fat(
        &mut self,
        host_data: &[u8],
        dos_path: &str,
    ) -> std::io::Result<String> {
        self.require_writable()?;
        let volume = self.fat_volume()?;
        let stored = put_path(&volume.root_dir(), dos_path, host_data);
        stored
    }
}

pub const HD_DEFAULT_SIZE_MB: u64 = 32;

/// A volume serial number for a freshly formatted disk, derived from the
/// clock like the one DOS writes. DOS tells disks apart by this number when
/// the change line fires, so two formatted disks must not share one.
fn new_volume_serial() -> u32 {
    use std::sync::atomic::{AtomicU32, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};

    static SEQUENCE: AtomicU32 = AtomicU32::new(0);
    let clock = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|elapsed| elapsed.as_nanos() as u64)
        .unwrap_or(0);
    let sequence = SEQUENCE.fetch_add(1, Ordering::Relaxed);
    ((clock >> 32) as u32 ^ clock as u32).wrapping_add(sequence.wrapping_mul(0x9E37_79B9))
}

/// Cylinders, heads and sectors per track.
type Geometry = (u16, u8, u8);

/// Size in bytes of a disk with the given geometry.
fn geometry_bytes((cylinders, heads, sectors_per_track): Geometry) -> u64 {
    cylinders as u64 * heads as u64 * sectors_per_track as u64 * SECTOR_SIZE as u64
}

/// Geometry of the floppy format that has `size` bytes, if there is one.
fn floppy_geometry(size: u64) -> Option<Geometry> {
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
fn geometry_from_size(size: u64) -> Geometry {
    floppy_geometry(size).unwrap_or_else(|| hd_geometry_from_size(size))
}

/// The geometry a FAT boot sector states in its BIOS parameter block, if
/// `sector` is such a boot sector and what it states is credible.
///
/// DOS computes its CHS addresses from these values, so they are the right
/// geometry for an image whose size alone does not tell it.
fn geometry_from_boot_sector(sector: &[u8]) -> Option<Geometry> {
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
const MAX_CYLINDERS: u64 = 1024;

/// Geometry of a hard disk of `size` bytes: as many whole cylinders as fit,
/// at least one and at most `MAX_CYLINDERS`.
fn hd_geometry_from_size(size: u64) -> Geometry {
    let cylinder_bytes = geometry_bytes((1, HD_HEADS, HD_SECTORS_PER_TRACK));
    let cylinders = (size / cylinder_bytes).clamp(1, MAX_CYLINDERS) as u16;
    (cylinders, HD_HEADS, HD_SECTORS_PER_TRACK)
}

/// Names DOS resolves to a device in every directory and with any
/// extension. A file stored under one of them could never be opened or
/// deleted from DOS.
const DOS_DEVICE_NAMES: [&str; 12] = [
    "CON", "PRN", "AUX", "NUL", "CLOCK$", "COM1", "COM2", "COM3", "COM4", "LPT1", "LPT2", "LPT3",
];

fn reject_device_name(name: &str) -> std::io::Result<()> {
    let base = name.split('.').next().unwrap_or("").trim_end();
    if DOS_DEVICE_NAMES.iter().any(|device| device.eq_ignore_ascii_case(base)) {
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

    let created = short_names(dir)?.into_iter().find(|short| !before.contains(short));
    Ok(created.unwrap_or_else(|| name.to_ascii_uppercase()))
}

/// Writes `data` to `dos_path` below `root`. The directories on the path
/// must exist. Returns the path as DOS shows it.
fn put_path<T: Read + Write + Seek>(
    root: &fatfs::Dir<T>,
    dos_path: &str,
    data: &[u8],
) -> std::io::Result<String> {
    let components: Vec<&str> = dos_path.split(['\\', '/']).filter(|c| !c.is_empty()).collect();
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

/// Port of the BIOS disk service trap: the INT 13h handler does
/// `OUT 0xB0, AL`, which makes the controller read the CPU registers, perform
/// the disk I/O, and set the return values and flags.
pub const DISK_TRAP_PORT: u16 = 0xB0;

/// Port through which the BIOS bootstrap (INT 19h) learns the boot order:
/// any write rewinds the list, each read returns the next BIOS drive number,
/// and `BOOT_ORDER_END` is returned once the list is exhausted.
pub const BOOT_ORDER_PORT: u16 = 0xB1;
pub const BOOT_ORDER_END: u8 = 0xFF;

/// BIOS drive numbers 0..FLOPPY_UNITS are floppy drives.
const FLOPPY_UNITS: usize = 4;

/// Parameters reported for a floppy drive that has no disk in it: those of
/// a 3.5" 1.44M drive.
const EMPTY_FLOPPY_DRIVE_TYPE: u8 = 0x04;
const EMPTY_FLOPPY_MAX_CYLINDER: u16 = 79;
const EMPTY_FLOPPY_SECTORS: u8 = 18;
const EMPTY_FLOPPY_MAX_HEAD: u8 = 1;

/// INT 13h status codes: returned in AH and kept in the BIOS data area.
const STATUS_OK: u8 = 0x00;
const STATUS_BAD_COMMAND: u8 = 0x01;
const STATUS_WRITE_PROTECTED: u8 = 0x03;
const STATUS_SECTOR_NOT_FOUND: u8 = 0x04;
const STATUS_DISK_CHANGED: u8 = 0x06;
const STATUS_NO_PARAMETERS: u8 = 0x07;
const STATUS_NOT_READY: u8 = 0x80;

/// BIOS data area: the status of the last diskette operation, of the last
/// hard disk operation, and the number of hard disks.
const BDA_DISKETTE_STATUS: usize = 0x0441;
const BDA_HARD_DISK_STATUS: usize = 0x0474;
const BDA_HARD_DISK_COUNT: usize = 0x0475;

/// The drive type AH=08h reports for a floppy disk of `size` bytes: the
/// drive that disk is made for.
fn floppy_drive_type(size: u64) -> u8 {
    match size {
        163_840 | 184_320 | 327_680 | 368_640 => 0x01, // 5.25" 360K
        1_228_800 => 0x02,                             // 5.25" 1.2M
        737_280 => 0x03,                               // 3.5" 720K
        2_949_120 => 0x06,                             // 3.5" 2.88M
        _ => 0x04,                                     // 3.5" 1.44M
    }
}

/// The disk address packet a caller of the extended transfer functions
/// passes in DS:SI: its own size (byte 0), the number of blocks (word at 2),
/// the transfer buffer as offset and segment (words at 4 and 6) and the
/// 64-bit number of the first block (at 8).
struct AddressPacket {
    /// Linear address of the packet itself.
    address: usize,
    /// Number of blocks to transfer.
    count: u16,
    /// Linear address of the transfer buffer.
    buffer: usize,
    /// Number of the first block.
    lba: u64,
}

impl AddressPacket {
    const MIN_SIZE: u8 = 16;

    /// Reads the packet at the linear address `address`; `None` if its size
    /// field says that it is too short to hold a block number.
    fn read(cpu: &Cpu, address: usize) -> Option<Self> {
        if cpu.memory.read_byte(address) < Self::MIN_SIZE {
            return None;
        }
        let word = |offset: usize| cpu.memory.read_word(address + offset);
        let buffer = ((word(6) as usize) << 4) + word(4) as usize;
        let lba = (8..16)
            .step_by(2)
            .rev()
            .fold(0u64, |lba, offset| lba << 16 | word(offset) as u64);
        Some(Self {
            address,
            count: word(2),
            buffer: buffer & 0xFFFFF,
            lba,
        })
    }

    /// Stores the number of blocks that were transferred in the packet,
    /// where the caller looks for it after a failed transfer.
    fn set_transferred(&self, cpu: &mut Cpu, count: u16) {
        DiskController::write_guest(cpu, self.address + 2, &count.to_le_bytes());
    }
}

/// A run of sectors that lie on one track.
struct TrackRun {
    cylinder: u16,
    head: u8,
    sector: u8,
    count: u8,
}

/// Splits a transfer of `count` sectors that starts at a CHS address into
/// runs of sectors on one track each: a transfer that reaches the end of a
/// track continues on the next head and, after the last head, on the next
/// cylinder.
fn track_runs(
    heads: u8,
    sectors_per_track: u8,
    start: (u16, u8, u8),
    count: u16,
) -> impl Iterator<Item = TrackRun> {
    let (mut cylinder, mut head, mut sector) = start;
    let mut remaining = count;
    std::iter::from_fn(move || {
        if remaining == 0 {
            return None;
        }
        let left_on_track = sectors_per_track.saturating_sub(sector) as u16 + 1;
        let run = TrackRun {
            cylinder,
            head,
            sector,
            count: remaining.min(left_on_track).min(255) as u8,
        };
        remaining -= run.count as u16;
        sector = 1;
        if head as u16 + 1 < heads as u16 {
            head += 1;
        } else {
            head = 0;
            cylinder = cylinder.saturating_add(1);
        }
        Some(run)
    })
}

pub struct DiskController {
    pub drives: Vec<(u8, DiskImage)>, // (drive number, image)
    boot_order: Vec<u8>,
    boot_cursor: usize,
    /// Floppy units that exist as drives, with or without a disk in them.
    /// A: and B: always do; further units once an image was attached to them.
    floppy_installed: [bool; FLOPPY_UNITS],
    /// Change line of each floppy drive: the disk was inserted, removed or
    /// rewritten from the host since the guest last asked (AH=16h).
    floppy_changed: [bool; FLOPPY_UNITS],
}

impl DiskController {
    pub fn new() -> Self {
        Self {
            drives: Vec::new(),
            boot_order: vec![0x00, 0x80], // floppy A:, then hard disk 0
            boot_cursor: 0,
            floppy_installed: [true, true, false, false],
            floppy_changed: [false; FLOPPY_UNITS],
        }
    }

    /// Sets the BIOS drive numbers the bootstrap tries, in order.
    pub fn set_boot_order(&mut self, order: Vec<u8>) {
        self.boot_order = order;
        self.boot_cursor = 0;
    }

    pub fn boot_order(&self) -> &[u8] {
        &self.boot_order
    }

    /// Puts `image` into `drive`, in place of whatever was there.
    pub fn attach(&mut self, drive: u8, image: DiskImage) {
        if let Some(unit) = Self::floppy_unit(drive) {
            self.floppy_installed[unit] = true;
            self.floppy_changed[unit] = true;
        }
        self.drives.retain(|(attached, _)| *attached != drive);
        self.drives.push((drive, image));
    }

    /// Raises the change line of a floppy drive after its disk was modified
    /// from the host, so the guest drops what it has cached of that disk.
    pub fn mark_changed(&mut self, drive: u8) {
        if let Some(unit) = Self::floppy_unit(drive) {
            self.floppy_changed[unit] = true;
        }
    }

    /// Copies a host file into the FAT volume on the disk in `drive` and
    /// returns the path under which DOS sees it.
    pub fn put_file(
        &mut self,
        drive: u8,
        data: &[u8],
        dos_path: &str,
    ) -> std::io::Result<String> {
        let result = self.disk_in(drive)?.copy_file_to_fat(data, dos_path);
        self.mark_changed(drive);
        result
    }

    /// Copies a host directory into the FAT volume on the disk in `drive`
    /// and returns the number of files copied.
    pub fn put_dir(&mut self, drive: u8, host_dir: &Path) -> std::io::Result<usize> {
        let result = self.disk_in(drive)?.copy_dir_to_fat(host_dir);
        self.mark_changed(drive);
        result
    }

    fn disk_in(&mut self, drive: u8) -> std::io::Result<&mut DiskImage> {
        self.find_drive(drive).ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::NotFound, "no disk in the drive")
        })
    }

    fn floppy_unit(drive: u8) -> Option<usize> {
        ((drive as usize) < FLOPPY_UNITS).then_some(drive as usize)
    }

    /// Whether `drive` is a floppy drive that exists, with or without a disk.
    fn is_installed_floppy(&self, drive: u8) -> bool {
        Self::floppy_unit(drive).is_some_and(|unit| self.floppy_installed[unit])
    }

    fn floppy_drive_count(&self) -> u8 {
        self.floppy_installed.iter().filter(|installed| **installed).count() as u8
    }

    pub(crate) fn find_drive(&mut self, drive: u8) -> Option<&mut DiskImage> {
        self.drives
            .iter_mut()
            .find(|(d, _)| *d == drive)
            .map(|(_, img)| img)
    }

    /// The drive whose image lives in the host file `path`, if any.
    pub fn drive_backed_by(&self, path: &Path) -> Option<u8> {
        self.drives
            .iter()
            .find(|(_, img)| img.is_backed_by(path))
            .map(|(drive, _)| *drive)
    }

    /// Points ES:DI at the diskette parameter table (the INT 1Eh vector).
    fn point_to_diskette_parameters(cpu: &mut Cpu) {
        let offset = cpu.memory.read_word(0x0078);
        let segment = cpu.memory.read_word(0x007A);
        cpu.registers.es.reg_mut().set(segment);
        cpu.registers.di.set(offset);
    }

    /// Where the BIOS data area keeps the status of the last operation on
    /// `drive`: the diskettes and the hard disks have a byte each.
    fn status_address(drive: u8) -> usize {
        if drive < 0x80 {
            BDA_DISKETTE_STATUS
        } else {
            BDA_HARD_DISK_STATUS
        }
    }

    /// Ends an INT 13h call: AH holds the status, CF is set unless the
    /// status is 0, and the status is recorded for AH=01h.
    fn finish(cpu: &mut Cpu, drive: u8, status: u8) {
        cpu.registers.ax.set_high(status);
        if status == STATUS_OK {
            cpu.unset_flag(Carry);
        } else {
            cpu.set_flag(Carry);
        }
        cpu.memory.write_byte(Self::status_address(drive), status);
    }

    /// The CHS address in CX and DH: cylinder (10 bits), head and sector
    /// (1-based).
    fn decode_chs(cpu: &Cpu) -> (u16, u8, u8) {
        let cl = cpu.registers.cx.low();
        let cylinder = cpu.registers.cx.high() as u16 | ((cl as u16 & 0xC0) << 2);
        (cylinder, cpu.registers.dx.high(), cl & 0x3F)
    }

    /// The sector count in AL. AL=0 is the IBM PC BIOS convention for
    /// "transfer 256 sectors".
    fn sector_count(cpu: &Cpu) -> u16 {
        match cpu.registers.ax.low() {
            0 => 256,
            count => count as u16,
        }
    }

    /// Linear address of the transfer buffer at ES:BX.
    fn buffer_address(cpu: &Cpu) -> usize {
        let es = cpu.registers.es.reg().word() as usize;
        let bx = cpu.registers.bx.word() as usize;
        ((es << 4) + bx) & 0xFFFFF
    }

    /// Copies `data` into guest memory at the linear address `address`,
    /// wrapping at 1 MB, and returns the address behind it.
    fn write_guest(cpu: &mut Cpu, address: usize, data: &[u8]) -> usize {
        for (i, byte) in data.iter().enumerate() {
            cpu.memory.write_byte((address + i) & 0xFFFFF, *byte);
        }
        (address + data.len()) & 0xFFFFF
    }

    /// Reads `length` bytes of guest memory at the linear address `address`,
    /// wrapping at 1 MB.
    fn read_guest(cpu: &Cpu, address: usize, length: usize) -> Vec<u8> {
        (0..length)
            .map(|i| cpu.memory.read_byte((address + i) & 0xFFFFF))
            .collect()
    }

    /// The status for a write that failed.
    fn write_error_status(error: &std::io::Error) -> u8 {
        if error.kind() == std::io::ErrorKind::PermissionDenied {
            STATUS_WRITE_PROTECTED
        } else {
            STATUS_SECTOR_NOT_FOUND
        }
    }

    fn hard_disk_count(&self) -> u8 {
        self.drives.iter().filter(|(drive, _)| *drive >= 0x80).count() as u8
    }

    /// Whether `drive` is a floppy drive that exists or a hard disk with an
    /// image attached.
    fn drive_exists(&self, drive: u8) -> bool {
        self.is_installed_floppy(drive)
            || (drive >= 0x80 && self.drives.iter().any(|(attached, _)| *attached == drive))
    }

    fn handle_trap(&mut self, cpu: &mut Cpu) {
        let function = cpu.registers.ax.high();
        let drive = cpu.registers.dx.low();

        // Update the BDA hard disk count dynamically
        cpu.memory.write_byte(BDA_HARD_DISK_COUNT, self.hard_disk_count());

        let status = match function {
            // AH=00: Reset
            0x00 => STATUS_OK,

            // AH=01: Status of the last operation, in AH and AL
            0x01 => {
                let status = cpu.memory.read_byte(Self::status_address(drive));
                cpu.registers.ax.set_low(status);
                status
            }

            0x02 => self.read(cpu, drive),
            0x03 => self.write(cpu, drive),
            0x04 => self.verify(cpu, drive),
            0x05 => self.format(cpu, drive),
            0x08 => self.drive_parameters(cpu, drive),

            // AH=09/0C/0D/10/11/14: initialise, seek, reset, test ready,
            // recalibrate and diagnose have nothing to do on an image. They
            // succeed for a drive that exists.
            0x09 | 0x0C | 0x0D | 0x10 | 0x11 | 0x14 => {
                if self.drive_exists(drive) {
                    STATUS_OK
                } else {
                    STATUS_BAD_COMMAND
                }
            }

            // AH=15: Disk type. AH carries the type instead of a status.
            0x15 => {
                let disk_type = self.disk_type(cpu, drive);
                Self::finish(cpu, drive, STATUS_OK);
                cpu.registers.ax.set_high(disk_type);
                return;
            }

            0x16 => self.change_line(drive),

            // AH=41: Extensions installation check. The caller passes 55AAh
            // in BX; hard disks have the extended disk access functions
            // (42h-44h, 47h, 48h). AH carries the version instead of a
            // status.
            0x41 => {
                let asked_properly = cpu.registers.bx.word() == 0x55AA;
                if asked_properly && drive >= 0x80 && self.find_drive(drive).is_some() {
                    cpu.registers.bx.set(0xAA55);
                    cpu.registers.cx.set(0x0001); // extended disk access functions
                    Self::finish(cpu, drive, STATUS_OK);
                    cpu.registers.ax.set_high(0x01); // version 1.x
                    return;
                }
                STATUS_BAD_COMMAND
            }

            0x42 => self.read_lba(cpu, drive),
            0x43 => self.write_lba(cpu, drive),
            0x44 => self.verify_lba(cpu, drive),
            0x47 => self.seek_lba(cpu, drive),
            0x48 => self.extended_parameters(cpu, drive),

            _ => STATUS_BAD_COMMAND,
        };
        Self::finish(cpu, drive, status);
    }

    /// AH=02: Read sectors. A transfer that runs past the end of a track
    /// continues on the next one. Returns the status; AL is set to the
    /// number of sectors transferred.
    fn read(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let count = Self::sector_count(cpu);
        let (cylinder, head, sector) = Self::decode_chs(cpu);

        // CHS sector is 1-based; 0 is invalid.
        if sector == 0 {
            return STATUS_SECTOR_NOT_FOUND;
        }
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };
        // Sector number must fit within the disk's track geometry.
        if sector > disk.sectors_per_track {
            return STATUS_SECTOR_NOT_FOUND;
        }

        let mut address = Self::buffer_address(cpu);
        let mut done: u16 = 0;
        let runs = track_runs(disk.heads, disk.sectors_per_track, (cylinder, head, sector), count);
        for run in runs {
            match disk.read_sectors(run.cylinder, run.head, run.sector, run.count) {
                Ok(data) => address = Self::write_guest(cpu, address, &data),
                Err(e) => {
                    debug!("Disk read error: {}", e);
                    cpu.registers.ax.set_low(done as u8);
                    return STATUS_SECTOR_NOT_FOUND;
                }
            }
            done += run.count as u16;
        }
        // AL reports count transferred; 256 wraps to 0 per BIOS convention.
        cpu.registers.ax.set_low(count as u8);
        STATUS_OK
    }

    /// AH=03: Write sectors, continuing across tracks like a read.
    fn write(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let count = Self::sector_count(cpu);
        let (cylinder, head, sector) = Self::decode_chs(cpu);

        if sector == 0 {
            return STATUS_SECTOR_NOT_FOUND;
        }
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };
        if sector > disk.sectors_per_track {
            return STATUS_SECTOR_NOT_FOUND;
        }

        let mut address = Self::buffer_address(cpu);
        let mut done: u16 = 0;
        let runs = track_runs(disk.heads, disk.sectors_per_track, (cylinder, head, sector), count);
        for run in runs {
            let length = run.count as usize * SECTOR_SIZE;
            let data = Self::read_guest(cpu, address, length);
            address = (address + length) & 0xFFFFF;

            if let Err(e) = disk.write_sectors(run.cylinder, run.head, run.sector, run.count, &data)
            {
                debug!("Disk write error: {}", e);
                cpu.registers.ax.set_low(done as u8);
                return Self::write_error_status(&e);
            }
            done += run.count as u16;
        }
        // AL reports count transferred; 256 wraps to 0 per BIOS convention.
        cpu.registers.ax.set_low(count as u8);
        STATUS_OK
    }

    /// AH=04: Verify sectors. An image has nothing to verify; only the
    /// address is checked.
    fn verify(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let count = cpu.registers.ax.low();
        let (cylinder, head, sector) = Self::decode_chs(cpu);

        match self.find_drive(drive) {
            Some(disk) if disk.verify_sectors(cylinder, head, sector, count) => STATUS_OK,
            Some(_) => STATUS_SECTOR_NOT_FOUND,
            None => STATUS_NOT_READY,
        }
    }

    /// AH=05: Format track
    fn format(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let (cylinder, head, _) = Self::decode_chs(cpu);

        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };
        match disk.format_track(cylinder, head) {
            Ok(()) => STATUS_OK,
            Err(e) => {
                debug!("Disk format error: {}", e);
                Self::write_error_status(&e)
            }
        }
    }

    /// AH=08: Get drive parameters. CX and DH hold the highest cylinder,
    /// sector and head; DL the number of drives of that kind. For a floppy
    /// drive BX holds the drive type and ES:DI points to the diskette
    /// parameter table.
    fn drive_parameters(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let installed_floppy = self.is_installed_floppy(drive);
        let (max_cylinder, max_head, sectors, drive_type) = match self.find_drive(drive) {
            Some(disk) => (
                disk.cylinders - 1,
                disk.heads - 1,
                disk.sectors_per_track,
                floppy_drive_type(disk.total_bytes()),
            ),
            // The drive exists but holds no disk: describe the drive.
            None if installed_floppy => (
                EMPTY_FLOPPY_MAX_CYLINDER,
                EMPTY_FLOPPY_MAX_HEAD,
                EMPTY_FLOPPY_SECTORS,
                EMPTY_FLOPPY_DRIVE_TYPE,
            ),
            None => return STATUS_NO_PARAMETERS,
        };

        cpu.registers.ax.set_low(0x00);
        cpu.registers.cx.set_high((max_cylinder & 0xFF) as u8);
        cpu.registers.cx.set_low((sectors & 0x3F) | (((max_cylinder >> 8) as u8) << 6));
        cpu.registers.dx.set_high(max_head);
        if drive < 0x80 {
            cpu.registers.bx.set(drive_type as u16);
            cpu.registers.dx.set_low(self.floppy_drive_count());
            Self::point_to_diskette_parameters(cpu);
        } else {
            // BX describes floppy drives only and is left alone.
            cpu.registers.dx.set_low(self.hard_disk_count());
        }
        STATUS_OK
    }

    /// AH=15: Get disk type: 0 = no such drive, 2 = floppy drive with change
    /// line, 3 = hard disk, whose size in sectors is returned in CX:DX.
    ///
    /// Every floppy drive that exists has a change line, whatever disk is in
    /// it (or none): the guest decides once, at start-up, whether it may
    /// rely on AH=16h.
    fn disk_type(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        if drive < 0x80 {
            return if self.is_installed_floppy(drive) { 0x02 } else { 0x00 };
        }
        match self.find_drive(drive) {
            Some(disk) => {
                let total = disk.total_sectors() as u32;
                cpu.registers.cx.set((total >> 16) as u16);
                cpu.registers.dx.set((total & 0xFFFF) as u16);
                0x03
            }
            None => 0x00,
        }
    }

    /// AH=16: Change line status. A change is reported once.
    fn change_line(&mut self, drive: u8) -> u8 {
        match Self::floppy_unit(drive) {
            None => STATUS_BAD_COMMAND, // hard disks have no change line
            Some(_) if self.find_drive(drive).is_none() => STATUS_NOT_READY, // no disk
            Some(unit) if self.floppy_changed[unit] => {
                self.floppy_changed[unit] = false;
                STATUS_DISK_CHANGED
            }
            Some(_) => STATUS_OK,
        }
    }

    /// The linear address DS:SI points to, where the extended functions
    /// find their argument.
    fn ds_si(cpu: &Cpu) -> usize {
        let ds = cpu.registers.ds.reg().word() as usize;
        let si = cpu.registers.si.word() as usize;
        ((ds << 4) + si) & 0xFFFFF
    }

    /// The CHS address of block `index` of the range a packet names. Blocks
    /// are numbered with 64 bits; the disk decides which of them exist.
    fn block_address(
        disk: &DiskImage,
        packet: &AddressPacket,
        index: u16,
    ) -> std::io::Result<(u16, u8, u8)> {
        packet
            .lba
            .checked_add(index as u64)
            .and_then(|lba| disk.lba_to_chs(lba))
            .ok_or_else(DiskImage::outside_the_disk)
    }

    /// AH=42: Extended read. On an error the packet says how many blocks
    /// were transferred.
    fn read_lba(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let Some(packet) = AddressPacket::read(cpu, Self::ds_si(cpu)) else {
            return STATUS_BAD_COMMAND;
        };
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };

        let mut address = packet.buffer;
        for done in 0..packet.count {
            let block = Self::block_address(disk, &packet, done)
                .and_then(|(c, h, s)| disk.read_sectors(c, h, s, 1));
            match block {
                Ok(data) => address = Self::write_guest(cpu, address, &data),
                Err(e) => {
                    debug!("LBA read error: {}", e);
                    packet.set_transferred(cpu, done);
                    return STATUS_SECTOR_NOT_FOUND;
                }
            }
        }
        STATUS_OK
    }

    /// AH=43: Extended write. On an error the packet says how many blocks
    /// were transferred.
    fn write_lba(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let Some(packet) = AddressPacket::read(cpu, Self::ds_si(cpu)) else {
            return STATUS_BAD_COMMAND;
        };
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };

        let mut address = packet.buffer;
        for done in 0..packet.count {
            let data = Self::read_guest(cpu, address, SECTOR_SIZE);
            address = (address + SECTOR_SIZE) & 0xFFFFF;

            let written = Self::block_address(disk, &packet, done)
                .and_then(|(c, h, s)| disk.write_sectors(c, h, s, 1, &data));
            if let Err(e) = written {
                debug!("LBA write error: {}", e);
                packet.set_transferred(cpu, done);
                return Self::write_error_status(&e);
            }
        }
        STATUS_OK
    }

    /// AH=44: Extended verify. An image has nothing to verify; the range of
    /// blocks is checked, and on an error the packet says how many of them
    /// are on the disk.
    fn verify_lba(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let Some(packet) = AddressPacket::read(cpu, Self::ds_si(cpu)) else {
            return STATUS_BAD_COMMAND;
        };
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };

        let on_disk = disk.total_sectors().saturating_sub(packet.lba);
        if packet.count as u64 > on_disk {
            packet.set_transferred(cpu, on_disk as u16);
            return STATUS_SECTOR_NOT_FOUND;
        }
        STATUS_OK
    }

    /// AH=47: Extended seek. Only the block number is checked.
    fn seek_lba(&mut self, cpu: &Cpu, drive: u8) -> u8 {
        let Some(packet) = AddressPacket::read(cpu, Self::ds_si(cpu)) else {
            return STATUS_BAD_COMMAND;
        };
        match self.find_drive(drive) {
            Some(disk) if disk.lba_to_chs(packet.lba).is_some() => STATUS_OK,
            Some(_) => STATUS_SECTOR_NOT_FOUND,
            None => STATUS_NOT_READY,
        }
    }

    /// AH=48: Extended drive parameters. The caller's buffer at DS:SI
    /// starts with its size and receives the geometry, the number of
    /// sectors and the sector size.
    fn extended_parameters(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        /// Size of the result in version 1 of the interface.
        const RESULT_SIZE: u16 = 0x1A;
        /// Flags: the CHS values are valid; the medium is removable; the
        /// drive has a change line.
        const CHS_VALID: u16 = 0x0002;
        const REMOVABLE: u16 = 0x0004;
        const CHANGE_LINE: u16 = 0x0010;

        let buffer = Self::ds_si(cpu);
        if cpu.memory.read_word(buffer) < RESULT_SIZE {
            return STATUS_BAD_COMMAND;
        }
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };
        let flags = if drive < 0x80 {
            CHS_VALID | REMOVABLE | CHANGE_LINE
        } else {
            CHS_VALID
        };

        let mut result = Vec::with_capacity(RESULT_SIZE as usize);
        result.extend_from_slice(&RESULT_SIZE.to_le_bytes());
        result.extend_from_slice(&flags.to_le_bytes());
        result.extend_from_slice(&(disk.cylinders as u32).to_le_bytes());
        result.extend_from_slice(&(disk.heads as u32).to_le_bytes());
        result.extend_from_slice(&(disk.sectors_per_track as u32).to_le_bytes());
        result.extend_from_slice(&disk.total_sectors().to_le_bytes());
        result.extend_from_slice(&(SECTOR_SIZE as u16).to_le_bytes());
        Self::write_guest(cpu, buffer, &result);
        STATUS_OK
    }

    /// Remove a drive and return its image.
    pub fn detach(&mut self, drive: u8) -> Option<DiskImage> {
        if let Some(pos) = self.drives.iter().position(|(d, _)| *d == drive) {
            self.mark_changed(drive);
            Some(self.drives.remove(pos).1)
        } else {
            None
        }
    }
}

impl IoDevice for DiskController {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        if port != BOOT_ORDER_PORT {
            return 0xFF;
        }
        match self.boot_order.get(self.boot_cursor) {
            Some(&drive) => {
                self.boot_cursor += 1;
                drive
            }
            None => BOOT_ORDER_END,
        }
    }

    fn port_out_byte(&mut self, port: u16, _value: u8, cpu: &mut Cpu) {
        match port {
            DISK_TRAP_PORT => self.handle_trap(cpu),
            BOOT_ORDER_PORT => self.boot_cursor = 0,
            _ => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Regression tests — INT 13h disk services.
// ---------------------------------------------------------------------------
#[cfg(test)]
mod inspection_tests {
    use super::*;
    use crate::vm::runtime::{CpuFlag, Runtime};

    /// INT 13h AH=02 with AL=0 transfers 256 sectors, as the IBM PC BIOS
    /// does, rather than none.
    #[test]
    fn int13h_al_zero_should_mean_256_sectors() {
        let mut rt = Runtime::new_test();

        // Pick a disk big enough for 256 sectors at CHS(0,0,1).
        // 10 MB hard disk: geometry is (cyl, 16, 63) — far more than 256 sectors.
        let mut img = DiskImage::new_in_memory_hard_disk(10);

        // Seed the first 256 sectors with a per-sector marker byte so we
        // can tell a real read from a silent no-op.
        for sector in 0..256u16 {
            let heads = img.heads as u16;
            let spt = img.sectors_per_track as u16;
            let c = sector / (heads * spt);
            let h = ((sector / spt) % heads) as u8;
            let s = ((sector % spt) + 1) as u8;
            let marker = sector as u8;
            let data: Vec<u8> = vec![marker; 512];
            img.write_sectors(c, h, s, 1, &data).expect("seed write");
        }

        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x80, img);

        // INT 13h AH=02, AL=0 (=>256), CHS=(0,0,1), ES:BX=0x1000:0000, DL=0x80
        rt.cpu.registers.ax.set_high(0x02);
        rt.cpu.registers.ax.set_low(0x00);
        rt.cpu.registers.cx.set_high(0x00);
        rt.cpu.registers.cx.set_low(0x01);
        rt.cpu.registers.dx.set_high(0x00);
        rt.cpu.registers.dx.set_low(0x80);
        rt.cpu.registers.es.reg_mut().set(0x1000);
        rt.cpu.registers.bx.set(0x0000);

        disk_ctrl.port_out_byte(0xB0, 0x00, &mut rt.cpu);

        // Real BIOS would have placed 256 * 512 = 131_072 bytes at 0x10000
        // with markers 0, 1, 2, ..., 255. Check the first byte of sector 0
        // and the first byte of sector 255.
        let sector_0_byte = rt.cpu.memory.read_byte(0x10000);
        let sector_255_byte = rt.cpu.memory.read_byte(0x10000 + 255 * 512);

        assert_eq!(
            sector_0_byte, 0,
            "Sector 0 marker missing — AL=0 was treated as 0 sectors rather \
             than 256"
        );
        assert_eq!(
            sector_255_byte, 255,
            "Sector 255 marker missing — AL=0 only transferred a partial \
             range (or nothing)"
        );
    }

    /// INT 13h AH=08 packs the geometry as CH = max cylinder bits 0-7,
    /// CL bits 6-7 = max cylinder bits 8-9, CL bits 0-5 = SPT, DH = max head;
    /// for a 1.44MB floppy that is max_cyl=79, SPT=18, max_head=1.
    #[test]
    fn int13h_get_drive_params_floppy_returns_correct_geometry() {
        let mut rt = Runtime::new_test();
        let img = DiskImage::new_in_memory_sized(1_474_560, false);
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, img);

        rt.cpu.registers.ax.set_high(0x08);
        rt.cpu.registers.dx.set_low(0x00); // drive A
        disk_ctrl.port_out_byte(0xB0, 0x00, &mut rt.cpu);

        let ch = rt.cpu.registers.cx.high();
        let cl = rt.cpu.registers.cx.low();
        let dh = rt.cpu.registers.dx.high();

        let max_cyl = (ch as u16) | (((cl as u16) & 0xC0) << 2);
        let spt = cl & 0x3F;
        assert_eq!(max_cyl, 79, "max_cyl for 80-cylinder floppy should be 79");
        assert_eq!(spt, 18, "SPT=18 for 1.44M floppy");
        assert_eq!(dh, 1, "max_head for 2-head floppy should be 1");
        assert_eq!(
            rt.cpu.registers.ax.high(),
            0x00,
            "AH=0 on success"
        );
    }

    /// A write to a read-only disk fails with CF set and AH=0x03 (write
    /// protected).
    #[test]
    fn int13h_write_to_readonly_disk_returns_0x03() {
        let mut rt = Runtime::new_test();
        // 1.44 MB floppy, readonly
        let img = DiskImage::new_in_memory_sized(1_474_560, true);
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, img);

        // INT 13h AH=03 (write), AL=1, CHS=(0,0,1), drive=0
        rt.cpu.registers.ax.set_high(0x03);
        rt.cpu.registers.ax.set_low(0x01);
        rt.cpu.registers.cx.set_high(0x00);
        rt.cpu.registers.cx.set_low(0x01);
        rt.cpu.registers.dx.set_high(0x00);
        rt.cpu.registers.dx.set_low(0x00);
        rt.cpu.registers.es.reg_mut().set(0x1000);
        rt.cpu.registers.bx.set(0x0000);

        disk_ctrl.port_out_byte(0xB0, 0x00, &mut rt.cpu);

        assert!(
            rt.cpu.check_flag(CpuFlag::Carry),
            "CF should be set for write-protect error"
        );
        assert_eq!(
            rt.cpu.registers.ax.high(),
            0x03,
            "AH should be 0x03 (write protected), got 0x{:02X}",
            rt.cpu.registers.ax.high()
        );
    }

    /// A CHS sector number above the track's sectors per track (S=20 on a
    /// 1.44 MB floppy with SPT=18) fails with CF set and AH=0x04 (sector
    /// not found) instead of underflowing the per-track sector count.
    #[test]
    fn int13h_sector_above_spt_should_return_error_not_underflow() {
        let mut rt = Runtime::new_test();

        // 1.44 MB floppy: (80, 2, 18). SPT = 18.
        let img = DiskImage::new_in_memory_sized(1_474_560, false);
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, img);

        // INT 13h AH=02, AL=1, CHS=(0, 0, 20). S=20 > SPT=18 → should error.
        rt.cpu.registers.ax.set_high(0x02);
        rt.cpu.registers.ax.set_low(0x01);
        rt.cpu.registers.cx.set_high(0x00);
        rt.cpu.registers.cx.set_low(20); // S=20, cyl_hi=0
        rt.cpu.registers.dx.set_high(0x00);
        rt.cpu.registers.dx.set_low(0x00);
        rt.cpu.registers.es.reg_mut().set(0x1000);
        rt.cpu.registers.bx.set(0x0000);

        disk_ctrl.port_out_byte(0xB0, 0x00, &mut rt.cpu);

        assert!(
            rt.cpu.check_flag(CpuFlag::Carry),
            "Carry should be set on an invalid sector number"
        );
        assert_eq!(
            rt.cpu.registers.ax.high(),
            0x04,
            "AH should be 0x04 (sector not found); got 0x{:02X}",
            rt.cpu.registers.ax.high()
        );
    }

    /// A file in the host's temp directory that is removed again on drop.
    struct ScratchFile(PathBuf);

    impl ScratchFile {
        fn new(name: &str) -> Self {
            let path = std::env::temp_dir()
                .join(format!("emu8086-test-{}-{}", std::process::id(), name));
            let _ = std::fs::remove_file(&path);
            Self(path)
        }

        fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for ScratchFile {
        fn drop(&mut self) {
            let _ = std::fs::remove_file(&self.0);
        }
    }

    /// Floppy-sized content in which every sector is filled with its own
    /// number, so a read shows which sector it really came from.
    fn numbered_sectors() -> Vec<u8> {
        (0..FLOPPY_144_SIZE as usize).map(|i| (i / SECTOR_SIZE) as u8).collect()
    }

    // -----------------------------------------------------------------------
    // Saving an image to a host file
    // -----------------------------------------------------------------------

    /// Saving a file-backed image onto the file it lives in must not open
    /// that file for writing: truncating it would empty the image itself.
    #[test]
    fn save_onto_own_backing_file_keeps_the_image() {
        let scratch = ScratchFile::new("save-self.img");
        let content = numbered_sectors();
        std::fs::write(scratch.path(), &content).unwrap();

        let mut img = DiskImage::open(scratch.path(), false).unwrap();
        img.save_to_file(scratch.path())
            .expect("saving an image onto its own file");

        assert_eq!(std::fs::read(scratch.path()).unwrap(), content);
        assert_eq!(
            img.read_sectors(0, 1, 1, 1).unwrap(),
            content[18 * SECTOR_SIZE..19 * SECTOR_SIZE]
        );
    }

    #[test]
    fn save_tells_a_copy_from_the_image_s_own_file() {
        let own = ScratchFile::new("save-own.img");
        let other = ScratchFile::new("save-other.img");
        std::fs::write(own.path(), numbered_sectors()).unwrap();

        let mut img = DiskImage::open(own.path(), false).unwrap();
        assert_eq!(
            img.save_to_file(own.path()).unwrap(),
            SaveOutcome::AlreadyBacking
        );
        assert_eq!(img.save_to_file(other.path()).unwrap(), SaveOutcome::Written);
        assert_eq!(std::fs::read(other.path()).unwrap(), numbered_sectors());
    }

    /// The copy replaces an existing target as a whole, and nothing else is
    /// left behind in the target's directory.
    #[test]
    fn save_replaces_an_existing_file_with_a_complete_copy() {
        let target = ScratchFile::new("save-target.img");
        std::fs::write(target.path(), vec![0xEEu8; 3 * FLOPPY_144_SIZE as usize]).unwrap();

        let mut img = DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false);
        img.write_sectors(0, 0, 1, 1, &[0x5Au8; SECTOR_SIZE]).unwrap();
        assert_eq!(img.save_to_file(target.path()).unwrap(), SaveOutcome::Written);

        let saved = std::fs::read(target.path()).unwrap();
        assert_eq!(saved.len() as u64, FLOPPY_144_SIZE);
        assert_eq!(saved[..SECTOR_SIZE], [0x5Au8; SECTOR_SIZE]);
        assert!(saved[SECTOR_SIZE..].iter().all(|b| *b == 0));

        let target_name = target.path().file_name().unwrap().to_string_lossy().into_owned();
        let leftovers: Vec<String> = std::fs::read_dir(target.path().parent().unwrap())
            .unwrap()
            .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
            .filter(|name| name.starts_with(&target_name) && *name != target_name)
            .collect();
        assert!(leftovers.is_empty(), "temporary files left behind: {:?}", leftovers);
    }

    #[test]
    fn controller_knows_which_drive_lives_in_a_host_file() {
        let scratch = ScratchFile::new("backing.img");
        std::fs::write(scratch.path(), numbered_sectors()).unwrap();

        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false));
        disk_ctrl.attach(0x01, DiskImage::open(scratch.path(), false).unwrap());

        assert_eq!(disk_ctrl.drive_backed_by(scratch.path()), Some(0x01));
        assert_eq!(
            disk_ctrl.drive_backed_by(&scratch.path().with_extension("other")),
            None
        );
    }

    /// A drive holds one disk: attaching to a drive that already has one
    /// replaces it, so a later detach leaves the drive empty.
    #[test]
    fn attach_replaces_the_disk_in_the_drive() {
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false));
        disk_ctrl.attach(0x00, DiskImage::new_in_memory_sized(737_280, false));
        assert_eq!(disk_ctrl.find_drive(0x00).unwrap().total_bytes(), 737_280);
        assert_eq!(disk_ctrl.drives.len(), 1);

        assert!(disk_ctrl.detach(0x00).is_some());
        assert!(disk_ctrl.find_drive(0x00).is_none());
    }

    // -----------------------------------------------------------------------
    // Floppy drives: presence, drive type and change line
    // -----------------------------------------------------------------------

    /// Calls the disk trap the way the BIOS INT 13h handler does.
    fn int13(disk_ctrl: &mut DiskController, rt: &mut Runtime, ax: u16, cx: u16, dx: u16) {
        rt.cpu.registers.ax.set(ax);
        rt.cpu.registers.cx.set(cx);
        rt.cpu.registers.dx.set(dx);
        disk_ctrl.port_out_byte(DISK_TRAP_PORT, 0x00, &mut rt.cpu);
    }

    fn carry(rt: &Runtime) -> bool {
        rt.cpu.check_flag(CpuFlag::Carry)
    }

    fn floppy(size: u64) -> DiskImage {
        DiskImage::new_in_memory_sized(size, false)
    }

    /// Drives A: and B: exist whether or not a disk is in them. If an empty
    /// one were reported as "no such drive", DOS would fall back to a 360K
    /// drive without change line.
    #[test]
    fn empty_floppy_drive_reports_the_parameters_of_a_144m_drive() {
        let mut rt = Runtime::new_test();
        rt.cpu.memory.write_word(0x78, 0xEFC7); // INT 1Eh -> diskette parameter table
        rt.cpu.memory.write_word(0x7A, 0xF000);
        let mut disk_ctrl = DiskController::new();

        for drive in [0x00u16, 0x01] {
            rt.cpu.registers.bx.set(0xBBBB);
            int13(&mut disk_ctrl, &mut rt, 0x08EE, 0xCCCC, 0xDD00 | drive);
            assert!(!carry(&rt), "AH=08h must succeed for drive {:02X}", drive);
            assert_eq!(rt.cpu.registers.ax.word(), 0x0000);
            assert_eq!(rt.cpu.registers.bx.word(), 0x0004, "drive type 1.44M");
            assert_eq!(rt.cpu.registers.cx.word(), 0x4F12, "79 cylinders max, 18 sectors");
            assert_eq!(rt.cpu.registers.dx.word(), 0x0102, "head 1 max, two drives");
            assert_eq!(rt.cpu.registers.es.reg().word(), 0xF000);
            assert_eq!(rt.cpu.registers.di.word(), 0xEFC7);
        }
    }

    #[test]
    fn floppy_units_beyond_b_exist_only_once_configured() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();

        int13(&mut disk_ctrl, &mut rt, 0x0800, 0, 0x0002);
        assert!(carry(&rt), "unit 2 is not installed");
        assert_eq!(rt.cpu.registers.ax.high(), 0x07);
        int13(&mut disk_ctrl, &mut rt, 0x1500, 0, 0x0002);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x00, "no such drive");

        disk_ctrl.attach(0x02, floppy(737_280));
        int13(&mut disk_ctrl, &mut rt, 0x1500, 0, 0x0002);
        assert_eq!(rt.cpu.registers.ax.high(), 0x02);
        int13(&mut disk_ctrl, &mut rt, 0x0800, 0, 0x0000);
        assert_eq!(rt.cpu.registers.dx.low(), 3, "A:, B: and the configured unit 2");

        // The unit stays installed after its disk is ejected.
        disk_ctrl.detach(0x02);
        int13(&mut disk_ctrl, &mut rt, 0x1500, 0, 0x0002);
        assert_eq!(rt.cpu.registers.ax.high(), 0x02);
    }

    /// Every floppy drive has a change line, whatever is in it, small images
    /// and empty drives included. Without one DOS is left with its "nobody
    /// can change a disk in under two seconds" rule.
    #[test]
    fn floppy_drives_report_change_line_support() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();

        int13(&mut disk_ctrl, &mut rt, 0x15FF, 0, 0x0000);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x02, "empty drive A:");

        for size in [163_840u64, 368_640, 737_280, 1_228_800, FLOPPY_144_SIZE] {
            disk_ctrl.detach(0x00);
            disk_ctrl.attach(0x00, floppy(size));
            int13(&mut disk_ctrl, &mut rt, 0x15FF, 0, 0x0000);
            assert!(!carry(&rt));
            assert_eq!(rt.cpu.registers.ax.high(), 0x02, "{}-byte image", size);
        }
    }

    /// AH=16h is how the guest learns that a disk was swapped: the change
    /// is reported once, by the drive it happened in.
    #[test]
    fn change_line_reports_a_swap_exactly_once() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));

        // The disk that was put in before power-on counts as a change too.
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x06);
        assert_eq!(rt.cpu.memory.read_byte(0x0441), 0x06, "status byte at 40:41");

        for _ in 0..2 {
            int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
            assert!(!carry(&rt), "no change since the last query");
            assert_eq!(rt.cpu.registers.ax.high(), 0x00);
        }

        // What the monitor's `swap` does.
        disk_ctrl.detach(0x00);
        disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x06);
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);

        // The latch belongs to the drive: B: is unaffected.
        disk_ctrl.attach(0x01, floppy(FLOPPY_144_SIZE));
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0001);
        assert_eq!(rt.cpu.registers.ax.high(), 0x06);
    }

    #[test]
    fn change_line_of_an_empty_drive_is_not_ready_and_hard_disks_have_none() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(1));

        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x80, "no disk in A:");

        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0080);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x01, "invalid function for a hard disk");

        // Ejecting counts as a change of the (now empty) drive, not of the
        // next disk: inserting one raises the line again.
        disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        disk_ctrl.detach(0x00);
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert_eq!(rt.cpu.registers.ax.high(), 0x80);
        disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert_eq!(rt.cpu.registers.ax.high(), 0x06);
    }

    #[test]
    fn transfers_on_an_empty_drive_time_out() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        rt.cpu.registers.es.reg_mut().set(0x1000);
        rt.cpu.registers.bx.set(0);

        for function in [0x02u16, 0x03, 0x04, 0x05] {
            int13(&mut disk_ctrl, &mut rt, function << 8 | 0x01, 0x0001, 0x0000);
            assert!(carry(&rt));
            assert_eq!(rt.cpu.registers.ax.high(), 0x80, "AH={:02X}h", function);
        }
    }

    fn volume_serial(image: &mut DiskImage) -> u32 {
        let boot = image.read_sectors(0, 0, 1, 1).unwrap();
        u32::from_le_bytes([boot[0x27], boot[0x28], boot[0x29], boot[0x2A]])
    }

    /// DOS compares volume serial numbers to decide whether a disk was
    /// changed, so two floppies formatted here must not carry the same one.
    #[test]
    fn formatted_floppies_get_distinct_volume_serials() {
        let mut first = floppy(FLOPPY_144_SIZE);
        let mut second = floppy(FLOPPY_144_SIZE);
        first.format_fat().unwrap();
        second.format_fat().unwrap();
        assert_ne!(volume_serial(&mut first), volume_serial(&mut second));
    }

    /// A file written into the disk from the host changes what the guest
    /// has cached of it just like a swap does.
    #[test]
    fn host_side_write_raises_the_change_line() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        let mut image = floppy(FLOPPY_144_SIZE);
        image.format_fat().unwrap();
        disk_ctrl.attach(0x00, image);
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000); // consume the insertion

        disk_ctrl.put_file(0x00, b"hello", "HELLO.TXT").unwrap();
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x06);
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);

        assert!(disk_ctrl.put_file(0x01, b"hello", "HELLO.TXT").is_err(), "B: is empty");
    }

    // -----------------------------------------------------------------------
    // Geometry of image files
    // -----------------------------------------------------------------------

    /// The contents of a 1.44M floppy with an empty FAT volume.
    fn formatted_floppy_bytes() -> Vec<u8> {
        let mut image = floppy(FLOPPY_144_SIZE);
        image.format_fat().unwrap();
        (0..160u16)
            .flat_map(|track| image.read_sectors(track / 2, (track % 2) as u8, 1, 18).unwrap())
            .collect()
    }

    fn geometry_of(path: &Path) -> (u16, u8, u8) {
        let image = DiskImage::open(path, false).unwrap();
        (image.cylinders, image.heads, image.sectors_per_track)
    }

    /// DOS addresses a disk with the heads and sectors per track its boot
    /// sector states. An image whose size is not one of the floppy formats
    /// has to get that geometry: with any other, every sector DOS asks for
    /// comes from the wrong place.
    #[test]
    fn image_of_unlisted_size_takes_its_geometry_from_the_boot_sector() {
        let scratch = ScratchFile::new("geometry-bpb.img");

        // A 1.44M disk with one sector of padding behind it.
        let mut bytes = formatted_floppy_bytes();
        bytes.extend_from_slice(&[0u8; SECTOR_SIZE]);
        std::fs::write(scratch.path(), &bytes).unwrap();
        assert_eq!(geometry_of(scratch.path()), (80, 2, 18));

        // A 1.68M disk: 80 cylinders of 21 sectors per track.
        let mut bytes = formatted_floppy_bytes();
        bytes.resize(80 * 2 * 21 * SECTOR_SIZE, 0);
        bytes[0x13..0x15].copy_from_slice(&3360u16.to_le_bytes());
        bytes[0x18..0x1A].copy_from_slice(&21u16.to_le_bytes());
        std::fs::write(scratch.path(), &bytes).unwrap();
        assert_eq!(geometry_of(scratch.path()), (80, 2, 21));

        // A volume that ends inside a cylinder still gets that cylinder.
        bytes[0x13..0x15].copy_from_slice(&3340u16.to_le_bytes());
        std::fs::write(scratch.path(), &bytes).unwrap();
        assert_eq!(geometry_of(scratch.path()), (80, 2, 21));
    }

    /// The size table decides for the floppy formats, and an image without
    /// a credible boot sector keeps the geometry its size gives it.
    #[test]
    fn boot_sector_is_consulted_only_when_it_is_needed_and_credible() {
        let scratch = ScratchFile::new("geometry-fallback.img");

        // A floppy format, whatever the boot sector claims.
        let mut bytes = formatted_floppy_bytes();
        bytes[0x18..0x1A].copy_from_slice(&21u16.to_le_bytes());
        std::fs::write(scratch.path(), &bytes).unwrap();
        assert_eq!(geometry_of(scratch.path()), (80, 2, 18));

        // No boot sector at all.
        std::fs::write(scratch.path(), vec![0u8; FLOPPY_144_SIZE as usize + SECTOR_SIZE]).unwrap();
        assert_eq!(geometry_of(scratch.path()), (2, 16, 63));

        // Boot sectors whose parameter block cannot be right.
        for (offset, value) in [
            (0x0Busize, 1024u16), // bytes per sector
            (0x13, 0),            // no sectors
            (0x18, 0),            // sectors per track
            (0x18, 64),
            (0x1A, 0), // heads
            (0x1A, 256),
        ] {
            let mut bytes = formatted_floppy_bytes();
            bytes.extend_from_slice(&[0u8; SECTOR_SIZE]);
            bytes[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
            std::fs::write(scratch.path(), &bytes).unwrap();
            assert_eq!(
                geometry_of(scratch.path()),
                (2, 16, 63),
                "word at {:#x} = {}",
                offset,
                value
            );
        }
        let mut bytes = formatted_floppy_bytes();
        bytes.extend_from_slice(&[0u8; SECTOR_SIZE]);
        bytes[0] = 0x00; // no jump instruction: not a boot sector
        std::fs::write(scratch.path(), &bytes).unwrap();
        assert_eq!(geometry_of(scratch.path()), (2, 16, 63));
    }

    // -----------------------------------------------------------------------
    // Registers and status of the individual functions
    // -----------------------------------------------------------------------

    /// AH=08h returns AX = 0 and, for a floppy drive, the drive type in BX.
    /// BX describes floppy drives only: for a hard disk it keeps the
    /// caller's value.
    #[test]
    fn drive_parameters_set_the_documented_registers() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
        disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(32));

        rt.cpu.registers.bx.set(0xBBBB);
        int13(&mut disk_ctrl, &mut rt, 0x08EE, 0xCCCC, 0xDD00);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.word(), 0x0000, "AX");
        assert_eq!(rt.cpu.registers.bx.word(), 0x0004, "BH = 0, BL = drive type");
        assert_eq!(rt.cpu.registers.cx.word(), 0x4F12);
        assert_eq!(rt.cpu.registers.dx.word(), 0x0102);

        rt.cpu.registers.bx.set(0xBBBB);
        int13(&mut disk_ctrl, &mut rt, 0x08EE, 0xCCCC, 0xDD80);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.word(), 0x0000, "AX");
        assert_eq!(rt.cpu.registers.bx.word(), 0xBBBB, "BX is not used for a hard disk");
        assert_eq!(rt.cpu.registers.cx.word(), 0x403F, "cylinder 64 max, 63 sectors");
        assert_eq!(rt.cpu.registers.dx.word(), 0x0F01, "head 15 max, one hard disk");
    }

    /// The 40-track formats (160K, 180K, 320K and 360K) belong to a 360K
    /// drive, not to the 1.44M drive of an unknown size.
    #[test]
    fn forty_track_floppies_report_a_360k_drive() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        for (size, drive_type) in [
            (163_840u64, 0x01u8),
            (184_320, 0x01),
            (327_680, 0x01),
            (368_640, 0x01),
            (1_228_800, 0x02),
            (737_280, 0x03),
            (FLOPPY_144_SIZE, 0x04),
            (2_949_120, 0x06),
        ] {
            disk_ctrl.attach(0x00, floppy(size));
            int13(&mut disk_ctrl, &mut rt, 0x0800, 0, 0x0000);
            assert_eq!(rt.cpu.registers.bx.low(), drive_type, "{}-byte image", size);
        }
    }

    /// AH=01h returns the status of the last operation in AH and in AL.
    #[test]
    fn status_of_the_last_operation_is_returned_in_ah_and_al() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();

        int13(&mut disk_ctrl, &mut rt, 0x0201, 0x0001, 0x0000); // read from the empty A:
        int13(&mut disk_ctrl, &mut rt, 0x01EE, 0, 0x0000);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.word(), 0x8080);

        int13(&mut disk_ctrl, &mut rt, 0x0000, 0, 0x0000); // a reset clears it
        int13(&mut disk_ctrl, &mut rt, 0x01EE, 0, 0x0000);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.word(), 0x0000);
    }

    /// Every function records its result in the BIOS data area, unknown
    /// ones included: AH=01h must not find the status of an earlier
    /// operation there.
    #[test]
    fn every_function_records_its_status() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(1));

        // An unknown function, on the hard disk and on a floppy drive.
        int13(&mut disk_ctrl, &mut rt, 0x1800, 0, 0x0080);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x01);
        assert_eq!(rt.cpu.memory.read_byte(0x0474), 0x01, "hard disk status at 40:74");
        assert_eq!(rt.cpu.memory.read_byte(0x0441), 0x00, "diskette status at 40:41");
        int13(&mut disk_ctrl, &mut rt, 0x1800, 0, 0x0000);
        assert_eq!(rt.cpu.memory.read_byte(0x0441), 0x01);

        // A call that succeeds clears it again.
        for function in [0x0Cu16, 0x15] {
            rt.cpu.memory.write_byte(0x0474, 0x04);
            int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0080);
            assert!(!carry(&rt), "AH={:02X}h", function);
            assert_eq!(rt.cpu.memory.read_byte(0x0474), 0x00, "AH={:02X}h", function);
        }
    }

    /// Seek, recalibrate, reset and the drive tests have nothing to do on an
    /// image, but they must not succeed for a drive that is not there.
    #[test]
    fn housekeeping_functions_succeed_only_for_drives_that_exist() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(1));

        for function in [0x09u16, 0x0C, 0x0D, 0x10, 0x11, 0x14] {
            int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0080);
            assert!(!carry(&rt), "AH={:02X}h on the hard disk", function);
            assert_eq!(rt.cpu.registers.ax.high(), 0x00);

            int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0081);
            assert!(carry(&rt), "AH={:02X}h on a hard disk that does not exist", function);
            assert_eq!(rt.cpu.registers.ax.high(), 0x01);

            int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0000);
            assert!(!carry(&rt), "AH={:02X}h on drive A:", function);
            int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0003);
            assert!(carry(&rt), "AH={:02X}h on a floppy drive that does not exist", function);
        }
    }

    /// Read long (0Ah) and write long (0Bh) transfer the ECC bytes too,
    /// which an image does not have. They are invalid functions rather than
    /// a success without data.
    #[test]
    fn read_long_and_write_long_are_not_supported() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(1));

        for function in [0x0Au16, 0x0B] {
            int13(&mut disk_ctrl, &mut rt, function << 8 | 0x01, 0x0001, 0x0080);
            assert!(carry(&rt), "AH={:02X}h", function);
            assert_eq!(rt.cpu.registers.ax.high(), 0x01);
        }
    }

    // -----------------------------------------------------------------------
    // CHS addresses outside the geometry
    // -----------------------------------------------------------------------

    /// CX for a CHS address: CH = cylinder bits 0-7, CL = sector plus
    /// cylinder bits 8-9 in its top two bits.
    fn chs_cx(cylinder: u16, sector: u8) -> u16 {
        (cylinder & 0xFF) << 8 | (cylinder >> 8 & 0x03) << 6 | (sector & 0x3F) as u16
    }

    /// An in-memory 1.44M floppy in which every sector holds its own number.
    fn numbered_floppy() -> DiskImage {
        let mut image = floppy(FLOPPY_144_SIZE);
        let content = numbered_sectors();
        for (track, data) in content.chunks(18 * SECTOR_SIZE).enumerate() {
            image
                .write_sectors((track / 2) as u16, (track % 2) as u8, 1, 18, data)
                .unwrap();
        }
        image
    }

    /// A write past the last cylinder must fail on a file-backed image too.
    /// If it extended the host file, the file would lose the size of a
    /// floppy and be opened with a different geometry the next time.
    #[test]
    fn write_beyond_the_last_cylinder_fails_and_leaves_the_file_alone() {
        let scratch = ScratchFile::new("chs-write.img");
        std::fs::write(scratch.path(), numbered_sectors()).unwrap();
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, DiskImage::open(scratch.path(), false).unwrap());
        rt.cpu.registers.es.reg_mut().set(0x1000);
        rt.cpu.registers.bx.set(0);

        for (cylinder, head) in [(80u16, 0u16), (100, 0), (1023, 1)] {
            int13(&mut disk_ctrl, &mut rt, 0x0301, chs_cx(cylinder, 1), head << 8);
            assert!(carry(&rt), "write at C={} H={}", cylinder, head);
            assert_eq!(rt.cpu.registers.ax.high(), 0x04);
        }
        assert_eq!(std::fs::metadata(scratch.path()).unwrap().len(), FLOPPY_144_SIZE);
    }

    #[test]
    fn format_beyond_the_last_cylinder_fails_and_leaves_the_file_alone() {
        let scratch = ScratchFile::new("chs-format.img");
        std::fs::write(scratch.path(), vec![0u8; 368_640]).unwrap();
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, DiskImage::open(scratch.path(), false).unwrap());

        int13(&mut disk_ctrl, &mut rt, 0x0509, chs_cx(39, 1), 0x0100);
        assert!(!carry(&rt), "the last track of a 360K disk can be formatted");
        int13(&mut disk_ctrl, &mut rt, 0x0509, chs_cx(40, 1), 0x0000);
        assert!(carry(&rt), "cylinder 40 does not exist");
        assert_eq!(rt.cpu.registers.ax.high(), 0x04);
        int13(&mut disk_ctrl, &mut rt, 0x0509, chs_cx(0, 1), 0x0200);
        assert!(carry(&rt), "head 2 does not exist");

        assert_eq!(std::fs::metadata(scratch.path()).unwrap().len(), 368_640);
    }

    /// A head number beyond the last head is an error. Folded into the
    /// address, head 2 of cylinder 0 would read (and write) cylinder 1.
    #[test]
    fn head_beyond_the_geometry_is_rejected() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, numbered_floppy());
        rt.cpu.registers.es.reg_mut().set(0x1000);
        rt.cpu.registers.bx.set(0);
        rt.cpu.memory.write_byte(0x10000, 0xEE);

        int13(&mut disk_ctrl, &mut rt, 0x0201, chs_cx(0, 1), 0x0200);
        assert!(carry(&rt), "read from head 2 of a two-sided disk");
        assert_eq!(rt.cpu.registers.ax.high(), 0x04);
        assert_eq!(rt.cpu.memory.read_byte(0x10000), 0xEE, "nothing was transferred");

        int13(&mut disk_ctrl, &mut rt, 0x0301, chs_cx(0, 1), 0x0500);
        assert!(carry(&rt), "write to head 5");
        assert_eq!(rt.cpu.registers.ax.high(), 0x04);
        let image = disk_ctrl.find_drive(0x00).unwrap();
        assert_eq!(image.read_sectors(2, 1, 1, 1).unwrap()[0], 90, "sector 90 is untouched");
    }

    /// DH=FFh with a transfer that continues on the next track overflowed
    /// the 8-bit head counter.
    #[test]
    fn head_ff_with_a_transfer_across_tracks_is_rejected() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(32));
        rt.cpu.registers.es.reg_mut().set(0x1000);
        rt.cpu.registers.bx.set(0);

        int13(&mut disk_ctrl, &mut rt, 0x0202, chs_cx(0, 63), 0xFF80);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x04);
    }

    /// The image itself refuses addresses outside its geometry, whether it
    /// lives in memory or in a file.
    #[test]
    fn images_are_bounded_by_their_geometry() {
        let scratch = ScratchFile::new("chs-bounds.img");
        std::fs::write(scratch.path(), numbered_sectors()).unwrap();
        let sector = [0x5Au8; SECTOR_SIZE];

        for mut image in [DiskImage::open(scratch.path(), false).unwrap(), numbered_floppy()] {
            assert!(image.read_sectors(79, 1, 18, 1).is_ok(), "the last sector");
            assert!(image.read_sectors(79, 1, 18, 2).is_err(), "one past the last sector");
            assert!(image.read_sectors(80, 0, 1, 1).is_err(), "cylinder 80");
            assert!(image.read_sectors(0, 2, 1, 1).is_err(), "head 2");
            assert!(image.read_sectors(0, 0, 19, 1).is_err(), "sector 19");
            assert!(image.read_sectors(0, 0, 0, 1).is_err(), "sector 0");
            assert!(image.write_sectors(80, 0, 1, 1, &sector).is_err());
            assert!(image.write_sectors(0, 2, 1, 1, &sector).is_err());
            assert!(image.format_track(80, 0).is_err());
            assert!(image.format_track(0, 2).is_err());
        }
        assert_eq!(std::fs::metadata(scratch.path()).unwrap().len(), FLOPPY_144_SIZE);
    }

    /// A transfer that starts on the disk and runs off its end stops there
    /// and reports how many sectors it moved.
    #[test]
    fn transfer_running_off_the_end_reports_the_sectors_done() {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, numbered_floppy());
        rt.cpu.registers.es.reg_mut().set(0x1000);
        rt.cpu.registers.bx.set(0);

        // 20 sectors from C=79 H=1 S=10: only 9 are left on the disk.
        int13(&mut disk_ctrl, &mut rt, 0x0214, chs_cx(79, 10), 0x0100);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.word(), 0x0409, "sector not found after 9 sectors");
        assert_eq!(rt.cpu.memory.read_byte(0x10000), (2871 % 256) as u8);
        assert_eq!(rt.cpu.memory.read_byte(0x10000 + 8 * SECTOR_SIZE), (2879 % 256) as u8);
    }

    // -----------------------------------------------------------------------
    // INT 13h extensions (AH=41h-48h)
    // -----------------------------------------------------------------------

    /// Linear address of the disk address packet and of the transfer buffer
    /// (1000:0000) the tests use.
    const PACKET: usize = 0x0600;
    const BUFFER: usize = 0x10000;

    /// Number of sectors on the disk `numbered_hard_disk` returns.
    const HARD_DISK_SECTORS: u64 = 2 * 16 * 63;

    /// A hard disk on which every sector starts with its own number.
    fn numbered_hard_disk() -> DiskImage {
        let mut image = DiskImage::new_in_memory_hard_disk(1);
        assert_eq!((image.cylinders, image.heads, image.sectors_per_track), (2, 16, 63));
        for lba in 0..HARD_DISK_SECTORS {
            let mut sector = [0u8; SECTOR_SIZE];
            sector[..8].copy_from_slice(&lba.to_le_bytes());
            let (c, h, s) = ((lba / 1008) as u16, (lba / 63 % 16) as u8, (lba % 63) as u8 + 1);
            image.write_sectors(c, h, s, 1, &sector).unwrap();
        }
        image
    }

    fn machine_with_numbered_hard_disk() -> (Runtime, DiskController) {
        let mut rt = Runtime::new_test();
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x80, numbered_hard_disk());
        // Something recognisable where the transfers go.
        for i in 0..16 * SECTOR_SIZE {
            rt.cpu.memory.write_byte(BUFFER + i, 0xEE);
        }
        (rt, disk_ctrl)
    }

    /// Calls an extended function with a disk address packet for `count`
    /// sectors at `lba`, transfer buffer 1000:0000.
    fn int13_packet(
        disk_ctrl: &mut DiskController,
        rt: &mut Runtime,
        function: u8,
        drive: u8,
        lba: u64,
        count: u16,
    ) {
        let mut packet = vec![0x10, 0x00];
        packet.extend_from_slice(&count.to_le_bytes());
        packet.extend_from_slice(&[0x00, 0x00, 0x00, 0x10]);
        packet.extend_from_slice(&lba.to_le_bytes());
        for (i, byte) in packet.iter().enumerate() {
            rt.cpu.memory.write_byte(PACKET + i, *byte);
        }
        rt.cpu.registers.ds.reg_mut().set(0x0000);
        rt.cpu.registers.si.set(PACKET as u16);
        int13(disk_ctrl, rt, (function as u16) << 8, 0, drive as u16);
    }

    /// The sector number at the start of the `index`th sector in the buffer.
    fn sector_in_buffer(rt: &Runtime, index: usize) -> u64 {
        let at = BUFFER + index * SECTOR_SIZE;
        (0..8).fold(0, |number, i| number | (rt.cpu.memory.read_byte(at + i) as u64) << (8 * i))
    }

    #[test]
    fn lba_transfers_reach_the_addressed_sectors() {
        let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

        int13_packet(&mut disk_ctrl, &mut rt, 0x42, 0x80, 1000, 3);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);
        assert_eq!(
            [sector_in_buffer(&rt, 0), sector_in_buffer(&rt, 1), sector_in_buffer(&rt, 2)],
            [1000, 1001, 1002]
        );
        assert_eq!(rt.cpu.memory.read_byte(BUFFER + 3 * SECTOR_SIZE), 0xEE);

        // Written back 500 sectors further on.
        int13_packet(&mut disk_ctrl, &mut rt, 0x43, 0x80, 1500, 2);
        assert!(!carry(&rt));
        let image = disk_ctrl.find_drive(0x80).unwrap();
        assert_eq!(image.read_sectors(1, 7, 52, 1).unwrap()[..2], [0xE8, 0x03], "sector 1500");
        assert_eq!(image.read_sectors(1, 7, 53, 1).unwrap()[..2], [0xE9, 0x03], "sector 1501");
        assert_eq!(image.read_sectors(1, 7, 54, 1).unwrap()[..2], [0xDE, 0x05], "sector 1502");

        // The last sector of the disk.
        int13_packet(&mut disk_ctrl, &mut rt, 0x42, 0x80, HARD_DISK_SECTORS - 1, 1);
        assert!(!carry(&rt));
        assert_eq!(sector_in_buffer(&rt, 0), HARD_DISK_SECTORS - 1);
    }

    /// The installation check answers "supported" only to a caller that
    /// passes 55AAh in BX, and only for a hard disk that exists.
    #[test]
    fn extensions_check_requires_the_signature_in_bx() {
        let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

        rt.cpu.registers.bx.set(0x1234);
        int13(&mut disk_ctrl, &mut rt, 0x4100, 0, 0x0080);
        assert!(carry(&rt), "BX is not 55AAh");
        assert_eq!(rt.cpu.registers.ax.high(), 0x01);
        assert_eq!(rt.cpu.registers.bx.word(), 0x1234);

        rt.cpu.registers.bx.set(0x55AA);
        int13(&mut disk_ctrl, &mut rt, 0x4100, 0, 0x0080);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.bx.word(), 0xAA55);
        assert_eq!(rt.cpu.registers.ax.high(), 0x01, "version 1.x");
        assert_eq!(rt.cpu.registers.cx.word(), 0x0001, "extended disk access functions");

        for drive in [0x0081u16, 0x0000] {
            rt.cpu.registers.bx.set(0x55AA);
            int13(&mut disk_ctrl, &mut rt, 0x4100, 0, drive);
            assert!(carry(&rt), "drive {:02X}h has no extensions", drive);
            assert_eq!(rt.cpu.registers.bx.word(), 0x55AA);
        }
    }

    /// Block numbers have 64 bits. A block far beyond the disk is an error,
    /// not the sector near the start of the disk that its number turns into
    /// when it is cut to 32 bits or its cylinder to 16.
    #[test]
    fn lba_read_beyond_the_disk_is_rejected() {
        let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

        for lba in [
            HARD_DISK_SECTORS,
            65_536 * 1008 + 5, // cylinder 65536, which is cylinder 0 in 16 bits
            0x1_0000_0007,     // sector 7 in 32 bits
            u64::MAX,
        ] {
            int13_packet(&mut disk_ctrl, &mut rt, 0x42, 0x80, lba, 2);
            assert!(carry(&rt), "block {:#x}", lba);
            assert_eq!(rt.cpu.registers.ax.high(), 0x04, "block {:#x}", lba);
            assert_eq!(rt.cpu.memory.read_byte(BUFFER), 0xEE, "block {:#x}: nothing is read", lba);
            assert_eq!(rt.cpu.memory.read_word(PACKET + 2), 0, "blocks transferred");
        }
        assert_eq!(rt.cpu.memory.read_byte(0x0474), 0x04, "hard disk status at 40:74");
    }

    /// A transfer that starts on the disk and runs off its end stops there
    /// and reports in the packet how many blocks it moved.
    #[test]
    fn lba_read_running_off_the_end_reports_the_blocks_done() {
        let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

        int13_packet(&mut disk_ctrl, &mut rt, 0x42, 0x80, HARD_DISK_SECTORS - 6, 10);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x04);
        assert_eq!(rt.cpu.memory.read_word(PACKET + 2), 6);
        assert_eq!(sector_in_buffer(&rt, 5), HARD_DISK_SECTORS - 1);
        assert_eq!(rt.cpu.memory.read_byte(BUFFER + 6 * SECTOR_SIZE), 0xEE);
    }

    /// A write to a block far beyond the disk must not land on the sector
    /// its number turns into when it is cut to 32 or 16 bits: sector 0.
    #[test]
    fn lba_write_beyond_the_disk_is_rejected() {
        let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

        for lba in [65_536 * 1008, 0x1_0000_0000, HARD_DISK_SECTORS] {
            int13_packet(&mut disk_ctrl, &mut rt, 0x43, 0x80, lba, 1);
            assert!(carry(&rt), "block {:#x}", lba);
            assert_eq!(rt.cpu.registers.ax.high(), 0x04, "block {:#x}", lba);
        }
        let image = disk_ctrl.find_drive(0x80).unwrap();
        assert_eq!(image.read_sectors(0, 0, 1, 1).unwrap()[..2], [0x00, 0x00], "sector 0");

        disk_ctrl.find_drive(0x80).unwrap().readonly = true;
        int13_packet(&mut disk_ctrl, &mut rt, 0x43, 0x80, 0, 1);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x03, "write-protected");
    }

    /// Extended verify succeeded for drives that do not exist and for any
    /// range of blocks.
    #[test]
    fn extended_verify_checks_the_drive_and_the_range() {
        let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

        int13_packet(&mut disk_ctrl, &mut rt, 0x44, 0x83, 0, 1);
        assert!(carry(&rt), "drive 83h does not exist");
        assert_eq!(rt.cpu.registers.ax.high(), 0x80);

        int13_packet(&mut disk_ctrl, &mut rt, 0x44, 0x80, 0, HARD_DISK_SECTORS as u16);
        assert!(!carry(&rt), "the whole disk");
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);

        int13_packet(&mut disk_ctrl, &mut rt, 0x44, 0x80, 1, HARD_DISK_SECTORS as u16);
        assert!(carry(&rt), "one block past the end");
        assert_eq!(rt.cpu.registers.ax.high(), 0x04);
        assert_eq!(rt.cpu.memory.read_word(PACKET + 2), HARD_DISK_SECTORS as u16 - 1);

        int13_packet(&mut disk_ctrl, &mut rt, 0x44, 0x80, 0x7FFF_FFFF, 1);
        assert!(carry(&rt));

        // A packet that is too short to hold a block number.
        rt.cpu.memory.write_byte(PACKET, 0x0C);
        int13(&mut disk_ctrl, &mut rt, 0x4400, 0, 0x0080);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x01);
    }

    /// The installation check promises the extended seek and the extended
    /// drive parameters along with the transfers.
    #[test]
    fn extended_seek_checks_the_block_number() {
        let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

        int13_packet(&mut disk_ctrl, &mut rt, 0x47, 0x80, HARD_DISK_SECTORS - 1, 0);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);

        int13_packet(&mut disk_ctrl, &mut rt, 0x47, 0x80, HARD_DISK_SECTORS, 0);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x04);

        int13_packet(&mut disk_ctrl, &mut rt, 0x47, 0x81, 0, 0);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x80);
    }

    #[test]
    fn extended_drive_parameters_describe_the_disk() {
        let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();
        let word = |rt: &Runtime, offset: usize| rt.cpu.memory.read_word(PACKET + offset);
        rt.cpu.registers.ds.reg_mut().set(0x0000);
        rt.cpu.registers.si.set(PACKET as u16);

        // The caller says how large its buffer is; 30 bytes is the size
        // version 2 of the interface uses.
        for i in 0..0x20 {
            rt.cpu.memory.write_byte(PACKET + i, 0xEE);
        }
        rt.cpu.memory.write_word(PACKET, 0x001E);
        int13(&mut disk_ctrl, &mut rt, 0x4800, 0, 0x0080);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);
        assert_eq!(word(&rt, 0x00), 0x001A, "size of the data returned");
        assert_eq!(word(&rt, 0x02), 0x0002, "flags: the CHS values are valid");
        assert_eq!([word(&rt, 0x04), word(&rt, 0x06)], [2, 0], "cylinders");
        assert_eq!([word(&rt, 0x08), word(&rt, 0x0A)], [16, 0], "heads");
        assert_eq!([word(&rt, 0x0C), word(&rt, 0x0E)], [63, 0], "sectors per track");
        assert_eq!(
            [word(&rt, 0x10), word(&rt, 0x12), word(&rt, 0x14), word(&rt, 0x16)],
            [HARD_DISK_SECTORS as u16, 0, 0, 0],
            "total sectors"
        );
        assert_eq!(word(&rt, 0x18), 512, "bytes per sector");
        assert_eq!(rt.cpu.memory.read_byte(PACKET + 0x1A), 0xEE, "nothing beyond 26 bytes");

        // A buffer that cannot hold the 26 bytes.
        rt.cpu.memory.write_word(PACKET, 0x0010);
        int13(&mut disk_ctrl, &mut rt, 0x4800, 0, 0x0080);
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x01);

        rt.cpu.memory.write_word(PACKET, 0x001A);
        int13(&mut disk_ctrl, &mut rt, 0x4800, 0, 0x0081);
        assert!(carry(&rt), "drive 81h does not exist");
        assert_eq!(rt.cpu.registers.ax.high(), 0x80);

        // A floppy disk is a removable medium in a drive with a change line.
        disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
        rt.cpu.memory.write_word(PACKET, 0x001A);
        int13(&mut disk_ctrl, &mut rt, 0x4800, 0, 0x0000);
        assert!(!carry(&rt));
        assert_eq!(word(&rt, 0x02), 0x0016, "flags");
        assert_eq!([word(&rt, 0x04), word(&rt, 0x08), word(&rt, 0x0C)], [80, 2, 18]);
        assert_eq!(word(&rt, 0x10), 2880, "total sectors");
    }

    // -----------------------------------------------------------------------
    // Copying host files into the FAT volume (the monitor's `put`)
    // -----------------------------------------------------------------------

    fn formatted_floppy() -> DiskImage {
        let mut image = floppy(FLOPPY_144_SIZE);
        image.format_fat().unwrap();
        image
    }

    /// Reads a file back from the FAT volume of an unpartitioned in-memory
    /// image; `path` uses '/' between directories.
    fn fat_file(image: &mut DiskImage, path: &str) -> Option<Vec<u8>> {
        let DiskStorage::InMemory(data) = &mut image.storage else {
            panic!("in-memory image expected");
        };
        let volume = std::io::Cursor::new(data.as_mut_slice());
        let fs = fatfs::FileSystem::new(volume, fatfs::FsOptions::new()).unwrap();
        let mut file = fs.root_dir().open_file(path).ok()?;
        let mut content = Vec::new();
        file.read_to_end(&mut content).unwrap();
        Some(content)
    }

    /// The root directory of an unpartitioned FAT12 floppy as it is on the
    /// disk: the number of long-name entries and the 8.3 names.
    fn root_directory(image: &mut DiskImage) -> (usize, Vec<String>) {
        let boot = image.read_sectors(0, 0, 1, 1).unwrap();
        let reserved = u16::from_le_bytes([boot[14], boot[15]]) as usize;
        let fats = boot[16] as usize;
        let root_entries = u16::from_le_bytes([boot[17], boot[18]]) as usize;
        let sectors_per_fat = u16::from_le_bytes([boot[22], boot[23]]) as usize;
        let first = reserved + fats * sectors_per_fat;

        let mut long_names = 0;
        let mut names = Vec::new();
        for sector in first..first + root_entries * 32 / SECTOR_SIZE {
            let (track, index) = (sector / 18, sector % 18);
            let data = image
                .read_sectors((track / 2) as u16, (track % 2) as u8, index as u8 + 1, 1)
                .unwrap();
            for entry in data.chunks(32) {
                match (entry[0], entry[11]) {
                    (0x00, _) => return (long_names, names),
                    (0xE5, _) => {}
                    (_, 0x0F) => long_names += 1,
                    _ => {
                        let base = String::from_utf8_lossy(&entry[..8]).trim_end().to_string();
                        let ext = String::from_utf8_lossy(&entry[8..11]).trim_end().to_string();
                        names.push(if ext.is_empty() { base } else { format!("{}.{}", base, ext) });
                    }
                }
            }
        }
        (long_names, names)
    }

    /// Putting a file over an existing longer one replaces it as a whole:
    /// nothing of the old tail stays, and the file has the new length.
    #[test]
    fn put_over_an_existing_file_replaces_it() {
        let mut image = formatted_floppy();
        let old: Vec<u8> = b"ECHO OLD LINE\r\n".iter().cycle().take(600).copied().collect();
        let new = b"@ECHO NEW\r\n".to_vec();

        image.copy_file_to_fat(&old, "AUTOEXEC.BAT").unwrap();
        image.copy_file_to_fat(&new, "AUTOEXEC.BAT").unwrap();
        assert_eq!(fat_file(&mut image, "AUTOEXEC.BAT"), Some(new));

        // Growing again works too, across several clusters.
        let big: Vec<u8> = (0..5000u32).map(|i| i as u8).collect();
        image.copy_file_to_fat(&big, "autoexec.bat").unwrap();
        assert_eq!(fat_file(&mut image, "AUTOEXEC.BAT"), Some(big));
        assert_eq!(root_directory(&mut image).1, ["AUTOEXEC.BAT"]);
    }

    /// MS-DOS 6 shows a VFAT long-name entry as the volume label when the
    /// disk has none, and every file costs a second directory slot.
    #[test]
    fn put_writes_no_long_name_entries() {
        let mut image = formatted_floppy();
        image.copy_file_to_fat(b"hello", "GAME.EXE").unwrap();
        image.copy_file_to_fat(b"hello", "readme.txt").unwrap();
        image.copy_file_to_fat(b"hello", "My Document.final.txt").unwrap();

        let (long_names, names) = root_directory(&mut image);
        assert_eq!(long_names, 0);
        assert_eq!(names, ["GAME.EXE", "README.TXT", "MYDOCU~1.TXT"]);
    }

    #[test]
    fn put_respects_the_write_protection() {
        let mut image = DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false);
        image.format_fat().unwrap();
        image.readonly = true;

        let error = image.copy_file_to_fat(b"hello", "HELLO.TXT").unwrap_err();
        assert_eq!(error.kind(), std::io::ErrorKind::PermissionDenied);
        assert_eq!(root_directory(&mut image).1, Vec::<String>::new());
    }

    /// DOS resolves these names to devices in every directory, so a file
    /// called CON or NUL.TXT could never be opened or deleted.
    #[test]
    fn put_refuses_dos_device_names() {
        let mut image = formatted_floppy();
        for name in ["CON", "nul", "NUL.TXT", "prn.log", "AUX", "COM1", "lpt3.dat", "CLOCK$"] {
            assert!(image.copy_file_to_fat(b"x", name).is_err(), "{} is a device", name);
        }
        for name in ["CONSOLE.TXT", "COM5", "NULL", "LPT.TXT"] {
            assert!(image.copy_file_to_fat(b"x", name).is_ok(), "{} is a plain file name", name);
        }
        assert_eq!(root_directory(&mut image).1, ["CONSOLE.TXT", "COM5", "NULL", "LPT.TXT"]);
    }

    #[test]
    fn put_reports_the_name_the_file_has_in_dos() {
        let mut image = formatted_floppy();

        assert_eq!(image.copy_file_to_fat(b"1", "game.exe").unwrap(), "GAME.EXE");
        assert_eq!(image.copy_file_to_fat(b"2", "\\GAME.EXE").unwrap(), "GAME.EXE");
        assert_eq!(
            image.copy_file_to_fat(b"3", "My Document.final.txt").unwrap(),
            "MYDOCU~1.TXT"
        );
        // A long name cannot be matched against the entry it was stored
        // under, so putting it again makes a second file rather than
        // overwriting one that merely shares the generated name.
        assert_eq!(
            image.copy_file_to_fat(b"4", "My Document.final.txt").unwrap(),
            "MYDOCU~2.TXT"
        );
        assert_eq!(fat_file(&mut image, "GAME.EXE"), Some(b"2".to_vec()));
        assert_eq!(fat_file(&mut image, "MYDOCU~1.TXT"), Some(b"3".to_vec()));

        let error = image.copy_file_to_fat(b"5", "NODIR\\FILE.TXT").unwrap_err();
        assert_eq!(error.kind(), std::io::ErrorKind::NotFound);
        assert!(image.copy_file_to_fat(b"6", "what?.txt").is_err());
        assert!(image.copy_file_to_fat(b"7", "\\").is_err());
    }

    /// A directory tree on the host, removed again on drop.
    struct ScratchDir(PathBuf);

    impl ScratchDir {
        fn new(name: &str) -> Self {
            let path = std::env::temp_dir()
                .join(format!("emu8086-test-{}-{}", std::process::id(), name));
            let _ = std::fs::remove_dir_all(&path);
            std::fs::create_dir_all(&path).unwrap();
            Self(path)
        }

        fn write(&self, relative: &str, content: &[u8]) {
            let path = self.0.join(relative);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, content).unwrap();
        }
    }

    impl Drop for ScratchDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn put_of_a_directory_copies_the_tree_and_replaces_existing_files() {
        let host = ScratchDir::new("put-dir");
        host.write("readme.txt", &[b'r'; 3000]);
        host.write("data/level1.dat", b"level one");
        host.write("data/deep/level2.dat", b"level two");

        let mut image = formatted_floppy();
        assert_eq!(image.copy_dir_to_fat(&host.0).unwrap(), 3);
        assert_eq!(fat_file(&mut image, "README.TXT"), Some(vec![b'r'; 3000]));
        assert_eq!(fat_file(&mut image, "DATA/LEVEL1.DAT"), Some(b"level one".to_vec()));
        assert_eq!(
            fat_file(&mut image, "DATA/DEEP/LEVEL2.DAT"),
            Some(b"level two".to_vec())
        );

        // A second copy goes into the same directories and files.
        host.write("readme.txt", b"short");
        assert_eq!(image.copy_dir_to_fat(&host.0).unwrap(), 3);
        assert_eq!(fat_file(&mut image, "README.TXT"), Some(b"short".to_vec()));
        let (long_names, mut names) = root_directory(&mut image);
        names.sort();
        assert_eq!((long_names, names), (0, vec!["DATA".to_string(), "README.TXT".to_string()]));

        // A file can also be put into one of the copied directories.
        assert_eq!(
            image.copy_file_to_fat(b"new", "data\\deep\\level3.dat").unwrap(),
            "DATA\\DEEP\\LEVEL3.DAT"
        );
        assert_eq!(fat_file(&mut image, "DATA/DEEP/LEVEL3.DAT"), Some(b"new".to_vec()));
    }

    // -----------------------------------------------------------------------
    // The FAT volume of a partitioned disk
    // -----------------------------------------------------------------------

    /// The bytes 0, 1, ..., 99 with a partition of 30 bytes that starts at
    /// byte 10.
    fn partition_slice() -> PartitionSlice<std::io::Cursor<Vec<u8>>> {
        PartitionSlice::new(std::io::Cursor::new((0..100).collect()), 10, 30).unwrap()
    }

    #[test]
    fn partition_slice_positions_are_relative_to_the_partition() {
        let mut slice = partition_slice();
        let mut two = [0u8; 2];

        assert_eq!(slice.seek(SeekFrom::Current(0)).unwrap(), 0);
        assert_eq!(slice.seek(SeekFrom::Start(5)).unwrap(), 5);
        slice.read_exact(&mut two).unwrap();
        assert_eq!(two, [15, 16]);
        assert_eq!(slice.seek(SeekFrom::Current(0)).unwrap(), 7);
        assert_eq!(slice.seek(SeekFrom::Current(-4)).unwrap(), 3);
        assert_eq!(slice.seek(SeekFrom::End(0)).unwrap(), 30, "the end of the partition");
        assert_eq!(slice.seek(SeekFrom::End(-2)).unwrap(), 28);
        slice.read_exact(&mut two).unwrap();
        assert_eq!(two, [38, 39]);
    }

    /// A seek cannot end up in front of the partition or behind it, and
    /// reads and writes stop at its end.
    #[test]
    fn partition_slice_keeps_inside_the_partition() {
        let mut slice = partition_slice();
        let mut buffer = [0u8; 8];

        assert!(slice.seek(SeekFrom::Current(-4)).is_err(), "in front of the partition");
        assert!(slice.seek(SeekFrom::Start(31)).is_err(), "behind the partition");
        assert!(slice.seek(SeekFrom::End(1)).is_err());
        assert_eq!(slice.seek(SeekFrom::Current(0)).unwrap(), 0, "a failed seek does not move");
        slice.read_exact(&mut buffer[..1]).unwrap();
        assert_eq!(buffer[0], 10);

        slice.seek(SeekFrom::Start(28)).unwrap();
        assert_eq!(slice.read(&mut buffer).unwrap(), 2, "a read stops at the end");
        assert_eq!(buffer[..2], [38, 39]);
        assert_eq!(slice.read(&mut buffer).unwrap(), 0);

        slice.seek(SeekFrom::Start(28)).unwrap();
        assert_eq!(slice.write(&[0xFF; 8]).unwrap(), 2, "and so does a write");
        assert!(slice.write_all(&[0xFF; 8]).is_err());
        assert_eq!(slice.inner.get_ref()[36..42], [36, 37, 0xFF, 0xFF, 40, 41]);
    }

    /// A master boot record with the given partitions: type, first sector
    /// and number of sectors of each.
    fn master_boot_record(partitions: &[(u8, u32, u32)]) -> [u8; SECTOR_SIZE] {
        let mut sector = [0u8; SECTOR_SIZE];
        sector[0] = 0xFA; // boot code, not the jump of a FAT boot sector
        for (index, (kind, first, count)) in partitions.iter().enumerate() {
            let entry = 0x1BE + 16 * index;
            sector[entry + 4] = *kind;
            sector[entry + 8..entry + 12].copy_from_slice(&first.to_le_bytes());
            sector[entry + 12..entry + 16].copy_from_slice(&count.to_le_bytes());
        }
        sector[510] = 0x55;
        sector[511] = 0xAA;
        sector
    }

    /// First sector of the FAT volume on the disks below.
    const VOLUME_START: u64 = 63;

    /// A hard disk of 8064 sectors with `mbr` in its first sector and an
    /// empty FAT volume of 2880 sectors from sector 63 on.
    fn hard_disk_with_a_fat_volume(mbr: &[u8; SECTOR_SIZE]) -> DiskImage {
        let mut image = DiskImage::new_in_memory_hard_disk(4);
        for (index, sector) in formatted_floppy_bytes().chunks(SECTOR_SIZE).enumerate() {
            write_block(&mut image, VOLUME_START + index as u64, sector);
        }
        image.write_sectors(0, 0, 1, 1, mbr).unwrap();
        image
    }

    fn write_block(image: &mut DiskImage, lba: u64, data: &[u8]) {
        let (c, h, s) = ((lba / 1008) as u16, (lba / 63 % 16) as u8, (lba % 63) as u8 + 1);
        image.write_sectors(c, h, s, 1, data).unwrap();
    }

    fn read_block(image: &mut DiskImage, lba: u64) -> Vec<u8> {
        let (c, h, s) = ((lba / 1008) as u16, (lba / 63 % 16) as u8, (lba % 63) as u8 + 1);
        image.read_sectors(c, h, s, 1).unwrap()
    }

    /// The first sector of the root directory of the volume at sector 63.
    fn root_directory_of_the_volume(image: &mut DiskImage) -> Vec<u8> {
        let boot = read_block(image, VOLUME_START);
        let reserved = u16::from_le_bytes([boot[14], boot[15]]) as u64;
        let sectors_per_fat = u16::from_le_bytes([boot[22], boot[23]]) as u64;
        read_block(image, VOLUME_START + reserved + boot[16] as u64 * sectors_per_fat)
    }

    /// The FAT partition may be described by any of the four entries of
    /// the partition table, behind empty entries and partitions of other
    /// types.
    #[test]
    fn put_finds_the_fat_partition_in_any_entry() {
        for partitions in [
            vec![(0x06u8, 63u32, 2880u32)],
            vec![(0x00, 0, 0), (0x06, 63, 2880)],
            vec![(0x05, 4000, 2000), (0x01, 63, 2880)],
            vec![(0x83, 4000, 2000), (0x00, 0, 0), (0x00, 0, 0), (0x04, 63, 2880)],
        ] {
            let mut image = hard_disk_with_a_fat_volume(&master_boot_record(&partitions));
            let stored = image.copy_file_to_fat(b"hello", "HELLO.TXT");
            assert_eq!(stored.ok().as_deref(), Some("HELLO.TXT"), "{:?}", partitions);
            let root = root_directory_of_the_volume(&mut image);
            assert_eq!(&root[..11], b"HELLO   TXT", "{:?}", partitions);
        }
    }

    #[test]
    fn put_reports_a_disk_without_a_fat_partition() {
        for partitions in [
            vec![],
            vec![(0x83u8, 63u32, 2880u32)],
            vec![(0x06, 63, 0)],
            vec![(0x06, 9000, 100)], // behind the end of the disk
        ] {
            let mut image = hard_disk_with_a_fat_volume(&master_boot_record(&partitions));
            let error = image.copy_file_to_fat(b"hello", "HELLO.TXT").unwrap_err();
            assert!(
                error.to_string().contains("no FAT partition"),
                "{:?}: {}",
                partitions,
                error
            );
        }
    }
}
