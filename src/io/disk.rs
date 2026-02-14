use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

const SECTOR_SIZE: usize = 512;
pub const FLOPPY_144_SIZE: u64 = 1_474_560; // 80 * 2 * 18 * 512

/// Where the disk data lives.
enum DiskStorage {
    FileBacked(File),
    InMemory(Vec<u8>),
}

/// What the user asked for on the CLI.
pub enum DiskSource {
    FilePath(PathBuf),
    Memory(u64), // size in bytes
}

/// Parsed `--disk` argument: drive letter + source.
pub struct DiskSpec {
    pub drive: u8,   // 0 = A:, 1 = B:
    pub source: DiskSource,
}

pub struct DiskImage {
    storage: DiskStorage,
    pub cylinders: u16,
    pub heads: u8,
    pub sectors_per_track: u8,
}

impl DiskImage {
    /// Open an existing disk image file (read-write).
    pub fn open(path: &Path) -> std::io::Result<Self> {
        let file = OpenOptions::new().read(true).write(true).open(path)?;
        let size = file.metadata()?.len();
        let (cylinders, heads, spt) = geometry_from_size(size);
        Ok(Self {
            storage: DiskStorage::FileBacked(file),
            cylinders,
            heads,
            sectors_per_track: spt,
        })
    }

    /// Open an existing file or create a blank 1.44MB floppy image.
    pub fn open_or_create(path: &Path) -> std::io::Result<Self> {
        if path.exists() {
            Self::open(path)
        } else {
            let mut file = OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)?;
            file.set_len(FLOPPY_144_SIZE)?;
            file.flush()?;
            Ok(Self {
                storage: DiskStorage::FileBacked(file),
                cylinders: 80,
                heads: 2,
                sectors_per_track: 18,
            })
        }
    }

    /// Create a blank 1.44MB in-memory floppy.
    pub fn new_in_memory() -> Self {
        Self::new_in_memory_sized(FLOPPY_144_SIZE)
    }

    /// Create a blank in-memory floppy of the given size in bytes.
    /// Uses standard floppy geometry for known sizes (720K, 1.2M, 1.44M, 2.88M).
    pub fn new_in_memory_sized(size: u64) -> Self {
        let (cylinders, heads, spt) = geometry_from_size(size);
        let total_bytes = cylinders as u64 * heads as u64 * spt as u64 * SECTOR_SIZE as u64;
        Self {
            storage: DiskStorage::InMemory(vec![0u8; total_bytes as usize]),
            cylinders,
            heads,
            sectors_per_track: spt,
        }
    }

    /// Open an existing hard disk image file (read-write).
    pub fn open_hard_disk(path: &Path) -> std::io::Result<Self> {
        let file = OpenOptions::new().read(true).write(true).open(path)?;
        let size = file.metadata()?.len();
        let (cylinders, heads, spt) = hd_geometry_from_size(size);
        Ok(Self {
            storage: DiskStorage::FileBacked(file),
            cylinders,
            heads,
            sectors_per_track: spt,
        })
    }

    /// Open an existing hard disk image or create one with the given size.
    pub fn open_or_create_hard_disk(path: &Path, default_size_mb: u64) -> std::io::Result<Self> {
        if path.exists() {
            Self::open_hard_disk(path)
        } else {
            Self::create_hard_disk(path, default_size_mb)
        }
    }

    /// Create a new hard disk image of the given size in MB.
    pub fn create_hard_disk(path: &Path, size_mb: u64) -> std::io::Result<Self> {
        let heads: u8 = 16;
        let spt: u8 = 63;
        let cylinders = ((size_mb * 1024 * 1024 / 512) / (heads as u64 * spt as u64)) as u16;
        let total_bytes = cylinders as u64 * heads as u64 * spt as u64 * SECTOR_SIZE as u64;
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)?;
        file.set_len(total_bytes)?;
        file.flush()?;
        Ok(Self {
            storage: DiskStorage::FileBacked(file),
            cylinders,
            heads,
            sectors_per_track: spt,
        })
    }

    /// Create a blank in-memory hard disk of the given size in MB.
    pub fn new_in_memory_hard_disk(size_mb: u64) -> Self {
        let heads: u8 = 16;
        let spt: u8 = 63;
        let cylinders = ((size_mb * 1024 * 1024 / 512) / (heads as u64 * spt as u64)) as u16;
        let total_bytes = cylinders as u64 * heads as u64 * spt as u64 * SECTOR_SIZE as u64;
        Self {
            storage: DiskStorage::InMemory(vec![0u8; total_bytes as usize]),
            cylinders,
            heads,
            sectors_per_track: spt,
        }
    }

    /// Total size in bytes.
    pub fn total_bytes(&self) -> u64 {
        self.cylinders as u64 * self.heads as u64 * self.sectors_per_track as u64 * SECTOR_SIZE as u64
    }

    pub fn read_sectors(&mut self, c: u16, h: u8, s: u8, count: u8) -> std::io::Result<Vec<u8>> {
        let byte_offset = self.chs_to_offset(c, h, s);
        let total_bytes = count as usize * SECTOR_SIZE;
        let mut buf = vec![0u8; total_bytes];

        match &mut self.storage {
            DiskStorage::FileBacked(file) => {
                file.seek(SeekFrom::Start(byte_offset))?;
                file.read_exact(&mut buf)?;
            }
            DiskStorage::InMemory(data) => {
                let start = byte_offset as usize;
                let end = start + total_bytes;
                if end > data.len() {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::UnexpectedEof,
                        "read past end of disk",
                    ));
                }
                buf.copy_from_slice(&data[start..end]);
            }
        }
        Ok(buf)
    }

    pub fn write_sectors(&mut self, c: u16, h: u8, s: u8, count: u8, data: &[u8]) -> std::io::Result<()> {
        let byte_offset = self.chs_to_offset(c, h, s);
        let total_bytes = count as usize * SECTOR_SIZE;
        assert!(data.len() >= total_bytes);

        match &mut self.storage {
            DiskStorage::FileBacked(file) => {
                file.seek(SeekFrom::Start(byte_offset))?;
                file.write_all(&data[..total_bytes])?;
                file.flush()?;
            }
            DiskStorage::InMemory(mem) => {
                let start = byte_offset as usize;
                let end = start + total_bytes;
                if end > mem.len() {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::UnexpectedEof,
                        "write past end of disk",
                    ));
                }
                mem[start..end].copy_from_slice(&data[..total_bytes]);
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
        let start_lba = self.chs_to_lba(c, h, s);
        let end_lba = start_lba + count as u64;
        let total_sectors = self.cylinders as u64 * self.heads as u64 * self.sectors_per_track as u64;
        end_lba <= total_sectors
    }

    /// Zero-fill a track with the standard DOS format fill byte (0xF6).
    pub fn format_track(&mut self, c: u16, h: u8) -> std::io::Result<()> {
        let fill = vec![0xF6u8; self.sectors_per_track as usize * SECTOR_SIZE];
        self.write_sectors(c, h, 1, self.sectors_per_track, &fill)
    }

    /// Save the entire disk contents to a file.
    pub fn save_to_file(&mut self, path: &Path) -> std::io::Result<()> {
        let mut out = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)?;
        match &mut self.storage {
            DiskStorage::InMemory(data) => {
                out.write_all(data)?;
            }
            DiskStorage::FileBacked(file) => {
                let total = self.cylinders as u64
                    * self.heads as u64
                    * self.sectors_per_track as u64
                    * SECTOR_SIZE as u64;
                file.seek(SeekFrom::Start(0))?;
                let mut buf = vec![0u8; total as usize];
                file.read_exact(&mut buf)?;
                out.write_all(&buf)?;
            }
        }
        out.flush()?;
        Ok(())
    }

    fn chs_to_lba(&self, c: u16, h: u8, s: u8) -> u64 {
        (c as u64 * self.heads as u64 + h as u64) * self.sectors_per_track as u64 + (s as u64 - 1)
    }

    fn chs_to_offset(&self, c: u16, h: u8, s: u8) -> u64 {
        self.chs_to_lba(c, h, s) * SECTOR_SIZE as u64
    }
}

pub const HD_DEFAULT_SIZE_MB: u64 = 32;

fn geometry_from_size(size: u64) -> (u16, u8, u8) {
    match size {
        163_840 => (40, 1, 8),
        184_320 => (40, 1, 9),
        327_680 => (40, 2, 8),
        368_640 => (40, 2, 9),
        737_280 => (80, 2, 9),
        1_228_800 => (80, 2, 15),
        1_474_560 => (80, 2, 18),
        2_949_120 => (80, 2, 36),
        _ => {
            let total_sectors = (size / 512) as u16;
            let spt = 18u8;
            let heads = 2u8;
            let cylinders = total_sectors / (heads as u16 * spt as u16);
            (cylinders.max(1), heads, spt)
        }
    }
}

fn hd_geometry_from_size(size: u64) -> (u16, u8, u8) {
    let total_sectors = size / 512;
    let heads: u8 = 16;
    let spt: u8 = 63;
    let cylinders = (total_sectors / (heads as u64 * spt as u64)) as u16;
    (cylinders.max(1), heads, spt)
}

impl DiskImage {
    /// Format this disk with a FAT filesystem using the fatfs crate.
    /// Sets BPB geometry, media descriptor, cluster size, and root directory entries
    /// to match standard DOS floppy formats. Forces FAT12 for floppy-sized disks.
    pub fn format_fat(&mut self) -> std::io::Result<()> {
        let total_sectors = self.cylinders as u32 * self.heads as u32 * self.sectors_per_track as u32;

        // Standard DOS floppy format parameters: (media_byte, sectors_per_cluster, root_dir_entries)
        let (media_byte, spc, root_entries) = match self.total_bytes() {
            163_840   => (0xFEu8, 1u8, 64u16),   // 160K
            184_320   => (0xFC,   1,   64),        // 180K
            327_680   => (0xFF,   2,   112),       // 320K
            368_640   => (0xFD,   2,   112),       // 360K
            737_280   => (0xF9,   2,   112),       // 720K
            1_228_800 => (0xF9,   1,   224),       // 1.2M
            1_474_560 => (0xF0,   1,   224),       // 1.44M
            2_949_120 => (0xF0,   2,   240),       // 2.88M
            _         => (0xF0,   1,   224),       // other
        };

        let mut options = fatfs::FormatVolumeOptions::new()
            .media(media_byte)
            .total_sectors(total_sectors)
            .bytes_per_cluster(spc as u32 * SECTOR_SIZE as u32)
            .max_root_dir_entries(root_entries)
            .sectors_per_track(self.sectors_per_track as u16)
            .heads(self.heads as u16);

        // Force FAT12 for floppy-sized disks — DOS expects FAT12 on floppies
        if self.heads <= 2 {
            options = options.fat_type(fatfs::FatType::Fat12);
        }

        match &mut self.storage {
            DiskStorage::InMemory(data) => {
                let mut cursor = std::io::Cursor::new(data.as_mut_slice());
                fatfs::format_volume(&mut cursor, options)
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
            }
            DiskStorage::FileBacked(file) => {
                file.seek(SeekFrom::Start(0))?;
                fatfs::format_volume(&mut *file, options)
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
            }
        }
        Ok(())
    }

    /// Copy all files from a host directory into the FAT filesystem on this disk.
    /// Returns the number of files copied.
    pub fn copy_dir_to_fat(&mut self, host_dir: &Path) -> std::io::Result<usize> {
        let offset = self.first_partition_offset();

        match &mut self.storage {
            DiskStorage::FileBacked(file) => {
                let cloned = file.try_clone()?;
                let partition = PartitionSlice::new(cloned, offset)?;
                copy_dir_to_fat_impl(partition, host_dir)
            }
            DiskStorage::InMemory(data) => {
                let cursor = std::io::Cursor::new(data.as_mut_slice());
                let partition = PartitionSlice::new(cursor, offset)?;
                copy_dir_to_fat_impl(partition, host_dir)
            }
        }
    }
}

// --- FAT filesystem access for the `put` command ---

/// Wraps an I/O object and offsets all Seek operations to a partition boundary.
struct PartitionSlice<T> {
    inner: T,
    offset: u64,
}

impl<T: Seek> PartitionSlice<T> {
    fn new(mut inner: T, offset: u64) -> std::io::Result<Self> {
        inner.seek(SeekFrom::Start(offset))?;
        Ok(Self { inner, offset })
    }

}

impl<T: Read> Read for PartitionSlice<T> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.inner.read(buf)
    }
}

impl<T: Write> Write for PartitionSlice<T> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.inner.write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

impl<T: Seek> Seek for PartitionSlice<T> {
    fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        let actual = match pos {
            SeekFrom::Start(n) => SeekFrom::Start(n + self.offset),
            SeekFrom::Current(n) => SeekFrom::Current(n),
            SeekFrom::End(n) => SeekFrom::End(n),
        };
        let result = self.inner.seek(actual)?;
        Ok(result.saturating_sub(self.offset))
    }
}

impl DiskImage {
    /// Read the MBR and return the byte offset of the first partition.
    /// Returns 0 for floppies or disks without a partition table.
    pub fn first_partition_offset(&mut self) -> u64 {
        let mbr = match self.read_sectors(0, 0, 1, 1) {
            Ok(data) => data,
            Err(_) => return 0,
        };
        if mbr.len() < 512 || mbr[510] != 0x55 || mbr[511] != 0xAA {
            return 0;
        }
        // Distinguish a FAT boot sector (floppy / unpartitioned) from an MBR.
        // A FAT boot sector starts with a JMP (0xEB xx 0x90 or 0xE9 xx xx) and
        // has a valid BPB: bytes_per_sector (offset 11-12) == 512.
        let has_jmp = mbr[0] == 0xEB || mbr[0] == 0xE9;
        let bps = u16::from_le_bytes([mbr[11], mbr[12]]);
        if has_jmp && bps == 512 {
            // This is a FAT volume boot record, not an MBR — no partition table.
            return 0;
        }
        // First partition entry at 0x1BE, LBA start at bytes 8-11
        let entry = &mbr[0x1BE..];
        let lba_start = u32::from_le_bytes([entry[8], entry[9], entry[10], entry[11]]);
        if lba_start == 0 {
            return 0;
        }
        lba_start as u64 * SECTOR_SIZE as u64
    }

    /// Copy host file data into the FAT filesystem on this disk.
    /// `dos_path` can be a filename ("GAME.EXE") or path ("\DOS\GAME.EXE").
    pub fn copy_file_to_fat(&mut self, host_data: &[u8], dos_path: &str) -> std::io::Result<()> {
        let offset = self.first_partition_offset();

        match &mut self.storage {
            DiskStorage::FileBacked(file) => {
                let cloned = file.try_clone()?;
                let partition = PartitionSlice::new(cloned, offset)?;
                write_to_fat(partition, host_data, dos_path)?;
            }
            DiskStorage::InMemory(data) => {
                // Cursor<&mut [u8]> implements Read + Write + Seek,
                // and writes go through the mutable reference in-place.
                let cursor = std::io::Cursor::new(data.as_mut_slice());
                let partition = PartitionSlice::new(cursor, offset)?;
                write_to_fat(partition, host_data, dos_path)?;
            }
        }
        Ok(())
    }
}

/// Write a file into a FAT filesystem accessed through the given I/O object.
fn write_to_fat<T: Read + Write + Seek>(io: T, host_data: &[u8], dos_path: &str) -> std::io::Result<()> {
    let fs = fatfs::FileSystem::new(io, fatfs::FsOptions::new())
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;

    let root_dir = fs.root_dir();
    create_fat_file(&root_dir, dos_path, host_data)?;
    Ok(())
}

/// Navigate subdirectories and create the file at the final path component.
fn create_fat_file<T: Read + Write + Seek>(
    root: &fatfs::Dir<T>,
    dos_path: &str,
    data: &[u8],
) -> std::io::Result<()> {
    // Normalize path separators and strip leading backslash
    let path = dos_path.replace('/', "\\");
    let path = path.trim_start_matches('\\');

    let parts: Vec<&str> = path.split('\\').filter(|s| !s.is_empty()).collect();
    if parts.is_empty() {
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "empty filename"));
    }

    // Navigate to parent directory
    let mut current_dir;
    let filename;

    if parts.len() == 1 {
        filename = parts[0];
        let mut f = root.create_file(filename)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
        std::io::Write::write_all(&mut f, data)?;
        return Ok(());
    }

    // Navigate subdirectories
    current_dir = root.open_dir(parts[0])
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
    for &component in &parts[1..parts.len() - 1] {
        current_dir = current_dir.open_dir(component)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
    }

    filename = parts[parts.len() - 1];
    let mut f = current_dir.create_file(filename)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
    std::io::Write::write_all(&mut f, data)?;
    Ok(())
}

/// Open a FAT filesystem and recursively copy a host directory into it.
fn copy_dir_to_fat_impl<T: Read + Write + Seek>(io: T, host_dir: &Path) -> std::io::Result<usize> {
    let fs = fatfs::FileSystem::new(io, fatfs::FsOptions::new())
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
    let root = fs.root_dir();
    copy_host_dir_recursive(&root, host_dir)
}

/// Recursively copy files from a host directory into a FAT directory.
fn copy_host_dir_recursive<T: Read + Write + Seek>(
    fat_dir: &fatfs::Dir<T>,
    host_dir: &Path,
) -> std::io::Result<usize> {
    let mut count = 0;
    for entry in std::fs::read_dir(host_dir)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let name = entry.file_name().to_string_lossy().to_uppercase();

        if file_type.is_file() {
            let data = std::fs::read(entry.path())?;
            let mut f = fat_dir.create_file(&name)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
            std::io::Write::write_all(&mut f, &data)?;
            count += 1;
        } else if file_type.is_dir() {
            let _ = fat_dir.create_dir(&name)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
            let sub_dir = fat_dir.open_dir(&name)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e)))?;
            count += copy_host_dir_recursive(&sub_dir, &entry.path())?;
        }
    }
    Ok(count)
}
