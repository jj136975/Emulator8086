use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

const SECTOR_SIZE: usize = 512;
const FLOPPY_144_SIZE: u64 = 1_474_560; // 80 * 2 * 18 * 512

/// Where the disk data lives.
enum DiskStorage {
    FileBacked(File),
    InMemory(Vec<u8>),
}

/// What the user asked for on the CLI.
pub enum DiskSource {
    FilePath(PathBuf),
    Memory,
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
        Self {
            storage: DiskStorage::InMemory(vec![0u8; FLOPPY_144_SIZE as usize]),
            cylinders: 80,
            heads: 2,
            sectors_per_track: 18,
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
