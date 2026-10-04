//! A disk image: the sectors of one disk, in a host file or in memory, and
//! the geometry that addresses them. Opening and creating image files,
//! sector transfers by CHS address, and saving an image to a host file.
//!
//! The invariant: a file-backed image is its file. Guest writes go straight
//! to that file, so it must never be truncated or replaced under the image:
//! the image keeps the canonical path of its file, a save onto that path
//! only flushes, and creating an image never replaces an existing file.
//!
//! Every transfer is checked against the geometry first (`verify_sectors`),
//! whichever storage the sectors live in.

use super::SECTOR_SIZE;
use super::geometry::{
    Geometry, MAX_CYLINDERS, floppy_geometry, geometry_bytes, geometry_from_boot_sector,
    geometry_from_size, hd_geometry_from_size,
};
use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

/// Where the disk data lives.
pub(super) enum DiskStorage {
    /// An image file on the host. `path` is its canonical path, kept so the
    /// image can tell when it is asked to save onto itself.
    FileBacked {
        file: File,
        path: PathBuf,
    },
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

pub struct DiskImage {
    pub(super) storage: DiskStorage,
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
                (&mut file)
                    .take(SECTOR_SIZE as u64)
                    .read_to_end(&mut boot_sector)?;
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
        Ok(Self::on_file(
            file,
            path,
            hd_geometry_from_size(size),
            readonly,
        ))
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
    pub(super) fn total_sectors(&self) -> u64 {
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

    pub(super) fn outside_the_disk() -> std::io::Error {
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

    pub(super) fn read_sectors(
        &mut self,
        c: u16,
        h: u8,
        s: u8,
        count: u8,
    ) -> std::io::Result<Vec<u8>> {
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
    pub(super) fn verify_sectors(&self, c: u16, h: u8, s: u8, count: u8) -> bool {
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
    pub(super) fn format_track(&mut self, c: u16, h: u8) -> std::io::Result<()> {
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
    pub(super) fn is_backed_by(&self, path: &Path) -> bool {
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
    #[cfg_attr(
        not(test),
        expect(dead_code, reason = "only the test-only monitor calls it")
    )]
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
    pub(super) fn lba_to_chs(&self, lba: u64) -> Option<(u16, u8, u8)> {
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

    pub(super) fn require_writable(&self) -> std::io::Result<()> {
        if self.readonly {
            return Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "disk is write-protected",
            ));
        }
        Ok(())
    }
}
