//! A disk image: the sectors of one disk, in a host file or in memory, and
//! the geometry that addresses them. Opening and creating image files,
//! taking an image whose contents a host hands over, sector transfers by
//! CHS address, and giving an image back: saved to a host file, or as its
//! contents.
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

/// A new file at `path` of `size` bytes, filled with zeros: never a file
/// that existed. One made here that could not be given its size is removed
/// again, so that it does not stand in the way of a later try.
fn new_image_file(path: &Path, size: u64) -> std::io::Result<File> {
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create_new(true)
        .open(path)?;
    // On an error the file goes with the closure that was to hand it on, and
    // is closed by the time it is removed: a host may refuse to remove a
    // file that is open.
    file.set_len(size).map(|()| file).inspect_err(|_| {
        let _ = std::fs::remove_file(path);
    })
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
    /// The name of the file whose contents an image held in memory was
    /// made of (`from_bytes`). A blank image has none, and a file-backed
    /// one has its path.
    name: Option<String>,
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
            name: None,
            cylinders,
            heads,
            sectors_per_track,
            readonly,
        }
    }

    /// A blank image held in memory.
    fn in_memory(geometry: Geometry, readonly: bool) -> Self {
        Self::holding(
            vec![0u8; geometry_bytes(geometry) as usize],
            None,
            geometry,
            readonly,
        )
    }

    /// An image held in memory whose contents are `data`. A disk reaches as
    /// far as its geometry: contents that end before it are filled up with
    /// zeros, since the guest may write there, as it may behind the end of
    /// an image file that is shorter than its disk. Contents that go on
    /// behind the geometry are kept, out of the guest's reach.
    fn holding(
        mut data: Vec<u8>,
        name: Option<String>,
        geometry: Geometry,
        readonly: bool,
    ) -> Self {
        let (cylinders, heads, sectors_per_track) = geometry;
        let disk_bytes = geometry_bytes(geometry) as usize;
        if data.len() < disk_bytes {
            data.resize(disk_bytes, 0);
        }
        Self {
            storage: DiskStorage::InMemory(data),
            name,
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
    /// existing file is never replaced, and a file that could not be given
    /// its size is not left behind.
    pub fn create(path: &Path, size: u64) -> std::io::Result<Self> {
        let file = new_image_file(path, size)?;
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
    /// file is never replaced, and a file that could not be given its size
    /// is not left behind.
    pub fn create_hard_disk(path: &Path, size_mb: u64) -> std::io::Result<Self> {
        let geometry = hd_geometry_from_size(size_mb * 1024 * 1024);
        let file = new_image_file(path, geometry_bytes(geometry))?;
        Ok(Self::on_file(file, path, geometry, false))
    }

    /// Create a blank in-memory hard disk of the given size in MB.
    pub fn new_in_memory_hard_disk(size_mb: u64) -> Self {
        Self::in_memory(hd_geometry_from_size(size_mb * 1024 * 1024), false)
    }

    /// A floppy image whose contents are `bytes`, held in memory: what a
    /// host gives that hands a file over without a path to it. `name` is
    /// the name of that file. The geometry is found as `open` finds it.
    pub fn from_bytes(bytes: Vec<u8>, name: &str, readonly: bool) -> Self {
        let size = bytes.len() as u64;
        let geometry = floppy_geometry(size)
            .or_else(|| geometry_from_boot_sector(&bytes))
            .unwrap_or_else(|| hd_geometry_from_size(size));
        Self::holding(bytes, Some(name.to_string()), geometry, readonly)
    }

    /// A hard disk image whose contents are `bytes`, held in memory. `name`
    /// is the name of the file they were handed over as. The geometry is
    /// found as `open_hard_disk` finds it.
    pub fn hard_disk_from_bytes(bytes: Vec<u8>, name: &str, readonly: bool) -> Self {
        let geometry = hd_geometry_from_size(bytes.len() as u64);
        Self::holding(bytes, Some(name.to_string()), geometry, readonly)
    }

    /// The name of the file whose contents an image held in memory was made
    /// of. `None` for a blank image, and for a file-backed one, which has
    /// its path (`backing_path`).
    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
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
    /// image: CHS addresses end at 1024 cylinders, and whatever a larger
    /// image holds beyond them is out of reach.
    pub fn capacity_note(&self) -> Option<String> {
        let image_size = match &self.storage {
            DiskStorage::FileBacked { file, .. } => file.metadata().ok()?.len(),
            DiskStorage::InMemory(data) => data.len() as u64,
        };
        let cylinder = geometry_bytes((1, self.heads, self.sectors_per_track));
        let cut_off =
            self.cylinders as u64 == MAX_CYLINDERS && image_size >= self.total_bytes() + cylinder;
        cut_off.then(|| {
            format!(
                "the image is {} MB, but a disk has at most {} cylinders: the guest sees only \
                 the first {} MB",
                image_size >> 20,
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

    /// The whole image as bytes: what `save_to_file` writes to its target,
    /// for a host that takes a file back as its contents. Such a host has
    /// neither a path to save to nor a process to name a temporary file
    /// after (a browser), so nothing here asks for one.
    pub fn contents(&mut self) -> std::io::Result<Vec<u8>> {
        match &mut self.storage {
            DiskStorage::InMemory(data) => Ok(data.clone()),
            DiskStorage::FileBacked { file, .. } => {
                let mut contents = Vec::new();
                file.seek(SeekFrom::Start(0))?;
                file.read_to_end(&mut contents)?;
                Ok(contents)
            }
        }
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
