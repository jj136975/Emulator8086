//! FAT volumes as the host writes and reads them, through fatfs: a volume
//! in memory mounted at a stamp (`mount`), or to be read alone
//! (`mount_to_read`), files put onto it and taken off it, a host directory
//! copied onto it, the names DOS can have, and text as DOS reads it.
//!
//! The invariant: every name the tool writes is a short name that DOS takes
//! as it is (`dos_name`). fatfs, without its `alloc` feature, writes no long
//! names: it would make up a short name of its own for any other.

use crate::stamp::Stamp;
use fatfs::{Dir, FileSystem, ReadWriteSeek};
use std::io::{self, Cursor, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

/// A FAT volume held in memory.
pub(crate) type Volume<'a> = FileSystem<Cursor<&'a mut [u8]>>;

/// Mounts the FAT volume that `bytes` hold; what is written to it is
/// stamped with `stamp`.
pub(crate) fn mount<'a>(bytes: &'a mut [u8], stamp: &'static Stamp) -> io::Result<Volume<'a>> {
    FileSystem::new(Cursor::new(bytes), stamp.options())
}

/// A FAT volume held in memory, mounted to be read alone.
pub(crate) type ReadVolume<'a> = FileSystem<ReadOnly<'a>>;

/// The bytes of a volume, as fatfs reads them and cannot write them.
pub(crate) struct ReadOnly<'a>(Cursor<&'a [u8]>);

impl Read for ReadOnly<'_> {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        self.0.read(buffer)
    }
}

impl Seek for ReadOnly<'_> {
    fn seek(&mut self, to: SeekFrom) -> io::Result<u64> {
        self.0.seek(to)
    }
}

impl Write for ReadOnly<'_> {
    fn write(&mut self, _: &[u8]) -> io::Result<usize> {
        Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            "the volume is mounted to be read alone",
        ))
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// Mounts the FAT volume that `bytes` hold to be read alone, at `stamp` as
/// `mount` does: a write to it fails, and the bytes stay as they are.
pub(crate) fn mount_to_read<'a>(
    bytes: &'a [u8],
    stamp: &'static Stamp,
) -> io::Result<ReadVolume<'a>> {
    FileSystem::new(ReadOnly(Cursor::new(bytes)), stamp.options())
}

/// `name` as DOS spells it, if DOS can have it: upper case, a base of one
/// to eight characters and an extension of up to three, of the characters
/// a short name may hold.
pub(crate) fn dos_name(name: &str) -> Option<String> {
    let name = name.to_ascii_uppercase();
    let (base, extension) = name.split_once('.').unwrap_or((name.as_str(), ""));
    let allowed =
        |c: char| c.is_ascii_uppercase() || c.is_ascii_digit() || "!#$%&'()-@^_`{}~".contains(c);
    let fits = (1..=8).contains(&base.len())
        && extension.len() <= 3
        && !name.ends_with('.')
        && base.chars().chain(extension.chars()).all(allowed);
    fits.then_some(name)
}

/// `text` as DOS reads text: each line ends in CR LF. A line that does
/// already is left as it is.
pub(crate) fn dos_text(text: &[u8]) -> Vec<u8> {
    let mut lines = Vec::with_capacity(text.len() + text.len() / 32);
    let mut previous = 0;
    for &byte in text {
        if byte == b'\n' && previous != b'\r' {
            lines.push(b'\r');
        }
        lines.push(byte);
        previous = byte;
    }
    lines
}

/// Writes `data` as the file `name` in `dir`, in the place of a file of
/// that name.
pub(crate) fn put<T: ReadWriteSeek>(dir: &Dir<'_, T>, name: &str, data: &[u8]) -> io::Result<()> {
    let mut file = dir.create_file(name)?;
    file.truncate()?;
    file.write_all(data)?;
    file.flush()
}

/// The contents of the file at `path`, below `dir`.
pub(crate) fn get<T: ReadWriteSeek>(dir: &Dir<'_, T>, path: &str) -> io::Result<Vec<u8>> {
    let mut data = Vec::new();
    dir.open_file(path)?.read_to_end(&mut data)?;
    Ok(data)
}

/// The short name of `entry`, as DOS spells it.
pub(crate) fn short_name<T: ReadWriteSeek>(entry: &fatfs::DirEntry<'_, T>) -> String {
    String::from_utf8_lossy(entry.short_file_name_as_bytes()).into_owned()
}

/// The names of the files in `dir`, its directories left out, in the order
/// of its entries.
pub(crate) fn file_names<T: ReadWriteSeek>(dir: &Dir<'_, T>) -> io::Result<Vec<String>> {
    let mut names = Vec::new();
    for entry in dir.iter() {
        let entry = entry?;
        if entry.is_file() {
            names.push(short_name(&entry));
        }
    }
    Ok(names)
}

/// Why a host directory could not be copied onto a volume.
#[derive(Debug)]
pub(crate) enum TreeError {
    /// A file or a directory of the host has a name DOS cannot have.
    NotDosName(PathBuf),
    /// The host could not read it, or the volume could not take it.
    Io(PathBuf, io::Error),
}

impl std::fmt::Display for TreeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotDosName(path) => write!(f, "{} has a name DOS cannot have", path.display()),
            Self::Io(path, error) => write!(f, "{}: {error}", path.display()),
        }
    }
}

/// Copies the files and the directories in the host directory `host` into
/// `dir`, each directory's entries in the order of their names, and
/// returns how many files it copied. The order is the host's on no host:
/// the same tree makes the same volume everywhere.
pub(crate) fn put_tree<T: ReadWriteSeek>(
    dir: &Dir<'_, T>,
    host: &Path,
) -> Result<usize, TreeError> {
    let failed = |error| TreeError::Io(host.to_path_buf(), error);
    let mut entries = Vec::new();
    for entry in std::fs::read_dir(host).map_err(failed)? {
        let entry = entry.map_err(failed)?;
        let path = entry.path();
        let name = entry
            .file_name()
            .to_str()
            .and_then(dos_name)
            .ok_or_else(|| TreeError::NotDosName(path.clone()))?;
        entries.push((name, path));
    }
    entries.sort();
    let mut files = 0;
    for (name, path) in entries {
        let failed = |error| TreeError::Io(path.clone(), error);
        if path.is_dir() {
            let below = dir.create_dir(&name).map_err(failed)?;
            files += put_tree(&below, &path)?;
        } else {
            put(dir, &name, &std::fs::read(&path).map_err(failed)?).map_err(failed)?;
            files += 1;
        }
    }
    Ok(files)
}

#[cfg(test)]
#[path = "fat_tests.rs"]
mod tests;
