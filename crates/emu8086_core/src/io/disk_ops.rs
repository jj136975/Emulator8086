//! The disk tools: what a user does to the drives of a machine from the
//! host while the guest runs. Each is an `Operation` (insert a disk, create
//! an image file and insert it, move a disk to another drive, save it,
//! eject it, copy host files onto it) that `apply` carries out on the disk
//! controller; `contents` hands a disk back whole, for a host that takes it
//! as its bytes. The app's Disks panel is the caller.
//!
//! An operation answers with an `Outcome`: what was done or why it could
//! not be, in words for the user, and whether the guest has to be rebooted.
//! Nothing here prints, and nothing asks which host this is: a file comes
//! as a `HostFile`, by its path or by its contents, whichever the host has.
//!
//! The invariants:
//!
//! - An operation that may have changed a hard disk says that the guest
//!   must be rebooted, and one that cannot have changed one does not. DOS
//!   never re-reads a fixed disk: it writes the FAT and directory sectors
//!   in its buffers back over whatever the host changed. A floppy needs no
//!   word: the controller raises its change line.
//! - One image file is one disk in one drive. Inserting an image never
//!   creates a file, and creating one never replaces a file.
//! - An image of no bytes is no disk, however it comes: inserting one is
//!   refused, and the drive keeps the disk it holds.

use crate::io::disk::{
    DiskController, DiskImage, FLOPPY_144_SIZE, HD_DEFAULT_SIZE_MB, SaveOutcome, WallClock,
};
use crate::io::disk_spec::{FLOPPY_SIZES_KB, drive_label, is_drive, require_unshared};
use std::fmt;
use std::path::{Path, PathBuf};

/// A file of the host, as it reaches the tools.
#[derive(Clone, PartialEq, Eq)]
pub enum HostFile {
    /// By its path, on a host whose files have one: the tools open, read or
    /// create the file themselves. Where files to copy are asked for, it
    /// may be the path of a directory.
    Path(PathBuf),
    /// By its name and its contents, on a host that hands a file over and
    /// gives no path to it (a browser).
    Contents { name: String, bytes: Vec<u8> },
}

impl HostFile {
    /// What the user knows the file by: its path, or its name.
    fn shown(&self) -> String {
        match self {
            Self::Path(path) => path.display().to_string(),
            Self::Contents { name, .. } => name.clone(),
        }
    }
}

impl fmt::Debug for HostFile {
    /// The contents by their length: an image is megabytes.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Path(path) => write!(f, "Path({path:?})"),
            Self::Contents { name, bytes } => {
                write!(f, "Contents({name:?}, {} bytes)", bytes.len())
            }
        }
    }
}

/// What goes into a drive.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Disk {
    /// An existing image. One that comes by its path is the disk itself:
    /// the guest's writes go to that file. One that comes as its contents
    /// is a disk held in memory, and its file stays as it was.
    Image(HostFile),
    /// A blank disk held in memory. In a floppy drive it is a formatted
    /// floppy of `kb` KB, one of `FLOPPY_SIZES_KB`, and of 1440 KB if no
    /// size is given. In a hard disk drive it is an unformatted disk of
    /// `HD_DEFAULT_SIZE_MB`, the one size there is: giving one is refused.
    Blank { kb: Option<u64> },
}

/// What a user does to the drives. A drive is named by its BIOS number:
/// 00h to 03h for the floppy drives A: to D:, 80h and up for the hard
/// disks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operation {
    /// Puts `disk` into `drive`, in place of the disk that may be there.
    /// `read_only` write-protects it.
    Insert {
        drive: u8,
        disk: Disk,
        read_only: bool,
    },
    /// Creates the image file `path` and inserts it: a formatted 1.44 MB
    /// floppy, or a blank hard disk of `HD_DEFAULT_SIZE_MB`.
    Create { drive: u8, path: PathBuf },
    /// Takes the disk out of `from` and puts it into `to`, in place of the
    /// disk that may be there.
    Move { from: u8, to: u8 },
    /// Writes the disk in `drive` to the file `path`, as a whole, or
    /// flushes it if that is the file it lives in.
    Save { drive: u8, path: PathBuf },
    /// Takes the disk out of `drive`.
    Eject { drive: u8 },
    /// Copies `files` into the FAT volume on the disk in `drive`, each
    /// under its own name; a directory with all that is below it.
    Put { drive: u8, files: Vec<HostFile> },
}

/// What came of an operation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Outcome {
    /// What was done, or why it could not be done: either way, words for
    /// the user.
    pub result: Result<String, String>,
    /// The operation changed a hard disk, or may have: the guest has to be
    /// rebooted before it uses the drive.
    pub needs_reboot: bool,
}

impl Outcome {
    /// The outcome of an operation that changes the disks in `drives` when
    /// it succeeds, and nothing when it fails.
    fn of_change(drives: &[u8], result: Result<String, String>) -> Self {
        let needs_reboot = result.is_ok() && drives.iter().copied().any(is_hard_disk);
        Self {
            result,
            needs_reboot,
        }
    }

    /// The outcome of an operation that was refused before it touched a
    /// disk.
    fn refused(reason: String) -> Self {
        Self {
            result: Err(reason),
            needs_reboot: false,
        }
    }
}

/// Carries `operation` out on `disks`. `clock` is the host's wall clock,
/// for the operations that format a disk or copy files onto one.
pub fn apply(disks: &mut DiskController, operation: Operation, clock: &dyn WallClock) -> Outcome {
    match operation {
        Operation::Insert {
            drive,
            disk,
            read_only,
        } => Outcome::of_change(&[drive], insert(disks, drive, disk, read_only, clock)),
        Operation::Create { drive, path } => {
            Outcome::of_change(&[drive], create(disks, drive, &path, clock))
        }
        Operation::Move { from, to } => Outcome::of_change(&[from, to], move_disk(disks, from, to)),
        Operation::Save { drive, path } => Outcome {
            result: save(disks, drive, &path),
            needs_reboot: false,
        },
        Operation::Eject { drive } => Outcome::of_change(&[drive], eject(disks, drive)),
        Operation::Put { drive, files } => put(disks, drive, &files, clock),
    }
}

/// The disk in `drive` as the bytes of its image: what `Operation::Save`
/// writes to its file, for a host that takes a disk back as its contents.
/// It changes nothing.
pub fn contents(disks: &mut DiskController, drive: u8) -> Result<Vec<u8>, String> {
    require_drive(drive)?;
    let label = drive_label(drive);
    let disk = disks
        .find_drive(drive)
        .ok_or_else(|| format!("{} is empty", label))?;
    disk.contents()
        .map_err(|error| format!("The disk in {} cannot be read: {}", label, error))
}

fn is_hard_disk(drive: u8) -> bool {
    drive >= 0x80
}

/// A number that is no drive is refused before it reaches the controller,
/// which would take a disk for it and make a drive of it.
fn require_drive(drive: u8) -> Result<(), String> {
    if is_drive(drive) {
        Ok(())
    } else {
        Err(format!("{:#04X} is not the number of a drive", drive))
    }
}

/// Why the image the user knows as `shown` is refused: it has no bytes, and
/// the disk made of it would be a blank one the user never asked for.
fn no_disk(shown: &str) -> String {
    format!("{shown} is empty: an image of no bytes is no disk")
}

fn insert(
    disks: &mut DiskController,
    drive: u8,
    disk: Disk,
    read_only: bool,
    clock: &dyn WallClock,
) -> Result<String, String> {
    require_drive(drive)?;
    let label = drive_label(drive);
    let hard_disk = is_hard_disk(drive);

    let (image, mut message) = match disk {
        Disk::Image(HostFile::Path(path)) => {
            // A wrong path must not leave a new, empty image behind, nor
            // take the disk out of the drive.
            if !path.exists() {
                return Err(format!(
                    "'{}' not found: inserting an image creates none",
                    path.display()
                ));
            }
            if path
                .metadata()
                .is_ok_and(|file| file.is_file() && file.len() == 0)
            {
                return Err(no_disk(&format!("'{}'", path.display())));
            }
            require_unshared(disks, &path, drive)?;
            let opened = if hard_disk {
                DiskImage::open_hard_disk(&path, read_only)
            } else {
                DiskImage::open(&path, read_only)
            };
            let image =
                opened.map_err(|error| format!("Cannot open '{}': {}", path.display(), error))?;
            (image, format!("{} now holds {}", label, path.display()))
        }
        Disk::Image(HostFile::Contents { name, bytes }) => {
            if bytes.is_empty() {
                return Err(no_disk(&name));
            }
            let image = if hard_disk {
                DiskImage::hard_disk_from_bytes(bytes, &name, read_only)
            } else {
                DiskImage::from_bytes(bytes, &name, read_only)
            };
            (image, format!("{} now holds {}, in memory", label, name))
        }
        Disk::Blank { kb: Some(_) } if hard_disk => {
            return Err(format!(
                "A blank hard disk has one size, {} MB: none can be asked for",
                HD_DEFAULT_SIZE_MB
            ));
        }
        Disk::Blank { kb: None } if hard_disk => {
            let mut image = DiskImage::new_in_memory_hard_disk(HD_DEFAULT_SIZE_MB);
            image.readonly = read_only;
            let message = format!(
                "{} now holds a blank {}MB in-memory hard disk",
                label, HD_DEFAULT_SIZE_MB
            );
            (image, message)
        }
        Disk::Blank { kb } => {
            let kb = kb.unwrap_or(FLOPPY_144_SIZE / 1024);
            if !FLOPPY_SIZES_KB.contains(&kb) {
                let sizes: Vec<String> = FLOPPY_SIZES_KB.iter().map(u64::to_string).collect();
                return Err(format!(
                    "{} KB is no floppy size (the sizes in KB: {})",
                    kb,
                    sizes.join(", ")
                ));
            }
            let mut image = DiskImage::new_in_memory_sized(kb * 1024, false);
            image
                .format_fat(clock)
                .map_err(|error| format!("Could not format the floppy: {}", error))?;
            image.readonly = read_only;
            let message = format!("{} now holds a formatted {}KB in-memory floppy", label, kb);
            (image, message)
        }
    };

    if let Some(note) = image.capacity_note() {
        message.push_str(". Warning: ");
        message.push_str(&note);
    }
    disks.attach(drive, image);
    Ok(message)
}

fn create(
    disks: &mut DiskController,
    drive: u8,
    path: &Path,
    clock: &dyn WallClock,
) -> Result<String, String> {
    require_drive(drive)?;
    let label = drive_label(drive);
    let cannot_create = |error: std::io::Error| {
        if error.kind() == std::io::ErrorKind::AlreadyExists {
            format!(
                "Cannot create '{}': it exists, and a new image never replaces a file",
                path.display()
            )
        } else {
            format!("Cannot create '{}': {}", path.display(), error)
        }
    };

    if is_hard_disk(drive) {
        let image = DiskImage::create_hard_disk(path, HD_DEFAULT_SIZE_MB).map_err(cannot_create)?;
        disks.attach(drive, image);
        Ok(format!(
            "{} now holds the new blank {}MB image {}",
            label,
            HD_DEFAULT_SIZE_MB,
            path.display()
        ))
    } else {
        let mut image = DiskImage::create(path, FLOPPY_144_SIZE).map_err(cannot_create)?;
        image.format_fat(clock).map_err(cannot_create)?;
        disks.attach(drive, image);
        Ok(format!(
            "{} now holds the new formatted {}KB image {}",
            label,
            FLOPPY_144_SIZE / 1024,
            path.display()
        ))
    }
}

fn move_disk(disks: &mut DiskController, from: u8, to: u8) -> Result<String, String> {
    require_drive(from)?;
    require_drive(to)?;
    let disk = disks
        .detach(from)
        .ok_or_else(|| format!("{} is empty", drive_label(from)))?;
    disks.attach(to, disk);
    Ok(format!(
        "Moved {} -> {}",
        drive_label(from),
        drive_label(to)
    ))
}

fn save(disks: &mut DiskController, drive: u8, target: &Path) -> Result<String, String> {
    require_drive(drive)?;
    let label = drive_label(drive);
    if let Some(other) = disks.drive_backed_by(target)
        && other != drive
    {
        return Err(format!(
            "{} is the image file of {}, which has to be ejected first",
            target.display(),
            drive_label(other)
        ));
    }
    let disk = disks
        .find_drive(drive)
        .ok_or_else(|| format!("{} is empty", label))?;
    match disk
        .save_to_file(target)
        .map_err(|error| format!("Cannot save {} to {}: {}", label, target.display(), error))?
    {
        SaveOutcome::Written => Ok(format!("{} saved to {}", label, target.display())),
        SaveOutcome::AlreadyBacking => Ok(format!(
            "{} already lives in {}: every guest write goes straight to that file",
            label,
            target.display()
        )),
    }
}

fn eject(disks: &mut DiskController, drive: u8) -> Result<String, String> {
    require_drive(drive)?;
    let label = drive_label(drive);
    if disks.detach(drive).is_none() {
        return Err(format!("{} is empty", label));
    }
    Ok(format!("{} ejected", label))
}

/// What a `Put` copied of one of its files.
struct Copied {
    /// The files that are on the disk now: one, or those of a directory.
    files: usize,
    /// What was done, in words, had this file been the only one.
    report: String,
}

fn put(
    disks: &mut DiskController,
    drive: u8,
    files: &[HostFile],
    clock: &dyn WallClock,
) -> Outcome {
    if let Err(reason) = require_drive(drive) {
        return Outcome::refused(reason);
    }
    let label = drive_label(drive);
    let Some(disk) = disks.find_drive(drive) else {
        return Outcome::refused(format!("{} is empty", label));
    };
    let write_protected = disk.readonly;
    if files.is_empty() {
        return Outcome::refused("No file to copy".to_string());
    }

    let mut copied = 0;
    let mut began = false;
    let mut last_report = String::new();
    let mut failure = None;
    for file in files {
        match copy(disks, drive, file, clock, &mut began) {
            Ok(done) => {
                copied += done.files;
                last_report = done.report;
            }
            Err(reason) => {
                failure = Some(reason);
                break;
            }
        }
    }

    let result = match failure {
        None if files.len() == 1 => Ok(last_report),
        None => Ok(format!("Copied {} files -> {}\\", copied, label)),
        Some(reason) if copied == 0 => Err(reason),
        Some(reason) => Err(format!(
            "{} ({} copied before it)",
            reason,
            if copied == 1 {
                "1 file was".to_string()
            } else {
                format!("{} files were", copied)
            }
        )),
    };
    Outcome {
        result,
        // A copy that failed half-way has changed the disk too; only a
        // write-protected disk is certain to be untouched.
        needs_reboot: is_hard_disk(drive) && began && !write_protected,
    }
}

/// Copies one file, or one directory, onto the disk in `drive`. `began` is
/// set once the controller was asked to write: from there on the disk may
/// have changed, whatever comes of the copy.
fn copy(
    disks: &mut DiskController,
    drive: u8,
    file: &HostFile,
    clock: &dyn WallClock,
    began: &mut bool,
) -> Result<Copied, String> {
    let label = drive_label(drive);
    let shown = file.shown();
    // The report shows the name the file has in DOS, which is a generated
    // one if its own is not a valid 8.3 name.
    let report = |stored: String, bytes: usize| Copied {
        files: 1,
        report: format!(
            "Copied '{}' -> {}\\{} ({} bytes)",
            shown, label, stored, bytes
        ),
    };
    match file {
        HostFile::Path(path) if !path.exists() => Err(format!("'{}' not found", shown)),
        HostFile::Path(path) if path.is_dir() => {
            *began = true;
            let count = disks
                .put_dir(drive, path, clock)
                .map_err(|error| format!("'{}': {}", shown, error))?;
            Ok(Copied {
                files: count,
                report: format!("Copied {} files from '{}' -> {}\\", count, shown, label),
            })
        }
        HostFile::Path(path) => {
            let data = std::fs::read(path)
                .map_err(|error| format!("Cannot read '{}': {}", shown, error))?;
            let name = path
                .file_name()
                .map(|name| name.to_string_lossy().into_owned())
                .unwrap_or_else(|| "FILE.DAT".to_string());
            *began = true;
            let stored = disks
                .put_file(drive, &data, &name, clock)
                .map_err(|error| format!("'{}': {}", shown, error))?;
            Ok(report(stored, data.len()))
        }
        HostFile::Contents { name, bytes } => {
            *began = true;
            let stored = disks
                .put_file(drive, bytes, name, clock)
                .map_err(|error| format!("'{}': {}", shown, error))?;
            Ok(report(stored, bytes.len()))
        }
    }
}

#[cfg(test)]
#[path = "disk_ops_tests.rs"]
mod tests;
