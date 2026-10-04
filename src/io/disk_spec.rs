//! Parsing of what the user types to name drives and disk images, and the
//! opening of the images the command line names. The `--disk`, `--hd` and
//! `--boot` options and the commands of the F12 monitor share these
//! functions, so both accept the same spellings.
//!
//! The invariant: a colon separates the parts of a specification
//! (`b:disk.img:ro`, `memory:720`) and is also part of a Windows path, so
//! each part is recognised by what stands around its colon: a drive letter
//! only when no path separator follows, `memory` only as the whole word, an
//! access suffix only as the last part. What is left is the path.

use crate::io::disk::{
    BOOT_ORDER_END, DiskController, DiskImage, DiskSource, DiskSpec, FLOPPY_144_SIZE,
    HD_DEFAULT_SIZE_MB,
};
use std::path::PathBuf;

/// Sizes, in KB, of the floppy formats an in-memory disk can have.
pub const FLOPPY_SIZES_KB: [u64; 8] = [160, 180, 320, 360, 720, 1200, 1440, 2880];

/// Parses a drive name: `a` to `d` for the floppy drives, `hd0`, `hd1`, ...
/// for the hard disks. Returns the BIOS drive number.
pub fn parse_drive(name: &str) -> Result<u8, String> {
    let lower = name.trim().to_ascii_lowercase();
    let drive = match lower.as_str() {
        "a" => Some(0x00),
        "b" => Some(0x01),
        "c" => Some(0x02),
        "d" => Some(0x03),
        _ => lower
            .strip_prefix("hd")
            .filter(|index| !index.is_empty() && index.bytes().all(|b| b.is_ascii_digit()))
            .and_then(|index| index.parse::<u8>().ok())
            .and_then(|index| 0x80u8.checked_add(index))
            // The boot-order port ends its list with this value.
            .filter(|drive| *drive != BOOT_ORDER_END),
    };
    drive.ok_or_else(|| format!("invalid drive '{}' (use a-d or hd0, hd1, ...)", name))
}

/// How a BIOS drive number is shown to the user: `A:` or `HD0`.
pub fn drive_label(drive: u8) -> String {
    if drive < 0x80 {
        format!("{}:", (b'A' + drive) as char)
    } else {
        format!("HD{}", drive - 0x80)
    }
}

/// Splits a trailing `:ro` or `:rw` (in either case) off an image
/// specification. Returns the rest and whether the image is read-only.
pub fn split_access_suffix(text: &str) -> (&str, bool) {
    match text.rsplit_once(':') {
        Some((rest, mode)) if mode.eq_ignore_ascii_case("ro") => (rest, true),
        Some((rest, mode)) if mode.eq_ignore_ascii_case("rw") => (rest, false),
        _ => (text, false),
    }
}

/// Whether `body` is `memory:<something>`.
fn memory_argument(body: &str) -> Option<&str> {
    body.split_once(':')
        .filter(|(keyword, _)| keyword.eq_ignore_ascii_case("memory"))
        .map(|(_, argument)| argument)
}

/// Parses what goes into a floppy drive: `<path>`, `memory` (a blank
/// formatted 1.44M disk held in memory) or `memory:<KB>` for another
/// standard floppy size; each may be followed by `:ro` or `:rw`.
pub fn parse_source(text: &str) -> Result<(DiskSource, bool), String> {
    let (body, readonly) = split_access_suffix(text.trim());
    let source = if body.eq_ignore_ascii_case("memory") {
        DiskSource::Memory(FLOPPY_144_SIZE)
    } else if let Some(size) = memory_argument(body) {
        match size.parse::<u64>() {
            Ok(kb) if FLOPPY_SIZES_KB.contains(&kb) => DiskSource::Memory(kb * 1024),
            _ => {
                return Err(format!(
                    "unsupported floppy size '{}' (sizes in KB: 160, 180, 320, 360, 720, 1200, \
                     1440, 2880)",
                    size
                ));
            }
        }
    } else if body.is_empty() {
        return Err("missing image path".to_string());
    } else {
        DiskSource::FilePath(PathBuf::from(body))
    };
    Ok((source, readonly))
}

/// Parses what a hard disk is: `<path>` or `memory` (a blank disk of the
/// default size held in memory); each may be followed by `:ro` or `:rw`.
pub fn parse_hd_source(text: &str) -> Result<(DiskSource, bool), String> {
    let (body, readonly) = split_access_suffix(text.trim());
    let source = if body.eq_ignore_ascii_case("memory") {
        DiskSource::Memory(HD_DEFAULT_SIZE_MB * 1024 * 1024)
    } else if memory_argument(body).is_some() {
        return Err("an in-memory hard disk has a fixed size: use plain 'memory'".to_string());
    } else if body.is_empty() {
        return Err("missing image path".to_string());
    } else {
        DiskSource::FilePath(PathBuf::from(body))
    };
    Ok((source, readonly))
}

/// Parses one `--disk` value: `[<drive>:]<source>`. The drive is a floppy
/// drive letter and defaults to `a`.
pub fn parse_disk_spec(spec: &str) -> Result<DiskSpec, String> {
    // "a:image" names an emulator drive, while "C:\image" and "C:/image" are
    // Windows paths: the character after the colon tells them apart.
    let bytes = spec.as_bytes();
    let has_drive = bytes.len() >= 2
        && bytes[0].is_ascii_alphabetic()
        && bytes[1] == b':'
        && !matches!(bytes.get(2), Some(b'\\' | b'/'));

    let (drive, source_text) = if has_drive {
        let letter = &spec[..1];
        let drive = parse_drive(letter).map_err(|_| {
            format!(
                "unsupported drive letter '{}' in '{}' (floppy drives are a-d; hard disks are \
                 attached with --hd)",
                letter, spec
            )
        })?;
        (drive, &spec[2..])
    } else {
        (0x00, spec)
    };

    let (source, readonly) =
        parse_source(source_text).map_err(|error| format!("{} in '{}'", error, spec))?;
    Ok(DiskSpec {
        drive,
        source,
        readonly,
    })
}

/// Parses all `--disk` values. Each drive can be given only once.
pub fn parse_disk_specs(values: &[String]) -> Result<Vec<DiskSpec>, String> {
    let mut specs: Vec<DiskSpec> = Vec::new();
    for value in values {
        let spec = parse_disk_spec(value)?;
        if specs.iter().any(|earlier| earlier.drive == spec.drive) {
            return Err(format!(
                "drive {} is given more than once ('{}')",
                drive_label(spec.drive),
                value
            ));
        }
        specs.push(spec);
    }
    Ok(specs)
}

/// Parses a `--boot` value such as `hd0,a` into BIOS drive numbers.
pub fn parse_boot_order(spec: &str) -> Result<Vec<u8>, String> {
    spec.split(',')
        .map(|name| {
            parse_drive(name).map_err(|error| format!("{} in boot order '{}'", error, spec))
        })
        .collect()
}

/// Opens the images named on the command line and attaches them to a new
/// controller: `disk_specs` to their floppy drives, `hd_specs` to hard
/// disks 0, 1, ... in the order given.
///
/// The image for floppy drive A: has to exist. An image for another floppy
/// drive or for a hard disk is created when its file does not exist. That,
/// and a hard disk image too large to be used in full, is reported on
/// stderr.
pub fn open_drives(
    disk_specs: Vec<DiskSpec>,
    hd_specs: &[String],
) -> Result<DiskController, String> {
    let mut controller = DiskController::new();

    for spec in disk_specs {
        let label = drive_label(spec.drive);
        let image = match spec.source {
            DiskSource::Memory(size) => {
                let mut image = DiskImage::new_in_memory_sized(size, spec.readonly);
                if let Err(error) = image.format_fat() {
                    eprintln!("Warning: could not format floppy: {}", error);
                }
                image
            }
            DiskSource::FilePath(path) if spec.drive == 0x00 || path.exists() => {
                DiskImage::open(&path, spec.readonly).map_err(|error| {
                    format!(
                        "cannot open disk image '{}' for drive {}: {}",
                        path.display(),
                        label,
                        error
                    )
                })?
            }
            DiskSource::FilePath(path) => {
                let mut image = DiskImage::create(&path, FLOPPY_144_SIZE).map_err(|error| {
                    format!(
                        "cannot create disk image '{}' for drive {}: {}",
                        path.display(),
                        label,
                        error
                    )
                })?;
                image.readonly = spec.readonly;
                eprintln!(
                    "{} new blank 1.44M image created: {}",
                    label,
                    path.display()
                );
                image
            }
        };
        controller.attach(spec.drive, image);
    }

    for (index, text) in hd_specs.iter().enumerate() {
        let name = format!("hd{}", index);
        let drive = parse_drive(&name).map_err(|_| "too many hard disks".to_string())?;
        let label = drive_label(drive);
        let (source, readonly) =
            parse_hd_source(text).map_err(|error| format!("{} in --hd '{}'", error, text))?;
        let mut image = match source {
            DiskSource::Memory(_) => DiskImage::new_in_memory_hard_disk(HD_DEFAULT_SIZE_MB),
            DiskSource::FilePath(path) if path.exists() => {
                let image = DiskImage::open_hard_disk(&path, readonly).map_err(|error| {
                    format!(
                        "cannot open hard disk image '{}' for {}: {}",
                        path.display(),
                        label,
                        error
                    )
                })?;
                if let Some(note) = image.capacity_note() {
                    eprintln!("Warning: {} ({}): {}", label, path.display(), note);
                }
                image
            }
            DiskSource::FilePath(path) => {
                let image =
                    DiskImage::create_hard_disk(&path, HD_DEFAULT_SIZE_MB).map_err(|error| {
                        format!(
                            "cannot create hard disk image '{}' for {}: {}",
                            path.display(),
                            label,
                            error
                        )
                    })?;
                eprintln!(
                    "{}: new blank {} MB image created: {}",
                    label,
                    HD_DEFAULT_SIZE_MB,
                    path.display()
                );
                image
            }
        };
        image.readonly = readonly;
        controller.attach(drive, image);
    }

    Ok(controller)
}

#[cfg(test)]
#[path = "disk_spec_tests.rs"]
mod tests;
