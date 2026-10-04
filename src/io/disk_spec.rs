//! Parsing of what the user types to name drives and disk images. The
//! `--disk`, `--hd` and `--boot` options and the commands of the F12 monitor
//! share these functions, so both accept the same spellings.

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
mod tests {
    use super::*;

    fn file(path: &str) -> DiskSource {
        DiskSource::FilePath(PathBuf::from(path))
    }

    fn describe(spec: &str) -> Result<(u8, DiskSource, bool), String> {
        parse_disk_spec(spec).map(|spec| (spec.drive, spec.source, spec.readonly))
    }

    #[test]
    fn drive_names() {
        assert_eq!(parse_drive("a"), Ok(0x00));
        assert_eq!(parse_drive("D"), Ok(0x03));
        assert_eq!(parse_drive(" b "), Ok(0x01));
        assert_eq!(parse_drive("hd0"), Ok(0x80));
        assert_eq!(parse_drive("HD3"), Ok(0x83));
        assert_eq!(parse_drive("hd126"), Ok(0xFE));
        for bad in [
            "", "e", "z", "hd", "hdx", "hd-1", "hd+1", "hd 1", "hd127", "hd128", "hd200", "hd256",
            "ab",
        ] {
            assert!(parse_drive(bad).is_err(), "'{}' is not a drive", bad);
        }
        assert_eq!(
            parse_drive("x").unwrap_err(),
            "invalid drive 'x' (use a-d or hd0, hd1, ...)"
        );
    }

    #[test]
    fn drive_labels() {
        assert_eq!(drive_label(0x00), "A:");
        assert_eq!(drive_label(0x03), "D:");
        assert_eq!(drive_label(0x80), "HD0");
        assert_eq!(drive_label(0x81), "HD1");
    }

    #[test]
    fn floppy_sources() {
        assert_eq!(parse_source("disk.img"), Ok((file("disk.img"), false)));
        assert_eq!(parse_source("disk.img:ro"), Ok((file("disk.img"), true)));
        assert_eq!(parse_source("DISK.IMG:RO"), Ok((file("DISK.IMG"), true)));
        assert_eq!(parse_source("disk.img:rw"), Ok((file("disk.img"), false)));
        assert_eq!(
            parse_source("C:\\imgs\\a b.img:Ro"),
            Ok((file("C:\\imgs\\a b.img"), true))
        );
        assert_eq!(
            parse_source("memory"),
            Ok((DiskSource::Memory(1_474_560), false))
        );
        assert_eq!(
            parse_source("MEMORY:RO"),
            Ok((DiskSource::Memory(1_474_560), true))
        );
        assert_eq!(
            parse_source("memory:720"),
            Ok((DiskSource::Memory(737_280), false))
        );
        assert_eq!(
            parse_source("Memory:360:ro"),
            Ok((DiskSource::Memory(368_640), true))
        );
        // Only the exact keyword means an in-memory disk.
        assert_eq!(
            parse_source("memory_card.img"),
            Ok((file("memory_card.img"), false))
        );
        assert_eq!(parse_source("memory.img"), Ok((file("memory.img"), false)));
        for bad in [
            "memory:1000",
            "memory:abc",
            "memory:",
            "memory:0",
            "",
            ":ro",
        ] {
            assert!(parse_source(bad).is_err(), "'{}' must be rejected", bad);
        }
    }

    #[test]
    fn hard_disk_sources() {
        assert_eq!(parse_hd_source("dos.vhd"), Ok((file("dos.vhd"), false)));
        assert_eq!(parse_hd_source("dos.vhd:RO"), Ok((file("dos.vhd"), true)));
        assert_eq!(
            parse_hd_source("memory"),
            Ok((DiskSource::Memory(32 * 1024 * 1024), false))
        );
        assert!(parse_hd_source("memory:64").is_err());
        assert!(parse_hd_source("").is_err());
    }

    #[test]
    fn disk_specs() {
        assert_eq!(describe("a:boot.img"), Ok((0, file("boot.img"), false)));
        assert_eq!(describe("boot.img"), Ok((0, file("boot.img"), false)));
        assert_eq!(describe("B:data.img:ro"), Ok((1, file("data.img"), true)));
        assert_eq!(
            describe("d:memory:720"),
            Ok((3, DiskSource::Memory(737_280), false))
        );
        // A drive letter followed by a path separator is a Windows path.
        assert_eq!(
            describe("C:\\imgs\\dos.img"),
            Ok((0, file("C:\\imgs\\dos.img"), false))
        );
        assert_eq!(
            describe("C:/imgs/dos.img"),
            Ok((0, file("C:/imgs/dos.img"), false))
        );
        assert_eq!(
            describe("b:C:\\imgs\\dos.img:ro"),
            Ok((1, file("C:\\imgs\\dos.img"), true))
        );
    }

    /// Byte 7, where a `memory:` prefix would end, is not a character
    /// boundary in these names.
    #[test]
    fn disk_specs_with_non_ascii_paths() {
        assert_eq!(
            describe("a:discosñ.img"),
            Ok((0, file("discosñ.img"), false))
        );
        assert_eq!(
            describe("a:дискета.img"),
            Ok((0, file("дискета.img"), false))
        );
        assert_eq!(
            describe("b:文件夹/dos.img:ro"),
            Ok((1, file("文件夹/dos.img"), true))
        );
        assert_eq!(describe("ñ.img"), Ok((0, file("ñ.img"), false)));
    }

    #[test]
    fn disk_specs_that_are_rejected() {
        for bad in [
            "e:foo.img",
            "z:memory",
            "b:",
            "a:memory:1000",
            "a:memory:abc",
        ] {
            assert!(parse_disk_spec(bad).is_err(), "'{}' must be rejected", bad);
        }
        let error = parse_disk_spec("e:foo.img").unwrap_err();
        assert!(error.contains("'e'") && error.contains("--hd"), "{}", error);
    }

    #[test]
    fn a_drive_can_be_given_only_once() {
        let specs = |values: &[&str]| {
            parse_disk_specs(&values.iter().map(|v| v.to_string()).collect::<Vec<_>>())
        };
        assert_eq!(specs(&["a:one.img", "b:two.img"]).unwrap().len(), 2);
        let error = specs(&["a:one.img", "b:two.img", "two.img"]).unwrap_err();
        assert!(
            error.contains("A:") && error.contains("two.img"),
            "{}",
            error
        );
    }

    #[test]
    fn boot_orders() {
        assert_eq!(parse_boot_order("a,hd0"), Ok(vec![0x00, 0x80]));
        assert_eq!(parse_boot_order("HD1, a ,b"), Ok(vec![0x81, 0x00, 0x01]));
        for bad in ["", "a,", "hdx", "z", "hd0;a", "hd128", "hd200"] {
            assert!(parse_boot_order(bad).is_err(), "'{}' must be rejected", bad);
        }
    }

    /// A directory on the host that is removed again on drop.
    struct ScratchDir(PathBuf);

    impl ScratchDir {
        fn new(name: &str) -> Self {
            let path =
                std::env::temp_dir().join(format!("emu8086-spec-{}-{}", std::process::id(), name));
            let _ = std::fs::remove_dir_all(&path);
            std::fs::create_dir_all(&path).unwrap();
            Self(path)
        }

        fn path(&self, name: &str) -> PathBuf {
            self.0.join(name)
        }

        fn spec(&self, drive: &str, name: &str) -> DiskSpec {
            parse_disk_spec(&format!("{}:{}", drive, self.path(name).display())).unwrap()
        }
    }

    impl Drop for ScratchDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn missing_boot_floppy_is_an_error_that_names_the_file() {
        let dir = ScratchDir::new("missing-a");
        let error = open_drives(vec![dir.spec("a", "nothere.img")], &[])
            .err()
            .unwrap();
        assert!(
            error.contains("nothere.img") && error.contains("A:"),
            "{}",
            error
        );
        assert!(
            !dir.path("nothere.img").exists(),
            "nothing is created for A:"
        );
    }

    #[test]
    fn images_for_other_drives_are_created_when_missing() {
        let dir = ScratchDir::new("create");
        let hd = dir.path("new.vhd").display().to_string();
        let specs = vec![
            dir.spec("b", "new.img"),
            parse_disk_spec("a:memory").unwrap(),
        ];
        let mut controller = open_drives(specs, std::slice::from_ref(&hd)).unwrap();

        assert_eq!(
            std::fs::metadata(dir.path("new.img")).unwrap().len(),
            FLOPPY_144_SIZE
        );
        assert_eq!(
            std::fs::metadata(dir.path("new.vhd")).unwrap().len(),
            65 * 16 * 63 * 512
        );
        assert!(controller.find_drive(0x00).is_some());
        assert!(controller.find_drive(0x01).is_some());
        assert!(controller.find_drive(0x80).is_some());

        // The second time the files exist and are opened as they are.
        std::fs::write(dir.path("new.img"), vec![0x5Au8; 737_280]).unwrap();
        let mut controller =
            open_drives(vec![dir.spec("b", "new.img:ro")], &[format!("{}:ro", hd)]).unwrap();
        let floppy = controller.find_drive(0x01).unwrap();
        assert!(floppy.readonly);
        assert_eq!((floppy.cylinders, floppy.sectors_per_track), (80, 9));
        assert!(controller.find_drive(0x80).unwrap().readonly);
    }

    #[test]
    fn a_directory_given_as_an_image_is_an_error_not_a_panic() {
        let dir = ScratchDir::new("dir-as-image");
        let error = open_drives(Vec::new(), &[dir.0.display().to_string()])
            .err()
            .unwrap();
        assert!(error.contains("HD0"), "{}", error);
    }
}
