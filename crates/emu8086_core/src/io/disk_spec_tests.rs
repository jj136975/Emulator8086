use super::*;
use crate::io::disk::FixedClock;

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
        "", "e", "z", "hd", "hdx", "hd-1", "hd+1", "hd 1", "hd127", "hd128", "hd200", "hd256", "ab",
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
    let error = open_drives(vec![dir.spec("a", "nothere.img")], &[], &FixedClock)
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
    let mut controller = open_drives(specs, std::slice::from_ref(&hd), &FixedClock).unwrap();

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
    let mut controller = open_drives(
        vec![dir.spec("b", "new.img:ro")],
        &[format!("{}:ro", hd)],
        &FixedClock,
    )
    .unwrap();
    let floppy = controller.find_drive(0x01).unwrap();
    assert!(floppy.readonly);
    assert_eq!((floppy.cylinders, floppy.sectors_per_track), (80, 9));
    assert!(controller.find_drive(0x80).unwrap().readonly);
}

/// The disks a machine starts with are no change behind the back of a
/// guest: there is none yet, and its first bootstrap reads them all.
#[test]
fn the_drives_a_machine_starts_with_leave_no_reboot_due() {
    let controller = open_drives(
        vec![parse_disk_spec("a:memory").unwrap()],
        &["memory".to_string(), "memory".to_string()],
        &FixedClock,
    )
    .unwrap();
    assert_eq!(controller.drives.len(), 3);
    assert!(!controller.reboot_due());
}

/// One image file is one disk in one drive: a second drive on the same file
/// would cache and write it without knowing of the first.
#[test]
fn one_image_file_cannot_be_given_to_two_drives() {
    let dir = ScratchDir::new("shared");
    std::fs::write(dir.path("one.img"), vec![0u8; 737_280]).unwrap();

    let specs = vec![dir.spec("a", "one.img"), dir.spec("b", "one.img")];
    let error = open_drives(specs, &[], &FixedClock).err().unwrap();
    assert!(
        error.contains("one.img") && error.contains("A:"),
        "{}",
        error
    );

    let hd = dir.path("one.vhd").display().to_string();
    let error = open_drives(Vec::new(), &[hd.clone(), hd], &FixedClock)
        .err()
        .unwrap();
    assert!(
        error.contains("one.vhd") && error.contains("HD0"),
        "{}",
        error
    );
}

#[test]
fn a_directory_given_as_an_image_is_an_error_not_a_panic() {
    let dir = ScratchDir::new("dir-as-image");
    let error = open_drives(Vec::new(), &[dir.0.display().to_string()], &FixedClock)
        .err()
        .unwrap();
    assert!(error.contains("HD0"), "{}", error);
}
