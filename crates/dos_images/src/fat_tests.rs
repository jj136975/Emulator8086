//! FAT volumes held in memory, and host directories in the system's
//! temporary directory, removed when a test ends.

use super::*;
use crate::stamp;
use std::path::PathBuf;

/// A 1.44 MB floppy, formatted.
fn floppy() -> Vec<u8> {
    let mut bytes = vec![0; 1_474_560];
    let options = fatfs::FormatVolumeOptions::new()
        .fat_type(fatfs::FatType::Fat12)
        .total_sectors(2880);
    fatfs::format_volume(Cursor::new(&mut bytes[..]), options).unwrap();
    bytes
}

/// A directory of the host's, below the temporary one, removed on drop.
struct HostDirectory(PathBuf);

impl HostDirectory {
    fn new(name: &str) -> Self {
        let path = std::env::temp_dir().join(format!("dos_images-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&path);
        std::fs::create_dir_all(&path).unwrap();
        Self(path)
    }
}

impl Drop for HostDirectory {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// A name DOS can have is given in capitals; a base longer than eight
/// characters, an extension longer than three, a second dot, a blank or a
/// character a short name cannot hold make a name DOS cannot have.
#[test]
fn dos_names_are_short_names_in_capitals() {
    for (name, dos) in [
        ("io.sys", "IO.SYS"),
        ("MAKEFILE", "MAKEFILE"),
        ("4201.CPI", "4201.CPI"),
        ("A_B-C~1.X$", "A_B-C~1.X$"),
    ] {
        assert_eq!(dos_name(name).as_deref(), Some(dos), "{name}");
    }
    for name in [
        "LONGNAME9.TXT",
        "A.TEXT",
        "A.B.C",
        "A B.TXT",
        "A+B.TXT",
        "",
        ".",
        "FOO.",
        "ÉTÉ.TXT",
    ] {
        assert_eq!(dos_name(name), None, "{name}");
    }
}

/// Text as DOS reads it ends each line in CR LF: a line that ends in LF
/// alone is given the CR, and one that ends in CR LF already, or a CR
/// alone, is left as it is.
#[test]
fn dos_text_ends_every_line_in_cr_lf() {
    assert_eq!(dos_text(b"a\nb\r\nc\rd\n"), b"a\r\nb\r\nc\rd\r\n");
    assert_eq!(dos_text(b""), b"");
}

/// What is put on a volume is taken off it as it was, stamped with the
/// stamp of the mount; the names of the files of a directory leave its
/// directories out.
#[test]
fn a_file_put_on_a_volume_is_stamped_with_the_mounts_stamp() {
    let mut bytes = floppy();
    let volume = mount(&mut bytes, &stamp::MSDOS).unwrap();
    let root = volume.root_dir();
    put(&root, "HELLO.TXT", b"hello").unwrap();
    put(&root, "HELLO.TXT", b"again").unwrap();
    root.create_dir("SUB").unwrap();

    assert_eq!(get(&root, "HELLO.TXT").unwrap(), b"again");
    assert_eq!(file_names(&root).unwrap(), ["HELLO.TXT"]);
    let entry = root.iter().next().unwrap().unwrap();
    let modified = entry.modified();
    assert_eq!(
        (modified.date.year, modified.date.month, modified.date.day),
        (1988, 10, 6)
    );
    assert_eq!(
        (modified.time.hour, modified.time.min, modified.time.sec),
        (0, 0, 0)
    );
}

/// A host directory is copied in the order of its names, whatever order
/// the host lists them in, with the names in capitals and its directories
/// below it; a name DOS cannot have stops the copy, naming the file.
#[test]
fn a_host_directory_is_copied_in_the_order_of_its_names() {
    let host = HostDirectory::new("tree");
    std::fs::write(host.0.join("b.txt"), b"b").unwrap();
    std::fs::write(host.0.join("A.TXT"), b"a").unwrap();
    std::fs::create_dir(host.0.join("sub")).unwrap();
    std::fs::write(host.0.join("sub").join("c.dat"), b"c").unwrap();

    let mut bytes = floppy();
    let volume = mount(&mut bytes, &stamp::MSDOS).unwrap();
    let root = volume.root_dir();
    assert_eq!(put_tree(&root, &host.0).unwrap(), 3);
    let names: Vec<String> = root
        .iter()
        .map(|entry| short_name(&entry.unwrap()))
        .collect();
    assert_eq!(names, ["A.TXT", "B.TXT", "SUB"]);
    assert_eq!(get(&root, "SUB/C.DAT").unwrap(), b"c");

    std::fs::write(host.0.join("sub").join("longfilename.txt"), b"x").unwrap();
    let mut bytes = floppy();
    let volume = mount(&mut bytes, &stamp::MSDOS).unwrap();
    match put_tree(&volume.root_dir(), &host.0) {
        Err(TreeError::NotDosName(path)) => assert!(path.ends_with("longfilename.txt")),
        other => panic!("{other:?}"),
    }
}
