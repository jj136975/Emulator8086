//! The operations, applied to controllers the tests fill by hand. A disk is
//! held in memory unless the test is about its file; the files and
//! directories of the host live in the system temp directory and are removed
//! on drop.

use super::*;
use crate::io::disk::FixedClock;

/// Applies an operation that is to succeed, and returns what it says it
/// did.
fn did(disks: &mut DiskController, operation: Operation) -> String {
    match apply(disks, operation.clone(), &FixedClock).result {
        Ok(message) => message,
        Err(reason) => panic!("{operation:?} failed: {reason}"),
    }
}

/// Applies an operation that is to be refused, and returns the reason it
/// gives.
fn refusal(disks: &mut DiskController, operation: Operation) -> String {
    match apply(disks, operation.clone(), &FixedClock).result {
        Ok(message) => panic!("{operation:?} was carried out: {message}"),
        Err(reason) => reason,
    }
}

fn insert(drive: u8, disk: Disk) -> Operation {
    Operation::Insert {
        drive,
        disk,
        read_only: false,
    }
}

fn image_at(path: &Path) -> Disk {
    Disk::Image(HostFile::Path(path.to_path_buf()))
}

fn image_of(name: &str, bytes: Vec<u8>) -> Disk {
    Disk::Image(HostFile::Contents {
        name: name.to_string(),
        bytes,
    })
}

const BLANK: Disk = Disk::Blank { kb: None };

fn put(drive: u8, files: &[HostFile]) -> Operation {
    Operation::Put {
        drive,
        files: files.to_vec(),
    }
}

fn file_at(path: &Path) -> HostFile {
    HostFile::Path(path.to_path_buf())
}

fn file_of(name: &str, bytes: &[u8]) -> HostFile {
    HostFile::Contents {
        name: name.to_string(),
        bytes: bytes.to_vec(),
    }
}

fn formatted_floppy() -> DiskImage {
    let mut image = DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false);
    image.format_fat(&FixedClock).unwrap();
    image
}

fn formatted_hard_disk() -> DiskImage {
    let mut image = DiskImage::new_in_memory_hard_disk(1);
    image.format_fat(&FixedClock).unwrap();
    image
}

/// A controller as the bootstrap leaves it: a floppy in A:, a hard disk,
/// and no reboot due.
fn machine_with_floppy_and_hard_disk() -> DiskController {
    let mut disks = DiskController::new();
    disks.attach(0x00, formatted_floppy());
    disks.attach(0x80, formatted_hard_disk());
    disks.bootstrap_starts();
    disks
}

/// A path in the host's temp directory that does not exist yet. What is
/// made there, a file or a directory, is removed again on drop.
struct Scratch(PathBuf);

impl Scratch {
    fn missing(name: &str) -> Self {
        let path =
            std::env::temp_dir().join(format!("emu8086-tools-{}-{}", std::process::id(), name));
        let scratch = Self(path);
        scratch.remove();
        scratch
    }

    fn file(name: &str, content: &[u8]) -> Self {
        let scratch = Self::missing(name);
        std::fs::write(&scratch.0, content).unwrap();
        scratch
    }

    /// A directory holding `files`, each a path below it and its content.
    fn directory(name: &str, files: &[(&str, &[u8])]) -> Self {
        let scratch = Self::missing(name);
        for (relative, content) in files {
            let path = scratch.0.join(relative);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, content).unwrap();
        }
        scratch
    }

    fn remove(&self) {
        let _ = std::fs::remove_file(&self.0);
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        self.remove();
    }
}

// ---- eject, move ---------------------------------------------------------

/// An eject takes the disk out and a move puts it into the other drive,
/// and each says what it did. A drive without a disk has none to eject or
/// to move, and says that it is empty.
#[test]
fn eject_and_move_report_what_they_did() {
    let mut disks = DiskController::new();
    disks.attach(0x00, formatted_floppy());

    assert_eq!(
        did(
            &mut disks,
            Operation::Move {
                from: 0x00,
                to: 0x01
            }
        ),
        "Moved A: -> B:"
    );
    assert!(disks.find_drive(0x00).is_none());
    assert_eq!(
        refusal(
            &mut disks,
            Operation::Move {
                from: 0x00,
                to: 0x02
            }
        ),
        "A: is empty"
    );
    assert_eq!(
        did(&mut disks, Operation::Eject { drive: 0x01 }),
        "B: ejected"
    );
    assert_eq!(
        refusal(&mut disks, Operation::Eject { drive: 0x01 }),
        "B: is empty"
    );
    assert!(disks.drives.is_empty());
}

/// A move puts the disk into the other drive as it is, in place of the disk
/// that was there.
#[test]
fn a_moved_disk_takes_the_place_of_the_one_in_its_new_drive() {
    let mut disks = DiskController::new();
    disks.attach(0x00, DiskImage::new_in_memory_sized(737_280, true));
    disks.attach(0x01, formatted_floppy());

    did(
        &mut disks,
        Operation::Move {
            from: 0x00,
            to: 0x01,
        },
    );
    assert_eq!(disks.drives.len(), 1);
    let moved = disks.find_drive(0x01).unwrap();
    assert_eq!(moved.total_bytes(), 737_280);
    assert!(moved.readonly);
}

/// The drives are 00h to 03h and 80h to FEh. An operation on any other
/// number is refused, and the controller is not handed a drive it would
/// make of it.
#[test]
fn a_number_that_is_no_drive_is_refused() {
    let host_file = Scratch::file("no-drive.txt", b"hello");
    let new_image = Scratch::missing("no-drive.img");
    for number in [0x04, 0x7F, 0xFF] {
        let mut disks = machine_with_floppy_and_hard_disk();
        for operation in [
            insert(number, BLANK),
            insert(number, image_of("a.img", vec![0u8; 737_280])),
            Operation::Create {
                drive: number,
                path: new_image.0.clone(),
            },
            Operation::Move {
                from: 0x00,
                to: number,
            },
            Operation::Move {
                from: number,
                to: 0x01,
            },
            Operation::Save {
                drive: number,
                path: new_image.0.clone(),
            },
            Operation::Eject { drive: number },
            put(number, &[file_at(&host_file.0)]),
        ] {
            let outcome = apply(&mut disks, operation.clone(), &FixedClock);
            let reason = outcome.result.expect_err("no such drive");
            assert!(
                reason.contains("is not the number of a drive"),
                "{operation:?}: {reason}"
            );
            assert!(!outcome.needs_reboot, "{operation:?}");
        }
        assert!(contents(&mut disks, number).is_err());
        assert_eq!(disks.drives.len(), 2, "no drive {number:#04X} was made");
        assert!(disks.find_drive(0x00).is_some(), "A: is not touched");
        assert!(!new_image.0.exists());
    }
}

// ---- insert --------------------------------------------------------------

/// A wrong path must neither create a blank image under that name nor take
/// the disk out of the drive.
#[test]
fn inserting_does_not_create_a_missing_image() {
    let missing = Scratch::missing("typo.img");
    let mut disks = machine_with_floppy_and_hard_disk();

    for drive in [0x00, 0x80] {
        let reason = refusal(&mut disks, insert(drive, image_at(&missing.0)));
        assert!(
            reason.contains("not found: inserting an image creates none"),
            "{reason}"
        );
    }
    assert!(!missing.0.exists(), "no file may be created");
    for drive in [0x00, 0x80] {
        let disk = disks.find_drive(drive).expect("the old disk stays");
        assert!(disk.backing_path().is_none());
    }
}

/// An image that comes by its path is the disk itself: it lives in that
/// file, whose name may hold blanks, and is write-protected if asked.
#[test]
fn inserting_an_image_file_makes_that_file_the_disk() {
    let image = Scratch::file("two words.img", &vec![0u8; 737_280]);

    for (drive, read_only) in [(0x00, false), (0x01, true), (0x03, false)] {
        let mut disks = DiskController::new();
        let message = did(
            &mut disks,
            Operation::Insert {
                drive,
                disk: image_at(&image.0),
                read_only,
            },
        );
        let label = drive_label(drive);
        assert_eq!(message, format!("{label} now holds {}", image.0.display()));
        let disk = disks.find_drive(drive).expect("the image is inserted");
        assert_eq!(disk.total_bytes(), 737_280);
        assert_eq!(disk.readonly, read_only);
        assert!(disk.backing_path().is_some());
        assert_eq!(disks.drive_backed_by(&image.0), Some(drive));
    }

    // In a hard disk drive the same file is a hard disk: whole cylinders
    // of 16 heads and 63 sectors, one here.
    let mut disks = DiskController::new();
    did(&mut disks, insert(0x80, image_at(&image.0)));
    let disk = disks.find_drive(0x80).expect("the image is inserted");
    assert_eq!(
        (disk.cylinders, disk.heads, disk.sectors_per_track),
        (1, 16, 63)
    );
    assert_eq!(disks.drive_backed_by(&image.0), Some(0x80));
}

/// One image file is one disk in one drive: a second drive on the same file
/// would cache and write it without knowing of the first. Putting the file
/// back into the drive that holds it is a disk change like any other.
#[test]
fn an_image_file_that_is_in_another_drive_is_refused() {
    let image = Scratch::file("shared.img", &vec![0u8; 737_280]);
    let mut disks = DiskController::new();

    did(&mut disks, insert(0x00, image_at(&image.0)));
    let reason = refusal(&mut disks, insert(0x01, image_at(&image.0)));
    assert_eq!(
        reason,
        format!(
            "'{}' cannot be in two drives: it is already the disk in A:",
            image.0.display()
        )
    );
    assert!(disks.find_drive(0x01).is_none(), "B: stays empty");
    did(&mut disks, insert(0x00, image_at(&image.0)));
    assert_eq!(disks.drives.len(), 1);
}

/// CHS addresses end at 1024 cylinders, which is 504 MB of a hard disk.
/// A larger image is cut down to that, and the user is told so.
#[test]
fn a_hard_disk_image_the_guest_cannot_reach_in_full_is_reported() {
    let large = Scratch::missing("large.vhd");
    let small = Scratch::file("small.vhd", &vec![0u8; 4 * 16 * 63 * 512]);
    // 1030 cylinders. The file is only given its size, nothing is
    // written to it; without room for it there is nothing to test.
    let sized = std::fs::File::create(&large.0).and_then(|file| file.set_len(1030 * 16 * 63 * 512));
    if let Err(error) = sized {
        eprintln!("skipped: cannot make a scratch file of 506 MB: {}", error);
        return;
    }
    let mut disks = DiskController::new();

    let message = did(&mut disks, insert(0x80, image_at(&large.0)));
    assert!(
        message.starts_with(&format!("HD0 now holds {}. Warning: ", large.0.display())),
        "{message}"
    );
    assert!(
        message.contains("1024 cylinders")
            && message.contains("506 MB")
            && message.contains("504 MB"),
        "{message}"
    );
    assert_eq!(
        did(&mut disks, insert(0x81, image_at(&small.0))),
        format!("HD1 now holds {}", small.0.display())
    );
    assert_eq!(
        disks.find_drive(0x80).expect("still inserted").cylinders,
        1024
    );
    assert_eq!(disks.find_drive(0x81).expect("inserted").cylinders, 4);
}

/// An image that comes as its contents is a disk held in memory, known by
/// the name of the file it came from: a floppy by its size or its boot
/// sector, a hard disk by its size.
#[test]
fn an_image_handed_over_as_its_contents_is_a_disk_in_memory() {
    let mut disks = DiskController::new();

    assert_eq!(
        did(
            &mut disks,
            insert(0x00, image_of("dos disk 1.img", vec![0u8; 737_280]))
        ),
        "A: now holds dos disk 1.img, in memory"
    );
    let floppy = disks.find_drive(0x00).unwrap();
    assert_eq!(
        (floppy.cylinders, floppy.heads, floppy.sectors_per_track),
        (80, 2, 9)
    );
    assert_eq!(floppy.name(), Some("dos disk 1.img"));
    assert!(floppy.backing_path().is_none() && !floppy.readonly);

    let hard_disk = Operation::Insert {
        drive: 0x80,
        disk: image_of("c.vhd", vec![0u8; 4 * 16 * 63 * 512]),
        read_only: true,
    };
    assert_eq!(did(&mut disks, hard_disk), "HD0 now holds c.vhd, in memory");
    let hard_disk = disks.find_drive(0x80).unwrap();
    assert_eq!(
        (
            hard_disk.cylinders,
            hard_disk.heads,
            hard_disk.sectors_per_track
        ),
        (4, 16, 63)
    );
    assert!(hard_disk.readonly);
}

/// An image of no bytes is no disk, whether it comes by its path or as its
/// contents, for a floppy drive as for a hard disk drive: inserting one is
/// refused, and the drive keeps the disk it holds, unchanged.
#[test]
fn an_image_of_no_bytes_is_refused() {
    let empty = Scratch::file("empty.img", &[]);
    let mut disks = machine_with_floppy_and_hard_disk();

    for drive in [0x00, 0x80] {
        assert_eq!(
            refusal(&mut disks, insert(drive, image_at(&empty.0))),
            format!(
                "'{}' is empty: an image of no bytes is no disk",
                empty.0.display()
            )
        );
        let handed_over = insert(drive, image_of("nothing.img", Vec::new()));
        let outcome = apply(&mut disks, handed_over, &FixedClock);
        assert_eq!(
            outcome.result,
            Err("nothing.img is empty: an image of no bytes is no disk".to_string())
        );
        assert!(!outcome.needs_reboot);
    }
    assert_eq!(disks.drives.len(), 2);
    assert_eq!(
        disks.find_drive(0x00).unwrap().total_bytes(),
        FLOPPY_144_SIZE
    );
    let hard_disk = disks.find_drive(0x80).unwrap();
    assert!(hard_disk.name().is_none() && hard_disk.backing_path().is_none());
    assert!(!disks.reboot_due(), "no hard disk was changed");
}

/// A blank disk is a formatted floppy of a standard size in a floppy drive,
/// 1.44 MB unless another is asked for, and a blank disk of the one size
/// there is in a hard disk drive.
#[test]
fn a_blank_disk_is_a_formatted_floppy_or_a_hard_disk_of_the_default_size() {
    let mut disks = DiskController::new();

    let reason = refusal(&mut disks, insert(0x00, Disk::Blank { kb: Some(1000) }));
    assert_eq!(
        reason,
        "1000 KB is no floppy size (the sizes in KB: 160, 180, 320, 360, 720, 1200, 1440, 2880)"
    );
    let reason = refusal(&mut disks, insert(0x80, Disk::Blank { kb: Some(64) }));
    assert!(reason.contains("one size, 32 MB"), "{reason}");
    assert!(disks.drives.is_empty());

    assert_eq!(
        did(&mut disks, insert(0x00, Disk::Blank { kb: Some(720) })),
        "A: now holds a formatted 720KB in-memory floppy"
    );
    assert_eq!(disks.find_drive(0x00).unwrap().total_bytes(), 737_280);
    // Formatted: it takes a file as it is.
    did(&mut disks, put(0x00, &[file_of("HELLO.TXT", b"hello")]));

    let protected = Operation::Insert {
        drive: 0x01,
        disk: BLANK,
        read_only: true,
    };
    assert_eq!(
        did(&mut disks, protected),
        "B: now holds a formatted 1440KB in-memory floppy"
    );
    let floppy = disks.find_drive(0x01).unwrap();
    assert_eq!(floppy.total_bytes(), 1_474_560);
    assert!(floppy.readonly);

    assert_eq!(
        did(&mut disks, insert(0x80, BLANK)),
        "HD0 now holds a blank 32MB in-memory hard disk"
    );
    assert_eq!(
        disks.find_drive(0x80).unwrap().total_bytes(),
        65 * 16 * 63 * 512
    );

    // Every standard size makes a floppy of that size.
    for kb in FLOPPY_SIZES_KB {
        did(&mut disks, insert(0x02, Disk::Blank { kb: Some(kb) }));
        assert_eq!(disks.find_drive(0x02).unwrap().total_bytes(), kb * 1024);
    }
}

// ---- create ----------------------------------------------------------------

/// A new image file, a formatted 1.44 MB floppy or a blank hard disk of the
/// default size, is created where asked and put into the drive at once. A
/// file that is there already is never replaced, whichever drive asks.
#[test]
fn creating_an_image_file_inserts_it_and_never_replaces_a_file() {
    let floppy = Scratch::missing("new floppy.img");
    let hard_disk = Scratch::missing("new.vhd");
    let mut disks = DiskController::new();
    let create = |drive: u8, scratch: &Scratch| Operation::Create {
        drive,
        path: scratch.0.clone(),
    };

    assert_eq!(
        did(&mut disks, create(0x00, &floppy)),
        format!(
            "A: now holds the new formatted 1440KB image {}",
            floppy.0.display()
        )
    );
    assert_eq!(
        did(&mut disks, create(0x80, &hard_disk)),
        format!(
            "HD0 now holds the new blank 32MB image {}",
            hard_disk.0.display()
        )
    );
    assert_eq!(std::fs::metadata(&floppy.0).unwrap().len(), FLOPPY_144_SIZE);
    assert_eq!(
        std::fs::metadata(&hard_disk.0).unwrap().len(),
        65 * 16 * 63 * 512
    );

    // The new floppy is formatted: a file can be put on it right away.
    let host_file = Scratch::file("hello.txt", b"hello");
    let message = did(&mut disks, put(0x00, &[file_at(&host_file.0)]));
    assert!(message.contains("-> A:\\"), "{message}");

    // An existing file is left alone, whichever drive it is asked for.
    disks.detach(0x00);
    let before = std::fs::read(&floppy.0).unwrap();
    for drive in [0x01, 0x81] {
        let reason = refusal(&mut disks, create(drive, &floppy));
        assert!(
            reason.starts_with("Cannot create") && reason.contains("never replaces a file"),
            "{reason}"
        );
        assert!(disks.find_drive(drive).is_none());
    }
    assert_eq!(std::fs::read(&floppy.0).unwrap(), before);
}

// ---- save, contents --------------------------------------------------------

/// A save writes the whole disk to the file it names, and only flushes the
/// disk's own file, which holds every write of the guest already. The image
/// file of another drive is refused: it would be replaced under that drive.
#[test]
fn save_writes_the_image_and_recognises_its_own_file() {
    let target = Scratch::missing("saved disk.img");
    let mut disks = DiskController::new();
    disks.attach(0x00, formatted_floppy());
    let save = |drive: u8| Operation::Save {
        drive,
        path: target.0.clone(),
    };

    assert_eq!(refusal(&mut disks, save(0x02)), "C: is empty");
    assert!(!target.0.exists());
    assert_eq!(
        did(&mut disks, save(0x00)),
        format!("A: saved to {}", target.0.display())
    );
    assert_eq!(std::fs::metadata(&target.0).unwrap().len(), FLOPPY_144_SIZE);

    did(&mut disks, insert(0x01, image_at(&target.0)));
    let own = did(&mut disks, save(0x01));
    assert!(own.starts_with("B: already lives in"), "{own}");
    let others = refusal(&mut disks, save(0x00));
    assert_eq!(
        others,
        format!(
            "{} is the image file of B:, which has to be ejected first",
            target.0.display()
        )
    );
    assert_eq!(std::fs::metadata(&target.0).unwrap().len(), FLOPPY_144_SIZE);
}

/// A disk is handed back whole as the bytes a save would write, whether it
/// lives in memory or in a file, and nothing is changed by it.
#[test]
fn the_contents_of_a_disk_are_what_a_save_writes() {
    let saved = Scratch::missing("contents.img");
    let own = Scratch::file("contents-own.img", &vec![0x5Au8; 737_280]);
    let mut disks = machine_with_floppy_and_hard_disk();
    did(&mut disks, insert(0x01, image_at(&own.0)));
    did(&mut disks, put(0x00, &[file_of("HELLO.TXT", b"hello")]));

    let bytes = contents(&mut disks, 0x00).unwrap();
    did(
        &mut disks,
        Operation::Save {
            drive: 0x00,
            path: saved.0.clone(),
        },
    );
    assert_eq!(bytes.len() as u64, FLOPPY_144_SIZE);
    assert!(bytes == std::fs::read(&saved.0).unwrap());
    assert!(contents(&mut disks, 0x01).unwrap() == vec![0x5Au8; 737_280]);
    assert_eq!(
        contents(&mut disks, 0x80).unwrap().len() as u64,
        disks.find_drive(0x80).unwrap().total_bytes()
    );

    assert_eq!(contents(&mut disks, 0x02), Err("C: is empty".to_string()));
    assert!(!disks.reboot_due(), "reading a hard disk changes nothing");
}

// ---- put -------------------------------------------------------------------

/// A file is copied from its path, whose name may hold blanks, or from its
/// contents, under its own name.
#[test]
fn put_copies_a_file_that_comes_by_its_path_or_as_its_contents() {
    let host_file = Scratch::file("HELLO .TXT", b"hello");
    let mut disks = DiskController::new();
    disks.attach(0x00, formatted_floppy());

    let message = did(&mut disks, put(0x00, &[file_at(&host_file.0)]));
    assert!(
        message.starts_with(&format!("Copied '{}' -> A:\\", host_file.0.display()))
            && message.ends_with("(5 bytes)"),
        "{message}"
    );
    assert_eq!(
        did(&mut disks, put(0x00, &[file_of("WORLD.TXT", b"world!")])),
        "Copied 'WORLD.TXT' -> A:\\WORLD.TXT (6 bytes)"
    );

    let missing = Scratch::missing("no such file.txt");
    assert_eq!(
        refusal(&mut disks, put(0x00, &[file_at(&missing.0)])),
        format!("'{}' not found", missing.0.display())
    );
    assert_eq!(
        refusal(&mut disks, put(0x01, &[file_at(&host_file.0)])),
        "B: is empty"
    );
    assert_eq!(refusal(&mut disks, put(0x00, &[])), "No file to copy");
}

/// What a copy reports is the name the file has in DOS: one generated for a
/// name that is no valid 8.3 name, the name as DOS spells it otherwise. A
/// write-protected disk refuses the copy, and the reason says so.
#[test]
fn put_shows_the_name_the_file_has_in_dos() {
    let host_file = Scratch::file("longname.txt", b"hello");
    let mut disks = DiskController::new();
    disks.attach(0x00, formatted_floppy());

    // The host name is not a valid 8.3 name, so DOS sees a generated one.
    let message = did(&mut disks, put(0x00, &[file_at(&host_file.0)]));
    assert!(
        message.ends_with("-> A:\\EMU808~1.TXT (5 bytes)"),
        "{message}"
    );

    // A name that is one is stored as DOS spells it.
    assert_eq!(
        did(&mut disks, put(0x00, &[file_of("hello.txt", b"hello")])),
        "Copied 'hello.txt' -> A:\\HELLO.TXT (5 bytes)"
    );

    disks.find_drive(0x00).unwrap().readonly = true;
    let reason = refusal(&mut disks, put(0x00, &[file_of("other.txt", b"hello")]));
    assert_eq!(reason, "'other.txt': disk is write-protected");
}

/// A directory is copied with all that is below it, and several files in
/// one go; the report counts the files that are on the disk.
#[test]
fn put_copies_a_directory_and_several_files_at_once() {
    let tree = Scratch::directory(
        "tree",
        &[
            ("README.TXT", b"read me"),
            ("SUB/ONE.TXT", b"1"),
            ("SUB/TWO.TXT", b"2"),
        ],
    );
    let mut disks = DiskController::new();
    disks.attach(0x00, formatted_floppy());

    assert_eq!(
        did(&mut disks, put(0x00, &[file_at(&tree.0)])),
        format!("Copied 3 files from '{}' -> A:\\", tree.0.display())
    );
    assert_eq!(
        did(
            &mut disks,
            put(
                0x00,
                &[
                    file_of("A.TXT", b"a"),
                    file_at(&tree.0),
                    file_of("B.TXT", b"b")
                ]
            )
        ),
        "Copied 5 files -> A:\\"
    );
}

/// A copy of several files stops at the first that fails, and says which
/// one it was and how many are on the disk already.
#[test]
fn put_stops_at_the_first_file_that_fails() {
    let mut disks = DiskController::new();
    disks.attach(0x00, formatted_floppy());
    let files = [
        file_of("ONE.TXT", b"1"),
        file_of("TWO.TXT", b"2"),
        // DOS has a device of this name: no file can be called so.
        file_of("NUL", b"3"),
        file_of("FOUR.TXT", b"4"),
    ];

    let reason = refusal(&mut disks, put(0x00, &files));
    assert!(
        reason.starts_with("'NUL': ") && reason.ends_with("(2 files were copied before it)"),
        "{reason}"
    );
    let reason = refusal(&mut disks, put(0x00, &files[1..3]));
    assert!(
        reason.ends_with("(1 file was copied before it)"),
        "{reason}"
    );
    let reason = refusal(&mut disks, put(0x00, &files[2..]));
    assert!(
        reason.starts_with("'NUL': ") && !reason.contains("copied before"),
        "{reason}"
    );
}

// ---- the reboot a hard disk needs --------------------------------------------

/// DOS never re-reads a fixed disk: whatever the tools change on one is
/// invisible to a running guest and is overwritten from its buffers. Every
/// operation that changes one says that the guest must be rebooted, and the
/// controller holds the reboot due.
#[test]
fn changing_a_hard_disk_asks_for_a_reboot() {
    let host_file = Scratch::file("warn.txt", b"hello");
    let new_image = Scratch::missing("warn.vhd");
    for operation in [
        Operation::Eject { drive: 0x80 },
        insert(0x80, BLANK),
        insert(0x81, image_of("d.vhd", vec![0u8; 516_096])),
        Operation::Move {
            from: 0x80,
            to: 0x81,
        },
        Operation::Move {
            from: 0x00,
            to: 0x80,
        },
        Operation::Move {
            from: 0x80,
            to: 0x01,
        },
        put(0x80, &[file_at(&host_file.0)]),
        put(0x80, &[file_of("WARN.TXT", b"hello")]),
        // A copy that failed after it began has changed the disk too.
        put(0x80, &[file_of("ONE.TXT", b"1"), file_of("NUL", b"2")]),
        Operation::Create {
            drive: 0x81,
            path: new_image.0.clone(),
        },
    ] {
        let mut disks = machine_with_floppy_and_hard_disk();
        let outcome = apply(&mut disks, operation.clone(), &FixedClock);
        assert!(outcome.needs_reboot, "{operation:?}: {outcome:?}");
        assert!(disks.reboot_due(), "{operation:?}");
        drop(disks);
        new_image.remove();
    }
}

/// A floppy changed from the host asks for no reboot: its change line tells
/// the guest. Nor does an operation refused before it touched a disk, a
/// save, which only reads one, or a copy that a write-protected hard disk
/// refused.
#[test]
fn floppy_changes_and_refused_operations_ask_for_no_reboot() {
    let host_file = Scratch::file("quiet.txt", b"hello");
    let missing = Scratch::missing("no-such-image.vhd");
    let target = Scratch::missing("quiet-save.vhd");
    for operation in [
        Operation::Eject { drive: 0x00 },
        insert(0x00, BLANK),
        Operation::Move {
            from: 0x00,
            to: 0x01,
        },
        put(0x00, &[file_at(&host_file.0)]),
        // Refused: nothing was changed.
        Operation::Eject { drive: 0x81 },
        insert(0x80, image_at(&missing.0)),
        insert(0x80, Disk::Blank { kb: Some(64) }),
        Operation::Move {
            from: 0x81,
            to: 0x80,
        },
        put(0x81, &[file_at(&host_file.0)]),
        put(0x80, &[file_at(&missing.0)]),
        put(0x80, &[]),
        // A save reads the disk and changes nothing.
        Operation::Save {
            drive: 0x80,
            path: target.0.clone(),
        },
    ] {
        let mut disks = machine_with_floppy_and_hard_disk();
        let outcome = apply(&mut disks, operation.clone(), &FixedClock);
        assert!(!outcome.needs_reboot, "{operation:?}: {outcome:?}");
        assert!(!disks.reboot_due(), "{operation:?}");
    }

    // A write-protected hard disk is not changed by a `Put` it refuses.
    let mut disks = machine_with_floppy_and_hard_disk();
    disks.find_drive(0x80).unwrap().readonly = true;
    let outcome = apply(&mut disks, put(0x80, &[file_at(&host_file.0)]), &FixedClock);
    let reason = outcome.result.expect_err("write-protected");
    assert!(reason.ends_with("disk is write-protected"), "{reason}");
    assert!(!outcome.needs_reboot && !disks.reboot_due());
}

/// What the tools say of a file reads the same in a failed assertion as in
/// a log: the contents are megabytes, and are shown by their length.
#[test]
fn a_host_file_is_shown_without_its_contents() {
    let shown = format!("{:?}", insert(0x00, image_of("a.img", vec![0u8; 737_280])));
    assert_eq!(
        shown,
        "Insert { drive: 0, disk: Image(Contents(\"a.img\", 737280 bytes)), read_only: false }"
    );
}
