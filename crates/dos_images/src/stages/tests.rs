//! The stages that write into the output directory, run on what a test
//! makes by hand: a build disk as a build leaves one, and what a build
//! made, of files that are no programs, with a boot sector and a master
//! boot record that boot nothing; and the reading of the inputs before
//! them.

use super::*;
use crate::fat;
use crate::layout::{HardDisk, LayoutError, SECTOR};
use crate::stamp;

/// A directory below the system's temporary one, removed on drop: an output
/// directory as `run` makes it, and the inputs a disk takes beside it.
struct Scratch(PathBuf);

impl Scratch {
    fn new(name: &str) -> Self {
        let path =
            std::env::temp_dir().join(format!("dos_images-stages-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&path);
        let scratch = Self(path);
        for directory in [
            scratch.out().join("disks"),
            scratch.out().join(BUILT_FILES),
            scratch.0.join("msdos"),
            scratch.0.join("doom8088"),
        ] {
            std::fs::create_dir_all(directory).unwrap();
        }
        std::fs::write(scratch.0.join("msdos").join("LICENSE"), "MIT\n").unwrap();
        for name in ["D8M13H.EXE", "DOOM1.WAD", "README.TXT", "COPYING"] {
            std::fs::write(scratch.0.join("doom8088").join(name), name).unwrap();
        }
        scratch
    }

    fn out(&self) -> PathBuf {
        self.0.join("out")
    }

    fn inputs(&self) -> Inputs {
        Inputs {
            freedos: self.0.join("freedos"),
            msdos: self.0.join("msdos"),
            doom8088: self.0.join("doom8088"),
        }
    }

    /// The names of the files in the directory `directory` of the output,
    /// in their order.
    fn written(&self, directory: &str) -> Vec<String> {
        let mut names: Vec<String> = std::fs::read_dir(self.out().join(directory))
            .unwrap()
            .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        names
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// An image of `length` bytes whose last sector ends in the boot signature.
fn signed(length: usize) -> Vec<u8> {
    let mut image = vec![0; length];
    image[length - 2..].copy_from_slice(&[0x55, 0xAA]);
    image
}

/// A BUILD.LOG of no error.
const CLEAN_LOG: &[u8] = b"===== MESSAGES \r\n";

/// A build disk as a build leaves it: `log` as BUILD.LOG, a CPY.BAT that
/// copies IO.SYS and FIND.EXE, both in C:\BIN, and `msboot_bin` and
/// `fdboot_bin` as the images of the boot sector and of FDISK's master boot
/// record.
fn build_disk(log: &[u8], msboot_bin: &[u8], fdboot_bin: &[u8]) -> Vec<u8> {
    let mut disk = HardDisk::new(1, 0x01, None);
    let sectors = disk.volume().len() / SECTOR;
    let options = fatfs::FormatVolumeOptions::new()
        .fat_type(fatfs::FatType::Fat12)
        .total_sectors(sectors as u32);
    disk.format(options).unwrap();
    let volume = fat::mount(disk.volume(), &stamp::MSDOS).unwrap();
    {
        let root = volume.root_dir();
        fat::put(&root, "BUILD.LOG", log).unwrap();
        let src = root.create_dir("SRC").unwrap();
        let cpy = b"copy .\\bios\\io.sys %1\r\ncopy .\\cmd\\find\\find.exe %1\r\n";
        fat::put(&src, "CPY.BAT", cpy).unwrap();
        fat::put(&src.create_dir("BOOT").unwrap(), "MSBOOT.BIN", msboot_bin).unwrap();
        let fdisk = src.create_dir("CMD").unwrap().create_dir("FDISK").unwrap();
        fat::put(&fdisk, "FDBOOT.BIN", fdboot_bin).unwrap();
        let bin = root.create_dir("BIN").unwrap();
        fat::put(&bin, "IO.SYS", b"IO").unwrap();
        fat::put(&bin, "FIND.EXE", b"FIND").unwrap();
    }
    volume.unmount().unwrap();
    disk.into_bytes()
}

/// What a build made, as the disks take it: the system, the programs of the
/// hard disk and a file of the floppy's alone, of bytes that are no
/// programs; a boot sector of zeros but for its signature, which the BIOS
/// does not take for code on a floppy (its first byte is below 06h); and
/// `master_boot_record`.
fn built(master_boot_record: [u8; SECTOR]) -> Built {
    let names = [
        "IO.SYS",
        "MSDOS.SYS",
        "COMMAND.COM",
        "MEM.EXE",
        "CHKDSK.COM",
        "FIND.EXE",
    ];
    Built {
        files: names
            .map(|name| (name.to_string(), name.as_bytes().to_vec()))
            .to_vec(),
        boot_sector: signed(SECTOR).try_into().unwrap(),
        master_boot_record,
        log_bytes: 0,
        warnings: 0,
    }
}

/// A run begins by removing the outputs an earlier run left: the two disks,
/// every file of `bin`, the report and a failed build's disk. What else the
/// output directory holds is left alone.
#[test]
fn a_run_removes_the_outputs_of_an_earlier_run_first() {
    let scratch = Scratch::new("clear");
    let out = scratch.out();
    for name in [FLOPPY_IMAGE, HARD_DISK_IMAGE, REPORT, FAILED_BUILD_DISK] {
        std::fs::write(out.join(name), b"earlier").unwrap();
    }
    for name in ["IO.SYS", "FIND.EXE"] {
        std::fs::write(out.join(BUILT_FILES).join(name), b"earlier").unwrap();
    }
    std::fs::write(out.join("notes.txt"), b"the user's").unwrap();

    clear_outputs(&out).unwrap();
    for name in [FLOPPY_IMAGE, HARD_DISK_IMAGE, REPORT, FAILED_BUILD_DISK] {
        assert!(!out.join(name).exists(), "{name}");
    }
    assert!(scratch.written(BUILT_FILES).is_empty());
    assert!(scratch.written("disks").is_empty());
    assert!(out.join("notes.txt").exists());
    clear_outputs(&out).unwrap();
}

/// What the build made goes into `bin` only once all of it has passed its
/// checks: a boot sector or a master boot record that is not one (an image
/// without the boot signature) writes no file there, and once both are
/// every file of CPY.BAT is written, as the build made it.
#[test]
fn the_files_of_the_build_go_into_bin_once_all_of_it_is_checked() {
    let scratch = Scratch::new("collect");
    let (boot_sector, master_boot_record) = (signed(0x7E00), signed(0x800));
    for (msboot_bin, fdboot_bin) in [
        (&vec![0; 0x7E00], &master_boot_record),
        (&boot_sector, &vec![0; 0x800]),
    ] {
        let disk = build_disk(CLEAN_LOG, msboot_bin, fdboot_bin);
        let Err(error) = collect::collect(&disk, &scratch.out()) else {
            panic!("an image without the boot signature is taken");
        };
        assert!(
            matches!(error, collect::Error::Layout(LayoutError::NoSignature(_))),
            "{error}"
        );
        assert_eq!(scratch.written(BUILT_FILES), Vec::<String>::new());
    }

    let disk = build_disk(CLEAN_LOG, &boot_sector, &master_boot_record);
    let built = collect::collect(&disk, &scratch.out()).unwrap();
    assert_eq!(built.files.len(), 2);
    assert_eq!(scratch.written(BUILT_FILES), ["FIND.EXE", "IO.SYS"]);
    let io_sys = scratch.out().join(BUILT_FILES).join("IO.SYS");
    assert_eq!(std::fs::read(io_sys).unwrap(), b"IO");
}

/// The size of BUILD.LOG is that of its bytes as the guest wrote them: a
/// byte of DOS's code page that is no UTF-8, as the lines a listing draws
/// with, counts as one byte.
#[test]
fn the_size_of_build_log_is_that_of_its_bytes() {
    let scratch = Scratch::new("log");
    let log = b"===== MESSAGES \r\n\xC4\xC4\xC4 \xB3 MESSAGES\r\n";
    let disk = build_disk(log, &signed(0x7E00), &signed(0x800));
    let built = collect::collect(&disk, &scratch.out()).unwrap();
    assert_eq!(built.log_bytes, log.len());
}

/// A floppy that does not boot to MS-DOS 4.00's prompt is never written:
/// the BIOS takes its boot sector for no code, finds nothing else to boot,
/// and waits for a key until the budget is spent.
#[test]
fn a_floppy_that_does_not_boot_is_not_written() {
    let scratch = Scratch::new("floppy");
    let built = built([0; SECTOR]);
    let inputs = InputFiles::read(&scratch.inputs()).unwrap();
    let pins = Pins::compiled_in();
    let Err(error) = floppy::make(&built, &inputs, &pins, &scratch.out()) else {
        panic!("the floppy booted");
    };
    assert!(matches!(error, floppy::Error::NoPrompt(_)), "{error}");
    assert_eq!(scratch.written("disks"), Vec::<String>::new());
}

/// A hard disk that does not start Doom8088 is never written: its master
/// boot record halts, and Mode 13h is never set.
#[test]
fn a_hard_disk_that_does_not_start_doom8088_is_not_written() {
    let scratch = Scratch::new("hard-disk");
    let mut halting = [0; SECTOR];
    halting[..3].copy_from_slice(&[0xF4, 0xEB, 0xFD]); // hlt; jmp short back to it
    let built = built(halting);
    let floppy = Floppy {
        bytes: Vec::new(),
        files: 0,
        free: 0,
        prompt_at: 0,
        chkdsk: String::new(),
    };
    let inputs = InputFiles::read(&scratch.inputs()).unwrap();
    let pins = Pins::compiled_in();
    let Err(error) = hard_disk::make(&built, &floppy, &inputs, &pins, &scratch.out()) else {
        panic!("the hard disk started Doom8088");
    };
    assert!(matches!(error, hard_disk::Error::NoMode13h(_)), "{error}");
    assert_eq!(scratch.written("disks"), Vec::<String>::new());
}

/// The files of the inputs that go onto the disks are read before the
/// first stage: inputs that lack one fail the run on it, naming it, and
/// inputs that lack none fail it in stage 1, on the boot floppy they lack.
#[test]
fn an_input_file_missing_fails_the_run_before_the_first_stage() {
    let scratch = Scratch::new("inputs");
    for missing in [
        scratch.0.join("msdos").join("LICENSE"),
        scratch.0.join("doom8088").join("DOOM1.WAD"),
    ] {
        let kept = std::fs::read(&missing).unwrap();
        std::fs::remove_file(&missing).unwrap();
        let Err(failure) = run(&scratch.inputs(), &scratch.out()) else {
            panic!("the run went on without {}", missing.display());
        };
        assert!(
            matches!(&failure, Failure::Input(path, _) if *path == missing),
            "{failure}"
        );
        std::fs::write(&missing, kept).unwrap();
    }
    let Err(failure) = run(&scratch.inputs(), &scratch.out()) else {
        panic!("the run went on without the boot floppy");
    };
    assert!(matches!(failure, Failure::Host(_)), "{failure}");
}
