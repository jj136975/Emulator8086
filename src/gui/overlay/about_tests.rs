//! What the About panel lists, asked of machines built the way the command
//! line builds them, and read off the panel drawn over one.

use std::sync::mpsc::channel;

use super::*;
use crate::gui::overlay::testing::Shell;
use crate::wall_clock::HostClock;
use emu8086_core::io::disk_spec::parse_disk_specs;
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::vm::runtime::Runtime;

/// A machine with the floppies of `--disk` values `disks`, the hard disks of
/// `--hd` values `hard_disks` and the boot order `boot`.
fn machine(disks: &[&str], hard_disks: &[&str], boot: Option<Vec<u8>>) -> Runtime {
    let strings =
        |values: &[&str]| -> Vec<String> { values.iter().map(|value| value.to_string()).collect() };
    let (_keys, key_events) = channel();
    Runtime::new(
        parse_disk_specs(&strings(disks)).unwrap(),
        strings(hard_disks),
        boot,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .unwrap()
}

fn drive(name: &str, image: &str, size: &str, access: &'static str) -> Drive {
    Drive {
        name: name.to_string(),
        image: image.to_string(),
        size: size.to_string(),
        access,
    }
}

/// The ROM is listed by the name it was loaded under, asked of the machine
/// on every frame: one that another tool loads, as Machine > Load ROM does,
/// is listed at once, and a machine none was loaded into says so.
#[test]
fn the_rom_is_listed_by_the_name_it_was_loaded_under() {
    let mut about = Shell::with_panel(machine(&[], &[], None), "About");
    assert!(about.screen().shows("ROM: none"));

    about
        .target
        .runtime
        .load_rom_image(&[0x90; 16], "small.bin")
        .unwrap();
    assert!(about.frame(Vec::new()).shows("ROM: small.bin"));
}

/// The drives that hold a disk are listed in the order of their numbers,
/// the floppies before the hard disks, whatever order they were given in:
/// each with where its image lives, its size and whether it can be written.
#[test]
fn drives_are_listed_with_image_size_and_access() {
    let rt = machine(&["b:memory:720:ro", "a:memory"], &["memory"], None);
    assert_eq!(
        drives(&rt.disks.borrow()),
        [
            drive("A:", "in memory", "1440 KB", "read-write"),
            drive("B:", "in memory", "720 KB", "read-only"),
            drive("HD0", "in memory", "32 MB", "read-write"),
        ]
    );
    assert!(drives(&machine(&[], &[], None).disks.borrow()).is_empty());
}

/// An image in a host file is listed by the path of that file, without the
/// `\\?\` Windows puts in front of a canonical path.
#[test]
fn a_file_backed_image_is_listed_by_its_path() {
    let name = format!("emu8086-about-{}.img", std::process::id());
    let file = std::env::temp_dir().join(&name);
    let _ = std::fs::remove_file(&file);

    // A missing image for B: is created.
    let rt = machine(&[&format!("b:{}:ro", file.display())], &[], None);
    let listed = drives(&rt.disks.borrow());
    drop(rt);
    let _ = std::fs::remove_file(&file);

    let [listed] = listed.as_slice() else {
        panic!("one drive holds a disk: {listed:?}");
    };
    assert!(listed.image.ends_with(&name), "{listed:?}");
    assert!(!listed.image.starts_with(r"\\"), "{listed:?}");
    assert_eq!(
        (listed.name.as_str(), listed.size.as_str(), listed.access),
        ("B:", "1440 KB", "read-only")
    );
}

/// The boot order is listed as the BIOS tries the drives: the default, and
/// the one the command line gave.
#[test]
fn the_boot_order_is_listed_as_the_bios_tries_it() {
    let default = machine(&[], &[], None);
    assert_eq!(boot_order(&default.disks.borrow()), "A:, HD0");

    let given = machine(&[], &["memory"], Some(vec![0x80, 0x01, 0x00]));
    assert_eq!(boot_order(&given.disks.borrow()), "HD0, B:, A:");
}
