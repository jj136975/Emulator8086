//! The Disks panel: what it lists of machines built the way the command
//! line builds them, and what each of its controls asks of the app, driven
//! as a user drives it. Nobody carries the commands out here: what they do
//! to the machine, with the host's dialogs and files, is in the tests of
//! `gui::disk_tools`.

use std::sync::mpsc::channel;

use egui::accesskit::Role;

use super::*;
use crate::gui::meter::Meter;
use crate::gui::overlay::Messages;
use crate::gui::overlay::testing::{Driver, Passes, Shell};
use crate::gui::overlay::theme;
use crate::wall_clock::HostClock;
use emu8086_core::io::disk_spec::parse_disk_specs;
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::vm::runtime::Runtime;

/// A machine with the floppies of `--disk` values `disks` and the hard
/// disks of `--hd` values `hard_disks`. It never runs.
fn machine(disks: &[&str], hard_disks: &[&str]) -> Runtime {
    let strings =
        |values: &[&str]| -> Vec<String> { values.iter().map(|value| value.to_string()).collect() };
    let (_keys, key_events) = channel();
    Runtime::new(
        parse_disk_specs(&strings(disks)).unwrap(),
        strings(hard_disks),
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .unwrap()
}

/// The Disks panel open over `runtime`, on the host the tests run on: a
/// desktop, whose files have paths.
fn panel(runtime: Runtime) -> Driver<Shell> {
    Shell::with_panel(runtime, "Disks")
}

/// The commands asked for since the last look.
fn asked(shell: &mut Driver<Shell>) -> Vec<Command> {
    std::mem::take(&mut shell.target.commands)
}

fn held(image: &str, size: &str, geometry: &str, access: &'static str) -> Option<Held> {
    Some(Held {
        image: image.to_string(),
        size: size.to_string(),
        geometry: geometry.to_string(),
        access,
    })
}

/// Every drive is listed, with or without a disk, in the order of the BIOS
/// numbers: the four floppy drives, then the hard disks. A disk is listed
/// with where its image lives, its size, its cylinders, heads and sectors
/// per track, and whether it can be written.
#[test]
fn every_drive_is_listed_with_the_disk_it_holds() {
    let rt = machine(&["b:memory:720:ro", "a:memory"], &["memory"]);
    let listed: Vec<(String, Option<Held>)> = rows(&rt.disks.borrow())
        .into_iter()
        .map(|row| (row.name, row.disk))
        .collect();
    assert_eq!(
        listed,
        [
            (
                "A:".to_string(),
                held("in memory", "1440 KB", "80/2/18", "read-write")
            ),
            (
                "B:".to_string(),
                held("in memory", "720 KB", "80/2/9", "read-only")
            ),
            ("C:".to_string(), None),
            ("D:".to_string(), None),
            (
                "HD0".to_string(),
                held("in memory", "32 MB", "65/16/63", "read-write")
            ),
            ("HD1".to_string(), None),
        ]
    );
}

/// There is always a hard disk to put a disk into: two are listed at least,
/// and one more than the last that holds a disk, whichever that is.
#[test]
fn the_hard_disks_listed_end_with_an_empty_one() {
    let numbers = |rt: &Runtime| -> Vec<u8> {
        rows(&rt.disks.borrow())
            .iter()
            .map(|row| row.number)
            .filter(|number| *number >= 0x80)
            .collect()
    };
    let hard_disk = || DiskImage::new_in_memory_hard_disk(1);

    let rt = machine(&[], &[]);
    assert_eq!(numbers(&rt), [0x80, 0x81]);
    rt.disks.borrow_mut().attach(0x80, hard_disk());
    assert_eq!(numbers(&rt), [0x80, 0x81]);
    rt.disks.borrow_mut().attach(0x81, hard_disk());
    assert_eq!(numbers(&rt), [0x80, 0x81, 0x82]);
    rt.disks.borrow_mut().attach(0x84, hard_disk());
    assert_eq!(numbers(&rt), [0x80, 0x81, 0x82, 0x83, 0x84, 0x85]);

    // The last hard disk there can be has none behind it.
    rt.disks.borrow_mut().attach(0xFE, hard_disk());
    assert_eq!(numbers(&rt).last(), Some(&0xFE));
    assert_eq!(numbers(&rt).len(), 0x7F);
}

/// An image in a host file is listed by the path of that file, without the
/// `\\?\` Windows puts in front of a canonical path; an image made of a
/// file's contents by the name of that file, and as held in memory.
#[test]
fn an_image_is_listed_by_its_path_or_by_the_name_it_came_under() {
    let name = format!("emu8086-disks-{}.img", std::process::id());
    let file = std::env::temp_dir().join(&name);
    std::fs::write(&file, vec![0u8; 737_280]).unwrap();
    let on_file = DiskImage::open(&file, false).unwrap();
    let place = image_place(&on_file);
    drop(on_file);
    let _ = std::fs::remove_file(&file);
    assert!(place.ends_with(&name), "{place}");
    assert!(!place.starts_with(r"\\"), "{place}");

    let handed_over = DiskImage::from_bytes(vec![0u8; 737_280], "DOS disk 1.img", false);
    assert_eq!(image_place(&handed_over), "DOS disk 1.img (in memory)");
    let blank = DiskImage::new_in_memory_sized(737_280, false);
    assert_eq!(image_place(&blank), "in memory");
}

/// A path too long for its column is cut at its start, where the
/// directories are: what is shown ends with the name of the file. A short
/// one is shown as it is.
#[test]
fn a_long_path_is_cut_at_its_start() {
    assert_eq!(cut_at_start(r"C:\images\dos.img"), r"C:\images\dos.img");
    let exact = "x".repeat(PATH_CHARS);
    assert_eq!(cut_at_start(&exact), exact);

    let long = r"C:\Users\somebody\Documents\Disk images\MS-DOS 6.22\disk 1.img";
    let shown = cut_at_start(long);
    assert_eq!(shown, r"...ts\Disk images\MS-DOS 6.22\disk 1.img");
    assert_eq!(shown.chars().count(), PATH_CHARS);

    // Characters, not bytes: a path is cut between two of them.
    let accented = format!("{}\\disquette numéro 1.img", "é".repeat(30));
    let shown = cut_at_start(&accented);
    assert_eq!(shown.chars().count(), PATH_CHARS);
    assert!(shown.ends_with("disquette numéro 1.img"), "{shown}");
}

/// A floppy is named by its KB below a megabyte and by its megabytes from
/// there on, as its box names it.
#[test]
fn a_floppy_size_is_named_as_its_box_names_it() {
    let named: Vec<String> = FLOPPY_SIZES_KB.into_iter().map(floppy_size).collect();
    assert_eq!(
        named,
        [
            "160 KB", "180 KB", "320 KB", "360 KB", "720 KB", "1.2 MB", "1.44 MB", "2.88 MB"
        ]
    );
}

/// The panel shows the machine as it is on that frame: every drive in a row
/// of its own, A: chosen at first. A disk put into a drive while the panel
/// is open is listed on the next frame.
#[test]
fn the_panel_shows_the_drives_as_they_are() {
    let mut shell = panel(machine(&["a:memory"], &["memory"]));
    let screen = shell.screen();
    for heading in ["Drive", "Image", "Size", "C/H/S", "Access"] {
        assert!(screen.shows(heading), "{heading} in {:?}", screen.texts());
    }
    assert_eq!(
        screen.row_of(Role::RadioButton, "A:"),
        ["in memory", "1440 KB", "80/2/18", "read-write"]
    );
    assert_eq!(screen.row_of(Role::RadioButton, "B:"), ["no disk"]);
    assert_eq!(screen.row_of(Role::RadioButton, "D:"), ["no disk"]);
    assert_eq!(
        screen.row_of(Role::RadioButton, "HD0"),
        ["in memory", "32 MB", "65/16/63", "read-write"]
    );
    assert_eq!(screen.row_of(Role::RadioButton, "HD1"), ["no disk"]);
    assert!(!screen.has(Role::RadioButton, "HD2"));
    assert!(screen.chosen("A:") && !screen.chosen("B:"));
    assert!(screen.shows("Put into A:") && screen.shows("The disk in A:"));

    let floppy = DiskImage::from_bytes(vec![0u8; 368_640], "games.img", true);
    shell.target.runtime.disks.borrow_mut().attach(0x01, floppy);
    let screen = shell.frame(Vec::new());
    assert_eq!(
        screen.row_of(Role::RadioButton, "B:"),
        ["games.img (in memory)", "360 KB", "40/2/9", "read-only"]
    );
}

/// The buttons act on the drive that is chosen in the list, and say which
/// one that is.
#[test]
fn the_buttons_act_on_the_drive_that_is_chosen() {
    let mut shell = panel(machine(&["a:memory"], &["memory"]));
    let screen = shell.click(Role::RadioButton, "HD0");
    assert!(screen.chosen("HD0") && !screen.chosen("A:"));
    assert!(screen.shows("Put into HD0") && screen.shows("The disk in HD0"));
    assert!(!screen.shows("Put into A:"));

    shell.click(Role::Button, "Eject");
    assert_eq!(
        asked(&mut shell),
        [Command::Disk(DiskCommand::Eject { drive: 0x80 })]
    );
}

/// Each control asks the app for its command, for the drive that is chosen,
/// and nothing more: a menu that opens asks for nothing.
#[test]
fn each_control_asks_the_app_for_its_command() {
    let mut shell = panel(machine(&["a:memory"], &["memory"]));
    let drive = 0x00;
    for (button, command) in [
        (
            "Image file...",
            DiskCommand::InsertImage {
                drive,
                read_only: false,
            },
        ),
        ("New image file...", DiskCommand::CreateImage { drive }),
        ("Eject", DiskCommand::Eject { drive }),
        ("Save as...", DiskCommand::Save { drive }),
        ("Put files...", DiskCommand::PutFiles { drive }),
        ("Put folder...", DiskCommand::PutFolder { drive }),
    ] {
        shell.click(Role::Button, button);
        assert_eq!(asked(&mut shell), [Command::Disk(command)], "{button}");
    }

    // A blank floppy has a size, chosen from a menu.
    shell.click(Role::Button, "Blank disk");
    assert_eq!(asked(&mut shell), [], "the menu opened");
    shell.click(Role::Button, "720 KB");
    assert_eq!(
        asked(&mut shell),
        [Command::Disk(DiskCommand::InsertBlank {
            drive,
            kb: Some(720),
            read_only: false
        })]
    );
    assert!(
        !shell.screen().has(Role::Button, "720 KB"),
        "the menu closed"
    );

    // The drives a disk can move to are the others.
    shell.click(Role::Button, "Move to");
    assert_eq!(asked(&mut shell), []);
    let screen = shell.screen();
    assert!(!screen.has(Role::Button, "A:"), "not where it is");
    for other in ["B:", "C:", "D:", "HD0", "HD1"] {
        assert!(screen.has(Role::Button, other), "{other}");
    }
    shell.click(Role::Button, "HD1");
    assert_eq!(
        asked(&mut shell),
        [Command::Disk(DiskCommand::Move {
            from: drive,
            to: 0x81
        })]
    );
}

/// What is inserted is write-protected while the box is ticked. A blank
/// hard disk has one size: its button asks for none.
#[test]
fn a_disk_is_inserted_write_protected_if_the_box_is_ticked() {
    let mut shell = panel(machine(&["a:memory"], &[]));
    assert!(!shell.screen().ticked("write-protected"));
    shell.click(Role::CheckBox, "write-protected");
    assert!(shell.screen().ticked("write-protected"));

    shell.click(Role::Button, "Image file...");
    shell.click(Role::Button, "Blank disk");
    shell.click(Role::Button, "1.44 MB");
    shell.click(Role::RadioButton, "HD0");
    shell.click(Role::Button, "Blank disk");
    assert_eq!(
        asked(&mut shell),
        [
            Command::Disk(DiskCommand::InsertImage {
                drive: 0x00,
                read_only: true
            }),
            Command::Disk(DiskCommand::InsertBlank {
                drive: 0x00,
                kb: Some(1440),
                read_only: true
            }),
            Command::Disk(DiskCommand::InsertBlank {
                drive: 0x80,
                kb: None,
                read_only: true
            }),
        ]
    );
}

/// An empty drive offers what puts a disk into it, and nothing that needs
/// a disk: those buttons are greyed out and ask for nothing.
#[test]
fn an_empty_drive_offers_only_what_puts_a_disk_in() {
    let mut shell = panel(machine(&["a:memory"], &[]));
    let needs_a_disk = [
        "Eject",
        "Move to",
        "Save as...",
        "Put files...",
        "Put folder...",
    ];
    for button in needs_a_disk {
        assert!(shell.screen().enabled(Role::Button, button), "{button}");
    }

    let screen = shell.click(Role::RadioButton, "B:");
    for button in ["Image file...", "Blank disk", "New image file..."] {
        assert!(screen.enabled(Role::Button, button), "{button}");
    }
    for button in needs_a_disk {
        assert!(screen.has(Role::Button, button), "{button} is there");
        assert!(!screen.enabled(Role::Button, button), "{button}");
    }
    for button in needs_a_disk {
        shell.click(Role::Button, button);
    }
    assert_eq!(asked(&mut shell), [], "a greyed-out button asks nothing");
    assert!(!shell.screen().has(Role::Button, "A:"), "no menu opened");
}

/// While a reboot is due, the panel says why the guest must be rebooted and
/// how files get onto a hard disk without one, with a button that asks for
/// the reset. All of it goes when the guest's bootstrap has run.
#[test]
fn a_reboot_that_is_due_is_shown_with_its_reset_until_the_guest_boots() {
    let mut shell = panel(machine(&["a:memory"], &["memory"]));
    let shows_the_notice = |shell: &Driver<Shell>| {
        let screen = shell.screen();
        let lines = REBOOT_DUE.iter().chain([&REBOOT_NOW, &REBOOT_AVOIDED]);
        let shown = lines.filter(|line| screen.shows(line)).count();
        assert!(shown == 0 || shown == 4, "{:?}", screen.texts());
        assert_eq!(screen.has(Role::Button, "Reset the machine"), shown == 4);
        shown == 4
    };
    assert!(
        !shows_the_notice(&shell),
        "the machine starts with its disks"
    );

    let hard_disk = DiskImage::new_in_memory_hard_disk(1);
    shell
        .target
        .runtime
        .disks
        .borrow_mut()
        .attach(0x81, hard_disk);
    shell.frame(Vec::new());
    assert!(shows_the_notice(&shell));
    assert!(REBOOT_AVOIDED.contains("floppy") && REBOOT_AVOIDED.contains("COPY"));

    shell.click(Role::Button, "Reset the machine");
    assert_eq!(asked(&mut shell), [Command::Reset]);
    assert!(shows_the_notice(&shell), "nothing has booted yet");

    // What INT 19h does first: it rewinds the boot order.
    shell.target.runtime.port_out_byte(0xB1, 0x00);
    shell.frame(Vec::new());
    assert!(!shows_the_notice(&shell));
}

/// The panel by itself in a window, on a host whose files come by their
/// contents. The shell shows the panel of the host the tests run on, which
/// is the other kind.
struct HandedOver {
    ctx: egui::Context,
    runtime: Runtime,
    meter: Meter,
    commands: Vec<Command>,
    messages: Messages,
}

impl Passes for HandedOver {
    fn context(&self) -> &egui::Context {
        &self.ctx
    }

    fn pass(&mut self, input: egui::RawInput) -> egui::PlatformOutput {
        let mut cx = Cx {
            runtime: &mut self.runtime,
            paused: false,
            starting: None,
            meter: &self.meter,
            drawn_before: true,
            commands: &mut self.commands,
            messages: &mut self.messages,
        };
        let output = self.ctx.run_ui(input, |ui| {
            egui::Window::new("Disks").show(ui.ctx(), |ui| {
                body_on(ui, &mut cx, FileAccess::Contents);
            });
        });
        let egui::FullOutput {
            platform_output,
            mut textures_delta,
            ..
        } = output;
        // Nothing draws here: the texture changes are dropped on purpose.
        textures_delta.clear();
        platform_output
    }
}

/// In a browser the panel offers what a page can do: an image and files
/// that the user hands over, a blank disk, and the disk back as a download.
/// What needs a path, a new image file or a folder, is not offered, and
/// the panel says so.
#[test]
fn a_host_that_hands_files_over_is_offered_what_it_can_do() {
    let ctx = egui::Context::default();
    theme::apply(&ctx);
    let mut page = Driver::new(HandedOver {
        ctx,
        runtime: machine(&["a:memory"], &[]),
        meter: Meter::default(),
        commands: Vec::new(),
        messages: Messages::default(),
    });
    page.frame(Vec::new());
    let screen = page.wait(1.0);

    for button in [
        "Image file...",
        "Blank disk",
        "Eject",
        "Move to",
        "Download",
        "Put files...",
    ] {
        assert!(screen.enabled(Role::Button, button), "{button}");
    }
    for button in ["New image file...", "Save as...", "Put folder..."] {
        assert!(!screen.has(Role::Button, button), "{button}");
    }
    for line in CONTENTS_HOST {
        assert!(screen.shows(line), "{line:?} in {:?}", screen.texts());
    }
    let said = CONTENTS_HOST.join(" ");
    assert!(
        said.contains("No image file can be created") && said.contains("no folder"),
        "{said}"
    );

    page.click(Role::Button, "Download");
    page.click(Role::Button, "Put files...");
    assert_eq!(
        page.target.commands,
        [
            Command::Disk(DiskCommand::Save { drive: 0x00 }),
            Command::Disk(DiskCommand::PutFiles { drive: 0x00 }),
        ]
    );
}

/// On a desktop the panel says nothing of what a browser cannot do.
#[test]
fn a_host_with_paths_is_told_nothing_of_browsers() {
    let shell = panel(machine(&["a:memory"], &[]));
    for line in CONTENTS_HOST {
        assert!(!shell.screen().shows(line), "{line:?}");
    }
    assert!(!shell.screen().has(Role::Button, "Download"));
}
