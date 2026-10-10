//! The disk tools on a host whose dialogs the tests answer by hand, when
//! they like and in the order they like: what each command asks of the
//! host, when the drives change, and what the messages say. The rules of
//! the operations are the core's and are tested there
//! (`emu8086_core::io::disk_ops`); the panel that asks for the commands is
//! tested beside it (`overlay::disks`).

use std::path::PathBuf;
use std::sync::mpsc::channel;

use egui::accesskit::Role;
use winit::event::ElementState;

use super::*;
use crate::gui::host::testing::ByHand;
use crate::gui::overlay::testing::Driver;
use crate::gui::session::Session;
use emu8086_core::io::disk::DiskImage;
use emu8086_core::io::disk_spec::parse_disk_specs;
use emu8086_core::io::keyboard::KeyboardController;

fn question(title: &str, wanted: Wanted) -> FileQuestion {
    FileQuestion {
        title: title.to_string(),
        wanted,
    }
}

/// A message that tells of something done.
fn done(text: &str) -> (String, bool) {
    (text.to_string(), false)
}

/// A message that tells of something that failed.
fn failed(text: &str) -> (String, bool) {
    (text.to_string(), true)
}

/// A machine that does nothing but jump to itself.
const BUSY_ROM: [u8; 16] = {
    let mut rom = [0x90; 16]; // NOP
    rom[0] = 0xEB; // JMP $
    rom[1] = 0xFE;
    rom
};

/// A machine with the floppies of `--disk` values `disks` and the hard
/// disks of `--hd` values `hard_disks`, running `BUSY_ROM`.
fn machine(disks: &[&str], hard_disks: &[&str]) -> Runtime {
    let strings =
        |values: &[&str]| -> Vec<String> { values.iter().map(|value| value.to_string()).collect() };
    let (_keys, key_events) = channel();
    let mut runtime = Runtime::new(
        parse_disk_specs(&strings(disks)).unwrap(),
        strings(hard_disks),
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .unwrap();
    runtime.load_rom_image(&BUSY_ROM, "a busy loop").unwrap();
    runtime
}

/// The disk tools on a host whose files come by `access` and whose dialogs
/// the test answers, the machine whose drives they work on, and the
/// messages they report in.
struct Bench {
    tools: DiskTools,
    host: ByHand,
    runtime: Runtime,
    messages: Messages,
}

impl Bench {
    fn new(access: FileAccess, disks: &[&str], hard_disks: &[&str]) -> Self {
        let host = ByHand::default();
        Self {
            tools: DiskTools::on(Box::new(host.clone()), access),
            host,
            runtime: machine(disks, hard_disks),
            messages: Messages::default(),
        }
    }

    fn execute(&mut self, command: DiskCommand) {
        self.tools
            .execute(command, &mut self.runtime, &mut self.messages);
    }

    /// What a frame does first: takes up the answers that came.
    fn poll(&mut self) {
        self.tools.poll(&mut self.runtime, &mut self.messages);
    }

    /// What the messages said since the last look.
    fn said(&mut self) -> Vec<(String, bool)> {
        self.messages.take_said()
    }

    /// The size of the disk in `drive`, if it holds one.
    fn size_in(&self, drive: u8) -> Option<u64> {
        let disks = self.runtime.disks.borrow();
        let (_, image) = disks.drives.iter().find(|(held, _)| *held == drive)?;
        Some(image.total_bytes())
    }

    /// The drives that hold a disk.
    fn held(&self) -> Vec<u8> {
        let disks = self.runtime.disks.borrow();
        let mut held: Vec<u8> = disks.drives.iter().map(|(drive, _)| *drive).collect();
        held.sort();
        held
    }
}

/// A path in the host's temp directory that does not exist yet. What is
/// made there, a file or a directory, is removed again on drop.
struct Scratch(PathBuf);

impl Scratch {
    fn missing(name: &str) -> Self {
        let path = std::env::temp_dir().join(format!(
            "emu8086-disk-tools-{}-{}",
            std::process::id(),
            name
        ));
        let scratch = Self(path);
        scratch.remove();
        scratch
    }

    fn file(name: &str, content: &[u8]) -> Self {
        let scratch = Self::missing(name);
        std::fs::write(&scratch.0, content).unwrap();
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

/// What needs no file of the user's is done at once: no dialog is asked
/// for, the drives have changed when `execute` returns, and the messages
/// say what was done.
#[test]
fn what_needs_no_file_is_done_at_once() {
    let mut bench = Bench::new(FileAccess::Paths, &[], &[]);

    bench.execute(DiskCommand::InsertBlank {
        drive: 0x00,
        kb: Some(720),
        read_only: false,
    });
    assert_eq!(bench.size_in(0x00), Some(737_280));
    bench.execute(DiskCommand::Move {
        from: 0x00,
        to: 0x01,
    });
    assert_eq!(bench.held(), [0x01]);
    bench.execute(DiskCommand::Eject { drive: 0x01 });
    assert_eq!(bench.held(), []);

    assert_eq!(
        bench.said(),
        [
            done("A: now holds a formatted 720KB in-memory floppy"),
            done("Moved A: -> B:"),
            done("B: ejected"),
        ]
    );
    assert!(bench.host.questions().is_empty(), "no dialog");
}

/// What needs a file of the user's asks the host for it in a dialog and
/// returns: the drives stay as they are while the dialog is open, however
/// many frames go by, and the command is carried out on the first frame
/// after the answer came.
#[test]
fn what_needs_a_file_waits_for_the_users_answer() {
    let image = Scratch::file("insert.img", &vec![0u8; 368_640]);
    let mut bench = Bench::new(FileAccess::Paths, &[], &[]);

    bench.execute(DiskCommand::InsertImage {
        drive: 0x01,
        read_only: true,
    });
    let (asked, answer) = bench.host.question();
    assert_eq!(
        asked,
        question("Insert a disk image into B:", Wanted::Image)
    );
    for _ in 0..3 {
        bench.poll();
    }
    assert_eq!(bench.held(), [], "the dialog is open");
    assert_eq!(bench.said(), []);

    answer.send(vec![HostFile::Path(image.0.clone())]).unwrap();
    bench.poll();
    assert_eq!(
        bench.said(),
        [done(&format!("B: now holds {}", image.0.display()))]
    );
    let disks = bench.runtime.disks.borrow();
    assert_eq!(disks.drive_backed_by(&image.0), Some(0x01));
    let (_, inserted) = &disks.drives[0];
    assert!(inserted.readonly, "write-protected, as asked");
}

/// A dialog the user closes without choosing anything asks for nothing:
/// the drives stay as they are, nothing is said, and the dialog is done
/// with.
#[test]
fn a_dialog_closed_without_a_choice_does_nothing() {
    let mut bench = Bench::new(FileAccess::Paths, &["a:memory"], &[]);
    for command in [
        DiskCommand::InsertImage {
            drive: 0x00,
            read_only: false,
        },
        DiskCommand::CreateImage { drive: 0x01 },
        DiskCommand::Save { drive: 0x00 },
        DiskCommand::PutFiles { drive: 0x00 },
        DiskCommand::PutFolder { drive: 0x00 },
    ] {
        bench.execute(command);
        let (_, answer) = bench.host.question();
        answer.send(Vec::new()).unwrap();
        bench.poll();
        assert_eq!(bench.said(), [], "{command:?}");
        assert_eq!(bench.held(), [0x00], "{command:?}");
        assert_eq!(bench.tools.asked.waiting(), 0, "{command:?}");
    }
}

/// A dialog whose channel closes without an answer could not be shown:
/// that is reported, once, and the command goes no further.
#[test]
fn a_dialog_that_could_not_be_shown_is_reported() {
    let mut bench = Bench::new(FileAccess::Paths, &[], &[]);
    bench.execute(DiskCommand::InsertImage {
        drive: 0x00,
        read_only: false,
    });
    let (_, answer) = bench.host.question();
    drop(answer);

    bench.poll();
    bench.poll();
    assert_eq!(bench.said(), [failed("The file dialog could not be shown")]);
    assert_eq!(bench.held(), []);
}

/// Any number of dialogs may be open at once. Each one's command is carried
/// out when its answer comes, in the order the answers come, and one that
/// is never answered holds up none of the others.
#[test]
fn each_dialog_is_carried_out_when_its_own_answer_comes() {
    let first = Scratch::file("first.img", &vec![0u8; 368_640]);
    let second = Scratch::file("second.img", &vec![0u8; 737_280]);
    let mut bench = Bench::new(FileAccess::Paths, &["c:memory"], &[]);

    bench.execute(DiskCommand::InsertImage {
        drive: 0x00,
        read_only: false,
    });
    bench.execute(DiskCommand::InsertImage {
        drive: 0x01,
        read_only: false,
    });
    bench.execute(DiskCommand::PutFiles { drive: 0x02 });
    let mut questions = bench.host.questions();
    assert_eq!(questions.len(), 3);
    let (_, never) = questions.pop().unwrap();
    let (_, for_b) = questions.pop().unwrap();
    let (_, for_a) = questions.pop().unwrap();

    for_b.send(vec![HostFile::Path(second.0.clone())]).unwrap();
    bench.poll();
    assert_eq!(bench.held(), [0x01, 0x02]);
    for_a.send(vec![HostFile::Path(first.0.clone())]).unwrap();
    bench.poll();
    assert_eq!(bench.held(), [0x00, 0x01, 0x02]);
    assert_eq!(
        (bench.size_in(0x00), bench.size_in(0x01)),
        (Some(368_640), Some(737_280))
    );
    assert_eq!(bench.said().len(), 2);
    assert_eq!(bench.tools.asked.waiting(), 1, "the one never answered");
    drop(never);
}

/// Where files have paths, a disk is saved to the file the user names in
/// the host's dialog, which offers the name of the disk's own file, or one
/// made of its drive's. The disk's own file is recognised: the disk lives
/// there already.
#[test]
fn a_disk_is_saved_to_the_file_the_user_names() {
    let target = Scratch::missing("saved.img");
    let mut bench = Bench::new(FileAccess::Paths, &["a:memory"], &[]);
    let in_a = disk_ops::contents(&mut bench.runtime.disks.borrow_mut(), 0x00).unwrap();

    bench.execute(DiskCommand::Save { drive: 0x00 });
    let (asked, answer) = bench.host.question();
    let suggested = "floppy-a.img".to_string();
    assert_eq!(
        asked,
        question("Save the disk in A:", Wanted::NewImage { suggested })
    );
    answer.send(vec![HostFile::Path(target.0.clone())]).unwrap();
    bench.poll();
    assert_eq!(
        bench.said(),
        [done(&format!("A: saved to {}", target.0.display()))]
    );
    assert!(std::fs::read(&target.0).unwrap() == in_a);

    // The saved image, inserted, is offered under its own name, and saving
    // it onto itself writes nothing.
    bench.execute(DiskCommand::InsertImage {
        drive: 0x01,
        read_only: false,
    });
    let (_, answer) = bench.host.question();
    answer.send(vec![HostFile::Path(target.0.clone())]).unwrap();
    bench.poll();
    bench.said();
    bench.execute(DiskCommand::Save { drive: 0x01 });
    let (asked, answer) = bench.host.question();
    let own_name = target.0.file_name().unwrap().to_string_lossy().into_owned();
    assert_eq!(
        asked.wanted,
        Wanted::NewImage {
            suggested: own_name
        }
    );
    answer.send(vec![HostFile::Path(target.0.clone())]).unwrap();
    bench.poll();
    let said = bench.said();
    let [(text, false)] = said.as_slice() else {
        panic!("one message of something done: {said:?}");
    };
    assert!(text.starts_with("B: already lives in "), "{text}");
}

/// Where files have no paths, a disk is handed to the host as its contents,
/// under the name of the file it was made of or one made of its drive's:
/// no dialog is asked for, and no file is written. A save to a file would
/// need a path, and a process to name its temporary file after: a browser
/// has neither.
#[test]
fn a_disk_is_handed_over_as_its_contents_where_files_have_no_paths() {
    let mut bench = Bench::new(FileAccess::Contents, &["a:memory"], &[]);
    let games = DiskImage::from_bytes(vec![0x5Au8; 368_640], "games.img", false);
    bench.runtime.disks.borrow_mut().attach(0x01, games);
    let in_a = disk_ops::contents(&mut bench.runtime.disks.borrow_mut(), 0x00).unwrap();

    bench.execute(DiskCommand::Save { drive: 0x00 });
    bench.execute(DiskCommand::Save { drive: 0x01 });
    assert!(bench.host.questions().is_empty(), "no dialog");
    let handed_out = bench.host.handed_out();
    assert_eq!(handed_out.len(), 2);
    assert_eq!(handed_out[0].0, Path::new("floppy-a.img"));
    assert!(handed_out[0].1 == in_a);
    assert_eq!(handed_out[1].0, Path::new("games.img"));
    assert!(handed_out[1].1 == vec![0x5Au8; 368_640]);
    assert_eq!(
        bench.said(),
        [
            done("A: offered as the download floppy-a.img"),
            done("B: offered as the download games.img"),
        ]
    );
}

/// Where files have no paths there is no file to create and no folder to
/// read: those commands are refused with the reason, before a dialog is
/// asked for that could only come back empty.
#[test]
fn what_needs_a_path_is_refused_where_files_have_none() {
    let mut bench = Bench::new(FileAccess::Contents, &["a:memory"], &[]);
    bench.execute(DiskCommand::CreateImage { drive: 0x01 });
    bench.execute(DiskCommand::PutFolder { drive: 0x00 });
    assert!(bench.host.questions().is_empty(), "no dialog");
    assert_eq!(
        bench.said(),
        [
            failed("There is no file to create here: insert a blank disk"),
            failed("There is no folder to read here: put its files"),
        ]
    );
    assert_eq!(bench.held(), [0x00]);
}

/// A disk is saved, and files are copied onto it, only from a drive that
/// holds one: an empty drive is reported at once, and no dialog is asked
/// for that would have the user choose for nothing.
#[test]
fn an_empty_drive_is_reported_before_any_dialog() {
    for access in [FileAccess::Paths, FileAccess::Contents] {
        let mut bench = Bench::new(access, &["a:memory"], &[]);
        bench.execute(DiskCommand::Save { drive: 0x01 });
        bench.execute(DiskCommand::PutFiles { drive: 0x01 });
        assert!(bench.host.questions().is_empty(), "{access:?}");
        assert!(bench.host.handed_out().is_empty(), "{access:?}");
        assert_eq!(
            bench.said(),
            [failed("B: is empty"), failed("B: is empty")],
            "{access:?}"
        );
    }
    let mut bench = Bench::new(FileAccess::Paths, &[], &[]);
    bench.execute(DiskCommand::PutFolder { drive: 0x80 });
    assert!(bench.host.questions().is_empty());
    assert_eq!(bench.said(), [failed("HD0 is empty")]);
}

/// The files the user chooses are copied onto the disk, all of them, once
/// the answer is there. Where files have paths, a folder is copied with all
/// that is below it.
#[test]
fn the_files_chosen_are_copied_onto_the_disk() {
    let folder = Scratch::missing("folder");
    std::fs::create_dir_all(folder.0.join("SUB")).unwrap();
    std::fs::write(folder.0.join("README.TXT"), b"read me").unwrap();
    std::fs::write(folder.0.join("SUB").join("ONE.TXT"), b"1").unwrap();
    let mut bench = Bench::new(FileAccess::Paths, &["a:memory"], &[]);

    bench.execute(DiskCommand::PutFiles { drive: 0x00 });
    let (asked, answer) = bench.host.question();
    assert_eq!(
        asked,
        question("Copy files onto the disk in A:", Wanted::Files)
    );
    let handed = |name: &str| HostFile::Contents {
        name: name.to_string(),
        bytes: name.as_bytes().to_vec(),
    };
    answer
        .send(vec![handed("ONE.TXT"), handed("TWO.TXT")])
        .unwrap();
    bench.poll();
    assert_eq!(bench.said(), [done("Copied 2 files -> A:\\")]);

    bench.execute(DiskCommand::PutFolder { drive: 0x00 });
    let (asked, answer) = bench.host.question();
    assert_eq!(
        asked,
        question("Copy a folder onto the disk in A:", Wanted::Folder)
    );
    answer.send(vec![HostFile::Path(folder.0.clone())]).unwrap();
    bench.poll();
    assert_eq!(
        bench.said(),
        [done(&format!(
            "Copied 2 files from '{}' -> A:\\",
            folder.0.display()
        ))]
    );
}

/// Where files have paths, a new image file is created where the user says
/// and put into the drive, its name offered made of the drive's. A file
/// that is there already is never replaced, whatever the dialog asked.
#[test]
fn a_new_image_file_is_created_where_the_user_says() {
    let new = Scratch::missing("new.img");
    let mut bench = Bench::new(FileAccess::Paths, &[], &[]);

    bench.execute(DiskCommand::CreateImage { drive: 0x01 });
    let (asked, answer) = bench.host.question();
    let suggested = "floppy-b.img".to_string();
    assert_eq!(
        asked,
        question("Create a disk image for B:", Wanted::NewImage { suggested })
    );
    answer.send(vec![HostFile::Path(new.0.clone())]).unwrap();
    bench.poll();
    assert_eq!(
        bench.said(),
        [done(&format!(
            "B: now holds the new formatted 1440KB image {}",
            new.0.display()
        ))]
    );
    assert_eq!(std::fs::metadata(&new.0).unwrap().len(), 1_474_560);

    bench.execute(DiskCommand::CreateImage { drive: 0x80 });
    let (_, answer) = bench.host.question();
    answer.send(vec![HostFile::Path(new.0.clone())]).unwrap();
    bench.poll();
    assert_eq!(
        bench.said(),
        [failed(&format!(
            "Cannot create '{}': it exists, and a new image never replaces a file",
            new.0.display()
        ))]
    );
    assert_eq!(bench.held(), [0x01]);
}

/// A file to write is named by its path: an answer that hands one over as
/// its contents creates nothing and saves nothing, and says why.
#[test]
fn a_file_to_write_has_to_be_named_by_its_path() {
    let mut bench = Bench::new(FileAccess::Paths, &["a:memory"], &[]);
    let handed = || HostFile::Contents {
        name: "new.img".to_string(),
        bytes: Vec::new(),
    };
    for command in [
        DiskCommand::CreateImage { drive: 0x01 },
        DiskCommand::Save { drive: 0x00 },
    ] {
        bench.execute(command);
        let (_, answer) = bench.host.question();
        answer.send(vec![handed()]).unwrap();
        bench.poll();
        assert_eq!(
            bench.said(),
            [failed("The host named no file to write")],
            "{command:?}"
        );
    }
    assert_eq!(bench.held(), [0x00]);
}

/// A command that changed a hard disk says that the guest must be rebooted
/// before it uses the drive. One that changed a floppy, whose change line
/// tells the guest, says nothing of it, nor does one that was refused before
/// it touched a disk.
#[test]
fn a_change_to_a_hard_disk_says_that_the_guest_must_be_rebooted() {
    let mut bench = Bench::new(FileAccess::Paths, &["a:memory"], &["memory"]);
    bench.execute(DiskCommand::InsertBlank {
        drive: 0x81,
        kb: None,
        read_only: false,
    });
    bench.execute(DiskCommand::Eject { drive: 0x81 });
    bench.execute(DiskCommand::Eject { drive: 0x81 });
    bench.execute(DiskCommand::Move {
        from: 0x00,
        to: 0x01,
    });
    bench.execute(DiskCommand::Move {
        from: 0x01,
        to: 0x81,
    });
    assert_eq!(
        bench.said(),
        [
            done(
                "HD1 now holds a blank 32MB in-memory hard disk. Reboot the guest before it uses the drive"
            ),
            done("HD1 ejected. Reboot the guest before it uses the drive"),
            failed("HD1 is empty"),
            done("Moved A: -> B:"),
            done("Moved B: -> HD1. Reboot the guest before it uses the drive"),
        ]
    );
}

/// A disk is offered under the name of its own file, or of the file it was
/// made of, and under one made of its drive's when it has neither:
/// `floppy-a.img`, `hd0.img`. An empty drive has none to offer.
#[test]
fn a_disk_is_offered_under_its_own_name_or_its_drives() {
    let on_file = Scratch::file("own name.img", &vec![0u8; 368_640]);
    let runtime = machine(&["a:memory"], &["memory"]);
    {
        let mut disks = runtime.disks.borrow_mut();
        disks.attach(0x01, DiskImage::open(&on_file.0, false).unwrap());
        let games = DiskImage::from_bytes(vec![0u8; 368_640], "games.img", false);
        disks.attach(0x02, games);
    }
    let own = on_file
        .0
        .file_name()
        .unwrap()
        .to_string_lossy()
        .into_owned();

    assert_eq!(image_name(&runtime, 0x00).as_deref(), Some("floppy-a.img"));
    assert_eq!(image_name(&runtime, 0x01), Some(own));
    assert_eq!(image_name(&runtime, 0x02).as_deref(), Some("games.img"));
    assert_eq!(image_name(&runtime, 0x03), None);
    assert_eq!(image_name(&runtime, 0x80).as_deref(), Some("hd0.img"));
    assert_eq!(default_name(0x03), "floppy-d.img");
    assert_eq!(default_name(0x85), "hd5.img");
}

/// The session takes up a dialog's answer on its first frame after the
/// answer came, whether the overlay is shown or not. While the dialog is
/// open the frames go on and the guest runs. The Disks panel lists the disk
/// the answer put in, and the messages say what was done.
#[test]
fn the_session_carries_out_an_answer_on_the_frame_after_it_came() {
    let floppy = Scratch::file("session.img", &vec![0u8; 737_280]);
    let host = ByHand::default();
    let (kbd_tx, _sent) = channel();
    let mut session = Session::new(machine(&[], &[]), kbd_tx);
    session.disks = DiskTools::on(Box::new(host.clone()), FileAccess::Paths);
    session.overlay_key(ElementState::Pressed, false);
    let mut shown = Driver::new(session);
    shown.frame(Vec::new());
    shown.toggle_in_view_menu("Disks");
    let run_frames = |shown: &mut Driver<Session>, frames: usize| {
        for _ in 0..frames {
            let input = shown.next_input(Vec::new());
            shown.target.frame(|| input);
        }
    };

    shown.click(Role::Button, "Image file...");
    let (asked, answer) = host.question();
    assert_eq!(
        asked,
        question("Insert a disk image into A:", Wanted::Image)
    );
    let ran = shown.target.runtime.instruction_count;
    run_frames(&mut shown, 3);
    assert!(
        shown.target.runtime.instruction_count > ran,
        "the guest runs"
    );
    assert!(shown.target.runtime.disks.borrow().drives.is_empty());

    answer.send(vec![HostFile::Path(floppy.0.clone())]).unwrap();
    run_frames(&mut shown, 1);
    let screen = shown.frame(Vec::new());
    let message = format!("A: now holds {}", floppy.0.display());
    assert!(screen.shows(&message), "{:?}", screen.texts());
    let row = screen.row_of(Role::RadioButton, "A:");
    assert!(row[0].ends_with("session.img"), "{row:?}");
    assert_eq!(row[1..], ["720 KB", "80/2/9", "read-write"]);

    // Hidden, the overlay asks for no input, and the answer that comes
    // meanwhile is carried out all the same.
    shown.click(Role::RadioButton, "B:");
    shown.click(Role::Button, "Blank disk");
    shown.click(Role::Button, "360 KB");
    shown.click(Role::Button, "Save as...");
    let (_, answer) = host.question();
    let session = &mut shown.target;
    session.overlay_key(ElementState::Pressed, false);
    assert!(!session.overlay.draws());
    let saved = Scratch::missing("session-saved.img");
    answer.send(vec![HostFile::Path(saved.0.clone())]).unwrap();
    session.frame(|| panic!("no input is gathered for an overlay that shows nothing"));
    assert_eq!(std::fs::metadata(&saved.0).unwrap().len(), 368_640);
}

/// A command that is refused says why on the screen, as one that is done
/// says what it did: an image file that is the disk of another drive stays
/// out of the drive it was chosen for, and the message names the drive it
/// is in.
#[test]
fn a_command_that_fails_shows_its_reason_on_the_screen() {
    let floppy = Scratch::file("refused.img", &vec![0u8; 737_280]);
    let host = ByHand::default();
    let (kbd_tx, _sent) = channel();
    let mut session = Session::new(machine(&[], &[]), kbd_tx);
    session.disks = DiskTools::on(Box::new(host.clone()), FileAccess::Paths);
    let in_a = DiskImage::open(&floppy.0, false).unwrap();
    session.runtime.disks.borrow_mut().attach(0x00, in_a);
    session.overlay_key(ElementState::Pressed, false);
    let mut shown = Driver::new(session);
    shown.frame(Vec::new());
    shown.toggle_in_view_menu("Disks");

    shown.click(Role::RadioButton, "B:");
    shown.click(Role::Button, "Image file...");
    let (_, answer) = host.question();
    answer.send(vec![HostFile::Path(floppy.0.clone())]).unwrap();
    let input = shown.next_input(Vec::new());
    shown.target.frame(|| input);

    let screen = shown.frame(Vec::new());
    let reason = format!(
        "'{}' cannot be in two drives: it is already the disk in A:",
        floppy.0.display()
    );
    assert!(screen.shows(&reason), "{:?}", screen.texts());
    assert_eq!(screen.row_of(Role::RadioButton, "B:"), ["no disk"]);
}
