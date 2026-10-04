use super::*;
use crate::io::disk::FixedClock;
use std::path::PathBuf;

/// Runs the monitor over `commands` and returns what it printed in
/// reply, i.e. everything from the first prompt on.
fn monitor(disk_ctrl: &mut DiskController, commands: &str) -> String {
    let mut out = Vec::new();
    run_monitor(disk_ctrl, &mut commands.as_bytes(), &mut out, &FixedClock);
    let text = String::from_utf8(out).unwrap();
    let (_, replies) = text.split_once("\n> ").unwrap();
    replies.to_string()
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

fn machine_with_floppy_and_hard_disk() -> DiskController {
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, formatted_floppy());
    disk_ctrl.attach(0x80, formatted_hard_disk());
    disk_ctrl
}

/// A path in the host's temp directory that does not exist yet and is
/// removed again on drop.
struct HostFile(PathBuf);

impl HostFile {
    fn missing(name: &str) -> Self {
        let path =
            std::env::temp_dir().join(format!("emu8086-monitor-{}-{}", std::process::id(), name));
        let _ = std::fs::remove_file(&path);
        Self(path)
    }

    fn new(name: &str, content: &[u8]) -> Self {
        let file = Self::missing(name);
        std::fs::write(&file.0, content).unwrap();
        file
    }

    /// The path as it is typed in a monitor command.
    fn quoted(&self) -> String {
        format!("\"{}\"", self.0.display())
    }
}

impl Drop for HostFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

// ---- argument splitting ------------------------------------------------

fn words(text: &str, max: usize) -> Vec<String> {
    split_args(text, max).unwrap()
}

#[test]
fn arguments_are_split_at_blanks() {
    assert_eq!(words("a disk.img", 2), ["a", "disk.img"]);
    assert_eq!(words("  a    disk.img  ", 2), ["a", "disk.img"]);
    assert_eq!(words("a", 2), ["a"]);
    assert_eq!(words("", 2), Vec::<String>::new());
    assert_eq!(words("a\tb", 3), ["a", "b"]);
}

#[test]
fn quoted_arguments_keep_their_blanks() {
    assert_eq!(
        words("\"C:\\My Files\\game.exe\" a GAME.EXE", 3),
        ["C:\\My Files\\game.exe", "a", "GAME.EXE"]
    );
    assert_eq!(words("a \"my disk.img\"", 2), ["a", "my disk.img"]);
    assert_eq!(words("a \"my disk.img\":ro", 2), ["a", "my disk.img:ro"]);
    assert_eq!(words("\"\" a", 2), ["", "a"]);
    assert!(split_args("\"no end a", 2).is_err());
}

/// The last argument takes the rest of the line, so a path with blanks
/// works unquoted where it comes last, as in `swap` and `save`.
#[test]
fn the_last_argument_takes_the_rest_of_the_line() {
    assert_eq!(
        words("a C:\\My Images\\disk 2.img", 2),
        ["a", "C:\\My Images\\disk 2.img"]
    );
    assert_eq!(
        words("file.txt a MY NAME.TXT", 3),
        ["file.txt", "a", "MY NAME.TXT"]
    );
    assert_eq!(words("a b c d", 2), ["a", "b c d"]);
    assert_eq!(words("  hd0  ", 1), ["hd0"]);
}

// ---- commands ----------------------------------------------------------

#[test]
fn monitor_lists_the_drives_and_stops_at_resume() {
    let mut disk_ctrl = machine_with_floppy_and_hard_disk();

    let mut out = Vec::new();
    run_monitor(
        &mut disk_ctrl,
        &mut "resume\neject a\n".as_bytes(),
        &mut out,
        &FixedClock,
    );
    let text = String::from_utf8(out).unwrap();

    assert!(text.contains("  A: [loaded] (1440KB)"), "{}", text);
    assert!(text.contains("  B: [empty]"), "{}", text);
    assert!(text.contains("  HD0: [loaded]"), "{}", text);
    assert!(
        disk_ctrl.find_drive(0x00).is_some(),
        "commands after `resume` are not read"
    );
}

#[test]
fn eject_and_move_report_what_they_did() {
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, formatted_floppy());

    let replies = monitor(
        &mut disk_ctrl,
        "move a b\neject b\neject b\neject x\nfrobnicate\n",
    );

    assert!(replies.contains("Moved A: -> B:"), "{}", replies);
    assert!(replies.contains("B: ejected"), "{}", replies);
    assert!(replies.contains("Error: B: is empty"), "{}", replies);
    assert!(replies.contains("Error: invalid drive 'x'"), "{}", replies);
    assert!(replies.contains("Error: unknown command"), "{}", replies);
    assert!(disk_ctrl.drives.is_empty());
}

#[test]
fn commands_with_missing_arguments_show_their_usage() {
    let mut disk_ctrl = DiskController::new();
    let replies = monitor(
        &mut disk_ctrl,
        "swap\nswap a\nnew a\nmove a\nsave a\neject\nput x\n",
    );
    for command in ["swap", "new", "move", "save", "eject", "put"] {
        assert!(
            replies.contains(&format!("Error: usage: {}", command)),
            "{}",
            replies
        );
    }
    assert_eq!(
        replies.matches("Error: usage: swap").count(),
        2,
        "{}",
        replies
    );
}

/// A mistyped path must neither create a blank image under that name
/// nor replace the disk in the drive.
#[test]
fn swap_does_not_create_a_missing_image() {
    let missing = HostFile::missing("typo.img");
    let mut disk_ctrl = machine_with_floppy_and_hard_disk();

    let replies = monitor(
        &mut disk_ctrl,
        &format!(
            "swap a {}\nswap hd0 {}\n",
            missing.quoted(),
            missing.quoted()
        ),
    );

    assert!(!missing.0.exists(), "no file may be created");
    assert_eq!(
        replies.matches("not found (use 'new'").count(),
        2,
        "{}",
        replies
    );
    for drive in [0x00, 0x80] {
        let disk = disk_ctrl.find_drive(drive).expect("the old disk stays");
        assert!(disk.backing_path().is_none());
    }
}

#[test]
fn swap_inserts_an_existing_image() {
    let image = HostFile::new("two words.img", &vec![0u8; 737_280]);

    // Unquoted as the last argument, quoted, and with the suffix.
    for (drive, source) in [
        (0x00, image.0.display().to_string()),
        (0x01, image.quoted()),
        (0x02, format!("{}:RO", image.quoted())),
        (0x03, format!("{}:rw", image.0.display())),
    ] {
        let mut disk_ctrl = DiskController::new();
        let letter = (b'a' + drive) as char;
        let replies = monitor(&mut disk_ctrl, &format!("swap {}  {}\n", letter, source));
        assert!(replies.contains("now holds"), "{}", replies);
        let disk = disk_ctrl.find_drive(drive).expect("the image is inserted");
        assert_eq!(disk.total_bytes(), 737_280);
        assert_eq!(disk.readonly, drive == 0x02);
    }
}

/// One image file is one disk in one drive: a second drive on the same file
/// would cache and write it without knowing of the first. Putting the file
/// back into the drive that holds it is a disk change like any other.
#[test]
fn swap_refuses_an_image_that_is_in_another_drive() {
    let image = HostFile::new("shared.img", &vec![0u8; 737_280]);
    let mut disk_ctrl = DiskController::new();

    let replies = monitor(
        &mut disk_ctrl,
        &format!("swap a {0}\nswap b {0}\nswap a {0}\n", image.quoted()),
    );

    assert_eq!(replies.matches("now holds").count(), 2, "{}", replies);
    assert!(replies.contains("already the disk in A:"), "{}", replies);
    assert!(disk_ctrl.find_drive(0x01).is_none(), "B: stays empty");
}

/// CHS addresses end at 1024 cylinders, which is 504 MB of a hard disk.
/// A larger image is cut down to that, and the user is told so.
#[test]
fn swap_reports_a_hard_disk_image_the_guest_cannot_reach_in_full() {
    let large = HostFile::missing("large.vhd");
    let small = HostFile::new("small.vhd", &vec![0u8; 4 * 16 * 63 * 512]);
    // 1030 cylinders. The file is only given its size, nothing is
    // written to it; without room for it there is nothing to test.
    let sized = std::fs::File::create(&large.0).and_then(|file| file.set_len(1030 * 16 * 63 * 512));
    if let Err(error) = sized {
        eprintln!("skipped: cannot make a scratch file of 506 MB: {}", error);
        return;
    }
    let mut disk_ctrl = DiskController::new();

    let replies = monitor(
        &mut disk_ctrl,
        &format!("swap hd0 {}\nswap hd1 {}\n", large.quoted(), small.quoted()),
    );

    assert_eq!(replies.matches("1024 cylinders").count(), 1, "{}", replies);
    assert!(
        replies.contains("506 MB") && replies.contains("504 MB"),
        "{}",
        replies
    );
    assert_eq!(
        disk_ctrl
            .find_drive(0x80)
            .expect("still inserted")
            .cylinders,
        1024
    );
    assert_eq!(disk_ctrl.find_drive(0x81).expect("inserted").cylinders, 4);
}

/// Only the word `memory`, alone or with a standard size, means an
/// in-memory disk.
#[test]
fn memory_is_a_keyword_only_as_a_whole_word() {
    let mut disk_ctrl = DiskController::new();

    let replies = monitor(
        &mut disk_ctrl,
        "swap a memoryx\nswap b memory:1000\nswap hd0 memory:64\n",
    );
    assert!(disk_ctrl.drives.is_empty(), "{}", replies);
    assert!(replies.contains("'memoryx' not found"), "{}", replies);
    assert!(
        replies.contains("unsupported floppy size '1000'"),
        "{}",
        replies
    );
    assert!(replies.contains("fixed size"), "{}", replies);

    let replies = monitor(
        &mut disk_ctrl,
        "swap a memory:720\nswap b MEMORY:ro\nswap hd0 memory\n",
    );
    assert!(
        replies.contains("A: now holds a formatted 720KB in-memory floppy"),
        "{}",
        replies
    );
    assert_eq!(disk_ctrl.find_drive(0x00).unwrap().total_bytes(), 737_280);
    assert_eq!(disk_ctrl.find_drive(0x01).unwrap().total_bytes(), 1_474_560);
    assert!(disk_ctrl.find_drive(0x01).unwrap().readonly);
    assert!(disk_ctrl.find_drive(0x80).is_some());
}

#[test]
fn new_creates_an_image_file_and_never_replaces_one() {
    let floppy = HostFile::missing("new floppy.img");
    let hard_disk = HostFile::missing("new.vhd");
    let mut disk_ctrl = DiskController::new();

    let replies = monitor(
        &mut disk_ctrl,
        &format!(
            "new a {}\nnew hd0 {}\n",
            floppy.quoted(),
            hard_disk.quoted()
        ),
    );
    assert!(
        replies.contains("A: now holds the new formatted 1440KB image"),
        "{}",
        replies
    );
    assert!(
        replies.contains("HD0 now holds the new blank 32MB image"),
        "{}",
        replies
    );
    assert_eq!(std::fs::metadata(&floppy.0).unwrap().len(), FLOPPY_144_SIZE);
    assert_eq!(
        std::fs::metadata(&hard_disk.0).unwrap().len(),
        65 * 16 * 63 * 512
    );

    // The new floppy is formatted: a file can be put on it right away.
    let host_file = HostFile::new("hello.txt", b"hello");
    let replies = monitor(
        &mut disk_ctrl,
        &format!("put {} a HELLO.TXT\n", host_file.quoted()),
    );
    assert!(
        replies.contains("-> A:\\HELLO.TXT (5 bytes)"),
        "{}",
        replies
    );

    // An existing file is left alone.
    disk_ctrl.detach(0x00);
    let size_before = std::fs::read(&floppy.0).unwrap();
    let replies = monitor(&mut disk_ctrl, &format!("new b {}\n", floppy.quoted()));
    assert!(replies.contains("Error: cannot create"), "{}", replies);
    assert!(disk_ctrl.find_drive(0x01).is_none());
    assert_eq!(std::fs::read(&floppy.0).unwrap(), size_before);
}

/// hd127 is the last hard disk: a higher number must not wrap around to
/// the floppy drive numbers.
#[test]
fn hard_disk_numbers_out_of_range_are_rejected() {
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, formatted_floppy());

    let replies = monitor(&mut disk_ctrl, "eject hd128\neject hd200\nmove a hd999\n");

    assert!(
        disk_ctrl.find_drive(0x00).is_some(),
        "A: must not be touched"
    );
    assert_eq!(replies.matches("invalid drive").count(), 3, "{}", replies);
}

#[test]
fn save_writes_the_image_and_recognises_its_own_file() {
    let target = HostFile::missing("saved disk.img");
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, formatted_floppy());

    let replies = monitor(&mut disk_ctrl, &format!("save a {}\n", target.0.display()));
    assert!(replies.contains("A: saved to"), "{}", replies);
    assert_eq!(std::fs::metadata(&target.0).unwrap().len(), FLOPPY_144_SIZE);

    let replies = monitor(
        &mut disk_ctrl,
        &format!(
            "swap b {}\nsave b {}\nsave a {}\n",
            target.quoted(),
            target.quoted(),
            target.quoted()
        ),
    );
    assert!(replies.contains("B: already lives in"), "{}", replies);
    assert!(replies.contains("is the image file of B:"), "{}", replies);
    assert_eq!(std::fs::metadata(&target.0).unwrap().len(), FLOPPY_144_SIZE);
}

#[test]
fn put_takes_a_quoted_host_path_with_blanks() {
    let host_file = HostFile::new("two words.txt", b"hello");
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, formatted_floppy());

    let replies = monitor(
        &mut disk_ctrl,
        &format!("put {} a HELLO.TXT\n", host_file.quoted()),
    );
    assert!(
        replies.contains("-> A:\\HELLO.TXT (5 bytes)"),
        "{}",
        replies
    );

    let replies = monitor(&mut disk_ctrl, "put \"no such file.txt\" a\nput . b\n");
    assert!(
        replies.contains("Error: 'no such file.txt' not found"),
        "{}",
        replies
    );
    assert!(replies.contains("Error: B: is empty"), "{}", replies);
}

#[test]
fn put_shows_the_name_the_file_has_in_dos() {
    let host_file = HostFile::new("longname.txt", b"hello");
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, formatted_floppy());

    // The host name is not a valid 8.3 name, so DOS sees a generated one.
    let replies = monitor(&mut disk_ctrl, &format!("put {} a\n", host_file.quoted()));
    assert!(
        replies.contains("-> A:\\EMU808~1.TXT (5 bytes)"),
        "{}",
        replies
    );

    let replies = monitor(
        &mut disk_ctrl,
        &format!("put {} a hello.txt\n", host_file.quoted()),
    );
    assert!(
        replies.contains("-> A:\\HELLO.TXT (5 bytes)"),
        "{}",
        replies
    );

    disk_ctrl.find_drive(0x00).unwrap().readonly = true;
    let replies = monitor(
        &mut disk_ctrl,
        &format!("put {} a other.txt\n", host_file.quoted()),
    );
    assert!(
        replies.contains("Error: disk is write-protected"),
        "{}",
        replies
    );
}

/// DOS never re-reads a fixed disk: whatever the monitor changes on one
/// is invisible to a running guest and is overwritten from its buffers.
#[test]
fn changing_a_hard_disk_warns_that_the_guest_must_be_rebooted() {
    let host_file = HostFile::new("warn.txt", b"hello");
    let new_image = HostFile::missing("warn.vhd");
    for command in [
        "eject hd0".to_string(),
        "swap hd0 memory".to_string(),
        "move hd0 hd1".to_string(),
        "move a hd0".to_string(),
        format!("put {} hd0", host_file.quoted()),
        format!("new hd1 {}", new_image.quoted()),
    ] {
        let mut disk_ctrl = machine_with_floppy_and_hard_disk();
        let replies = monitor(&mut disk_ctrl, &format!("{}\n", command));
        assert!(
            replies.contains("Warning") && replies.contains("type 'reset'"),
            "`{}` must warn and point at `reset`, got:\n{}",
            command,
            replies
        );
        let _ = std::fs::remove_file(&new_image.0);
    }
}

#[test]
fn floppy_changes_and_failed_commands_do_not_warn() {
    let host_file = HostFile::new("quiet.txt", b"hello");
    for command in [
        "eject a".to_string(),
        "swap a memory".to_string(),
        "move a b".to_string(),
        format!("put {} a", host_file.quoted()),
        "eject hd1".to_string(),
        "save hd0".to_string(),
        "swap hd0 no-such-image.vhd".to_string(),
    ] {
        let mut disk_ctrl = machine_with_floppy_and_hard_disk();
        let replies = monitor(&mut disk_ctrl, &format!("{}\n", command));
        assert!(
            !replies.contains("Warning"),
            "`{}` printed:\n{}",
            command,
            replies
        );
    }

    // A write-protected hard disk is not changed by a refused `put`.
    let mut disk_ctrl = machine_with_floppy_and_hard_disk();
    disk_ctrl.find_drive(0x80).unwrap().readonly = true;
    let replies = monitor(&mut disk_ctrl, &format!("put {} hd0\n", host_file.quoted()));
    assert!(replies.contains("write-protected"), "{}", replies);
    assert!(!replies.contains("Warning"), "{}", replies);
}

#[test]
fn reset_command_asks_the_runtime_to_reboot_the_guest() {
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, formatted_floppy());
    let mut out = Vec::new();

    for (commands, expected) in [
        ("reset\neject a\n", MonitorAction::Reset),
        ("RESET\n", MonitorAction::Reset),
        ("resume\n", MonitorAction::Resume),
        ("\n", MonitorAction::Resume),
        ("", MonitorAction::Resume),
        ("eject b\n", MonitorAction::Resume),
    ] {
        let action = run_monitor(
            &mut disk_ctrl,
            &mut commands.as_bytes(),
            &mut out,
            &FixedClock,
        );
        assert_eq!(action, expected, "commands {:?}", commands);
    }
    assert!(
        disk_ctrl.find_drive(0x00).is_some(),
        "nothing is read after `reset`"
    );
}

#[test]
fn help_shows_how_to_get_files_onto_a_hard_disk_safely() {
    let mut out = Vec::new();
    run_monitor(
        &mut DiskController::new(),
        &mut "".as_bytes(),
        &mut out,
        &FixedClock,
    );
    let help = String::from_utf8(out).unwrap();
    assert!(help.contains("reset"), "{}", help);
    assert!(help.contains("COPY A:"), "{}", help);
    assert!(help.contains("double"), "{}", help);
}
