use crate::io::disk::{
    DiskController, DiskImage, DiskSource, SaveOutcome, FLOPPY_144_SIZE, HD_DEFAULT_SIZE_MB,
};
use crate::io::disk_spec::{drive_label, parse_drive, parse_hd_source, parse_source};
use std::io::{BufRead, Write};
use std::path::Path;

/// Writes one line of monitor output. A failed write to the terminal has
/// nowhere to be reported, so it is ignored.
macro_rules! say {
    ($out:expr) => {{
        let _ = writeln!($out);
    }};
    ($out:expr, $($arg:tt)*) => {{
        let _ = writeln!($out, $($arg)*);
    }};
}

/// What the runtime has to do when the monitor returns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MonitorAction {
    /// Continue the guest where it was interrupted.
    Resume,
    /// Reboot the guest.
    Reset,
}

/// Printed after the monitor changed a hard disk. DOS never re-reads a fixed
/// disk: it keeps using the directory and FAT sectors in its buffers and
/// writes them back over whatever was changed from the host.
fn warn_if_hard_disk(drive: u8, out: &mut dyn Write) {
    if drive >= 0x80 {
        say!(
            out,
            "Warning: {} was changed behind the guest's back. DOS does not notice changes",
            drive_label(drive)
        );
        say!(out, "         to a hard disk while it runs and overwrites them from its buffers.");
        say!(out, "         Reboot the guest (type 'reset') before the drive is used.");
    }
}

/// Suspends the terminal UI and runs the monitor on the console.
pub fn enter_monitor(disk_ctrl: &mut DiskController) -> MonitorAction {
    let mut stdout = std::io::stdout();
    let _ = crossterm::terminal::disable_raw_mode();
    let _ = write!(stdout, "\x1B[0m\x1B[2J\x1B[H");
    let _ = stdout.flush();

    let action = run_monitor(disk_ctrl, &mut std::io::stdin().lock(), &mut stdout);

    let _ = crossterm::terminal::enable_raw_mode();
    let _ = write!(stdout, "\x1B[2J\x1B[H");
    let _ = stdout.flush();
    action
}

/// The monitor itself: shows the drives, then executes commands read from
/// `input` until `resume`, `reset`, an empty line or the end of the input.
fn run_monitor(
    disk_ctrl: &mut DiskController,
    input: &mut dyn BufRead,
    out: &mut dyn Write,
) -> MonitorAction {
    show_drives(disk_ctrl, out);
    show_help(out);

    loop {
        let _ = write!(out, "> ");
        let _ = out.flush();
        let mut line = String::new();
        if input.read_line(&mut line).is_err() {
            return MonitorAction::Resume;
        }
        let line = line.trim();
        if line.is_empty() {
            return MonitorAction::Resume;
        }

        let (command, arguments) = match line.split_once(char::is_whitespace) {
            Some((command, arguments)) => (command, arguments.trim()),
            None => (line, ""),
        };
        let result = match command.to_ascii_lowercase().as_str() {
            "resume" if arguments.is_empty() => return MonitorAction::Resume,
            "reset" if arguments.is_empty() => return MonitorAction::Reset,
            "swap" => swap(disk_ctrl, arguments, out),
            "new" => create(disk_ctrl, arguments, out),
            "move" => move_disk(disk_ctrl, arguments, out),
            "save" => save(disk_ctrl, arguments, out),
            "eject" => eject(disk_ctrl, arguments, out),
            "put" => put(disk_ctrl, arguments, out),
            _ => Err("unknown command (type 'resume' or press Enter to continue)".to_string()),
        };
        if let Err(message) = result {
            say!(out, "Error: {}", message);
        }
    }
}

fn show_drives(disk_ctrl: &mut DiskController, out: &mut dyn Write) {
    say!(out, "=== Emulator Monitor (F12) ===");
    say!(out);

    say!(out, "Floppy Drives:");
    for drive_num in 0x00..0x04u8 {
        let letter = (b'A' + drive_num) as char;
        match disk_ctrl.find_drive(drive_num) {
            Some(d) => {
                let size_kb = d.total_bytes() / 1024;
                let ro = if d.readonly { " [ro]" } else { "" };
                say!(out, "  {}: [loaded] ({}KB){}", letter, size_kb, ro);
            }
            None => say!(out, "  {}: [empty]", letter),
        }
    }

    let hd_count = disk_ctrl.drives.iter().filter(|(id, _)| *id >= 0x80).count();
    if hd_count > 0 {
        say!(out, "Hard Disks:");
        for i in 0..4u8 {
            let drive_num = 0x80 + i;
            if let Some(d) = disk_ctrl.find_drive(drive_num) {
                let size_mb = d.total_bytes() / (1024 * 1024);
                let ro = if d.readonly { " [ro]" } else { "" };
                say!(
                    out,
                    "  HD{}: [loaded] ({}MB, C/H/S={}/{}/{}){}",
                    i, size_mb, d.cylinders, d.heads, d.sectors_per_track, ro
                );
            }
        }
    } else {
        say!(out, "Hard Disks: (none)");
    }
    say!(out);
}

fn show_help(out: &mut dyn Write) {
    say!(out, "Commands:");
    say!(out, "  swap <drive> <image>[:ro]  Insert an existing disk image (e.g. swap a disk2.img)");
    say!(out, "  swap <drive> memory[:KB]   Insert an empty disk held in memory (floppy: formatted,");
    say!(out, "                             KB one of 160 180 320 360 720 1200 1440 2880, default");
    say!(out, "                             1440; hard disk: blank 32MB)");
    say!(out, "  new <drive> <path>         Create an image file and insert it");
    say!(out, "                             (floppy: formatted 1.44M, hard disk: blank 32MB)");
    say!(out, "  move <from> <to>           Move disk between drives (e.g. move b a)");
    say!(out, "  save <drive> <path>        Save disk contents to file");
    say!(out, "  eject <drive>              Eject disk from drive");
    say!(out, "  put <file> <drive> [name]  Copy host file to FAT disk");
    say!(out, "  put <dir> <drive>          Copy host directory to FAT disk recursively");
    say!(out, "  reset                      Reboot the guest");
    say!(out, "  resume                     Resume emulation (or just press Enter)");
    say!(out);
    say!(out, "Drives are a-d and hd0, hd1, ... A path that contains blanks goes in double");
    say!(out, "quotes.");
    say!(out);
    say!(out, "DOS does not notice changes to a hard disk while it runs: after put, swap,");
    say!(out, "eject or move on hdN, reboot the guest with 'reset'. To copy files without");
    say!(out, "a reboot, go through a floppy: swap a memory, put <file> a, resume, then");
    say!(out, "COPY A:<file> C: in DOS.");
    say!(out);
}

/// Splits the arguments of a command into at most `max` words. Words are
/// separated by blanks; a word in double quotes may contain blanks. The last
/// of `max` words takes the rest of the line, so that a path with blanks
/// also works unquoted where it is the final argument.
fn split_args(text: &str, max: usize) -> Result<Vec<String>, String> {
    let mut args = Vec::new();
    let mut rest = text.trim();
    while !rest.is_empty() {
        let last = args.len() + 1 == max;
        let (word, remainder) = next_word(rest, last)?;
        args.push(word);
        rest = remainder.trim_start();
    }
    Ok(args)
}

/// Takes one word off the front of `text`. A word that starts with a double
/// quote runs to the closing quote, and what follows the quote directly
/// still belongs to it (`"my disk.img":ro`). An unquoted word runs to the
/// next blank, or to the end of the text if it is the final argument.
fn next_word(text: &str, last: bool) -> Result<(String, &str), String> {
    let (quoted, unquoted) = match text.strip_prefix('"') {
        Some(after_quote) => after_quote
            .split_once('"')
            .ok_or_else(|| "missing closing double quote".to_string())?,
        None => ("", text),
    };
    let end = if last {
        unquoted.len()
    } else {
        unquoted.find(char::is_whitespace).unwrap_or(unquoted.len())
    };
    Ok((format!("{}{}", quoted, &unquoted[..end]), &unquoted[end..]))
}

/// `swap` only inserts images that exist: a mistyped path must not leave a
/// new, empty image behind.
fn require_existing(path: &Path) -> Result<(), String> {
    if path.exists() {
        Ok(())
    } else {
        Err(format!(
            "'{}' not found (use 'new' to create an image)",
            path.display()
        ))
    }
}

fn swap(disk_ctrl: &mut DiskController, arguments: &str, out: &mut dyn Write) -> Result<(), String> {
    let args = split_args(arguments, 2)?;
    let [drive, source] = args.as_slice() else {
        return Err("usage: swap <drive> <image>[:ro] | memory[:KB]".to_string());
    };
    let drive = parse_drive(drive)?;
    let label = drive_label(drive);
    let cannot_open = |path: &Path, error: std::io::Error| {
        format!("cannot open '{}': {}", path.display(), error)
    };

    if drive >= 0x80 {
        let (source, readonly) = parse_hd_source(source)?;
        match source {
            DiskSource::Memory(_) => {
                let mut image = DiskImage::new_in_memory_hard_disk(HD_DEFAULT_SIZE_MB);
                image.readonly = readonly;
                disk_ctrl.attach(drive, image);
                say!(
                    out,
                    "{} now holds a blank {}MB in-memory hard disk",
                    label, HD_DEFAULT_SIZE_MB
                );
            }
            DiskSource::FilePath(path) => {
                require_existing(&path)?;
                let image = DiskImage::open_hard_disk(&path, readonly)
                    .map_err(|error| cannot_open(&path, error))?;
                let note = image.capacity_note();
                disk_ctrl.attach(drive, image);
                say!(out, "{} now holds {}", label, path.display());
                if let Some(note) = note {
                    say!(out, "Warning: {}", note);
                }
            }
        }
    } else {
        let (source, readonly) = parse_source(source)?;
        match source {
            DiskSource::Memory(size) => {
                let mut image = DiskImage::new_in_memory_sized(size, false);
                image
                    .format_fat()
                    .map_err(|error| format!("could not format the floppy: {}", error))?;
                image.readonly = readonly;
                disk_ctrl.attach(drive, image);
                say!(
                    out,
                    "{} now holds a formatted {}KB in-memory floppy",
                    label,
                    size / 1024
                );
            }
            DiskSource::FilePath(path) => {
                require_existing(&path)?;
                let image = DiskImage::open(&path, readonly)
                    .map_err(|error| cannot_open(&path, error))?;
                disk_ctrl.attach(drive, image);
                say!(out, "{} now holds {}", label, path.display());
            }
        }
    }
    warn_if_hard_disk(drive, out);
    Ok(())
}

/// `new <drive> <path>`: creates an image file and inserts it. A floppy
/// image is a formatted 1.44M disk, a hard disk image a blank disk of the
/// default size. An existing file is not touched.
fn create(disk_ctrl: &mut DiskController, arguments: &str, out: &mut dyn Write) -> Result<(), String> {
    let args = split_args(arguments, 2)?;
    let [drive, path] = args.as_slice() else {
        return Err("usage: new <drive> <path>".to_string());
    };
    let drive = parse_drive(drive)?;
    let label = drive_label(drive);
    let path = Path::new(path);
    let cannot_create =
        |error: std::io::Error| format!("cannot create '{}': {}", path.display(), error);

    if drive >= 0x80 {
        let image = DiskImage::create_hard_disk(path, HD_DEFAULT_SIZE_MB).map_err(cannot_create)?;
        disk_ctrl.attach(drive, image);
        say!(
            out,
            "{} now holds the new blank {}MB image {}",
            label,
            HD_DEFAULT_SIZE_MB,
            path.display()
        );
    } else {
        let mut image = DiskImage::create(path, FLOPPY_144_SIZE).map_err(cannot_create)?;
        image.format_fat().map_err(cannot_create)?;
        disk_ctrl.attach(drive, image);
        say!(
            out,
            "{} now holds the new formatted {}KB image {}",
            label,
            FLOPPY_144_SIZE / 1024,
            path.display()
        );
    }
    warn_if_hard_disk(drive, out);
    Ok(())
}

fn move_disk(disk_ctrl: &mut DiskController, arguments: &str, out: &mut dyn Write) -> Result<(), String> {
    let args = split_args(arguments, 2)?;
    let [from, to] = args.as_slice() else {
        return Err("usage: move <from> <to>".to_string());
    };
    let from = parse_drive(from)?;
    let to = parse_drive(to)?;

    let disk = disk_ctrl
        .detach(from)
        .ok_or_else(|| format!("{} is empty", drive_label(from)))?;
    disk_ctrl.attach(to, disk);
    say!(out, "Moved {} -> {}", drive_label(from), drive_label(to));
    warn_if_hard_disk(from, out);
    warn_if_hard_disk(to, out);
    Ok(())
}

fn save(disk_ctrl: &mut DiskController, arguments: &str, out: &mut dyn Write) -> Result<(), String> {
    let args = split_args(arguments, 2)?;
    let [drive, target] = args.as_slice() else {
        return Err("usage: save <drive> <path>".to_string());
    };
    let drive = parse_drive(drive)?;
    let label = drive_label(drive);
    let target = Path::new(target);

    if let Some(other) = disk_ctrl.drive_backed_by(target) {
        if other != drive {
            return Err(format!(
                "{} is the image file of {}; eject that drive first",
                target.display(),
                drive_label(other)
            ));
        }
    }
    let disk = disk_ctrl
        .find_drive(drive)
        .ok_or_else(|| format!("{} is empty", label))?;
    match disk.save_to_file(target).map_err(|error| error.to_string())? {
        SaveOutcome::Written => say!(out, "{} saved to {}", label, target.display()),
        SaveOutcome::AlreadyBacking => say!(
            out,
            "{} already lives in {}: every guest write goes straight to that file",
            label,
            target.display()
        ),
    }
    Ok(())
}

fn eject(disk_ctrl: &mut DiskController, arguments: &str, out: &mut dyn Write) -> Result<(), String> {
    let args = split_args(arguments, 1)?;
    let [drive] = args.as_slice() else {
        return Err("usage: eject <drive>".to_string());
    };
    let drive = parse_drive(drive)?;
    let label = drive_label(drive);

    if disk_ctrl.detach(drive).is_none() {
        return Err(format!("{} is empty", label));
    }
    say!(out, "{} ejected", label);
    warn_if_hard_disk(drive, out);
    Ok(())
}

fn put(disk_ctrl: &mut DiskController, arguments: &str, out: &mut dyn Write) -> Result<(), String> {
    let args = split_args(arguments, 3)?;
    let (host_path, drive, dos_name) = match args.as_slice() {
        [host_path, drive] => (host_path, drive, None),
        [host_path, drive, dos_name] => (host_path, drive, Some(dos_name)),
        _ => return Err("usage: put <host-path> <drive> [dos-name]".to_string()),
    };
    let host = Path::new(host_path);
    if !host.exists() {
        return Err(format!("'{}' not found", host_path));
    }
    let drive = parse_drive(drive)?;
    let label = drive_label(drive);
    let Some(disk) = disk_ctrl.find_drive(drive) else {
        return Err(format!("{} is empty", label));
    };
    let write_protected = disk.readonly;

    let result = if host.is_dir() {
        disk_ctrl
            .put_dir(drive, host)
            .map(|count| format!("Copied {} files from '{}' -> {}\\", count, host_path, label))
    } else {
        let data = std::fs::read(host)
            .map_err(|error| format!("cannot read '{}': {}", host_path, error))?;
        let name = match dos_name {
            Some(name) => name.clone(),
            None => host
                .file_name()
                .map(|name| name.to_string_lossy().into_owned())
                .unwrap_or_else(|| "FILE.DAT".to_string()),
        };
        // The reply shows the name the file has in DOS, which is a generated
        // one if `name` is not a valid 8.3 name.
        disk_ctrl.put_file(drive, &data, &name).map(|stored| {
            format!("Copied '{}' -> {}\\{} ({} bytes)", host_path, label, stored, data.len())
        })
    };

    match result {
        Ok(message) => say!(out, "{}", message),
        Err(error) => say!(out, "Error: {}", error),
    }
    // A copy that failed half-way has changed the disk too; only a
    // write-protected disk is certain to be untouched.
    if !write_protected {
        warn_if_hard_disk(drive, out);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    /// Runs the monitor over `commands` and returns what it printed in
    /// reply, i.e. everything from the first prompt on.
    fn monitor(disk_ctrl: &mut DiskController, commands: &str) -> String {
        let mut out = Vec::new();
        run_monitor(disk_ctrl, &mut commands.as_bytes(), &mut out);
        let text = String::from_utf8(out).unwrap();
        let (_, replies) = text.split_once("\n> ").unwrap();
        replies.to_string()
    }

    fn formatted_floppy() -> DiskImage {
        let mut image = DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false);
        image.format_fat().unwrap();
        image
    }

    fn formatted_hard_disk() -> DiskImage {
        let mut image = DiskImage::new_in_memory_hard_disk(1);
        image.format_fat().unwrap();
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
            let path = std::env::temp_dir()
                .join(format!("emu8086-monitor-{}-{}", std::process::id(), name));
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

    /// A path with blanks worked unquoted as the last argument of `swap`
    /// and `save`, and still does.
    #[test]
    fn the_last_argument_takes_the_rest_of_the_line() {
        assert_eq!(words("a C:\\My Images\\disk 2.img", 2), ["a", "C:\\My Images\\disk 2.img"]);
        assert_eq!(words("file.txt a MY NAME.TXT", 3), ["file.txt", "a", "MY NAME.TXT"]);
        assert_eq!(words("a b c d", 2), ["a", "b c d"]);
        assert_eq!(words("  hd0  ", 1), ["hd0"]);
    }

    // ---- commands ----------------------------------------------------------

    #[test]
    fn monitor_lists_the_drives_and_stops_at_resume() {
        let mut disk_ctrl = machine_with_floppy_and_hard_disk();

        let mut out = Vec::new();
        run_monitor(&mut disk_ctrl, &mut "resume\neject a\n".as_bytes(), &mut out);
        let text = String::from_utf8(out).unwrap();

        assert!(text.contains("  A: [loaded] (1440KB)"), "{}", text);
        assert!(text.contains("  B: [empty]"), "{}", text);
        assert!(text.contains("  HD0: [loaded]"), "{}", text);
        assert!(disk_ctrl.find_drive(0x00).is_some(), "commands after `resume` are not read");
    }

    #[test]
    fn eject_and_move_report_what_they_did() {
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, formatted_floppy());

        let replies = monitor(&mut disk_ctrl, "move a b\neject b\neject b\neject x\nfrobnicate\n");

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
        let replies = monitor(&mut disk_ctrl, "swap\nswap a\nnew a\nmove a\nsave a\neject\nput x\n");
        for command in ["swap", "new", "move", "save", "eject", "put"] {
            assert!(replies.contains(&format!("Error: usage: {}", command)), "{}", replies);
        }
        assert_eq!(replies.matches("Error: usage: swap").count(), 2, "{}", replies);
    }

    /// A mistyped path must neither create a blank image under that name
    /// nor replace the disk in the drive.
    #[test]
    fn swap_does_not_create_a_missing_image() {
        let missing = HostFile::missing("typo.img");
        let mut disk_ctrl = machine_with_floppy_and_hard_disk();

        let replies = monitor(
            &mut disk_ctrl,
            &format!("swap a {}\nswap hd0 {}\n", missing.quoted(), missing.quoted()),
        );

        assert!(!missing.0.exists(), "no file may be created");
        assert_eq!(replies.matches("not found (use 'new'").count(), 2, "{}", replies);
        for drive in [0x00, 0x80] {
            let disk = disk_ctrl.find_drive(drive).expect("the old disk stays");
            assert!(disk.backing_path().is_none());
        }
    }

    #[test]
    fn swap_inserts_an_existing_image() {
        let image = HostFile::new("two words.img", &vec![0u8; 737_280]);
        let mut disk_ctrl = DiskController::new();

        // Unquoted as the last argument, quoted, and with the suffix.
        for (drive, source) in [
            (0x00, image.0.display().to_string()),
            (0x01, image.quoted()),
            (0x02, format!("{}:RO", image.quoted())),
            (0x03, format!("{}:rw", image.0.display())),
        ] {
            let letter = (b'a' + drive) as char;
            let replies = monitor(&mut disk_ctrl, &format!("swap {}  {}\n", letter, source));
            assert!(replies.contains("now holds"), "{}", replies);
            let disk = disk_ctrl.find_drive(drive).expect("the image is inserted");
            assert_eq!(disk.total_bytes(), 737_280);
            assert_eq!(disk.readonly, drive == 0x02);
        }
    }

    /// CHS addresses end at 1024 cylinders, which is 504 MB of a hard disk.
    /// A larger image is cut down to that, and the user is told so.
    #[test]
    fn swap_reports_a_hard_disk_image_the_guest_cannot_reach_in_full() {
        let large = HostFile::missing("large.vhd");
        let small = HostFile::new("small.vhd", &vec![0u8; 4 * 16 * 63 * 512]);
        // 1030 cylinders. The file is only given its size, nothing is
        // written to it; without room for it there is nothing to test.
        let sized = std::fs::File::create(&large.0)
            .and_then(|file| file.set_len(1030 * 16 * 63 * 512));
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
        assert!(replies.contains("506 MB") && replies.contains("504 MB"), "{}", replies);
        assert_eq!(disk_ctrl.find_drive(0x80).expect("still inserted").cylinders, 1024);
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
        assert!(replies.contains("unsupported floppy size '1000'"), "{}", replies);
        assert!(replies.contains("fixed size"), "{}", replies);

        let replies = monitor(&mut disk_ctrl, "swap a memory:720\nswap b MEMORY:ro\nswap hd0 memory\n");
        assert!(replies.contains("A: now holds a formatted 720KB in-memory floppy"), "{}", replies);
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
            &format!("new a {}\nnew hd0 {}\n", floppy.quoted(), hard_disk.quoted()),
        );
        assert!(replies.contains("A: now holds the new formatted 1440KB image"), "{}", replies);
        assert!(replies.contains("HD0 now holds the new blank 32MB image"), "{}", replies);
        assert_eq!(std::fs::metadata(&floppy.0).unwrap().len(), FLOPPY_144_SIZE);
        assert_eq!(std::fs::metadata(&hard_disk.0).unwrap().len(), 65 * 16 * 63 * 512);

        // The new floppy is formatted: a file can be put on it right away.
        let host_file = HostFile::new("hello.txt", b"hello");
        let replies = monitor(&mut disk_ctrl, &format!("put {} a HELLO.TXT\n", host_file.quoted()));
        assert!(replies.contains("-> A:\\HELLO.TXT (5 bytes)"), "{}", replies);

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

        assert!(disk_ctrl.find_drive(0x00).is_some(), "A: must not be touched");
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
            &format!("swap b {}\nsave b {}\nsave a {}\n", target.quoted(), target.quoted(), target.quoted()),
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

        let replies = monitor(&mut disk_ctrl, &format!("put {} a HELLO.TXT\n", host_file.quoted()));
        assert!(replies.contains("-> A:\\HELLO.TXT (5 bytes)"), "{}", replies);

        let replies = monitor(&mut disk_ctrl, "put \"no such file.txt\" a\nput . b\n");
        assert!(replies.contains("Error: 'no such file.txt' not found"), "{}", replies);
        assert!(replies.contains("Error: B: is empty"), "{}", replies);
    }

    #[test]
    fn put_shows_the_name_the_file_has_in_dos() {
        let host_file = HostFile::new("longname.txt", b"hello");
        let mut disk_ctrl = DiskController::new();
        disk_ctrl.attach(0x00, formatted_floppy());

        // The host name is not a valid 8.3 name, so DOS sees a generated one.
        let replies = monitor(&mut disk_ctrl, &format!("put {} a\n", host_file.quoted()));
        assert!(replies.contains("-> A:\\EMU808~1.TXT (5 bytes)"), "{}", replies);

        let replies = monitor(&mut disk_ctrl, &format!("put {} a hello.txt\n", host_file.quoted()));
        assert!(replies.contains("-> A:\\HELLO.TXT (5 bytes)"), "{}", replies);

        disk_ctrl.find_drive(0x00).unwrap().readonly = true;
        let replies = monitor(&mut disk_ctrl, &format!("put {} a other.txt\n", host_file.quoted()));
        assert!(replies.contains("Error: disk is write-protected"), "{}", replies);
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
            assert!(!replies.contains("Warning"), "`{}` printed:\n{}", command, replies);
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
            let action = run_monitor(&mut disk_ctrl, &mut commands.as_bytes(), &mut out);
            assert_eq!(action, expected, "commands {:?}", commands);
        }
        assert!(disk_ctrl.find_drive(0x00).is_some(), "nothing is read after `reset`");
    }

    #[test]
    fn help_shows_how_to_get_files_onto_a_hard_disk_safely() {
        let mut out = Vec::new();
        run_monitor(&mut DiskController::new(), &mut "".as_bytes(), &mut out);
        let help = String::from_utf8(out).unwrap();
        assert!(help.contains("reset"), "{}", help);
        assert!(help.contains("COPY A:"), "{}", help);
        assert!(help.contains("double"), "{}", help);
    }
}
