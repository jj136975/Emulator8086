//! The F12 monitor: a command prompt on the console, run while the guest is
//! stopped, that works on the drives (`swap`, `new`, `move`, `save`, `eject`,
//! `put`) and ends in a [`MonitorAction`], resume or reset, the one thing it
//! hands back to the runtime. Only [`enter_monitor`] touches the terminal;
//! the commands read and write the streams they are given, which is how the
//! tests drive them.
//!
//! The invariant: a command that may have changed a hard disk tells the user
//! to reboot the guest, and one that cannot have changed one says nothing.
//! DOS never re-reads a fixed disk: it writes the FAT and directory sectors
//! in its buffers back over whatever the host changed.

use crate::io::disk::{
    DiskController, DiskImage, DiskSource, FLOPPY_144_SIZE, HD_DEFAULT_SIZE_MB, SaveOutcome,
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
        say!(
            out,
            "         to a hard disk while it runs and overwrites them from its buffers."
        );
        say!(
            out,
            "         Reboot the guest (type 'reset') before the drive is used."
        );
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

    let hd_count = disk_ctrl
        .drives
        .iter()
        .filter(|(id, _)| *id >= 0x80)
        .count();
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
                    i,
                    size_mb,
                    d.cylinders,
                    d.heads,
                    d.sectors_per_track,
                    ro
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
    say!(
        out,
        "  swap <drive> <image>[:ro]  Insert an existing disk image (e.g. swap a disk2.img)"
    );
    say!(
        out,
        "  swap <drive> memory[:KB]   Insert an empty disk held in memory (floppy: formatted,"
    );
    say!(
        out,
        "                             KB one of 160 180 320 360 720 1200 1440 2880, default"
    );
    say!(
        out,
        "                             1440; hard disk: blank 32MB)"
    );
    say!(
        out,
        "  new <drive> <path>         Create an image file and insert it"
    );
    say!(
        out,
        "                             (floppy: formatted 1.44M, hard disk: blank 32MB)"
    );
    say!(
        out,
        "  move <from> <to>           Move disk between drives (e.g. move b a)"
    );
    say!(
        out,
        "  save <drive> <path>        Save disk contents to file"
    );
    say!(out, "  eject <drive>              Eject disk from drive");
    say!(
        out,
        "  put <file> <drive> [name]  Copy host file to FAT disk"
    );
    say!(
        out,
        "  put <dir> <drive>          Copy host directory to FAT disk recursively"
    );
    say!(out, "  reset                      Reboot the guest");
    say!(
        out,
        "  resume                     Resume emulation (or just press Enter)"
    );
    say!(out);
    say!(
        out,
        "Drives are a-d and hd0, hd1, ... A path that contains blanks goes in double"
    );
    say!(out, "quotes.");
    say!(out);
    say!(
        out,
        "DOS does not notice changes to a hard disk while it runs: after put, swap,"
    );
    say!(
        out,
        "eject or move on hdN, reboot the guest with 'reset'. To copy files without"
    );
    say!(
        out,
        "a reboot, go through a floppy: swap a memory, put <file> a, resume, then"
    );
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

fn swap(
    disk_ctrl: &mut DiskController,
    arguments: &str,
    out: &mut dyn Write,
) -> Result<(), String> {
    let args = split_args(arguments, 2)?;
    let [drive, source] = args.as_slice() else {
        return Err("usage: swap <drive> <image>[:ro] | memory[:KB]".to_string());
    };
    let drive = parse_drive(drive)?;
    let label = drive_label(drive);
    let cannot_open =
        |path: &Path, error: std::io::Error| format!("cannot open '{}': {}", path.display(), error);

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
                    label,
                    HD_DEFAULT_SIZE_MB
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
                let image =
                    DiskImage::open(&path, readonly).map_err(|error| cannot_open(&path, error))?;
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
fn create(
    disk_ctrl: &mut DiskController,
    arguments: &str,
    out: &mut dyn Write,
) -> Result<(), String> {
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

fn move_disk(
    disk_ctrl: &mut DiskController,
    arguments: &str,
    out: &mut dyn Write,
) -> Result<(), String> {
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

fn save(
    disk_ctrl: &mut DiskController,
    arguments: &str,
    out: &mut dyn Write,
) -> Result<(), String> {
    let args = split_args(arguments, 2)?;
    let [drive, target] = args.as_slice() else {
        return Err("usage: save <drive> <path>".to_string());
    };
    let drive = parse_drive(drive)?;
    let label = drive_label(drive);
    let target = Path::new(target);

    if let Some(other) = disk_ctrl.drive_backed_by(target)
        && other != drive
    {
        return Err(format!(
            "{} is the image file of {}; eject that drive first",
            target.display(),
            drive_label(other)
        ));
    }
    let disk = disk_ctrl
        .find_drive(drive)
        .ok_or_else(|| format!("{} is empty", label))?;
    match disk
        .save_to_file(target)
        .map_err(|error| error.to_string())?
    {
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

fn eject(
    disk_ctrl: &mut DiskController,
    arguments: &str,
    out: &mut dyn Write,
) -> Result<(), String> {
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
            format!(
                "Copied '{}' -> {}\\{} ({} bytes)",
                host_path,
                label,
                stored,
                data.len()
            )
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
#[path = "cli_tests.rs"]
mod tests;
