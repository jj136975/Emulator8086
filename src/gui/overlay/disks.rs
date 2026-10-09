//! The Disks panel: every drive of the machine with the disk it holds
//! (where its image lives, its size, its geometry, whether it can be
//! written) and what a user does to the drive chosen there: put a disk in
//! (an image file, a blank disk, a new image file), take the disk out, move
//! it to another drive, save it, copy host files onto it.
//!
//! The panel does none of this itself. A disk tool needs the host's file
//! dialogs, its files and its clock, which are the app's to reach: each
//! button asks the app (`DiskCommand`), and the app says in the messages
//! what came of it. All the panel knows of the host is how its files are
//! reached (`platform::FILE_ACCESS`): it offers what that host can do, and
//! says what it cannot.
//!
//! The invariant: the panel shows the machine as it is, asked of the disk
//! controller on every frame: the drives and their disks, and whether a
//! hard disk was changed under the guest, which stays on the screen with
//! the way out of it, a reset, until the guest has booted again.

use egui::{Grid, Ui};

use emu8086_core::io::disk::{DiskController, DiskImage};
use emu8086_core::io::disk_spec::{FLOPPY_SIZES_KB, drive_label};

use super::theme::PALETTE;
use super::{Command, Cx};
use crate::platform::{self, FileAccess};

/// What the Disks panel asks the app to do with a drive, named by its BIOS
/// number. The app carries it out, with a file dialog first where the user
/// has to name a file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DiskCommand {
    /// Ask the user for a disk image and put it into the drive,
    /// write-protected if `read_only`.
    InsertImage { drive: u8, read_only: bool },
    /// Put a blank disk held in memory into the drive: a formatted floppy
    /// of `kb` KB into a floppy drive, a blank hard disk, which has one
    /// size and is given none, into a hard disk drive.
    InsertBlank {
        drive: u8,
        kb: Option<u64>,
        read_only: bool,
    },
    /// Ask the user where, create an image file there and put it into the
    /// drive.
    CreateImage { drive: u8 },
    /// Take the disk out of the drive.
    Eject { drive: u8 },
    /// Move the disk in `from` to `to`.
    Move { from: u8, to: u8 },
    /// Hand the disk to the user: in a file they name, or as a download.
    Save { drive: u8 },
    /// Ask the user for files and copy them onto the disk.
    PutFiles { drive: u8 },
    /// Ask the user for a folder and copy it onto the disk, with all that
    /// is below it.
    PutFolder { drive: u8 },
}

/// The first hard disk, as the BIOS numbers them.
const FIRST_HARD_DISK: u8 = 0x80;
/// The floppy drives: A: to D:.
const FLOPPY_DRIVES: u8 = 4;
/// The hard disks that are listed whether they hold a disk or not.
const HARD_DISKS_SHOWN: u8 = 2;
/// The last hard disk there can be: FFh ends the list of the boot order.
const LAST_HARD_DISK: u8 = 0xFE;
/// Characters of a path that are shown. A longer one is cut at its start,
/// where the directories are, and shown in full under the pointer.
const PATH_CHARS: usize = 40;

/// What is said while a reboot is due, a line each: the window takes the
/// width of the longest.
const REBOOT_DUE: [&str; 2] = [
    "A hard disk was changed while the guest runs. DOS does not notice:",
    "it goes on with what it holds in memory, and writes that over the change.",
];
/// The way out, beside its button, and the way around it.
const REBOOT_NOW: &str = "before the drive is used.";
const REBOOT_AVOIDED: &str =
    "To copy files without a reboot, put them on a floppy and COPY them in DOS.";

/// What a host says that hands files over by their contents, a line each.
const CONTENTS_HOST: [&str; 3] = [
    "A browser hands this page a copy of an image file: the disk is held in",
    "memory, and Download gives it back. No image file can be created from",
    "here, and no folder copied: insert a blank disk, and download it.",
];

/// A drive, as the panel lists it.
#[derive(Debug, PartialEq, Eq)]
struct Row {
    /// The BIOS number of the drive.
    number: u8,
    name: String,
    /// The disk in the drive, if it holds one.
    disk: Option<Held>,
}

/// The disk in a drive, as the panel lists it.
#[derive(Debug, PartialEq, Eq)]
struct Held {
    /// Where the image lives, in full.
    image: String,
    size: String,
    /// Cylinders, heads and sectors per track.
    geometry: String,
    access: &'static str,
}

/// Where the image of a disk lives, for the user: the path of its file,
/// or that it is held in memory, with the name of the file it was made of
/// if it was made of one.
pub(super) fn image_place(image: &DiskImage) -> String {
    match (image.backing_path(), image.name()) {
        // Windows spells a canonical path with a `\\?\` in front, which
        // says nothing to the reader.
        (Some(path), _) => path
            .display()
            .to_string()
            .trim_start_matches(r"\\?\")
            .to_string(),
        (None, Some(name)) => format!("{name} (in memory)"),
        (None, None) => "in memory".to_string(),
    }
}

/// The size of the disk of `bytes` in drive `number`. A floppy is known by
/// its size in KB, a hard disk in MB: the nearest one, since a hard disk is
/// whole cylinders and ends a few sectors short of the size it was made
/// for.
pub(super) fn disk_size(number: u8, bytes: u64) -> String {
    if number < FIRST_HARD_DISK {
        format!("{} KB", (bytes + (1 << 9)) >> 10)
    } else {
        format!("{} MB", (bytes + (1 << 19)) >> 20)
    }
}

/// The drives, in the order of their BIOS numbers: the four floppy drives,
/// then the hard disks, of which there are two at least and one more than
/// the last that holds a disk, so that there is always one to put a disk
/// into.
fn rows(disks: &DiskController) -> Vec<Row> {
    let last_held = disks
        .drives
        .iter()
        .map(|(number, _)| *number)
        .filter(|number| (FIRST_HARD_DISK..=LAST_HARD_DISK).contains(number))
        .max();
    let last_shown = match last_held {
        Some(last) => last.saturating_add(1).min(LAST_HARD_DISK),
        None => FIRST_HARD_DISK,
    }
    .max(FIRST_HARD_DISK + HARD_DISKS_SHOWN - 1);

    (0..FLOPPY_DRIVES)
        .chain(FIRST_HARD_DISK..=last_shown)
        .map(|number| Row {
            number,
            name: drive_label(number),
            disk: disks
                .drives
                .iter()
                .find(|(held, _)| *held == number)
                .map(|(_, image)| Held {
                    image: image_place(image),
                    size: disk_size(number, image.total_bytes()),
                    geometry: format!(
                        "{}/{}/{}",
                        image.cylinders, image.heads, image.sectors_per_track
                    ),
                    access: if image.readonly {
                        "read-only"
                    } else {
                        "read-write"
                    },
                }),
        })
        .collect()
}

/// `text` if it is short enough for its column, or else its end, where
/// the name of a file is, behind the mark of a cut.
fn cut_at_start(text: &str) -> String {
    let chars = text.chars().count();
    if chars <= PATH_CHARS {
        return text.to_string();
    }
    let kept: String = text.chars().skip(chars - (PATH_CHARS - 3)).collect();
    format!("...{kept}")
}

/// What a floppy of `kb` KB is called: by its KB below a megabyte, and
/// above it by the megabytes of a thousand KB that floppies are sold by.
fn floppy_size(kb: u64) -> String {
    if kb < 1000 {
        format!("{kb} KB")
    } else {
        format!("{} MB", kb as f64 / 1000.0)
    }
}

/// What the panel keeps from frame to frame, in egui's memory.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct View {
    /// The BIOS number of the drive the buttons act on: A: at first.
    selected: u8,
    /// Whether what is inserted is write-protected.
    protect: bool,
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    body_on(ui, cx, platform::FILE_ACCESS);
}

/// The panel on a host whose files are reached by `access`.
fn body_on(ui: &mut Ui, cx: &mut Cx<'_>, access: FileAccess) {
    let id = ui.make_persistent_id("disks_view");
    let mut view = ui.data_mut(|data| *data.get_temp_mut_or_default::<View>(id));
    let (rows, reboot_due) = {
        let disks = cx.runtime.disks.borrow();
        (rows(&disks), disks.reboot_due())
    };
    // A hard disk that is listed only for the disk it held goes with it.
    if !rows.iter().any(|row| row.number == view.selected) {
        view.selected = rows[0].number;
    }
    // Gaps, not separators, between the parts: a separator spans the
    // window, and the window takes the size of its contents.
    let gap = ui.spacing().item_spacing.y * 2.0;

    Grid::new("disks_drives")
        .num_columns(5)
        .striped(true)
        .show(ui, |ui| {
            for heading in ["Drive", "Image", "Size", "C/H/S", "Access"] {
                ui.strong(heading);
            }
            ui.end_row();
            for row in &rows {
                ui.radio_value(&mut view.selected, row.number, row.name.as_str());
                match &row.disk {
                    Some(disk) => {
                        let shown = cut_at_start(&disk.image);
                        let image = ui.label(&shown);
                        if shown != disk.image {
                            image.on_hover_text(&disk.image);
                        }
                        ui.label(&disk.size);
                        ui.label(&disk.geometry);
                        ui.label(disk.access);
                    }
                    None => {
                        ui.colored_label(PALETTE.off, "no disk");
                    }
                }
                ui.end_row();
            }
        });

    let selected = rows
        .iter()
        .find(|row| row.number == view.selected)
        .unwrap_or(&rows[0]);
    let drive = selected.number;
    let mut ask = |command: DiskCommand| cx.commands.push(Command::Disk(command));

    ui.add_space(gap);
    ui.horizontal(|ui| {
        ui.strong(format!("Put into {}", selected.name));
        let read_only = view.protect;
        if ui.button("Image file...").clicked() {
            ask(DiskCommand::InsertImage { drive, read_only });
        }
        if drive < FIRST_HARD_DISK {
            ui.menu_button("Blank disk", |ui| {
                for kb in FLOPPY_SIZES_KB {
                    if ui.button(floppy_size(kb)).clicked() {
                        ask(DiskCommand::InsertBlank {
                            drive,
                            kb: Some(kb),
                            read_only,
                        });
                    }
                }
            });
        } else if ui.button("Blank disk").clicked() {
            ask(DiskCommand::InsertBlank {
                drive,
                kb: None,
                read_only,
            });
        }
        if access == FileAccess::Paths && ui.button("New image file...").clicked() {
            ask(DiskCommand::CreateImage { drive });
        }
        ui.checkbox(&mut view.protect, "write-protected");
    });

    ui.add_enabled_ui(selected.disk.is_some(), |ui| {
        ui.horizontal(|ui| {
            ui.strong(format!("The disk in {}", selected.name));
            if ui.button("Eject").clicked() {
                ask(DiskCommand::Eject { drive });
            }
            ui.menu_button("Move to", |ui| {
                for other in rows.iter().filter(|other| other.number != drive) {
                    if ui.button(other.name.as_str()).clicked() {
                        ask(DiskCommand::Move {
                            from: drive,
                            to: other.number,
                        });
                    }
                }
            });
            let save = match access {
                FileAccess::Paths => "Save as...",
                FileAccess::Contents => "Download",
            };
            if ui.button(save).clicked() {
                ask(DiskCommand::Save { drive });
            }
            if ui.button("Put files...").clicked() {
                ask(DiskCommand::PutFiles { drive });
            }
            if access == FileAccess::Paths && ui.button("Put folder...").clicked() {
                ask(DiskCommand::PutFolder { drive });
            }
        });
    });

    if reboot_due {
        ui.add_space(gap);
        for line in REBOOT_DUE {
            ui.colored_label(PALETTE.warning, line);
        }
        ui.horizontal(|ui| {
            if ui.button("Reset the machine").clicked() {
                cx.commands.push(Command::Reset);
            }
            ui.label(REBOOT_NOW);
        });
        ui.label(REBOOT_AVOIDED);
    }

    if access == FileAccess::Contents {
        ui.add_space(gap);
        for line in CONTENTS_HOST {
            ui.label(line);
        }
    }

    ui.data_mut(|data| data.insert_temp(id, view));
}

#[cfg(test)]
#[path = "disks_tests.rs"]
mod tests;
