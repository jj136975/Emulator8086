//! The About panel: what the keys do, which version this is, and what the
//! machine holds: the disk in each drive, whether it can be written, and the
//! order in which the BIOS tries the drives. It only reads.
//!
//! The invariant: the panel shows the machine as it is, asked of the disk
//! controller on every frame, not what the command line said: a disk another
//! tool puts into a drive is listed at once.

use egui::{Grid, Ui};

use emu8086_core::io::disk::DiskController;
use emu8086_core::io::disk_spec::drive_label;

use super::Cx;

/// The keys the window keeps or shares with the tools, and what each does.
const KEYS: [(&str, &str); 3] = [
    ("F12", "Shows or hides the tools"),
    ("Tab", "While the tools are shown: their next control"),
    (
        "Other keys",
        "To the guest, unless a menu or a control of the tools has them",
    ),
];

/// A drive that holds a disk, as the panel lists it.
#[derive(Debug, PartialEq, Eq)]
struct Drive {
    name: String,
    /// The host file the disk lives in, or that it lives in memory.
    image: String,
    size: String,
    access: &'static str,
}

/// The drives that hold a disk, in the order of their BIOS numbers: the
/// floppy drives, then the hard disks.
fn drives(disks: &DiskController) -> Vec<Drive> {
    let mut held: Vec<_> = disks.drives.iter().collect();
    held.sort_by_key(|(number, _)| *number);
    held.into_iter()
        .map(|(number, image)| {
            let bytes = image.total_bytes();
            Drive {
                name: drive_label(*number),
                image: match image.backing_path() {
                    // Windows spells a canonical path with a `\\?\` in
                    // front, which says nothing to the reader.
                    Some(path) => path
                        .display()
                        .to_string()
                        .trim_start_matches(r"\\?\")
                        .to_string(),
                    None => "in memory".to_string(),
                },
                // A floppy is known by its size in KB, a hard disk in MB:
                // the nearest one, since a hard disk is whole cylinders and
                // ends a few sectors short of the size it was made for.
                size: if *number < 0x80 {
                    format!("{} KB", (bytes + (1 << 9)) >> 10)
                } else {
                    format!("{} MB", (bytes + (1 << 19)) >> 20)
                },
                access: if image.readonly {
                    "read-only"
                } else {
                    "read-write"
                },
            }
        })
        .collect()
}

/// The drives the BIOS bootstrap tries, in order: `A:, HD0`.
fn boot_order(disks: &DiskController) -> String {
    let order: Vec<String> = disks
        .boot_order()
        .iter()
        .map(|drive| drive_label(*drive))
        .collect();
    order.join(", ")
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    ui.heading("Emulator 8086");
    ui.label(format!("Version {}", env!("CARGO_PKG_VERSION")));

    ui.separator();
    ui.strong("Keys");
    Grid::new("about_keys").num_columns(2).show(ui, |ui| {
        for (key, effect) in KEYS {
            ui.label(key);
            ui.label(effect);
            ui.end_row();
        }
    });

    ui.separator();
    ui.strong("Machine");
    let disks = cx.runtime.disks.borrow();
    let drives = drives(&disks);
    if drives.is_empty() {
        ui.label("No drive holds a disk");
    }
    Grid::new("about_drives")
        .num_columns(4)
        .striped(true)
        .show(ui, |ui| {
            for drive in &drives {
                ui.label(&drive.name);
                ui.label(&drive.image);
                ui.label(&drive.size);
                ui.label(drive.access);
                ui.end_row();
            }
        });
    ui.label(format!("Boot order: {}", boot_order(&disks)));
}

#[cfg(test)]
#[path = "about_tests.rs"]
mod tests;
