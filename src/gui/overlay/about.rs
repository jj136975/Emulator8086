//! The About panel: what the keys do, which version this is, and what the
//! machine holds: the ROM it runs, the disk in each drive, whether it can be
//! written, and the order in which the BIOS tries the drives. It only reads.
//!
//! The invariant: the panel shows the machine as it is, asked of the
//! machine on every frame, not what the command line said: a ROM or a disk
//! another tool puts in is listed at once.

use egui::{Grid, Ui};

use emu8086_core::io::disk::DiskController;
use emu8086_core::io::disk_spec::drive_label;

use super::Cx;
use super::disks::{disk_size, image_place};
use crate::platform::OVERLAY_SWITCH;

/// What the overlay's switch does. A button of the pointer is the switch on
/// the picture only: over the tools it is theirs.
const SWITCH_EFFECT: &str = if OVERLAY_SWITCH.button.is_some() {
    "On the picture: shows or hides the tools"
} else {
    "Shows or hides the tools"
};

/// The overlay's switch, by the name the host gives it, and the keys the
/// window shares with the tools: what each does.
const KEYS: [(&str, &str); 3] = [
    (OVERLAY_SWITCH.name, SWITCH_EFFECT),
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
/// floppy drives, then the hard disks. A disk is described as the Disks
/// panel describes it.
fn drives(disks: &DiskController) -> Vec<Drive> {
    let mut held: Vec<_> = disks.drives.iter().collect();
    held.sort_by_key(|(number, _)| *number);
    held.into_iter()
        .map(|(number, image)| Drive {
            name: drive_label(*number),
            image: image_place(image),
            size: disk_size(*number, image.total_bytes()),
            access: if image.readonly {
                "read-only"
            } else {
                "read-write"
            },
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
    // Gaps, not separators, between the parts: a separator spans the
    // window, and the window takes the size of its contents.
    let gap = ui.spacing().item_spacing.y * 2.0;
    ui.heading("Emulator 8086");
    ui.label(format!("Version {}", env!("CARGO_PKG_VERSION")));

    ui.add_space(gap);
    ui.strong("Keys");
    Grid::new("about_keys").num_columns(2).show(ui, |ui| {
        for (key, effect) in KEYS {
            ui.label(key);
            ui.label(effect);
            ui.end_row();
        }
    });

    ui.add_space(gap);
    ui.strong("Machine");
    ui.label(format!("ROM: {}", cx.runtime.rom().unwrap_or("none")));
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
