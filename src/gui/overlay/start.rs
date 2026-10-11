//! The Start panel: the machine started with a disk of the site, in one
//! press. A button for each disk the site publishes (`site::FILES`): the
//! floppies, then the hard disks, each kind in the order of the table, the
//! button labelled with the disk's verb and title ("Boot MS-DOS 4.00"), and
//! under it where the disk goes. A press asks the app to start the machine
//! with the disk (`StartCommand`), which the app fetches from the site,
//! puts into its drive and restarts the machine from (`gui::starter`).
//! Until the disk has come the panel says so, and offers nothing more.
//!
//! The panel is a page's: only a host that came from a site offers it
//! (`Panel::needs_site`).
//!
//! The invariant: the panel fetches nothing and changes no drive itself. It
//! asks, with where the disk is to go (`Place`, which the disk's kind
//! decides) and with how its button was pressed: a finger is a touch
//! screen's, whose user has no keys at hand.

use egui::{Button, Event, RichText, Ui};

use super::theme::METRICS;
use super::{Command, Cx, with_echoes};
use crate::site::{Kind, SiteFile, of_kind};

/// What the Start panel asks the app: to start the machine with a disk of
/// the site.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StartCommand {
    /// The disk, a row of the site's table.
    pub disk: &'static SiteFile,
    /// Where it goes.
    pub place: Place,
    /// Whether a finger pressed the button: a touch screen's user, who
    /// types on the on-screen keyboard.
    pub by_finger: bool,
}

impl StartCommand {
    /// What the button of `disk` asks for, pressed by a finger or not: how a
    /// test of the app's side asks for a start without the panel.
    #[cfg(test)]
    pub fn of(disk: &'static SiteFile, by_finger: bool) -> Self {
        let place = place(disk.kind).expect("a disk of the site");
        Self {
            disk,
            place,
            by_finger,
        }
    }
}

/// Where a disk of the site goes, and so what the machine boots.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Place {
    /// The drive it goes into, by its BIOS number, in place of the disk
    /// that may be there.
    pub drive: u8,
    /// The drives the bootstrap tries then, in order: that one first.
    pub boot_order: [u8; 2],
    /// The disk and where the guest finds it, in words for the user: under
    /// the disk's button, and once the machine has started with it.
    pub words: &'static str,
}

/// Where a disk of `kind` goes: a floppy into A:, the drive a PC boots
/// first, the first hard disk tried after it; a hard disk in as the first
/// hard disk, which DOS calls C:, tried before A:. A ROM is no disk.
fn place(kind: Kind) -> Option<Place> {
    match kind {
        Kind::Floppy => Some(Place {
            drive: 0x00,
            boot_order: [0x00, 0x80],
            words: "the floppy in A:",
        }),
        Kind::HardDisk => Some(Place {
            drive: 0x80,
            boot_order: [0x80, 0x00],
            words: "the hard disk as C:",
        }),
        Kind::Rom => None,
    }
}

/// The disks of the site, each with where it goes, in the order the panel
/// offers them: the floppies, then the hard disks, each kind in the order
/// of the table.
fn disks() -> impl Iterator<Item = (&'static SiteFile, Place)> {
    [Kind::Floppy, Kind::HardDisk]
        .into_iter()
        .flat_map(of_kind)
        .filter_map(|disk| Some((disk, place(disk.kind)?)))
}

/// The label of the button of `disk`: its verb, then its title.
fn label(disk: &SiteFile) -> String {
    match disk.verb {
        Some(verb) => format!("{verb} {}", disk.title),
        None => disk.title.to_string(),
    }
}

/// Whether the click that a frame's `events` complete was a finger's: the
/// release of the primary button among them is a finger's echo
/// (`with_echoes`). A click of the pointer, or of a key, is not.
fn by_finger(events: &[Event]) -> bool {
    with_echoes(events)
        .any(|(event, echo)| echo && matches!(event, Event::PointerButton { pressed: false, .. }))
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    // Gaps, not separators, between the disks: a separator spans the
    // window, and the window takes the size of its contents.
    let gap = ui.spacing().item_spacing.y * 2.0;
    // One start at a time: while one is on its way, nothing is offered.
    let idle = cx.starting.is_none();
    for (index, (disk, place)) in disks().enumerate() {
        if index > 0 {
            ui.add_space(gap);
        }
        let button =
            Button::new(RichText::new(label(disk)).heading()).min_size(METRICS.start_button);
        if ui.add_enabled(idle, button).clicked() {
            let by_finger = ui.input(|input| by_finger(&input.events));
            cx.commands.push(Command::Start(StartCommand {
                disk,
                place,
                by_finger,
            }));
        }
        ui.label(format!("Restarts with {}", place.words));
    }
    if let Some(disk) = cx.starting {
        ui.add_space(gap);
        ui.label(format!("Fetching {}...", disk.title));
    }
}

#[cfg(test)]
#[path = "start_tests.rs"]
mod tests;
