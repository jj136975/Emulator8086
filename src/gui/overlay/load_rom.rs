//! Machine > Load ROM: the ROM images the machine can restart with. The
//! BIOS built into the app, the ROM images the site publishes beside the
//! page (`site::FILES`), and an image the user chooses in the host's file
//! dialog. Each entry asks the app to restart the machine with its image
//! (`RomCommand`), and the app says in the messages which ROM runs then, or
//! why the machine runs on with the one it had.
//!
//! The invariant: the menu offers what the host has, and no more: the
//! site's images only on a host that came from a site
//! (`platform::HAS_SITE`).

use egui::Ui;

use super::{Command, Cx};
use crate::platform;
use crate::site::{Kind, SiteFile, of_kind};

/// What Machine > Load ROM asks the app to restart the machine with.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RomCommand {
    /// The BIOS built into the app.
    BuiltIn,
    /// A ROM image of the site's, fetched from it.
    FromSite(&'static SiteFile),
    /// A ROM image the user chooses in the host's file dialog.
    FromFile,
}

/// The menu, within the Machine menu, on the host the app runs on.
pub(super) fn menu(ui: &mut Ui, cx: &mut Cx<'_>) {
    menu_on(ui, cx, platform::HAS_SITE);
}

/// The menu on a host that came from a site, or not: `has_site`.
fn menu_on(ui: &mut Ui, cx: &mut Cx<'_>, has_site: bool) {
    ui.menu_button("Load ROM", |ui| {
        let mut ask = |command| cx.commands.push(Command::Rom(command));
        if ui.button("Built-in BIOS").clicked() {
            ask(RomCommand::BuiltIn);
        }
        if has_site {
            for file in of_kind(Kind::Rom) {
                if ui.button(file.title).clicked() {
                    ask(RomCommand::FromSite(file));
                }
            }
        }
        if ui.button("From a file...").clicked() {
            ask(RomCommand::FromFile);
        }
    });
}

#[cfg(test)]
#[path = "load_rom_tests.rs"]
mod tests;
