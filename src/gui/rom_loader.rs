//! The session's side of Machine > Load ROM: what the menu asks for
//! (`RomCommand`) becomes the image the machine restarts with. The built-in
//! BIOS is at hand; an image of the site's is fetched from it, and one of
//! the user's asked for in the host's file dialog, and either is loaded once
//! its answer has come (`poll`, at the start of every frame). The overlay's
//! messages say which ROM runs then, or why the machine runs on with the one
//! it had.
//!
//! The host is reached through `FileHost` and no other way (`gui::host`):
//! `platform` for the app, hand-made answers for a test.
//!
//! The invariants: the machine changes only for an image the core takes
//! (`Runtime::load_rom_image`), and then restarts at the reset vector with
//! its memory, its devices and its drives as they are, as Machine > Reset
//! restarts it. Nothing here waits: a dialog or a fetch is answered when
//! its answer comes, and one never answered costs nothing.

use std::sync::Arc;

use winit::window::Window;

use emu8086_core::io::disk_ops::HostFile;
use emu8086_core::vm::runtime::Runtime;
use emu8086_core::{BIOS_ROM, BIOS_ROM_NAME};

use super::host::{Awaited, FileHost, Platform};
use super::overlay::{Messages, RomCommand};
use crate::platform::{Fetched, FileQuestion, Wanted};
use crate::site::SiteFile;

/// What the dialog for a ROM image of the user's is headed with.
const DIALOG_TITLE: &str = "Restart the machine with a ROM image";

/// What carries out Machine > Load ROM: the host it asks for images, and
/// the dialogs and fetches whose answers it waits for.
pub struct RomLoader {
    host: Box<dyn FileHost>,
    /// The dialogs asked for whose answers have not been taken up.
    chosen: Awaited<(), Vec<HostFile>>,
    /// The files of the site asked for whose answers have not been taken
    /// up.
    fetched: Awaited<&'static SiteFile, Fetched>,
}

impl RomLoader {
    /// The loader on the host the app runs on. Its dialogs belong to no
    /// window until one is named (`set_window`).
    pub fn new() -> Self {
        Self::on(Box::new(Platform::windowless()))
    }

    /// The loader on `host`.
    pub(super) fn on(host: Box<dyn FileHost>) -> Self {
        Self {
            host,
            chosen: Awaited::default(),
            fetched: Awaited::default(),
        }
    }

    /// Names the window the machine is shown in: the dialogs asked for
    /// from now on belong to it.
    pub fn set_window(&mut self, window: &Arc<Window>) {
        self.host = Box::new(Platform::of(window));
    }

    /// Restarts `runtime` with the image `command` names, or asks the host
    /// for it first, and says in `messages` what came of it.
    pub fn execute(&mut self, command: RomCommand, runtime: &mut Runtime, messages: &mut Messages) {
        match command {
            RomCommand::BuiltIn => restart(runtime, messages, |runtime| {
                runtime.load_rom_image(BIOS_ROM, BIOS_ROM_NAME)
            }),
            RomCommand::FromSite(file) => {
                let answer = self.host.fetch(file.path);
                self.fetched.push(file, answer);
            }
            RomCommand::FromFile => {
                let question = FileQuestion {
                    title: DIALOG_TITLE.to_string(),
                    wanted: Wanted::Image,
                };
                let answer = self.host.ask(question);
                self.chosen.push((), answer);
            }
        }
    }

    /// Takes up the answers that have arrived since the last call, and
    /// restarts the machine with the image each one brought. A dialog the
    /// user closed without choosing asked for nothing.
    pub fn poll(&mut self, runtime: &mut Runtime, messages: &mut Messages) {
        for ((), files) in self.chosen.answered() {
            let Some(files) = files else {
                messages.failed("The file dialog could not be shown");
                continue;
            };
            // The dialog asks for one file.
            let Some(file) = files.into_iter().next() else {
                continue;
            };
            restart(runtime, messages, |runtime| match file {
                HostFile::Path(path) => runtime.load_rom(path),
                HostFile::Contents { name, bytes } => runtime.load_rom_image(&bytes, name),
            });
        }
        for (file, fetched) in self.fetched.answered() {
            restart(runtime, messages, |runtime| match fetched {
                Some(Ok(image)) => runtime.load_rom_image(&image, from_the_site(file)),
                // The host's own words, which name the file.
                Some(Err(why)) => Err(why),
                None => Err(format!("The site could not be asked for {}", file.path)),
            });
        }
    }
}

/// What the user knows an image of the site's by, once it is loaded.
fn from_the_site(file: &SiteFile) -> String {
    format!("{} from the site", file.path)
}

/// Restarts `runtime` with the ROM `load` puts in its socket, and says in
/// `messages` which ROM runs now. A `load` that fails leaves the machine as
/// it was, and `messages` says why, and which ROM it runs on with.
fn restart(
    runtime: &mut Runtime,
    messages: &mut Messages,
    load: impl FnOnce(&mut Runtime) -> Result<(), String>,
) {
    match load(runtime) {
        Ok(()) => {
            runtime.reset();
            let rom = runtime.rom().unwrap_or("the new ROM");
            messages.done(format!("Restarted with {rom}"));
        }
        Err(why) => {
            let unchanged = match runtime.rom() {
                Some(rom) => format!("The machine runs on with {rom}"),
                None => "The machine runs on as it was".to_string(),
            };
            messages.failed(format!("{why}. {unchanged}"));
        }
    }
}

#[cfg(test)]
#[path = "rom_loader_tests.rs"]
mod tests;
