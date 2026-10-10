//! Machine > Load ROM on a host that came from a site and on one that did
//! not, driven as a user drives it: what it offers, and what each entry
//! asks of the app. Nobody carries the commands out here: what they do to
//! the machine, with the host's dialogs and its site, is in the tests of
//! `gui::rom_loader`.

use std::sync::mpsc::channel;

use egui::accesskit::Role;

use super::*;
use crate::gui::meter::Meter;
use crate::gui::overlay::Messages;
use crate::gui::overlay::testing::{Driver, Passes, Shell, submenu};
use crate::gui::overlay::theme;
use crate::site::FILES;
use crate::wall_clock::HostClock;
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::vm::runtime::Runtime;

/// A machine without disks. It never runs.
fn machine() -> Runtime {
    let (_keys, key_events) = channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    Runtime::new(vec![], vec![], None, false, keyboard, &HostClock)
        .expect("a machine without disks has nothing to open")
}

/// A menu bar with a Machine menu that holds Load ROM and nothing else, on
/// a host that came from a site, or not: `has_site`. The shell shows the
/// menu of the host the tests run on, which has no site.
struct OnHost {
    has_site: bool,
    ctx: egui::Context,
    runtime: Runtime,
    meter: Meter,
    commands: Vec<Command>,
    messages: Messages,
}

impl OnHost {
    fn new(has_site: bool) -> Driver<Self> {
        let ctx = egui::Context::default();
        theme::apply(&ctx);
        let mut host = Driver::new(Self {
            has_site,
            ctx,
            runtime: machine(),
            meter: Meter::default(),
            commands: Vec::new(),
            messages: Messages::default(),
        });
        host.frame(Vec::new());
        host
    }
}

impl Passes for OnHost {
    fn context(&self) -> &egui::Context {
        &self.ctx
    }

    fn pass(&mut self, input: egui::RawInput) -> egui::PlatformOutput {
        let mut cx = Cx {
            runtime: &mut self.runtime,
            paused: false,
            meter: &self.meter,
            drawn_before: true,
            commands: &mut self.commands,
            messages: &mut self.messages,
        };
        let has_site = self.has_site;
        let output = self.ctx.run_ui(input, |ui| {
            egui::Panel::top("menu_bar").show(ui, |ui| {
                egui::MenuBar::new().ui(ui, |ui| {
                    ui.menu_button("Machine", |ui| menu_on(ui, &mut cx, has_site));
                });
            });
        });
        let egui::FullOutput {
            platform_output,
            mut textures_delta,
            ..
        } = output;
        // Nothing draws here: the texture changes are dropped on purpose.
        textures_delta.clear();
        platform_output
    }
}

/// Opens Machine > Load ROM.
fn open_menu<T: Passes>(driver: &mut Driver<T>) {
    driver.click(Role::Button, "Machine");
    driver.click(Role::Button, &submenu("Load ROM"));
}

/// On a host that came from a site, Load ROM offers the built-in BIOS, each
/// ROM image of the site under its title, and a file of the user's, in that
/// order; each entry asks the app for its command, once, and closes the
/// menu.
#[test]
fn a_host_with_a_site_offers_the_sites_roms() {
    let mut page = OnHost::new(true);
    let site_rom = &FILES[0];
    assert_eq!(site_rom.title, "BIOS from the site");

    open_menu(&mut page);
    let entries = ["Built-in BIOS", site_rom.title, "From a file..."];
    let tops: Vec<f32> = entries
        .iter()
        .map(|entry| page.screen().rect(Role::Button, entry).top())
        .collect();
    assert!(tops.is_sorted(), "{entries:?} in that order: {tops:?}");
    assert_eq!(page.target.commands, [], "opening the menus asks nothing");
    page.click(Role::Button, "Machine");
    assert!(!page.screen().has(Role::Button, "Built-in BIOS"), "closed");

    for (entry, command) in [
        ("Built-in BIOS", RomCommand::BuiltIn),
        (site_rom.title, RomCommand::FromSite(site_rom)),
        ("From a file...", RomCommand::FromFile),
    ] {
        open_menu(&mut page);
        page.click(Role::Button, entry);
        assert_eq!(
            std::mem::take(&mut page.target.commands),
            [Command::Rom(command)],
            "{entry}"
        );
        assert!(!page.screen().has(Role::Button, entry), "the menu closed");
    }
}

/// On a host that came from no site, Load ROM offers the built-in BIOS and
/// a file of the user's, and nothing of a site.
#[test]
fn a_host_without_a_site_offers_none_of_its_files() {
    let mut program = OnHost::new(false);
    open_menu(&mut program);
    let screen = program.screen();
    assert!(screen.has(Role::Button, "Built-in BIOS"));
    assert!(screen.has(Role::Button, "From a file..."));
    for file in &FILES {
        assert!(!screen.has(Role::Button, file.title), "{}", file.title);
    }
}

/// The Machine menu of the app has Load ROM, between Reset and Dump VGA
/// state, on the host the tests run on: a desktop, which came from no
/// site.
#[test]
fn the_machine_menu_has_load_rom_on_this_host() {
    let mut shell = Shell::new(machine());
    shell.overlay.toggle();
    let mut shell = Driver::new(shell);
    shell.frame(Vec::new());

    shell.click(Role::Button, "Machine");
    let top = |shell: &Driver<Shell>, label: &str| shell.screen().rect(Role::Button, label).top();
    let load_rom = top(&shell, &submenu("Load ROM"));
    assert!(top(&shell, "Reset") < load_rom && load_rom < top(&shell, "Dump VGA state"));

    shell.click(Role::Button, &submenu("Load ROM"));
    assert!(shell.screen().has(Role::Button, "Built-in BIOS"));
    assert!(!shell.screen().has(Role::Button, FILES[0].title));
    shell.click(Role::Button, "From a file...");
    assert_eq!(shell.target.commands, [Command::Rom(RomCommand::FromFile)]);
}
