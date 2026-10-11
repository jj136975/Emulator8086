//! The Start panel, driven as a user drives it, on a host that came from a
//! site and on one that did not: what it offers, what each button asks of
//! the app and how it tells a finger from the pointer, and the panel while
//! a disk is on its way. Nobody carries the commands out here: what they do
//! to the machine, with the site's answers, is in the tests of
//! `gui::starter`, and what the session does with the overlay after a
//! start in `gui::session::tests`.

use std::sync::mpsc::channel;

use egui::accesskit::Role;

use super::*;
use crate::gui::overlay::Panel;
use crate::gui::overlay::testing::{Driver, Shell, first_finger_down, first_finger_up};
use crate::site::FILES;
use crate::wall_clock::HostClock;
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::vm::runtime::Runtime;

/// The labels of the buttons, as the user asked for them.
const BOOT: &str = "Boot MS-DOS 4.00";
const PLAY: &str = "Play DOOM";

/// Where each disk goes, as the user asked: the floppy into A:, tried
/// first, and the hard disk in as the first hard disk, tried before A:.
const FLOPPY_IN_A: Place = Place {
    drive: 0x00,
    boot_order: [0x00, 0x80],
    words: "the floppy in A:",
};
const HARD_DISK_AS_C: Place = Place {
    drive: 0x80,
    boot_order: [0x80, 0x00],
    words: "the hard disk as C:",
};

/// The width of a phone held upright, in points.
const PHONE_WIDTH: f32 = 360.0;

/// A machine without disks. It never runs.
fn machine() -> Runtime {
    let (_keys, key_events) = channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    Runtime::new(vec![], vec![], None, false, keyboard, &HostClock)
        .expect("a machine without disks has nothing to open")
}

/// The overlay shown, its first frame run, on a host that came from a site,
/// or not: `has_site`.
fn shown(has_site: bool) -> Driver<Shell> {
    let mut shell = Shell::on(machine(), has_site);
    shell.overlay.toggle();
    let mut shell = Driver::new(shell);
    shell.frame(Vec::new());
    shell
}

/// The Start panel open, on a host that came from a site.
fn panel() -> Driver<Shell> {
    let mut shell = shown(true);
    shell.toggle_in_view_menu("Start");
    shell
}

/// The commands asked for since the last look.
fn asked(shell: &mut Driver<Shell>) -> Vec<Command> {
    std::mem::take(&mut shell.target.commands)
}

/// The command a press of the button of the site's `index`th file asks for.
fn start(index: usize, place: Place, by_finger: bool) -> Command {
    Command::Start(StartCommand {
        disk: &FILES[index],
        place,
        by_finger,
    })
}

/// The panel has a button for each disk of the site, the floppy above the
/// hard disk, labelled with the disk's verb and title, and under each where
/// its disk goes. The ROM of the site is not among them. The window is
/// narrower than a phone held upright.
#[test]
fn the_panel_offers_each_disk_of_the_site_by_its_verb_and_title() {
    let shell = panel();
    let screen = shell.screen();
    let boot = screen.rect(Role::Button, BOOT);
    let play = screen.rect(Role::Button, PLAY);
    let to_a = screen.text_rect("Restarts with the floppy in A:");
    let as_c = screen.text_rect("Restarts with the hard disk as C:");
    let tops = [boot.top(), to_a.top(), play.top(), as_c.top()];
    assert!(tops.is_sorted(), "a button, then its line: {tops:?}");
    assert!(screen.enabled(Role::Button, BOOT) && screen.enabled(Role::Button, PLAY));
    assert!(!screen.has(Role::Button, FILES[0].title));
    assert!(
        !screen
            .texts()
            .iter()
            .any(|text| text.starts_with("Fetching"))
    );

    let labels: Vec<String> = disks().map(|(disk, _)| label(disk)).collect();
    assert_eq!(labels, [BOOT, PLAY], "one button per disk, and none more");
    let window = screen.rect(Role::Window, "Start");
    assert!(window.width() < PHONE_WIDTH, "{window:?}");
}

/// A click of a button asks the app, once, to start the machine with its
/// disk, where the disk's kind puts it, pressed with the pointer. The panel
/// stays as it was: whether the start happens is the app's to say.
#[test]
fn a_click_asks_for_a_start_with_the_pointer() {
    let mut shell = panel();
    shell.click(Role::Button, BOOT);
    assert_eq!(asked(&mut shell), [start(1, FLOPPY_IN_A, false)]);
    shell.click(Role::Button, PLAY);
    assert_eq!(asked(&mut shell), [start(2, HARD_DISK_AS_C, false)]);
    assert!(shell.screen().has(Role::Window, "Start"));
}

/// A tap of a finger on a button asks for the same start, as a finger's:
/// the press and the release of the pointer's button that came with the
/// finger are its echoes.
#[test]
fn a_tap_asks_for_a_start_with_a_finger() {
    let mut shell = panel();
    let at = shell.screen().rect(Role::Button, PLAY).center();
    shell.frame(first_finger_down(1, at));
    assert_eq!(asked(&mut shell), [], "a press is no click");
    shell.frame(first_finger_up(1, at));
    assert_eq!(asked(&mut shell), [start(2, HARD_DISK_AS_C, true)]);

    let at = shell.screen().rect(Role::Button, BOOT).center();
    shell.frame(first_finger_down(2, at));
    shell.frame(first_finger_up(2, at));
    assert_eq!(asked(&mut shell), [start(1, FLOPPY_IN_A, true)]);
}

/// While the disk of a start is on its way, the panel says which, and its
/// buttons are greyed out and ask for nothing. Once the app has done with
/// it, they are offered again and the line is gone.
#[test]
fn while_a_disk_is_on_its_way_the_panel_says_so_and_offers_nothing() {
    let mut shell = panel();
    shell.target.starting = Some(&FILES[1]);
    let screen = shell.wait(1.0);
    assert!(
        screen.shows("Fetching MS-DOS 4.00..."),
        "{:?}",
        screen.texts()
    );
    for button in [BOOT, PLAY] {
        assert!(screen.has(Role::Button, button), "{button} is there");
        assert!(!screen.enabled(Role::Button, button), "{button}");
    }
    shell.click(Role::Button, PLAY);
    shell.click(Role::Button, BOOT);
    assert_eq!(asked(&mut shell), [], "a greyed-out button asks nothing");

    shell.target.starting = None;
    let screen = shell.wait(1.0);
    assert!(!screen.shows("Fetching MS-DOS 4.00..."));
    assert!(screen.enabled(Role::Button, BOOT) && screen.enabled(Role::Button, PLAY));
}

/// Only a host that came from a site offers the Start panel, first in its
/// View menu; there the app opens it as the menu does. A host that came
/// from none, as the desktop the tests run on, offers no such panel, and an
/// opening asked for by the app leaves it shut.
#[test]
fn only_a_host_with_a_site_offers_the_start_panel() {
    for has_site in [true, false] {
        let mut shell = shown(has_site);
        shell.click(Role::Button, "View");
        let screen = shell.screen();
        assert_eq!(screen.has(Role::CheckBox, "Start"), has_site);
        if has_site {
            let top = |title| screen.rect(Role::CheckBox, title).top();
            assert!(top("Start") < top("About"), "first in the View menu");
        }
        shell.click(Role::Button, "View");

        shell.target.overlay.open(Panel::Start);
        assert_eq!(shell.wait(1.0).has(Role::Window, "Start"), has_site);
        shell.target.overlay.close(Panel::Start);
        assert!(!shell.wait(1.0).has(Role::Window, "Start"));
    }

    let mut desktop = Driver::new(Shell::new(machine()));
    desktop.target.overlay.toggle();
    desktop.frame(Vec::new());
    desktop.click(Role::Button, "View");
    assert!(desktop.screen().has(Role::CheckBox, "About"));
    assert!(!desktop.screen().has(Role::CheckBox, "Start"));
}
