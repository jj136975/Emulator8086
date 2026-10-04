//! The shell without a window: an `Overlay` driven frame by frame with
//! hand-made input (`testing`), and what each pass shows and asks for.
//!
//! The invariant: time is the input's. A test states when each frame is;
//! nothing here waits or reads a clock, and the machine the shell is lent
//! never runs.

use egui::accesskit::Role;

use super::messages::{DONE_SECONDS, FAILED_SECONDS};
use super::testing::{Driver, Shell, key};
use super::*;
use crate::wall_clock::HostClock;
use emu8086_core::io::disk::DiskImage;
use emu8086_core::io::disk_spec::parse_disk_specs;
use emu8086_core::io::keyboard::KeyboardController;

/// A shell as the app starts it: hidden, no frame run yet. Its machine has
/// a floppy in A: and a hard disk, both in memory.
fn shell() -> Driver<Shell> {
    let (_keys, key_events) = std::sync::mpsc::channel();
    let runtime = Runtime::new(
        parse_disk_specs(&["a:memory".to_string()]).unwrap(),
        vec!["memory".to_string()],
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .unwrap();
    Driver::new(Shell::new(runtime))
}

/// A shell whose overlay is shown and has had its first frame.
fn shown() -> Driver<Shell> {
    let mut shell = shell();
    shell.target.overlay.toggle();
    shell.frame(Vec::new());
    shell
}

/// Two frames: what a frame shows for the first time is laid out in the
/// first and on the screen in the second.
fn settle(shell: &mut Driver<Shell>) {
    shell.frame(Vec::new());
    shell.frame(Vec::new());
}

/// The commands asked for since the last look.
fn asked(shell: &mut Driver<Shell>) -> Vec<Command> {
    std::mem::take(&mut shell.target.commands)
}

const NOTHING: [Command; 0] = [];

/// The number of meshes the last pass left to draw.
fn painted(overlay: &mut Overlay) -> usize {
    let mut paint = overlay.take_paint();
    // Nothing draws here: the texture changes are dropped on purpose.
    paint.textures.clear();
    paint.primitives.len()
}

/// The overlay starts hidden. Its key shows it, with the menu bar, and
/// hides it again.
#[test]
fn the_overlay_starts_hidden_and_its_key_toggles_it() {
    let mut shell = shell();
    assert!(!shell.target.overlay.visible());
    assert!(!shell.frame(Vec::new()).has(Role::Button, "Machine"));

    shell.target.overlay.toggle();
    assert!(shell.target.overlay.visible());
    assert!(shell.frame(Vec::new()).has(Role::Button, "Machine"));
    assert!(painted(&mut shell.target.overlay) > 0);

    shell.target.overlay.toggle();
    assert!(!shell.target.overlay.visible());
    assert!(!shell.frame(Vec::new()).has(Role::Button, "Machine"));
    assert_eq!(painted(&mut shell.target.overlay), 0);
}

/// Hidden, the overlay shows the hint for the first seconds, counted from
/// its first frame wherever egui's clock stands then. After that there is
/// nothing to show: `draws` says so, and a pass run all the same paints
/// nothing.
#[test]
fn the_hint_shows_for_the_first_seconds_only() {
    let mut shell = shell();
    assert!(shell.target.overlay.draws(), "the hint is yet to be shown");

    shell.wait(1000.0);
    shell.frame(Vec::new());
    assert!(shell.screen().shows(HINT));
    assert!(painted(&mut shell.target.overlay) > 0);

    shell.wait(HINT_SECONDS - 1.0);
    assert!(shell.screen().shows(HINT), "its time is not over");
    assert!(shell.target.overlay.draws());

    shell.wait(1.0);
    assert!(!shell.screen().shows(HINT));
    assert!(!shell.target.overlay.draws(), "nothing is left to show");
    assert_eq!(painted(&mut shell.target.overlay), 0);
}

/// The key ends the hint for good: whoever pressed it has found it.
#[test]
fn the_key_ends_the_hint() {
    let mut shell = shell();
    settle(&mut shell);
    assert!(shell.screen().shows(HINT));

    shell.target.overlay.toggle();
    settle(&mut shell);
    assert!(!shell.screen().shows(HINT), "the overlay is shown instead");

    shell.target.overlay.toggle();
    assert!(!shell.target.overlay.draws());
    assert!(!shell.frame(Vec::new()).shows(HINT));
}

/// Each entry of the Machine menu asks the app for its command, once, and
/// closes the menu. Opening the menu asks for nothing.
#[test]
fn each_machine_action_asks_for_its_command() {
    let mut shell = shown();
    for (entry, command) in [
        ("Pause", Command::Pause),
        ("Reset", Command::Reset),
        ("Dump VGA state", Command::DumpVga),
    ] {
        shell.click(Role::Button, "Machine");
        assert_eq!(asked(&mut shell), NOTHING, "opening the menu");

        shell.click(Role::Button, entry);
        assert_eq!(asked(&mut shell), [command], "{entry}");
        assert!(!shell.screen().has(Role::Button, entry), "the menu closed");
    }
}

/// A paused machine is marked in the menu bar, and its Machine menu offers
/// to resume it where a running machine's offers to pause.
#[test]
fn a_paused_machine_is_marked_and_can_be_resumed() {
    let mut shell = shown();
    assert!(!shell.screen().shows(PAUSED));
    shell.click(Role::Button, "Machine");
    assert!(shell.screen().has(Role::Button, "Pause"));
    assert!(!shell.screen().has(Role::Button, "Resume"));
    shell.click(Role::Button, "Pause");
    assert!(
        !shell.screen().shows(PAUSED),
        "the shell asks; the app has not paused the machine"
    );

    shell.target.paused = true;
    assert!(shell.frame(Vec::new()).shows(PAUSED));
    shell.click(Role::Button, "Machine");
    assert!(shell.screen().has(Role::Button, "Resume"));
    assert!(!shell.screen().has(Role::Button, "Pause"));
    asked(&mut shell);
    shell.click(Role::Button, "Resume");
    assert_eq!(asked(&mut shell), [Command::Resume]);

    shell.target.paused = false;
    assert!(!shell.frame(Vec::new()).shows(PAUSED));
}

/// A message shows from the first pass after it is reported and for a few
/// seconds, a failure for longer. Reported while the overlay is hidden, it
/// has its whole time once the overlay is shown.
#[test]
fn a_message_has_its_time_on_the_screen() {
    let mut shell = shown();
    shell.target.overlay.messages().done("disk saved");
    shell.target.overlay.messages().failed("no such file");
    settle(&mut shell);
    assert!(shell.screen().shows("disk saved"));
    assert!(shell.screen().shows("no such file"));

    shell.wait(DONE_SECONDS);
    assert!(!shell.screen().shows("disk saved"));
    assert!(shell.screen().shows("no such file"), "a failure stays");
    shell.wait(FAILED_SECONDS - DONE_SECONDS);
    assert!(!shell.screen().shows("no such file"));

    shell.target.overlay.toggle();
    shell.target.overlay.messages().done("reported unseen");
    shell.wait(60.0);
    assert!(!shell.screen().shows("reported unseen"), "hidden");
    shell.target.overlay.toggle();
    settle(&mut shell);
    assert!(shell.screen().shows("reported unseen"));
    shell.wait(DONE_SECONDS - 1.0);
    assert!(shell.screen().shows("reported unseen"));
    shell.wait(1.0);
    assert!(!shell.screen().shows("reported unseen"));
}

/// The keyboard is the overlay's while it is shown and either one of its
/// menus is open or one of its widgets has the focus. Escape closes a menu,
/// Tab moves the focus into the overlay, and a click on a menu leaves no
/// focus behind.
#[test]
fn the_overlay_has_the_keyboard_with_an_open_menu_or_the_focus() {
    let mut shell = shown();
    assert!(!shell.target.overlay.wants_keyboard());

    shell.click(Role::Button, "Machine");
    assert!(shell.target.overlay.wants_keyboard(), "a menu is open");
    shell.frame(vec![key(egui::Key::Escape, true)]);
    shell.frame(vec![key(egui::Key::Escape, false)]);
    assert!(
        !shell.screen().has(Role::Button, "Reset"),
        "Escape closed it"
    );
    assert!(
        !shell.target.overlay.wants_keyboard(),
        "a click gives no focus"
    );

    shell.frame(vec![key(egui::Key::Tab, true)]);
    shell.frame(vec![key(egui::Key::Tab, false)]);
    assert!(shell.target.overlay.wants_keyboard(), "Tab gave the focus");

    shell.target.overlay.toggle();
    assert!(!shell.target.overlay.wants_keyboard(), "hidden");
}

/// A key the guest gets is not a shortcut of the overlay as well. egui
/// would zoom its whole UI on Ctrl with `-` or `+`, and copy the text
/// selected in a label on Ctrl+C: the first is off, and there is no text
/// to select.
#[test]
fn the_overlay_has_no_shortcut_on_keys_the_guest_gets() {
    let mut shell = shown();
    let ctrl_minus = egui::Event::Key {
        key: egui::Key::Minus,
        physical_key: None,
        pressed: true,
        repeat: false,
        modifiers: egui::Modifiers::CTRL | egui::Modifiers::COMMAND,
    };
    shell.frame(vec![ctrl_minus]);
    shell.frame(Vec::new());
    let ctx = shell.target.overlay.context();
    assert_eq!(ctx.zoom_factor(), 1.0);
    for theme in [egui::Theme::Dark, egui::Theme::Light] {
        assert!(!ctx.style_of(theme).interaction.selectable_labels);
    }
}

/// The panel table is complete. `ALL` lists every variant, once; every
/// panel has a title of its own, a checkbox in the View menu and a window
/// with a body, which the checkbox opens and closes and which the View menu
/// shows open or closed.
#[test]
fn every_panel_has_a_title_a_checkbox_and_a_window() {
    // One arm per variant: a new variant does not compile until it is
    // named here, and is then looked for in `ALL`.
    fn listed(panel: Panel) -> bool {
        match panel {
            Panel::About => Panel::ALL.contains(&panel),
            Panel::Memory => Panel::ALL.contains(&panel),
            Panel::Interrupts => Panel::ALL.contains(&panel),
            Panel::Timer => Panel::ALL.contains(&panel),
            Panel::Video => Panel::ALL.contains(&panel),
            Panel::Keyboard => Panel::ALL.contains(&panel),
            Panel::DebugPort => Panel::ALL.contains(&panel),
        }
    }
    for panel in [
        Panel::About,
        Panel::Memory,
        Panel::Interrupts,
        Panel::Timer,
        Panel::Video,
        Panel::Keyboard,
        Panel::DebugPort,
    ] {
        assert!(listed(panel), "{panel:?} is in ALL");
    }

    let mut shell = shown();
    for (index, panel) in Panel::ALL.into_iter().enumerate() {
        let title = panel.title();
        assert!(!title.is_empty());
        for earlier in &Panel::ALL[..index] {
            assert_ne!(*earlier, panel, "{title} is listed twice");
            assert_ne!(earlier.title(), title, "two panels share a title");
        }

        assert!(!shell.screen().has(Role::Window, title), "closed at first");
        shell.click(Role::Button, "View");
        assert!(!shell.screen().ticked(title));
        shell.click(Role::Button, "View");

        shell.toggle_in_view_menu(title);
        assert!(shell.screen().has(Role::Window, title), "{title} opened");
        shell.click(Role::Button, "View");
        assert!(shell.screen().ticked(title));
        shell.click(Role::Button, "View");

        shell.toggle_in_view_menu(title);
        assert!(!shell.screen().has(Role::Window, title), "{title} closed");
    }
}

/// The window of a panel is closed by its own button as well, and the View
/// menu then shows the panel closed.
#[test]
fn a_panel_closed_by_its_window_is_unticked_in_the_view_menu() {
    let mut shell = shown();
    shell.toggle_in_view_menu("About");
    assert!(shell.screen().has(Role::Window, "About"));

    shell.click(Role::Button, "Close window");
    shell.wait(1.0);
    assert!(!shell.screen().has(Role::Window, "About"));
    shell.click(Role::Button, "View");
    assert!(!shell.screen().ticked("About"));
}

/// The About panel shows the version, the keys and the machine as it is on
/// that frame: its drives, their images and the boot order. A disk put in a
/// drive while the panel is open is listed on the next frame.
#[test]
fn the_about_panel_shows_the_keys_and_the_machine_as_it_is() {
    let mut shell = shown();
    shell.toggle_in_view_menu("About");
    let screen = shell.screen();
    let version = format!("Version {}", env!("CARGO_PKG_VERSION"));
    for text in [
        version.as_str(),
        "F12",
        "Shows or hides the tools",
        "A:",
        "in memory",
        "1440 KB",
        "read-write",
        "HD0",
        "32 MB",
        "Boot order: A:, HD0",
    ] {
        assert!(screen.shows(text), "{text:?} in {:?}", screen.texts());
    }
    assert!(!screen.shows("B:"));

    let floppy = DiskImage::new_in_memory_sized(720 * 1024, true);
    shell.target.runtime.disks.borrow_mut().attach(0x01, floppy);
    let screen = shell.frame(Vec::new());
    for text in ["B:", "720 KB", "read-only"] {
        assert!(screen.shows(text), "{text:?} in {:?}", screen.texts());
    }
}
