//! The shell without a window: an `Overlay` driven frame by frame with
//! hand-made input (`testing`), and what each pass shows and asks for.
//!
//! The invariant: time is the input's. A test states when each frame is;
//! nothing here waits or reads a clock.

use egui::accesskit::Role;

use super::messages::{DONE_SECONDS, FAILED_SECONDS};
use super::testing::{Driver, Passes, key};
use super::*;

/// The shell, and the commands its passes asked for.
struct Shell {
    overlay: Overlay,
    commands: Vec<Command>,
}

impl Passes for Shell {
    fn context(&self) -> &egui::Context {
        self.overlay.context()
    }

    fn pass(&mut self, input: egui::RawInput) -> egui::PlatformOutput {
        let commands = self.overlay.run(input);
        self.commands.extend(commands);
        self.overlay.take_platform_output()
    }
}

/// A shell as the app starts it: hidden, no frame run yet.
fn shell() -> Driver<Shell> {
    Driver::new(Shell {
        overlay: Overlay::new(),
        commands: Vec::new(),
    })
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
