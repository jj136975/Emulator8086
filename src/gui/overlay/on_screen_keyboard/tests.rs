//! The On-screen keyboard driven as a user drives it: clicked with the
//! pointer, and touched with fingers as the hosts hand a touch screen's
//! events to egui. The shell is lent a machine and nobody carries out what
//! the panel asks: the tests read the commands. What the guest gets of them
//! through the session is in `gui::session::tests`.
//!
//! The invariant: a key is found by its name, as a screen reader is told
//! it, and pressed in the middle of where the screen shows it.

use std::sync::mpsc::channel;

use egui::accesskit::Role;

use super::presses::{REPEAT_DELAY, REPEAT_PERIOD};
use super::*;
use crate::gui::meter::Meter;
use crate::gui::overlay::Messages;
use crate::gui::overlay::testing::{
    Driver, Passes, SCREEN, Shell, first_finger_down, first_finger_up, key, primary, touch,
    waiting_for_a_key,
};
use crate::wall_clock::HostClock;
use emu8086_core::io::keyboard::{EmulatorEvent, KeyboardController};
use emu8086_core::vm::runtime::Runtime;

/// The panel's title, which the View menu and its window show.
const TITLE: &str = "On-screen keyboard";

const NOTHING: [Command; 0] = [];

/// The keyboard open over a machine without disks that never runs.
fn keyboard() -> Driver<Shell> {
    let (_keys, key_events) = channel();
    let runtime = Runtime::new(
        Vec::new(),
        Vec::new(),
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .expect("a machine without disks has nothing to open");
    Shell::with_panel(runtime, TITLE)
}

/// The commands asked for since the last look.
fn asked(shell: &mut Driver<Shell>) -> Vec<Command> {
    std::mem::take(&mut shell.target.commands)
}

fn press(make: u8) -> Command {
    Command::Key {
        make,
        pressed: true,
    }
}

fn release(make: u8) -> Command {
    Command::Key {
        make,
        pressed: false,
    }
}

/// The middle of the key named `name`, as the last frame showed it.
fn at(shell: &Driver<Shell>, name: &str) -> Pos2 {
    shell.screen().rect(Role::Button, name).center()
}

/// The pointer moved to `pos` and its primary button pressed there.
fn pointer_down(pos: Pos2) -> Vec<Event> {
    vec![Event::PointerMoved(pos), primary(pos, true)]
}

/// The keyboard has the 83 keys of the XT, make codes 01h to 53h, each
/// once, each with a name of its own and something printed on it but for
/// the space bar; every row is as wide as the others. Each key is on the
/// screen as a button, by its name.
#[test]
fn every_make_code_is_on_the_keyboard_once() {
    let mut makes: Vec<u8> = layout::ROWS
        .iter()
        .flat_map(|row| row.iter())
        .map(|(make, _)| *make)
        .filter(|make| *make != 0)
        .collect();
    makes.sort_unstable();
    assert_eq!(makes, (0x01..=0x53).collect::<Vec<u8>>());
    for row in layout::ROWS {
        let quarters: u32 = row.iter().map(|(_, width)| u32::from(*width)).sum();
        assert_eq!(quarters, layout::WIDTH);
    }

    let shell = keyboard();
    let mut names = Vec::new();
    for make in 0x01..=0x53 {
        let (name, legend) = layout::label(make).expect("every key has a label");
        assert!(!name.is_empty(), "{make:02X}h");
        assert!(
            !legend.is_empty() || name == "Space",
            "{name} bears nothing"
        );
        assert!(!names.contains(&name), "two keys are named {name}");
        names.push(name);
        assert!(shell.screen().has(Role::Button, name), "{name}");
    }
    assert_eq!(layout::label(0x00), None);
    assert_eq!(layout::label(0x54), None);
}

/// Every legend can be printed: the overlay's fonts, egui's own, have each
/// of its characters.
#[test]
fn the_fonts_have_every_legend() {
    let shell = keyboard();
    let font = egui::FontId::proportional(12.0);
    for make in 0x01..=0x53 {
        let (name, legend) = layout::label(make).expect("every key has a label");
        let printed: String = legend.lines().collect();
        let has = shell
            .target
            .overlay
            .context()
            .fonts_mut(|fonts| fonts.has_glyphs(&font, &printed));
        assert!(has, "{name}: {legend:?}");
    }
}

/// A click on a key presses it when the button goes down and releases it
/// when the button comes up, wherever the pointer is by then: the key is
/// down in between, and shows it.
#[test]
fn a_click_presses_a_key_and_releases_it() {
    let mut shell = keyboard();
    let a = at(&shell, "A");
    shell.frame(pointer_down(a));
    assert_eq!(asked(&mut shell), [press(0x1E)]);
    assert!(shell.screen().pressed("A"));

    let elsewhere = at(&shell, "L");
    shell.frame(vec![Event::PointerMoved(elsewhere)]);
    assert!(shell.screen().pressed("A"));
    assert!(!shell.screen().pressed("L"));
    shell.frame(vec![primary(elsewhere, false)]);
    assert_eq!(asked(&mut shell), [release(0x1E)]);
    assert!(!shell.screen().pressed("A"));

    shell.click(Role::Button, "Q");
    assert_eq!(asked(&mut shell), [press(0x10), release(0x10)]);
}

/// A finger presses the key it comes down on, and the key stays down
/// wherever the finger moves, off the key included, until the finger lifts
/// or the host takes it away.
#[test]
fn a_finger_holds_its_key_from_when_it_comes_down_until_it_lifts() {
    let mut shell = keyboard();
    let (z, x) = (at(&shell, "Z"), at(&shell, "X"));
    shell.frame(first_finger_down(1, z));
    assert_eq!(asked(&mut shell), [press(0x2C)]);
    shell.frame(vec![touch(1, TouchPhase::Move, x), Event::PointerMoved(x)]);
    assert_eq!(asked(&mut shell), NOTHING);
    assert!(shell.screen().pressed("Z"));
    assert!(!shell.screen().pressed("X"));
    shell.frame(first_finger_up(1, x));
    assert_eq!(asked(&mut shell), [release(0x2C)]);
    assert!(!shell.screen().pressed("Z"));

    shell.frame(first_finger_down(2, x));
    shell.frame(vec![touch(2, TouchPhase::Cancel, x), Event::PointerGone]);
    assert_eq!(asked(&mut shell), [press(0x2D), release(0x2D)]);
}

/// Each finger holds a key of its own, so that several are held at once:
/// in DOOM, an arrow held while Ctrl fires. Each key comes up when its own
/// finger lifts, and Ctrl, which was down with another key, does not latch.
#[test]
fn two_fingers_hold_two_keys() {
    let mut shell = keyboard();
    let (up, ctrl) = (at(&shell, "Up"), at(&shell, "Ctrl"));
    shell.frame(first_finger_down(1, up));
    shell.frame(vec![touch(2, TouchPhase::Start, ctrl)]);
    assert_eq!(asked(&mut shell), [press(0x48), press(0x1D)]);
    assert!(shell.screen().pressed("Up"));
    assert!(shell.screen().pressed("Ctrl"));

    shell.frame(vec![touch(2, TouchPhase::End, ctrl)]);
    assert_eq!(asked(&mut shell), [release(0x1D)]);
    shell.frame(first_finger_up(1, up));
    assert_eq!(asked(&mut shell), [release(0x48)]);
    assert!(!shell.screen().pressed("Ctrl"));
}

/// A tap shorter than a frame, the finger down and up in the same frame's
/// input, still presses its key and then releases it, and so does a click
/// within a frame.
#[test]
fn a_tap_within_one_frame_presses_and_releases() {
    let mut shell = keyboard();
    let enter = at(&shell, "Enter");
    let mut tap = first_finger_down(1, enter);
    tap.extend(first_finger_up(1, enter));
    shell.frame(tap);
    assert_eq!(asked(&mut shell), [press(0x1C), release(0x1C)]);

    let space = at(&shell, "Space");
    let mut click = pointer_down(space);
    click.push(primary(space, false));
    shell.frame(click);
    assert_eq!(asked(&mut shell), [press(0x39), release(0x39)]);
}

/// The first finger is egui's pointer as well, and the host follows its
/// touch with the pointer's press and release: that echo presses nothing a
/// second time, and releases nothing of its own, where the finger lifts or
/// elsewhere. A press of the pointer that follows no touch is the
/// pointer's own.
#[test]
fn the_first_fingers_echo_presses_nothing_twice() {
    let mut shell = keyboard();
    let (j, k) = (at(&shell, "J"), at(&shell, "K"));
    shell.frame(first_finger_down(1, j));
    assert_eq!(asked(&mut shell), [press(0x24)], "once");
    assert!(shell.screen().pressed("J"));

    // A host releases the pointer where it last was, wherever the finger
    // lifts.
    shell.frame(vec![
        touch(1, TouchPhase::End, k),
        primary(j, false),
        Event::PointerGone,
    ]);
    assert_eq!(asked(&mut shell), [release(0x24)], "once");

    shell.frame(pointer_down(k));
    assert_eq!(asked(&mut shell), [press(0x25)], "the pointer's own");
    shell.frame(vec![primary(k, false)]);
    assert_eq!(asked(&mut shell), [release(0x25)]);
}

/// Shift tapped alone latches: it stays down in the guest, and shows down,
/// until the next key that is not a modifier comes up, and comes up after
/// it. Modifiers tapped alone one after the other latch together, for
/// Ctrl+Alt+Del.
#[test]
fn a_modifier_tapped_alone_latches_until_the_next_key_comes_up() {
    let mut shell = keyboard();
    shell.click(Role::Button, "Left Shift");
    assert_eq!(asked(&mut shell), [press(0x2A)], "no break");
    assert!(shell.screen().pressed("Left Shift"));
    shell.click(Role::Button, "A");
    assert_eq!(
        asked(&mut shell),
        [press(0x1E), release(0x1E), release(0x2A)]
    );
    assert!(!shell.screen().pressed("Left Shift"));

    shell.click(Role::Button, "Ctrl");
    shell.click(Role::Button, "Alt");
    assert_eq!(asked(&mut shell), [press(0x1D), press(0x38)]);
    shell.click(Role::Button, "Del");
    assert_eq!(
        asked(&mut shell),
        [press(0x53), release(0x53), release(0x1D), release(0x38)]
    );
}

/// A latched modifier tapped again comes up, and does not latch again.
#[test]
fn tapping_a_latched_modifier_lets_it_go() {
    let mut shell = keyboard();
    shell.click(Role::Button, "Right Shift");
    assert_eq!(asked(&mut shell), [press(0x36)]);
    shell.click(Role::Button, "Right Shift");
    assert_eq!(asked(&mut shell), [release(0x36)]);
    assert!(!shell.screen().pressed("Right Shift"));
    shell.click(Role::Button, "B");
    assert_eq!(asked(&mut shell), [press(0x30), release(0x30)]);
}

/// A modifier that is down while another key is, whichever went down first,
/// is held and comes up when its finger lifts: it does not latch. Nor does
/// one held alone until it repeats: that was a hold, not a tap.
#[test]
fn a_modifier_held_with_another_key_or_long_is_simply_held() {
    let mut shell = keyboard();
    let (shift, a) = (at(&shell, "Left Shift"), at(&shell, "A"));
    shell.frame(first_finger_down(1, shift));
    shell.frame(vec![touch(2, TouchPhase::Start, a)]);
    shell.frame(vec![touch(2, TouchPhase::End, a)]);
    shell.frame(first_finger_up(1, shift));
    assert_eq!(
        asked(&mut shell),
        [press(0x2A), press(0x1E), release(0x1E), release(0x2A)]
    );

    shell.frame(first_finger_down(1, a));
    shell.frame(vec![touch(2, TouchPhase::Start, shift)]);
    shell.frame(vec![touch(2, TouchPhase::End, shift)]);
    shell.frame(first_finger_up(1, a));
    assert_eq!(
        asked(&mut shell),
        [press(0x1E), press(0x2A), release(0x2A), release(0x1E)]
    );

    let alt = at(&shell, "Alt");
    shell.frame(first_finger_down(3, alt));
    shell.wait(REPEAT_DELAY);
    shell.frame(first_finger_up(3, alt));
    assert_eq!(asked(&mut shell), [press(0x38), press(0x38), release(0x38)]);
    assert!(!shell.screen().pressed("Alt"));
}

/// A key held down is pressed again once it has been held `REPEAT_DELAY`,
/// then every `REPEAT_PERIOD`, for as long as it is held: an XT's
/// typematic, half a second, then 10.9 times a second. A latched modifier
/// is held by nothing, and does not repeat.
#[test]
fn a_key_held_repeats_and_a_latched_modifier_does_not() {
    let mut shell = keyboard();
    let b = at(&shell, "B");
    shell.frame(pointer_down(b));
    assert_eq!(asked(&mut shell), [press(0x30)]);
    shell.wait(REPEAT_DELAY - 0.1);
    assert_eq!(asked(&mut shell), NOTHING, "not yet");
    shell.wait(0.1);
    assert_eq!(asked(&mut shell), [press(0x30)], "half a second");

    // A second of frames.
    for _ in 0..60 {
        shell.frame(Vec::new());
    }
    let repeats = asked(&mut shell);
    assert!(repeats.iter().all(|command| *command == press(0x30)));
    let per_second = (1.0 / REPEAT_PERIOD).round() as usize;
    assert!(
        (per_second - 1..=per_second).contains(&repeats.len()),
        "{} repeats in a second",
        repeats.len()
    );

    shell.frame(vec![primary(b, false)]);
    shell.wait(1.0);
    assert_eq!(asked(&mut shell), [release(0x30)], "no repeat once up");

    shell.click(Role::Button, "Ctrl");
    asked(&mut shell);
    shell.wait(2.0);
    assert_eq!(asked(&mut shell), NOTHING);
}

/// The lock keys are lit from the BIOS's keyboard flags in guest memory.
/// As the BIOS leaves them after its self test, all three are off; once
/// the guest has taken in a press of Caps Lock, Caps Lock is on and the
/// others are not.
#[test]
fn the_lock_keys_are_lit_from_the_bios_flags() {
    const LOCKS: [&str; 3] = ["Caps Lock", "Num Lock", "Scroll Lock"];
    let (runtime, keys) = waiting_for_a_key();
    let mut shell = Shell::with_panel(runtime, TITLE);
    for lock in LOCKS {
        assert!(!shell.screen().pressed(lock), "{lock}");
    }

    keys.send(EmulatorEvent::Scancode(0x3A)).unwrap();
    keys.send(EmulatorEvent::ScanRelease(0xBA)).unwrap();
    let runtime = &mut shell.target.runtime;
    for _ in 0..100 {
        runtime.run(10_000);
    }
    assert_ne!(runtime.cpu.memory.read_byte(BIOS_KEYBOARD_FLAGS) & 0x40, 0);

    let screen = shell.frame(Vec::new());
    assert!(screen.pressed("Caps Lock"));
    assert!(!screen.pressed("Num Lock"));
    assert!(!screen.pressed("Scroll Lock"));
}

/// Holds W with a finger that is not egui's pointer, which is then free for
/// another finger to tap with: the first finger comes down on the picture,
/// the second on W, and the first lifts. Returns where W is.
fn hold_w_with_a_second_finger(shell: &mut Driver<Shell>) -> Pos2 {
    let w = at(shell, "W");
    let picture = egui::pos2(SCREEN[0] as f32 - 4.0, SCREEN[1] as f32 - 4.0);
    shell.frame(first_finger_down(1, picture));
    shell.frame(vec![touch(2, TouchPhase::Start, w)]);
    shell.frame(first_finger_up(1, picture));
    w
}

/// A tap of a finger that is egui's pointer on the control labelled
/// `label`, given the time to do what it does.
fn tap(shell: &mut Driver<Shell>, id: u64, label: &str) {
    let at = shell.screen().rect(Role::Button, label).center();
    shell.frame(first_finger_down(id, at));
    shell.frame(first_finger_up(id, at));
    shell.wait(1.0);
}

/// What the keyboard held is forgotten once it is not drawn: closed while
/// a finger holds a key and opened again, it shows nothing down, and the
/// finger's lift asks for nothing. The tap that closed it asked for
/// nothing either: the app lets go of the key (`gui::session::tests`).
#[test]
fn the_keyboard_forgets_what_it_held_once_it_is_not_drawn() {
    let mut shell = keyboard();
    let w = hold_w_with_a_second_finger(&mut shell);
    assert_eq!(asked(&mut shell), [press(0x11)]);

    tap(&mut shell, 3, "Close window");
    assert!(!shell.screen().has(Role::Window, TITLE));
    assert_eq!(asked(&mut shell), NOTHING);
    shell.toggle_in_view_menu(TITLE);
    assert!(!shell.screen().pressed("W"));
    shell.frame(vec![touch(2, TouchPhase::End, w)]);
    assert_eq!(asked(&mut shell), NOTHING);
}

/// While a menu of the overlay is open, the overlay has the keyboard: the
/// keys held from the screen come up, a latched modifier with them, and a
/// finger that comes down on a key presses nothing. Once the menu is shut,
/// a key works again.
#[test]
fn an_open_menu_lets_go_of_the_keys_and_takes_none() {
    let mut shell = keyboard();
    shell.click(Role::Button, "Ctrl");
    assert_eq!(asked(&mut shell), [press(0x1D)]);

    shell.click(Role::Button, "Machine");
    assert_eq!(asked(&mut shell), [release(0x1D)]);
    assert!(!shell.screen().pressed("Ctrl"));
    let tab = at(&shell, "Tab");
    shell.frame(vec![touch(5, TouchPhase::Start, tab)]);
    shell.frame(vec![touch(5, TouchPhase::End, tab)]);
    assert_eq!(asked(&mut shell), NOTHING);

    shell.frame(vec![key(egui::Key::Escape, true)]);
    shell.frame(vec![key(egui::Key::Escape, false)]);
    assert!(!shell.screen().has(Role::Button, "Reset"), "shut");
    shell.click(Role::Button, "Tab");
    assert_eq!(asked(&mut shell), [press(0x0F), release(0x0F)]);
}

/// The panel by itself in a window, in frames that egui is asked to run
/// twice, as it runs one that shows something for the first time: the
/// first run is discarded, and the second is given no events. Nobody
/// carries out the commands.
struct RunTwice {
    ctx: egui::Context,
    runtime: Runtime,
    meter: Meter,
    commands: Vec<Command>,
    messages: Messages,
    drawn_before: bool,
}

impl Passes for RunTwice {
    fn context(&self) -> &egui::Context {
        &self.ctx
    }

    fn pass(&mut self, input: egui::RawInput) -> egui::PlatformOutput {
        let mut cx = Cx {
            runtime: &mut self.runtime,
            paused: false,
            meter: &self.meter,
            drawn_before: self.drawn_before,
            commands: &mut self.commands,
            messages: &mut self.messages,
        };
        let output = self.ctx.run_ui(input, |ui| {
            egui::Window::new(TITLE).show(ui.ctx(), |ui| body(ui, &mut cx));
            cx.drawn_before = true;
            if ui.ctx().current_pass_index() == 0 {
                ui.ctx().request_discard("the test runs every frame twice");
            }
        });
        self.drawn_before = true;
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

/// egui may run the UI twice for one frame, and gives the frame's events to
/// the first run alone: in frames that are each run twice, a key is pressed
/// once and released once, and a modifier latched once.
#[test]
fn a_frame_run_twice_counts_its_events_once() {
    let (_keys, key_events) = channel();
    let runtime = Runtime::new(
        Vec::new(),
        Vec::new(),
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .expect("a machine without disks has nothing to open");
    let mut panel = Driver::new(RunTwice {
        ctx: egui::Context::default(),
        runtime,
        meter: Meter::default(),
        commands: Vec::new(),
        messages: Messages::default(),
        drawn_before: false,
    });
    panel.frame(Vec::new());
    let passes = panel.target.ctx.cumulative_pass_nr();
    panel.frame(Vec::new());
    assert_eq!(
        panel.target.ctx.cumulative_pass_nr() - passes,
        2,
        "every frame is run twice"
    );

    let x = panel.screen().rect(Role::Button, "X").center();
    panel.frame(first_finger_down(1, x));
    panel.frame(first_finger_up(1, x));
    let shift = panel.screen().rect(Role::Button, "Right Shift").center();
    panel.frame(pointer_down(shift));
    panel.frame(vec![primary(shift, false)]);
    assert_eq!(
        std::mem::take(&mut panel.target.commands),
        [press(0x2D), release(0x2D), press(0x36)]
    );
}

/// A key takes no focus: after a click on a key, and while a finger holds
/// one, the overlay does not have the keyboard, which stays the guest's.
#[test]
fn a_key_leaves_the_keyboard_to_the_guest() {
    let mut shell = keyboard();
    shell.click(Role::Button, "A");
    assert!(!shell.target.overlay.wants_keyboard());
    let s = at(&shell, "S");
    shell.frame(first_finger_down(1, s));
    assert!(!shell.target.overlay.wants_keyboard());
    shell.frame(first_finger_up(1, s));
    assert!(!shell.target.overlay.wants_keyboard());
}
