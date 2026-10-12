//! The session without a window: the overlay is driven with hand-made input
//! (`overlay::testing`), key events are handed in as the window would hand
//! them, and the tests read the machine and the far end of the keyboard's
//! channel.
//!
//! The invariant: nothing here needs a window or a GPU. `Session::frame`
//! runs on the host's real clock, a hundredth of a second per call.

use std::sync::mpsc::{Receiver, channel};

use egui::accesskit::Role;
use winit::keyboard::KeyCode;

use super::*;
use crate::gui::hold::HOLD;
use crate::gui::host::testing::ByHand;
use crate::gui::overlay::testing::{
    Driver, Passes, SCREEN, first_finger_down, first_finger_up, input, key, touch,
};
use crate::wall_clock::HostClock;
use EmulatorEvent::{ScanRelease, Scancode};
use emu8086_core::BIOS_ROM;
use emu8086_core::io::disk_spec::parse_disk_specs;
use emu8086_core::io::keyboard::KeyboardController;

impl Passes for Session {
    fn context(&self) -> &egui::Context {
        self.overlay.context()
    }

    fn pass(&mut self, input: egui::RawInput) -> egui::PlatformOutput {
        self.run_overlay(input);
        self.overlay.take_platform_output()
    }
}

/// A session around a machine without disks, with `rom` as its ROM, and the
/// end of the keyboard's channel the machine would read: the tests read it
/// instead.
fn session_with(rom: &[u8]) -> (Session, Receiver<EmulatorEvent>) {
    let (_unused, key_events) = channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    let mut runtime = Runtime::new(vec![], vec![], None, false, keyboard, &HostClock)
        .expect("a machine without disks has nothing to open");
    runtime.load_rom_image(rom, "the test's ROM").unwrap();
    let (kbd_tx, sent) = channel();
    (Session::new(runtime, kbd_tx), sent)
}

/// A machine that does nothing but jump to itself.
const BUSY_ROM: [u8; 16] = {
    let mut rom = [0x90; 16]; // NOP
    rom[0] = 0xEB; // JMP $
    rom[1] = 0xFE;
    rom
};

/// A session whose overlay is shown and has had its first frame.
fn shown(rom: &[u8]) -> (Driver<Session>, Receiver<EmulatorEvent>) {
    let (mut session, sent) = session_with(rom);
    session.overlay_key(ElementState::Pressed, false);
    let mut driver = Driver::new(session);
    driver.frame(Vec::new());
    (driver, sent)
}

/// What was sent to the guest's keyboard since the last look.
fn sent(events: &Receiver<EmulatorEvent>) -> Vec<EmulatorEvent> {
    events.try_iter().collect()
}

const NOTHING: [EmulatorEvent; 0] = [];

fn code(code: KeyCode) -> PhysicalKey {
    PhysicalKey::Code(code)
}

/// The overlay's key toggles the overlay when it goes down, once however
/// long it is held: neither its repeats nor its release toggle again.
#[test]
fn the_overlay_key_toggles_once_per_press() {
    let (mut session, _sent) = session_with(&BUSY_ROM);
    assert!(!session.overlay.visible());

    session.overlay_key(ElementState::Pressed, false);
    assert!(session.overlay.visible());
    session.overlay_key(ElementState::Pressed, true);
    session.overlay_key(ElementState::Pressed, true);
    session.overlay_key(ElementState::Released, false);
    assert!(session.overlay.visible(), "one press, one toggle");

    session.overlay_key(ElementState::Pressed, false);
    assert!(!session.overlay.visible());
}

/// The overlay's button of the pointer toggles the overlay when it goes
/// down on the picture, once per press. A press the overlay kept, because
/// the pointer is over it, is the overlay's own and toggles nothing.
#[test]
fn the_overlay_button_toggles_on_the_picture_only() {
    let (mut session, _sent) = session_with(&BUSY_ROM);
    assert!(!session.overlay.visible());

    session.overlay_button(ElementState::Pressed, false);
    assert!(session.overlay.visible());
    session.overlay_button(ElementState::Released, false);
    assert!(session.overlay.visible(), "one press, one toggle");

    session.overlay_button(ElementState::Pressed, true);
    session.overlay_button(ElementState::Released, true);
    assert!(session.overlay.visible(), "the overlay kept the press");

    session.overlay_button(ElementState::Pressed, false);
    assert!(!session.overlay.visible());
}

/// What makes a press of the pointer the overlay's own: egui has a use for
/// the pointer over the menu bar and over the window of a panel, and none
/// on the picture that lies bare between them, where the overlay's button
/// is its switch.
#[test]
fn the_pointer_is_the_overlays_over_what_it_shows_and_not_on_the_picture() {
    let (mut session, _sent) = shown(&BUSY_ROM);
    let kept_at = |session: &mut Driver<Session>, at: egui::Pos2| {
        session.frame(vec![egui::Event::PointerMoved(at)]);
        session.target.overlay.context().egui_wants_pointer_input()
    };

    let bar = session.screen().rect(Role::Button, "Machine").center();
    assert!(kept_at(&mut session, bar), "over the menu bar");
    let picture = egui::pos2(SCREEN[0] as f32 / 2.0, SCREEN[1] as f32 / 2.0);
    assert!(!kept_at(&mut session, picture), "on the picture");

    session.click(Role::Button, "View");
    session.click(Role::CheckBox, "About");
    session.wait(1.0);
    let panel = session.screen().rect(Role::Window, "About").center();
    assert!(kept_at(&mut session, panel), "over a panel");
}

/// The point in the middle of the picture, which nothing of the overlay
/// covers while no panel is open.
fn middle() -> egui::Pos2 {
    egui::pos2(SCREEN[0] as f32 / 2.0, SCREEN[1] as f32 / 2.0)
}

/// A finger held still on the picture shows the overlay as soon as its time
/// is up, before it lifts, and a finger held there again hides it. Nothing
/// but the session's own frames is needed: a hidden overlay runs no pass
/// for it.
#[test]
fn a_finger_held_still_on_the_picture_works_the_switch() {
    let (mut session, _sent) = session_with(&BUSY_ROM);
    let start = Instant::now();
    session.touch(1, TouchPhase::Started, middle(), start);
    session.hold(start + HOLD - Duration::from_millis(1));
    assert!(!session.overlay.visible(), "not yet");
    session.hold(start + HOLD);
    assert!(session.overlay.visible(), "its time is up");
    session.hold(start + 10 * HOLD);
    assert!(session.overlay.visible(), "once per hold");

    session.touch(1, TouchPhase::Ended, middle(), start + 10 * HOLD);
    let again = start + 20 * HOLD;
    session.touch(2, TouchPhase::Started, middle(), again);
    session.hold(again + HOLD);
    assert!(!session.overlay.visible());
    assert!(
        !session.frame(|| panic!("no input is gathered for an overlay that shows nothing")),
        "the hint was over when the switch was found"
    );
}

/// A finger held on what the overlay shows is the overlay's, however long it
/// stays there: held on the menu bar or on the window of a panel, it leaves
/// the overlay shown.
#[test]
fn a_finger_held_on_the_overlay_leaves_it_shown() {
    let (mut session, _sent) = shown(&BUSY_ROM);
    session.toggle_in_view_menu("About");
    let bar = session.screen().rect(Role::Button, "View").center();
    let panel = session.screen().rect(Role::Window, "About").center();

    let start = Instant::now();
    for (finger, at) in [(1, bar), (2, panel)] {
        session.target.touch(finger, TouchPhase::Started, at, start);
    }
    session.target.hold(start + 10 * HOLD);
    assert!(session.target.overlay.visible());
}

/// A finger that came down before the overlay showed something under it
/// presses nothing there when it lifts: held on the picture where the
/// Machine menu's button will be, it shows the overlay, and its lift does
/// not open the menu.
#[test]
fn a_finger_down_before_the_overlay_showed_presses_nothing_when_it_lifts() {
    let (mut session, _sent) = shown(&BUSY_ROM);
    let machine = session.screen().rect(Role::Button, "Machine").center();
    session.target.overlay_key(ElementState::Pressed, false);
    assert!(!session.target.overlay.visible());

    // While the overlay is hidden, the window drops what egui would make
    // of the finger.
    let start = Instant::now();
    session.target.touch(1, TouchPhase::Started, machine, start);
    session.target.hold(start + HOLD);
    assert!(session.target.overlay.visible());
    session.frame(Vec::new());

    // The lift, as the window hands it to egui.
    let finger = egui::TouchId(1);
    session.frame(vec![
        egui::Event::Touch {
            device_id: egui::TouchDeviceId(0),
            id: finger,
            phase: egui::TouchPhase::End,
            pos: machine,
            force: None,
        },
        egui::Event::PointerButton {
            pos: machine,
            button: egui::PointerButton::Primary,
            pressed: false,
            modifiers: egui::Modifiers::NONE,
        },
        egui::Event::PointerGone,
    ]);
    session.frame(Vec::new());
    assert!(session.screen().has(Role::Button, "Machine"));
    assert!(
        !session.screen().has(Role::Button, "Reset"),
        "the menu is shut"
    );
}

/// The title of the on-screen keyboard's panel.
const KEYBOARD: &str = "On-screen keyboard";

/// The keys of the on-screen keyboard reach the guest's keyboard as their
/// make and break codes: a key clicked, and Shift latched by a tap, then
/// the key typed after it, which takes Shift up with it.
#[test]
fn the_on_screen_keys_reach_the_guest() {
    let (mut session, events) = shown(&BUSY_ROM);
    session.toggle_in_view_menu(KEYBOARD);
    session.click(Role::Button, "A");
    assert_eq!(sent(&events), [Scancode(0x1E), ScanRelease(0x9E)]);

    session.click(Role::Button, "Right Shift");
    session.click(Role::Button, "B");
    assert_eq!(
        sent(&events),
        [
            Scancode(0x36),
            Scancode(0x30),
            ScanRelease(0xB0),
            ScanRelease(0xB6)
        ]
    );
}

/// A key held from the on-screen keyboard is let go in the guest the moment
/// the keyboard leaves the screen, its window closed or the overlay hidden,
/// and when the window loses the keyboard. The lift of the finger that held
/// it sends nothing after that, and the key no longer shows down.
#[test]
fn the_on_screen_keys_are_let_go_when_the_keyboard_leaves_the_screen() {
    let (mut session, events) = shown(&BUSY_ROM);
    session.toggle_in_view_menu(KEYBOARD);
    let w = session.screen().rect(Role::Button, "W").center();
    let picture = egui::pos2(SCREEN[0] as f32 - 4.0, SCREEN[1] as f32 - 4.0);

    // A finger holds W while another, which is egui's pointer, taps the
    // close button of the keyboard's window.
    session.frame(first_finger_down(1, picture));
    session.frame(vec![touch(2, egui::TouchPhase::Start, w)]);
    session.frame(first_finger_up(1, picture));
    assert_eq!(sent(&events), [Scancode(0x11)]);
    let close = session.screen().rect(Role::Button, "Close window").center();
    session.frame(first_finger_down(3, close));
    session.frame(first_finger_up(3, close));
    session.wait(1.0);
    assert!(!session.screen().has(Role::Window, KEYBOARD));
    assert_eq!(sent(&events), [ScanRelease(0x91)], "closed");
    session.frame(vec![touch(2, egui::TouchPhase::End, w)]);
    assert_eq!(sent(&events), NOTHING);

    session.toggle_in_view_menu(KEYBOARD);
    session.frame(first_finger_down(4, w));
    assert_eq!(sent(&events), [Scancode(0x11)]);
    session.target.overlay_key(ElementState::Pressed, false);
    assert_eq!(sent(&events), [ScanRelease(0x91)], "hidden");
    session.target.overlay_key(ElementState::Pressed, false);
    assert!(!session.frame(first_finger_up(4, w)).pressed("W"));
    assert_eq!(sent(&events), NOTHING);

    session.frame(first_finger_down(5, w));
    assert_eq!(sent(&events), [Scancode(0x11)]);
    session.target.focus_lost();
    assert_eq!(sent(&events), [ScanRelease(0x91)], "the keyboard lost");
    let mut lost = vec![egui::Event::WindowFocused(false)];
    lost.extend(first_finger_up(5, w));
    assert!(!session.frame(lost).pressed("W"));
    assert_eq!(sent(&events), NOTHING);
}

/// A finger that holds a key of the on-screen keyboard is on the overlay:
/// however long it stays there it does not hide the overlay, and its key
/// repeats meanwhile.
#[test]
fn a_finger_holding_an_on_screen_key_never_hides_the_overlay() {
    let (mut session, events) = shown(&BUSY_ROM);
    session.toggle_in_view_menu(KEYBOARD);
    let d = session.screen().rect(Role::Button, "D").center();
    let start = Instant::now();
    session.target.touch(1, TouchPhase::Started, d, start);
    session.frame(first_finger_down(1, d));
    session.target.hold(start + 4 * HOLD);
    assert!(session.target.overlay.visible());

    session.wait(0.5);
    for _ in 0..30 {
        session.frame(Vec::new());
    }
    let repeated = sent(&events);
    assert!(repeated.len() > 2, "{repeated:?}");
    assert!(repeated.iter().all(|event| *event == Scancode(0x20)));
}

/// An overlay shown by its button with the focus where it was left has the
/// keyboard from that moment, as one shown by its key has: the keys the
/// guest holds are let go.
#[test]
fn showing_the_overlay_by_its_button_lets_go_of_the_guests_keys() {
    let (mut session, events) = shown(&BUSY_ROM);
    let guest = &mut session.target;
    assert!(guest.frame(|| input(1.0, vec![key(egui::Key::Tab, true)])));
    assert!(guest.overlay.wants_keyboard());

    guest.overlay_button(ElementState::Pressed, false);
    assert!(!guest.overlay.wants_keyboard(), "hidden");
    guest.key(code(KeyCode::KeyA), ElementState::Pressed, false);
    assert_eq!(sent(&events), [Scancode(0x1E)]);

    guest.overlay_button(ElementState::Pressed, false);
    assert_eq!(sent(&events), [ScanRelease(0x9E)]);
}

/// A key goes to the guest unless the overlay kept it. The release of a
/// key the guest holds goes to the guest whoever kept the event.
#[test]
fn a_key_goes_to_the_guest_unless_the_overlay_kept_it() {
    let (mut session, events) = session_with(&BUSY_ROM);
    session.key(code(KeyCode::KeyA), ElementState::Pressed, false);
    assert_eq!(sent(&events), [Scancode(0x1E)]);

    session.key(code(KeyCode::KeyB), ElementState::Pressed, true);
    session.key(code(KeyCode::KeyB), ElementState::Released, true);
    assert_eq!(sent(&events), NOTHING, "the overlay kept it");

    session.key(code(KeyCode::KeyA), ElementState::Released, true);
    assert_eq!(sent(&events), [ScanRelease(0x9E)]);
}

/// When the overlay takes the keyboard while the guest holds keys, the
/// guest gets their releases at once: none stays down in it. The releases
/// of the host that follow find nothing left to release.
#[test]
fn the_guests_keys_are_let_go_when_the_overlay_takes_the_keyboard() {
    let (mut session, events) = shown(&BUSY_ROM);
    let guest = &mut session.target;
    guest.key(code(KeyCode::ShiftLeft), ElementState::Pressed, false);
    guest.key(code(KeyCode::KeyA), ElementState::Pressed, false);
    assert_eq!(sent(&events), [Scancode(0x2A), Scancode(0x1E)]);
    assert!(guest.frame(|| input(1.0, Vec::new())));
    assert_eq!(sent(&events), NOTHING, "the keyboard is still the guest's");

    // Tab moves the focus into the overlay: from then on it has the
    // keyboard.
    assert!(guest.frame(|| input(1.1, vec![key(egui::Key::Tab, true)])));
    assert!(guest.overlay.wants_keyboard());
    assert_eq!(sent(&events), [ScanRelease(0xAA), ScanRelease(0x9E)]);

    guest.key(code(KeyCode::KeyA), ElementState::Released, true);
    guest.key(code(KeyCode::ShiftLeft), ElementState::Released, true);
    assert_eq!(sent(&events), NOTHING);
}

/// While a menu of the overlay is open the keys are the overlay's, whether
/// egui has a use for them or not: Escape closes the menu and does not
/// reach the guest, nor does any other key, and what the guest held when
/// the menu opened is let go.
#[test]
fn an_open_menu_has_the_keyboard() {
    let (mut session, events) = shown(&BUSY_ROM);
    session
        .target
        .key(code(KeyCode::KeyA), ElementState::Pressed, false);
    assert_eq!(sent(&events), [Scancode(0x1E)]);

    session.click(Role::Button, "Machine");
    assert_eq!(sent(&events), [ScanRelease(0x9E)], "the menu opened");

    let guest = &mut session.target;
    guest.key(code(KeyCode::KeyB), ElementState::Pressed, false);
    guest.key(code(KeyCode::Escape), ElementState::Pressed, false);
    session.frame(vec![key(egui::Key::Escape, true)]);
    session.frame(vec![key(egui::Key::Escape, false)]);
    assert!(!session.screen().has(Role::Button, "Reset"), "closed");
    let guest = &mut session.target;
    guest.key(code(KeyCode::Escape), ElementState::Released, false);
    guest.key(code(KeyCode::KeyB), ElementState::Released, false);
    assert_eq!(sent(&events), NOTHING, "the menu had the keyboard");

    guest.key(code(KeyCode::KeyC), ElementState::Pressed, false);
    assert_eq!(sent(&events), [Scancode(0x2E)], "it is the guest's again");
}

/// An overlay that is shown again with the focus where it was left has the
/// keyboard from that moment: the keys the guest holds are let go then.
#[test]
fn showing_an_overlay_that_has_the_focus_lets_go_of_the_guests_keys() {
    let (mut session, events) = shown(&BUSY_ROM);
    let guest = &mut session.target;
    assert!(guest.frame(|| input(1.0, vec![key(egui::Key::Tab, true)])));
    assert!(guest.overlay.wants_keyboard());

    guest.overlay_key(ElementState::Pressed, false);
    assert!(!guest.overlay.wants_keyboard(), "hidden");
    guest.key(code(KeyCode::KeyA), ElementState::Pressed, false);
    assert_eq!(sent(&events), [Scancode(0x1E)]);

    guest.overlay_key(ElementState::Pressed, false);
    assert_eq!(sent(&events), [ScanRelease(0x9E)]);
}

/// A window that loses the keyboard lets go of the keys the guest holds.
#[test]
fn losing_the_focus_lets_go_of_the_guests_keys() {
    let (mut session, events) = session_with(&BUSY_ROM);
    session.key(code(KeyCode::AltLeft), ElementState::Pressed, false);
    session.key(code(KeyCode::Tab), ElementState::Pressed, false);
    assert_eq!(sent(&events), [Scancode(0x38), Scancode(0x0F)]);

    session.focus_lost();
    assert_eq!(sent(&events), [ScanRelease(0xB8), ScanRelease(0x8F)]);
}

/// A frame runs the guest, and a pass of the overlay only if the overlay
/// has anything to show: for a hidden overlay past its hint no input is
/// even gathered.
#[test]
fn a_hidden_overlay_runs_no_pass() {
    let (mut session, _sent) = session_with(&BUSY_ROM);
    assert!(
        session.frame(|| input(0.0, Vec::new())),
        "the hint is shown at first"
    );
    let ran = session.runtime.instruction_count;
    assert!(ran > 0, "the guest ran");

    // Showing and hiding the overlay ends the hint.
    session.overlay_key(ElementState::Pressed, false);
    session.overlay_key(ElementState::Released, false);
    session.overlay_key(ElementState::Pressed, false);
    assert!(!session.frame(|| panic!("no input is gathered for an overlay that shows nothing")));
    assert!(session.runtime.instruction_count > ran, "the guest ran on");

    session.overlay_key(ElementState::Released, false);
    session.overlay_key(ElementState::Pressed, false);
    assert!(session.frame(|| input(1.0, Vec::new())));
}

/// Each frame tells the meter of its slice and of what the machine counted
/// after it, whether the overlay shows anything or not: the meter keeps the
/// session's own machine, from the first frame on.
#[test]
fn each_frame_tells_the_meter_what_the_machine_counted() {
    let (mut session, _sent) = session_with(&BUSY_ROM);
    assert_eq!(session.meter.counts(), None);
    session.overlay_key(ElementState::Pressed, false);
    session.overlay_key(ElementState::Pressed, false);
    assert!(!session.overlay.draws(), "nothing to show");

    session.frame(|| panic!("no input is gathered for an overlay that shows nothing"));
    session.frame(|| panic!("no input is gathered for an overlay that shows nothing"));
    assert_eq!(
        session.meter.counts(),
        Some(&Counts::read(&session.runtime))
    );
    assert!(session.meter.counts().unwrap().instructions > 0);
    assert!(session.meter.uptime() >= 2 * CPU_SLICE, "two slices long");
}

/// The overlay's pass is timed, and what the window draws after a frame is
/// told to the meter: each frame of an average holds the drawing that
/// followed the frame before it, one for one, so the waits and the drawing
/// come to what was told, and the passes of the overlay take host time.
#[test]
fn the_overlay_and_the_drawing_are_measured_into_the_frames() {
    let (mut session, _sent) = shown(&BUSY_ROM);
    let (waited, drawing) = (Duration::from_millis(2), Duration::from_millis(1));
    // A frame is a slice of 10 ms and the overlay's pass: a second of frames
    // is about a hundred of them.
    let mut frames_run = 0;
    while session.target.meter.frames().is_none() {
        assert!(frames_run < 1000, "no second of frames in 1000 frames");
        let input = session.next_input(Vec::new());
        assert!(session.target.frame(|| input), "the overlay is shown");
        session.target.drawn(Drawn { waited, drawing });
        frames_run += 1;
    }

    let frames = *session.target.meter.frames().expect("a second of frames");
    assert_eq!(frames.waited, frames.count * waited);
    assert_eq!(frames.drawing, frames.count * drawing);
    assert!(frames.overlay > Duration::ZERO, "the passes took time");
}

/// A machine on which the BIOS found no disk to boot and waits for a key
/// in HLT, with interrupts on. It gets there without any time passing.
fn idling_in_the_bios() -> (Driver<Session>, Receiver<EmulatorEvent>) {
    let (mut session, sent) = shown(BIOS_ROM);
    let runtime = &mut session.target.runtime;
    for _ in 0..2000 {
        runtime.run(50_000);
        if runtime.cpu.halted {
            break;
        }
    }
    assert!(runtime.cpu.halted, "the BIOS waits for a key");
    (session, sent)
}

/// The BIOS tick counter at 0040:006C, incremented by every INT 08h.
fn timer_ticks(runtime: &Runtime) -> u32 {
    let memory = &runtime.cpu.memory;
    memory.read_word(0x046C) as u32 | (memory.read_word(0x046E) as u32) << 16
}

/// Runs `frames` frames as the window does: each a slice of the guest's
/// time, a hundredth of a second of the host's, and a pass of the overlay.
fn run_frames(session: &mut Driver<Session>, frames: usize) {
    for _ in 0..frames {
        let input = session.next_input(Vec::new());
        assert!(session.target.frame(|| input), "the overlay is shown");
    }
}

/// Machine > Pause holds the guest still while the frames go on: no
/// instruction runs and its timer does not tick, and the overlay marks the
/// machine as paused. Machine > Resume lets it run again from where it was,
/// without the time it stood still: its timer goes on at its own rate.
#[test]
fn pause_holds_the_guest_and_resume_does_not_replay_the_paused_time() {
    let (mut session, _sent) = idling_in_the_bios();
    run_frames(&mut session, 6);

    session.click(Role::Button, "Machine");
    session.click(Role::Button, "Pause");
    assert!(session.target.paused());
    assert!(session.screen().shows("Paused"));

    let ran = session.target.runtime.instruction_count;
    let ticks = timer_ticks(&session.target.runtime);
    run_frames(&mut session, 30);
    assert_eq!(session.target.runtime.instruction_count, ran);
    assert_eq!(timer_ticks(&session.target.runtime), ticks);

    session.click(Role::Button, "Machine");
    session.click(Role::Button, "Resume");
    assert!(!session.target.paused());
    assert!(!session.frame(Vec::new()).shows("Paused"));

    run_frames(&mut session, 11);
    let since = timer_ticks(&session.target.runtime) - ticks;
    // 110 ms hold two ticks of 55 ms. The 300 ms of the pause would be
    // four or five more.
    assert!(
        (1..=3).contains(&since),
        "{since} ticks in 110 ms after a pause of 300 ms, expected 2"
    );
    assert!(session.target.runtime.instruction_count > ran);
}

/// Machine > Reset reboots the guest: the CPU is back at the reset vector,
/// running, and the overlay says so.
#[test]
fn reset_reboots_the_guest_and_says_so() {
    let (mut session, _sent) = idling_in_the_bios();
    session.click(Role::Button, "Machine");
    session.click(Role::Button, "Reset");

    let cpu = &session.target.runtime.cpu;
    assert!(!cpu.halted);
    assert_eq!(cpu.registers.cs.reg().word(), 0xFFFF);
    assert_eq!(cpu.registers.pc.word(), 0x0000);
    session.frame(Vec::new());
    assert!(session.screen().shows("Machine reset"));
}

/// Machine > Dump VGA state writes the machine's dump to the session's
/// file and says where; when the file cannot be written it says why.
#[test]
fn dump_vga_writes_the_file_and_says_so() {
    let (mut session, _sent) = idling_in_the_bios();
    let file = std::env::temp_dir().join(format!("emu8086-vga-dump-{}.txt", std::process::id()));
    session.target.dump_file = file.clone();

    session.click(Role::Button, "Machine");
    session.click(Role::Button, "Dump VGA state");
    session.frame(Vec::new());
    let written = std::fs::read_to_string(&file).expect("the dump was written");
    let _ = std::fs::remove_file(&file);
    assert!(written.starts_with("=== VGA Diagnostic Dump ===\n"));
    assert!(written.contains("No bootable disk"), "{written}");
    let message = format!("VGA state written to {}", file.display());
    assert!(
        session.screen().shows(&message),
        "{:?}",
        session.screen().texts()
    );

    // A directory that does not exist cannot take the file.
    session.target.dump_file = file.with_extension("missing").join("dump.txt");
    session.click(Role::Button, "Machine");
    session.click(Role::Button, "Dump VGA state");
    session.frame(Vec::new());
    let texts = session.screen().texts();
    assert!(
        texts.iter().any(|text| text.starts_with("Cannot write ")),
        "{texts:?}"
    );
}

/// Where a ROM that marks its runs leaves its mark: 0000:0500, just above
/// the BIOS data area, which the ROM does not set up.
const MARK_AT: usize = 0x0500;

/// A ROM that, from its reset vector, writes 4D52h to `MARK_AT` and then
/// jumps to itself: a machine that runs it shows that it was restarted.
const MARKING_ROM: [u8; 16] = [
    0xB8, 0x52, 0x4D, // MOV AX, 4D52h
    0xA3, 0x00, 0x05, // MOV [0500h], AX (DS is 0 after a reset)
    0xEB, 0xFE, // JMP $
    0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x90, // NOP
];

/// The labels of the buttons of the Start panel.
const BOOT: &str = "Boot MS-DOS 4.00";
const PLAY: &str = "Play DOOM";

/// A machine that runs `rom`, with a 720 KB floppy in A: and no hard disk
/// if `with_a_floppy`, and no disk at all if not.
fn machine_running(rom: &[u8], with_a_floppy: bool) -> Runtime {
    let specs = if with_a_floppy {
        vec!["a:memory:720".to_string()]
    } else {
        Vec::new()
    };
    let (_unused, key_events) = channel();
    let mut runtime = Runtime::new(
        parse_disk_specs(&specs).unwrap(),
        Vec::new(),
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .unwrap();
    runtime.load_rom_image(rom, "the test's ROM").unwrap();
    runtime
}

/// The session of `runtime` on a host that came from a site, whose starts
/// are carried out on a host whose site the test answers.
fn on_a_site(runtime: Runtime) -> (Session, ByHand) {
    let (kbd_tx, _sent) = channel();
    let mut session = Session::on(runtime, kbd_tx, true);
    let host = ByHand::default();
    session.starts = Starter::on(Box::new(host.clone()));
    (session, host)
}

/// A session on a host that came from a site, around a machine with a
/// 720 KB floppy in A: and no hard disk that runs `MARKING_ROM`, whose
/// starts are carried out on a host whose site the test answers. Its
/// overlay is shown with the Start panel open, and the mark the machine
/// left when it was powered on is wiped.
fn at_the_start_panel() -> (Driver<Session>, ByHand) {
    let (mut session, host) = on_a_site(machine_running(&MARKING_ROM, true));
    session.overlay_key(ElementState::Pressed, false);
    let mut session = Driver::new(session);
    session.frame(Vec::new());
    session.toggle_in_view_menu("Start");
    assert!(session.screen().has(Role::Window, "Start"));
    // A slice of the guest's time: the ROM has left its mark, and loops.
    let input = session.next_input(Vec::new());
    session.target.frame(|| input);
    let memory = &mut session.target.runtime.cpu.memory;
    assert_eq!(memory.read_word(MARK_AT), 0x4D52);
    memory.write_byte(MARK_AT, 0);
    memory.write_byte(MARK_AT + 1, 0);
    (session, host)
}

/// Runs the session's own frame, which takes up the site's answer before
/// the guest's slice, with no input, and then a pass of the test's to read
/// the screen if the overlay is shown.
fn frame_after_the_answer(session: &mut Driver<Session>) {
    let input = session.next_input(Vec::new());
    session.target.frame(|| input);
    if session.target.overlay.visible() {
        session.frame(Vec::new());
    }
}

/// A start pressed with the pointer: the disk is fetched, and the panel
/// says so with its buttons greyed out; once it has come the floppy is in
/// A:, the bootstrap tries A: first, and the machine has restarted and run.
/// The Start panel closes and the overlay hides, so that the keys go to the
/// guest; shown again, it says what the machine restarted with.
#[test]
fn a_start_pressed_with_the_pointer_hides_the_overlay() {
    let (mut session, host) = at_the_start_panel();
    session.click(Role::Button, BOOT);
    let (path, answer) = host.fetch_asked();
    assert_eq!(path, "disks/msdos400.img");
    let screen = session.wait(1.0);
    assert!(screen.shows("Fetching MS-DOS 4.00..."));
    assert!(!screen.enabled(Role::Button, BOOT) && !screen.enabled(Role::Button, PLAY));

    answer.send(Ok(vec![0xF6; 163_840])).unwrap();
    frame_after_the_answer(&mut session);
    let target = &session.target;
    assert!(!target.overlay.visible());
    assert!(!target.overlay.wants_keyboard());
    let disks = target.runtime.disks.borrow();
    let (_, floppy) = disks
        .drives
        .iter()
        .find(|(drive, _)| *drive == 0x00)
        .unwrap();
    assert_eq!(floppy.name(), Some("msdos400.img"));
    assert_eq!(disks.boot_order(), [0x00, 0x80]);
    drop(disks);
    assert_eq!(
        target.runtime.cpu.memory.read_word(MARK_AT),
        0x4D52,
        "the machine restarted"
    );

    // Shown again, the overlay lets the window it closed while hidden fade
    // out, as it does any window that closes.
    session.target.overlay_key(ElementState::Pressed, false);
    session.frame(Vec::new());
    let screen = session.wait(1.0);
    assert!(!screen.has(Role::Window, "Start"));
    assert!(
        screen.shows("Restarted with MS-DOS 4.00, the floppy in A:"),
        "{:?}",
        screen.texts()
    );
}

/// A start carried out runs the machine it restarted, a paused one too:
/// what the Start panel's buttons promise is a machine to type at or to
/// play, and the overlay that marked the pause hides with the start.
#[test]
fn a_start_runs_a_paused_machine() {
    let (mut session, host) = at_the_start_panel();
    session.click(Role::Button, "Machine");
    session.click(Role::Button, "Pause");
    assert!(session.target.paused());

    session.click(Role::Button, BOOT);
    let (_, answer) = host.fetch_asked();
    answer.send(Ok(vec![0xF6; 163_840])).unwrap();
    frame_after_the_answer(&mut session);
    assert!(!session.target.overlay.visible());
    assert!(!session.target.paused());
    assert_eq!(
        session.target.runtime.cpu.memory.read_word(MARK_AT),
        0x4D52,
        "the machine restarted and ran"
    );
}

/// A start pressed with a finger: once the disk has come the hard disk is
/// the first hard disk, the bootstrap tries it first, A: keeps its floppy,
/// and the machine has restarted and run. The Start panel closes, and the
/// overlay stays shown with the on-screen keyboard open, for a touch screen
/// that has no keys, which go to the guest.
#[test]
fn a_start_pressed_with_a_finger_opens_the_on_screen_keyboard() {
    let (mut session, host) = at_the_start_panel();
    let play = session.screen().rect(Role::Button, PLAY).center();
    session.frame(first_finger_down(1, play));
    session.frame(first_finger_up(1, play));
    let (path, answer) = host.fetch_asked();
    assert_eq!(path, "disks/doom8088.img");

    answer.send(Ok(vec![0x5A; 2 * 16 * 63 * 512])).unwrap();
    frame_after_the_answer(&mut session);
    session.wait(1.0);
    let screen = session.screen();
    assert!(session.target.overlay.visible());
    assert!(screen.has(Role::Window, "On-screen keyboard"));
    assert!(!screen.has(Role::Window, "Start"));
    assert!(screen.shows("Restarted with DOOM, the hard disk as C:"));
    assert!(!session.target.overlay.wants_keyboard());

    let target = &session.target;
    let disks = target.runtime.disks.borrow();
    let held: Vec<(u8, Option<&str>)> = disks
        .drives
        .iter()
        .map(|(drive, image)| (*drive, image.name()))
        .collect();
    assert!(
        held.contains(&(0x00, None)),
        "A: keeps its floppy: {held:?}"
    );
    assert!(held.contains(&(0x80, Some("doom8088.img"))), "{held:?}");
    assert_eq!(disks.boot_order(), [0x80, 0x00]);
    drop(disks);
    assert_eq!(target.runtime.cpu.memory.read_word(MARK_AT), 0x4D52);
}

/// A start pressed with a finger shows the on-screen keyboard once the disk
/// has come, even if the overlay was hidden while the disk was on its way:
/// a touch screen has no other keys to type at the guest with.
#[test]
fn a_finger_start_shows_the_keyboard_after_the_overlay_was_hidden_meanwhile() {
    let (mut session, host) = at_the_start_panel();
    let boot = session.screen().rect(Role::Button, BOOT).center();
    session.frame(first_finger_down(1, boot));
    session.frame(first_finger_up(1, boot));
    let (_, answer) = host.fetch_asked();
    session.target.overlay_key(ElementState::Pressed, false);
    assert!(!session.target.overlay.visible());

    answer.send(Ok(vec![0xF6; 163_840])).unwrap();
    frame_after_the_answer(&mut session);
    assert!(session.target.overlay.visible());
    let screen = session.wait(1.0);
    assert!(screen.has(Role::Window, "On-screen keyboard"));
    assert!(!screen.has(Role::Window, "Start"));
    assert!(!session.target.overlay.wants_keyboard());
}

/// A start that fails changes nothing of the overlay either: the Start
/// panel stays open, its buttons offered again, and the message says why.
#[test]
fn a_start_that_fails_leaves_the_start_panel_open() {
    let (mut session, host) = at_the_start_panel();
    session.click(Role::Button, PLAY);
    let (_, answer) = host.fetch_asked();
    answer
        .send(Err("The site has no disks/doom8088.img".to_string()))
        .unwrap();
    frame_after_the_answer(&mut session);
    session.wait(1.0);
    let screen = session.screen();
    assert!(session.target.overlay.visible());
    assert!(screen.has(Role::Window, "Start"));
    assert!(screen.enabled(Role::Button, PLAY) && screen.enabled(Role::Button, BOOT));
    assert!(screen.shows("The site has no disks/doom8088.img. The machine runs on as it was"));
    assert_eq!(session.target.runtime.cpu.memory.read_word(MARK_AT), 0);
}

/// What the hint says on the host the tests run on, a desktop, whose
/// switch is F12.
const HINT: &str = "F12: tools";

/// On a host that came from a site, a machine that starts with no disk in
/// any drive has nothing to boot but the site's disks: the session starts
/// with the overlay shown and the Start panel open. On a host that came
/// from none, or with a disk named for the start, the overlay starts hidden
/// as it always has, with the hint that names its switch.
#[test]
fn the_start_panel_opens_at_start_on_a_site_with_no_disk() {
    for (has_site, with_a_floppy, opens) in [
        (true, false, true),
        (false, false, false),
        (true, true, false),
    ] {
        let (kbd_tx, _sent) = channel();
        let runtime = machine_running(&BUSY_ROM, with_a_floppy);
        let session = Session::on(runtime, kbd_tx, has_site);
        let case = format!("a site: {has_site}, a floppy: {with_a_floppy}");
        assert_eq!(session.overlay.visible(), opens, "{case}");
        let mut session = Driver::new(session);
        session.frame(Vec::new());
        let screen = session.wait(1.0);
        assert_eq!(screen.has(Role::Window, "Start"), opens, "{case}");
        assert_eq!(screen.has(Role::Button, BOOT), opens, "{case}");
        assert_eq!(screen.shows(HINT), !opens, "{case}");
    }
}

/// The overlay the session showed at the start was not shown by the user,
/// who has yet to find its switch: once a start from its Start panel has
/// hidden it, however long the user took to choose, the hint names the
/// switch on the bare picture for its seconds.
#[test]
fn once_a_start_hides_the_overlay_shown_at_start_the_hint_names_the_switch() {
    let (session, host) = on_a_site(machine_running(&BUSY_ROM, false));
    let mut session = Driver::new(session);
    session.frame(Vec::new());
    session.wait(60.0);
    assert!(!session.screen().shows(HINT), "the overlay is shown");

    session.click(Role::Button, BOOT);
    let (_, answer) = host.fetch_asked();
    answer.send(Ok(vec![0xF6; 163_840])).unwrap();
    frame_after_the_answer(&mut session);
    assert!(!session.target.overlay.visible());
    assert!(session.frame(Vec::new()).shows(HINT));
    assert!(session.wait(4.0).shows(HINT));
    assert!(!session.wait(1.0).shows(HINT), "its seconds are over");
    assert!(!session.target.overlay.draws());
}
