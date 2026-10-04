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
use crate::gui::overlay::testing::{Driver, Passes, input, key};
use crate::wall_clock::HostClock;
use EmulatorEvent::{ScanRelease, Scancode};
use emu8086_core::BIOS_ROM;
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
    runtime.load_rom_image(rom);
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
