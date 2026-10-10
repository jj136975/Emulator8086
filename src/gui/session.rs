//! A session of the emulator: the machine, the pacer that feeds it time,
//! the meter that measures it, the guest's keyboard, the fingers on the
//! screen and the overlay. It is all of the app that needs no window: the
//! window (`gui`) gathers the input and draws, and what the input means is
//! decided here: whose a key is, when a finger held on the picture works
//! the overlay's switch (`hold`), and what a command of the overlay does to
//! the machine. What the Disks panel asks for is carried out by
//! `disk_tools`, which has the file dialogs to wait for.
//!
//! The invariants: the keyboard has one owner at a time. A key goes to the
//! guest unless the overlay kept it, and the moment the overlay has the
//! keyboard (one of its widgets has the focus, or one of its menus is open)
//! the keys the guest holds are let go: their releases will go to the
//! overlay, and a key the guest never sees released stays down in it. And
//! a key of the on-screen keyboard is held from the screen: the moment the
//! keyboard is not on it (hidden with the overlay, closed, or not drawn by
//! a pass), the keys the guest holds from there are let go.

use std::path::PathBuf;
use std::sync::Arc;
use std::sync::mpsc::Sender;
use std::time::Duration;

use winit::event::{ElementState, TouchPhase};
use winit::keyboard::PhysicalKey;
use winit::window::Window;

use emu8086_core::io::keyboard::EmulatorEvent;
use emu8086_core::vm::runtime::Runtime;

use super::disk_tools::DiskTools;
use super::hold::Hold;
use super::keys::GuestKeys;
use super::meter::{Counts, Meter};
use super::overlay::{Command, Overlay, Panel};
use super::pacer::Pacer;
use super::renderer::Drawn;
use crate::platform::{self, Instant};

/// Host time the CPU runs for before each frame is drawn. The rest of the
/// frame period is left for rasterising and for waiting on vsync.
const CPU_SLICE: Duration = Duration::from_millis(10);

/// The file the VGA dump goes to: on a host with files, in the working
/// directory.
const VGA_DUMP_FILE: &str = "vga_dump.txt";

pub struct Session {
    pub runtime: Runtime,
    pacer: Pacer,
    /// What the frames measured of the emulation, which the overlay shows.
    meter: Meter,
    keys: GuestKeys,
    /// The fingers on the screen, one of which held still on the picture
    /// works the overlay's switch.
    hold: Hold,
    pub overlay: Overlay,
    /// Where `Command::DumpVga` puts the dump.
    dump_file: PathBuf,
    /// What carries out `Command::Disk`, and the file dialogs it has open.
    pub(super) disks: DiskTools,
}

impl Session {
    /// `kbd_tx` feeds the keyboard controller `runtime` was built with.
    pub fn new(runtime: Runtime, kbd_tx: Sender<EmulatorEvent>) -> Self {
        Self {
            runtime,
            pacer: Pacer::new(),
            meter: Meter::default(),
            keys: GuestKeys::new(kbd_tx),
            hold: Hold::default(),
            overlay: Overlay::new(),
            dump_file: PathBuf::from(VGA_DUMP_FILE),
            disks: DiskTools::new(),
        }
    }

    /// The window the machine is shown in is there: the file dialogs of the
    /// disk tools belong to it from now on. The session needs none.
    pub fn shown_in(&mut self, window: &Arc<Window>) {
        self.disks.set_window(window);
    }

    /// The overlay's key went down or came up. Going down, it shows the
    /// overlay or hides it, once however long it is held: `repeat` says the
    /// key is only still down.
    pub fn overlay_key(&mut self, state: ElementState, repeat: bool) {
        if state == ElementState::Pressed && !repeat {
            self.toggle_overlay();
        }
    }

    /// The overlay's button of the pointer went down or came up. `kept`
    /// says that egui, which was offered the event first, has a use for the
    /// pointer where it is: over the overlay, a press is the overlay's own.
    /// Going down on the picture, the button shows the overlay or hides it.
    pub fn overlay_button(&mut self, state: ElementState, kept: bool) {
        if state == ElementState::Pressed && !kept {
            self.toggle_overlay();
        }
    }

    /// Finger `finger` came down, moved, lifted or was taken away by the
    /// host, at `at`, in points, at `now`: the session watches for one held
    /// still on the picture. The window offers the event to egui as well.
    pub fn touch(&mut self, finger: u64, phase: TouchPhase, at: egui::Pos2, now: Instant) {
        self.hold.touch(finger, phase, at, now);
    }

    /// Shows the overlay or hides it if, by `now`, a finger has been held
    /// still on the picture long enough. A finger rests between its events:
    /// this is asked at every frame, whether the overlay is shown or not,
    /// and no pass is run for it. What lies under the finger is what the
    /// overlay's last pass laid out there (`Overlay::covers`).
    pub fn hold(&mut self, now: Instant) {
        let overlay = &self.overlay;
        if self.hold.due(now, |at| overlay.covers(at)) {
            self.toggle_overlay();
        }
    }

    /// Shows the overlay, or hides it.
    fn toggle_overlay(&mut self) {
        self.overlay.toggle();
        // A hidden overlay has no keyboard on the screen.
        self.release_unshown_keys();
        // An overlay that comes back with the focus where it left it has
        // the keyboard at once.
        self.yield_keyboard();
    }

    /// Any other key of the window went down or came up. `kept` says that
    /// egui, which was offered the event first, kept it for itself. While
    /// the overlay has the keyboard every key is its own, whether egui has
    /// a use for it or not.
    pub fn key(&mut self, key: PhysicalKey, state: ElementState, kept: bool) {
        let taken = kept || self.overlay.wants_keyboard();
        self.keys.key(key, state, taken);
    }

    /// The window lost the keyboard: the releases of the keys held now go
    /// to whoever has it next.
    pub fn focus_lost(&mut self) {
        self.keys.release_all();
    }

    /// Whether the guest is held still (Machine > Pause).
    pub fn paused(&self) -> bool {
        self.pacer.paused()
    }

    /// One frame: the answers of the file dialogs that have come in, a
    /// slice of the guest's time, which a paused guest lets go by, and the
    /// meter told of it whether the overlay is shown or not; then a pass of
    /// the overlay if it has anything to show. `input` is asked for only
    /// then. Returns whether there was a pass: something to paint, and
    /// wishes of egui for the window to heed.
    pub fn frame(&mut self, input: impl FnOnce() -> egui::RawInput) -> bool {
        // Shown or hidden, the overlay is told what came of them: a dialog
        // may be answered long after the tools were put away.
        self.disks.poll(&mut self.runtime, self.overlay.messages());
        let slice = self.pacer.run_slice(&mut self.runtime, CPU_SLICE);
        self.meter.frame(slice, Counts::read(&self.runtime));
        let draws = self.overlay.draws();
        if draws {
            self.run_overlay(input());
        }
        draws
    }

    /// What the window drew after the frame's slice, as the renderer reports
    /// it: the meter counts it in the frame that the next slice ends.
    pub fn drawn(&mut self, drawn: Drawn) {
        self.meter.drawn(drawn);
    }

    /// One pass of the overlay over `input`, and what follows from it: the
    /// commands it asked for are carried out, the keys of an on-screen
    /// keyboard the pass did not draw are let go, and the keyboard is given
    /// up if the pass took it. The meter is told how long all of that took.
    fn run_overlay(&mut self, input: egui::RawInput) {
        let started = Instant::now();
        let paused = self.pacer.paused();
        for command in self
            .overlay
            .run(input, &mut self.runtime, paused, &self.meter)
        {
            self.execute(command);
        }
        self.release_unshown_keys();
        self.yield_keyboard();
        self.meter.overlay_pass(started.elapsed());
    }

    /// Lets go of the keys the guest holds if the overlay has the keyboard.
    fn yield_keyboard(&mut self) {
        if self.overlay.wants_keyboard() {
            self.keys.release_all();
        }
    }

    /// Lets go of the keys the guest holds from the on-screen keyboard if
    /// the keyboard is not on the screen: the overlay is hidden, or its
    /// last pass did not draw the panel. Nothing holds them there anymore.
    fn release_unshown_keys(&mut self) {
        if !self.overlay.shows(Panel::OnScreenKeyboard) {
            self.keys.release_screen();
        }
    }

    /// Carries out what a tool of the overlay asked for, and says in the
    /// overlay what came of it. A pause needs no message: the overlay marks
    /// a paused machine for as long as it is one.
    fn execute(&mut self, command: Command) {
        match command {
            Command::Pause => self.pacer.set_paused(true),
            Command::Resume => self.pacer.set_paused(false),
            Command::Reset => {
                self.runtime.reset();
                self.overlay.messages().done("Machine reset");
            }
            Command::DumpVga => {
                // The one place where the session hands a text to the host,
                // which puts it where its user can read it: not every host
                // has files.
                match self.runtime.vga_dump() {
                    Some(dump) => match platform::save_text(&self.dump_file, &dump) {
                        Ok(saved) => self.overlay.messages().done(format!("VGA state {saved}")),
                        Err(error) => self.overlay.messages().failed(error),
                    },
                    None => self.overlay.messages().failed("The machine has no VGA"),
                }
            }
            Command::Disk(command) => {
                self.disks
                    .execute(command, &mut self.runtime, self.overlay.messages());
            }
            Command::Key { make, pressed } => {
                let state = if pressed {
                    ElementState::Pressed
                } else {
                    ElementState::Released
                };
                // A press while the overlay has the keyboard is the
                // overlay's, as a key of the host's would be.
                let taken = self.overlay.wants_keyboard();
                self.keys.screen_key(make, state, taken);
            }
        }
    }
}

#[cfg(test)]
#[path = "session_tests.rs"]
mod tests;
