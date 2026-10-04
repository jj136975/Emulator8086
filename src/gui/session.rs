//! A session of the emulator: the machine, the pacer that feeds it time,
//! the guest's keyboard and the overlay. It is all of the app that needs no
//! window: the window (`gui`) gathers the input and draws, and what the
//! input means is decided here: whose a key is, and what a command of the
//! overlay does to the machine.
//!
//! The invariant: the keyboard has one owner at a time. A key goes to the
//! guest unless the overlay kept it, and the moment the overlay has the
//! keyboard (one of its widgets has the focus, or one of its menus is open)
//! the keys the guest holds are let go: their releases will go to the
//! overlay, and a key the guest never sees released stays down in it.

use std::path::PathBuf;
use std::sync::mpsc::Sender;
use std::time::Duration;

use winit::event::ElementState;
use winit::keyboard::PhysicalKey;

use emu8086_core::io::keyboard::EmulatorEvent;
use emu8086_core::vm::runtime::Runtime;

use super::keys::GuestKeys;
use super::overlay::{Command, Overlay};
use super::pacer::Pacer;
use crate::platform;

/// Host time the CPU runs for before each frame is drawn. The rest of the
/// frame period is left for rasterising and for waiting on vsync.
const CPU_SLICE: Duration = Duration::from_millis(10);

/// The file the VGA dump goes to: on a host with files, in the working
/// directory.
const VGA_DUMP_FILE: &str = "vga_dump.txt";

pub struct Session {
    pub runtime: Runtime,
    pacer: Pacer,
    keys: GuestKeys,
    pub overlay: Overlay,
    /// Where `Command::DumpVga` puts the dump.
    dump_file: PathBuf,
}

impl Session {
    /// `kbd_tx` feeds the keyboard controller `runtime` was built with.
    pub fn new(runtime: Runtime, kbd_tx: Sender<EmulatorEvent>) -> Self {
        Self {
            runtime,
            pacer: Pacer::new(),
            keys: GuestKeys::new(kbd_tx),
            overlay: Overlay::new(),
            dump_file: PathBuf::from(VGA_DUMP_FILE),
        }
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

    /// Shows the overlay, or hides it.
    fn toggle_overlay(&mut self) {
        self.overlay.toggle();
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

    /// One frame: a slice of the guest's time, which a paused guest lets go
    /// by, then a pass of the overlay if it has anything to show. `input`
    /// is asked for only then. Returns whether there was a pass: something
    /// to paint, and wishes of egui for the window to heed.
    pub fn frame(&mut self, input: impl FnOnce() -> egui::RawInput) -> bool {
        self.pacer.run_slice(&mut self.runtime, CPU_SLICE);
        let draws = self.overlay.draws();
        if draws {
            self.run_overlay(input());
        }
        draws
    }

    /// One pass of the overlay over `input`, and what follows from it: the
    /// commands it asked for are carried out, and the keyboard is given up
    /// if the pass took it.
    fn run_overlay(&mut self, input: egui::RawInput) {
        let paused = self.pacer.paused();
        for command in self.overlay.run(input, &mut self.runtime, paused) {
            self.execute(command);
        }
        self.yield_keyboard();
    }

    /// Lets go of the keys the guest holds if the overlay has the keyboard.
    fn yield_keyboard(&mut self) {
        if self.overlay.wants_keyboard() {
            self.keys.release_all();
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
        }
    }
}

#[cfg(test)]
#[path = "session_tests.rs"]
mod tests;
