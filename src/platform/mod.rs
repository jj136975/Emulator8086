//! What depends on the host the app runs on, behind one set of names. The
//! rest of the app calls these names and never asks which host it is on:
//! `native` is the desktop, with its command line, its files and a thread
//! that may block.
//!
//! What a host offers:
//!
//! - `init`: where the log goes.
//! - `options`: what the start asks of the machine (`crate::Options`).
//! - `fail`: tells the user of an error at start-up, and ends the process.
//! - `run_app`: runs the winit application in the host's event loop.
//! - `create_window`, `set_title`: the window the machine is shown in.
//! - `spawn`: runs a future to its end, at once if the host may block.
//! - `OverlayInput`: the window's events as egui's input.
//! - `OVERLAY_SWITCH`: how the user shows and hides the overlay
//!   (`OverlaySwitch`).
//! - `wait`: lets host time go by, and says whether the host could.
//! - `fat_options`: the FAT mount options that carry the host's local time.
//! - `save_text`: puts a text where the user can read it.
//!
//! and the clocks, which are `web_time`'s on every host: `Instant`,
//! `SystemTime`, `UNIX_EPOCH`.
//!
//! The invariant: this directory is the only part of the app that knows
//! which host it is built for. Outside it no source file names
//! `cfg(target_arch`, `wasm_bindgen`, `web_sys` or `js_sys`, and none
//! sleeps, blocks on a future (`block_on`) or names a clock of `std::time`:
//! not every host has a thread that may stand still or a clock std can
//! read. The rule is a test (`tests.rs`).

mod native;

use winit::event::MouseButton;
use winit::keyboard::KeyCode;

pub use native::*;
pub use web_time::{Instant, SystemTime, UNIX_EPOCH};

/// How the user shows and hides the overlay on a host: with a key, with a
/// button of the pointer, or with either.
pub struct OverlaySwitch {
    /// The key that does it. It is the window's: neither the overlay nor
    /// the guest sees it.
    pub key: Option<KeyCode>,
    /// The button of the pointer that does it, pressed on the picture. Where
    /// the overlay has something under the pointer, the press is the
    /// overlay's own.
    pub button: Option<MouseButton>,
    /// What the overlay calls the switch, in its hint and in its About
    /// panel.
    pub name: &'static str,
}

#[cfg(test)]
mod tests;
