//! What depends on the host the app runs on, behind one set of names. There
//! are two hosts: `native` is a desktop, with its command line, its files
//! and a thread that may block; `web` is a browser, built for wasm32, where
//! the app is part of a page and has none of those. Each side defines all
//! the names, this module hands on the side that was built, and the rest of
//! the app calls them without asking which host it is on.
//!
//! What a host offers:
//!
//! - `init`: where the log goes and, in a browser, where a panic is told.
//! - `options`: what the start asks of the machine (`crate::Options`): a
//!   desktop's command line, and nothing in a browser.
//! - `fail`: tells the user of an error at start-up, and ends the process
//!   where there is one.
//! - `notice`: tells the user what a start did unasked, such as creating a
//!   disk image, where `fail` tells its errors.
//! - `run_app`: runs the winit application in the host's event loop: to its
//!   end on a desktop; in a browser the loop is the page's, and the call
//!   returns at once.
//! - `create_window`, `set_title`: the window the machine is shown in. In a
//!   browser it is a canvas, put into the page and as large as its
//!   viewport, and the title is the page's.
//! - `spawn`: runs a future to its end: at once on a desktop, which may
//!   block; later in a browser, which may not.
//! - `OverlayInput`: the window's events as egui's input.
//! - `OVERLAY_SWITCH`: how the user shows and hides the overlay
//!   (`OverlaySwitch`).
//! - `wait`: lets host time go by, and says whether the host could: a
//!   browser cannot.
//! - `fat_options`: the FAT mount options that carry the host's local time.
//! - `save_text`, `save_bytes`: put a text, or a file made here, where the
//!   user can get at it: in a file, or in a download.
//! - `FILE_ACCESS`: how the user's files reach the app (`FileAccess`): by
//!   their paths on a desktop, by their contents in a browser.
//! - `FileDialogs`: asks the user for files in the host's own dialog
//!   (`FileQuestion`) and does not wait for the answer, which comes later,
//!   down a channel, as `HostFile`s: paths, or names and contents.
//!
//! and the clocks, which are `web_time`'s on every host: `Instant`,
//! `SystemTime`, `UNIX_EPOCH`. They are std's on a desktop and the page's
//! in a browser, where std's panic.
//!
//! `egui_events` and `fat_stamp` are the parts of the browser's side that
//! know nothing of a browser. They are built on a desktop too, for their
//! tests alone.
//!
//! The invariant: this directory is the only part of the app that knows
//! which host it is built for. Outside it no source file names
//! `cfg(target_arch`, `wasm_bindgen`, `web_sys` or `js_sys`, and none
//! sleeps, blocks on a future (`block_on`) or names a clock of `std::time`:
//! a browser has no thread that may stand still and no clock std can read.
//! The rule is a test (`tests.rs`).

#[cfg(not(target_arch = "wasm32"))]
mod native;
#[cfg(target_arch = "wasm32")]
mod web;

#[cfg(any(target_arch = "wasm32", test))]
mod egui_events;
#[cfg(any(target_arch = "wasm32", test))]
mod fat_stamp;

use winit::event::MouseButton;
use winit::keyboard::KeyCode;

#[cfg(not(target_arch = "wasm32"))]
pub use native::*;
#[cfg(target_arch = "wasm32")]
pub use web::*;
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

/// How the files of the user reach the app on a host. The tools offer what
/// the host has, and no more.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FileAccess {
    /// By their paths: the app opens, creates and writes the files the
    /// user names, and reads a folder of them. A disk image stays in its
    /// file.
    Paths,
    /// By their contents: the user hands a file over, and the app is given
    /// its name and its bytes, never a path. What the app makes goes back
    /// as a download. There is no file to create and no folder to read.
    Contents,
}

/// What the user is asked in a file dialog (`FileDialogs::ask`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FileQuestion {
    /// What the dialog is headed with, where a host's dialog has a heading.
    pub title: String,
    pub wanted: Wanted,
}

/// What a file dialog is to come back with.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Wanted {
    /// One file that exists: a disk image.
    Image,
    /// A disk image to write: a new file, or one to replace, which the
    /// dialog has the user confirm. `suggested` is the name it offers.
    /// Asked only where files have paths.
    NewImage { suggested: String },
    /// Files that exist, one or more, of any kind.
    Files,
    /// A folder. Asked only where files have paths.
    Folder,
}

#[cfg(test)]
mod tests;
