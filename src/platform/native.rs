//! The desktop side of `platform`: a host with a command line, a stderr,
//! files, and a thread that may sleep. `Cli` is the command line (its doc
//! comments are the `--help` text), which `options` turns into what the
//! start asks of the machine.
//!
//! The invariant: every name here keeps the meaning `platform` gives it for
//! all hosts. A caller that works with this side works with any other.

use std::path::{Path, PathBuf};
use std::time::Duration;

use clap::Parser;
use winit::application::ApplicationHandler;
use winit::dpi::PhysicalSize;
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::window::Window;

use crate::Options;

#[derive(Parser, Debug)]
#[command(name = "emulator8086", about = "Intel 8086 emulator")]
struct Cli {
    /// Path to a BIOS ROM image to run in place of the built-in BIOS
    rom: Option<PathBuf>,
    /// Floppy disk: SOURCE or DRIVE:SOURCE, with DRIVE a-d (default a) and SOURCE an image file,
    /// `memory` or `memory:KB` (e.g. a:boot.img, b:memory, a:C:\path\to\image.img).
    /// Append :ro to write-protect the disk.
    #[arg(long, action = clap::ArgAction::Append)]
    disk: Vec<String>,
    /// Hard disk image (e.g. --hd hd0.img). Multiple --hd flags for multiple drives.
    /// Creates a 32MB image if the file doesn't exist.
    #[arg(long, action = clap::ArgAction::Append)]
    hd: Vec<String>,
    /// Boot order: comma-separated list of drives (e.g. --boot hd0,a or --boot a,hd0).
    /// Default: a,hd0 (floppy first, then hard disk).
    #[arg(long)]
    boot: Option<String>,
    /// Enable register trace output
    #[arg(long)]
    trace: bool,
}

impl From<Cli> for Options {
    fn from(cli: Cli) -> Self {
        Self {
            rom: cli.rom,
            disk: cli.disk,
            hd: cli.hd,
            boot: cli.boot,
            trace: cli.trace,
        }
    }
}

/// Sends the log to stderr, at the levels `RUST_LOG` names.
pub fn init() {
    env_logger::init();
}

/// What the command line asks of the machine. A command line that cannot
/// be read, or that asks for the help, ends the process here, with clap's
/// message.
pub fn options() -> Options {
    Cli::parse().into()
}

/// Tells the user why the app cannot start, and ends the process with
/// status 1.
pub fn fail(message: &str) {
    eprintln!("error: {}", message);
    std::process::exit(1);
}

/// Runs `app` in the host's event loop until its window is closed.
pub fn run_app(mut app: impl ApplicationHandler + 'static) {
    let event_loop = EventLoop::new().expect("Failed to create event loop");
    // The app redraws without being asked to: the guest runs in its
    // redraws, so the loop never waits for an event.
    event_loop.set_control_flow(ControlFlow::Poll);
    event_loop.run_app(&mut app).expect("Event loop failed");
}

/// Opens the window the machine is shown in, `size` physical pixels large.
pub fn create_window(event_loop: &ActiveEventLoop, title: &str, size: PhysicalSize<u32>) -> Window {
    event_loop
        .create_window(
            Window::default_attributes()
                .with_title(title)
                .with_inner_size(size),
        )
        .expect("Failed to create window")
}

/// Changes the title the host shows for `window`.
pub fn set_title(window: &Window, title: &str) {
    window.set_title(title);
}

/// Lets `duration` of host time go by. Returns whether it did: a host
/// whose one thread must not block returns `false` at once, and the caller
/// has to come back later by its own means.
pub fn wait(duration: Duration) -> bool {
    std::thread::sleep(duration);
    true
}

/// The options a FAT volume is mounted with so that its files are stamped
/// with the host's local time. The time comes from fatfs's own provider,
/// which reads it through chrono and is there only in a fatfs built with
/// its `chrono` feature: the manifest turns that feature on, and without it
/// every file would be stamped 1980-01-01 00:00.
pub fn fat_options() -> fatfs::FsOptions {
    fatfs::FsOptions::new()
}

/// Puts `text` where the user can read it: in the file `file`. Returns
/// what was done with it, for a message to the user, or why it could not
/// be done.
pub fn save_text(file: &Path, text: &str) -> Result<String, String> {
    match std::fs::write(file, text) {
        Ok(()) => Ok(format!("written to {}", file.display())),
        Err(error) => Err(format!("Cannot write {}: {error}", file.display())),
    }
}

#[cfg(test)]
#[path = "native_tests.rs"]
mod tests;
