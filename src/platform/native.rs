//! The desktop side of `platform`: a host with a command line, a stderr,
//! files, and a thread that may sleep. `Cli` is the command line (its doc
//! comments are the `--help` text), which `options` turns into what the
//! start asks of the machine.
//!
//! The invariant: every name here keeps the meaning `platform` gives it for
//! all hosts. A caller that works with this side works with any other.

use std::future::Future;
use std::path::{Path, PathBuf};
use std::time::Duration;

use clap::Parser;
use winit::application::ApplicationHandler;
use winit::dpi::PhysicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::KeyCode;
use winit::window::Window;

use super::OverlaySwitch;
use crate::Options;

/// How the overlay is shown and hidden: with F12, a key no XT keyboard has,
/// so that the guest misses nothing.
pub const OVERLAY_SWITCH: OverlaySwitch = OverlaySwitch {
    key: Some(KeyCode::F12),
    button: None,
    name: "F12",
};

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

/// Runs `task` to its end. Here that is before this call returns: the
/// thread may block on what the task waits for. A host whose thread may not
/// runs the task later, from its event loop, so a caller takes what the
/// task made from where the task put it, and not before it is there.
pub fn spawn(task: impl Future<Output = ()> + 'static) {
    pollster::block_on(task);
}

/// The window's events as egui's input, and what a pass of egui asks of the
/// window in return: here, egui-winit's.
pub struct OverlayInput(egui_winit::State);

impl OverlayInput {
    /// The input of the passes of `ctx`, gathered from `window`.
    pub fn new(ctx: egui::Context, window: &Window) -> Self {
        Self(egui_winit::State::new(
            ctx,
            egui::ViewportId::ROOT,
            window,
            Some(window.scale_factor() as f32),
            window.theme(),
            None,
        ))
    }

    /// The longest side a texture of the overlay may have: the limit of
    /// the device it is drawn with, known once there is one.
    pub fn set_max_texture_side(&mut self, side: usize) {
        self.0.set_max_texture_side(side);
    }

    /// Makes `event` part of the next pass's input. Returns whether egui
    /// keeps the event for itself.
    pub fn on_window_event(&mut self, window: &Window, event: &WindowEvent) -> bool {
        self.0.on_window_event(window, event).consumed
    }

    /// Drops what was gathered: no pass will take it.
    pub fn discard(&mut self) {
        let input = self.0.egui_input_mut();
        input.events.clear();
        input.hovered_files.clear();
        input.dropped_files.clear();
    }

    /// The input of a pass that is run now: all that was gathered since
    /// the last call.
    pub fn take(&mut self, window: &Window) -> egui::RawInput {
        self.0.take_egui_input(window)
    }

    /// Heeds what a pass asks of the window: the cursor, the IME.
    pub fn handle_output(&mut self, window: &Window, output: egui::PlatformOutput) {
        self.0.handle_platform_output(window, output);
    }
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
