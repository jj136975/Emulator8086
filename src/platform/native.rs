//! The desktop side of `platform`: a host with a command line, a stderr,
//! files, and a thread that may sleep, and no site. `Cli` is the command
//! line (its doc comments are the `--help` text), which `options` turns
//! into what the start asks of the machine.
//!
//! The invariant: every name here keeps the meaning `platform` gives it for
//! all hosts. A caller that works with this side works with any other. That
//! holds for what may block as well: the thread of the window blocks where
//! `spawn` says it does and nowhere else, so a file dialog, which is open
//! for as long as its user likes, is waited for on a thread of its own.

use std::future::Future;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::mpsc::{self, Receiver};
use std::time::Duration;

use clap::Parser;
use winit::application::ApplicationHandler;
use winit::dpi::PhysicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::KeyCode;
use winit::window::Window;

use super::{Fetched, FileAccess, FileQuestion, OverlaySwitch, Wanted};
use crate::Options;
use emu8086_core::io::disk_ops::HostFile;

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
    /// Path to a BIOS ROM image to run in place of the built-in BIOS: 16 bytes to 64 KB, which
    /// end at F000:FFFF
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

/// Tells the user what a start did that they did not ask for in so many
/// words: on stderr, where `fail` tells its errors, whatever the level of
/// the log.
pub fn notice(message: &str) {
    eprintln!("{}", message);
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
    save_bytes(file, text.as_bytes())
}

/// Puts `bytes` where the user can get at them: in the file `file`. Returns
/// what was done with them, for a message to the user, or why it could not
/// be done.
pub fn save_bytes(file: &Path, bytes: &[u8]) -> Result<String, String> {
    match std::fs::write(file, bytes) {
        Ok(()) => Ok(format!("written to {}", file.display())),
        Err(error) => Err(format!("Cannot write {}: {error}", file.display())),
    }
}

/// How the user's files reach the app: by their paths.
pub const FILE_ACCESS: FileAccess = FileAccess::Paths;

/// A file dialog's answer on its way: the paths the user chose, none if
/// they chose nothing.
type Choosing = std::pin::Pin<Box<dyn Future<Output = Vec<PathBuf>> + Send>>;

/// The file dialogs of the desktop's system. Those of a window belong to
/// it: while one is open, the system keeps it in front of that window.
pub struct FileDialogs {
    parent: Option<Arc<Window>>,
}

impl FileDialogs {
    /// Dialogs that belong to no window: there is none yet.
    pub fn windowless() -> Self {
        Self { parent: None }
    }

    /// The dialogs of `window`.
    pub fn new(window: &Arc<Window>) -> Self {
        Self {
            parent: Some(window.clone()),
        }
    }

    /// Asks the user `question` in a dialog of the system, and returns at
    /// once. The answer arrives later through the channel returned: the
    /// files chosen, by their paths, or none if the user chose nothing. A
    /// channel that closes without an answer says that no dialog could be
    /// shown.
    pub fn ask(&self, question: FileQuestion) -> Receiver<Vec<HostFile>> {
        // No filter by extension: a disk image is known by what it holds,
        // not by its name, and a filter that left one out would hide it.
        let mut dialog = rfd::AsyncFileDialog::new().set_title(question.title);
        if let Some(parent) = &self.parent {
            dialog = dialog.set_parent(parent.as_ref());
        }
        // The dialog is made here, on the thread of the window: there are
        // systems that show one from no other thread.
        let choosing: Choosing = match question.wanted {
            // A ROM file is read by its path, and a longer one refused
            // unread (`Runtime::load_rom`).
            Wanted::Image | Wanted::Rom => {
                let picked = dialog.pick_file();
                Box::pin(async move { picked.await.into_iter().map(PathBuf::from).collect() })
            }
            Wanted::NewImage { suggested } => {
                let named = dialog.set_file_name(suggested).save_file();
                Box::pin(async move { named.await.into_iter().map(PathBuf::from).collect() })
            }
            Wanted::Files => {
                let picked = dialog.pick_files();
                Box::pin(async move { paths(picked.await) })
            }
            Wanted::Folder => {
                let picked = dialog.pick_folder();
                Box::pin(async move { picked.await.into_iter().map(PathBuf::from).collect() })
            }
        };
        answer_later(choosing)
    }
}

/// The paths of the files a dialog came back with.
fn paths(handles: Option<Vec<rfd::FileHandle>>) -> Vec<PathBuf> {
    handles
        .unwrap_or_default()
        .into_iter()
        .map(PathBuf::from)
        .collect()
}

/// Waits for `choosing` on a thread of its own, and sends what the user
/// chose down the channel returned. The thread of the window does not wait:
/// it looks into the channel when it comes by. If the thread cannot be
/// started, the channel closes with nothing in it.
fn answer_later(
    choosing: impl Future<Output = Vec<PathBuf>> + Send + 'static,
) -> Receiver<Vec<HostFile>> {
    let (answer, receiver) = mpsc::channel();
    let waiting = std::thread::Builder::new()
        .name("file dialog".to_string())
        .spawn(move || {
            let chosen = pollster::block_on(choosing);
            // Whoever asked may have gone since.
            let _ = answer.send(chosen.into_iter().map(HostFile::Path).collect());
        });
    if let Err(error) = waiting {
        log::error!("No thread to wait for a file dialog on: {error}");
    }
    receiver
}

/// The app came from no site: a desktop runs it as a program of its own.
pub const HAS_SITE: bool = false;

/// Asks the site for the file at `path` and returns at once. A desktop's
/// program has no site: the answer, there at once, says so.
pub fn fetch(path: &str) -> Receiver<Fetched> {
    let (answer, receiver) = mpsc::channel();
    // The receiver is still here: the answer cannot be lost.
    let _ = answer.send(Err(format!("There is no site here to fetch {path} from")));
    receiver
}

#[cfg(test)]
#[path = "native_tests.rs"]
mod tests;
