//! The disk tools' side of the session: what the Disks panel asks for
//! (`DiskCommand`) becomes an operation of the core on the machine's
//! drives (`disk_ops::Operation`). A command that says all there is to say
//! is carried out at once; one that needs a file of the user first asks for
//! it in the host's dialog, and is carried out when the answer is there.
//! Either way the overlay's messages say what came of it.
//!
//! The host is reached through `FileHost` and no other way (`gui::host`):
//! `platform` for the app, hand-made answers for a test. How its files
//! come, by their paths or by their contents (`FileAccess`), decides what a
//! command does: a disk is saved to a file the user names, or handed over
//! as a download.
//!
//! The invariant: nothing here waits for the user. A dialog is asked for
//! and the frame goes on with the guest running; the answer arrives through
//! a channel, and `poll`, once a frame, takes up those that are there. Any
//! number of dialogs may be open: each is answered when its answer comes,
//! in whatever order, and one that is never answered costs nothing.

use std::path::Path;
use std::sync::Arc;

use winit::window::Window;

use emu8086_core::io::disk_ops::{self, Disk, HostFile, Operation};
use emu8086_core::io::disk_spec::drive_label;
use emu8086_core::vm::runtime::Runtime;

use super::host::{Awaited, FileHost, Platform};
use super::overlay::{DiskCommand, Messages};
use crate::platform::{self, FileAccess, FileQuestion, Wanted};
use crate::wall_clock::HostClock;

/// What a message says after an operation that changed a hard disk. The
/// Disks panel says it at length, and for as long as it holds.
const REBOOT: &str = "Reboot the guest before it uses the drive";

/// What a dialog was opened for: the operation its answer completes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Purpose {
    Insert { drive: u8, read_only: bool },
    Create { drive: u8 },
    Save { drive: u8 },
    Put { drive: u8 },
}

/// What carries out the Disks panel's commands: the host it asks for files,
/// and the dialogs whose answers it waits for.
pub struct DiskTools {
    host: Box<dyn FileHost>,
    /// How the host's files come, which decides what a command does.
    access: FileAccess,
    /// The dialogs asked for whose answers have not been taken up.
    asked: Awaited<Purpose, Vec<HostFile>>,
}

impl DiskTools {
    /// The tools on the host the app runs on. Their dialogs belong to no
    /// window until one is named (`set_window`).
    pub fn new() -> Self {
        Self::on(Box::new(Platform::windowless()), platform::FILE_ACCESS)
    }

    /// The tools on `host`, whose files come by `access`.
    pub(super) fn on(host: Box<dyn FileHost>, access: FileAccess) -> Self {
        Self {
            host,
            access,
            asked: Awaited::default(),
        }
    }

    /// Names the window the machine is shown in: the dialogs asked for
    /// from now on belong to it.
    pub fn set_window(&mut self, window: &Arc<Window>) {
        self.host = Box::new(Platform::of(window));
    }

    /// Carries `command` out on the drives of `runtime`, or asks the user
    /// for the file it needs first, and says in `messages` what came of
    /// it.
    pub fn execute(
        &mut self,
        command: DiskCommand,
        runtime: &mut Runtime,
        messages: &mut Messages,
    ) {
        match command {
            DiskCommand::InsertBlank {
                drive,
                kb,
                read_only,
            } => {
                let disk = Disk::Blank { kb };
                let operation = Operation::Insert {
                    drive,
                    disk,
                    read_only,
                };
                apply(operation, runtime, messages);
            }
            DiskCommand::Eject { drive } => apply(Operation::Eject { drive }, runtime, messages),
            DiskCommand::Move { from, to } => {
                apply(Operation::Move { from, to }, runtime, messages);
            }
            DiskCommand::InsertImage { drive, read_only } => self.ask(
                Purpose::Insert { drive, read_only },
                format!("Insert a disk image into {}", drive_label(drive)),
                Wanted::Image,
            ),
            DiskCommand::CreateImage { drive } => match self.access {
                FileAccess::Paths => self.ask(
                    Purpose::Create { drive },
                    format!("Create a disk image for {}", drive_label(drive)),
                    Wanted::NewImage {
                        suggested: default_name(drive),
                    },
                ),
                FileAccess::Contents => {
                    messages.failed("There is no file to create here: insert a blank disk");
                }
            },
            DiskCommand::Save { drive } => self.save(drive, runtime, messages),
            // No dialog is opened for a drive that has no disk to copy to:
            // the user would choose files for nothing.
            DiskCommand::PutFiles { drive } => {
                if holds_disk(drive, runtime, messages) {
                    self.ask(
                        Purpose::Put { drive },
                        format!("Copy files onto the disk in {}", drive_label(drive)),
                        Wanted::Files,
                    );
                }
            }
            DiskCommand::PutFolder { drive } => match self.access {
                FileAccess::Paths => {
                    if holds_disk(drive, runtime, messages) {
                        self.ask(
                            Purpose::Put { drive },
                            format!("Copy a folder onto the disk in {}", drive_label(drive)),
                            Wanted::Folder,
                        );
                    }
                }
                FileAccess::Contents => {
                    messages.failed("There is no folder to read here: put its files");
                }
            },
        }
    }

    /// Takes up the answers that have arrived since the last call, and
    /// carries out what each dialog was opened for. A dialog the user
    /// closed without choosing asked for nothing.
    pub fn poll(&mut self, runtime: &mut Runtime, messages: &mut Messages) {
        for (purpose, files) in self.asked.answered() {
            let Some(files) = files else {
                messages.failed("The file dialog could not be shown");
                continue;
            };
            if let Some(operation) = operation(purpose, files, messages) {
                apply(operation, runtime, messages);
            }
        }
    }

    /// Opens a dialog headed `title` for `wanted`, to be answered later.
    fn ask(&mut self, purpose: Purpose, title: String, wanted: Wanted) {
        let answer = self.host.ask(FileQuestion { title, wanted });
        self.asked.push(purpose, answer);
    }

    /// Hands the disk in `drive` to the user. Where files have paths, the
    /// user names the file first, and the disk is saved there once they
    /// have; where they have none, the host takes the disk's contents now.
    fn save(&mut self, drive: u8, runtime: &mut Runtime, messages: &mut Messages) {
        let label = drive_label(drive);
        let Some(name) = image_name(runtime, drive) else {
            messages.failed(format!("{label} is empty"));
            return;
        };
        match self.access {
            FileAccess::Paths => self.ask(
                Purpose::Save { drive },
                format!("Save the disk in {label}"),
                Wanted::NewImage { suggested: name },
            ),
            FileAccess::Contents => {
                // The contents, never `Operation::Save`: that names a file
                // and the process, and a host of this kind has neither.
                let contents = disk_ops::contents(&mut runtime.disks.borrow_mut(), drive);
                let saved =
                    contents.and_then(|bytes| self.host.save_bytes(Path::new(&name), &bytes));
                match saved {
                    Ok(saved) => messages.done(format!("{label} {saved}")),
                    Err(reason) => messages.failed(reason),
                }
            }
        }
    }
}

/// The operation a dialog opened for `purpose` comes to, now that the user
/// chose `files`: none if they chose nothing, or what cannot be used, which
/// `messages` is told.
fn operation(
    purpose: Purpose,
    mut files: Vec<HostFile>,
    messages: &mut Messages,
) -> Option<Operation> {
    if files.is_empty() {
        return None;
    }
    Some(match purpose {
        Purpose::Insert { drive, read_only } => Operation::Insert {
            drive,
            disk: Disk::Image(files.swap_remove(0)),
            read_only,
        },
        Purpose::Put { drive } => Operation::Put { drive, files },
        Purpose::Create { drive } | Purpose::Save { drive } => {
            // A file to write is named by its path, and by nothing else.
            let HostFile::Path(path) = files.swap_remove(0) else {
                messages.failed("The host named no file to write");
                return None;
            };
            match purpose {
                Purpose::Create { .. } => Operation::Create { drive, path },
                _ => Operation::Save { drive, path },
            }
        }
    })
}

/// Carries `operation` out on the drives of `runtime` and says in
/// `messages` what came of it, and that the guest has to be rebooted if a
/// hard disk was changed.
fn apply(operation: Operation, runtime: &mut Runtime, messages: &mut Messages) {
    let outcome = disk_ops::apply(&mut runtime.disks.borrow_mut(), operation, &HostClock);
    let needs_reboot = outcome.needs_reboot;
    let with_reboot = |text: String| {
        if needs_reboot {
            format!("{text}. {REBOOT}")
        } else {
            text
        }
    };
    match outcome.result {
        Ok(done) => messages.done(with_reboot(done)),
        Err(reason) => messages.failed(with_reboot(reason)),
    }
}

/// Whether `drive` holds a disk. If it holds none, `messages` is told.
fn holds_disk(drive: u8, runtime: &Runtime, messages: &mut Messages) -> bool {
    let holds = image_name(runtime, drive).is_some();
    if !holds {
        messages.failed(format!("{} is empty", drive_label(drive)));
    }
    holds
}

/// What the image of the disk in `drive` is called when it is handed to
/// the user: the name of its own file if it has one, and a name made of the
/// drive's if it has none. `None` if the drive is empty.
fn image_name(runtime: &Runtime, drive: u8) -> Option<String> {
    let disks = runtime.disks.borrow();
    let (_, image) = disks.drives.iter().find(|(held, _)| *held == drive)?;
    let own = match image.backing_path() {
        Some(path) => path
            .file_name()
            .map(|name| name.to_string_lossy().into_owned()),
        None => image.name().map(str::to_string),
    };
    Some(own.unwrap_or_else(|| default_name(drive)))
}

/// A file name for the image of a disk that has none of its own, made of
/// its drive's: `floppy-a.img`, `hd0.img`.
fn default_name(drive: u8) -> String {
    let label = drive_label(drive).to_ascii_lowercase();
    match label.strip_suffix(':') {
        Some(letter) => format!("floppy-{letter}.img"),
        None => format!("{label}.img"),
    }
}

#[cfg(test)]
#[path = "disk_tools_tests.rs"]
mod tests;
