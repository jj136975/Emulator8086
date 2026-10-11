//! The session's side of the Start panel: what the panel asks for
//! (`StartCommand`) becomes the machine started with a disk of the site.
//! The disk is fetched from the site (`FileHost::fetch`), and once it has
//! come (`poll`, at the start of every frame) it goes into the drive the
//! panel chose for its kind with the core's Insert (`disk_ops`), the
//! bootstrap is told to try that drive first, and the machine restarts at
//! the reset vector, as Machine > Reset restarts it. The overlay's messages
//! say what the machine restarted with, or why it runs on as it was.
//!
//! The host is reached through `FileHost` and no other way (`gui::host`):
//! `platform` for the app, hand-made answers for a test.
//!
//! The invariants: one start at a time, and a start that fails changes
//! nothing. While a disk is on its way, another start asked for is ignored
//! (the panel offers none meanwhile): a fetch that never ended would keep
//! the panel waiting, and a browser's fetch ends, with the file or with why
//! none came. A disk that does not come, or that the Insert refuses, leaves
//! the drives, the boot order and the running guest as they were. Nothing
//! here waits: the site's answer is taken up by the first poll after it
//! came.

use emu8086_core::io::disk_ops::{self, Disk, HostFile, Operation};
use emu8086_core::vm::runtime::Runtime;

use super::host::{Awaited, FileHost, Platform, fetched};
use super::overlay::{Messages, StartCommand};
use crate::platform::Fetched;
use crate::site::SiteFile;
use crate::wall_clock::HostClock;

/// What carries out the Start panel's commands: the host it fetches the
/// site's disks from, and the start whose disk is on its way.
pub struct Starter {
    host: Box<dyn FileHost>,
    /// The start asked for whose disk has not been taken up, if one was.
    fetched: Awaited<StartCommand, Fetched>,
}

impl Starter {
    /// The starter on the host the app runs on. A fetch belongs to no
    /// window: the host needs none named.
    pub fn new() -> Self {
        Self::on(Box::new(Platform::windowless()))
    }

    /// The starter on `host`.
    pub(super) fn on(host: Box<dyn FileHost>) -> Self {
        Self {
            host,
            fetched: Awaited::default(),
        }
    }

    /// The disk of the site whose start is on its way, if one is: the
    /// Start panel waits for it.
    pub fn starting(&self) -> Option<&'static SiteFile> {
        self.fetched.purposes().next().map(|start| start.disk)
    }

    /// Asks the site for the disk `start` names, to start the machine with
    /// once it has come. A start asked for while another is on its way is
    /// ignored: that one is the start the user gets.
    pub fn execute(&mut self, start: StartCommand) {
        if self.starting().is_some() {
            return;
        }
        let answer = self.host.fetch(start.disk.path);
        self.fetched.push(start, answer);
    }

    /// Takes up the site's answer if it has come, and starts the machine
    /// with the disk it brought: into the drive the start chose, the
    /// bootstrap trying that drive first, and the machine restarted. Says
    /// in `messages` what the machine restarted with, or why it runs on as
    /// it was. Returns the start carried out, if there was one.
    pub fn poll(&mut self, runtime: &mut Runtime, messages: &mut Messages) -> Option<StartCommand> {
        let mut started = None;
        for (start, answer) in self.fetched.answered() {
            let StartCommand { disk, place, .. } = start;
            match start_with(runtime, start, fetched(disk.path, answer)) {
                Ok(()) => {
                    messages.done(format!("Restarted with {}, {}", disk.title, place.words));
                    started = Some(start);
                }
                Err(why) => messages.failed(format!("{why}. The machine runs on as it was")),
            }
        }
        started
    }
}

/// Starts `runtime` with the disk the site brought for `start`, if it
/// brought one the Insert takes: it goes into the drive `start` chose, in
/// place of the disk that may be there, the bootstrap is told to try that
/// drive first, and the machine restarts. Otherwise nothing changes, and
/// the error says why.
fn start_with(runtime: &mut Runtime, start: StartCommand, fetched: Fetched) -> Result<(), String> {
    let bytes = fetched?;
    let insert = Operation::Insert {
        drive: start.place.drive,
        disk: Disk::Image(HostFile::Contents {
            name: file_name(start.disk.path).to_string(),
            bytes,
        }),
        read_only: false,
    };
    {
        let mut disks = runtime.disks.borrow_mut();
        // What the Insert did is said in the words of the Start panel: its
        // own are the Disks panel's.
        disk_ops::apply(&mut disks, insert, &HostClock).result?;
        disks.set_boot_order(start.place.boot_order.to_vec());
    }
    runtime.reset();
    Ok(())
}

/// The name of the file at `path`, relative to the page: what the disk made
/// of it is known by, and what it is called when it is handed back.
fn file_name(path: &str) -> &str {
    path.rsplit_once('/').map_or(path, |(_, name)| name)
}

#[cfg(test)]
#[path = "starter_tests.rs"]
mod tests;
