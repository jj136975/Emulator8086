//! Why the tool stopped, and what it prints of it: an error annotation for
//! GitHub Actions (`::error title=<stage>::<what>`), which is a line of
//! stdout, then what the guest showed when the stage gave up on it, the
//! failing module's part of BUILD.LOG, and where the build disk was saved.
//!
//! The invariant: a failure names its stage and says what went wrong in
//! one line; all else it prints follows that line.

use crate::guest::Stopped;
use crate::log_scan;
use crate::stages::{self, FAILED_BUILD_DISK};
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};

/// The stage that failed, with its error.
#[derive(Debug)]
pub(crate) enum Failure {
    /// The output directory could not be written.
    Output(PathBuf, io::Error),
    /// A file of the inputs that goes onto the disks could not be read,
    /// before the first stage (`stages::InputFiles`).
    Input(PathBuf, io::Error),
    Host(stages::host::Error),
    BuildDisk(stages::build_disk::Error),
    Build(Box<stages::build::Failed>),
    /// What the build made failed its checks: the build disk is kept.
    Collect {
        error: stages::collect::Error,
        disk: Vec<u8>,
    },
    Floppy(stages::floppy::Error),
    HardDisk(stages::hard_disk::Error),
}

/// The text of an annotation of GitHub Actions: the command, the title of
/// its box and the message, on one line. A title or a message may hold no
/// character the command line reads as its own; those are escaped as the
/// runner unescapes them.
pub(crate) fn annotation(title: &str, message: &str) -> String {
    let data = |text: &str| {
        text.replace('%', "%25")
            .replace('\r', "%0D")
            .replace('\n', "%0A")
    };
    let property = |text: &str| data(text).replace(':', "%3A").replace(',', "%2C");
    format!("::error title={}::{}", property(title), data(message))
}

impl Failure {
    /// The stage, as the annotation's title.
    fn stage(&self) -> &'static str {
        match self {
            Self::Output(..) => "Output",
            Self::Input(..) => "Inputs",
            Self::Host(_) => "Stage 1: the FreeDOS host floppy",
            Self::BuildDisk(_) => "Stage 2: the build disk",
            Self::Build(_) => "Stage 3: the build",
            Self::Collect { .. } => "Stage 4: what the build made",
            Self::Floppy(_) => "Stage 5: the MS-DOS 4.00 floppy",
            Self::HardDisk(_) => "Stage 6: the hard disk with Doom8088",
        }
    }

    /// What the guest showed when the stage gave up on it, if a guest ran.
    fn guest(&self) -> Option<&Stopped> {
        match self {
            Self::Build(failed) => Some(&failed.guest),
            Self::Floppy(error) => error.guest(),
            Self::HardDisk(error) => error.guest(),
            _ => None,
        }
    }

    /// What CHKDSK reported of a disk it did not find clean.
    fn chkdsk_report(&self) -> Option<&str> {
        match self {
            Self::Floppy(stages::floppy::Error::Check(error))
            | Self::HardDisk(stages::hard_disk::Error::Check(error)) => error.report(),
            _ => None,
        }
    }

    /// The build disk as the failed stage left it, and the module that was
    /// built on it, if the stage is the build's or the collection's.
    fn build_disk(&self) -> Option<(&[u8], Option<&str>)> {
        match self {
            Self::Build(failed) => Some((&failed.disk, failed.module.as_deref())),
            Self::Collect { error, disk } => Some((disk, error.module())),
            _ => None,
        }
    }

    /// Prints the failure on stdout, and saves the build disk of a failed
    /// build into `out`.
    pub(crate) fn print(&self, out: &Path) {
        println!("{}", annotation(self.stage(), &self.to_string()));
        if let Some(guest) = self.guest() {
            print!("{guest}");
        }
        if let Some(report) = self.chkdsk_report() {
            println!("--- what CHKDSK reported\n{}", report.trim_end());
        }
        if let Some((disk, module)) = self.build_disk() {
            if let Some(module) = module
                && let Some(log) = stages::build_log(disk)
            {
                match log_scan::section(&log, module) {
                    Some(part) => println!("--- BUILD.LOG, {module}\n{part}"),
                    None => println!("--- BUILD.LOG has no part of {module}"),
                }
            }
            let path = out.join(FAILED_BUILD_DISK);
            match std::fs::write(&path, disk) {
                Ok(()) => println!("The build disk is saved as {}.", path.display()),
                Err(error) => println!(
                    "The build disk cannot be saved as {}: {error}",
                    path.display()
                ),
            }
        }
    }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Output(path, error) => write!(f, "{} cannot be written: {error}", path.display()),
            Self::Input(path, error) => write!(f, "{} cannot be read: {error}", path.display()),
            Self::Host(error) => write!(f, "{error}"),
            Self::BuildDisk(error) => write!(f, "{error}"),
            Self::Build(failed) => write!(f, "{}", failed.error),
            Self::Collect { error, .. } => write!(f, "{error}"),
            Self::Floppy(error) => write!(f, "{error}"),
            Self::HardDisk(error) => write!(f, "{error}"),
        }
    }
}

#[cfg(test)]
#[path = "failure_tests.rs"]
mod tests;
