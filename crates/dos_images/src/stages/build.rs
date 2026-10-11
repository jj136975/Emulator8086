//! Stage 3: the build. FreeDOS boots from the host floppy and runs
//! BUILD.BAT on the build disk, which tells the host on the port E9h when
//! the build starts, when each module starts or fails, when what was built
//! is copied and when it is done (`markers`). `Progress` keeps the account
//! of it: a line is printed as each module ends, its instructions and the
//! host's seconds for it.
//!
//! The invariant: the markers come in the order BUILD.BAT has them, the
//! modules in the order of the build disk's list; any other line of the
//! port is the guest's own. The build gives up at the first marker out of
//! turn, the first module NMAKE fails, the first line of the port lost, and
//! the first budget of guest time spent: the start's, a module's or the
//! build's.

use super::build_disk::BuildDisk;
use crate::guest::{Guest, Stopped};
use crate::markers::Marker;
use crate::report::grouped;
use emu8086_core::io::disk::DiskImage;
use std::fmt;
use std::time::{Duration, Instant};

/// FreeDOS starts BUILD.BAT after 3.2 million instructions, a third of a
/// second of guest time.
const START_BUDGET: Duration = Duration::from_secs(10);
/// SELECT, the module that takes longest, takes 1.56 billion instructions:
/// 156 s of guest time.
const MODULE_BUDGET: Duration = Duration::from_secs(400);
/// The build takes 11.4 billion instructions: 1,137 s of guest time.
const BUILD_BUDGET: Duration = Duration::from_secs(3_000);

/// What the report calls the copy of what was built (CPY.BAT), which it
/// lists after the modules.
pub(crate) const COPYING: &str = "(CPY.BAT)";

/// A module the guest built: its instructions, and the host's seconds.
#[derive(Debug, PartialEq)]
pub(crate) struct ModuleRun {
    pub name: String,
    pub instructions: u64,
    pub seconds: f64,
}

/// The build as it went.
#[derive(Debug)]
pub(crate) struct Build {
    /// The instructions FreeDOS took to start BUILD.BAT.
    pub host_up: u64,
    /// The modules in the order they were built, then the copy of what
    /// they made (`COPYING`).
    pub modules: Vec<ModuleRun>,
    /// The instructions from the start of BUILD.BAT to its end, and the
    /// host's seconds for them.
    pub instructions: u64,
    pub seconds: f64,
}

/// Why the build failed.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Error {
    /// FreeDOS did not start BUILD.BAT in its time.
    NoStart,
    /// A module, or the copy, was not done in its time.
    ModuleTooLong(String),
    /// The build was not done in its time.
    TooLong,
    /// NMAKE ended with an error in the module.
    ModuleFailed(String),
    /// The guest told of a step that BUILD.BAT does not take then.
    OutOfTurn(String),
    /// The port lost lines in a batch, a marker perhaps among them.
    LinesLost(u64),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoStart => write!(
                f,
                "FreeDOS did not start BUILD.BAT in {} s of guest time",
                START_BUDGET.as_secs()
            ),
            Self::ModuleTooLong(module) => write!(
                f,
                "{module} was not done in {} s of guest time",
                MODULE_BUDGET.as_secs()
            ),
            Self::TooLong => write!(
                f,
                "the build was not done in {} s of guest time",
                BUILD_BUDGET.as_secs()
            ),
            Self::ModuleFailed(module) => write!(f, "NMAKE failed in {module}"),
            Self::OutOfTurn(line) => write!(f, "the guest told of a step out of turn: {line}"),
            Self::LinesLost(lost) => write!(
                f,
                "port E9h lost {lost} lines in a batch: a marker may be among them"
            ),
        }
    }
}

/// The build that failed: why, the module that was being built, what the
/// guest showed, and the build disk as it was then.
#[derive(Debug)]
pub(crate) struct Failed {
    pub error: Error,
    pub module: Option<String>,
    pub guest: Box<Stopped>,
    pub disk: Vec<u8>,
}

/// When something began: the guest's instructions and time, and the
/// host's instant.
#[derive(Clone, Copy, Debug)]
struct Mark {
    instructions: u64,
    time: Duration,
    instant: Instant,
}

impl Mark {
    fn of(guest: &Guest) -> Self {
        Self {
            instructions: guest.instructions(),
            time: guest.time(),
            instant: Instant::now(),
        }
    }
}

/// Where BUILD.BAT is.
#[derive(Debug)]
enum Turn {
    /// FreeDOS boots.
    Booting,
    /// BUILD.BAT builds the modules: `next` is the index of the module it
    /// starts next, and the module it builds now, if one, began at a mark.
    Building {
        next: usize,
        running: Option<(String, Mark)>,
    },
    /// CPY.BAT copies what was built, since a mark.
    Copying(Mark),
}

/// The account of a build: where BUILD.BAT is, by the markers the guest
/// told of, and what it did so far.
struct Progress<'a> {
    /// The modules BUILD.BAT builds, in its order.
    modules: &'a [String],
    turn: Turn,
    /// When BUILD.BAT started.
    start: Mark,
    runs: Vec<ModuleRun>,
}

impl<'a> Progress<'a> {
    /// The account of a build of `modules` by a guest powered on at
    /// `power_on`.
    fn new(modules: &'a [String], power_on: Mark) -> Self {
        Self {
            modules,
            turn: Turn::Booting,
            start: power_on,
            runs: Vec::new(),
        }
    }

    /// The module being built, if one is.
    fn module(&self) -> Option<&str> {
        match &self.turn {
            Turn::Building {
                running: Some((module, _)),
                ..
            } => Some(module),
            _ => None,
        }
    }

    /// What is being done, a module or the copy, and the mark it began at.
    fn current(&self) -> Option<(&str, Mark)> {
        match &self.turn {
            Turn::Building {
                running: Some((module, since)),
                ..
            } => Some((module, *since)),
            Turn::Copying(since) => Some((COPYING, *since)),
            _ => None,
        }
    }

    /// Ends what is being done at `now`, and prints its line.
    fn end_current(&mut self, now: Mark) {
        let Some((name, since)) = self.current() else {
            return;
        };
        let run = ModuleRun {
            name: name.to_string(),
            instructions: now.instructions - since.instructions,
            seconds: now.instant.duration_since(since.instant).as_secs_f64(),
        };
        println!(
            "  {:<16} {:>15} instructions {:>7.2} s",
            run.name,
            grouped(run.instructions),
            run.seconds
        );
        self.runs.push(run);
    }

    /// Takes the marker of the port's `line`, which the guest told of at
    /// `now`. Returns the build when it is done.
    fn told(&mut self, marker: Marker, line: &str, now: Mark) -> Result<Option<Build>, Error> {
        let turn = match (&self.turn, marker) {
            (Turn::Booting, Marker::BuildStart) => {
                println!(
                    "  FreeDOS started BUILD.BAT after {} instructions",
                    grouped(now.instructions)
                );
                self.start = now;
                Turn::Building {
                    next: 0,
                    running: None,
                }
            }
            (Turn::Building { next, .. }, Marker::Module(module))
                if self.modules.get(*next) == Some(&module) =>
            {
                let next = *next + 1;
                self.end_current(now);
                Turn::Building {
                    next,
                    running: Some((module, now)),
                }
            }
            (
                Turn::Building {
                    running: Some((running, _)),
                    ..
                },
                Marker::ModuleFailed(module),
            ) if *running == module => return Err(Error::ModuleFailed(module)),
            (Turn::Building { next, .. }, Marker::Copying) if *next == self.modules.len() => {
                self.end_current(now);
                Turn::Copying(now)
            }
            (Turn::Copying(_), Marker::BuildDone) => {
                self.end_current(now);
                return Ok(Some(Build {
                    host_up: self.start.instructions,
                    modules: std::mem::take(&mut self.runs),
                    instructions: now.instructions - self.start.instructions,
                    seconds: now.instant.duration_since(self.start.instant).as_secs_f64(),
                }));
            }
            _ => return Err(Error::OutOfTurn(line.trim().to_string())),
        };
        self.turn = turn;
        Ok(None)
    }

    /// The budget spent at the guest time `now`, if one is.
    fn spent(&self, now: Duration) -> Option<Error> {
        match self.current() {
            _ if matches!(self.turn, Turn::Booting) => {
                (now >= START_BUDGET).then_some(Error::NoStart)
            }
            _ if now - self.start.time >= BUILD_BUDGET => Some(Error::TooLong),
            Some((name, since)) if now - since.time >= MODULE_BUDGET => {
                Some(Error::ModuleTooLong(name.to_string()))
            }
            _ => None,
        }
    }
}

/// Builds MS-DOS 4.00 on the build disk, FreeDOS booted from `host`.
/// Returns how the build went, and the build disk as the guest left it.
pub(crate) fn run(host: DiskImage, build_disk: BuildDisk) -> Result<(Build, Vec<u8>), Box<Failed>> {
    let modules = build_disk.modules;
    let disk = DiskImage::hard_disk_from_bytes(build_disk.disk.into_bytes(), "build.img", false);
    let mut guest = Guest::power_on(vec![(0x00, host), (0x80, disk)]);
    let mut progress = Progress::new(&modules, Mark::of(&guest));
    let fail = |guest: &Guest, error: Error, progress: &Progress<'_>| {
        Box::new(Failed {
            error,
            module: progress.module().map(str::to_string),
            guest: guest.stopped(),
            disk: guest.disk(0x80).unwrap_or_default(),
        })
    };
    loop {
        guest.step();
        let lines = match guest.new_lines() {
            Ok(lines) => lines,
            Err(lost) => return Err(fail(&guest, Error::LinesLost(lost), &progress)),
        };
        for line in lines {
            let Some(marker) = Marker::parse(&line) else {
                continue;
            };
            match progress.told(marker, &line, Mark::of(&guest)) {
                Ok(None) => {}
                Ok(Some(build)) => return Ok((build, guest.disk(0x80).unwrap_or_default())),
                Err(error) => return Err(fail(&guest, error, &progress)),
            }
        }
        if let Some(error) = progress.spent(guest.time()) {
            return Err(fail(&guest, error, &progress));
        }
    }
}

#[cfg(test)]
#[path = "build_tests.rs"]
mod tests;
