//! The host as the session's tools reach it (`FileHost`): its file dialogs,
//! what it lets out, and the files of the site the app came from.
//! `Platform` is the host the app runs on; a test puts hand-made answers in
//! its place (`testing::ByHand`). A question to the host is answered later,
//! down a channel: `Awaited` holds the questions on their way, each with
//! what it was asked for, and hands over the answers that have come;
//! `fetched` says what such an answer of the site brought.
//!
//! The invariant: nothing here waits. A question returns at once, and its
//! answer is taken up by the first look after it came
//! (`Awaited::answered`), in whatever order the answers come; one that is
//! never answered costs nothing.

use std::path::Path;
use std::sync::Arc;
use std::sync::mpsc::{Receiver, TryRecvError};

use winit::window::Window;

use emu8086_core::io::disk_ops::HostFile;

use crate::platform::{self, Fetched, FileDialogs, FileQuestion};

/// The host, as the session's tools reach it.
pub(super) trait FileHost {
    /// Asks the user `question` in a dialog of the host's, and returns at
    /// once: the files chosen come down the channel, none if the user chose
    /// nothing. A channel that closes with nothing in it says that no
    /// dialog could be shown.
    fn ask(&mut self, question: FileQuestion) -> Receiver<Vec<HostFile>>;

    /// Hands the user `bytes` as a file named as `file` is, where the host
    /// lets a file out (`platform::save_bytes`), and says what was done
    /// with them, or why it could not be done.
    fn save_bytes(&mut self, file: &Path, bytes: &[u8]) -> Result<String, String>;

    /// Asks the site the app came from for the file at `path`, relative to
    /// the page (`platform::fetch`), and returns at once: the file's bytes,
    /// or why none came, come down the channel. A channel that closes with
    /// nothing in it says that the site could not be asked.
    fn fetch(&mut self, path: &str) -> Receiver<Fetched>;
}

/// The host the app runs on. Its dialogs belong to the window the machine
/// is shown in, once there is one.
pub(super) struct Platform(FileDialogs);

impl Platform {
    /// The host before there is a window: its dialogs belong to none.
    pub fn windowless() -> Self {
        Self(FileDialogs::windowless())
    }

    /// The host whose dialogs belong to `window`.
    pub fn of(window: &Arc<Window>) -> Self {
        Self(FileDialogs::new(window))
    }
}

impl FileHost for Platform {
    fn ask(&mut self, question: FileQuestion) -> Receiver<Vec<HostFile>> {
        self.0.ask(question)
    }

    fn save_bytes(&mut self, file: &Path, bytes: &[u8]) -> Result<String, String> {
        platform::save_bytes(file, bytes)
    }

    fn fetch(&mut self, path: &str) -> Receiver<Fetched> {
        platform::fetch(path)
    }
}

/// What the site's answer to the fetch of the file at `path` brought, as
/// `Awaited::answered` hands it over: the file's bytes, or why none came,
/// in words for the user that name the file. The host's own words say why
/// a file did not come; a question whose channel closed without an answer
/// could not be asked.
pub(super) fn fetched(path: &str, answer: Option<Fetched>) -> Fetched {
    answer.unwrap_or_else(|| Err(format!("The site could not be asked for {path}")))
}

/// The questions asked of the host whose answers have not been taken up,
/// each with what it was asked for, a `P`. Each answer is an `A`.
pub(super) struct Awaited<P, A> {
    asked: Vec<(P, Receiver<A>)>,
}

impl<P, A> Default for Awaited<P, A> {
    fn default() -> Self {
        Self { asked: Vec::new() }
    }
}

impl<P, A> Awaited<P, A> {
    /// Waits for `answer`, which is for `purpose`.
    pub fn push(&mut self, purpose: P, answer: Receiver<A>) {
        self.asked.push((purpose, answer));
    }

    /// The answers that have come since the last look, in the order their
    /// questions were asked, each with what it was asked for: `None` for a
    /// question whose channel closed without an answer, which the host
    /// could not ask. Those are done with; the others are still waited for.
    pub fn answered(&mut self) -> Vec<(P, Option<A>)> {
        let mut answered = Vec::new();
        let mut waiting = Vec::with_capacity(self.asked.len());
        for (purpose, answer) in self.asked.drain(..) {
            match answer.try_recv() {
                Ok(answer) => answered.push((purpose, Some(answer))),
                Err(TryRecvError::Empty) => waiting.push((purpose, answer)),
                Err(TryRecvError::Disconnected) => answered.push((purpose, None)),
            }
        }
        self.asked = waiting;
        answered
    }

    /// How many questions are still waited for: what a test reads of what
    /// a tool waits for.
    #[cfg(test)]
    pub fn waiting(&self) -> usize {
        self.asked.len()
    }
}

#[cfg(test)]
pub(super) mod testing;
