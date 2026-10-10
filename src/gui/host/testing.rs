//! A host whose dialogs the tests answer by hand, when they like and in the
//! order they like, for the tests of the tools that reach the host and of
//! the session around them. It keeps what it is asked on a desk the test
//! reads.
//!
//! The invariant: the host answers nothing by itself. Every answer is the
//! test's, sent down the end of the channel the host hands it.

use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::mpsc::{Receiver, Sender, channel};

use emu8086_core::io::disk_ops::HostFile;

use super::FileHost;
use crate::platform::FileQuestion;

/// What a host was asked: the dialogs it was to show, each with the end of
/// the channel its answer goes down, and the files it was handed to let
/// out.
#[derive(Default)]
struct Desk {
    questions: Vec<(FileQuestion, Sender<Vec<HostFile>>)>,
    handed_out: Vec<(PathBuf, Vec<u8>)>,
}

/// A host whose dialogs the test answers, through the ends their answers
/// go down. Its clones share one desk.
#[derive(Clone, Default)]
pub struct ByHand(Rc<RefCell<Desk>>);

impl ByHand {
    /// The dialogs asked for since the last look.
    pub fn questions(&self) -> Vec<(FileQuestion, Sender<Vec<HostFile>>)> {
        std::mem::take(&mut self.0.borrow_mut().questions)
    }

    /// The one dialog asked for since the last look.
    pub fn question(&self) -> (FileQuestion, Sender<Vec<HostFile>>) {
        let mut questions = self.questions();
        assert_eq!(questions.len(), 1, "one dialog");
        questions.remove(0)
    }

    /// The files handed out since the last look, by name.
    pub fn handed_out(&self) -> Vec<(PathBuf, Vec<u8>)> {
        std::mem::take(&mut self.0.borrow_mut().handed_out)
    }
}

impl FileHost for ByHand {
    fn ask(&mut self, question: FileQuestion) -> Receiver<Vec<HostFile>> {
        let (answer, receiver) = channel();
        self.0.borrow_mut().questions.push((question, answer));
        receiver
    }

    fn save_bytes(&mut self, file: &Path, bytes: &[u8]) -> Result<String, String> {
        let mut desk = self.0.borrow_mut();
        desk.handed_out.push((file.to_path_buf(), bytes.to_vec()));
        Ok(format!("offered as the download {}", file.display()))
    }
}
