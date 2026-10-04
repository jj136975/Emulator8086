//! Where a tool says what it did or why it failed: short messages the shell
//! shows for a few seconds in a corner of the picture, the newest at the
//! bottom. Whoever carries out an action reports here (`done`, `failed`), as
//! the app does for the commands of the Machine menu.
//!
//! The invariant: a message's time starts when it is first shown, not when
//! it is reported. Time is egui's input time, which only a pass tells, so a
//! message reported between two passes, or while the overlay is hidden, is
//! still read for its whole time once it is on the screen.

use egui::{Align, Align2, Area, Frame, Id, Layout, Order, Pos2};

/// How long a message that reports something done stays, in seconds.
pub(super) const DONE_SECONDS: f64 = 4.0;
/// How long a failure stays: it says why, which takes longer to read.
pub(super) const FAILED_SECONDS: f64 = 8.0;
/// Most messages kept at a time. A tool that reports in a loop pushes the
/// oldest out instead of filling the picture.
const MOST_SHOWN: usize = 5;

struct Message {
    text: String,
    failed: bool,
    /// Input time at which the message goes away; unset until a pass has
    /// shown it.
    until: Option<f64>,
}

/// The messages that are on the screen or about to be.
#[derive(Default)]
pub struct Messages {
    shown: Vec<Message>,
}

impl Messages {
    /// Reports what was done.
    pub fn done(&mut self, text: impl Into<String>) {
        let text = text.into();
        log::info!("{text}");
        self.push(text, false);
    }

    /// Reports what could not be done, and why. The message is logged as
    /// well: a failure is worth reading after it has left the screen.
    pub fn failed(&mut self, text: impl Into<String>) {
        let text = text.into();
        log::error!("{text}");
        self.push(text, true);
    }

    fn push(&mut self, text: String, failed: bool) {
        if self.shown.len() == MOST_SHOWN {
            self.shown.remove(0);
        }
        self.shown.push(Message {
            text,
            failed,
            until: None,
        });
    }

    /// Draws the messages with their top right corner at `corner`, at input
    /// time `now`, and drops those whose time is over.
    pub(super) fn show(&mut self, ctx: &egui::Context, corner: Pos2, now: f64) {
        self.shown
            .retain(|message| message.until.is_none_or(|until| now < until));
        if self.shown.is_empty() {
            return;
        }
        Area::new(Id::new("overlay_messages"))
            .order(Order::Foreground)
            .pivot(Align2::RIGHT_TOP)
            .fixed_pos(corner)
            // A message is read, not used: clicks go to what lies under it.
            .interactable(false)
            .layout(Layout::top_down(Align::Max))
            .show(ctx, |ui| {
                for message in &mut self.shown {
                    let seconds = if message.failed {
                        FAILED_SECONDS
                    } else {
                        DONE_SECONDS
                    };
                    message.until.get_or_insert(now + seconds);
                    let colour = if message.failed {
                        ui.visuals().error_fg_color
                    } else {
                        ui.visuals().strong_text_color()
                    };
                    Frame::popup(ui.style()).show(ui, |ui| {
                        ui.colored_label(colour, &message.text);
                    });
                }
            });
    }
}
