//! The overlay's switch on a touch screen: a finger held still on the
//! picture. A phone has no secondary button and no key the window could
//! keep, so a finger that comes down where the overlay shows nothing and
//! stays there, still, for `HOLD` shows the overlay or hides it, as soon as
//! its time is up and not when it lifts. It does so on every host, on a
//! desktop's touch screen too. `Hold` is a state machine that is told of
//! the fingers and of the time, and says when a hold is due: it reads no
//! clock.
//!
//! The invariants: one hold works the switch once. Every finger that is
//! down when it does is spent, and none of them works it again before it
//! lifts. And what lies under a finger is decided when its time is up, at
//! the position it came down at, from what the overlay's last pass laid
//! out: a finger on the overlay is the overlay's, however long it stays
//! there. (Whether egui kept the finger's event when it came down says
//! nothing: egui answers for where its pointer last was, which after the
//! last finger lifted is nowhere.)

use std::time::Duration;

use egui::Pos2;
use winit::event::TouchPhase;

use crate::platform::Instant;

/// How long a finger stays still on the picture to work the switch.
pub const HOLD: Duration = Duration::from_millis(500);

/// How far a finger may move from where it came down and still be still,
/// in points: no finger is quite still.
pub const STILL: f32 = 10.0;

/// A finger down on the screen.
struct Finger {
    /// The host's id of the finger, which is its own until it lifts.
    id: u64,
    /// Where it came down, in points.
    origin: Pos2,
    /// When it came down.
    since: Instant,
    /// Whether it may still work the switch: it has stayed still, its time
    /// has not come, and no hold was worked while it was down.
    live: bool,
}

/// The fingers on the screen, and which of them may still work the switch.
#[derive(Default)]
pub struct Hold {
    fingers: Vec<Finger>,
}

impl Hold {
    /// Finger `id` came down, moved, lifted or was taken away by the host,
    /// at `at`, in points, at `now`.
    pub fn touch(&mut self, id: u64, phase: TouchPhase, at: Pos2, now: Instant) {
        let known = self.fingers.iter().position(|finger| finger.id == id);
        match phase {
            TouchPhase::Started => {
                // A finger whose lift the host never told of is gone.
                if let Some(known) = known {
                    self.fingers.remove(known);
                }
                self.fingers.push(Finger {
                    id,
                    origin: at,
                    since: now,
                    live: true,
                });
            }
            TouchPhase::Moved => {
                if let Some(known) = known
                    && self.fingers[known].origin.distance(at) > STILL
                {
                    self.fingers[known].live = false;
                }
            }
            TouchPhase::Ended | TouchPhase::Cancelled => {
                if let Some(known) = known {
                    self.fingers.remove(known);
                }
            }
        }
    }

    /// Whether a hold is due at `now`: a finger that may still work the
    /// switch has been down, still, for `HOLD`, where the overlay shows
    /// nothing. `covers` says whether the overlay shows something at a
    /// position; it is asked once per finger, when its time is up. A finger
    /// whose time comes on the overlay is the overlay's from then on, and a
    /// hold that is due spends every finger that is down.
    pub fn due(&mut self, now: Instant, covers: impl Fn(Pos2) -> bool) -> bool {
        let mut due = false;
        for finger in self.fingers.iter_mut().filter(|finger| finger.live) {
            if now.saturating_duration_since(finger.since) >= HOLD {
                finger.live = false;
                due |= !covers(finger.origin);
            }
        }
        if due {
            for finger in &mut self.fingers {
                finger.live = false;
            }
        }
        due
    }
}

#[cfg(test)]
#[path = "hold_tests.rs"]
mod tests;
