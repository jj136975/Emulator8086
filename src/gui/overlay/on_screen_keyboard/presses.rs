//! Who holds which key of the on-screen keyboard: the fingers and the
//! pointer that press the keys, the modifiers latched, and the repeat of a
//! key held down. It knows make codes and egui's input time, and answers
//! with the app's commands (`Command::Key`), which say what the guest is to
//! be sent.
//!
//! Shift, Ctrl and Alt latch when tapped alone: down and up again with no
//! other key down meanwhile, and before the key began to repeat, which
//! makes it a key held rather than tapped. A latched modifier stays down in
//! the guest until the next key that is not a modifier comes up, or until
//! it is tapped again. A modifier held while another key is down, whichever
//! went down first, is held, and comes up when it is let go.
//!
//! The invariant: a key goes down in the guest once, when it is pressed and
//! was neither down nor latched, and comes up once, when nothing holds it
//! anymore and it does not latch. Between the two its only commands are
//! the presses of its repeats.

use egui::{TouchDeviceId, TouchId};

use super::super::Command;
use super::layout::is_modifier;

/// How long a key is held before it repeats, and how long from one repeat
/// to the next, in seconds: an XT's typematic, about half a second, then
/// 10.9 presses a second.
pub(super) const REPEAT_DELAY: f64 = 0.5;
pub(super) const REPEAT_PERIOD: f64 = 1.0 / 10.9;

/// What holds a key down.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Holder {
    /// A finger, by the device it is on and its own id there.
    Finger(TouchDeviceId, TouchId),
    /// The primary button of the pointer.
    Pointer,
}

/// A key held down.
#[derive(Clone, Debug)]
struct Down {
    make: u8,
    /// Who holds it: one or more.
    holders: Vec<Holder>,
    /// Input time of its next repeat.
    repeat_at: f64,
    /// A modifier that latches if it comes up now: it went down unlatched
    /// while no other key was down, and since then no other key went down
    /// and it did not repeat.
    tapped: bool,
}

/// The keys held down and the modifiers latched.
#[derive(Clone, Debug, Default)]
pub(super) struct Presses {
    down: Vec<Down>,
    latched: Vec<u8>,
}

impl Presses {
    /// Whether something holds the key `make` down.
    pub(super) fn is_held(&self, make: u8) -> bool {
        self.down.iter().any(|down| down.make == make)
    }

    /// Whether the modifier `make` is latched.
    pub(super) fn is_latched(&self, make: u8) -> bool {
        self.latched.contains(&make)
    }

    /// `holder` presses the key `make` at input time `now`. A key that is
    /// down already gains a holder and sends nothing; a latched modifier
    /// pressed again is held and comes up when it is let go. A holder
    /// presses one key at a time: one that is pressing another lets go of
    /// it first, its release never told.
    pub(super) fn press(
        &mut self,
        holder: Holder,
        make: u8,
        now: f64,
        commands: &mut Vec<Command>,
    ) {
        self.release(holder, commands);
        if let Some(down) = self.down.iter_mut().find(|down| down.make == make) {
            down.holders.push(holder);
            return;
        }
        // A key goes down: no modifier that is down is tapped alone anymore,
        // and this one is tapped alone only if no other key is down.
        let alone = self.down.is_empty();
        for down in &mut self.down {
            down.tapped = false;
        }
        let was_latched = self.latched.contains(&make);
        self.latched.retain(|latched| *latched != make);
        if !was_latched {
            commands.push(Command::Key {
                make,
                pressed: true,
            });
        }
        self.down.push(Down {
            make,
            holders: vec![holder],
            repeat_at: now + REPEAT_DELAY,
            tapped: is_modifier(make) && !was_latched && alone,
        });
    }

    /// `holder` lets go of the key it holds, if it holds one. The key comes
    /// up once nothing holds it, unless it is a modifier tapped alone,
    /// which latches; a key that is not a modifier takes the latched
    /// modifiers up with it.
    pub(super) fn release(&mut self, holder: Holder, commands: &mut Vec<Command>) {
        let Some(index) = self
            .down
            .iter()
            .position(|down| down.holders.contains(&holder))
        else {
            return;
        };
        let down = &mut self.down[index];
        down.holders.retain(|held| *held != holder);
        if !down.holders.is_empty() {
            return;
        }
        let down = self.down.remove(index);
        if down.tapped {
            self.latched.push(down.make);
            return;
        }
        commands.push(Command::Key {
            make: down.make,
            pressed: false,
        });
        if !is_modifier(down.make) {
            for make in self.latched.drain(..) {
                commands.push(Command::Key {
                    make,
                    pressed: false,
                });
            }
        }
    }

    /// Presses again, at input time `now`, each key held down past its
    /// time: `REPEAT_DELAY` after it went down, then every `REPEAT_PERIOD`.
    /// After a stall the repeats that were missed are not made up for. A
    /// latched modifier is held by nothing and does not repeat; a modifier
    /// that repeats is held, and does not latch when it comes up.
    pub(super) fn repeat(&mut self, now: f64, commands: &mut Vec<Command>) {
        for down in &mut self.down {
            if now < down.repeat_at {
                continue;
            }
            commands.push(Command::Key {
                make: down.make,
                pressed: true,
            });
            down.tapped = false;
            down.repeat_at += REPEAT_PERIOD;
            if down.repeat_at <= now {
                down.repeat_at = now + REPEAT_PERIOD;
            }
        }
    }

    /// Lets go of every key: those held come up, then the latched
    /// modifiers.
    pub(super) fn let_go(&mut self, commands: &mut Vec<Command>) {
        for make in self.down.drain(..).map(|down| down.make) {
            commands.push(Command::Key {
                make,
                pressed: false,
            });
        }
        for make in self.latched.drain(..) {
            commands.push(Command::Key {
                make,
                pressed: false,
            });
        }
    }
}
