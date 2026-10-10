//! The hold told of fingers and of the time by hand: when a finger held on
//! the screen works the switch, and when it does not. The time is the one
//! each test states, from an instant it takes once.

use super::*;

/// The overlay as `covers` sees it: showing nothing anywhere, and showing
/// something everywhere.
fn nothing(_: Pos2) -> bool {
    false
}

fn everything(_: Pos2) -> bool {
    true
}

const MS: Duration = Duration::from_millis(1);

/// A finger held still is due as soon as `HOLD` is up, not a millisecond
/// before, whether it has not moved at all or less than `STILL`; and once:
/// it does not work the switch again for as long as it stays down.
#[test]
fn a_finger_held_still_is_due_once_when_its_time_is_up() {
    let start = Instant::now();
    let mut hold = Hold::default();
    let origin = egui::pos2(100.0, 200.0);
    hold.touch(1, TouchPhase::Started, origin, start);
    assert!(!hold.due(start + HOLD - MS, nothing));
    hold.touch(
        1,
        TouchPhase::Moved,
        origin + egui::vec2(STILL - 1.0, 0.0),
        start + HOLD - MS,
    );
    assert!(hold.due(start + HOLD, nothing));
    assert!(!hold.due(start + 10 * HOLD, nothing), "once");

    // The finger that comes down after it has a time of its own.
    hold.touch(1, TouchPhase::Ended, origin, start + 10 * HOLD);
    let again = start + 20 * HOLD;
    hold.touch(1, TouchPhase::Started, origin, again);
    assert!(!hold.due(again + HOLD - MS, nothing));
    assert!(hold.due(again + HOLD, nothing));
}

/// A finger that moved further than `STILL` from where it came down is no
/// hold, even after it came back.
#[test]
fn a_finger_that_moved_is_no_hold() {
    let start = Instant::now();
    let mut hold = Hold::default();
    let origin = egui::pos2(50.0, 50.0);
    hold.touch(1, TouchPhase::Started, origin, start);
    hold.touch(
        1,
        TouchPhase::Moved,
        origin + egui::vec2(0.0, STILL + 1.0),
        start + 100 * MS,
    );
    hold.touch(1, TouchPhase::Moved, origin, start + 200 * MS);
    assert!(!hold.due(start + 2 * HOLD, nothing));
}

/// A finger that lifted, or that the host took away, before its time is
/// no hold.
#[test]
fn a_finger_that_lifted_is_no_hold() {
    let start = Instant::now();
    let mut hold = Hold::default();
    let origin = egui::pos2(10.0, 10.0);
    hold.touch(1, TouchPhase::Started, origin, start);
    hold.touch(1, TouchPhase::Ended, origin, start + HOLD - MS);
    hold.touch(2, TouchPhase::Started, origin, start);
    hold.touch(2, TouchPhase::Cancelled, origin, start + HOLD - MS);
    assert!(!hold.due(start + HOLD, nothing));
}

/// A finger whose time comes where the overlay shows something is the
/// overlay's: it is no hold then, nor later, whatever the overlay shows by
/// then. What the overlay covers is asked at the position the finger came
/// down at.
#[test]
fn a_finger_on_the_overlay_is_the_overlays() {
    let start = Instant::now();
    let mut hold = Hold::default();
    let on_the_bar = egui::pos2(30.0, 5.0);
    hold.touch(1, TouchPhase::Started, on_the_bar, start);
    let asked = std::cell::Cell::new(None);
    let bar = |at: Pos2| {
        asked.set(Some(at));
        at.y < 20.0
    };
    assert!(!hold.due(start + HOLD, bar));
    assert_eq!(asked.get(), Some(on_the_bar));
    assert!(!hold.due(start + 10 * HOLD, nothing), "and not later");

    // A finger on the picture beside it is a hold all the same.
    hold.touch(2, TouchPhase::Started, egui::pos2(30.0, 300.0), start);
    assert!(!hold.due(start + HOLD - MS, bar));
    assert!(hold.due(start + HOLD, bar));
}

/// One hold works the switch once: when one finger is due, every finger
/// that is down is spent, those whose time comes at the same moment and
/// those whose time is still to come. A finger that comes down afterwards
/// can hold again.
#[test]
fn a_hold_spends_every_finger_that_is_down() {
    let start = Instant::now();
    let mut hold = Hold::default();
    let at = egui::pos2(200.0, 200.0);
    hold.touch(1, TouchPhase::Started, at, start);
    hold.touch(2, TouchPhase::Started, at, start);
    hold.touch(3, TouchPhase::Started, at, start + 300 * MS);
    assert!(hold.due(start + HOLD, nothing));
    assert!(!hold.due(start + 10 * HOLD, nothing), "all three are spent");

    hold.touch(4, TouchPhase::Started, at, start + 10 * HOLD);
    assert!(hold.due(start + 11 * HOLD, nothing));
}

/// What the overlay covers is asked of no finger before its time: a finger
/// that is not due yet is not looked for under the overlay.
#[test]
fn the_overlay_is_asked_only_when_a_finger_is_due() {
    let start = Instant::now();
    let mut hold = Hold::default();
    hold.touch(1, TouchPhase::Started, egui::pos2(1.0, 1.0), start);
    let asked = std::cell::Cell::new(0);
    let counting = |_: Pos2| {
        asked.set(asked.get() + 1);
        false
    };
    for step in 0..5u32 {
        hold.due(start + step * 100 * MS, counting);
    }
    assert_eq!(asked.get(), 0);
    assert!(hold.due(start + HOLD, counting));
    assert!(!hold.due(start + 2 * HOLD, everything));
    assert_eq!(asked.get(), 1);
}
