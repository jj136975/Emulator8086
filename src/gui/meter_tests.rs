//! The meter's arithmetic, on a clock driven by hand: each test states the
//! frames, their slices to the millisecond, what the machine counted in each
//! and what the overlay and the window measured in its rest, and reads the
//! averages, the history and the rates back. Only the last test has a
//! machine, to read its counts.
//!
//! The invariant: nothing here waits. The clock's origin is the one instant
//! read from the host; every other instant is that origin and a duration
//! the test states.

use super::*;
use crate::gui::overlay::testing::waiting_for_a_key;
use emu8086_core::io::keyboard::EmulatorEvent;

fn ms(millis: u64) -> Duration {
    Duration::from_millis(millis)
}

/// A frame as a test states it: milliseconds of the rest of the frame, then
/// of its slice and of what the slice waited; what the machine counted in
/// the slice. Of the rest, what the overlay's pass, the drawing and the wait
/// for the display took, which the window measures; what is left of the rest
/// nothing measured.
#[derive(Clone, Copy, Default)]
struct Frame {
    rest: u64,
    slice: u64,
    waited: u64,
    paused: bool,
    instructions: u64,
    interrupts: [u64; LINES],
    delivered: u64,
    halted: u64,
    dropped: u64,
    overlay: u64,
    drawing: u64,
    display_wait: u64,
}

/// A frame of 20 ms, half of it a slice that runs the guest for 200,000
/// instructions: ten million a second, fifty frames a second.
const STEADY: Frame = Frame {
    rest: 10,
    slice: 10,
    waited: 0,
    paused: false,
    instructions: 200_000,
    interrupts: [0; LINES],
    delivered: 0,
    halted: 0,
    dropped: 0,
    overlay: 0,
    drawing: 0,
    display_wait: 0,
};

/// A paused guest's frame: 10 ms of the rest, a slice of 10 ms all waited,
/// and nothing counted.
const PAUSED: Frame = Frame {
    paused: true,
    waited: 10,
    instructions: 0,
    ..STEADY
};

/// A meter, the clock that drives it, and the counts of a machine that does
/// what the test says.
struct Feed {
    meter: Meter,
    /// The instant the clock started from.
    origin: Instant,
    /// Where the clock stands: the end of the last slice.
    now: Duration,
    /// What the machine has counted so far.
    counts: Counts,
}

impl Feed {
    /// A meter told of its first frame, a slice of 10 ms at the start of
    /// the clock, after which the machine had counted nothing.
    fn new() -> Self {
        Self::after(Counts::default())
    }

    /// A meter told of its first frame, after which the machine had
    /// counted `counts`.
    fn after(counts: Counts) -> Self {
        let mut feed = Self {
            meter: Meter::default(),
            origin: Instant::now(),
            now: Duration::ZERO,
            counts,
        };
        feed.frame(Frame {
            rest: 0,
            instructions: 0,
            ..STEADY
        });
        feed
    }

    /// Tells the meter of `frame`: what the overlay and the window measured
    /// in its rest, which came after the slice before it, and then its slice.
    fn frame(&mut self, frame: Frame) {
        let start = self.now + ms(frame.rest);
        self.now = start + ms(frame.slice);
        self.meter.overlay_pass(ms(frame.overlay));
        self.meter.drawn(Drawn {
            waited: ms(frame.display_wait),
            drawing: ms(frame.drawing),
        });
        let counts = &mut self.counts;
        counts.instructions += frame.instructions;
        for (count, taken) in counts.interrupts.iter_mut().zip(frame.interrupts) {
            *count += taken;
        }
        counts.time.delivered += frame.delivered;
        counts.time.halted += frame.halted;
        counts.time.dropped += frame.dropped;
        let slice = Slice {
            start: self.origin + start,
            end: self.origin + self.now,
            waited: ms(frame.waited),
            paused: frame.paused,
        };
        self.meter.frame(slice, self.counts);
    }

    /// Tells the meter of `count` frames like `frame`.
    fn frames(&mut self, count: usize, frame: Frame) {
        for _ in 0..count {
            self.frame(frame);
        }
    }
}

/// Before a second has gone by there is no average: no frames, no speed,
/// no rates. The frame that ends a second after the first one began closes
/// the first average.
#[test]
fn nothing_is_averaged_before_a_second_has_gone_by() {
    let mut feed = Feed::new();
    feed.frames(49, STEADY);
    let meter = &feed.meter;
    assert_eq!(meter.frames(), None);
    assert_eq!((meter.second(), meter.speed()), (None, None));
    assert_eq!(meter.interrupt_rates(), None);

    feed.frame(STEADY);
    assert_eq!(feed.meter.speed(), Some(10_000_000.0));
    assert_eq!(feed.meter.frames().map(|frames| frames.count), Some(50));
}

/// The speed is the instructions of the guest's last second over the host
/// time it took, and it changes only when the next second is over.
#[test]
fn the_speed_is_the_instructions_of_a_second_over_its_host_time() {
    let mut feed = Feed::new();
    feed.frames(50, STEADY);
    let fast = Frame {
        instructions: 500_000,
        ..STEADY
    };
    feed.frames(49, fast);
    assert_eq!(
        feed.meter.speed(),
        Some(10_000_000.0),
        "the second is not over"
    );
    feed.frame(fast);
    assert_eq!(feed.meter.speed(), Some(25_000_000.0));
    let second = feed.meter.second().unwrap();
    assert_eq!((second.time, second.instructions), (ms(1000), 25_000_000));
}

/// The frames of a second are counted and timed, and where nothing but the
/// slice is measured, each one's host time divides three ways: what its
/// slice ran the guest for, what its slice let go by, and the rest of it,
/// which is waiting for the display.
#[test]
fn the_host_time_of_the_frames_divides_three_ways() {
    let mut feed = Feed::new();
    // 5 ms of the rest, then 15 of a slice of which 12 waited: 3 ms run.
    let frame = Frame {
        rest: 5,
        slice: 15,
        waited: 12,
        ..STEADY
    };
    feed.frames(48, frame);
    feed.frame(Frame { rest: 25, ..frame }); // a frame of 40 ms
    let frames = *feed.meter.frames().expect("a second of frames");
    assert_eq!(frames.count, 49);
    assert_eq!((frames.time, frames.longest), (ms(1000), ms(40)));
    assert_eq!(frames.mean(), Duration::from_nanos(1_000_000_000 / 49));
    assert_eq!(frames.rate(), 49.0);
    assert_eq!(
        (frames.running, frames.idle, frames.waiting()),
        (ms(49 * 3), ms(49 * 12), ms(48 * 5 + 25))
    );
    assert_eq!(frames.share(frames.running), 0.147);
    assert_eq!(frames.share(frames.idle), 0.588);

    // The next average starts from nothing: the longest frame is its own.
    feed.frames(50, STEADY);
    let frames = feed.meter.frames().unwrap();
    assert_eq!((frames.count, frames.longest), (50, ms(20)));
}

/// Where the overlay's pass and the window's drawing are measured, each frame
/// divides five ways: what its slice ran the guest for, what its slice let go
/// by, the overlay, the drawing, and the wait for the display, which is the
/// wait the window measured with what nothing measured. The five make up the
/// frame's host time.
#[test]
fn a_frame_divides_five_ways() {
    let mut feed = Feed::new();
    // 10 ms of the rest: 3 of the overlay, 2 of drawing, 4 of waiting for the
    // display and 1 that nothing measured; then a slice of 10 ms that runs.
    let frame = Frame {
        overlay: 3,
        drawing: 2,
        display_wait: 4,
        ..STEADY
    };
    feed.frames(50, frame);
    let frames = *feed.meter.frames().expect("a second of frames");
    assert_eq!((frames.count, frames.time), (50, ms(1000)));
    assert_eq!(
        (
            frames.running,
            frames.idle,
            frames.overlay,
            frames.drawing,
            frames.waited
        ),
        (ms(500), ms(0), ms(150), ms(100), ms(200))
    );
    assert_eq!(frames.waiting(), ms(250), "200 measured, 50 unmeasured");
    let five = frames.running + frames.idle + frames.overlay + frames.drawing + frames.waiting();
    assert_eq!(five, frames.time);
}

/// What is measured after a slice is part of the frame that the next slice
/// ends, and not of the one that slice ended: told after the last slice of
/// an average, it is in the next average.
#[test]
fn what_is_measured_after_a_slice_is_part_of_the_next_frame() {
    let mut feed = Feed::new();
    feed.frames(49, STEADY);
    feed.meter.overlay_pass(ms(3));
    feed.frame(STEADY);
    assert_eq!(
        feed.meter.frames().map(|frames| frames.overlay),
        Some(ms(3))
    );

    feed.meter.overlay_pass(ms(5));
    feed.frames(49, STEADY);
    assert_eq!(
        feed.meter.frames().map(|frames| frames.overlay),
        Some(ms(3)),
        "the second second is not over"
    );
    feed.frame(STEADY);
    assert_eq!(
        feed.meter.frames().map(|frames| frames.overlay),
        Some(ms(5))
    );
}

/// What is measured before the first slice ended is part of no frame: the
/// first frame begins at that slice, so there is no frame to count it in.
#[test]
fn what_is_measured_before_the_first_slice_is_in_no_frame() {
    let mut feed = Feed {
        meter: Meter::default(),
        origin: Instant::now(),
        now: Duration::ZERO,
        counts: Counts::default(),
    };
    feed.meter.overlay_pass(ms(7));
    feed.meter.drawn(Drawn {
        waited: ms(2),
        drawing: ms(1),
    });
    feed.frames(51, STEADY);
    let frames = feed.meter.frames().expect("a second of frames");
    assert_eq!(
        (frames.overlay, frames.drawing, frames.waited),
        (ms(0), ms(0), ms(0))
    );
}

/// The history holds the guest's last minute: sixty seconds of running,
/// oldest first. The sixty-first second pushes the first out.
#[test]
fn the_history_holds_the_last_minute() {
    let mut feed = Feed::new();
    for second in 1..=HISTORY as u64 + 1 {
        // `second` million instructions a second.
        let frame = Frame {
            instructions: second * 20_000,
            ..STEADY
        };
        feed.frames(50, frame);
    }
    let speeds: Vec<f64> = feed.meter.seconds().map(Second::speed).collect();
    assert_eq!(speeds.len(), HISTORY);
    assert_eq!(speeds.first(), Some(&2_000_000.0));
    assert_eq!(speeds.last(), Some(&61_000_000.0));
    assert!(
        speeds.windows(2).all(|pair| pair[0] < pair[1]),
        "oldest first"
    );
}

/// A paused guest contributes no speed. The frames it stands paused are no
/// part of its seconds: a second split by a pause is the running before and
/// after it, and while it is paused the last speed and the history stay as
/// they were. The frames are still measured: they go on, all idle and waiting.
#[test]
fn a_paused_guest_contributes_no_speed() {
    let mut feed = Feed::new();
    feed.frames(50, STEADY);
    assert_eq!(feed.meter.speed(), Some(10_000_000.0));

    // Half a second at 10 M a second, three seconds paused, and half a
    // second at 30 M a second: a second of running, 20 M of instructions.
    feed.frames(25, STEADY);
    feed.frames(150, PAUSED);
    assert_eq!(feed.meter.speed(), Some(10_000_000.0), "the same second");
    assert_eq!(feed.meter.seconds().len(), 1, "no second while paused");
    let frames = *feed.meter.frames().unwrap();
    assert_eq!(frames.running, Duration::ZERO, "a second of paused frames");
    assert_eq!((frames.idle, frames.waiting()), (ms(500), ms(500)));

    let fast = Frame {
        instructions: 600_000,
        ..STEADY
    };
    feed.frames(25, fast);
    assert_eq!(feed.meter.speed(), Some(20_000_000.0));
    assert_eq!(feed.meter.second().unwrap().time, ms(1000));
    assert_eq!(feed.meter.seconds().len(), 2);
}

/// The halted share is of the guest's own time: the timer clocks handed to
/// it while its CPU was halted, over all the clocks handed to it in the
/// second. A second in which it was handed no time has no share.
#[test]
fn the_halted_share_is_of_the_time_the_guest_was_handed() {
    let mut feed = Feed::new();
    let idling = Frame {
        delivered: 23_864,
        halted: 17_898,
        ..STEADY
    };
    feed.frames(50, idling);
    assert_eq!(feed.meter.second().unwrap().halted_share(), Some(0.75));

    feed.frames(50, STEADY);
    assert_eq!(feed.meter.second().unwrap().halted_share(), None);
}

/// The guest's clock is the time it was handed over the host time it ran
/// for: all of it when the guest keeps up. Time dropped makes the guest
/// fall behind for the second it was dropped in; the next second keeps up
/// again, and what was dropped is still told in all. What was dropped
/// before the first frame, while the window opened, is left out.
#[test]
fn time_dropped_shows_the_guest_falling_behind() {
    let start = Counts {
        time: GuestTime {
            dropped: 1_000_000,
            ..GuestTime::default()
        },
        ..Counts::default()
    };
    let mut feed = Feed::after(start);
    // 1,193,182 clocks a second: 49 frames of 23,864 and one of 23,846.
    let keeping_up = Frame {
        delivered: 23_864,
        ..STEADY
    };
    feed.frames(49, keeping_up);
    feed.frame(Frame {
        delivered: 23_846,
        ..STEADY
    });
    let second = *feed.meter.second().unwrap();
    assert_eq!(second.delivered, INPUT_HZ);
    assert_eq!(second.clock_rate(), 1.0);
    assert!(second.kept_up());
    assert_eq!(feed.meter.dropped(), Duration::ZERO);

    // A frame of 300 ms: 250 of them owed, 50 dropped.
    feed.frame(Frame {
        rest: 290,
        delivered: 298_295,
        dropped: 59_659,
        ..STEADY
    });
    feed.frames(35, keeping_up);
    let second = *feed.meter.second().unwrap();
    assert_eq!(second.dropped, 59_659);
    // 59,659 clocks of 1,193,182 a second, to the nanosecond below.
    assert_eq!(second.dropped_time(), Duration::from_nanos(49_999_916));
    assert!(!second.kept_up());
    assert!(second.clock_rate() < KEEPING_UP);

    feed.frames(50, keeping_up);
    assert!(feed.meter.second().unwrap().kept_up());
    assert_eq!(feed.meter.dropped(), Duration::from_nanos(49_999_916));
}

/// A guest whose clock falls short of the host's by more than a percent
/// over a second has not kept up, even with nothing dropped: it is owed
/// more and more.
#[test]
fn a_guest_owed_more_and_more_does_not_keep_up() {
    let mut feed = Feed::new();
    feed.frames(
        50,
        Frame {
            delivered: 23_000,
            ..STEADY
        },
    );
    let second = feed.meter.second().unwrap();
    assert_eq!(second.dropped, 0);
    assert!(second.clock_rate() < 0.97);
    assert!(!second.kept_up());
}

/// The interrupts of each line are counted per second over the guest's
/// last ten seconds of running, or as many as there are: over ten, a timer
/// that interrupts 18 times in one second and 19 in the next is at 18.5 a
/// second, where a single second says 18 or 19.
#[test]
fn interrupt_rates_are_averaged_over_the_last_ten_seconds() {
    let mut irq0 = [0; LINES];
    irq0[0] = 1;
    let tick = Frame {
        interrupts: irq0,
        ..STEADY
    };
    // A second in which the timer interrupts `ticks` times.
    let second = |feed: &mut Feed, ticks: usize| {
        feed.frames(ticks, tick);
        feed.frames(50 - ticks, STEADY);
    };

    let mut feed = Feed::new();
    second(&mut feed, 18);
    let rates = feed.meter.interrupt_rates().unwrap();
    assert_eq!(rates[0], 18.0);
    assert_eq!(rates[1..], [0.0; LINES - 1]);

    for index in 0..11 {
        second(&mut feed, if index % 2 == 0 { 19 } else { 18 });
    }
    // Two keystrokes in the last second: each a press and a release.
    let mut irq1 = [0; LINES];
    irq1[1] = 4;
    feed.frame(Frame {
        interrupts: irq1,
        ..STEADY
    });
    feed.frames(49, STEADY);
    // The last ten: nine seconds of the timer, 19 + 18 + ... + 19 = 167,
    // then the keyboard's second.
    let rates = feed.meter.interrupt_rates().unwrap();
    assert_eq!(rates[0], 16.7);
    assert_eq!(rates[1], 0.4);
}

/// The programmed rate of IRQ0 is channel 0's: the input clock over its
/// period, while it repeats; none while it does not.
#[test]
fn the_programmed_rate_of_the_timer_is_channel_0s() {
    let bios = Counts {
        timer_period: Some(0x10000),
        ..Counts::default()
    };
    let feed = Feed::after(bios);
    assert_eq!(feed.meter.timer_rate(), Some(INPUT_HZ as f64 / 65536.0));
    assert_eq!(Feed::new().meter.timer_rate(), None);
}

/// What the machine counted is told as it stands after the last slice,
/// since power-on, and the uptime is the host time from the beginning of
/// the first frame to the end of the last, paused or not.
#[test]
fn counts_and_uptime_are_from_power_on() {
    let start = Counts {
        instructions: 1234,
        ..Counts::default()
    };
    let mut feed = Feed::after(start);
    assert_eq!(feed.meter.uptime(), ms(10));
    feed.frames(10, STEADY);
    feed.frames(10, PAUSED);
    assert_eq!(feed.meter.uptime(), ms(410));
    assert_eq!(
        feed.meter.counts().map(|counts| counts.instructions),
        Some(1234 + 10 * 200_000)
    );
    assert_eq!(Meter::default().uptime(), Duration::ZERO);
    assert_eq!(Meter::default().counts(), None);
}

/// The counts are read from the machine as it stands: its instructions,
/// what its 8259A acknowledged on each line, the account of its time, and
/// the period of its timer as the BIOS programmed it (mode 3, a count of
/// zero: 65536 clocks). A key the guest takes is counted on IRQ1.
#[test]
fn counts_are_read_from_the_machine() {
    let (mut runtime, keys) = waiting_for_a_key();
    let counts = Counts::read(&runtime);
    assert_eq!(
        counts,
        Counts {
            instructions: runtime.instruction_count,
            interrupts: runtime.cpu.pic.acknowledged(),
            time: runtime.guest_time(),
            timer_period: Some(0x10000),
        }
    );

    keys.send(EmulatorEvent::Scancode(0x1E)).unwrap();
    runtime.run(1); // the keyboard is polled, and IRQ1 taken
    let after = Counts::read(&runtime);
    assert_eq!(after.interrupts[1], counts.interrupts[1] + 1);
}
