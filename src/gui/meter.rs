//! Measuring the emulation: how fast the guest runs, how the host's frames
//! divide their time between running it and everything else, and whether
//! the guest's time keeps up with the host's. `Meter` is told of each frame
//! once its slice is over (`Session::frame`): what the pacer did with the
//! slice's host time (`pacer::Slice`), and what the machine has counted
//! since power-on (`Counts`). It is told too of what the frame spent outside
//! its slice: the overlay's pass (`overlay_pass`), and the window's drawing
//! and its wait for the display (`drawn`, the renderer's `Drawn`). It reads
//! no clock and holds no machine: what it shows is made of what it is told,
//! so a test tells it a history by hand, instant by instant.
//!
//! A frame is the host time from the end of one slice to the end of the
//! next: whatever came between the two (the overlay, the drawing, the wait
//! for the display), then the slice. What is told after a slice is part of
//! the frame that the next slice ends, and it is kept until then. The
//! figures are averages over about a second: the frames until the first
//! that ends a second after the last average, and the guest's seconds of
//! running. A paused guest runs nothing and is owed nothing: the frames it
//! stands paused are measured as frames, and are no part of its seconds.
//!
//! The invariant: a figure is the ratio of two counts over the same span.
//! The counts are read after a slice and a frame ends with its slice, so
//! what a slice did and the host time it was credited for fall into the
//! same second. The parts of a frame do not overlap: its slice, the overlay's
//! pass and the window's drawing and wait are spans of the host clock within
//! the frame. So they add up to no more than its time, and the waiting for
//! the display is what the rest of the frame leaves.

use std::collections::VecDeque;
use std::time::Duration;

use emu8086_core::io::pit::{INPUT_HZ, PitChannelView};
use emu8086_core::vm::runtime::{GuestTime, Runtime};

use super::pacer::Slice;
use super::renderer::Drawn;
use crate::platform::Instant;

/// The span of an average, at the least: it covers the frames until the
/// first that ends this long after the last average.
pub const AVERAGED_OVER: Duration = Duration::from_secs(1);
/// The seconds of the guest's running the meter keeps: the last minute.
pub const HISTORY: usize = 60;
/// The seconds of running the rates of interrupts are averaged over. In
/// one second the BIOS's timer, at 18.2 Hz, interrupts 18 times or 19,
/// five percent apart; over ten, the count is within half a percent.
pub const RATE_SECONDS: usize = 10;
/// The interrupt lines of the 8259A.
pub const LINES: usize = 8;
/// The share of real time the guest's clock may fall short of over a
/// second and still keep up: what it is owed at either end of the second
/// differs by up to a frame's worth.
const KEEPING_UP: f64 = 0.99;

/// What the machine has counted since power-on, as the meter reads it
/// after a slice.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Counts {
    /// Instructions executed.
    pub instructions: u64,
    /// Interrupts acknowledged on each line, IRQ0 first.
    pub interrupts: [u64; LINES],
    /// Where the guest's time went, in timer clocks.
    pub time: GuestTime,
    /// The period of IRQ0 as channel 0 of the 8253 is programmed, in timer
    /// clocks, while the channel repeats: modes 2 and 3, counting, its gate
    /// high.
    pub timer_period: Option<u32>,
}

impl Counts {
    /// What `runtime` has counted so far. Nothing in the machine moves: the
    /// devices are read through their descriptions, never their ports.
    pub fn read(runtime: &Runtime) -> Self {
        let timer = runtime.pit().channels()[0];
        Self {
            instructions: runtime.instruction_count,
            interrupts: runtime.cpu.pic.acknowledged(),
            time: runtime.guest_time(),
            timer_period: repeats(&timer).then(|| timer.divisor()),
        }
    }
}

/// Whether a channel's output rises on its own, again and again: modes 2
/// and 3, with a count to run down and its gate high.
fn repeats(channel: &PitChannelView) -> bool {
    channel.counting && channel.gate && matches!(channel.mode, 2 | 3)
}

/// Frames, summed up over the span of an average.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Frames {
    /// How many ended in the span.
    pub count: u32,
    /// The host time they took, end to end.
    pub time: Duration,
    /// The longest of them.
    pub longest: Duration,
    /// Of their time, what their slices spent running the guest,
    pub running: Duration,
    /// and what their slices let go by: the waits for a halted guest, the
    /// slices of a paused one.
    pub idle: Duration,
    /// What the overlay's passes took, each in the frame after its slice.
    pub overlay: Duration,
    /// What the window's drawing took, presenting included.
    pub drawing: Duration,
    /// What the window waited for the display: for the surface to hand it a
    /// frame to draw on. `Frames::waiting` adds what nothing measured.
    pub waited: Duration,
}

impl Frames {
    fn add(&mut self, frame: Duration, parts: Parts) {
        self.count += 1;
        self.time += frame;
        self.longest = self.longest.max(frame);
        self.running += parts.running;
        self.idle += parts.idle;
        self.overlay += parts.overlay;
        self.drawing += parts.drawing;
        self.waited += parts.waited;
    }

    /// Frames per second.
    pub fn rate(&self) -> f64 {
        per_second(f64::from(self.count), self.time)
    }

    /// The host time of a frame, on average.
    pub fn mean(&self) -> Duration {
        self.time / self.count.max(1)
    }

    /// Of their time, what waited for the display: the wait the window
    /// measured, and whatever else of the time nothing measured. In a
    /// browser the latter is the wait for the next animation frame.
    pub fn waiting(&self) -> Duration {
        let measured = self.running + self.idle + self.overlay + self.drawing + self.waited;
        self.waited + self.time.saturating_sub(measured)
    }

    /// The share of their time `part` is, from 0 to 1.
    pub fn share(&self, part: Duration) -> f64 {
        per_second(part.as_secs_f64(), self.time)
    }
}

/// What a frame's host time went to, as the meter is told of it: what its
/// slice spent running the guest and letting time go by, and what the
/// overlay and the window measured outside the slice.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Parts {
    running: Duration,
    idle: Duration,
    overlay: Duration,
    drawing: Duration,
    waited: Duration,
}

/// A second of the guest's running: what the machine counted in it, and
/// the host time it took.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Second {
    /// The host time the guest was run for. The frames it stood paused are
    /// no part of it.
    pub time: Duration,
    /// Instructions executed.
    pub instructions: u64,
    /// Interrupts acknowledged on each line.
    pub interrupts: [u64; LINES],
    /// Timer clocks handed to the guest,
    pub delivered: u64,
    /// and of them, while its CPU was halted.
    pub halted: u64,
    /// Timer clocks dropped: host time the guest will never be handed.
    pub dropped: u64,
    /// Timer clocks owed when the second ended.
    pub owed: u64,
}

impl Second {
    /// What the machine counted from `from` to `to`, `time` of running.
    fn between(from: &Counts, to: &Counts, time: Duration) -> Self {
        Self {
            time,
            instructions: to.instructions.saturating_sub(from.instructions),
            interrupts: std::array::from_fn(|line| {
                to.interrupts[line].saturating_sub(from.interrupts[line])
            }),
            delivered: to.time.delivered.saturating_sub(from.time.delivered),
            halted: to.time.halted.saturating_sub(from.time.halted),
            dropped: to.time.dropped.saturating_sub(from.time.dropped),
            owed: to.time.owed,
        }
    }

    /// Instructions per second of host time.
    pub fn speed(&self) -> f64 {
        per_second(self.instructions as f64, self.time)
    }

    /// The share of the time it was handed that the guest spent halted,
    /// from 0 to 1: none when it was handed no time.
    pub fn halted_share(&self) -> Option<f64> {
        (self.delivered > 0).then(|| self.halted as f64 / self.delivered as f64)
    }

    /// The guest's time over the host's: 1 when the guest was handed all
    /// the time that passed while it ran.
    pub fn clock_rate(&self) -> f64 {
        per_second(self.delivered as f64 / INPUT_HZ as f64, self.time)
    }

    /// The host time that was dropped.
    pub fn dropped_time(&self) -> Duration {
        timer_time(self.dropped)
    }

    /// The timer time owed when the second ended.
    pub fn owed_time(&self) -> Duration {
        timer_time(self.owed)
    }

    /// Whether the guest kept up with real time: none of its time was
    /// dropped, and its clock ran at the host's, within a percent.
    pub fn kept_up(&self) -> bool {
        self.dropped == 0 && self.clock_rate() >= KEEPING_UP
    }
}

/// The measure of the emulation, frame by frame. See the module's header.
#[derive(Default)]
pub struct Meter {
    /// When the first frame began: power-on, as far as host time goes.
    started: Option<Instant>,
    /// What the machine had counted after the first slice: what it has
    /// counted since, the meter saw happen.
    origin: Counts,
    /// When the last slice ended, and what the machine had counted then.
    last: Option<(Instant, Counts)>,
    /// The frames since the last average,
    frames: Frames,
    /// and that average.
    averaged: Option<Frames>,
    /// The guest's running since its last second ended,
    running: Duration,
    /// and what the machine had counted when it began.
    opened: Counts,
    /// The guest's seconds, oldest first: the last minute of its running.
    seconds: VecDeque<Second>,
    /// What the overlay and the window measured since the last slice ended,
    /// for the frame that the next slice ends. Its running and idle stay at
    /// none: those come from the slice.
    outside: Parts,
}

impl Meter {
    /// Takes in a frame: the slice that ran the guest in it, and what the
    /// machine counted after that slice. The first frame sets the counts
    /// off; it has no slice before it to begin at.
    pub fn frame(&mut self, slice: Slice, counts: Counts) {
        let Some((last_end, _)) = self.last.replace((slice.end, counts)) else {
            self.started = Some(slice.start);
            self.origin = counts;
            self.opened = counts;
            // Nothing measured before the first slice ended is in a frame.
            self.outside = Parts::default();
            return;
        };

        let frame = slice.end.duration_since(last_end);
        let guest = slice.end.duration_since(slice.start).min(frame);
        let idle = slice.waited.min(guest);
        let outside = std::mem::take(&mut self.outside);
        self.frames.add(
            frame,
            Parts {
                running: guest - idle,
                idle,
                ..outside
            },
        );
        if self.frames.time >= AVERAGED_OVER {
            self.averaged = Some(std::mem::take(&mut self.frames));
        }

        if slice.paused {
            return;
        }
        self.running += frame;
        if self.running >= AVERAGED_OVER {
            if self.seconds.len() == HISTORY {
                self.seconds.pop_front();
            }
            let second = Second::between(&self.opened, &counts, self.running);
            self.seconds.push_back(second);
            self.opened = counts;
            self.running = Duration::ZERO;
        }
    }

    /// The overlay's pass took `time`, run after the last slice: it is part
    /// of the frame that the next slice ends.
    pub fn overlay_pass(&mut self, time: Duration) {
        self.outside.overlay += time;
    }

    /// The window drew after the last slice, as `drawn` tells it: that is
    /// part of the frame that the next slice ends.
    pub fn drawn(&mut self, drawn: Drawn) {
        self.outside.waited += drawn.waited;
        self.outside.drawing += drawn.drawing;
    }

    /// The frames of the last average: none before the first second.
    pub fn frames(&self) -> Option<&Frames> {
        self.averaged.as_ref()
    }

    /// The guest's last second of running.
    pub fn second(&self) -> Option<&Second> {
        self.seconds.back()
    }

    /// The guest's seconds, oldest first: the last minute of its running.
    pub fn seconds(&self) -> impl ExactSizeIterator<Item = &Second> {
        self.seconds.iter()
    }

    /// Instructions per second, over the guest's last second of running.
    pub fn speed(&self) -> Option<f64> {
        self.second().map(Second::speed)
    }

    /// Interrupts per second on each line, IRQ0 first, over the guest's
    /// last `RATE_SECONDS` seconds of running, or as many as there were.
    pub fn interrupt_rates(&self) -> Option<[f64; LINES]> {
        let recent = self.seconds.iter().rev().take(RATE_SECONDS);
        let (time, counts) = recent.fold(
            (Duration::ZERO, [0u64; LINES]),
            |(time, mut counts), second| {
                for (count, taken) in counts.iter_mut().zip(second.interrupts) {
                    *count += taken;
                }
                (time + second.time, counts)
            },
        );
        (!time.is_zero()).then(|| counts.map(|count| per_second(count as f64, time)))
    }

    /// What the machine had counted since power-on, after the last slice.
    pub fn counts(&self) -> Option<&Counts> {
        self.last.as_ref().map(|(_, counts)| counts)
    }

    /// The host time from the beginning of the first frame to the end of
    /// the last.
    pub fn uptime(&self) -> Duration {
        match (self.started, &self.last) {
            (Some(started), Some((end, _))) => end.duration_since(started),
            _ => Duration::ZERO,
        }
    }

    /// The host time dropped since the first frame. What was dropped
    /// before it is left out: that was the time the window took to open,
    /// which the guest never was to run in.
    pub fn dropped(&self) -> Duration {
        let dropped = self.counts().map_or(0, |counts| counts.time.dropped);
        timer_time(dropped.saturating_sub(self.origin.time.dropped))
    }

    /// The rate of IRQ0 as channel 0 of the 8253 is programmed, in Hz:
    /// none while the channel does not repeat.
    pub fn timer_rate(&self) -> Option<f64> {
        let period = self.counts()?.timer_period?;
        Some(INPUT_HZ as f64 / f64::from(period))
    }
}

/// `amount` per second of `time`; nothing in no time.
fn per_second(amount: f64, time: Duration) -> f64 {
    if time.is_zero() {
        0.0
    } else {
        amount / time.as_secs_f64()
    }
}

/// The time `clocks` of the 8253's input stand for.
fn timer_time(clocks: u64) -> Duration {
    let nanos = u128::from(clocks) * 1_000_000_000 / u128::from(INPUT_HZ);
    Duration::from_nanos(u64::try_from(nanos).unwrap_or(u64::MAX))
}

#[cfg(test)]
#[path = "meter_tests.rs"]
mod tests;
