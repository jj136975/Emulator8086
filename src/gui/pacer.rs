//! Real-time pacing: how the guest gets its time. The machine reads no
//! clock, so `Pacer` reads the host's for it: it tells the machine how much
//! time has passed since it last looked (`Runtime::credit_pit_time`) and
//! lets the guest run a few instructions (`Runtime::run`), over and over
//! until a slice of host time is used up. A paused guest is not run: its
//! slices go by without it.
//!
//! The invariant: the guest's time is the host's, less the time it stood
//! paused. All the time the machine is credited with was measured here
//! between two samples that follow one another, with none left out and none
//! counted twice, and the clock is sampled every `CLOCK_SAMPLE_INTERVAL`
//! instructions while the guest runs and at once when it halts: no timer
//! interrupt is later than that. A slice that goes by paused moves the last
//! sample to its end and credits nothing, so that the pause is owed to
//! nobody when the guest runs again. On a host that cannot wait, a slice
//! with nothing to run ends early, and the time until the next slice is
//! credited by that slice's first sample like any other.

use std::time::Duration;

use emu8086_core::vm::runtime::Runtime;

use crate::platform::{self, Instant};

/// Instructions the guest runs between two samples of the host clock;
/// reading it for every instruction would dominate run time.
const CLOCK_SAMPLE_INTERVAL: u64 = 256;
/// How long a halted guest is left alone before it is looked at again.
const IDLE_POLL: Duration = Duration::from_millis(1);

/// How a pacer lets host time go by: `platform::wait`, or what a test puts
/// in its place. `false` says the host cannot wait.
type Wait = fn(Duration) -> bool;

/// Measures host time and feeds it to the machine it paces.
pub struct Pacer {
    /// Host time of the last clock sample: the machine has been credited
    /// with all the time up to it that it did not stand paused.
    last_sample: Instant,
    /// The guest stands still: slices go by and credit nothing.
    paused: bool,
    /// How time the guest has no use for is let go by.
    wait: Wait,
}

impl Pacer {
    /// A pacer that waits as the host does. The first sample credits the
    /// time since this call.
    pub fn new() -> Self {
        Self::waiting_with(platform::wait)
    }

    /// A pacer that lets time go by with `wait`.
    fn waiting_with(wait: Wait) -> Self {
        Self {
            last_sample: Instant::now(),
            paused: false,
            wait,
        }
    }

    /// Holds the guest still from the next slice on, or lets it run again.
    pub fn set_paused(&mut self, paused: bool) {
        self.paused = paused;
    }

    pub fn paused(&self) -> bool {
        self.paused
    }

    /// Samples the host clock and credits `runtime` with the time since
    /// the previous sample. Returns the sample.
    fn sample(&mut self, runtime: &mut Runtime) -> Instant {
        let now = Instant::now();
        let elapsed = now.duration_since(self.last_sample);
        self.last_sample = now;
        runtime.credit_pit_time(u64::try_from(elapsed.as_nanos()).unwrap_or(u64::MAX));
        now
    }

    /// Runs `runtime` for about `slice` of host time. The time that went by
    /// since the previous slice (rendering, vsync) is credited first. A
    /// halted guest does not end the slice: it is looked at again every
    /// `IDLE_POLL`, so that timer ticks and key presses reach an idle guest
    /// promptly. A paused guest is not looked at: the slice is waited out,
    /// as long as any other, so that whoever calls slice after slice is
    /// paced the same.
    ///
    /// On a host that cannot wait there is no waiting out: the slice of a
    /// paused guest, and of a guest that is halted with nothing due, ends
    /// at once, and it is for the caller to come back, as a browser does
    /// with its next frame. A halted guest is then looked at once per
    /// slice.
    pub fn run_slice(&mut self, runtime: &mut Runtime, slice: Duration) {
        if self.paused {
            (self.wait)(slice);
            self.last_sample = Instant::now();
            return;
        }

        let mut now = self.sample(runtime);
        let deadline = now + slice;
        while now < deadline {
            let executed = runtime.instruction_count;
            runtime.run(CLOCK_SAMPLE_INTERVAL);
            // A guest that halts gets the time that has passed at once: the
            // next sample follows without a wait. One that was halted all
            // through a look has had all the time there is, and waits.
            if runtime.cpu.halted
                && runtime.instruction_count == executed
                && !(self.wait)(IDLE_POLL.min(deadline - now))
            {
                // Sampling again on the spot would spin until the deadline.
                // The time from the last sample on is not lost: the next
                // slice credits it.
                return;
            }
            now = self.sample(runtime);
        }
    }
}

#[cfg(test)]
#[path = "pacer_tests.rs"]
mod tests;
