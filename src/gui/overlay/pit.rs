//! The 8253 panel: the interval timer's three channels side by side, as its
//! datasheet describes them: the mode, the reload value and the current
//! count, the output and the gate, and what a period comes to, for channel
//! 0 the rate of IRQ0. It only reads.
//!
//! The invariant: the panel reads the channels' view (`Pit::channels`),
//! never a count port. A read of one moves on the byte sequence the guest
//! counts on, or releases a count it latched.

use egui::{Grid, RichText, Ui};

use emu8086_core::io::pit::{AccessMode, INPUT_HZ, PitChannelView};

use super::Cx;
use super::theme::PALETTE;

/// What each channel drives on the PC/XT.
const WIRED_TO: [&str; 3] = ["IRQ0", "DRAM refresh", "speaker"];

/// The six modes, as the datasheet names them, shortened.
const MODES: [&str; 6] = [
    "terminal count",
    "one-shot",
    "rate generator",
    "square wave",
    "software strobe",
    "hardware strobe",
];

/// What a cell shows where there is nothing to say.
const NOTHING: &str = "—";

/// A frequency, to four figures: `18.21 Hz`, `896.4 Hz`, `66.29 kHz`.
fn hertz(hz: f64) -> String {
    if hz >= 1000.0 {
        format!("{:.2} kHz", hz / 1000.0)
    } else if hz >= 100.0 {
        format!("{hz:.1} Hz")
    } else {
        format!("{hz:.2} Hz")
    }
}

/// What the channel's output does over time: the rate of its periods in
/// modes 2 and 3, which repeat; a single event in the others.
fn rate(channel: &PitChannelView) -> String {
    match channel.mode {
        _ if !channel.counting => NOTHING.to_string(),
        2 | 3 if !channel.gate => "stopped by its gate".to_string(),
        2 | 3 => hertz(INPUT_HZ as f64 / f64::from(channel.divisor())),
        _ => "once".to_string(),
    }
}

/// The period of IRQ0 in words, when channel 0 repeats: `IRQ0 at 18.21 Hz,
/// every 54.93 ms`.
fn tick(channel: &PitChannelView) -> Option<String> {
    let repeats = channel.counting && channel.gate && matches!(channel.mode, 2 | 3);
    repeats.then(|| {
        let period = f64::from(channel.divisor()) / INPUT_HZ as f64;
        format!(
            "IRQ0 at {}, every {:.2} ms",
            hertz(1.0 / period),
            period * 1000.0
        )
    })
}

/// The reload value as written, and the count it stands for.
fn reload(channel: &PitChannelView) -> String {
    format!("{:04X}h = {}", channel.reload, channel.divisor())
}

/// The state of the counter in words: counting, waiting for a count, or
/// holding a latched count for the guest to read.
fn state(channel: &PitChannelView) -> String {
    match channel.latched {
        _ if !channel.counting => "waiting for a count".to_string(),
        Some(latched) => format!("latched at {latched:04X}h"),
        None => "counting".to_string(),
    }
}

/// How the count goes through the port, RL1 and RL0 of the control word.
fn access(mode: AccessMode) -> &'static str {
    match mode {
        AccessMode::Low => "LSB only",
        AccessMode::High => "MSB only",
        AccessMode::LowThenHigh => "LSB, then MSB",
    }
}

/// A pin: high in the colour of an active line, low in that of one at rest.
fn pin(high: bool) -> RichText {
    if high {
        RichText::new("high").color(PALETTE.on)
    } else {
        RichText::new("low").color(PALETTE.off)
    }
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    let channels = cx.runtime.pit().channels();

    Grid::new("pit_channels")
        .num_columns(4)
        .striped(true)
        .show(ui, |ui| {
            ui.strong("Channel");
            for index in 0..channels.len() {
                ui.strong(index.to_string());
            }
            ui.end_row();

            // One row per property, one column per channel.
            let mut row = |name: &str, cell: &dyn Fn(usize, &PitChannelView) -> RichText| {
                ui.label(name);
                for (index, channel) in channels.iter().enumerate() {
                    ui.label(cell(index, channel));
                }
                ui.end_row();
            };
            row("Wired to", &|index, _| RichText::new(WIRED_TO[index]));
            row("Mode", &|_, channel| {
                let name = MODES[channel.mode as usize];
                RichText::new(format!("{} {name}", channel.mode))
            });
            row("Reload", &|_, channel| {
                RichText::new(reload(channel)).monospace()
            });
            row("Count", &|_, channel| {
                RichText::new(format!("{:04X}h", channel.count)).monospace()
            });
            row("Access", &|_, channel| {
                RichText::new(access(channel.access))
            });
            row("Counts in", &|_, channel| {
                RichText::new(if channel.bcd { "BCD" } else { "binary" })
            });
            row("Output", &|_, channel| pin(channel.output));
            row("Gate", &|_, channel| pin(channel.gate));
            row("State", &|_, channel| RichText::new(state(channel)));
            row("Rate", &|_, channel| RichText::new(rate(channel)));
        });

    if let Some(tick) = tick(&channels[0]) {
        ui.add_space(ui.spacing().item_spacing.y);
        ui.label(tick);
    }
}

#[cfg(test)]
#[path = "pit_tests.rs"]
mod tests;
