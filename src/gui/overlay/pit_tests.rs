//! The 8253 panel: what it makes of a channel's view, and what it shows of
//! the timer as the BIOS leaves it and as a guest programs a tone on it.

use super::*;
use crate::gui::overlay::testing::{Shell, waiting_for_a_key};

/// The panel's title, which the View menu and its window show.
const TITLE: &str = "Timer (8253)";

/// A channel counting in `mode` from `reload`, binary, its output and its
/// gate high.
fn channel(mode: u8, reload: u16) -> PitChannelView {
    PitChannelView {
        mode,
        access: AccessMode::LowThenHigh,
        bcd: false,
        reload,
        count: reload,
        output: true,
        gate: true,
        counting: true,
        latched: None,
    }
}

/// A frequency is given to four figures, in Hz below a kilohertz.
#[test]
fn a_frequency_has_four_figures() {
    assert_eq!(hertz(18.206_5), "18.21 Hz");
    assert_eq!(hertz(119.318_2), "119.3 Hz");
    assert_eq!(hertz(896.4), "896.4 Hz");
    assert_eq!(hertz(1000.15), "1.00 kHz");
    assert_eq!(hertz(66_287.9), "66.29 kHz");
}

/// Modes 2 and 3 repeat at the input clock over the divisor: channel 0 as
/// the BIOS sets it, a count of zero in mode 3, at 18.21 Hz, IRQ0 every
/// 54.93 ms. A low gate stops them; the other modes give one event per
/// count; a channel without a count gives nothing.
#[test]
fn the_rate_follows_the_mode_the_divisor_and_the_gate() {
    let bios = channel(3, 0x0000);
    assert_eq!(rate(&bios), "18.21 Hz");
    assert_eq!(
        tick(&bios).as_deref(),
        Some("IRQ0 at 18.21 Hz, every 54.93 ms")
    );
    assert_eq!(rate(&channel(2, 1193)), "1.00 kHz");
    let bcd = PitChannelView {
        bcd: true,
        ..channel(2, 0x0000)
    };
    assert_eq!(rate(&bcd), "119.3 Hz", "a BCD zero stands for 10000");

    let gated = PitChannelView {
        gate: false,
        ..channel(3, 1193)
    };
    assert_eq!(rate(&gated), "stopped by its gate");
    assert_eq!(tick(&gated), None);
    assert_eq!(rate(&channel(0, 100)), "once");
    assert_eq!(tick(&channel(0, 100)), None);
    let idle = PitChannelView {
        counting: false,
        ..channel(3, 0x0000)
    };
    assert_eq!((rate(&idle).as_str(), tick(&idle)), (NOTHING, None));
}

/// The reload value is shown as written and as the count it stands for;
/// the state says whether the counter counts, waits for a count, or holds
/// a latched count; the access names the bytes the port moves.
#[test]
fn reload_state_and_access_are_set_out_in_words() {
    assert_eq!(reload(&channel(3, 0x0000)), "0000h = 65536");
    assert_eq!(reload(&channel(2, 0x0012)), "0012h = 18");
    let bcd = PitChannelView {
        bcd: true,
        ..channel(2, 0x1234)
    };
    assert_eq!(reload(&bcd), "1234h = 1234");

    assert_eq!(state(&channel(3, 0x0000)), "counting");
    let idle = PitChannelView {
        counting: false,
        ..channel(3, 0x0000)
    };
    assert_eq!(state(&idle), "waiting for a count");
    let latched = PitChannelView {
        latched: Some(0x0FF0),
        ..channel(2, 0x1000)
    };
    assert_eq!(state(&latched), "latched at 0FF0h");

    assert_eq!(access(AccessMode::Low), "LSB only");
    assert_eq!(access(AccessMode::High), "MSB only");
    assert_eq!(access(AccessMode::LowThenHigh), "LSB, then MSB");
}

/// As the BIOS leaves the timer, channel 0 runs in mode 3 from a reload of
/// zero, which stands for 65536: IRQ0 at 18.21 Hz. The two other channels
/// were never given a count, and channel 2's gate is low.
#[test]
fn after_the_bios_channel_0_ticks_at_18_2_hz() {
    let (runtime, _keys) = waiting_for_a_key();
    let shell = Shell::with_panel(runtime, TITLE);
    let screen = shell.screen();
    assert_eq!(screen.row("Channel"), ["Channel", "0", "1", "2"]);
    assert_eq!(
        screen.row("Wired to"),
        ["Wired to", "IRQ0", "DRAM refresh", "speaker"]
    );
    assert_eq!(
        screen.row("Mode"),
        [
            "Mode",
            "3 square wave",
            "0 terminal count",
            "0 terminal count"
        ]
    );
    assert_eq!(
        screen.row("Reload"),
        ["Reload", "0000h = 65536", "0000h = 65536", "0000h = 65536"]
    );
    assert_eq!(screen.row("Gate"), ["Gate", "high", "high", "low"]);
    assert_eq!(
        screen.row("State"),
        [
            "State",
            "counting",
            "waiting for a count",
            "waiting for a count"
        ]
    );
    assert_eq!(screen.row("Rate"), ["Rate", "18.21 Hz", NOTHING, NOTHING]);
    assert!(screen.shows("IRQ0 at 18.21 Hz, every 54.93 ms"));
}

/// A tone programmed on channel 2 as a guest beeps (mode 3, a count of
/// 1193, the gate raised through port 61h) shows its frequency, and the
/// counts run down as timer time passes: a millisecond takes 1193 clocks
/// off channel 0, two at a time in mode 3.
#[test]
fn a_tone_on_channel_2_shows_its_frequency_and_the_counts_run() {
    let (mut runtime, _keys) = waiting_for_a_key();
    for (port, value) in [(0x43, 0xB6), (0x42, 0xA9), (0x42, 0x04), (0x61, 0x4B)] {
        runtime.port_out_byte(port, value);
    }
    let mut shell = Shell::with_panel(runtime, TITLE);
    let screen = shell.screen();
    assert_eq!(screen.row("Gate"), ["Gate", "high", "high", "high"]);
    assert_eq!(
        screen.row("Rate"),
        ["Rate", "18.21 Hz", NOTHING, "1.00 kHz"]
    );
    assert_eq!(screen.row("Count")[1], "0000h");

    shell.target.runtime.credit_pit_time(1_000_000);
    shell.target.runtime.run(1);
    let screen = shell.frame(Vec::new());
    assert_eq!(
        screen.row("Count")[1],
        format!("{:04X}h", 0x10000 - 2 * 1193)
    );
}
