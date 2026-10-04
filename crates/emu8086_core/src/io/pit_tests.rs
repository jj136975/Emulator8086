use super::*;
use crate::vm::runtime::Runtime;

/// A timer and the CPU its port handlers are handed.
struct Bench {
    pit: Pit,
    rt: Runtime,
}

impl Bench {
    fn new() -> Self {
        Self {
            pit: Pit::new(),
            rt: Runtime::new_test(),
        }
    }

    fn out(&mut self, port: u16, value: u8) {
        self.pit.port_out_byte(port, value, &mut self.rt.cpu);
    }

    fn inp(&mut self, port: u16) -> u8 {
        self.pit.port_in_byte(port, &mut self.rt.cpu)
    }

    /// Writes `control`, then `count` low byte first, to the channel the
    /// control word selects.
    fn program(&mut self, control: u8, count: u16) {
        let port = 0x40 + (control >> 6) as u16;
        self.out(0x43, control);
        self.out(port, count as u8);
        self.out(port, (count >> 8) as u8);
    }

    /// Latches `channel` and reads its count back, low byte first.
    fn latched_count(&mut self, channel: u8) -> u16 {
        let port = 0x40 + channel as u16;
        self.out(0x43, channel << 6);
        let lo = self.inp(port) as u16;
        let hi = self.inp(port) as u16;
        lo | (hi << 8)
    }

    /// Runs the timer for `clocks` and returns the clocks (counted from 1)
    /// on which channel 0's output rose.
    fn rising_edges(&mut self, clocks: u32) -> Vec<u32> {
        let mut edges = Vec::new();
        let mut elapsed = 0;
        while elapsed < clocks {
            let advance = self.pit.advance(clocks - elapsed);
            elapsed += advance.clocks;
            if advance.ch0_rising_edge {
                edges.push(elapsed);
            }
        }
        edges
    }
}

#[test]
fn control_word_sets_the_initial_output_level() {
    let mut bench = Bench::new();
    bench.out(0x43, 0xB0); // channel 2, mode 0
    assert!(!bench.pit.channel2_output(), "mode 0 starts low");
    for mode in 1..=5u8 {
        bench.out(0x43, 0xB0 | (mode << 1));
        assert!(bench.pit.channel2_output(), "mode {mode} starts high");
    }
}

#[test]
fn mode_0_output_rises_at_terminal_count() {
    let mut bench = Bench::new();
    bench.program(0x30, 100);
    assert_eq!(bench.rising_edges(150), [100]);
}

#[test]
fn mode_2_period_equals_the_count() {
    for (count, period) in [(2u16, 2u32), (3, 3), (1193, 1193), (0, 65536)] {
        let mut bench = Bench::new();
        bench.program(0x34, count);
        assert_eq!(
            bench.rising_edges(3 * period),
            [period, 2 * period, 3 * period],
            "count {count}"
        );
    }
}

#[test]
fn mode_3_period_equals_an_even_count() {
    for (count, period) in [(4u16, 4u32), (1192, 1192), (0, 65536)] {
        let mut bench = Bench::new();
        bench.program(0x36, count);
        assert_eq!(
            bench.rising_edges(3 * period),
            [period, 2 * period, 3 * period],
            "count {count}"
        );
    }
}

#[test]
fn mode_4_strobes_low_for_one_clock_at_terminal_count() {
    let mut bench = Bench::new();
    bench.program(0x38, 5);
    // The output falls on the fifth clock and rises again on the sixth.
    assert_eq!(bench.rising_edges(100), [6]);
}

#[test]
fn count_written_while_running_takes_effect_at_the_next_reload() {
    // Mode 2 finishes the period in progress with the old count.
    let mut bench = Bench::new();
    bench.program(0x34, 10);
    bench.pit.advance(3);
    bench.out(0x40, 20);
    bench.out(0x40, 0);
    assert_eq!(bench.rising_edges(60), [7, 27, 47]);

    // Mode 3 finishes the half-cycle in progress with the old count.
    let mut bench = Bench::new();
    bench.program(0x36, 10);
    bench.pit.advance(3);
    bench.out(0x40, 20);
    bench.out(0x40, 0);
    assert_eq!(bench.rising_edges(60), [12, 32, 52]);
}

#[test]
fn latch_command_holds_the_count_until_both_bytes_are_read() {
    let mut bench = Bench::new();
    bench.program(0x34, 0x1000);
    bench.pit.advance(0x10);
    bench.out(0x43, 0x00); // latch channel 0 at 0FF0h
    bench.pit.advance(0x10);
    bench.out(0x43, 0x00); // a second latch before the read is ignored
    let lo = bench.inp(0x40) as u16;
    bench.pit.advance(0x100);
    let hi = bench.inp(0x40) as u16;
    assert_eq!(lo | (hi << 8), 0x0FF0);
    assert_eq!(
        bench.latched_count(0),
        0x0EE0,
        "the next latch sees the running count"
    );
}

#[test]
fn bcd_mode_counts_in_decimal() {
    let mut bench = Bench::new();
    bench.program(0x35, 0x0100); // mode 2, BCD, count 100
    bench.pit.advance(1);
    assert_eq!(bench.latched_count(0), 0x0099);
    bench.pit.advance(9);
    assert_eq!(bench.latched_count(0), 0x0090);
    assert_eq!(bench.rising_edges(90), [90]);
}

#[test]
fn channel_2_gate_suspends_counting_and_restarts_it() {
    let mut bench = Bench::new();
    bench.program(0xB6, 16); // channel 2, mode 3; its gate starts low
    bench.pit.advance(40);
    assert_eq!(
        bench.latched_count(2),
        16,
        "no counting while the gate is low"
    );
    assert!(
        bench.pit.channel2_output(),
        "output is held high while the gate is low"
    );

    bench.pit.set_gate(2, true);
    let mut levels = String::new();
    for _ in 0..24 {
        bench.pit.advance(1);
        levels.push(if bench.pit.channel2_output() {
            'H'
        } else {
            'L'
        });
    }
    assert_eq!(levels, "HHHHHHHLLLLLLLLHHHHHHHHL");

    bench.pit.set_gate(2, false);
    bench.pit.advance(1);
    assert!(bench.pit.channel2_output());
}

/// A control word clears the count register, so a one-byte count leaves
/// the other byte zero.
#[test]
fn control_word_clears_the_count_register() {
    // Low byte only: nothing of an earlier count of 04A9h may remain.
    let mut bench = Bench::new();
    bench.program(0xB6, 0x04A9);
    bench.out(0x43, 0x90); // channel 2, low byte only, mode 0
    bench.out(0x42, 0x40);
    bench.pit.set_gate(2, true);
    bench.pit.advance(0x3F);
    assert!(!bench.pit.channel2_output());
    bench.pit.advance(1);
    assert!(
        bench.pit.channel2_output(),
        "terminal count after 40h clocks"
    );

    // High byte only: the low byte is zero.
    let mut bench = Bench::new();
    bench.program(0x36, 0x1234);
    bench.out(0x43, 0x26); // channel 0, high byte only, mode 3
    bench.out(0x40, 0x80);
    assert_eq!(bench.rising_edges(0x10000), [0x8000, 0x10000]);
}

/// In mode 0 a new count restarts the one-shot with the output low, so
/// writing only a count re-arms it for another edge.
#[test]
fn mode_0_count_rewrite_restarts_the_one_shot() {
    let mut bench = Bench::new();
    bench.program(0x30, 100);
    assert_eq!(bench.rising_edges(150), [100]);

    bench.out(0x40, 50);
    bench.out(0x40, 0);
    assert_eq!(bench.rising_edges(150), [50]);

    // One-byte counts re-arm the same way.
    let mut bench = Bench::new();
    bench.out(0x43, 0x10); // channel 0, low byte only, mode 0
    bench.out(0x40, 30);
    assert_eq!(bench.rising_edges(40), [30]);
    bench.out(0x40, 20);
    assert_eq!(bench.rising_edges(40), [20]);
}

/// In mode 0 the first byte of a two-byte count stops the counter and
/// takes the output low until the second byte is written.
#[test]
fn mode_0_first_count_byte_stops_the_counter() {
    let mut bench = Bench::new();
    bench.program(0xB0, 100); // channel 2
    bench.pit.set_gate(2, true);
    bench.pit.advance(150);
    assert!(bench.pit.channel2_output());

    bench.out(0x42, 10); // first byte only
    assert!(
        !bench.pit.channel2_output(),
        "the output falls with the first byte"
    );
    bench.pit.advance(500);
    assert!(
        !bench.pit.channel2_output(),
        "no counting until the second byte"
    );
    bench.out(0x42, 0);
    bench.pit.advance(9);
    assert!(!bench.pit.channel2_output());
    bench.pit.advance(1);
    assert!(bench.pit.channel2_output());
}

/// In mode 3 an odd count N gives a period of N clocks, as an even one
/// does.
#[test]
fn mode_3_period_equals_an_odd_count() {
    for (control, count, period) in [
        (0x36u8, 3u16, 3u32),
        (0x36, 5, 5),
        (0x36, 1193, 1193),
        (0x36, 0xFFFF, 0xFFFF),
        (0x37, 0x0015, 15), // BCD
    ] {
        let mut bench = Bench::new();
        bench.program(control, count);
        assert_eq!(
            bench.rising_edges(3 * period),
            [period, 2 * period, 3 * period],
            "count {count:04X}h"
        );
    }
}

/// An odd count N is counted as N-1 in steps of two; the output is high
/// for (N+1)/2 clocks and low for (N-1)/2.
#[test]
fn mode_3_odd_count_is_high_one_clock_longer_than_low() {
    let mut bench = Bench::new();
    bench.program(0xB6, 5); // channel 2, where the level can be observed
    bench.pit.set_gate(2, true);
    let mut levels = String::new();
    let mut counts = Vec::new();
    for _ in 0..10 {
        bench.pit.advance(1);
        levels.push(if bench.pit.channel2_output() {
            'H'
        } else {
            'L'
        });
        counts.push(bench.latched_count(2));
    }
    // The count is loaded as soon as the gate rises, without the load
    // clock of the real chip, so the first high half is one clock short.
    assert_eq!(levels, "HHLLHHHLLH");
    assert_eq!(counts, [2, 0, 4, 2, 4, 2, 0, 4, 2, 4]);
}

/// In modes 0, 1, 4 and 5 the counter does not stop at zero: it wraps to
/// the highest count and keeps running.
#[test]
fn counter_keeps_running_after_terminal_count() {
    for control in [0x30u8, 0x32, 0x38, 0x3A] {
        let mut bench = Bench::new();
        bench.program(control, 3);
        let mut counts = Vec::new();
        for _ in 0..6 {
            bench.pit.advance(1);
            counts.push(bench.latched_count(0));
        }
        assert_eq!(
            counts,
            [2, 1, 0, 0xFFFF, 0xFFFE, 0xFFFD],
            "control word {control:02X}h"
        );
    }

    // A BCD counter wraps to 9999.
    let mut bench = Bench::new();
    bench.program(0x31, 2);
    let mut counts = Vec::new();
    for _ in 0..4 {
        bench.pit.advance(1);
        counts.push(bench.latched_count(0));
    }
    assert_eq!(counts, [1, 0, 0x9999, 0x9998]);
}

/// The counter passing through zero again after it has wrapped does
/// not move the output a second time.
#[test]
fn wrapped_counter_does_not_fire_again() {
    // Mode 0: the output stays high.
    let mut bench = Bench::new();
    bench.program(0x30, 10);
    assert_eq!(bench.rising_edges(10 + 2 * 65536), [10]);

    // Mode 4: one strobe per count written.
    let mut bench = Bench::new();
    bench.program(0x38, 10);
    assert_eq!(bench.rising_edges(11 + 2 * 65536), [11]);
}

#[test]
fn mode_5_strobes_after_a_gate_trigger() {
    let mut bench = Bench::new();
    bench.program(0xBA, 3); // channel 2, mode 5
    bench.pit.set_gate(2, true);
    let mut levels = String::new();
    for _ in 0..6 {
        bench.pit.advance(1);
        levels.push(if bench.pit.channel2_output() {
            'H'
        } else {
            'L'
        });
    }
    assert_eq!(levels, "HHHLHH");
}
