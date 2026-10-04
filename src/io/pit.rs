//! 8253/8254 Programmable Interval Timer
//!
//! Ports:
//!   0x40 - Channel 0 data (system timer)
//!   0x41 - Channel 1 data (DRAM refresh, ignored)
//!   0x42 - Channel 2 data (speaker)
//!   0x43 - Mode/command register

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

/// Outcome of `Pit::advance`.
pub struct PitAdvance {
    /// Input clocks actually consumed.
    pub clocks: u32,
    /// Channel 0's output went from low to high on the last clock consumed.
    pub ch0_rising_edge: bool,
}

pub struct Pit {
    channels: [PitChannel; 3],
}

/// Which bytes of the 16-bit count a read or write of the data port moves.
#[derive(Clone, Copy, PartialEq)]
enum AccessMode {
    Low,
    High,
    LowThenHigh,
}

#[derive(Clone, Copy, PartialEq)]
enum RWState {
    LoByte,
    HiByte,
}

struct PitChannel {
    // Configuration
    mode: u8,
    access: AccessMode,
    bcd: bool,

    // Counter state
    count_register: u16,   // Value written by software (reload value)
    counting_element: u16, // The actual decrementing counter
    odd_count: bool,       // Mode 3 is running an odd count (see tick_mode3)

    // Output
    output: bool,           // Output pin state
    output_latch: u16,      // Latched value for reads
    count_is_latched: bool, // Latch frozen?

    // Gate
    gate: bool, // Gate input (true=high, false=low)

    // State
    enabled: bool,
    newly_loaded: bool, // Just programmed, counter=0 means max
    armed: bool,        // For one-shot modes (4, 5)

    // Read/write sequencing
    write_state: RWState,
    read_state: RWState,
}

impl PitChannel {
    fn new(gate_default: bool) -> Self {
        Self {
            mode: 0,
            access: AccessMode::LowThenHigh,
            bcd: false,

            count_register: 0,
            counting_element: 0,
            odd_count: false,

            output: false,
            output_latch: 0,
            count_is_latched: false,

            gate: gate_default,

            enabled: false,
            newly_loaded: false,
            armed: false,

            write_state: RWState::LoByte,
            read_state: RWState::LoByte,
        }
    }

    /// Convert a BCD-encoded u16 to its binary equivalent.
    fn bcd_to_binary(bcd: u16) -> u32 {
        let d0 = (bcd & 0xF) as u32;
        let d1 = ((bcd >> 4) & 0xF) as u32;
        let d2 = ((bcd >> 8) & 0xF) as u32;
        let d3 = ((bcd >> 12) & 0xF) as u32;
        d3 * 1000 + d2 * 100 + d1 * 10 + d0
    }

    /// Convert a binary value to BCD-encoded u16.
    fn binary_to_bcd(val: u32) -> u16 {
        let val = val % 10000;
        let d3 = val / 1000;
        let d2 = (val % 1000) / 100;
        let d1 = (val % 100) / 10;
        let d0 = val % 10;
        ((d3 << 12) | (d2 << 8) | (d1 << 4) | d0) as u16
    }

    /// Get the effective count value, handling BCD and the "0 means max" rule.
    fn effective_count(&self) -> u32 {
        let max_count = if self.bcd { 10000u32 } else { 0x10000u32 };
        if self.bcd {
            let bin = Self::bcd_to_binary(self.counting_element);
            if self.newly_loaded && bin == 0 {
                max_count
            } else {
                bin
            }
        } else if self.newly_loaded && self.counting_element == 0 {
            max_count
        } else {
            self.counting_element as u32
        }
    }

    /// Get the effective reload value, handling BCD and the "0 means max" rule.
    fn effective_reload(&self) -> u32 {
        let max_count = if self.bcd { 10000u32 } else { 0x10000u32 };
        if self.bcd {
            let r = Self::bcd_to_binary(self.count_register);
            if r == 0 { max_count } else { r }
        } else if self.count_register == 0 {
            max_count
        } else {
            self.count_register as u32
        }
    }

    /// Store a binary value (below the maximum count) back into
    /// counting_element, handling BCD. A stored zero means zero, not the
    /// maximum count.
    fn store_count(&mut self, val: u32) {
        self.counting_element = if self.bcd {
            Self::binary_to_bcd(val)
        } else {
            val as u16
        };
        self.newly_loaded = false;
    }

    /// Count down by one the way modes 0, 1, 4 and 5 do: the counter does
    /// not stop at zero but wraps to the highest count (FFFFh, or 9999 in
    /// BCD) and keeps running. Returns true on the clock that takes it to
    /// zero.
    fn count_down(&mut self) -> bool {
        let max_count = if self.bcd { 10000u32 } else { 0x10000u32 };
        let count = match self.effective_count() {
            0 => max_count,
            count => count,
        };
        self.store_count(count - 1);
        count == 1
    }

    /// Reload counting_element from count_register.
    fn reload(&mut self) {
        self.counting_element = self.count_register;
        self.newly_loaded = true;

        // Mode 3 counts an odd value N as N-1, in steps of two like an even
        // one, and makes up for it with a longer high half.
        self.odd_count = false;
        if self.mode == 3 {
            let count = self.effective_reload();
            if count % 2 == 1 && count > 1 {
                self.odd_count = true;
                self.store_count(count - 1);
            }
        }
    }

    /// Write a data byte to this channel's data port.
    fn write_data(&mut self, value: u8) {
        match self.access {
            AccessMode::Low => {
                self.count_register = (self.count_register & 0xFF00) | value as u16;
                self.finish_write();
            }
            AccessMode::High => {
                self.count_register = (self.count_register & 0x00FF) | ((value as u16) << 8);
                self.finish_write();
            }
            AccessMode::LowThenHigh => {
                match self.write_state {
                    RWState::LoByte => {
                        self.count_register = (self.count_register & 0xFF00) | value as u16;
                        self.write_state = RWState::HiByte;
                        if self.mode == 0 {
                            // The first byte of a new count stops a mode 0
                            // counter and takes its output low; the second
                            // byte starts it again.
                            self.enabled = false;
                            self.output = false;
                        }
                    }
                    RWState::HiByte => {
                        self.count_register =
                            (self.count_register & 0x00FF) | ((value as u16) << 8);
                        self.write_state = RWState::LoByte;
                        self.finish_write();
                    }
                }
            }
        }
    }

    /// Called when a full count value has been written.
    fn finish_write(&mut self) {
        match self.mode {
            2 | 3 if self.enabled => {
                // A running rate or square wave generator finishes the
                // period (or half-cycle) in progress; the new count is
                // picked up from the count register at the next reload.
            }
            _ => {
                self.reload();
                self.enabled = true;
                // A new count restarts mode 0 with the output low, whether
                // or not the previous count had run out.
                if self.mode == 0 {
                    self.output = false;
                }
                // For modes 4/5, arm the strobe
                if self.mode == 4 || self.mode == 5 {
                    self.armed = true;
                }
            }
        }
    }

    /// Read a data byte from this channel's data port.
    fn read_data(&mut self) -> u8 {
        let value = if self.count_is_latched {
            self.output_latch
        } else {
            self.counting_element
        };

        match self.access {
            AccessMode::Low => {
                if self.count_is_latched {
                    self.count_is_latched = false;
                }
                value as u8
            }
            AccessMode::High => {
                if self.count_is_latched {
                    self.count_is_latched = false;
                }
                (value >> 8) as u8
            }
            AccessMode::LowThenHigh => match self.read_state {
                RWState::LoByte => {
                    self.read_state = RWState::HiByte;
                    value as u8
                }
                RWState::HiByte => {
                    self.read_state = RWState::LoByte;
                    if self.count_is_latched {
                        self.count_is_latched = false;
                    }
                    (value >> 8) as u8
                }
            },
        }
    }

    /// Process a single PIT clock tick. Returns whether the output went from
    /// low to high.
    fn tick_one(&mut self) -> bool {
        if !self.enabled {
            return false;
        }

        let old_output = self.output;

        match self.mode {
            0 => self.tick_mode0(),
            1 => self.tick_mode1(),
            2 => self.tick_mode2(),
            3 => self.tick_mode3(),
            4 => self.tick_mode4(),
            5 => self.tick_mode5(),
            _ => {}
        }

        // Update output latch if not frozen by a latch command
        if !self.count_is_latched {
            self.output_latch = self.counting_element;
        }

        self.output && !old_output
    }

    /// Mode 0: Interrupt on Terminal Count
    /// - Gate LOW: suspend counting (don't decrement)
    /// - Gate HIGH: decrement counting_element
    /// - When counting_element reaches 0: set output = true
    /// - Output starts LOW after programming, goes HIGH at terminal count, stays HIGH
    /// - The counter keeps running after terminal count
    fn tick_mode0(&mut self) {
        if !self.gate {
            return; // Gate low: suspend counting
        }

        if self.count_down() {
            self.output = true;
        }
    }

    /// Mode 1: Hardware Retriggerable One-Shot
    /// - Gate rising edge: reload counting_element from count_register, output = false, start
    /// - Decrement each tick
    /// - When reaches 0: output = true
    /// - Output stays HIGH until next gate rising edge
    /// - The counter keeps running after terminal count
    fn tick_mode1(&mut self) {
        // Gate rising edge is handled in set_gate(), which reloads and sets output = false.
        // Here we just decrement.
        if self.count_down() {
            self.output = true;
        }
    }

    /// Mode 2: Rate Generator
    /// - Gate LOW: suspend counting, force output HIGH
    /// - Gate HIGH: decrement counting_element
    /// - When counting_element reaches 1: set output = false (LOW for one cycle)
    /// - Next tick: reload from count_register, set output = true
    fn tick_mode2(&mut self) {
        if !self.gate {
            self.output = true;
            return;
        }

        let count = self.effective_count();

        // If output is currently low (the one-cycle strobe), reload and go high
        if !self.output {
            self.reload();
            self.output = true;
            return;
        }

        if count <= 2 {
            // Reaching 1 on next decrement: go low for one cycle
            self.counting_element = 1;
            self.newly_loaded = false;
            self.output = false;
        } else {
            self.store_count(count - 1);
        }
    }

    /// Mode 3: Square Wave Generator
    /// - Gate LOW: suspend counting, force output HIGH
    /// - Gate HIGH: decrement counting_element by 2
    /// - When counting_element reaches 0: toggle output, reload from count_register
    /// - Odd count N: the counter holds N-1; in the high half it runs all
    ///   the way down to 0 and the output falls one clock later, so the
    ///   output is high for (N+1)/2 clocks and low for (N-1)/2
    fn tick_mode3(&mut self) {
        if !self.gate {
            self.output = true;
            return;
        }

        let count = self.effective_count();

        if self.odd_count && self.output {
            if count == 0 {
                self.output = false;
                self.reload();
            } else {
                self.store_count(count - 2);
            }
        } else if count <= 2 {
            // Terminal count: toggle output and reload
            self.output = !self.output;
            self.reload();
        } else {
            self.store_count(count - 2);
        }
    }

    /// Mode 4: Software Triggered Strobe
    /// - Gate LOW: suspend counting
    /// - Gate HIGH: decrement
    /// - When reaches 0: set output = false for ONE tick, then true
    /// - One-shot, does not auto-reload
    fn tick_mode4(&mut self) {
        if !self.gate {
            return;
        }

        self.tick_strobe();
    }

    /// Mode 5: Hardware Triggered Strobe
    /// - Gate rising edge: reload and start counting (handled in set_gate)
    /// - Decrement each tick
    /// - When reaches 0: set output = false for ONE tick, then true
    fn tick_mode5(&mut self) {
        self.tick_strobe();
    }

    /// Modes 4 and 5: the output strobes low for the one clock on which an
    /// armed count runs out. The counter keeps running afterwards, but only
    /// a new count or trigger arms another strobe.
    fn tick_strobe(&mut self) {
        if !self.output {
            self.output = true;
            if self.armed {
                // Low since a mode 5 trigger (set_gate), not from a strobe:
                // the count starts on the next clock.
                return;
            }
        }

        if self.count_down() && self.armed {
            self.output = false; // Strobe low for one tick
            self.armed = false;
        }
    }
}

impl Pit {
    pub fn new() -> Self {
        Self {
            // Channel 0 & 1 have gate tied high; channel 2 gate is controlled by port 0x61
            channels: [
                PitChannel::new(true),
                PitChannel::new(true),
                PitChannel::new(false),
            ],
        }
    }

    /// Advances all channels by up to `max_clocks` input clocks (1,193,182 Hz),
    /// one clock at a time, stopping right after a rising edge of channel 0's
    /// output so the caller can raise IRQ0 before more timer time goes by.
    pub fn advance(&mut self, max_clocks: u32) -> PitAdvance {
        let mut clocks = 0;
        while clocks < max_clocks {
            let ch0_rising_edge = self.channels[0].tick_one();
            self.channels[1].tick_one();
            self.channels[2].tick_one();
            clocks += 1;
            if ch0_rising_edge {
                return PitAdvance {
                    clocks,
                    ch0_rising_edge: true,
                };
            }
        }
        PitAdvance {
            clocks,
            ch0_rising_edge: false,
        }
    }

    /// Get channel 2 output state (used by port 0x61 bit 5).
    pub fn channel2_output(&self) -> bool {
        self.channels[2].output
    }

    /// Set the gate input for a channel.
    /// Gate rising edges trigger special behavior in modes 1, 2, 3, and 5.
    pub fn set_gate(&mut self, channel: usize, state: bool) {
        if channel >= 3 {
            return;
        }
        let ch = &mut self.channels[channel];
        let rising_edge = state && !ch.gate;
        ch.gate = state;

        if rising_edge {
            match ch.mode {
                1 | 5 => {
                    // Reload counting_element and start counting
                    ch.reload();
                    ch.output = false;
                    ch.armed = true;
                }
                2 | 3 => {
                    // Reload counting_element
                    ch.reload();
                }
                _ => {}
            }
        }
    }
}

impl IoDevice for Pit {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            0x40 => self.channels[0].read_data(),
            0x41 => self.channels[1].read_data(),
            0x42 => self.channels[2].read_data(),
            0x43 => 0x00, // Reading port 0x43 returns 0x00
            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            0x40 => self.channels[0].write_data(value),
            0x41 => self.channels[1].write_data(value),
            0x42 => self.channels[2].write_data(value),
            0x43 => {
                // Mode/command register
                let channel = (value >> 6) & 0x03;
                if channel == 3 {
                    return; // Read-back command (8254 only), ignore on 8253
                }

                let access = (value >> 4) & 0x03;
                let raw_mode = (value >> 1) & 0x07;
                let mode = match raw_mode {
                    6 => 2,
                    7 => 3,
                    m => m,
                };
                let bcd = value & 0x01 != 0;
                let ch = &mut self.channels[channel as usize];

                if access == 0 {
                    // Latch count command: freeze the current counting_element
                    if !ch.count_is_latched {
                        ch.output_latch = ch.counting_element;
                        ch.count_is_latched = true;
                    }
                } else {
                    // Programming a channel
                    ch.mode = mode;
                    ch.bcd = bcd;
                    ch.access = match access {
                        1 => AccessMode::Low,
                        2 => AccessMode::High,
                        3 => AccessMode::LowThenHigh,
                        _ => AccessMode::LowThenHigh,
                    };
                    // Programming a channel clears its count register, so a
                    // one-byte count leaves the other byte zero.
                    ch.count_register = 0;
                    ch.write_state = RWState::LoByte;
                    ch.read_state = RWState::LoByte;
                    ch.count_is_latched = false;
                    ch.armed = false;

                    // Initial output state: low in mode 0, high in the others
                    ch.output = mode != 0;

                    ch.enabled = false;
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
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

    /// Regression: a control word left the previous count in place, so a
    /// channel reprogrammed for one-byte counts kept the other byte of the
    /// old value.
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

    /// Regression: in mode 0 a new count reloaded the counter but left the
    /// output high after the first terminal count, so a one-shot timer
    /// re-armed by writing only a count never produced another edge.
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

    /// Regression: the first byte of a two-byte count had no effect in
    /// mode 0. It stops the counter and takes the output low until the
    /// second byte is written.
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

    /// Regression: an odd count N in mode 3 gave a period of N+1 clocks.
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

    /// Regression: in modes 0, 1, 4 and 5 the counter stopped at zero. It
    /// wraps to the highest count and keeps running.
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
}
