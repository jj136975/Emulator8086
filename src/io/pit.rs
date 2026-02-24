use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

/// 8253/8254 Programmable Interval Timer
///
/// Ports:
///   0x40 - Channel 0 data (system timer)
///   0x41 - Channel 1 data (DRAM refresh, ignored)
///   0x42 - Channel 2 data (speaker)
///   0x43 - Mode/command register

pub struct PitTickResult {
    pub ch0_rising_edge: bool,
    pub ch0_falling_edge: bool,
}

pub struct Pit {
    channels: [PitChannel; 3],
}

#[derive(Clone, Copy, PartialEq)]
enum AccessMode {
    LatchCount,
    LoByte,
    HiByte,
    LoHiByte,
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
    count_register: u16,    // Value written by software (reload value)
    counting_element: u16,  // The actual decrementing counter
    reload_pending: bool,   // Deferred reload for modes 2/3

    // Output
    output: bool,           // Output pin state
    output_latch: u16,      // Latched value for reads
    count_is_latched: bool, // Latch frozen?

    // Gate
    gate: bool,             // Gate input (true=high, false=low)
    gate_prev: bool,        // Previous gate state (for edge detection)

    // State
    enabled: bool,
    newly_loaded: bool,     // Just programmed, counter=0 means max
    armed: bool,            // For one-shot modes (4, 5)

    // Read/write sequencing
    write_state: RWState,
    read_state: RWState,
}

impl PitChannel {
    fn new(gate_default: bool) -> Self {
        Self {
            mode: 0,
            access: AccessMode::LoHiByte,
            bcd: false,

            count_register: 0,
            counting_element: 0,
            reload_pending: false,

            output: false,
            output_latch: 0,
            count_is_latched: false,

            gate: gate_default,
            gate_prev: gate_default,

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
            if self.newly_loaded && bin == 0 { max_count } else { bin }
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

    /// Store a binary value back into counting_element, handling BCD.
    fn store_count(&mut self, val: u32) {
        let max_count = if self.bcd { 10000u32 } else { 0x10000u32 };
        if val == max_count || val == 0 {
            self.counting_element = 0;
            self.newly_loaded = true;
        } else {
            self.counting_element = if self.bcd {
                Self::binary_to_bcd(val)
            } else {
                val as u16
            };
            self.newly_loaded = false;
        }
    }

    /// Reload counting_element from count_register.
    fn reload(&mut self) {
        self.counting_element = self.count_register;
        self.newly_loaded = true;
        self.reload_pending = false;
    }

    /// Write a data byte to this channel's data port.
    fn write_data(&mut self, value: u8) {
        match self.access {
            AccessMode::LoByte => {
                self.count_register = (self.count_register & 0xFF00) | value as u16;
                self.finish_write();
            }
            AccessMode::HiByte => {
                self.count_register = (self.count_register & 0x00FF) | ((value as u16) << 8);
                self.finish_write();
            }
            AccessMode::LoHiByte => {
                match self.write_state {
                    RWState::LoByte => {
                        self.count_register = (self.count_register & 0xFF00) | value as u16;
                        self.write_state = RWState::HiByte;
                    }
                    RWState::HiByte => {
                        self.count_register =
                            (self.count_register & 0x00FF) | ((value as u16) << 8);
                        self.write_state = RWState::LoByte;
                        self.finish_write();
                    }
                }
            }
            AccessMode::LatchCount => {}
        }
    }

    /// Called when a full count value has been written.
    fn finish_write(&mut self) {
        match self.mode {
            2 | 3 if self.enabled => {
                // Deferred reload: don't immediately load counting_element.
                // It will be picked up at the next terminal count.
                self.reload_pending = true;
            }
            _ => {
                self.counting_element = self.count_register;
                self.newly_loaded = true;
                self.enabled = true;
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
            AccessMode::LoByte => {
                if self.count_is_latched {
                    self.count_is_latched = false;
                }
                value as u8
            }
            AccessMode::HiByte => {
                if self.count_is_latched {
                    self.count_is_latched = false;
                }
                (value >> 8) as u8
            }
            AccessMode::LoHiByte => {
                match self.read_state {
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
                }
            }
            AccessMode::LatchCount => {
                if self.count_is_latched {
                    self.count_is_latched = false;
                }
                value as u8
            }
        }
    }

    /// Process a single PIT clock tick. Returns (rising_edge, falling_edge) of output.
    fn tick_one(&mut self) -> (bool, bool) {
        if !self.enabled {
            return (false, false);
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

        let rising = self.output && !old_output;
        let falling = !self.output && old_output;
        (rising, falling)
    }

    /// Mode 0: Interrupt on Terminal Count
    /// - Gate LOW: suspend counting (don't decrement)
    /// - Gate HIGH: decrement counting_element
    /// - When counting_element reaches 0: set output = true
    /// - Output starts LOW after programming, goes HIGH at terminal count, stays HIGH
    fn tick_mode0(&mut self) {
        if !self.gate {
            return; // Gate low: suspend counting
        }

        let count = self.effective_count();
        if count <= 1 {
            self.counting_element = 0;
            self.newly_loaded = false;
            self.output = true;
        } else {
            self.store_count(count - 1);
        }
    }

    /// Mode 1: Hardware Retriggerable One-Shot
    /// - Gate rising edge: reload counting_element from count_register, output = false, start
    /// - Decrement each tick
    /// - When reaches 0: output = true
    /// - Output stays HIGH until next gate rising edge
    fn tick_mode1(&mut self) {
        // Gate rising edge is handled in set_gate(), which reloads and sets output = false.
        // Here we just decrement.
        let count = self.effective_count();
        if count <= 1 {
            self.counting_element = 0;
            self.newly_loaded = false;
            self.output = true;
        } else {
            self.store_count(count - 1);
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
            if self.reload_pending {
                self.reload();
            } else {
                self.reload();
            }
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
    fn tick_mode3(&mut self) {
        if !self.gate {
            self.output = true;
            return;
        }

        let count = self.effective_count();

        if count <= 2 {
            // Terminal count: toggle output and reload
            self.output = !self.output;
            if self.reload_pending {
                self.reload();
            } else {
                self.counting_element = self.count_register;
                self.newly_loaded = true;
            }
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

        // If output was strobed low last tick, bring it back high
        if !self.output {
            self.output = true;
            return;
        }

        if !self.armed {
            return;
        }

        let count = self.effective_count();
        if count <= 1 {
            self.counting_element = 0;
            self.newly_loaded = false;
            self.output = false; // Strobe low for one tick
            self.armed = false;
        } else {
            self.store_count(count - 1);
        }
    }

    /// Mode 5: Hardware Triggered Strobe
    /// - Gate rising edge: reload and start counting (handled in set_gate)
    /// - Decrement each tick
    /// - When reaches 0: set output = false for ONE tick, then true
    fn tick_mode5(&mut self) {
        // If output was strobed low last tick, bring it back high
        if !self.output {
            self.output = true;
            return;
        }

        if !self.armed {
            return;
        }

        let count = self.effective_count();
        if count <= 1 {
            self.counting_element = 0;
            self.newly_loaded = false;
            self.output = false; // Strobe low for one tick
            self.armed = false;
        } else {
            self.store_count(count - 1);
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

    /// Tick all channels by `ticks` PIT clock cycles.
    /// Iterates one cycle at a time for accuracy.
    ///
    /// The PIT runs at 1,193,182 Hz. If your CPU runs at ~4.77 MHz,
    /// that's roughly 1 PIT tick per 4 CPU cycles.
    pub fn tick(&mut self, ticks: u16) -> PitTickResult {
        let mut result = PitTickResult {
            ch0_rising_edge: false,
            ch0_falling_edge: false,
        };

        for _ in 0..ticks {
            // Tick channel 0
            let (rising, falling) = self.channels[0].tick_one();
            if rising {
                result.ch0_rising_edge = true;
            }
            if falling {
                result.ch0_falling_edge = true;
            }

            // Tick channels 1 and 2
            self.channels[1].tick_one();
            self.channels[2].tick_one();
        }

        result
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
        ch.gate_prev = ch.gate;
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
                        1 => AccessMode::LoByte,
                        2 => AccessMode::HiByte,
                        3 => AccessMode::LoHiByte,
                        _ => AccessMode::LoHiByte,
                    };
                    ch.write_state = RWState::LoByte;
                    ch.read_state = RWState::LoByte;
                    ch.count_is_latched = false;
                    ch.reload_pending = false;
                    ch.armed = false;

                    // Initial output state per mode
                    ch.output = match mode {
                        0 => false,                  // Mode 0: output starts LOW
                        1 | 2 | 3 | 4 | 5 => true,  // All others: output starts HIGH
                        _ => true,
                    };

                    ch.enabled = false;
                }
            }
            _ => {}
        }
    }

    fn name(&self) -> &'static str {
        "8253 PIT"
    }
}
