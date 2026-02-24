use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

/// 8253/8254 Programmable Interval Timer
///
/// Ports:
///   0x40 - Channel 0 data (system timer)
///   0x41 - Channel 1 data (DRAM refresh, ignored)
///   0x42 - Channel 2 data (speaker)
///   0x43 - Mode/command register
pub struct Pit {
    channels: [PitChannel; 3],
}

struct PitChannel {
    reload_value: u16,      // divisor programmed by software
    counter: u16,           // current countdown value
    mode: u8,               // operating mode (0-5)
    access: AccessMode,     // how the counter is read/written
    latch_value: Option<u16>, // latched counter for reading
    write_state: RWState,   // tracks lobyte/hibyte writes
    read_state: RWState,    // tracks lobyte/hibyte reads
    bcd: bool,              // BCD counting mode (wraps at 9999)
    output: bool,           // output pin state
    enabled: bool,          // has been programmed at least once
    newly_loaded: bool,     // counter was just loaded (0 means 65536)
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

impl PitChannel {
    fn new() -> Self {
        Self {
            reload_value: 0,
            counter: 0,
            mode: 0,
            bcd: false,
            access: AccessMode::LoHiByte,
            latch_value: None,
            write_state: RWState::LoByte,
            read_state: RWState::LoByte,
            output: false,
            enabled: false,
            newly_loaded: false,
        }
    }

    fn write_data(&mut self, value: u8) {
        match self.access {
            AccessMode::LoByte => {
                self.reload_value = (self.reload_value & 0xFF00) | value as u16;
                self.load_counter();
            }
            AccessMode::HiByte => {
                self.reload_value = (self.reload_value & 0x00FF) | ((value as u16) << 8);
                self.load_counter();
            }
            AccessMode::LoHiByte => {
                match self.write_state {
                    RWState::LoByte => {
                        self.reload_value = (self.reload_value & 0xFF00) | value as u16;
                        self.write_state = RWState::HiByte;
                    }
                    RWState::HiByte => {
                        self.reload_value = (self.reload_value & 0x00FF) | ((value as u16) << 8);
                        self.write_state = RWState::LoByte;
                        self.load_counter();
                    }
                }
            }
            AccessMode::LatchCount => {}
        }
    }

    fn read_data(&mut self) -> u8 {
        let value = self.latch_value.unwrap_or(self.counter);

        match self.access {
            AccessMode::LoByte => {
                self.latch_value = None;
                value as u8
            }
            AccessMode::HiByte => {
                self.latch_value = None;
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
                        self.latch_value = None;
                        (value >> 8) as u8
                    }
                }
            }
            AccessMode::LatchCount => {
                self.latch_value = None;
                value as u8
            }
        }
    }

    fn load_counter(&mut self) {
        self.counter = self.reload_value;
        self.newly_loaded = true;
        self.enabled = true;
        self.output = match self.mode {
            0 => false,
            _ => true,
        };
    }


    fn bcd_to_binary(bcd: u16) -> u32 {
        let d0 = (bcd & 0xF) as u32;
        let d1 = ((bcd >> 4) & 0xF) as u32;
        let d2 = ((bcd >> 8) & 0xF) as u32;
        let d3 = ((bcd >> 12) & 0xF) as u32;
        d3 * 1000 + d2 * 100 + d1 * 10 + d0
    }

    fn binary_to_bcd(val: u32) -> u16 {
        let val = val % 10000;
        let d3 = val / 1000;
        let d2 = (val % 1000) / 100;
        let d1 = (val % 100) / 10;
        let d0 = val % 10;
        ((d3 << 12) | (d2 << 8) | (d1 << 4) | d0) as u16
    }

    /// Tick the channel by `count` PIT clock cycles.
    /// Returns true if output transitioned (IRQ should fire).
    fn tick(&mut self, count: u16) -> bool {
        if !self.enabled {
            return false;
        }

        // A reload value of 0 means 65536 (binary) or 10000 (BCD) on the real PIT.
        // When freshly loaded, treat counter=0 as max so it doesn't fire immediately.
        let max_count = if self.bcd { 10000u32 } else { 0x10000u32 };
        let eff = if self.bcd {
            let bin = Self::bcd_to_binary(self.counter);
            if self.newly_loaded && bin == 0 { max_count } else { bin }
        } else if self.newly_loaded && self.counter == 0 {
            max_count
        } else {
            self.counter as u32
        };
        self.newly_loaded = false;

        let mut fired = false;

        match self.mode {
            // Mode 0: Interrupt on terminal count
            // Mode 1: Hardware retriggerable one-shot (same behavior in emulation)
            // Mode 4/5: Software/hardware triggered strobe (same countdown)
            0 | 1 | 4 | 5 => {
                if eff <= count as u32 {
                    self.counter = 0;
                    if !self.output {
                        self.output = true;
                        fired = true;
                    }
                } else {
                    let result = eff - count as u32;
                    self.counter = if self.bcd { Self::binary_to_bcd(result) } else { result as u16 };
                }
            }
            // Mode 2: Rate generator (periodic, used for system timer)
            2 => {
                if eff <= count as u32 {
                    fired = true;
                    let reload = if self.bcd {
                        let r = Self::bcd_to_binary(self.reload_value);
                        if r == 0 { max_count } else { r }
                    } else if self.reload_value == 0 { max_count } else { self.reload_value as u32 };
                    let remainder = count as u32 - eff;
                    let result = reload - (remainder % reload);
                    self.counter = if self.bcd { Self::binary_to_bcd(result) } else { result as u16 };
                    if result == max_count {
                        self.newly_loaded = true;
                        self.counter = 0;
                    }
                } else {
                    let result = eff - count as u32;
                    self.counter = if self.bcd { Self::binary_to_bcd(result) } else { result as u16 };
                }
            }
            // Mode 3: Square wave (used by BIOS for system timer)
            3 => {
                if eff <= count as u32 {
                    fired = true;
                    let reload = if self.bcd {
                        let r = Self::bcd_to_binary(self.reload_value);
                        if r == 0 { max_count } else { r }
                    } else if self.reload_value == 0 { max_count } else { self.reload_value as u32 };
                    let remainder = count as u32 - eff;
                    let result = reload - (remainder % reload);
                    self.counter = if self.bcd { Self::binary_to_bcd(result) } else { result as u16 };
                    self.output = !self.output;
                    if result == max_count {
                        self.newly_loaded = true;
                        self.counter = 0;
                    }
                } else {
                    let result = eff - count as u32;
                    self.counter = if self.bcd { Self::binary_to_bcd(result) } else { result as u16 };
                }
            }
            _ => {
                if eff <= count as u32 {
                    self.counter = 0;
                } else {
                    let result = eff - count as u32;
                    self.counter = if self.bcd { Self::binary_to_bcd(result) } else { result as u16 };
                }
            }
        }

        fired
    }
}

impl Pit {
    pub fn new() -> Self {
        Self {
            channels: [PitChannel::new(), PitChannel::new(), PitChannel::new()],
        }
    }

    /// Call this periodically from the main loop.
    /// `ticks` = number of PIT clock cycles elapsed.
    ///
    /// The PIT runs at 1,193,182 Hz. If your CPU runs at ~4.77 MHz,
    /// that's roughly 1 PIT tick per 4 CPU cycles.
    ///
    /// Returns true if channel 0 fired (raise IRQ0).
    pub fn tick(&mut self, ticks: u16) -> bool {
        let ch0_fired = self.channels[0].tick(ticks);
        self.channels[1].tick(ticks);
        self.channels[2].tick(ticks);
        ch0_fired
    }

    /// Get channel 2 output state (used by port 0x61 bit 5).
    pub fn channel2_output(&self) -> bool {
        self.channels[2].output
    }
}

impl IoDevice for Pit {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            0x40 => self.channels[0].read_data(),
            0x41 => self.channels[1].read_data(),
            0x42 => self.channels[2].read_data(),
            0x43 => 0xFF, // write-only register
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
                    return; // read-back command (8254 only), ignore
                }

                let access = (value >> 4) & 0x03;
                let mode = (value >> 1) & 0x07;
                let bcd = value & 0x01 != 0;
                let ch = &mut self.channels[channel as usize];

                if access == 0 {
                    // Latch count command
                    ch.latch_value = Some(ch.counter);
                } else {
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
                    ch.output = false;
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