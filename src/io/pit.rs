use crate::io::bus::IoDevice;

#[derive(Clone, Copy, PartialEq)]
enum AccessMode {
    LatchCount,
    LoByte,
    HiByte,
    LoByteThenHiByte,
}

struct Channel {
    reload: u16,
    counter: u16,
    mode: u8,
    access: AccessMode,
    write_latch: bool, // true = waiting for hi byte on LoHi write
    read_latch: bool,  // true = waiting for hi byte on LoHi read
    latched_count: u16,
    has_latch: bool,
}

impl Channel {
    fn new() -> Self {
        Self {
            reload: 0,
            counter: 0,
            mode: 0,
            access: AccessMode::LoByteThenHiByte,
            write_latch: false,
            read_latch: false,
            latched_count: 0,
            has_latch: false,
        }
    }
}

pub struct Pit {
    channels: [Channel; 3],
}

impl Pit {
    pub fn new() -> Self {
        Self {
            channels: [Channel::new(), Channel::new(), Channel::new()],
        }
    }
}

impl IoDevice for Pit {
    fn port_in_byte(&mut self, port: u16) -> u8 {
        let ch_idx = (port - 0x40) as usize;
        if ch_idx >= 3 {
            return 0xFF;
        }
        let ch = &mut self.channels[ch_idx];

        if ch.has_latch {
            match ch.access {
                AccessMode::LoByte => {
                    ch.has_latch = false;
                    ch.latched_count as u8
                }
                AccessMode::HiByte => {
                    ch.has_latch = false;
                    (ch.latched_count >> 8) as u8
                }
                AccessMode::LoByteThenHiByte => {
                    if !ch.read_latch {
                        ch.read_latch = true;
                        ch.latched_count as u8
                    } else {
                        ch.read_latch = false;
                        ch.has_latch = false;
                        (ch.latched_count >> 8) as u8
                    }
                }
                AccessMode::LatchCount => {
                    ch.has_latch = false;
                    ch.counter as u8
                }
            }
        } else {
            ch.counter as u8
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8) {
        if port == 0x43 {
            // Control word
            let ch_sel = (value >> 6) & 0x03;
            if ch_sel == 3 {
                return; // Read-back command, ignore
            }
            let ch = &mut self.channels[ch_sel as usize];
            let access = (value >> 4) & 0x03;
            let mode = (value >> 1) & 0x07;

            if access == 0 {
                // Latch count
                ch.latched_count = ch.counter;
                ch.has_latch = true;
                ch.read_latch = false;
            } else {
                ch.access = match access {
                    1 => AccessMode::LoByte,
                    2 => AccessMode::HiByte,
                    3 => AccessMode::LoByteThenHiByte,
                    _ => unreachable!(),
                };
                ch.mode = mode;
                ch.write_latch = false;
            }
            return;
        }

        let ch_idx = (port - 0x40) as usize;
        if ch_idx >= 3 {
            return;
        }
        let ch = &mut self.channels[ch_idx];

        match ch.access {
            AccessMode::LoByte => {
                ch.reload = (ch.reload & 0xFF00) | value as u16;
                ch.counter = ch.reload;
            }
            AccessMode::HiByte => {
                ch.reload = (ch.reload & 0x00FF) | ((value as u16) << 8);
                ch.counter = ch.reload;
            }
            AccessMode::LoByteThenHiByte => {
                if !ch.write_latch {
                    ch.reload = (ch.reload & 0xFF00) | value as u16;
                    ch.write_latch = true;
                } else {
                    ch.reload = (ch.reload & 0x00FF) | ((value as u16) << 8);
                    ch.counter = ch.reload;
                    ch.write_latch = false;
                }
            }
            _ => {}
        }
    }

    fn name(&self) -> &'static str {
        "8253 PIT"
    }
}
