use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

/// Per-channel DMA state.
#[derive(Clone, Copy, Default)]
struct DmaChannel {
    base_address: u16,
    base_count: u16,
    current_address: u16,
    current_count: u16,
}

/// Minimal Intel 8237A DMA controller stub.
///
/// The IBM PC uses this for memory-to-device transfers (floppy, etc.).
/// Since the emulator traps disk I/O at the BIOS level, no actual DMA
/// transfers are performed -- we just store register state so software
/// that probes the DMA ports gets sensible values back.
pub struct DmaController {
    channels: [DmaChannel; 4],
    status: u8,
    command: u8,
    mask: u8,
    mode: [u8; 4],
    flip_flop: bool,
    temp: u8,
}

impl DmaController {
    pub fn new() -> Self {
        Self {
            channels: [DmaChannel::default(); 4],
            status: 0x0F, // all channels TC reached (safe default)
            command: 0,
            mask: 0x0F, // all channels masked on reset
            mode: [0; 4],
            flip_flop: false,
            temp: 0,
        }
    }

    /// Read one byte from a 16-bit register, toggling the flip-flop.
    fn read_ff(&mut self, value: u16) -> u8 {
        let byte = if !self.flip_flop {
            value as u8 // low byte
        } else {
            (value >> 8) as u8 // high byte
        };
        self.flip_flop = !self.flip_flop;
        byte
    }

}

impl IoDevice for DmaController {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            // Channel address registers (current)
            0x00 => self.read_ff(self.channels[0].current_address),
            0x02 => self.read_ff(self.channels[1].current_address),
            0x04 => self.read_ff(self.channels[2].current_address),
            0x06 => self.read_ff(self.channels[3].current_address),

            // Channel count registers (current)
            0x01 => self.read_ff(self.channels[0].current_count),
            0x03 => self.read_ff(self.channels[1].current_count),
            0x05 => self.read_ff(self.channels[2].current_count),
            0x07 => self.read_ff(self.channels[3].current_count),

            // Status register
            0x08 => {
                let s = self.status;
                self.status &= 0xF0; // reading clears TC bits (low nibble)
                s
            }

            // Temp register (master clear read)
            0x0D => self.temp,

            other => {
                debug!("DMA: unhandled read from port {:04X}", other);
                0xFF
            }
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            // Channel base+current address registers
            0x00 => {
                let ch = &mut self.channels[0];
                self.flip_flop = { let ff = self.flip_flop; Self::write_ff_static(ff, &mut ch.base_address, value); Self::write_ff_static(ff, &mut ch.current_address, value); !ff };
            }
            0x02 => {
                let ff = self.flip_flop;
                let ch = &mut self.channels[1];
                Self::write_ff_static(ff, &mut ch.base_address, value);
                Self::write_ff_static(ff, &mut ch.current_address, value);
                self.flip_flop = !ff;
            }
            0x04 => {
                let ff = self.flip_flop;
                let ch = &mut self.channels[2];
                Self::write_ff_static(ff, &mut ch.base_address, value);
                Self::write_ff_static(ff, &mut ch.current_address, value);
                self.flip_flop = !ff;
            }
            0x06 => {
                let ff = self.flip_flop;
                let ch = &mut self.channels[3];
                Self::write_ff_static(ff, &mut ch.base_address, value);
                Self::write_ff_static(ff, &mut ch.current_address, value);
                self.flip_flop = !ff;
            }

            // Channel base+current count registers
            0x01 => {
                let ff = self.flip_flop;
                let ch = &mut self.channels[0];
                Self::write_ff_static(ff, &mut ch.base_count, value);
                Self::write_ff_static(ff, &mut ch.current_count, value);
                self.flip_flop = !ff;
            }
            0x03 => {
                let ff = self.flip_flop;
                let ch = &mut self.channels[1];
                Self::write_ff_static(ff, &mut ch.base_count, value);
                Self::write_ff_static(ff, &mut ch.current_count, value);
                self.flip_flop = !ff;
            }
            0x05 => {
                let ff = self.flip_flop;
                let ch = &mut self.channels[2];
                Self::write_ff_static(ff, &mut ch.base_count, value);
                Self::write_ff_static(ff, &mut ch.current_count, value);
                self.flip_flop = !ff;
            }
            0x07 => {
                let ff = self.flip_flop;
                let ch = &mut self.channels[3];
                Self::write_ff_static(ff, &mut ch.base_count, value);
                Self::write_ff_static(ff, &mut ch.current_count, value);
                self.flip_flop = !ff;
            }

            // Command register
            0x08 => {
                self.command = value;
            }

            // Request register (write only)
            0x09 => {
                // Software DMA request -- ignored in stub
                debug!("DMA: request register write {:02X}", value);
            }

            // Single channel mask
            0x0A => {
                let ch = (value & 0x03) as usize;
                if value & 0x04 != 0 {
                    self.mask |= 1 << ch;
                } else {
                    self.mask &= !(1 << ch);
                }
            }

            // Mode register
            0x0B => {
                let ch = (value & 0x03) as usize;
                self.mode[ch] = value;
            }

            // Clear flip-flop
            0x0C => {
                self.flip_flop = false;
            }

            // Master clear (reset)
            0x0D => {
                self.command = 0;
                self.status = 0;
                self.mask = 0x0F;
                self.flip_flop = false;
                self.temp = 0;
            }

            // Clear mask register (unmask all channels)
            0x0E => {
                self.mask = 0x00;
            }

            // Write all mask bits
            0x0F => {
                self.mask = value & 0x0F;
            }

            other => {
                debug!("DMA: unhandled write to port {:04X} = {:02X}", other, value);
            }
        }
    }

    fn name(&self) -> &'static str {
        "8237A DMA Controller"
    }
}

impl DmaController {
    /// Static helper for flip-flop writes (avoids borrow issues with &mut self).
    fn write_ff_static(flip_flop: bool, reg: &mut u16, byte: u8) {
        if !flip_flop {
            *reg = (*reg & 0xFF00) | byte as u16;
        } else {
            *reg = (*reg & 0x00FF) | ((byte as u16) << 8);
        }
    }
}

// ---------- DMA Page Registers (separate device, non-contiguous ports) ----------

/// DMA page registers at ports 0x81-0x87.
///
/// These set the upper address bits (A16-A23) for each DMA channel.
/// Port mapping:
///   0x81 -> Channel 2
///   0x82 -> Channel 3
///   0x83 -> Channel 1
///   0x87 -> Channel 0
pub struct DmaPageRegisters {
    pages: [u8; 4], // indexed by channel number 0-3
}

impl DmaPageRegisters {
    pub fn new() -> Self {
        Self { pages: [0; 4] }
    }

    fn port_to_channel(port: u16) -> Option<usize> {
        match port {
            0x87 => Some(0),
            0x83 => Some(1),
            0x81 => Some(2),
            0x82 => Some(3),
            _ => None,
        }
    }
}

impl IoDevice for DmaPageRegisters {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match Self::port_to_channel(port) {
            Some(ch) => self.pages[ch],
            None => {
                debug!("DMA Page: unhandled read from port {:04X}", port);
                0xFF
            }
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match Self::port_to_channel(port) {
            Some(ch) => self.pages[ch] = value,
            None => {
                debug!("DMA Page: unhandled write to port {:04X} = {:02X}", port, value);
            }
        }
    }

    fn name(&self) -> &'static str {
        "DMA Page Registers"
    }
}
