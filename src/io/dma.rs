//! The Intel 8237A DMA controller at ports 00h-0Fh and the page registers
//! at 81h-87h, as registers only. The disks are served at the BIOS's disk
//! trap, so no transfer ever runs: what steers one (the command, request,
//! mask and mode registers) is accepted and forgotten, and what software
//! can read back (each channel's address and count, the status) is kept.
//!
//! The invariant: the chip has one byte-pointer flip-flop, shared by every
//! channel's address and count registers. Each byte read or written
//! through any of them toggles it, and a write to 0Ch or a master clear
//! puts it back on the low byte.

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

/// Per-channel DMA state.
#[derive(Clone, Copy, Default)]
struct DmaChannel {
    current_address: u16,
    current_count: u16,
}

/// Minimal Intel 8237A DMA controller stub: the registers software can
/// read back, and the flip-flop that orders their bytes.
pub struct DmaController {
    channels: [DmaChannel; 4],
    status: u8,
    flip_flop: bool,
}

impl DmaController {
    pub fn new() -> Self {
        Self {
            channels: [DmaChannel::default(); 4],
            status: 0x0F, // all channels TC reached (safe default)
            flip_flop: false,
        }
    }

    /// The 16-bit register behind a channel port: even ports are the
    /// address registers, odd ports the count registers.
    fn channel_register(&mut self, port: u16) -> &mut u16 {
        let channel = &mut self.channels[(port >> 1) as usize];
        if port & 1 == 0 {
            &mut channel.current_address
        } else {
            &mut channel.current_count
        }
    }
}

impl IoDevice for DmaController {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            // Channel address and count registers: low byte, then high
            // byte, as the flip-flop says
            0x00..=0x07 => {
                let value = *self.channel_register(port);
                let byte = if self.flip_flop {
                    (value >> 8) as u8
                } else {
                    value as u8
                };
                self.flip_flop = !self.flip_flop;
                byte
            }

            // Status register
            0x08 => {
                let s = self.status;
                self.status &= 0xF0; // reading clears TC bits (low nibble)
                s
            }

            // Temporary register: only memory-to-memory transfers load it
            0x0D => 0x00,

            other => {
                debug!("DMA: unhandled read from port {:04X}", other);
                0xFF
            }
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            // Channel address and count registers
            0x00..=0x07 => {
                let flip_flop = self.flip_flop;
                let register = self.channel_register(port);
                if flip_flop {
                    *register = (*register & 0x00FF) | ((value as u16) << 8);
                } else {
                    *register = (*register & 0xFF00) | value as u16;
                }
                self.flip_flop = !flip_flop;
            }

            // Command, request, mask and mode registers. They steer
            // transfers, which never happen here, and cannot be read back.
            0x08..=0x0B | 0x0E | 0x0F => {}

            // Clear flip-flop
            0x0C => {
                self.flip_flop = false;
            }

            // Master clear (reset)
            0x0D => {
                self.status = 0;
                self.flip_flop = false;
            }

            other => {
                debug!("DMA: unhandled write to port {:04X} = {:02X}", other, value);
            }
        }
    }
}

/// DMA page registers at ports 0x81-0x87: a device of their own, because
/// the bus gives each device one contiguous range of ports.
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
                debug!(
                    "DMA Page: unhandled write to port {:04X} = {:02X}",
                    port, value
                );
            }
        }
    }
}

#[cfg(test)]
#[path = "dma_tests.rs"]
mod tests;
