use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

/// Per-channel DMA state.
#[derive(Clone, Copy, Default)]
struct DmaChannel {
    current_address: u16,
    current_count: u16,
}

/// Minimal Intel 8237A DMA controller stub.
///
/// The IBM PC uses this for memory-to-device transfers (floppy, etc.).
/// Since the emulator traps disk I/O at the BIOS level, no actual DMA
/// transfers are performed -- we just keep the registers that can be read
/// back, so software that probes the DMA ports gets sensible values.
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
                let byte = if self.flip_flop { (value >> 8) as u8 } else { value as u8 };
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::runtime::Runtime;

    #[test]
    fn address_and_count_registers_read_back_through_the_flip_flop() {
        let mut rt = Runtime::new_test();
        let mut dma = DmaController::new();
        for channel in 0..4u16 {
            let address_port = channel * 2;
            let count_port = channel * 2 + 1;
            let address = 0x1234 + channel * 0x1111;
            let count = 0xA0B0 + channel;

            dma.port_out_byte(0x0C, 0, &mut rt.cpu); // clear the flip-flop
            dma.port_out_byte(address_port, address as u8, &mut rt.cpu);
            dma.port_out_byte(address_port, (address >> 8) as u8, &mut rt.cpu);
            dma.port_out_byte(count_port, count as u8, &mut rt.cpu);
            dma.port_out_byte(count_port, (count >> 8) as u8, &mut rt.cpu);

            let lo = dma.port_in_byte(address_port, &mut rt.cpu) as u16;
            let hi = dma.port_in_byte(address_port, &mut rt.cpu) as u16;
            assert_eq!(lo | (hi << 8), address, "channel {channel} address");
            let lo = dma.port_in_byte(count_port, &mut rt.cpu) as u16;
            let hi = dma.port_in_byte(count_port, &mut rt.cpu) as u16;
            assert_eq!(lo | (hi << 8), count, "channel {channel} count");
        }
    }

    #[test]
    fn clearing_the_flip_flop_selects_the_low_byte_again() {
        let mut rt = Runtime::new_test();
        let mut dma = DmaController::new();
        dma.port_out_byte(0x04, 0x34, &mut rt.cpu);
        dma.port_out_byte(0x04, 0x12, &mut rt.cpu);

        assert_eq!(dma.port_in_byte(0x04, &mut rt.cpu), 0x34);
        dma.port_out_byte(0x0C, 0, &mut rt.cpu);
        assert_eq!(dma.port_in_byte(0x04, &mut rt.cpu), 0x34, "low byte again, not the high byte");
        assert_eq!(dma.port_in_byte(0x04, &mut rt.cpu), 0x12);
    }

    #[test]
    fn status_is_cleared_by_reading_it_and_by_master_clear() {
        let mut rt = Runtime::new_test();
        let mut dma = DmaController::new();
        assert_eq!(dma.port_in_byte(0x08, &mut rt.cpu), 0x0F);
        assert_eq!(dma.port_in_byte(0x08, &mut rt.cpu), 0x00);

        let mut dma = DmaController::new();
        dma.port_out_byte(0x02, 0x55, &mut rt.cpu); // leaves the flip-flop on the high byte
        dma.port_out_byte(0x0D, 0, &mut rt.cpu); // master clear
        assert_eq!(dma.port_in_byte(0x08, &mut rt.cpu), 0x00);
        assert_eq!(dma.port_in_byte(0x02, &mut rt.cpu), 0x55, "flip-flop is back on the low byte");
    }

    #[test]
    fn temporary_register_reads_zero_and_write_only_ports_read_ff() {
        let mut rt = Runtime::new_test();
        let mut dma = DmaController::new();
        assert_eq!(dma.port_in_byte(0x0D, &mut rt.cpu), 0x00);
        for port in [0x09, 0x0A, 0x0B, 0x0C, 0x0E, 0x0F] {
            assert_eq!(dma.port_in_byte(port, &mut rt.cpu), 0xFF, "port {port:02X}h");
        }
    }

    #[test]
    fn page_registers_hold_one_byte_per_channel() {
        let mut rt = Runtime::new_test();
        let mut pages = DmaPageRegisters::new();
        for (port, value) in [(0x87, 0x10), (0x83, 0x21), (0x81, 0x32), (0x82, 0x43)] {
            pages.port_out_byte(port, value, &mut rt.cpu);
        }
        for (port, value) in [(0x87, 0x10), (0x83, 0x21), (0x81, 0x32), (0x82, 0x43)] {
            assert_eq!(pages.port_in_byte(port, &mut rt.cpu), value, "port {port:02X}h");
        }
        assert_eq!(pages.port_in_byte(0x84, &mut rt.cpu), 0xFF, "no register at 84h");
    }
}
