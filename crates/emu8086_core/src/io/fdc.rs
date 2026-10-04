//! A stand-in for the NEC uPD765 floppy disk controller at ports
//! 3F0h-3F7h. No sector passes through it: the BIOS serves INT 13h at its
//! disk trap. It answers software that probes the controller, and keeps
//! the digital output register that the BIOS's timer tick writes when it
//! turns the drive motor off.
//!
//! The invariant: the controller is always idle and ready for a command.
//! The main status register reads 80h, the data register holds no result
//! byte, command bytes are swallowed, and no IRQ6 is ever raised.

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

/// Minimal NEC uPD765 floppy disk controller stub.
pub struct FdcStub {
    dor: u8, // Digital Output Register (0x3F2)
}

impl FdcStub {
    pub fn new() -> Self {
        Self { dor: 0 }
    }
}

impl IoDevice for FdcStub {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            // Digital Output Register
            0x3F2 => self.dor,

            // Main Status Register -- RQM=1 (ready for commands), DIO=0 (CPU->FDC)
            0x3F4 => 0x80,

            // Data Register -- return idle (no result bytes)
            0x3F5 => 0x00,

            other => {
                debug!("FDC: unhandled read from port {:04X}", other);
                0xFF
            }
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        match port {
            // Digital Output Register (motor control, drive select, reset)
            0x3F2 => {
                self.dor = value;
            }

            // Data Register (command bytes) -- silently consumed
            0x3F5 => {
                debug!("FDC: command byte {:02X}", value);
            }

            other => {
                debug!("FDC: unhandled write to port {:04X} = {:02X}", other, value);
            }
        }
    }
}
