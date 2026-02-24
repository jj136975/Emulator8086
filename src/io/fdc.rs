use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

/// Minimal NEC uPD765 floppy disk controller stub.
///
/// The emulator handles floppy I/O via BIOS-level trapping, so this device
/// only needs to return sensible register values when software probes the
/// FDC ports directly.
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

    fn name(&self) -> &'static str {
        "NEC uPD765 FDC Stub"
    }
}
