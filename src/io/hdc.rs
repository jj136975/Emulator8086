use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

/// Minimal Xebec-style hard disk controller stub.
///
/// The emulator handles hard disk I/O via BIOS-level trapping, so this
/// device only needs to return sensible status values when software
/// probes the HDC ports directly.
pub struct HdcStub;

impl HdcStub {
    pub fn new() -> Self {
        Self
    }
}

impl IoDevice for HdcStub {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        match port {
            // Status register -- 0x00: ready, no errors
            0x321 => 0x00,

            // DIP switch / config register
            0x322 => 0x00,

            other => {
                debug!("HDC: unhandled read from port {:04X}", other);
                0xFF
            }
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
        debug!("HDC: write to port {:04X} = {:02X}", port, value);
    }

    fn name(&self) -> &'static str {
        "Xebec HDC Stub"
    }
}
