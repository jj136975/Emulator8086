//! A stand-in for the PC/XT's Xebec hard disk controller at ports
//! 320h-323h. No sector passes through it: the BIOS serves INT 13h at its
//! disk trap. It holds no state, and answers software that probes the
//! controller: the configuration switches (322h) read 00h.
//!
//! The invariant: the controller always reports itself ready. The status
//! register (321h) reads 00h, ready and without error, and every write is
//! dropped, so nothing a program sends starts a command.

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

/// Minimal Xebec-style hard disk controller stub.
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
}
