use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

pub struct DebugConsole;

impl IoDevice for DebugConsole {
    fn port_in_byte(&mut self, _port: u16, _cpu: &mut Cpu) -> u8 { 0xFF }
    fn port_out_byte(&mut self, _port: u16, value: u8, _cpu: &mut Cpu) {
        print!("{}", value as char);
    }
    fn name(&self) -> &'static str { "Debug Console" }
}

// --- PIT: just swallow writes ---
pub struct PitStub;

impl IoDevice for PitStub {
    fn port_in_byte(&mut self, _port: u16, _cpu: &mut Cpu) -> u8 { 0x00 }
    fn port_out_byte(&mut self, _port: u16, _value: u8, _cpu: &mut Cpu) {
        // Accept and ignore. No timer ticks yet.
    }
    fn name(&self) -> &'static str { "PIT Stub" }
}