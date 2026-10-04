use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

pub struct DebugConsole;

impl IoDevice for DebugConsole {
    fn port_in_byte(&mut self, _port: u16, _cpu: &mut Cpu) -> u8 { 0xFF }
    fn port_out_byte(&mut self, _port: u16, value: u8, _cpu: &mut Cpu) {
        eprint!("{}", value as char);
    }
}
