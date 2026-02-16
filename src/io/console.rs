use crate::io::bus::IoDevice;

pub struct DebugConsole;

impl IoDevice for DebugConsole {
    fn port_in_byte(&mut self, _port: u16) -> u8 { 0xFF }
    fn port_out_byte(&mut self, _port: u16, value: u8) {
        print!("{}", value as char);
    }
    fn name(&self) -> &'static str { "Debug Console" }
}

// --- Disk trap: reads CPU regs, loads from disk image ---
// This one is special — it needs access to CPU state and memory.
// For now, just accept the write and handle it in your main loop
// by detecting the OUT to 0xB0.
pub struct DiskTrap;

impl IoDevice for DiskTrap {
    fn port_in_byte(&mut self, _port: u16) -> u8 { 0xFF }
    fn port_out_byte(&mut self, _port: u16, _value: u8) {
        // Set a flag; main loop handles the actual disk I/O
        // because it needs access to CPU regs + memory
    }
    fn name(&self) -> &'static str { "Disk Trap" }
}

// --- PIT: just swallow writes ---
pub struct PitStub;

impl IoDevice for PitStub {
    fn port_in_byte(&mut self, _port: u16) -> u8 { 0x00 }
    fn port_out_byte(&mut self, _port: u16, _value: u8) {
        // Accept and ignore. No timer ticks yet.
    }
    fn name(&self) -> &'static str { "PIT Stub" }
}