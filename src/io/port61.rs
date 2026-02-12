use crate::io::bus::IoDevice;

/// System Control Port B (0x61)
///
/// On a real PC/AT, bit 4 toggles with each DRAM refresh cycle (~15μs).
/// MS-DOS uses this for timing delays by polling until the bit changes.
pub struct SystemControl {
    value: u8,
    toggle: bool,
}

impl SystemControl {
    pub fn new() -> Self {
        Self {
            value: 0,
            toggle: false,
        }
    }
}

impl IoDevice for SystemControl {
    fn port_in_byte(&mut self, _port: u16) -> u8 {
        // Toggle bit 4 on every read (refresh detect)
        self.toggle = !self.toggle;
        if self.toggle {
            self.value | 0x10
        } else {
            self.value & !0x10
        }
    }

    fn port_out_byte(&mut self, _port: u16, value: u8) {
        // Only store bits 0-1 (timer 2 gate, speaker data)
        self.value = value & 0x03;
    }

    fn name(&self) -> &'static str {
        "System Control Port B"
    }
}
