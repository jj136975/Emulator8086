pub trait IoDevice {
    fn port_in_byte(&mut self, port: u16) -> u8;
    fn port_out_byte(&mut self, port: u16, value: u8);

    fn port_in_word(&mut self, port: u16) -> u16 {
        let lo = self.port_in_byte(port) as u16;
        let hi = self.port_in_byte(port.wrapping_add(1)) as u16;
        lo | (hi << 8)
    }

    fn port_out_word(&mut self, port: u16, value: u16) {
        self.port_out_byte(port, value as u8);
        self.port_out_byte(port.wrapping_add(1), (value >> 8) as u8);
    }

    fn name(&self) -> &'static str;
}

struct PortMapping {
    start: u16,
    end: u16,
    device_idx: usize,
}

pub struct IoBus {
    devices: Vec<Box<dyn IoDevice>>,
    mappings: Vec<PortMapping>,
}

impl IoBus {
    pub fn new() -> Self {
        Self {
            devices: Vec::new(),
            mappings: Vec::new(),
        }
    }

    pub fn register(&mut self, start: u16, end: u16, device: Box<dyn IoDevice>) {
        let idx = self.devices.len();
        self.devices.push(device);
        self.mappings.push(PortMapping {
            start,
            end,
            device_idx: idx,
        });
    }

    pub fn port_in_byte(&mut self, port: u16) -> u8 {
        for i in 0..self.mappings.len() {
            if port >= self.mappings[i].start && port <= self.mappings[i].end {
                return self.devices[self.mappings[i].device_idx].port_in_byte(port);
            }
        }
        0xFF
    }

    pub fn port_in_word(&mut self, port: u16) -> u16 {
        for i in 0..self.mappings.len() {
            if port >= self.mappings[i].start && port <= self.mappings[i].end {
                return self.devices[self.mappings[i].device_idx].port_in_word(port);
            }
        }
        0xFFFF
    }

    pub fn port_out_byte(&mut self, port: u16, value: u8) {
        for i in 0..self.mappings.len() {
            if port >= self.mappings[i].start && port <= self.mappings[i].end {
                self.devices[self.mappings[i].device_idx].port_out_byte(port, value);
                return;
            }
        }
    }

    pub fn port_out_word(&mut self, port: u16, value: u16) {
        for i in 0..self.mappings.len() {
            if port >= self.mappings[i].start && port <= self.mappings[i].end {
                self.devices[self.mappings[i].device_idx].port_out_word(port, value);
                return;
            }
        }
    }
}
