use log::debug;

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
    first_port: u16,
    last_port: u16,
}

impl IoBus {
    pub fn new() -> Self {
        Self {
            devices: Vec::new(),
            mappings: Vec::new(),
            first_port: 0,
            last_port: 0,
        }
    }

    // Registers a device to handle I/O ports in the range [start, end].
    // Should only be called during initialization, and with increasing port ranges.
    pub fn register(&mut self, start: u16, end: u16, device: Box<dyn IoDevice>) {
        if !self.mappings.is_empty() && (start <= self.last_port || end < start) {
            panic!("Port ranges must be registered in increasing order without overlap");
        }
        let idx = self.devices.len();
        self.devices.push(device);
        self.mappings.push(PortMapping {
            start,
            end,
            device_idx: idx,
        });
        if self.mappings.len() == 1 {
            self.first_port = start;
        }
        self.last_port = end;
    }

    pub fn port_in_byte(&mut self, port: u16) -> u8 {
        if port < self.first_port || port > self.last_port {
            return 0xFF; // Unmapped ports read as 0xFF
        }
        for mapping in &self.mappings {
            if port >= mapping.start && port <= mapping.end {
                return self.devices[mapping.device_idx].port_in_byte(port);
            }
        }
        debug!("Read from unmapped port {:04X}", port);
        0xFF
    }

    pub fn port_in_word(&mut self, port: u16) -> u16 {
        if port < self.first_port || port > self.last_port {
            return 0xFF; // Unmapped ports read as 0xFF
        }
        for mapping in &self.mappings {
            if port >= mapping.start && port <= mapping.end {
                return self.devices[mapping.device_idx].port_in_word(port);
            }
        }
        debug!("Read from unmapped port {:04X}", port);
        0xFFFF
    }

    pub fn port_out_byte(&mut self, port: u16, value: u8) {
        if port < self.first_port || port > self.last_port {
            return; // Ignore writes to unmapped ports
        }
        for mapping in &self.mappings {
            if port >= mapping.start && port <= mapping.end {
                self.devices[mapping.device_idx].port_out_byte(port, value);
                return;
            }
        }
        debug!("Write to unmapped port {:04X}: {:02X}", port, value);
    }

    pub fn port_out_word(&mut self, port: u16, value: u16) {
        if port < self.first_port || port > self.last_port {
            return; // Ignore writes to unmapped ports
        }
        for mapping in &self.mappings {
            if port >= mapping.start && port <= mapping.end {
                self.devices[mapping.device_idx].port_out_word(port, value);
                return;
            }
        }
        debug!("Write to unmapped port {:04X}: {:04X}", port, value);
    }
}
