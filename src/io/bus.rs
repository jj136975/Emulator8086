use crate::vm::cpu::Cpu;
use log::debug;
use std::cell::RefCell;
use std::rc::Rc;

pub trait IoDevice {
    fn port_in_byte(&mut self, port: u16, cpu: &mut Cpu) -> u8;
    fn port_out_byte(&mut self, port: u16, value: u8, cpu: &mut Cpu);
}

impl<T: IoDevice> IoDevice for Rc<RefCell<T>> {
    fn port_in_byte(&mut self, port: u16, cpu: &mut Cpu) -> u8 {
        self.borrow_mut().port_in_byte(port, cpu)
    }
    fn port_out_byte(&mut self, port: u16, value: u8, cpu: &mut Cpu) {
        self.borrow_mut().port_out_byte(port, value, cpu)
    }
}

struct PortMapping {
    start: u16,
    end: u16,
    device: Box<dyn IoDevice>,
}

pub struct IoBus {
    mappings: Vec<PortMapping>,
    first_port: u16,
    last_port: u16,
}

impl IoBus {
    pub fn new() -> Self {
        Self {
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
        self.mappings.push(PortMapping { start, end, device });
        if self.mappings.len() == 1 {
            self.first_port = start;
        }
        self.last_port = end;
    }

    /// The device that owns `port`, if any.
    fn device_for(&mut self, port: u16) -> Option<&mut Box<dyn IoDevice>> {
        if port < self.first_port || port > self.last_port {
            return None;
        }
        self.mappings
            .iter_mut()
            .find(|mapping| port >= mapping.start && port <= mapping.end)
            .map(|mapping| &mut mapping.device)
    }

    pub fn port_in_byte(&mut self, port: u16, cpu: &mut Cpu) -> u8 {
        match self.device_for(port) {
            Some(device) => device.port_in_byte(port, cpu),
            None => {
                debug!("Read from unmapped port {:04X}", port);
                0xFF // Unmapped ports read as 0xFF
            }
        }
    }

    /// A word access is two byte accesses, low byte first, at `port` and
    /// `port + 1`. Each is decoded on its own: the two ports may belong to
    /// different devices, or only one of them to a device at all.
    pub fn port_in_word(&mut self, port: u16, cpu: &mut Cpu) -> u16 {
        let lo = self.port_in_byte(port, cpu) as u16;
        let hi = self.port_in_byte(port.wrapping_add(1), cpu) as u16;
        lo | (hi << 8)
    }

    pub fn port_out_byte(&mut self, port: u16, value: u8, cpu: &mut Cpu) {
        match self.device_for(port) {
            Some(device) => device.port_out_byte(port, value, cpu),
            // Writes to unmapped ports are ignored
            None => debug!("Write to unmapped port {:04X}: {:02X}", port, value),
        }
    }

    pub fn port_out_word(&mut self, port: u16, value: u16, cpu: &mut Cpu) {
        self.port_out_byte(port, value as u8, cpu);
        self.port_out_byte(port.wrapping_add(1), (value >> 8) as u8, cpu);
    }
}

// ---------------------------------------------------------------------------
// Regression tests — IoBus registration and dispatch invariants.
// ---------------------------------------------------------------------------
#[cfg(test)]
mod inspection_tests {
    use super::*;

    struct DummyDevice;
    impl IoDevice for DummyDevice {
        fn port_in_byte(&mut self, _port: u16, _cpu: &mut Cpu) -> u8 {
            0
        }
        fn port_out_byte(&mut self, _port: u16, _value: u8, _cpu: &mut Cpu) {}
    }

    /// Adjacent, non-overlapping port ranges (0x00..=0x0F then 0x10..=0x1F)
    /// register without panicking.
    #[test]
    fn adjacent_nonoverlapping_port_ranges_register_cleanly() {
        let mut bus = IoBus::new();
        bus.register(0x00, 0x0F, Box::new(DummyDevice));
        bus.register(0x10, 0x1F, Box::new(DummyDevice));
        bus.register(0x20, 0x2F, Box::new(DummyDevice));
        // If we made it here, no panic. Good.
        assert_eq!(bus.first_port, 0x00);
        assert_eq!(bus.last_port, 0x2F);
    }

    /// Sanity: overlapping ranges must panic to prevent silent collisions.
    #[test]
    #[should_panic(expected = "increasing order without overlap")]
    fn overlapping_port_range_panics() {
        let mut bus = IoBus::new();
        bus.register(0x00, 0x1F, Box::new(DummyDevice));
        // Overlaps the previous range (0x10 <= 0x1F)
        bus.register(0x10, 0x2F, Box::new(DummyDevice));
    }

    /// Out-of-increasing-order registration also panics (start < last_port).
    #[test]
    #[should_panic(expected = "increasing order without overlap")]
    fn out_of_order_registration_panics() {
        let mut bus = IoBus::new();
        bus.register(0x20, 0x2F, Box::new(DummyDevice));
        bus.register(0x00, 0x0F, Box::new(DummyDevice));
    }

    /// A device that logs every access it is given as (id, port, value);
    /// reads return its id.
    struct Probe {
        id: u8,
        log: Rc<RefCell<Vec<(u8, u16, u8)>>>,
    }

    impl IoDevice for Probe {
        fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
            self.log.borrow_mut().push((self.id, port, self.id));
            self.id
        }
        fn port_out_byte(&mut self, port: u16, value: u8, _cpu: &mut Cpu) {
            self.log.borrow_mut().push((self.id, port, value));
        }
    }

    /// Regression: a word access was handed whole to the device that owns
    /// the low port. The high byte was dropped when only the high port was
    /// mapped, and devices were given ports outside their range.
    #[test]
    fn word_access_is_two_byte_accesses_decoded_separately() {
        let mut rt = crate::vm::runtime::Runtime::new_test();
        let log = Rc::new(RefCell::new(Vec::new()));
        let mut bus = IoBus::new();
        bus.register(
            0x20,
            0x21,
            Box::new(Probe {
                id: 0xA1,
                log: Rc::clone(&log),
            }),
        );
        bus.register(
            0x22,
            0x22,
            Box::new(Probe {
                id: 0xB2,
                log: Rc::clone(&log),
            }),
        );
        bus.register(
            0x40,
            0x41,
            Box::new(Probe {
                id: 0xC3,
                log: Rc::clone(&log),
            }),
        );

        // Both bytes inside one device, low byte first.
        bus.port_out_word(0x20, 0x1234, &mut rt.cpu);
        // The two bytes belong to different devices.
        bus.port_out_word(0x21, 0x5678, &mut rt.cpu);
        // The high byte has no device.
        bus.port_out_word(0x41, 0x9ABC, &mut rt.cpu);
        // The low byte has no device.
        bus.port_out_word(0x3F, 0xDEF0, &mut rt.cpu);
        assert_eq!(
            *log.borrow(),
            [
                (0xA1, 0x20, 0x34),
                (0xA1, 0x21, 0x12),
                (0xA1, 0x21, 0x78),
                (0xB2, 0x22, 0x56),
                (0xC3, 0x41, 0xBC),
                (0xC3, 0x40, 0xDE),
            ]
        );

        assert_eq!(bus.port_in_word(0x20, &mut rt.cpu), 0xA1A1);
        assert_eq!(bus.port_in_word(0x21, &mut rt.cpu), 0xB2A1);
        assert_eq!(
            bus.port_in_word(0x41, &mut rt.cpu),
            0xFFC3,
            "unmapped high byte reads FFh"
        );
        assert_eq!(
            bus.port_in_word(0x3F, &mut rt.cpu),
            0xC3FF,
            "unmapped low byte reads FFh"
        );
        assert_eq!(bus.port_in_word(0x30, &mut rt.cpu), 0xFFFF);
    }
}
