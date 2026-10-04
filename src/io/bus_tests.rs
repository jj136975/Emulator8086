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
    assert_eq!(bus.first_port, 0x00);
    assert_eq!(bus.last_port, 0x2F);
}

/// A range that overlaps the one before it panics: a port has one owner.
#[test]
#[should_panic(expected = "increasing order without overlap")]
fn overlapping_port_range_panics() {
    let mut bus = IoBus::new();
    bus.register(0x00, 0x1F, Box::new(DummyDevice));
    bus.register(0x10, 0x2F, Box::new(DummyDevice));
}

/// Ranges are registered in increasing order: one below the previous range
/// panics.
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

/// A word access is two byte accesses, low byte first: each byte goes to
/// the device that owns its port, and a byte whose port has no device
/// reads FFh or is dropped.
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
