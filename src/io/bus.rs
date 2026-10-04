//! The port bus. `IoDevice` is what every device answers its ports
//! through, and `IoBus` decodes a port to the device registered for it.
//! The access is handed on with the `Cpu`, because the interrupt
//! controller lives there and the disk trap works on the guest's registers
//! and memory. A port no device owns reads FFh, and a write to it is
//! dropped.
//!
//! The invariant: a device only ever sees byte accesses to the ports it
//! was registered for. A word access is two byte accesses, low byte first,
//! each decoded on its own, and ranges are registered in increasing order
//! without overlap, so a port has at most one owner.

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

    /// Gives `device` the ports `start..=end`. Ranges are registered while
    /// the machine is built, in increasing order and without overlap; one
    /// that ends before it starts, or does not lie wholly past the previous
    /// range, panics.
    pub fn register(&mut self, start: u16, end: u16, device: Box<dyn IoDevice>) {
        if end < start || (!self.mappings.is_empty() && start <= self.last_port) {
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
                0xFF
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
            None => debug!("Write to unmapped port {:04X}: {:02X}", port, value),
        }
    }

    pub fn port_out_word(&mut self, port: u16, value: u16, cpu: &mut Cpu) {
        self.port_out_byte(port, value as u8, cpu);
        self.port_out_byte(port.wrapping_add(1), (value >> 8) as u8, cpu);
    }
}

#[cfg(test)]
#[path = "bus_tests.rs"]
mod tests;
