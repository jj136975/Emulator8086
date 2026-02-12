use std::sync::{Arc, Mutex};
use crate::io::bus::IoDevice;

#[derive(PartialEq)]
enum InitState {
    Ready,
    WaitIcw2,
    WaitIcw3,
    WaitIcw4,
}

pub struct Pic {
    irr: u8,
    isr: u8,
    imr: u8,
    vector_offset: u8,
    init_state: InitState,
    icw4_needed: bool,
    read_isr: bool,
}

impl Pic {
    pub fn new() -> Self {
        Self {
            irr: 0,
            isr: 0,
            imr: 0xFF,
            vector_offset: 8,
            init_state: InitState::Ready,
            icw4_needed: false,
            read_isr: false,
        }
    }

    pub fn raise_irq(&mut self, irq: u8) {
        self.irr |= 1 << irq;
    }

    pub fn acknowledge(&mut self) -> Option<u8> {
        let pending = self.irr & !self.imr;
        if pending == 0 {
            return None;
        }
        for i in 0..8u8 {
            if pending & (1 << i) != 0 {
                self.irr &= !(1 << i);
                self.isr |= 1 << i;
                return Some(self.vector_offset + i);
            }
        }
        None
    }

    pub fn has_interrupt(&self) -> bool {
        (self.irr & !self.imr) != 0
    }

    fn handle_port_in(&mut self, port: u16) -> u8 {
        match port {
            0x20 => {
                if self.read_isr { self.isr } else { self.irr }
            }
            0x21 => self.imr,
            _ => 0xFF,
        }
    }

    fn handle_port_out(&mut self, port: u16, value: u8) {
        match port {
            0x20 => {
                if value & 0x10 != 0 {
                    self.init_state = InitState::WaitIcw2;
                    self.icw4_needed = value & 0x01 != 0;
                    self.imr = 0;
                    self.isr = 0;
                    self.irr = 0;
                } else if value & 0x08 != 0 {
                    self.read_isr = value & 0x01 != 0;
                } else if value == 0x20 {
                    for i in 0..8u8 {
                        if self.isr & (1 << i) != 0 {
                            self.isr &= !(1 << i);
                            break;
                        }
                    }
                } else if value & 0x60 == 0x60 {
                    let irq = value & 0x07;
                    self.isr &= !(1 << irq);
                }
            }
            0x21 => {
                match self.init_state {
                    InitState::WaitIcw2 => {
                        self.vector_offset = value;
                        self.init_state = InitState::WaitIcw3;
                    }
                    InitState::WaitIcw3 => {
                        if self.icw4_needed {
                            self.init_state = InitState::WaitIcw4;
                        } else {
                            self.init_state = InitState::Ready;
                        }
                    }
                    InitState::WaitIcw4 => {
                        self.init_state = InitState::Ready;
                    }
                    InitState::Ready => {
                        self.imr = value;
                    }
                }
            }
            _ => {}
        }
    }
}

/// Wrapper that delegates to a shared Pic via Arc<Mutex<>>
pub struct SharedPic {
    inner: Arc<Mutex<Pic>>,
}

impl SharedPic {
    pub fn new(pic: Arc<Mutex<Pic>>) -> Self {
        Self { inner: pic }
    }
}

impl IoDevice for SharedPic {
    fn port_in_byte(&mut self, port: u16) -> u8 {
        self.inner.lock().unwrap().handle_port_in(port)
    }

    fn port_out_byte(&mut self, port: u16, value: u8) {
        self.inner.lock().unwrap().handle_port_out(port, value);
    }

    fn name(&self) -> &'static str {
        "8259 PIC"
    }
}
