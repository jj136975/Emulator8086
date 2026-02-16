use std::sync::{Arc, Mutex};
use log::debug;
use crate::io::bus::IoDevice;

#[derive(PartialEq)]
enum InitState {
    Ready,
    WaitIcw2,
    WaitIcw3,
    WaitIcw4,
}

#[derive(PartialEq, Clone, Copy)]
enum TriggerMode {
    Edge,
    Level,
}

pub struct Pic {
    irr: u8,
    isr: u8,
    imr: u8,
    vector_offset: u8,
    init_state: InitState,
    icw4_needed: bool,
    read_isr: bool,
    auto_eoi: bool,
    single_mode: bool,
    trigger_mode: TriggerMode,
    /// Physical line state for edge detection (bit per IRQ)
    irq_lines: u8,
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
            auto_eoi: false,
            single_mode: false,
            trigger_mode: TriggerMode::Edge,
            irq_lines: 0,
        }
    }

    /// Set physical IRQ line level. In edge-triggered mode, IRR is latched
    /// only on a rising edge (low→high). In level-triggered mode, IRR is
    /// set whenever the line is high.
    pub fn set_irq_line(&mut self, irq: u8, level: bool) {
        let mask = 1u8 << irq;
        let was_high = self.irq_lines & mask != 0;

        if level {
            self.irq_lines |= mask;
            match self.trigger_mode {
                TriggerMode::Edge => {
                    // Latch to IRR only on rising edge
                    if !was_high {
                        self.irr |= mask;
                    }
                }
                TriggerMode::Level => {
                    self.irr |= mask;
                }
            }
        } else {
            self.irq_lines &= !mask;
        }
    }

    /// Convenience: pulse an IRQ line (rising edge then deassert).
    pub fn raise_irq(&mut self, irq: u8) {
        self.set_irq_line(irq, true);
        self.set_irq_line(irq, false);
    }

    /// Check if there is a pending interrupt that can be serviced.
    /// A pending IRQ must be unmasked AND have higher priority (lower IRQ#)
    /// than the highest-priority in-service interrupt.
    pub fn has_interrupt(&self) -> bool {
        let pending = self.irr & !self.imr;
        if pending == 0 {
            return false;
        }
        // Find highest-priority (lowest bit) pending IRQ
        let highest_pending = pending.trailing_zeros();
        // Find highest-priority in-service IRQ
        if self.isr != 0 {
            let highest_isr = self.isr.trailing_zeros();
            // Pending must have strictly higher priority (lower number)
            highest_pending < highest_isr
        } else {
            true
        }
    }

    /// Acknowledge the highest-priority pending interrupt.
    /// Returns the interrupt vector number.
    /// If IRR has been cleared between has_interrupt() and acknowledge()
    /// (shouldn't happen on 8086, but spec says), returns spurious IRQ7 vector.
    pub fn acknowledge(&mut self) -> Option<u8> {
        let pending = self.irr & !self.imr;
        if pending == 0 {
            // Spurious IRQ7: return vector but do NOT set ISR bit
            debug!("[PIC] ACK: spurious IRQ7 (IRR={:02X} IMR={:02X})", self.irr, self.imr);
            return Some(self.vector_offset + 7);
        }

        // Find highest priority pending (lowest bit number)
        for i in 0..8u8 {
            if pending & (1 << i) != 0 {
                // Check priority against in-service
                if self.isr != 0 {
                    let highest_isr = self.isr.trailing_zeros() as u8;
                    if i >= highest_isr {
                        debug!("[PIC] ACK: IRQ {} blocked by ISR bit {} (ISR={:02X})", i, highest_isr, self.isr);
                        return None;
                    }
                }
                self.irr &= !(1 << i);
                if self.auto_eoi {
                    // In auto-EOI mode, don't set ISR bit
                } else {
                    self.isr |= 1 << i;
                }
                return Some(self.vector_offset + i);
            }
        }
        None
    }

    pub fn irq_busy(&self, irq: u8) -> bool {
        let mask = 1 << irq;
        (self.irr & mask) != 0 || (self.isr & mask) != 0
    }

    /// Non-specific EOI: clear highest-priority ISR bit.
    /// Duplicates the OCW2 non-specific EOI logic so BIOS handlers can
    /// send EOI without borrowing the full IO bus.
    pub fn eoi(&mut self) {
        for i in 0..8u8 {
            if self.isr & (1 << i) != 0 {
                self.isr &= !(1 << i);
                break;
            }
        }
    }

    /// Directly set the IMR (interrupt mask register).
    pub fn set_imr(&mut self, value: u8) {
        self.imr = value;
    }

    pub fn debug_irr(&self) -> u8 { self.irr }
    pub fn debug_isr(&self) -> u8 { self.isr }
    pub fn debug_imr(&self) -> u8 { self.imr }

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
                    // ICW1
                    self.init_state = InitState::WaitIcw2;
                    self.icw4_needed = value & 0x01 != 0;
                    self.single_mode = value & 0x02 != 0;
                    self.trigger_mode = if value & 0x08 != 0 {
                        TriggerMode::Level
                    } else {
                        TriggerMode::Edge
                    };
                    // Reset state
                    self.imr = 0;
                    self.isr = 0;
                    self.irr = 0;
                    self.auto_eoi = false;
                    self.irq_lines = 0;
                    self.read_isr = false;
                } else if value & 0x08 != 0 {
                    // OCW3: read ISR/IRR select
                    self.read_isr = value & 0x01 != 0;
                } else if value == 0x20 {
                    // Non-specific EOI: clear highest-priority ISR bit
                    let old_isr = self.isr;
                    for i in 0..8u8 {
                        if self.isr & (1 << i) != 0 {
                            self.isr &= !(1 << i);
                            debug!("[PIC] non-specific EOI: cleared IRQ {} ISR {:02X}->{:02X}", i, old_isr, self.isr);
                            break;
                        }
                    }
                } else if value & 0x60 == 0x60 {
                    // Specific EOI
                    let irq = value & 0x07;
                    let old_isr = self.isr;
                    self.isr &= !(1 << irq);
                    debug!("[PIC] specific EOI IRQ {} ISR {:02X}->{:02X}", irq, old_isr, self.isr);
                }
            }
            0x21 => {
                match self.init_state {
                    InitState::WaitIcw2 => {
                        // ICW2: vector offset, mask lower 3 bits
                        self.vector_offset = value & 0xF8;
                        if self.single_mode {
                            // Skip ICW3 in single mode
                            if self.icw4_needed {
                                self.init_state = InitState::WaitIcw4;
                            } else {
                                self.init_state = InitState::Ready;
                            }
                        } else {
                            self.init_state = InitState::WaitIcw3;
                        }
                    }
                    InitState::WaitIcw3 => {
                        // ICW3: cascade info (ignored for single PIC emulation)
                        if self.icw4_needed {
                            self.init_state = InitState::WaitIcw4;
                        } else {
                            self.init_state = InitState::Ready;
                        }
                    }
                    InitState::WaitIcw4 => {
                        // ICW4: bit 1 = auto EOI
                        self.auto_eoi = value & 0x02 != 0;
                        self.init_state = InitState::Ready;
                        debug!("[PIC] ICW4={:02X} auto_eoi={}", value, self.auto_eoi);
                    }
                    InitState::Ready => {
                        // OCW1: set IMR
                        let old = self.imr;
                        self.imr = value;
                        if old != value {
                            debug!("[PIC] IMR {:02X}->{:02X}", old, value);
                        }
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
