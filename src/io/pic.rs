use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

/// Intel 8259A Programmable Interrupt Controller
///
/// Handles a single PIC (master). For a full PC/AT you'd add a slave,
/// but a single master is sufficient for MS-DOS on an 8086/8088.
///
/// Ports (master): 0x20 (command), 0x21 (data)

pub struct Pic {
    /// Interrupt Request Register — bits set by devices raising IRQs
    irr: u8,
    /// In-Service Register — bits set for interrupts currently being serviced
    isr: u8,
    /// Interrupt Mask Register — bits set = IRQ line masked (disabled)
    imr: u8,
    /// Base vector number (ICW2). IRQ0 maps to this vector, IRQ1 to base+1, etc.
    vector_base: u8,

    /// ICW initialization state machine
    icw_step: u8,
    /// Whether ICW4 is expected
    icw4_expected: bool,

    /// Set after full initialization
    initialized: bool,
}

impl Pic {
    pub fn new() -> Self {
        Self {
            irr: 0,
            isr: 0,
            imr: 0xFF, // all masked until initialized
            vector_base: 0x08, // default IBM PC mapping: IRQ0 = INT 08h
            icw_step: 0,
            icw4_expected: false,
            initialized: false,
        }
    }

    /// Called by a device to raise an IRQ line (0–7).
    pub fn raise_irq(&mut self, irq: u8) {
        assert!(irq < 8);
        self.irr |= 1 << irq;
    }

    /// Called by a device to lower (deassert) an IRQ line.
    pub fn lower_irq(&mut self, irq: u8) {
        assert!(irq < 8);
        self.irr &= !(1 << irq);
    }

    /// Returns true if the PIC has a pending, unmasked interrupt
    /// that is higher priority than anything currently in service.
    pub fn has_interrupt(&self) -> bool {
        let pending = self.irr & !self.imr;
        if pending == 0 {
            return false;
        }
        // Find highest priority pending (lowest bit number)
        let highest_pending = pending.trailing_zeros();
        // Find highest priority in-service
        if self.isr == 0 {
            return true;
        }
        let highest_in_service = self.isr.trailing_zeros();
        highest_pending < highest_in_service
    }

    /// Acknowledge the highest-priority pending interrupt.
    /// Returns the interrupt vector number for the CPU to call.
    /// Moves the IRQ from IRR to ISR.
    pub fn acknowledge(&mut self) -> u8 {
        let pending = self.irr & !self.imr;
        if pending == 0 {
            // Spurious — return IRQ7 vector (standard 8259 behavior)
            return self.vector_base + 7;
        }
        let irq = pending.trailing_zeros() as u8;
        self.irr &= !(1 << irq);
        self.isr |= 1 << irq;
        self.vector_base + irq
    }

    fn write_command(&mut self, value: u8) {
        if value & 0x10 != 0 {
            // ICW1 — start initialization sequence
            self.icw_step = 1;
            self.icw4_expected = value & 0x01 != 0;
            self.imr = 0;
            self.isr = 0;
            self.irr = 0;
            self.initialized = false;
        } else if value & 0x08 == 0 {
            // OCW2 — EOI commands
            let op = (value >> 5) & 0x07;
            match op {
                // Non-specific EOI: clear highest priority ISR bit
                0b001 => {
                    if self.isr != 0 {
                        let bit = self.isr.trailing_zeros() as u8;
                        self.isr &= !(1 << bit);
                    }
                }
                // Specific EOI: clear the specified IRQ
                0b011 => {
                    let irq = value & 0x07;
                    self.isr &= !(1 << irq);
                }
                _ => {} // Other EOI modes — ignore for now
            }
        }
        // OCW3 (value & 0x08 != 0) — read IRR/ISR commands, can be added later
    }

    fn write_data(&mut self, value: u8) {
        if self.icw_step > 0 {
            match self.icw_step {
                1 => {
                    // ICW2 — vector base (upper 5 bits matter)
                    self.vector_base = value & 0xF8;
                    // On a single-PIC system, skip ICW3
                    self.icw_step = if self.icw4_expected { 3 } else { 0 };
                    if self.icw_step == 0 {
                        self.initialized = true;
                    }
                }
                // ICW3 would be step 2 — skipped for single PIC
                3 => {
                    // ICW4 — we mostly ignore it (auto-EOI bit, 8086 mode, etc.)
                    // bit 1 = auto-EOI — could be implemented later
                    self.icw_step = 0;
                    self.initialized = true;
                }
                _ => {
                    self.icw_step = 0;
                    self.initialized = true;
                }
            }
        } else {
            // OCW1 — write to IMR
            self.imr = value;
        }
    }

    fn read_command(&self) -> u8 {
        // Default: return IRR (a full implementation would track
        // OCW3 read-register commands to switch between IRR/ISR)
        self.irr
    }

    fn read_data(&self) -> u8 {
        self.imr
    }
}

pub struct PicDevice;

impl IoDevice for PicDevice {
    fn port_in_byte(&mut self, port: u16, cpu: &mut Cpu) -> u8 {
        match port & 0x01 {
            0 => cpu.pic.read_command(),  // 0x20
            _ => cpu.pic.read_data(),     // 0x21
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, cpu: &mut Cpu) {
        match port & 0x01 {
            0 => cpu.pic.write_command(value),  // 0x20
            _ => cpu.pic.write_data(value),     // 0x21
        }
    }

    fn name(&self) -> &'static str {
        "8259A PIC"
    }
}