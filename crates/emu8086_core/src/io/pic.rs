//! The Intel 8259A interrupt controller, a single master at ports 20h-21h
//! as on the PC/XT: the request, in-service and mask registers, the
//! ICW1-ICW4 initialisation sequence, OCW2's EOI, rotation and priority
//! commands, and OCW3's status read, poll and special mask mode. There is
//! no slave to cascade to, so ICW3 is consumed and ignored. The controller
//! lives in `Cpu::pic`, where the run loop asks `has_interrupt` after every
//! instruction; `PicDevice` holds no state and reaches it through the
//! `cpu` each port access is handed.
//!
//! `Pic::view` describes the controller to a reader in its datasheet's
//! terms, without a port access: reading the status ports would answer a
//! poll the guest is waiting for.
//!
//! The invariant: a request is latched. `request_interrupt` sets an IRR bit
//! that stays until the interrupt is acknowledged (in edge-triggered mode)
//! or its device withdraws it with `clear_interrupt`. Devices raise a
//! request once per event and rely on it waiting, through a mask or a
//! higher-priority handler, until the CPU takes it.

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

#[derive(Clone, Copy, PartialEq)]
enum TriggerMode {
    Edge,
    Level,
}

/// What the 8259A holds and how it was programmed, named as its datasheet
/// names them. A copy: holding one changes nothing in the controller.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PicView {
    /// IRR: the lines with a request waiting, bit n for IRQn.
    pub request: u8,
    /// ISR: the lines whose interrupt is being serviced.
    pub in_service: u8,
    /// IMR: the lines that are masked.
    pub mask: u8,
    /// ICW2: the vector of IRQ0; IRQn is served by `vector_base + n`.
    pub vector_base: u8,
    /// ICW1 LTIM: a request is a level held high, not a rising edge.
    pub level_triggered: bool,
    /// ICW1 SNGL: the controller works alone, and its initialisation
    /// skips ICW3. Clear, it was set up for a cascade, which this machine
    /// does not have: ICW3 was taken and ignored.
    pub single: bool,
    /// ICW4 AEOI: an interrupt leaves service when it is acknowledged,
    /// without an EOI command.
    pub auto_eoi: bool,
    /// OCW2 "rotate in automatic EOI mode": each interrupt that ends
    /// automatically becomes the lowest priority.
    pub rotate_on_auto_eoi: bool,
    /// OCW3 SMM: special mask mode, in which a masked line in service no
    /// longer holds back the lines below it.
    pub special_mask: bool,
    /// The line with the highest priority: IRQ0 until a rotation moves it.
    pub highest_priority: u8,
    /// OCW3 RR/RIS: a read of port 20h returns the ISR, rather than the IRR.
    pub reads_in_service: bool,
    /// OCW3 P: a poll command waits for the read that answers it.
    pub poll: bool,
    /// The initialisation word the controller waits for, 2 to 4, while
    /// the sequence ICW1 started is not over.
    pub awaiting_icw: Option<u8>,
}

pub struct Pic {
    /// Interrupt Request Register — one bit per IRQ line with a request waiting
    irr: u8,
    /// In-Service Register — bits set for interrupts currently being serviced
    isr: u8,
    /// Interrupt Mask Register — bits set = IRQ line masked (disabled)
    imr: u8,
    /// Base vector number (ICW2). IRQ0 maps to this vector, IRQ1 to base+1, etc.
    vector_base: u8,

    /// The initialisation word expected next: 0 none, 1 ICW2, 2 ICW3, 3 ICW4
    icw_step: u8,
    /// Whether ICW4 is expected
    icw4_expected: bool,
    /// SNGL bit from ICW1: true = single PIC (no ICW3), false = cascade (ICW3 required)
    single_pic: bool,

    /// Auto-EOI mode (ICW4 bit 1)
    auto_eoi: bool,
    /// Which register to return on port 0x20 read: 0=IRR, 1=ISR
    read_register: u8,
    /// A poll command (OCW3 with P set) is waiting for the read that
    /// answers it
    poll: bool,
    /// Special mask mode (OCW3 with ESMM and SMM set)
    special_mask: bool,

    /// Edge vs level trigger mode, set from ICW1 bit 3
    trigger_mode: TriggerMode,
    /// Lowest priority IRQ (default 7). Highest priority = (lowest_priority + 1) % 8.
    lowest_priority: u8,
    /// Whether to rotate priorities on auto-EOI
    rotate_on_aeoi: bool,
}

impl Pic {
    pub(crate) fn new() -> Self {
        Self {
            irr: 0,
            isr: 0,
            imr: 0xFF,         // all masked until initialized
            vector_base: 0x08, // default IBM PC mapping: IRQ0 = INT 08h
            icw_step: 0,
            icw4_expected: false,
            single_pic: false,
            auto_eoi: false,
            read_register: 0,
            poll: false,
            special_mask: false,
            trigger_mode: TriggerMode::Edge,
            lowest_priority: 7,
            rotate_on_aeoi: false,
        }
    }

    /// The controller as a reader sees it. Nothing is acknowledged, polled
    /// or selected: the guest finds the controller as it left it.
    pub fn view(&self) -> PicView {
        PicView {
            request: self.irr,
            in_service: self.isr,
            mask: self.imr,
            vector_base: self.vector_base,
            level_triggered: self.trigger_mode == TriggerMode::Level,
            single: self.single_pic,
            auto_eoi: self.auto_eoi,
            rotate_on_auto_eoi: self.rotate_on_aeoi,
            special_mask: self.special_mask,
            highest_priority: (self.lowest_priority + 1) % 8,
            reads_in_service: self.read_register == 1,
            poll: self.poll,
            // `icw_step` counts the words after ICW1: 1 stands for ICW2.
            awaiting_icw: (self.icw_step > 0).then_some(self.icw_step + 1),
        }
    }

    /// Returns the priority value (0 = highest, 7 = lowest) for a given IRQ,
    /// taking into account the current priority rotation.
    fn priority_of(&self, irq: u8) -> u8 {
        // Highest priority IRQ = (lowest_priority + 1) % 8, which gets priority 0.
        // lowest_priority IRQ gets priority 7.
        (irq + 8 - self.lowest_priority - 1) % 8
    }

    /// Find the highest-priority IRQ that has its bit set in `mask`.
    /// Returns None if mask is 0.
    fn highest_priority_bit(&self, mask: u8) -> Option<u8> {
        if mask == 0 {
            return None;
        }
        let mut best_irq: Option<u8> = None;
        let mut best_priority: u8 = u8::MAX;
        for i in 0..8u8 {
            if mask & (1 << i) != 0 {
                let p = self.priority_of(i);
                if p < best_priority {
                    best_priority = p;
                    best_irq = Some(i);
                }
            }
        }
        best_irq
    }

    /// Called by a device to request an interrupt on an IRQ line (0-7).
    /// The request waits in the IRR until it is acknowledged or cleared.
    pub fn request_interrupt(&mut self, irq: u8) {
        assert!(irq < 8);
        self.irr |= 1u8 << irq;
    }

    /// Called by a device to withdraw its request on an IRQ line (0-7).
    pub fn clear_interrupt(&mut self, irq: u8) {
        assert!(irq < 8);
        self.irr &= !(1u8 << irq);
    }

    /// The in-service levels that take part in priority resolution. In
    /// special mask mode a masked level is left out: it does not hold back
    /// lower levels, and a non-specific EOI does not end it.
    fn nested_isr(&self) -> u8 {
        if self.special_mask {
            self.isr & !self.imr
        } else {
            self.isr
        }
    }

    /// Returns true if the PIC has a pending, unmasked interrupt
    /// that is higher priority than anything currently in service.
    ///
    /// The run loop asks this after every instruction and every iteration
    /// of a repeated string instruction, and nearly always there is no
    /// unmasked request at all. That case is decided inline; the priority
    /// resolution stays out of line.
    #[inline(always)]
    pub fn has_interrupt(&self) -> bool {
        self.irr & !self.imr != 0 && self.pending_outranks_in_service()
    }

    /// Whether the highest-priority unmasked request outranks everything
    /// that is in service.
    #[inline(never)]
    fn pending_outranks_in_service(&self) -> bool {
        let pending = self.irr & !self.imr;
        let highest_pending = match self.highest_priority_bit(pending) {
            Some(irq) => irq,
            None => return false,
        };
        let in_service = self.nested_isr();
        if in_service == 0 {
            return true;
        }
        let highest_in_service = match self.highest_priority_bit(in_service) {
            Some(irq) => irq,
            None => return true,
        };
        // Pending interrupt must be strictly higher priority (lower number) than in-service
        let pending_priority = self.priority_of(highest_pending);
        let in_service_priority = self.priority_of(highest_in_service);
        debug!(
            "PIC: pending IRQ{} (prio {}, vector {:02X}), in-service IRQ{} (prio {}, vector {:02X})",
            highest_pending,
            pending_priority,
            self.vector_base + highest_pending,
            highest_in_service,
            in_service_priority,
            self.vector_base + highest_in_service
        );
        pending_priority < in_service_priority
    }

    /// Acknowledge the highest-priority pending interrupt.
    /// Returns the interrupt vector number for the CPU to call.
    /// Moves the IRQ from IRR to ISR (unless auto-EOI).
    pub fn acknowledge(&mut self) -> u8 {
        match self.acknowledge_irq() {
            Some(irq) => self.vector_base + irq,
            // Spurious — return IRQ7 vector (standard 8259 behavior)
            None => self.vector_base + 7,
        }
    }

    /// Takes the highest-priority pending request into service and returns
    /// its level, or None when nothing is pending.
    fn acknowledge_irq(&mut self) -> Option<u8> {
        let irq = self.highest_priority_bit(self.irr & !self.imr)?;
        let mask = 1u8 << irq;

        // In edge-triggered mode, clear IRR (the edge has been consumed).
        // In level-triggered mode, do NOT clear IRR — the line is still held high.
        if self.trigger_mode == TriggerMode::Edge {
            self.irr &= !mask;
        }

        self.isr |= mask;

        if self.auto_eoi {
            self.isr &= !mask;
            if self.rotate_on_aeoi {
                self.lowest_priority = irq;
            }
        }

        Some(irq)
    }

    /// Answers a poll command. The read stands in for the interrupt
    /// acknowledge: the highest-priority request is taken into service and
    /// its level returned in bits 2-0, with bit 7 set. Zero means that
    /// nothing needed service.
    fn poll_read(&mut self) -> u8 {
        self.poll = false;
        if !self.has_interrupt() {
            return 0;
        }
        match self.acknowledge_irq() {
            Some(irq) => 0x80 | irq,
            None => 0,
        }
    }

    /// Perform a non-specific EOI: clear the highest-priority ISR bit.
    fn non_specific_eoi(&mut self) -> Option<u8> {
        let irq = self.highest_priority_bit(self.nested_isr())?;
        self.isr &= !(1 << irq);
        Some(irq)
    }

    /// Perform a specific EOI: clear the specified ISR bit.
    fn specific_eoi(&mut self, irq: u8) {
        self.isr &= !(1 << irq);
    }

    fn write_command(&mut self, value: u8) {
        if value & 0x10 != 0 {
            // ICW1 — start initialization sequence
            self.icw_step = 1;
            self.icw4_expected = value & 0x01 != 0;
            self.single_pic = value & 0x02 != 0;

            // ICW1 bit 3: trigger mode (0 = edge, 1 = level)
            self.trigger_mode = if value & 0x08 != 0 {
                TriggerMode::Level
            } else {
                TriggerMode::Edge
            };

            // Reset state on ICW1
            self.irr = 0;
            self.isr = 0;
            self.imr = 0;
            self.lowest_priority = 7;
            self.rotate_on_aeoi = false;
            self.read_register = 0; // status reads return the IRR
            self.poll = false;
            self.special_mask = false;
            if !self.icw4_expected {
                // Without an ICW4 its options take their defaults.
                self.auto_eoi = false;
            }
        } else if value & 0x08 == 0 {
            // OCW2 — EOI and rotation commands (bit 4=0, bit 3=0)
            let op = (value >> 5) & 0x07;
            let irq = value & 0x07;
            match op {
                // 000: Rotate in auto-EOI mode CLEAR
                0b000 => {
                    self.rotate_on_aeoi = false;
                }
                // 001: Non-specific EOI
                0b001 => {
                    self.non_specific_eoi();
                }
                // 011: Specific EOI
                0b011 => {
                    self.specific_eoi(irq);
                }
                // 100: Rotate in auto-EOI mode SET
                0b100 => {
                    self.rotate_on_aeoi = true;
                }
                // 101: Rotate on non-specific EOI
                0b101 => {
                    if let Some(cleared_irq) = self.non_specific_eoi() {
                        self.lowest_priority = cleared_irq;
                    }
                }
                // 110: Set priority command (no EOI)
                0b110 => {
                    self.lowest_priority = irq;
                }
                // 111: Rotate on specific EOI
                0b111 => {
                    self.specific_eoi(irq);
                    self.lowest_priority = irq;
                }
                // 010: NOP / reserved
                _ => {}
            }
        } else {
            // OCW3 (bit 4=0, bit 3=1)
            if value & 0x02 != 0 {
                // RR=1: select read register
                self.read_register = value & 0x01; // 0=IRR, 1=ISR
            }
            if value & 0x04 != 0 {
                // P=1: poll command, answered by the next read
                self.poll = true;
            }
            if value & 0x40 != 0 {
                // ESMM=1: SMM sets or clears special mask mode
                self.special_mask = value & 0x20 != 0;
            }
        }
    }

    fn write_data(&mut self, value: u8) {
        if self.icw_step > 0 {
            match self.icw_step {
                1 => {
                    // ICW2 — vector base (upper 5 bits matter)
                    self.vector_base = value & 0xF8;
                    if self.single_pic {
                        // SNGL=1: no ICW3 needed, skip to ICW4 or done
                        self.icw_step = if self.icw4_expected { 3 } else { 0 };
                    } else {
                        // SNGL=0 (cascade): ICW3 is next
                        self.icw_step = 2;
                    }
                }
                2 => {
                    // ICW3 — cascade configuration (consumed, not used in emulation)
                    self.icw_step = if self.icw4_expected { 3 } else { 0 };
                }
                3 => {
                    // ICW4. Only auto-EOI (bit 1) changes anything here:
                    // the controller always works in 8086 mode (bit 0), and
                    // buffered mode (bit 3) concerns a pin that is not
                    // modelled.
                    self.auto_eoi = value & 0x02 != 0;
                    self.icw_step = 0;
                }
                _ => {
                    self.icw_step = 0;
                }
            }
        } else {
            // OCW1 — write to IMR
            self.imr = value;
        }
    }

    fn read_command(&self) -> u8 {
        if self.read_register == 1 {
            self.isr
        } else {
            self.irr
        }
    }

    fn read_data(&self) -> u8 {
        self.imr
    }
}

pub struct PicDevice;

impl IoDevice for PicDevice {
    fn port_in_byte(&mut self, port: u16, cpu: &mut Cpu) -> u8 {
        // After a poll command the next read, of either port, is the poll.
        if cpu.pic.poll {
            return cpu.pic.poll_read();
        }
        match port & 0x01 {
            0 => cpu.pic.read_command(), // 0x20
            _ => cpu.pic.read_data(),    // 0x21
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8, cpu: &mut Cpu) {
        match port & 0x01 {
            0 => cpu.pic.write_command(value), // 0x20
            _ => cpu.pic.write_data(value),    // 0x21
        }
    }
}

#[cfg(test)]
#[path = "pic_tests.rs"]
mod tests;
