//! Intel 8259A Programmable Interrupt Controller
//!
//! Handles a single PIC (master). For a full PC/AT you'd add a slave,
//! but a single master is sufficient for MS-DOS on an 8086/8088.
//!
//! Ports (master): 0x20 (command), 0x21 (data)

use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use log::debug;

#[derive(Clone, Copy, PartialEq)]
enum TriggerMode {
    Edge,
    Level,
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

    /// ICW initialization state machine
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

    /// Set after full initialization
    initialized: bool,

    /// Edge vs level trigger mode, set from ICW1 bit 3
    trigger_mode: TriggerMode,
    /// Lowest priority IRQ (default 7). Highest priority = (lowest_priority + 1) % 8.
    lowest_priority: u8,
    /// Whether to rotate priorities on auto-EOI
    rotate_on_aeoi: bool,
}

impl Pic {
    pub fn new() -> Self {
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
            initialized: false,
            trigger_mode: TriggerMode::Edge,
            lowest_priority: 7,
            rotate_on_aeoi: false,
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
        // Find highest priority pending IRQ
        let highest_pending = match self.highest_priority_bit(pending) {
            Some(irq) => irq,
            None => return false,
        };
        // If nothing in service, any pending interrupt qualifies
        let in_service = self.nested_isr();
        if in_service == 0 {
            return true;
        }
        // Find highest priority in-service IRQ
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

        // Set ISR bit to indicate this interrupt is being serviced
        self.isr |= mask;

        // If auto-EOI, immediately clear ISR
        if self.auto_eoi {
            self.isr &= !mask;
            // Rotate priorities if rotate-on-auto-EOI is enabled
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
            self.initialized = false;
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
                    if self.icw_step == 0 {
                        self.initialized = true;
                    }
                }
                2 => {
                    // ICW3 — cascade configuration (consumed, not used in emulation)
                    self.icw_step = if self.icw4_expected { 3 } else { 0 };
                    if self.icw_step == 0 {
                        self.initialized = true;
                    }
                }
                3 => {
                    // ICW4. Only auto-EOI (bit 1) changes anything here:
                    // the controller always works in 8086 mode (bit 0), and
                    // buffered mode (bit 3) concerns a pin that is not
                    // modelled.
                    self.auto_eoi = value & 0x02 != 0;
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

// ---------------------------------------------------------------------------
// Regression tests — 8259A state-machine invariants.
// ---------------------------------------------------------------------------
#[cfg(test)]
mod inspection_tests {
    use super::*;
    use crate::vm::runtime::Runtime;

    /// ICW1 with SNGL=1 (bit 1) selects a single PIC: ICW3 is skipped, so
    /// initialization completes after ICW1, ICW2 and ICW4.
    #[test]
    fn icw1_sngl_bit_means_single_pic_no_icw3_needed() {
        let mut rt = Runtime::new_test();
        let mut dev = PicDevice;
        // ICW1 with SNGL=1, ICW4 expected: 0x10 | 0x02 | 0x01 = 0x13
        dev.port_out_byte(0x20, 0x13, &mut rt.cpu);
        // ICW2: vector base 0x08
        dev.port_out_byte(0x21, 0x08, &mut rt.cpu);
        // Because SNGL=1, ICW3 is skipped. Next data write is ICW4 directly.
        // If interpretation were reversed, ICW3 would still be expected here
        // and the init sequence would not complete until a 4th write.
        dev.port_out_byte(0x21, 0x01, &mut rt.cpu); // ICW4: 8086 mode
        assert!(
            rt.cpu.pic.initialized,
            "After ICW1(SNGL=1)+ICW2+ICW4 (3 writes), PIC should be initialized. \
             If single_pic semantics were reversed, the PIC would be waiting for ICW3."
        );
    }

    /// After initialization with vector base 0x08, a raised IRQ0 is pending
    /// and is acknowledged as INT 08h.
    #[test]
    fn irq0_routes_to_vector_0x08_after_init() {
        let mut rt = Runtime::new_test();
        let mut dev = PicDevice;
        dev.port_out_byte(0x20, 0x13, &mut rt.cpu);
        dev.port_out_byte(0x21, 0x08, &mut rt.cpu);
        dev.port_out_byte(0x21, 0x01, &mut rt.cpu);
        dev.port_out_byte(0x21, 0x00, &mut rt.cpu); // unmask all

        rt.cpu.pic.request_interrupt(0);
        assert!(rt.cpu.pic.has_interrupt());
        let vector = rt.cpu.pic.acknowledge();
        assert_eq!(vector, 0x08, "IRQ0 with vector_base=0x08 → INT 08h");
    }

    /// A controller initialised the way the BIOS does it (edge-triggered,
    /// vectors from 08h, normal EOI) with every line unmasked.
    fn initialized() -> (Runtime, PicDevice) {
        let mut rt = Runtime::new_test();
        let mut dev = PicDevice;
        for (port, value) in [(0x20, 0x13), (0x21, 0x08), (0x21, 0x01), (0x21, 0x00)] {
            dev.port_out_byte(port, value, &mut rt.cpu);
        }
        (rt, dev)
    }

    #[test]
    fn higher_priority_request_preempts_and_lower_waits() {
        let (mut rt, mut dev) = initialized();
        rt.cpu.pic.request_interrupt(1);
        assert_eq!(rt.cpu.pic.acknowledge(), 0x09);

        rt.cpu.pic.request_interrupt(3);
        assert!(
            !rt.cpu.pic.has_interrupt(),
            "IRQ3 waits while IRQ1 is in service"
        );
        rt.cpu.pic.request_interrupt(0);
        assert!(rt.cpu.pic.has_interrupt(), "IRQ0 preempts IRQ1");
        assert_eq!(rt.cpu.pic.acknowledge(), 0x08);

        dev.port_out_byte(0x20, 0x20, &mut rt.cpu); // non-specific EOI ends IRQ0
        assert_eq!(rt.cpu.pic.isr, 0x02);
        dev.port_out_byte(0x20, 0x61, &mut rt.cpu); // specific EOI for IRQ1
        assert_eq!(rt.cpu.pic.isr, 0x00);
        assert_eq!(rt.cpu.pic.acknowledge(), 0x0B, "IRQ3 is served last");
    }

    #[test]
    fn rotate_on_non_specific_eoi_makes_the_served_line_lowest_priority() {
        let (mut rt, mut dev) = initialized();
        rt.cpu.pic.request_interrupt(3);
        assert_eq!(rt.cpu.pic.acknowledge(), 0x0B);
        dev.port_out_byte(0x20, 0xA0, &mut rt.cpu); // rotate on non-specific EOI

        rt.cpu.pic.request_interrupt(0);
        rt.cpu.pic.request_interrupt(4);
        assert_eq!(rt.cpu.pic.acknowledge(), 0x0C, "IRQ4 now outranks IRQ0");
    }

    #[test]
    fn masked_request_is_delivered_once_unmasked() {
        let (mut rt, mut dev) = initialized();
        dev.port_out_byte(0x21, 0x02, &mut rt.cpu);
        rt.cpu.pic.request_interrupt(1);
        assert!(!rt.cpu.pic.has_interrupt());
        assert_eq!(
            dev.port_in_byte(0x21, &mut rt.cpu),
            0x02,
            "port 21h reads the mask"
        );

        dev.port_out_byte(0x21, 0x00, &mut rt.cpu);
        assert!(rt.cpu.pic.has_interrupt());
    }

    #[test]
    fn acknowledge_without_a_request_returns_the_irq7_vector() {
        let (mut rt, _dev) = initialized();
        assert_eq!(rt.cpu.pic.acknowledge(), 0x0F);
        assert_eq!(
            rt.cpu.pic.isr, 0x00,
            "a default IRQ7 is not marked in service"
        );
    }

    #[test]
    fn auto_eoi_ends_the_interrupt_at_acknowledge() {
        let mut rt = Runtime::new_test();
        let mut dev = PicDevice;
        for (port, value) in [(0x20, 0x13), (0x21, 0x08), (0x21, 0x03), (0x21, 0x00)] {
            dev.port_out_byte(port, value, &mut rt.cpu);
        }
        rt.cpu.pic.request_interrupt(1);
        assert_eq!(rt.cpu.pic.acknowledge(), 0x09);
        assert_eq!(rt.cpu.pic.isr, 0x00);
    }

    #[test]
    fn status_read_returns_the_register_selected_by_ocw3() {
        let (mut rt, mut dev) = initialized();
        rt.cpu.pic.request_interrupt(0);
        rt.cpu.pic.acknowledge();
        rt.cpu.pic.request_interrupt(2);

        assert_eq!(
            dev.port_in_byte(0x20, &mut rt.cpu),
            0x04,
            "IRR after initialisation"
        );
        dev.port_out_byte(0x20, 0x0B, &mut rt.cpu);
        assert_eq!(dev.port_in_byte(0x20, &mut rt.cpu), 0x01, "ISR");
        dev.port_out_byte(0x20, 0x0A, &mut rt.cpu);
        assert_eq!(dev.port_in_byte(0x20, &mut rt.cpu), 0x04, "IRR");
    }

    #[test]
    fn level_triggered_request_stays_until_the_line_is_released() {
        let mut rt = Runtime::new_test();
        let mut dev = PicDevice;
        for (port, value) in [(0x20, 0x1B), (0x21, 0x08), (0x21, 0x01), (0x21, 0x00)] {
            dev.port_out_byte(port, value, &mut rt.cpu);
        }
        rt.cpu.pic.request_interrupt(2);
        assert_eq!(rt.cpu.pic.acknowledge(), 0x0A);
        dev.port_out_byte(0x20, 0x20, &mut rt.cpu);
        assert!(
            rt.cpu.pic.has_interrupt(),
            "the line is still asserted after the EOI"
        );

        rt.cpu.pic.clear_interrupt(2);
        assert!(!rt.cpu.pic.has_interrupt());
    }

    /// Regression: ICW1 kept the status-read selection and the auto-EOI
    /// setting of the previous initialisation.
    #[test]
    fn icw1_selects_irr_for_status_reads_and_clears_auto_eoi_without_icw4() {
        let mut rt = Runtime::new_test();
        let mut dev = PicDevice;
        // First set-up: auto-EOI, then status reads switched to the ISR.
        for (port, value) in [
            (0x20, 0x13),
            (0x21, 0x08),
            (0x21, 0x03),
            (0x21, 0x00),
            (0x20, 0x0B),
        ] {
            dev.port_out_byte(port, value, &mut rt.cpu);
        }
        // Second set-up, this time without an ICW4.
        for (port, value) in [(0x20, 0x12), (0x21, 0x08), (0x21, 0x00)] {
            dev.port_out_byte(port, value, &mut rt.cpu);
        }

        rt.cpu.pic.request_interrupt(1);
        assert_eq!(
            dev.port_in_byte(0x20, &mut rt.cpu),
            0x02,
            "status reads return the IRR again"
        );
        assert_eq!(rt.cpu.pic.acknowledge(), 0x09);
        assert_eq!(
            rt.cpu.pic.isr, 0x02,
            "normal EOI: the interrupt stays in service"
        );
    }

    /// Regression: the poll command (OCW3 with P set) was ignored, and the
    /// read that should answer it returned the IRR.
    #[test]
    fn poll_command_makes_the_next_read_an_acknowledge() {
        let (mut rt, mut dev) = initialized();
        rt.cpu.pic.request_interrupt(3);
        rt.cpu.pic.request_interrupt(1);

        dev.port_out_byte(0x20, 0x0C, &mut rt.cpu);
        assert_eq!(
            dev.port_in_byte(0x20, &mut rt.cpu),
            0x81,
            "request present, level 1"
        );
        assert_eq!(rt.cpu.pic.isr, 0x02, "the polled level is in service");
        assert_eq!(
            dev.port_in_byte(0x20, &mut rt.cpu),
            0x08,
            "one read per poll command"
        );

        dev.port_out_byte(0x20, 0x20, &mut rt.cpu); // EOI for IRQ1
        dev.port_out_byte(0x20, 0x0C, &mut rt.cpu);
        assert_eq!(
            dev.port_in_byte(0x21, &mut rt.cpu),
            0x83,
            "either port answers the poll"
        );
        dev.port_out_byte(0x20, 0x20, &mut rt.cpu);

        dev.port_out_byte(0x20, 0x0C, &mut rt.cpu);
        assert_eq!(
            dev.port_in_byte(0x20, &mut rt.cpu),
            0x00,
            "nothing left to serve"
        );
        assert_eq!(rt.cpu.pic.isr, 0x00);
    }

    /// Regression: special mask mode (OCW3 with ESMM and SMM set) was
    /// ignored, so a handler that masked its own level could not let
    /// lower-priority interrupts through.
    #[test]
    fn special_mask_mode_lets_lower_levels_interrupt_a_masked_handler() {
        let (mut rt, mut dev) = initialized();
        rt.cpu.pic.request_interrupt(0);
        assert_eq!(rt.cpu.pic.acknowledge(), 0x08); // IRQ0 stays in service

        dev.port_out_byte(0x21, 0x01, &mut rt.cpu); // its handler masks IRQ0
        rt.cpu.pic.request_interrupt(1);
        assert!(
            !rt.cpu.pic.has_interrupt(),
            "fully nested: IRQ0 in service blocks IRQ1"
        );

        dev.port_out_byte(0x20, 0x68, &mut rt.cpu); // special mask mode on
        assert!(rt.cpu.pic.has_interrupt());
        assert_eq!(rt.cpu.pic.acknowledge(), 0x09);

        // A non-specific EOI ends IRQ1 and passes over the masked IRQ0.
        dev.port_out_byte(0x20, 0x20, &mut rt.cpu);
        assert_eq!(rt.cpu.pic.isr, 0x01);

        dev.port_out_byte(0x20, 0x48, &mut rt.cpu); // special mask mode off
        rt.cpu.pic.request_interrupt(1);
        assert!(!rt.cpu.pic.has_interrupt(), "fully nested again");
    }

    #[test]
    fn icw1_leaves_special_mask_mode() {
        let (mut rt, mut dev) = initialized();
        dev.port_out_byte(0x20, 0x68, &mut rt.cpu); // special mask mode on
        for (port, value) in [(0x20, 0x13), (0x21, 0x08), (0x21, 0x01), (0x21, 0x00)] {
            dev.port_out_byte(port, value, &mut rt.cpu);
        }

        rt.cpu.pic.request_interrupt(0);
        rt.cpu.pic.acknowledge();
        dev.port_out_byte(0x21, 0x01, &mut rt.cpu);
        rt.cpu.pic.request_interrupt(1);
        assert!(!rt.cpu.pic.has_interrupt());
    }
}
