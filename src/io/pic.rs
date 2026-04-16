use log::debug;
use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;

/// Intel 8259A Programmable Interrupt Controller
///
/// Handles a single PIC (master). For a full PC/AT you'd add a slave,
/// but a single master is sufficient for MS-DOS on an 8086/8088.
///
/// Ports (master): 0x20 (command), 0x21 (data)

#[derive(Clone, Copy, PartialEq)]
enum TriggerMode {
    Edge,
    Level,
}

pub struct Pic {
    /// Physical IR line state — reflects current electrical state of each IRQ line
    ir: u8,
    /// Interrupt Request Register — bits set by edge transitions or level-held lines
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
    /// 8086/8088 mode (ICW4 bit 0) — stored but always treated as 8086 mode
    mode_8086: bool,
    /// Buffered mode (ICW4 bit 3) — stored but not functionally used
    buffered: bool,
    /// Which register to return on port 0x20 read: 0=IRR, 1=ISR
    read_register: u8,

    /// Set after full initialization
    initialized: bool,

    /// Edge vs level trigger mode, set from ICW1 bit 3
    trigger_mode: TriggerMode,
    /// Output INTR line state — true when there is a serviceable interrupt
    intr: bool,
    /// Lowest priority IRQ (default 7). Highest priority = (lowest_priority + 1) % 8.
    lowest_priority: u8,
    /// Whether to rotate priorities on auto-EOI
    rotate_on_aeoi: bool,
}

impl Pic {
    pub fn new() -> Self {
        Self {
            ir: 0,
            irr: 0,
            isr: 0,
            imr: 0xFF, // all masked until initialized
            vector_base: 0x08, // default IBM PC mapping: IRQ0 = INT 08h
            icw_step: 0,
            icw4_expected: false,
            single_pic: false,
            auto_eoi: false,
            mode_8086: true,
            buffered: false,
            read_register: 0,
            initialized: false,
            trigger_mode: TriggerMode::Edge,
            intr: false,
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

    /// Called by a device to raise (assert) an IRQ line (0-7).
    /// In edge-triggered mode, only sets IRR on a rising edge (0->1 transition).
    /// In level-triggered mode, always sets IRR while line is held high.
    pub fn raise_irq(&mut self, irq: u8) {
        assert!(irq < 8);
        let mask = 1u8 << irq;
        let was_low = (self.ir & mask) == 0;
        self.ir |= mask;

        match self.trigger_mode {
            TriggerMode::Edge => {
                // Only set IRR on rising edge (was low, now high)
                if was_low {
                    self.irr |= mask;
                }
            }
            TriggerMode::Level => {
                // Level mode: IRR tracks the line state while high
                self.irr |= mask;
            }
        }
        self.recalc_intr();
    }

    /// Called by a device to lower (deassert) an IRQ line (0-7).
    /// In level-triggered mode, also clears the IRR bit.
    pub fn lower_irq(&mut self, irq: u8) {
        assert!(irq < 8);
        let mask = 1u8 << irq;
        self.ir &= !mask;

        if self.trigger_mode == TriggerMode::Level {
            // In level mode, IRR follows the line — when line goes low, clear IRR
            self.irr &= !mask;
        }
        self.recalc_intr();
    }

    /// Pulse an interrupt request regardless of previous line state.
    /// Useful for edge-triggered devices that need to force an IRR bit set
    /// without caring about the previous IR state.
    pub fn request_interrupt(&mut self, irq: u8) {
        assert!(irq < 8);
        let mask = 1u8 << irq;
        self.ir |= mask;
        self.irr |= mask;
        self.recalc_intr();
    }

    /// Clear both the physical IR line and the IRR bit for a given IRQ.
    pub fn clear_interrupt(&mut self, irq: u8) {
        assert!(irq < 8);
        let mask = 1u8 << irq;
        self.ir &= !mask;
        self.irr &= !mask;
        self.recalc_intr();
    }

    /// Returns true if the PIC has a pending, unmasked interrupt
    /// that is higher priority than anything currently in service.
    pub fn has_interrupt(&self) -> bool {
        let pending = self.irr & !self.imr;
        if pending == 0 {
            return false;
        }
        // Find highest priority pending IRQ
        let highest_pending = match self.highest_priority_bit(pending) {
            Some(irq) => irq,
            None => return false,
        };
        // If nothing in service, any pending interrupt qualifies
        if self.isr == 0 {
            return true;
        }
        // Find highest priority in-service IRQ
        let highest_in_service = match self.highest_priority_bit(self.isr) {
            Some(irq) => irq,
            None => return true,
        };
        // Pending interrupt must be strictly higher priority (lower number) than in-service
        let pending_priority = self.priority_of(highest_pending);
        let in_service_priority = self.priority_of(highest_in_service);
        debug!("PIC: pending IRQ{} (prio {}, vector {:02X}), in-service IRQ{} (prio {}, vector {:02X})",
            highest_pending, pending_priority, self.vector_base + highest_pending,
            highest_in_service, in_service_priority, self.vector_base + highest_in_service);
        pending_priority < in_service_priority
    }

    /// Acknowledge the highest-priority pending interrupt.
    /// Returns the interrupt vector number for the CPU to call.
    /// Moves the IRQ from IRR to ISR (unless auto-EOI).
    pub fn acknowledge(&mut self) -> u8 {
        let pending = self.irr & !self.imr;
        if pending == 0 {
            // Spurious — return IRQ7 vector (standard 8259 behavior)
            return self.vector_base + 7;
        }
        let irq = match self.highest_priority_bit(pending) {
            Some(irq) => irq,
            None => return self.vector_base + 7,
        };
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

        self.recalc_intr();
        self.vector_base + irq
    }

    /// Recalculate the INTR output line state.
    fn recalc_intr(&mut self) {
        self.intr = self.has_interrupt();
    }

    /// Perform a non-specific EOI: clear the highest-priority ISR bit.
    fn non_specific_eoi(&mut self) -> Option<u8> {
        if let Some(irq) = self.highest_priority_bit(self.isr) {
            self.isr &= !(1 << irq);
            self.recalc_intr();
            Some(irq)
        } else {
            None
        }
    }

    /// Perform a specific EOI: clear the specified ISR bit.
    fn specific_eoi(&mut self, irq: u8) {
        self.isr &= !(1 << irq);
        self.recalc_intr();
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
            self.ir = 0;
            self.irr = 0;
            self.isr = 0;
            self.imr = 0;
            self.lowest_priority = 7;
            self.rotate_on_aeoi = false;
            self.initialized = false;
            self.intr = false;
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
                    self.recalc_intr();
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
                        self.recalc_intr();
                    }
                }
                2 => {
                    // ICW3 — cascade configuration (consumed, not used in emulation)
                    self.icw_step = if self.icw4_expected { 3 } else { 0 };
                    if self.icw_step == 0 {
                        self.initialized = true;
                        self.recalc_intr();
                    }
                }
                3 => {
                    // ICW4
                    // Bit 0: 8086/8088 mode (vs MCS-80/85 mode)
                    self.mode_8086 = value & 0x01 != 0;
                    // Bit 1: auto-EOI
                    self.auto_eoi = value & 0x02 != 0;
                    // Bit 3: buffered mode
                    self.buffered = value & 0x08 != 0;
                    self.icw_step = 0;
                    self.initialized = true;
                    self.recalc_intr();
                }
                _ => {
                    self.icw_step = 0;
                    self.initialized = true;
                    self.recalc_intr();
                }
            }
        } else {
            // OCW1 — write to IMR
            self.imr = value;
            self.recalc_intr();
        }
    }

    fn read_command(&self) -> u8 {
        if self.read_register == 1 { self.isr } else { self.irr }
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

// ---------------------------------------------------------------------------
// Inspection tests — 8259A state-machine invariants.
// ---------------------------------------------------------------------------
#[cfg(test)]
mod inspection_tests {
    use super::*;
    use crate::vm::runtime::Runtime;

    /// FINDING (Phase 3): ICW1 bit 1 (SNGL) interpretation was claimed to be
    /// "reversed." Per Intel 8259A datasheet: SNGL=1 means single PIC (no
    /// ICW3), SNGL=0 means cascade (ICW3 required).
    /// STATUS: PASSES → retracted. `pic.rs:262` has `single_pic = value & 0x02 != 0`
    /// which is correct. We verify the ICW state-machine progression.
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

    /// FINDING (Phase 3): Single-PIC emulation handles IRQ0 and IRQ1 (timer
    /// + keyboard) but IRQ8-15 would need a slave PIC. Document this.
    /// STATUS: acceptable limitation for 8086 MS-DOS — this test just
    /// confirms IRQ0 path works end-to-end.
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
}
