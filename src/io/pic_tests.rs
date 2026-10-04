use super::*;
use crate::vm::runtime::Runtime;

/// ICW1 with SNGL=1 (bit 1) selects a single PIC: ICW3 is skipped, so
/// initialization completes after ICW1, ICW2 and ICW4, and the data write
/// after those is OCW1, the interrupt mask.
#[test]
fn icw1_sngl_bit_means_single_pic_no_icw3_needed() {
    let mut rt = Runtime::new_test();
    let mut dev = PicDevice;
    // ICW1 with SNGL=1, ICW4 expected: 0x10 | 0x02 | 0x01 = 0x13
    dev.port_out_byte(0x20, 0x13, &mut rt.cpu);
    // ICW2: vector base 0x08
    dev.port_out_byte(0x21, 0x08, &mut rt.cpu);
    // SNGL=1: no ICW3, so this data write is ICW4.
    dev.port_out_byte(0x21, 0x01, &mut rt.cpu); // ICW4: 8086 mode

    dev.port_out_byte(0x21, 0xAB, &mut rt.cpu);
    assert_eq!(
        dev.port_in_byte(0x21, &mut rt.cpu),
        0xAB,
        "a controller still waiting for ICW3 would have taken the mask as an initialisation word"
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

/// A controller initialised with the BIOS's settings (edge-triggered,
/// vectors from 08h, normal EOI) and every line unmasked.
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

/// ICW1 starts the set-up afresh: status reads return the IRR again, and
/// without an ICW4 auto-EOI is off.
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

/// A poll command (OCW3 with P set) makes the next read, of either port,
/// an acknowledge: it takes the highest request into service and returns
/// 80h plus its level, or 00h when nothing needs service.
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

/// In special mask mode (OCW3 with ESMM and SMM set) a level that is in
/// service but masked holds back no lower level, and a non-specific EOI
/// passes over it.
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
