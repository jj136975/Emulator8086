use super::*;
use crate::vm::runtime::Runtime;

#[test]
fn address_and_count_registers_read_back_through_the_flip_flop() {
    let mut rt = Runtime::new_test();
    let mut dma = DmaController::new();
    for channel in 0..4u16 {
        let address_port = channel * 2;
        let count_port = channel * 2 + 1;
        let address = 0x1234 + channel * 0x1111;
        let count = 0xA0B0 + channel;

        dma.port_out_byte(0x0C, 0, &mut rt.cpu); // clear the flip-flop
        dma.port_out_byte(address_port, address as u8, &mut rt.cpu);
        dma.port_out_byte(address_port, (address >> 8) as u8, &mut rt.cpu);
        dma.port_out_byte(count_port, count as u8, &mut rt.cpu);
        dma.port_out_byte(count_port, (count >> 8) as u8, &mut rt.cpu);

        let lo = dma.port_in_byte(address_port, &mut rt.cpu) as u16;
        let hi = dma.port_in_byte(address_port, &mut rt.cpu) as u16;
        assert_eq!(lo | (hi << 8), address, "channel {channel} address");
        let lo = dma.port_in_byte(count_port, &mut rt.cpu) as u16;
        let hi = dma.port_in_byte(count_port, &mut rt.cpu) as u16;
        assert_eq!(lo | (hi << 8), count, "channel {channel} count");
    }
}

#[test]
fn clearing_the_flip_flop_selects_the_low_byte_again() {
    let mut rt = Runtime::new_test();
    let mut dma = DmaController::new();
    dma.port_out_byte(0x04, 0x34, &mut rt.cpu);
    dma.port_out_byte(0x04, 0x12, &mut rt.cpu);

    assert_eq!(dma.port_in_byte(0x04, &mut rt.cpu), 0x34);
    dma.port_out_byte(0x0C, 0, &mut rt.cpu);
    assert_eq!(
        dma.port_in_byte(0x04, &mut rt.cpu),
        0x34,
        "low byte again, not the high byte"
    );
    assert_eq!(dma.port_in_byte(0x04, &mut rt.cpu), 0x12);
}

#[test]
fn status_is_cleared_by_reading_it_and_by_master_clear() {
    let mut rt = Runtime::new_test();
    let mut dma = DmaController::new();
    assert_eq!(dma.port_in_byte(0x08, &mut rt.cpu), 0x0F);
    assert_eq!(dma.port_in_byte(0x08, &mut rt.cpu), 0x00);

    let mut dma = DmaController::new();
    dma.port_out_byte(0x02, 0x55, &mut rt.cpu); // leaves the flip-flop on the high byte
    dma.port_out_byte(0x0D, 0, &mut rt.cpu); // master clear
    assert_eq!(dma.port_in_byte(0x08, &mut rt.cpu), 0x00);
    assert_eq!(
        dma.port_in_byte(0x02, &mut rt.cpu),
        0x55,
        "flip-flop is back on the low byte"
    );
}

#[test]
fn temporary_register_reads_zero_and_write_only_ports_read_ff() {
    let mut rt = Runtime::new_test();
    let mut dma = DmaController::new();
    assert_eq!(dma.port_in_byte(0x0D, &mut rt.cpu), 0x00);
    for port in [0x09, 0x0A, 0x0B, 0x0C, 0x0E, 0x0F] {
        assert_eq!(
            dma.port_in_byte(port, &mut rt.cpu),
            0xFF,
            "port {port:02X}h"
        );
    }
}

#[test]
fn page_registers_hold_one_byte_per_channel() {
    let mut rt = Runtime::new_test();
    let mut pages = DmaPageRegisters::new();
    for (port, value) in [(0x87, 0x10), (0x83, 0x21), (0x81, 0x32), (0x82, 0x43)] {
        pages.port_out_byte(port, value, &mut rt.cpu);
    }
    for (port, value) in [(0x87, 0x10), (0x83, 0x21), (0x81, 0x32), (0x82, 0x43)] {
        assert_eq!(
            pages.port_in_byte(port, &mut rt.cpu),
            value,
            "port {port:02X}h"
        );
    }
    assert_eq!(
        pages.port_in_byte(0x84, &mut rt.cpu),
        0xFF,
        "no register at 84h"
    );
}
