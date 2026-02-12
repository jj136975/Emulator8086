use crate::bios::handlers::*;
use crate::vm::memory::{BDA_BASE, BIOS_ROM};
use crate::vm::runtime::Runtime;

pub fn init_bios(vm: &mut Runtime) {
    // Register BIOS interrupt handlers
    vm.bios_handlers[0x10] = Some(int10h);
    vm.bios_handlers[0x11] = Some(int11h);
    vm.bios_handlers[0x12] = Some(int12h);
    vm.bios_handlers[0x13] = Some(int13h);
    vm.bios_handlers[0x16] = Some(int16h);
    vm.bios_handlers[0x19] = Some(int19h);
    vm.bios_handlers[0x1A] = Some(int1ah);

    // Write IRET stubs in ROM area for hardware IRQ handlers
    // Each stub is just an IRET (0xCF) opcode
    let iret_base = BIOS_ROM + 0x100;
    for i in 0..16u16 {
        let stub_addr = iret_base + i as usize;
        vm.memory.write_byte(stub_addr, 0xCF); // IRET
    }

    // Set up IVT entries for hardware IRQs (INT 8-15 -> IRQ 0-7)
    for i in 0..8u16 {
        let vector = 8 + i;
        let ivt_offset = (vector * 4) as usize;
        let stub_addr = (iret_base + i as usize) as u16;
        let stub_seg = (BIOS_ROM >> 4) as u16;
        vm.memory.write_word(ivt_offset, stub_addr & 0xFFFF);
        vm.memory.write_word(ivt_offset + 2, stub_seg);
    }

    // Also set IVT entries for BIOS service interrupts to point to stubs
    // (These will be intercepted by the bios_handlers array before IVT lookup)
    let bios_vectors: &[u16] = &[0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A];
    for (idx, &vector) in bios_vectors.iter().enumerate() {
        let ivt_offset = (vector * 4) as usize;
        let stub_addr = (iret_base + 16 + idx) as u16;
        let stub_seg = (BIOS_ROM >> 4) as u16;
        vm.memory.write_byte(BIOS_ROM + 0x110 + idx, 0xCF); // IRET stub
        vm.memory.write_word(ivt_offset, stub_addr & 0xFFFF);
        vm.memory.write_word(ivt_offset + 2, stub_seg);
    }

    // Initialize BDA
    vm.memory.write_byte(BDA_BASE + 0x49, 0x03);     // Video mode = 3 (80x25 color)
    vm.memory.write_word(BDA_BASE + 0x4A, 80);        // Columns per row
    vm.memory.write_word(BDA_BASE + 0x4C, 4000);      // Video buffer size
    vm.memory.write_word(BDA_BASE + 0x13, 640);       // Memory size in KB
    vm.memory.write_word(BDA_BASE + 0x10, 0x0021);    // Equipment word (1 floppy, 80x25 color)
    vm.memory.write_byte(BDA_BASE + 0x50, 0);         // Cursor col page 0
    vm.memory.write_byte(BDA_BASE + 0x51, 0);         // Cursor row page 0

    // Set up diskette parameter table at a fixed location in ROM
    // INT 1Eh vector (0x78-0x7B) should point to this table
    let dpt_addr = BIOS_ROM + 0x200;
    // Standard 1.44MB floppy parameter table (11 bytes)
    let dpt: [u8; 11] = [
        0xDF, // SRT=D, HUT=F (step rate time, head unload time)
        0x02, // HLT=01, DMA=1 (head load time, DMA mode)
        0x25, // Motor wait time (ticks)
        0x02, // Bytes per sector (2 = 512)
        0x12, // Sectors per track (18)
        0x1B, // Gap length
        0xFF, // Data length
        0x6C, // Format gap length
        0xF6, // Fill byte for format
        0x0F, // Head settle time (ms)
        0x08, // Motor start time (1/8 seconds)
    ];
    for (i, &b) in dpt.iter().enumerate() {
        vm.memory.write_byte(dpt_addr + i, b);
    }
    // Set INT 1Eh vector to point to the DPT
    let ivt_1e = 0x1E * 4;
    vm.memory.write_word(ivt_1e, (dpt_addr & 0xFFFF) as u16);
    vm.memory.write_word(ivt_1e + 2, (BIOS_ROM >> 4) as u16);

    // Clear VGA text buffer
    for i in 0..2000 {
        vm.memory.write_byte(crate::vm::memory::VGA_TEXT_BASE + i * 2, b' ');
        vm.memory.write_byte(crate::vm::memory::VGA_TEXT_BASE + i * 2 + 1, 0x07);
    }

    // Bootstrap: load boot sector
    int19h(vm);
}
