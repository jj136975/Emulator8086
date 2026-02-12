use crate::bios::handlers::*;
use crate::vm::memory::{BDA_BASE, BIOS_ROM};
use crate::vm::runtime::Runtime;

pub fn init_bios(vm: &mut Runtime) {
    // Register BIOS interrupt handlers
    vm.bios_handlers[0x10] = Some(int10h);
    vm.bios_handlers[0x11] = Some(int11h);
    vm.bios_handlers[0x12] = Some(int12h);
    vm.bios_handlers[0x13] = Some(int13h);
    vm.bios_handlers[0x15] = Some(int15h);
    vm.bios_handlers[0x16] = Some(int16h);
    vm.bios_handlers[0x19] = Some(int19h);
    vm.bios_handlers[0x1A] = Some(int1ah);

    // Write 256 IRET stubs in ROM area — one per interrupt vector.
    // This ensures ANY unhandled INT (e.g. INT 2Fh, INT 33h) safely
    // executes IRET instead of jumping to 0000:0000 and crashing.
    let iret_base = BIOS_ROM + 0x100;
    let stub_seg = (BIOS_ROM >> 4) as u16;
    for i in 0..256u16 {
        let stub_addr = iret_base + i as usize;
        vm.memory.write_byte(stub_addr, 0xCF); // IRET

        let ivt_offset = (i * 4) as usize;
        vm.memory.write_word(ivt_offset, (stub_addr & 0xFFFF) as u16);
        vm.memory.write_word(ivt_offset + 2, stub_seg);
    }

    // Initialize BDA
    vm.memory.write_byte(BDA_BASE + 0x49, 0x03);     // Video mode = 3 (80x25 color)
    vm.memory.write_word(BDA_BASE + 0x4A, 80);        // Columns per row
    vm.memory.write_word(BDA_BASE + 0x4C, 4000);      // Video buffer size
    vm.memory.write_word(BDA_BASE + 0x13, 640);       // Memory size in KB

    // Equipment word: bit 0 = has floppies, bits 6-7 = (floppy_count - 1)
    // bits 4-5 = initial video mode (01 = 80x25 color)
    let floppy_count = vm.disks.iter().filter(|d| d.is_some()).count() as u16;
    let equip = if floppy_count > 0 {
        0x0001 | ((floppy_count - 1) << 6) | 0x0020 // has floppies + count + 80x25 color
    } else {
        0x0020 // just 80x25 color, no floppies
    };
    vm.memory.write_word(BDA_BASE + 0x10, equip);

    // Hard disk count at BDA 0x75
    let hd_count = vm.hard_disks.iter().filter(|d| d.is_some()).count() as u8;
    vm.memory.write_byte(BDA_BASE + 0x75, hd_count);

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

    // Set up Fixed Disk Parameter Table (FDPT) for hard disk 0 at INT 41h
    if let Some(Some(hd)) = vm.hard_disks.get(0) {
        let fdpt_addr = BIOS_ROM + 0x300;
        // 16-byte FDPT
        vm.memory.write_word(fdpt_addr, hd.cylinders);           // 00h: cylinders
        vm.memory.write_byte(fdpt_addr + 2, hd.heads);           // 02h: heads
        vm.memory.write_word(fdpt_addr + 3, 0);                  // 03h: reduced write current (unused)
        vm.memory.write_word(fdpt_addr + 5, hd.cylinders + 1);   // 05h: write precomp cylinder
        vm.memory.write_byte(fdpt_addr + 7, 0);                  // 07h: max ECC burst
        let control = if hd.heads > 8 { 0x08 } else { 0x00 };    // 08h: control byte
        vm.memory.write_byte(fdpt_addr + 8, control);
        vm.memory.write_byte(fdpt_addr + 9, 0);                  // 09h: standard timeout
        vm.memory.write_byte(fdpt_addr + 10, 0);                 // 0Ah: formatting timeout
        vm.memory.write_byte(fdpt_addr + 11, 0);                 // 0Bh: check timeout
        vm.memory.write_word(fdpt_addr + 12, hd.cylinders + 1);  // 0Ch: landing zone
        vm.memory.write_byte(fdpt_addr + 14, hd.sectors_per_track); // 0Eh: sectors per track
        vm.memory.write_byte(fdpt_addr + 15, 0);                 // 0Fh: reserved

        // Point INT 41h vector to FDPT
        let ivt_41 = 0x41 * 4;
        vm.memory.write_word(ivt_41, (fdpt_addr & 0xFFFF) as u16);
        vm.memory.write_word(ivt_41 + 2, (BIOS_ROM >> 4) as u16);
    }

    // Set up FDPT for hard disk 1 at INT 46h
    if let Some(Some(hd)) = vm.hard_disks.get(1) {
        let fdpt_addr = BIOS_ROM + 0x310;
        vm.memory.write_word(fdpt_addr, hd.cylinders);
        vm.memory.write_byte(fdpt_addr + 2, hd.heads);
        vm.memory.write_word(fdpt_addr + 3, 0);
        vm.memory.write_word(fdpt_addr + 5, hd.cylinders + 1);
        vm.memory.write_byte(fdpt_addr + 7, 0);
        let control = if hd.heads > 8 { 0x08 } else { 0x00 };
        vm.memory.write_byte(fdpt_addr + 8, control);
        vm.memory.write_byte(fdpt_addr + 9, 0);
        vm.memory.write_byte(fdpt_addr + 10, 0);
        vm.memory.write_byte(fdpt_addr + 11, 0);
        vm.memory.write_word(fdpt_addr + 12, hd.cylinders + 1);
        vm.memory.write_byte(fdpt_addr + 14, hd.sectors_per_track);
        vm.memory.write_byte(fdpt_addr + 15, 0);

        let ivt_46 = 0x46 * 4;
        vm.memory.write_word(ivt_46, (fdpt_addr & 0xFFFF) as u16);
        vm.memory.write_word(ivt_46 + 2, (BIOS_ROM >> 4) as u16);
    }

    // BIOS ROM identification at FFFF:000E (physical 0xFFFFE)
    // IBM PC AT compatible model byte
    vm.memory.write_byte(0xFFFFE, 0xFC); // 0xFC = AT class machine

    // BIOS date string at FFFF:0005 (physical 0xFFFF5) - "01/01/00"
    let bios_date = b"01/01/00";
    for (i, &b) in bios_date.iter().enumerate() {
        vm.memory.write_byte(0xFFFF5 + i, b);
    }

    // Clear VGA text buffer
    for i in 0..2000 {
        vm.memory.write_byte(crate::vm::memory::VGA_TEXT_BASE + i * 2, b' ');
        vm.memory.write_byte(crate::vm::memory::VGA_TEXT_BASE + i * 2 + 1, 0x07);
    }

    // Bootstrap: load boot sector
    int19h(vm);
}
