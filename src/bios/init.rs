use crate::bios::handlers::*;
use crate::vm::memory::{BDA_BASE, BIOS_ROM};
use crate::vm::runtime::Runtime;

pub fn init_bios(vm: &mut Runtime) {
    // Register BIOS handlers on private trap vectors (0xF0-0xF8).
    // Each BIOS interrupt (INT 09h, 10h, …) gets a ROM stub that does
    //     INT Fxh ; IRET
    // The IVT entry for the interrupt points to this stub.  dispatch_int
    // finds bios_handlers[0xFx] for the trap vector and calls the Rust
    // handler directly.
    //
    // This design lets guest software (DOS, TSRs) hook any BIOS vector by
    // changing the IVT — dispatch_int will follow the IVT to the guest
    // hook, which can chain to the original ROM stub if desired.
    vm.bios_handlers[0xF0] = Some(int09h_bios);
    vm.bios_handlers[0xF1] = Some(int10h);
    vm.bios_handlers[0xF2] = Some(int11h);
    vm.bios_handlers[0xF3] = Some(int12h);
    vm.bios_handlers[0xF4] = Some(int13h);
    vm.bios_handlers[0xF5] = Some(int15h);
    vm.bios_handlers[0xF6] = Some(int16h);
    vm.bios_handlers[0xF7] = Some(int19h);
    vm.bios_handlers[0xF8] = Some(int1ah);

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

    // Write ROM stubs for all BIOS interrupt handlers.
    // Each stub is 3 bytes:  INT Fxh ; IRET
    // The IVT entry for the corresponding vector points here.
    // Guest software can hook a vector by changing the IVT; dispatch_int
    // will follow the IVT chain.  The original handler is reachable by
    // calling or jumping to the ROM stub address.
    let stub_seg = (BIOS_ROM >> 4) as u16;
    let bios_stubs: &[(u8, u8)] = &[
        // (BIOS vector, trap vector)
        (0x09, 0xF0),  // Keyboard IRQ
        (0x10, 0xF1),  // Video services
        (0x11, 0xF2),  // Equipment list
        (0x12, 0xF3),  // Memory size
        (0x13, 0xF4),  // Disk services
        (0x15, 0xF5),  // System services
        (0x16, 0xF6),  // Keyboard services
        (0x19, 0xF7),  // Bootstrap loader
        (0x1A, 0xF8),  // Time services
    ];
    for (i, &(bios_vec, trap_vec)) in bios_stubs.iter().enumerate() {
        let stub_addr = BIOS_ROM + 0x400 + i * 3;
        vm.memory.write_byte(stub_addr, 0xCD);         // INT
        vm.memory.write_byte(stub_addr + 1, trap_vec);  //   Fxh
        vm.memory.write_byte(stub_addr + 2, 0xCF);     // IRET

        // Point IVT[bios_vec] to this stub (overrides the IRET-only stub)
        let ivt_offset = bios_vec as usize * 4;
        vm.memory.write_word(ivt_offset, (stub_addr & 0xFFFF) as u16);
        vm.memory.write_word(ivt_offset + 2, stub_seg);
    }

    // Write a generic "EOI + IRET" stub for hardware interrupts (IRQ 0-7).
    // Without EOI, the PIC's ISR bit stays set and blocks lower-priority IRQs.
    //   PUSH AX      ; 50
    //   MOV AL, 20h  ; B0 20
    //   OUT 20h, AL  ; E6 20
    //   POP AX       ; 58
    //   IRET         ; CF
    let eoi_stub = BIOS_ROM + 0x440;
    vm.memory.write_byte(eoi_stub, 0x50);         // PUSH AX
    vm.memory.write_byte(eoi_stub + 1, 0xB0);     // MOV AL, imm8
    vm.memory.write_byte(eoi_stub + 2, 0x20);     //   0x20
    vm.memory.write_byte(eoi_stub + 3, 0xE6);     // OUT imm8, AL
    vm.memory.write_byte(eoi_stub + 4, 0x20);     //   0x20
    vm.memory.write_byte(eoi_stub + 5, 0x58);     // POP AX
    vm.memory.write_byte(eoi_stub + 6, 0xCF);     // IRET

    // Point hardware interrupt vectors (INT 08h-0Fh) to the EOI+IRET stub,
    // EXCEPT INT 09h which keeps its keyboard-specific handler.
    for vec in 0x08u16..=0x0F {
        if vec == 0x09 { continue; } // keyboard uses its own stub
        vm.memory.write_word((vec * 4) as usize, (eoi_stub & 0xFFFF) as u16);
        vm.memory.write_word((vec * 4 + 2) as usize, stub_seg);
    }

    // Program the PIC (like real BIOS POST) — unmask all IRQs
    if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_out_byte(0x20, 0x11);  // ICW1: edge-triggered, cascade, ICW4 needed
        io_bus.port_out_byte(0x21, 0x08);  // ICW2: vector offset 8 (IRQ0 → INT 08h)
        io_bus.port_out_byte(0x21, 0x04);  // ICW3: slave on IRQ 2
        io_bus.port_out_byte(0x21, 0x01);  // ICW4: 8086 mode
        io_bus.port_out_byte(0x21, 0x00);  // OCW1: unmask all IRQs
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

    // Initialize BDA keyboard buffer
    // Head and tail both point to buffer start (empty)
    vm.memory.write_word(BDA_BASE + 0x1A, 0x1E);     // Keyboard buffer head offset
    vm.memory.write_word(BDA_BASE + 0x1C, 0x1E);     // Keyboard buffer tail offset
    // Enhanced keyboard buffer start/end pointers (used by some programs)
    vm.memory.write_word(BDA_BASE + 0x80, 0x1E);     // Buffer start offset
    vm.memory.write_word(BDA_BASE + 0x82, 0x3E);     // Buffer end offset

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
