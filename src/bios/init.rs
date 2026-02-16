use crate::bios::{bda, disk, keyboard, system, timer, video};
use crate::vm::memory::{BIOS_ROM, VGA_TEXT_BASE};
use crate::vm::runtime::Runtime;

/// ROM stub layout starting at 0xF0000:
///   0xF0000: CD F0 CF    — INT 08h IVT stub → bios_handlers[0xF0]
///   0xF0003: CD F1 CF    — INT 09h IVT stub → bios_handlers[0xF1]
///   0xF0006: CF          — Generic IRET for default vectors
const ROM_STUB_BASE: usize = BIOS_ROM;      // 0xF0000
const ROM_STUB_INT08: usize = ROM_STUB_BASE; // CD F0 CF
const ROM_STUB_INT09: usize = ROM_STUB_BASE + 3; // CD F1 CF
const ROM_STUB_IRET: usize = ROM_STUB_BASE + 6; // CF

/// Diskette parameter table location in ROM
const DPT_ADDR: usize = BIOS_ROM + 0x100;  // 0xF0100
/// Hard disk 0 parameter table
const HDPT0_ADDR: usize = BIOS_ROM + 0x110; // 0xF0110
/// Hard disk 1 parameter table
const HDPT1_ADDR: usize = BIOS_ROM + 0x120; // 0xF0120

pub fn init_bios(vm: &mut Runtime) {
    write_rom_stubs(vm);
    setup_ivt(vm);
    setup_data_tables(vm);
    init_bda(vm);
    register_handlers(vm);
    init_pic(vm);
    clear_vga(vm);

    // Bootstrap: load boot sector and set CS:IP to 0000:7C00
    system::int19h(vm);
}

fn write_rom_stubs(vm: &mut Runtime) {
    // INT 08h stub: CD F0 CF (INT 0xF0; IRET)
    vm.memory.write_byte(ROM_STUB_INT08, 0xCD);
    vm.memory.write_byte(ROM_STUB_INT08 + 1, 0xF0);
    vm.memory.write_byte(ROM_STUB_INT08 + 2, 0xCF);

    // INT 09h stub: CD F1 CF (INT 0xF1; IRET)
    vm.memory.write_byte(ROM_STUB_INT09, 0xCD);
    vm.memory.write_byte(ROM_STUB_INT09 + 1, 0xF1);
    vm.memory.write_byte(ROM_STUB_INT09 + 2, 0xCF);

    // Generic IRET stub
    vm.memory.write_byte(ROM_STUB_IRET, 0xCF);
}

fn setup_ivt(vm: &mut Runtime) {
    // Default all vectors to the IRET stub at F000:0006
    for i in 0..256 {
        let ivt_offset = i * 4;
        vm.memory.write_word(ivt_offset, ROM_STUB_IRET as u16 - BIOS_ROM as u16); // IP offset within F000 segment
        vm.memory.write_word(ivt_offset + 2, 0xF000);  // CS = F000
    }

    // Timer IRQ (INT 08h) → F000:0000 (ROM stub that calls handler 0xF0)
    vm.memory.write_word(0x08 * 4, 0x0000);
    vm.memory.write_word(0x08 * 4 + 2, 0xF000);

    // Keyboard IRQ (INT 09h) → F000:0003 (ROM stub that calls handler 0xF1)
    vm.memory.write_word(0x09 * 4, 0x0003);
    vm.memory.write_word(0x09 * 4 + 2, 0xF000);

    // INT 1Ch (user timer hook) → IRET (default, can be hooked by user code)
    // Already set to IRET by the loop above
}

fn setup_data_tables(vm: &mut Runtime) {
    // Diskette Parameter Table (11 bytes) for INT 1Eh
    let dpt: [u8; 11] = [
        0xDF, // SRT=D, HUT=F
        0x02, // DMA mode, HLT=1
        0x25, // Motor wait time (ticks)
        0x02, // Bytes per sector (2 = 512)
        18,   // Sectors per track (1.44M)
        0x1B, // Gap length
        0xFF, // DTL
        0x54, // Format gap length
        0xF6, // Format fill byte
        0x0F, // Head settle time (ms)
        0x08, // Motor start time (1/8 sec)
    ];
    for (i, &b) in dpt.iter().enumerate() {
        vm.memory.write_byte(DPT_ADDR + i, b);
    }
    // Set INT 1Eh vector to point to DPT
    vm.memory.write_word(0x1E * 4, (DPT_ADDR - BIOS_ROM) as u16);
    vm.memory.write_word(0x1E * 4 + 2, 0xF000);

    // Hard disk parameter tables (INT 41h = HD0, INT 46h = HD1)
    for (hd_idx, ivt_num) in [(0usize, 0x41u16), (1usize, 0x46u16)] {
        if let Some(Some(disk)) = vm.hard_disks.get(hd_idx) {
            let table_addr = if hd_idx == 0 { HDPT0_ADDR } else { HDPT1_ADDR };
            // 16-byte HD parameter table
            let cyls = disk.cylinders;
            let heads = disk.heads;
            let spt = disk.sectors_per_track;

            vm.memory.write_word(table_addr, cyls);       // Max cylinders
            vm.memory.write_byte(table_addr + 2, heads);  // Max heads
            vm.memory.write_word(table_addr + 3, 0);      // Reduced write current cyl
            vm.memory.write_word(table_addr + 5, cyls);   // Write precomp cyl
            vm.memory.write_byte(table_addr + 7, 0);      // Max ECC burst length
            vm.memory.write_byte(table_addr + 8, 0x08);   // Control byte (no retries)
            vm.memory.write_byte(table_addr + 9, 0);      // Std timeout
            vm.memory.write_byte(table_addr + 10, 0);     // Format timeout
            vm.memory.write_byte(table_addr + 11, 0);     // Check timeout
            vm.memory.write_word(table_addr + 12, cyls);  // Landing zone
            vm.memory.write_byte(table_addr + 14, spt);   // Sectors per track
            vm.memory.write_byte(table_addr + 15, 0);     // Reserved

            // Set IVT vector
            vm.memory.write_word((ivt_num as usize) * 4, (table_addr - BIOS_ROM) as u16);
            vm.memory.write_word((ivt_num as usize) * 4 + 2, 0xF000);
        }
    }
}

fn init_bda(vm: &mut Runtime) {
    // Conventional memory
    vm.memory.write_word(bda::MEMORY_SIZE_KB, 640);

    // Video
    vm.memory.write_byte(bda::VIDEO_MODE, 3);
    vm.memory.write_word(bda::VIDEO_COLS, 80);
    vm.memory.write_byte(bda::VIDEO_ROWS, 24);
    vm.memory.write_word(bda::CRTC_PORT, 0x3D4);
    vm.memory.write_word(bda::CURSOR_SHAPE, 0x0607); // Standard underline cursor
    vm.memory.write_word(bda::CHAR_HEIGHT, 16);
    vm.memory.write_byte(bda::ACTIVE_PAGE, 0);

    // Keyboard buffer: empty, head = tail = start offset
    vm.memory.write_word(bda::KB_HEAD, bda::KB_BUF_OFFSET_START);
    vm.memory.write_word(bda::KB_TAIL, bda::KB_BUF_OFFSET_START);

    // Equipment word
    let num_floppies = vm.disks.iter().filter(|d| d.is_some()).count() as u16;
    let num_hd = vm.hard_disks.iter().filter(|d| d.is_some()).count() as u8;
    let mut equip: u16 = 0;
    if num_floppies > 0 {
        equip |= 0x01; // Floppy drive(s) installed
        equip |= ((num_floppies.min(4) - 1) & 0x03) << 6; // Number of floppies - 1
    }
    equip |= 0x02;  // Math coprocessor (pretend present for DOS compatibility)
    equip |= 0x20;  // Initial video mode: 80x25 color (bits 4-5 = 10)
    vm.memory.write_word(bda::EQUIP_WORD, equip);

    // Hard disk count
    vm.memory.write_byte(bda::HD_COUNT, num_hd);

    // Zero out cursor positions
    for i in 0..16 {
        vm.memory.write_byte(bda::CURSOR_POS + i, 0);
    }

    // Timer
    vm.memory.write_word(bda::TICK_COUNT, 0);
    vm.memory.write_word(bda::TICK_COUNT + 2, 0);
    vm.memory.write_byte(bda::TICK_OVERFLOW, 0);
}

fn register_handlers(vm: &mut Runtime) {
    // IRQ handlers (via ROM stubs, slots 0xF0-0xF1)
    vm.bios_handlers[0xF0] = Some(timer::int08h);
    vm.bios_handlers[0xF1] = Some(keyboard::int09h);

    // BIOS service interrupts (Tier 1 direct dispatch)
    vm.bios_handlers[0x10] = Some(video::int10h);
    vm.bios_handlers[0x11] = Some(system::int11h);
    vm.bios_handlers[0x12] = Some(system::int12h);
    vm.bios_handlers[0x13] = Some(disk::int13h);
    vm.bios_handlers[0x15] = Some(system::int15h);
    vm.bios_handlers[0x16] = Some(keyboard::int16h);
    vm.bios_handlers[0x19] = Some(system::int19h);
    vm.bios_handlers[0x1A] = Some(timer::int1ah);
}

fn init_pic(vm: &mut Runtime) {
    // Unmask IRQ 0 (timer) and IRQ 1 (keyboard), mask all others
    if let Some(ref pic) = vm.pic {
        pic.lock().unwrap().set_imr(0xFC);
    }
}

fn clear_vga(vm: &mut Runtime) {
    // Fill VGA text memory with spaces + light gray on black
    for i in 0..4000 {
        vm.memory.write_byte(VGA_TEXT_BASE + i * 2, 0x20);
        vm.memory.write_byte(VGA_TEXT_BASE + i * 2 + 1, 0x07);
    }
}
