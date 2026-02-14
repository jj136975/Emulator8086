use crate::vm::instructions::process;
use crate::vm::memory::{BDA_BASE, Memory, BIOS_ROM};
use crate::vm::runtime::CpuFlag;
use crate::vm::runtime::Runtime;

/// Code origin for BIOS tests — after IVT (0x000-0x3FF) and BDA (0x400-0x4FF).
const CODE_BASE: usize = 0x0500;

/// Create a BIOS-mode test VM (PIC + IoBus + keyboard, no input thread).
/// Programs the PIC via ICW1-4 and unmasks all IRQs, sets up IRET stubs in IVT.
/// Code is loaded at CODE_BASE (0x0500) to avoid IVT/BDA region.
fn bios_setup(code: &[u8]) -> Runtime {
    let mut vm = Runtime::new_bios_test();
    vm.registers.sp.set(0x8000);
    vm.registers.pc.set(CODE_BASE as u16);

    // Load code after IVT+BDA
    Memory::copy_data(&mut vm.memory, CODE_BASE, code);

    // Write IRET stubs for all 256 vectors in ROM area
    let iret_base = BIOS_ROM + 0x100;
    let stub_seg = (BIOS_ROM >> 4) as u16;
    for i in 0..256u16 {
        let stub_addr = iret_base + i as usize;
        vm.memory.write_byte(stub_addr, 0xCF); // IRET
        let ivt_offset = (i * 4) as usize;
        vm.memory.write_word(ivt_offset, (stub_addr & 0xFFFF) as u16);
        vm.memory.write_word(ivt_offset + 2, stub_seg);
    }

    // Program PIC via I/O bus (standard ICW1-4 sequence)
    if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_out_byte(0x20, 0x11); // ICW1: edge, cascade, ICW4 needed
        io_bus.port_out_byte(0x21, 0x08); // ICW2: vector offset 0x08
        io_bus.port_out_byte(0x21, 0x04); // ICW3: slave on IRQ2
        io_bus.port_out_byte(0x21, 0x01); // ICW4: 8086 mode, manual EOI
        io_bus.port_out_byte(0x21, 0x00); // OCW1: unmask all
    }

    // Initialize BDA keyboard buffer pointers
    vm.memory.write_word(BDA_BASE + 0x1A, 0x1E);
    vm.memory.write_word(BDA_BASE + 0x1C, 0x1E);
    vm.memory.write_word(BDA_BASE + 0x80, 0x1E);
    vm.memory.write_word(BDA_BASE + 0x82, 0x3E);

    vm
}

fn exec(vm: &mut Runtime) {
    process(vm);
}

fn exec_n(vm: &mut Runtime, n: usize) {
    for _ in 0..n {
        process(vm);
    }
}

/// Inject a scancode+ascii into the keyboard buffer (simulates crossterm input).
fn inject_key(vm: &mut Runtime, scancode: u8, ascii: u8) {
    if let Some(ref buf) = vm.keyboard_buffer {
        buf.lock().unwrap().push_back((scancode, ascii));
    }
}

// ========================================================================
// Test 1: PIC Initialization
// ========================================================================

#[test]
fn pic_initialization() {
    let vm = bios_setup(&[0x90]); // NOP

    // After ICW1-4 sequence, verify PIC state via shared Arc
    let pic = vm.pic.as_ref().unwrap().lock().unwrap();
    // vector_offset should be 0x08 (set by ICW2)
    // After ICW1-4 + OCW1(0x00), IMR should be 0x00
    assert_eq!(pic.debug_imr(), 0x00, "IMR should be 0 (all unmasked)");
    assert_eq!(pic.debug_irr(), 0x00, "IRR should be 0 (no pending)");
    assert_eq!(pic.debug_isr(), 0x00, "ISR should be 0 (none in service)");
    drop(pic);

    // Verify mask read-back via I/O port
    let mut vm = vm;
    // Mask all
    if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_out_byte(0x21, 0xFF);
        let imr = io_bus.port_in_byte(0x21);
        assert_eq!(imr, 0xFF, "IMR read-back should be 0xFF after masking all");
    }
    // Unmask all
    if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_out_byte(0x21, 0x00);
        let imr = io_bus.port_in_byte(0x21);
        assert_eq!(imr, 0x00, "IMR read-back should be 0x00 after unmasking all");
    }
}

// ========================================================================
// Test 2: Single Keyboard Interrupt
// ========================================================================

#[test]
fn single_keyboard_interrupt() {
    // Code at 0x0000: just NOPs — we'll manually drive the interrupt cycle
    let mut vm = bios_setup(&[0x90; 64]); // 64 NOPs
    vm.set_flag(CpuFlag::Interrupt); // Enable interrupts

    // Inject 'A' key press: scancode=0x1E, ascii='a'
    inject_key(&mut vm, 0x1E, 0x61);

    // Deliver the keyboard IRQ (pops from buffer, latches to port 0x60, raises IRQ1)
    vm.deliver_keyboard_irq();

    // Verify IRR bit 1 is set
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert!(pic.debug_irr() & 0x02 != 0, "IRR bit 1 should be set after raise_irq(1)");
    }

    // Save state before interrupt dispatch
    let old_flags = vm.flags;
    let old_cs = vm.registers.cs.reg().word();
    let old_pc = vm.registers.pc.word(); // 0x0000

    // Check hw interrupts — this should dispatch INT 09h
    vm.check_hw_interrupts();

    // Verify CPU pushed FLAGS, CS, IP and jumped to IVT[9]
    let ivt9_ip = vm.memory.read_word(9 * 4);
    let ivt9_cs = vm.memory.read_word(9 * 4 + 2);
    assert_eq!(vm.registers.pc.word(), ivt9_ip, "PC should point to IVT[9] handler");
    assert_eq!(vm.registers.cs.reg().word(), ivt9_cs, "CS should point to IVT[9] segment");

    // Stack should have: [IP, CS, FLAGS] (pushed in that order)
    let sp = vm.registers.sp.word();
    let pushed_ip = vm.registers.ss.read_word(sp);
    let pushed_cs = vm.registers.ss.read_word(sp.wrapping_add(2));
    let pushed_flags = vm.registers.ss.read_word(sp.wrapping_add(4));
    assert_eq!(pushed_ip, old_pc, "Pushed IP should be the old PC");
    assert_eq!(pushed_cs, old_cs, "Pushed CS should be the old CS");
    assert_eq!(pushed_flags, old_flags, "Pushed FLAGS should be the old flags");

    // PIC: ISR bit 1 should be set (acknowledged), IRR bit 1 cleared
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert!(pic.debug_isr() & 0x02 != 0, "ISR bit 1 should be set after acknowledge");
        assert!(pic.debug_irr() & 0x02 == 0, "IRR bit 1 should be cleared after acknowledge");
    }
}

// ========================================================================
// Test 3: EOI Handling
// ========================================================================

#[test]
fn eoi_handling() {
    let mut vm = bios_setup(&[0x90; 16]);
    vm.set_flag(CpuFlag::Interrupt);

    // Raise IRQ1 and acknowledge it
    inject_key(&mut vm, 0x1E, 0x61);
    vm.deliver_keyboard_irq();
    vm.check_hw_interrupts();

    // ISR bit 1 should be set
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert!(pic.debug_isr() & 0x02 != 0, "ISR bit 1 should be set before EOI");
    }

    // Send non-specific EOI via OUT 0x20, 0x20
    // Since ISR has bit 1 (and possibly bit 0 is clear), non-specific EOI clears lowest ISR bit
    // But we only have bit 1, so we send specific EOI for IRQ1: 0x61
    if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_out_byte(0x20, 0x61); // Specific EOI for IRQ1
    }

    // ISR bit 1 should be cleared
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert!(pic.debug_isr() & 0x02 == 0, "ISR bit 1 should be cleared after EOI");
    }

    // Subsequent keyboard interrupt should be deliverable
    inject_key(&mut vm, 0x1E, 0x61);
    vm.deliver_keyboard_irq();
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert!(pic.has_interrupt(), "New keyboard interrupt should be deliverable after EOI");
    }
}

// ========================================================================
// Test 4: Interrupt Masking
// ========================================================================

#[test]
fn interrupt_masking() {
    let mut vm = bios_setup(&[0x90; 16]);

    // Mask keyboard IRQ1: set IMR bit 1
    if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_out_byte(0x21, 0x02);
    }

    // Raise IRQ1
    {
        let mut pic = vm.pic.as_ref().unwrap().lock().unwrap();
        pic.raise_irq(1);
        // IRR bit 1 should be set
        assert!(pic.debug_irr() & 0x02 != 0, "IRR should show pending IRQ1");
        // But has_interrupt() should return false (masked)
        assert!(!pic.has_interrupt(), "has_interrupt() should be false when IRQ1 is masked");
    }

    // Unmask all
    if let Some(ref mut io_bus) = vm.io_bus {
        io_bus.port_out_byte(0x21, 0x00);
    }

    // Now has_interrupt() should return true
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert!(pic.has_interrupt(), "has_interrupt() should be true after unmasking");
    }
}

// ========================================================================
// Test 5: STI Delay (interrupt inhibit after STI)
// ========================================================================

#[test]
fn sti_delay() {
    // Code: CLI; NOP; STI; NOP; NOP
    let mut vm = bios_setup(&[
        0xFA,       // CLI (offset +0)
        0x90,       // NOP (offset +1)
        0xFB,       // STI (offset +2)
        0x90,       // NOP (offset +3) - inhibited instruction
        0x90,       // NOP (offset +4) - interrupt should fire BEFORE this
    ]);
    let base = CODE_BASE as u16;

    // Execute CLI
    exec(&mut vm);
    assert!(!vm.check_flag(CpuFlag::Interrupt));

    // Raise IRQ1 while interrupts are disabled
    {
        vm.pic.as_ref().unwrap().lock().unwrap().raise_irq(1);
    }

    // Execute NOP — interrupt can't fire (IF=0)
    exec(&mut vm);
    assert_eq!(vm.registers.pc.word(), base + 2);

    // Execute STI — sets IF=1 but sets interrupt_inhibit
    exec(&mut vm);
    assert!(vm.check_flag(CpuFlag::Interrupt));
    assert!(vm.interrupt_inhibit, "interrupt_inhibit should be set after STI");

    // Simulate the run loop: interrupt_inhibit means we skip check_hw_interrupts
    // The next instruction executes WITHOUT interrupt delivery
    vm.interrupt_inhibit = false; // run loop clears it
    exec(&mut vm); // NOP at offset +3
    assert_eq!(vm.registers.pc.word(), base + 4);

    // NOW the interrupt should fire (IF=1, no inhibit)
    vm.check_hw_interrupts();
    let ivt9_ip = vm.memory.read_word(9 * 4);
    assert_eq!(vm.registers.pc.word(), ivt9_ip, "Interrupt should fire after inhibit window");
}

// ========================================================================
// Test 6: MOV SS Delay (interrupt inhibit after MOV SS)
// ========================================================================

#[test]
fn mov_ss_delay() {
    // Code: MOV SS,AX (8E D0); MOV SP,BX (89 DC); NOP
    // MOV SS,AX: 8E D0 = mod=11 reg=010(SS) rm=000(AX)
    // MOV SP,BX: 89 DC = mod=11 reg=011(BX) rm=100(SP)
    let mut vm = bios_setup(&[
        0x8E, 0xD0, // MOV SS,AX (offset +0)
        0x89, 0xDC, // MOV SP,BX (offset +2) - inhibited
        0x90,       // NOP (offset +4)
    ]);
    let base = CODE_BASE as u16;
    vm.set_flag(CpuFlag::Interrupt);

    // Raise IRQ1
    {
        vm.pic.as_ref().unwrap().lock().unwrap().raise_irq(1);
    }

    // Execute MOV SS,AX — should set interrupt_inhibit
    exec(&mut vm);
    assert!(vm.interrupt_inhibit, "MOV SS should inhibit interrupts for one instruction");
    assert_eq!(vm.registers.pc.word(), base + 2);

    // Simulate run loop: clear inhibit, execute MOV SP,BX without checking interrupts
    vm.interrupt_inhibit = false;
    exec(&mut vm); // MOV SP,BX
    assert_eq!(vm.registers.pc.word(), base + 4);

    // Now interrupt should fire
    vm.check_hw_interrupts();
    let ivt9_ip = vm.memory.read_word(9 * 4);
    assert_eq!(vm.registers.pc.word(), ivt9_ip, "Interrupt should fire after MOV SS inhibit window");
}

// ========================================================================
// Test 7: Full Keyboard Round-Trip
// ========================================================================

#[test]
fn full_keyboard_round_trip() {
    let mut vm = bios_setup(&[0x90; 256]); // Lots of NOPs as "user code"

    // Set up INT 09h handler at a custom location that:
    //   1. Reads port 0x60 (IN AL,0x60 -> E4 60)
    //   2. Sends specific EOI for IRQ1 (MOV AL,0x61; OUT 0x20,AL -> B0 61; E6 20)
    //   3. IRET (CF)
    let handler_addr = 0x1000u16;
    let handler_code: &[u8] = &[
        0xE4, 0x60, // IN AL,0x60
        0xB0, 0x61, // MOV AL,0x61
        0xE6, 0x20, // OUT 0x20,AL
        0xCF,       // IRET
    ];
    Memory::copy_data(&mut vm.memory, handler_addr as usize, handler_code);

    // Point IVT[9] to our handler (CS=0x0000, IP=handler_addr)
    vm.memory.write_word(9 * 4, handler_addr);
    vm.memory.write_word(9 * 4 + 2, 0x0000);

    vm.set_flag(CpuFlag::Interrupt);

    // Inject 'A' key (scancode=0x1E, ascii=0x61)
    inject_key(&mut vm, 0x1E, 0x61);

    // Deliver keyboard IRQ
    vm.deliver_keyboard_irq();

    // Save original PC
    let original_pc = vm.registers.pc.word();

    // Dispatch the interrupt
    vm.check_hw_interrupts();
    assert_eq!(vm.registers.pc.word(), handler_addr, "Should jump to INT 09h handler");

    // Execute the ISR (4 instructions: IN, MOV, OUT, IRET)
    exec_n(&mut vm, 4);

    // After IRET, we should be back at original code
    assert_eq!(vm.registers.pc.word(), original_pc, "Should return to original code after IRET");
    assert_eq!(vm.registers.cs.reg().word(), 0x0000);

    // Scancode should have been read (AL had it, but IRET restored flags/regs)
    // The important thing: PIC ISR should be clear
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert_eq!(pic.debug_isr(), 0x00, "ISR should be clear after EOI in handler");
    }

    // Verify the latch had the right scancode (port 0x60 returns it)
    if let Some(ref latch) = vm.keyboard_latch {
        assert_eq!(*latch.lock().unwrap(), 0x1E, "Port 0x60 latch should hold scancode 0x1E");
    }
}

// ========================================================================
// Test 8: Multiple Keys in Sequence
// ========================================================================

#[test]
fn multiple_keys_sequence() {
    let mut vm = bios_setup(&[0x90; 256]);

    // Set up a simple ISR: IN AL,0x60; store to known address; EOI; IRET
    let handler_addr = 0x1000u16;
    // We'll store each scancode to a growing buffer at 0x2000+
    // ISR: IN AL,0x60; MOV [0x2000+idx],AL; MOV AL,0x61; OUT 0x20,AL; IRET
    // For simplicity, just do: IN AL,0x60; EOI; IRET (we verify via latch)
    let handler_code: &[u8] = &[
        0xE4, 0x60, // IN AL,0x60
        0xB0, 0x61, // MOV AL,0x61
        0xE6, 0x20, // OUT 0x20,AL
        0xCF,       // IRET
    ];
    Memory::copy_data(&mut vm.memory, handler_addr as usize, handler_code);
    vm.memory.write_word(9 * 4, handler_addr);
    vm.memory.write_word(9 * 4 + 2, 0x0000);
    vm.set_flag(CpuFlag::Interrupt);

    // Inject three keys rapidly
    inject_key(&mut vm, 0x1E, 0x61); // 'A' make
    inject_key(&mut vm, 0x1F, 0x73); // 'S' make
    inject_key(&mut vm, 0x20, 0x64); // 'D' make

    let scancodes_expected = [0x1E, 0x1F, 0x20];

    for &expected_sc in &scancodes_expected {
        // Deliver one key
        vm.deliver_keyboard_irq();

        // Verify latch
        if let Some(ref latch) = vm.keyboard_latch {
            assert_eq!(*latch.lock().unwrap(), expected_sc,
                       "Latch should hold scancode 0x{:02X}", expected_sc);
        }

        // Dispatch interrupt
        vm.check_hw_interrupts();

        // Execute ISR (IN, MOV, OUT, IRET)
        exec_n(&mut vm, 4);

        // PIC should be clear after EOI
        {
            let pic = vm.pic.as_ref().unwrap().lock().unwrap();
            assert_eq!(pic.debug_isr(), 0x00,
                       "ISR should be clear after handling scancode 0x{:02X}", expected_sc);
        }
    }

    // Buffer should now be empty
    if let Some(ref buf) = vm.keyboard_buffer {
        assert!(buf.lock().unwrap().is_empty(), "Keyboard buffer should be empty after all keys processed");
    }
}

// ========================================================================
// Test 9: Priority (IRQ0 before IRQ1)
// ========================================================================

#[test]
fn irq_priority() {
    let mut vm = bios_setup(&[0x90; 256]);
    vm.set_flag(CpuFlag::Interrupt);

    // Set up distinct handlers for IRQ0 (INT 08h) and IRQ1 (INT 09h)
    // IRQ0 handler at 0x1000: write marker 0x08 to [0x2000]; EOI; IRET
    let irq0_handler = 0x1000u16;
    Memory::copy_data(&mut vm.memory, irq0_handler as usize, &[
        0xB0, 0x08,       // MOV AL,0x08 (marker)
        0xA2, 0x00, 0x20, // MOV [0x2000],AL
        0xB0, 0x60,       // MOV AL,0x60 (specific EOI IRQ0)
        0xE6, 0x20,       // OUT 0x20,AL
        0xCF,             // IRET
    ]);
    vm.memory.write_word(8 * 4, irq0_handler);
    vm.memory.write_word(8 * 4 + 2, 0x0000);

    // IRQ1 handler at 0x1100: write marker 0x09 to [0x2001]; EOI; IRET
    let irq1_handler = 0x1100u16;
    Memory::copy_data(&mut vm.memory, irq1_handler as usize, &[
        0xB0, 0x09,       // MOV AL,0x09 (marker)
        0xA2, 0x01, 0x20, // MOV [0x2001],AL
        0xB0, 0x61,       // MOV AL,0x61 (specific EOI IRQ1)
        0xE6, 0x20,       // OUT 0x20,AL
        0xCF,             // IRET
    ]);
    vm.memory.write_word(9 * 4, irq1_handler);
    vm.memory.write_word(9 * 4 + 2, 0x0000);

    // Raise both IRQ0 and IRQ1 simultaneously
    {
        let mut pic = vm.pic.as_ref().unwrap().lock().unwrap();
        pic.raise_irq(0);
        pic.raise_irq(1);
        // Both should be pending
        assert!(pic.debug_irr() & 0x01 != 0, "IRQ0 should be pending");
        assert!(pic.debug_irr() & 0x02 != 0, "IRQ1 should be pending");
    }

    // First dispatch — should pick IRQ0 (higher priority = lower number)
    vm.check_hw_interrupts();
    assert_eq!(vm.registers.pc.word(), irq0_handler, "IRQ0 should be serviced first");

    // Execute IRQ0 handler (5 instructions)
    exec_n(&mut vm, 5);
    assert_eq!(vm.memory.read_byte(0x2000), 0x08, "IRQ0 handler should have written marker");

    // After IRET from IRQ0, IRQ1 should still be pending
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert!(pic.debug_irr() & 0x02 != 0, "IRQ1 should still be pending");
        assert_eq!(pic.debug_isr(), 0x00, "ISR should be clear after IRQ0 EOI");
    }

    // Second dispatch — should pick IRQ1
    vm.check_hw_interrupts();
    assert_eq!(vm.registers.pc.word(), irq1_handler, "IRQ1 should be serviced second");

    // Execute IRQ1 handler (5 instructions)
    exec_n(&mut vm, 5);
    assert_eq!(vm.memory.read_byte(0x2001), 0x09, "IRQ1 handler should have written marker");

    // Everything clear now
    {
        let pic = vm.pic.as_ref().unwrap().lock().unwrap();
        assert_eq!(pic.debug_irr(), 0x00, "No IRQs pending");
        assert_eq!(pic.debug_isr(), 0x00, "No IRQs in service");
    }
}

// ========================================================================
// Test 10: Spurious Interrupt
// ========================================================================

#[test]
fn spurious_interrupt() {
    let vm = bios_setup(&[0x90]);

    // Call acknowledge() with no IRQs pending — should return spurious IRQ7
    let mut pic = vm.pic.as_ref().unwrap().lock().unwrap();
    let vector = pic.acknowledge();

    // Spurious: returns vector_offset + 7 = 0x08 + 7 = 0x0F
    assert_eq!(vector, Some(0x0F), "Spurious interrupt should return vector 0x0F");

    // ISR should NOT be modified for spurious
    assert_eq!(pic.debug_isr(), 0x00, "ISR should not be set for spurious interrupt");
}
