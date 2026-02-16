use crate::bios::bda;
use crate::vm::memory::BDA_BASE;
use crate::vm::runtime::{CpuFlag, Runtime};

/// INT 09h — Keyboard IRQ handler (IRQ 1, slot 0xF1)
pub fn int09h(vm: &mut Runtime) {
    // Read scancode from port 0x60 latch
    let scancode = if let Some(ref latch) = vm.keyboard_latch {
        *latch.lock().unwrap()
    } else {
        send_eoi(vm);
        return;
    };

    // Clear data-available flag (port 0x64 OBF)
    if let Some(ref flag) = vm.keyboard_data_available {
        flag.store(false, std::sync::atomic::Ordering::SeqCst);
    }

    let ascii = vm.keyboard_pending_ascii;

    // E0 prefix byte (enhanced keyboard) — not a real keystroke, just skip it
    if scancode == 0xE0 {
        send_eoi(vm);
        return;
    }

    // Break code (key release) — bit 7 set
    if scancode & 0x80 != 0 {
        let make = scancode & 0x7F;
        // Update shift flags on release
        let mut flags = vm.memory.read_byte(bda::KB_SHIFT_FLAGS);
        match make {
            0x2A => flags &= !0x02, // Left Shift
            0x36 => flags &= !0x01, // Right Shift
            0x1D => flags &= !0x04, // Ctrl
            0x38 => flags &= !0x08, // Alt
            _ => {}
        }
        vm.memory.write_byte(bda::KB_SHIFT_FLAGS, flags);
        send_eoi(vm);
        return;
    }

    // Make code (key press) — update shift flags
    let mut flags = vm.memory.read_byte(bda::KB_SHIFT_FLAGS);
    match scancode {
        0x2A => flags |= 0x02, // Left Shift
        0x36 => flags |= 0x01, // Right Shift
        0x1D => flags |= 0x04, // Ctrl
        0x38 => flags |= 0x08, // Alt
        0x3A => flags ^= 0x40, // Caps Lock toggle
        0x45 => flags ^= 0x20, // Num Lock toggle
        0x46 => flags ^= 0x10, // Scroll Lock toggle
        _ => {}
    }
    vm.memory.write_byte(bda::KB_SHIFT_FLAGS, flags);

    // Don't insert modifier-only keys into the buffer
    match scancode {
        0x2A | 0x36 | 0x1D | 0x38 | 0x3A | 0x45 | 0x46 => {
            send_eoi(vm);
            return;
        }
        _ => {}
    }

    // Insert (scancode, ascii) word into the BDA keyboard ring buffer
    let tail = vm.memory.read_word(bda::KB_TAIL);
    let head = vm.memory.read_word(bda::KB_HEAD);
    let next = if tail + 2 >= bda::KB_BUF_OFFSET_END {
        bda::KB_BUF_OFFSET_START
    } else {
        tail + 2
    };

    if next == head {
        // Buffer full — discard keystroke
        send_eoi(vm);
        return;
    }

    // Write the keystroke word: low byte = ASCII, high byte = scancode
    let addr = BDA_BASE + tail as usize;
    vm.memory.write_byte(addr, ascii);
    vm.memory.write_byte(addr + 1, scancode);
    vm.memory.write_word(bda::KB_TAIL, next);
    eprintln!("[INT09] BDA insert sc={:02X} ascii={:02X} head={:04X} tail={:04X} next={:04X}",
             scancode, ascii, head, tail, next);

    send_eoi(vm);
}

fn send_eoi(vm: &mut Runtime) {
    if let Some(ref pic) = vm.pic {
        pic.lock().unwrap().eoi();
    }
}

/// INT 16h — Keyboard services
pub fn int16h(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // AH=00/10: Wait for keystroke (blocking)
        0x00 | 0x10 => {
            // Enable interrupts like real BIOS STI — allows timer ticks
            // and keyboard IRQs to be delivered during the blocking wait.
            vm.set_flag(CpuFlag::Interrupt);
            loop {
                let head = vm.memory.read_word(bda::KB_HEAD);
                let tail = vm.memory.read_word(bda::KB_TAIL);
                if head != tail {
                    // Pop keystroke from buffer
                    let addr = BDA_BASE + head as usize;
                    let ascii = vm.memory.read_byte(addr);
                    let scancode = vm.memory.read_byte(addr + 1);
                    let next_head = if head + 2 >= bda::KB_BUF_OFFSET_END {
                        bda::KB_BUF_OFFSET_START
                    } else {
                        head + 2
                    };
                    vm.memory.write_word(bda::KB_HEAD, next_head);
                    // AH=00: translate enhanced ASCII 0xE0 → 0x00 for DOS compatibility
                    // AH=10: return raw value (0xE0) for enhanced keyboard distinction
                    let ascii = if ah == 0x00 && ascii == 0xE0 { 0x00 } else { ascii };
                    vm.registers.ax.set_low(ascii);
                    vm.registers.ax.set_high(scancode);
                    return;
                }

                // Buffer empty — pump timer/keyboard and yield.
                // Use service_bios_irqs() instead of check_hw_interrupts() because
                // DOS may have hooked INT 09h in the IVT, and Tier 3 dispatch can't
                // execute from inside a Tier 1 handler (CS:IP change is meaningless,
                // ISR bit stays set forever, blocking all future keyboard delivery).
                vm.idle_tick();
                vm.service_bios_irqs();

                // Check if the user pressed F12 to open the monitor.
                // Without this, F12 deadlocks: the keyboard thread blocks
                // waiting for the flag to clear, but we never check it here.
                if vm.check_monitor_flag() {
                    vm.enter_monitor();
                }

                std::thread::sleep(std::time::Duration::from_millis(1));
            }
        }
        // AH=01/11: Check keystroke available (non-blocking)
        0x01 | 0x11 => {
            let head = vm.memory.read_word(bda::KB_HEAD);
            let tail = vm.memory.read_word(bda::KB_TAIL);
            if head == tail {
                // No key — set ZF
                vm.set_flag(CpuFlag::Zero);
            } else {
                // Peek without consuming
                let addr = BDA_BASE + head as usize;
                let ascii = vm.memory.read_byte(addr);
                let scancode = vm.memory.read_byte(addr + 1);
                let ascii = if ah == 0x01 && ascii == 0xE0 { 0x00 } else { ascii };
                vm.registers.ax.set_low(ascii);
                vm.registers.ax.set_high(scancode);
                vm.unset_flag(CpuFlag::Zero);
            }
        }
        // AH=02/12: Get shift flags
        0x02 | 0x12 => {
            let shift_flags = vm.memory.read_byte(bda::KB_SHIFT_FLAGS);
            vm.registers.ax.set_low(shift_flags);
        }
        _ => {}
    }
}
