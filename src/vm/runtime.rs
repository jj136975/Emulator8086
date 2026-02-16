use std::fmt::{Debug, Formatter};
use std::io::Write;
use std::ops::DerefMut;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use crate::io::bus::IoBus;
use crate::io::disk::{DiskImage, DiskSource, DiskSpec, HD_DEFAULT_SIZE_MB};
use crate::io::pic::Pic;
use crate::vm::instructions::process;
use crate::vm::memory::{Memory, Segment, BIOS_ROM};
use crate::vm::registers::Registers;
use crate::vm::runtime::CpuFlag::{Carry, Interrupt, Overflow, Sign, Zero};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CpuType {
    Intel8086,
    Intel80186,
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum CpuFlag {
    Carry = 0,
    Parity = 2,
    AuxCarry = 4,
    Zero = 6,
    Sign = 7,
    Trap = 8,
    Interrupt = 9,
    Directional = 10,
    Overflow = 11,
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum SegmentType {
    ES = 0b_00,
    CS = 0b_01,
    SS = 0b_10,
    DS = 0b_11,
}

impl From<u8> for SegmentType {
    fn from(value: u8) -> Self {
        match value {
            0b_00 => SegmentType::ES,
            0b_01 => SegmentType::CS,
            0b_10 => SegmentType::SS,
            0b_11 => SegmentType::DS,
            _ => panic!("Unknown segment: {}", value),
        }
    }
}

pub enum Prefix {
    Rep(bool),
    Lock,
    Seg(SegmentType),
    Queued(Box<Prefix>),
}

pub struct HeadlessConfig {
    pub screen_dump_path: PathBuf,
    pub key_input_path: PathBuf,
}

pub struct Runtime {
    pub cpu_type: CpuType,
    pub registers: Registers,
    pub memory: Box<Memory>,
    pub flags: u16,
    running: bool,
    status: u16,
    pub prefix: Option<Prefix>,
    pub segment_override: Option<SegmentType>,
    pub io_bus: Option<IoBus>,
    pub bios_handlers: [Option<fn(&mut Runtime)>; 256],
    pub disks: Vec<Option<DiskImage>>,
    pub hard_disks: Vec<Option<DiskImage>>,
    pub pic: Option<Arc<Mutex<Pic>>>,
    pub keyboard_buffer: Option<Arc<Mutex<crate::io::keyboard::KeyBuffer>>>,
    pub keyboard_irq: Option<Arc<Mutex<bool>>>,
    pub keyboard_latch: Option<Arc<Mutex<u8>>>,
    pub keyboard_pending_ascii: u8,
    pub keyboard_data_available: Option<Arc<AtomicBool>>,
    pub boot_order: Vec<u8>,
    pub instruction_count: u64,
    pub trace: bool,
    pub interrupt_inhibit: bool,
    last_tick: Option<Instant>,
    last_refresh: Option<Instant>,
    vga_shadow: Vec<u8>,
    monitor_flag: Option<Arc<AtomicBool>>,
    // Headless mode fields
    headless: bool,
    screen_dump_path: Option<PathBuf>,
    key_input_path: Option<PathBuf>,
    key_input_offset: u64,
    key_wait_until: Option<Instant>,
    headless_frame: u64,
}

impl Runtime {
    #[cfg(test)]
    pub fn new_test() -> Self {
        use std::ops::DerefMut;
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());
        Self {
            cpu_type: CpuType::Intel8086,
            registers,
            memory,
            flags: 0,
            running: true,
            status: 0,
            prefix: None,
            segment_override: None,
            io_bus: None,
            bios_handlers: [None; 256],
            disks: Vec::new(),
            hard_disks: Vec::new(),
            pic: None,
            keyboard_buffer: None,
            keyboard_irq: None,
            keyboard_latch: None,
            keyboard_pending_ascii: 0,
            keyboard_data_available: None,
            boot_order: Vec::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,
            last_tick: None,
            last_refresh: None,
            vga_shadow: Vec::new(),
            monitor_flag: None,
            headless: false,
            screen_dump_path: None,
            key_input_path: None,
            key_input_offset: 0,
            key_wait_until: None,
            headless_frame: 0,
        }
    }

    /// Test constructor with PIC, IoBus, and keyboard shared state.
    /// Does NOT start input thread, raw mode, or BIOS init.
    #[cfg(test)]
    pub fn new_bios_test() -> Self {
        use std::ops::DerefMut;
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

        let pic = Arc::new(Mutex::new(Pic::new()));

        let keyboard = crate::io::keyboard::Keyboard::new();
        let kb_buffer = keyboard.shared_buffer();
        let kb_irq = keyboard.shared_irq();
        let kb_latch = keyboard.shared_latch();
        let kb_data_avail = keyboard.shared_data_available();

        let mut io_bus = crate::io::bus::IoBus::new();
        io_bus.register(
            0x20, 0x21,
            Box::new(crate::io::pic::SharedPic::new(pic.clone())),
        );
        io_bus.register(0x60, 0x60, Box::new(keyboard));
        io_bus.register(
            0x61, 0x61,
            Box::new(crate::io::port61::SystemControl::new()),
        );
        io_bus.register(
            0x64, 0x64,
            Box::new(crate::io::keyboard::KeyboardStatus::new(kb_data_avail.clone())),
        );

        Self {
            cpu_type: CpuType::Intel8086,
            registers,
            memory,
            flags: 0,
            running: true,
            status: 0,
            prefix: None,
            segment_override: None,
            io_bus: Some(io_bus),
            bios_handlers: [None; 256],
            disks: Vec::new(),
            hard_disks: Vec::new(),
            pic: Some(pic),
            keyboard_buffer: Some(kb_buffer),
            keyboard_irq: Some(kb_irq),
            keyboard_latch: Some(kb_latch),
            keyboard_pending_ascii: 0,
            keyboard_data_available: Some(kb_data_avail),
            boot_order: Vec::new(),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,
            last_tick: None,
            last_refresh: None,
            vga_shadow: Vec::new(),
            monitor_flag: None,
            headless: false,
            screen_dump_path: None,
            key_input_path: None,
            key_input_offset: 0,
            key_wait_until: None,
            headless_frame: 0,
        }
    }

    pub fn new(disk_specs: &[DiskSpec], hd_specs: &[String], boot_order: Option<Vec<u8>>, headless_config: Option<HeadlessConfig>) -> Self {
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

        // Build disks vec — size to fit the highest requested drive index
        let max_drive = disk_specs.iter().map(|s| s.drive).max().unwrap_or(0) as usize;
        let mut disks: Vec<Option<DiskImage>> = (0..=max_drive).map(|_| None).collect();
        for spec in disk_specs {
            let image = match &spec.source {
                DiskSource::FilePath(path) => {
                    if spec.drive == 0 {
                        // Boot drive: must exist
                        DiskImage::open(path).expect("Failed to open disk image")
                    } else {
                        // Non-boot drive: create if doesn't exist
                        DiskImage::open_or_create(path).expect("Failed to open/create disk image")
                    }
                }
                DiskSource::Memory(size) => {
                    let mut img = DiskImage::new_in_memory_sized(*size);
                    if let Err(e) = img.format_fat() {
                        eprintln!("Warning: could not format floppy: {}", e);
                    }
                    img
                }
            };
            disks[spec.drive as usize] = Some(image);
        }

        // Build hard disks vec
        let mut hard_disks: Vec<Option<DiskImage>> = Vec::new();
        for spec in hd_specs {
            let image = if spec.eq_ignore_ascii_case("memory") {
                DiskImage::new_in_memory_hard_disk(HD_DEFAULT_SIZE_MB)
            } else {
                let path = std::path::PathBuf::from(spec);
                DiskImage::open_or_create_hard_disk(&path, HD_DEFAULT_SIZE_MB)
                    .expect("Failed to open/create hard disk image")
            };
            hard_disks.push(Some(image));
        }

        // Create shared PIC
        let pic = Arc::new(Mutex::new(Pic::new()));

        // Create keyboard and clone shared state before moving to IoBus
        let keyboard = crate::io::keyboard::Keyboard::new();
        let kb_buffer = keyboard.shared_buffer();
        let kb_irq = keyboard.shared_irq();
        let kb_latch = keyboard.shared_latch();
        let kb_data_avail = keyboard.shared_data_available();
        let monitor_flag = keyboard.shared_monitor_flag();

        // Create I/O bus and register devices
        let mut io_bus = IoBus::new();
        io_bus.register(
            0x20,
            0x21,
            Box::new(crate::io::pic::SharedPic::new(pic.clone())),
        );
        io_bus.register(0x40, 0x43, Box::new(crate::io::pit::Pit::new()));
        io_bus.register(0x60, 0x60, Box::new(keyboard));
        io_bus.register(
            0x61,
            0x61,
            Box::new(crate::io::port61::SystemControl::new()),
        );
        io_bus.register(
            0x64,
            0x64,
            Box::new(crate::io::keyboard::KeyboardStatus::new(kb_data_avail.clone())),
        );
        io_bus.register(0x3D4, 0x3DA, Box::new(crate::io::vga::Vga::new()));

        let (headless, screen_dump_path, key_input_path) = match headless_config {
            Some(cfg) => (true, Some(cfg.screen_dump_path), Some(cfg.key_input_path)),
            None => (false, None, None),
        };

        let mut vm = Self {
            cpu_type: CpuType::Intel80186,
            registers,
            memory,
            flags: 0,
            running: true,
            status: 0,
            prefix: None,
            segment_override: None,
            io_bus: Some(io_bus),
            bios_handlers: [None; 256],
            disks,
            hard_disks,
            pic: Some(pic),
            keyboard_buffer: Some(kb_buffer),
            keyboard_irq: Some(kb_irq),
            keyboard_latch: Some(kb_latch),
            keyboard_pending_ascii: 0,
            keyboard_data_available: Some(kb_data_avail),
            boot_order: boot_order.unwrap_or_else(|| vec![0x00, 0x80]),
            instruction_count: 0,
            trace: false,
            interrupt_inhibit: false,
            last_tick: Some(Instant::now()),
            last_refresh: Some(Instant::now()),
            vga_shadow: vec![0u8; 4000],
            monitor_flag: Some(monitor_flag),
            headless,
            screen_dump_path,
            key_input_path,
            key_input_offset: 0,
            key_wait_until: None,
            headless_frame: 0,
        };

        vm.set_flag(Interrupt);
        crate::bios::init::init_bios(&mut vm);
        vm
    }

    #[inline]
    pub fn is_186(&self) -> bool {
        self.cpu_type == CpuType::Intel80186
    }

    pub fn exit(&mut self, status: u16) {
        self.running = false;
        self.status = status;
    }

    #[inline]
    pub fn fetch_byte(&mut self) -> u8 {
        let res = self.registers.cs.read_byte(self.registers.pc.word());
        self.registers.pc.operation(1, u16::wrapping_add);
        res
    }

    #[inline]
    pub fn fetch_word(&mut self) -> u16 {
        let res = self.registers.cs.read_word(self.registers.pc.word());
        self.registers.pc.operation(2, u16::wrapping_add);
        res
    }

    pub fn data_segment(&mut self) -> &mut Segment {
        if let Some(segment) = self.segment_override {
            return self.get_segment(segment);
        }
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.get_segment(*segment);
        }
        &mut self.registers.ds
    }

    /// Picks the correct default segment for a ModR/M memory operand.
    /// BP-based addressing (rm=010, 011, or rm=110 with mod!=00) defaults to SS.
    /// All other modes default to DS. An explicit segment override prefix wins.
    pub fn effective_segment(&mut self, mod_val: u8, rm: u8) -> &mut Segment {
        if let Some(segment) = self.segment_override {
            return self.get_segment(segment);
        }
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.get_segment(*segment);
        }
        let bp_based = match rm {
            0b010 | 0b011 => true,
            0b110 => mod_val != 0b00,
            _ => false,
        };
        if bp_based {
            &mut self.registers.ss
        } else {
            &mut self.registers.ds
        }
    }



    #[inline(always)]
    pub fn set_flag(&mut self, flag: CpuFlag) {
        self.flags |= 1u16 << (flag as u8);
    }
    #[inline(always)]
    pub fn unset_flag(&mut self, flag: CpuFlag) {
        self.flags &= !(1u16 << (flag as u8));
    }

    #[inline(always)]
    pub fn update_flag(&mut self, flag: CpuFlag, active: bool) {
        if active {
            self.set_flag(flag);
        } else {
            self.unset_flag(flag);
        }
    }

    #[inline(always)]
    pub fn flip_flag(&mut self, flag: CpuFlag) {
        self.update_flag(flag, !self.check_flag(flag));
    }

    #[inline(always)]
    pub fn check_flag(&self, flag: CpuFlag) -> bool {
        (self.flags & 1u16 << (flag as u8)) != 0
    }

    pub fn run(&mut self) {
        if !self.headless {
            // In BIOS mode: enable raw terminal and clear screen for VGA rendering
            let _ = crossterm::terminal::enable_raw_mode();

            let _ = write!(std::io::stdout(), "\x1B[2J\x1B[H");
            let _ = std::io::stdout().flush();

            // Start keyboard input thread AFTER raw mode is enabled so it
            // doesn't pick up stale console events from before raw mode.
            if let (Some(ref buf), Some(ref irq), Some(ref mon)) = (
                &self.keyboard_buffer,
                &self.keyboard_irq,
                &self.monitor_flag,
            ) {
                crate::io::keyboard::start_keyboard_thread(buf.clone(), irq.clone(), mon.clone());
            }
        } else {
            eprintln!("[HEADLESS] Emulator started in headless mode");
            if let Some(ref p) = self.screen_dump_path {
                eprintln!("[HEADLESS] Screen dump: {}", p.display());
            }
            if let Some(ref p) = self.key_input_path {
                eprintln!("[HEADLESS] Key input: {}", p.display());
            }
        }

        while self.running {
            if self.trace {
                eprintln!("{:?}", self);
            }

            // In BIOS mode: check for pending hardware interrupts
            if self.interrupt_inhibit {
                // STI/MOV SS/POP SS inhibit interrupts for one instruction
                self.interrupt_inhibit = false;
            } else if self.check_flag(Interrupt) {
                self.check_hw_interrupts();
            }

            process(self);
            self.instruction_count += 1;

            // Deliver keyboard scancodes frequently (every 50 instructions)
            // so arrow keys (4 IRQ cycles each) don't lag. This is lightweight
            // — just a mutex check + optional VecDeque pop.
            if self.instruction_count % 50 == 0 {
                self.deliver_keyboard_irq();
            }

            // In BIOS mode: periodic timer tick, VGA refresh, and monitor check
            if self.instruction_count % 1000 == 0 {
                self.idle_tick();
                if !self.headless && self.check_monitor_flag() {
                    self.enter_monitor();
                }
            }
        }

        if !self.headless {
            // In BIOS mode: restore terminal on exit
            let _ = crossterm::terminal::disable_raw_mode();

            let _ = write!(std::io::stdout(), "\x1B[0m\x1B[?25h\n");
            let _ = std::io::stdout().flush();
        } else {
            eprintln!("[HEADLESS] Emulator stopped");
        }
    }

    pub(crate) fn check_hw_interrupts(&mut self) {
        let (vector, irr, isr) = if let Some(ref pic) = self.pic {
            let mut pic = pic.lock().unwrap();
            if pic.has_interrupt() {
                let v = pic.acknowledge();
                (v, pic.debug_irr(), pic.debug_isr())
            } else {
                (None, pic.debug_irr(), pic.debug_isr())
            }
        } else {
            (None, 0, 0)
        };

        if let Some(vector) = vector {
            let ivt_offset = vector as usize * 4;
            let new_ip = self.memory.read_word(ivt_offset);
            let new_cs = self.memory.read_word(ivt_offset + 2);
            let phys = (((new_cs as usize) << 4) + new_ip as usize) & 0xFFFFF;

            // ROM stub fast path (e.g. keyboard IRQ → INT 09h → CD F0 CF)
            if phys >= BIOS_ROM {
                let b0 = self.memory.read_byte(phys);
                if b0 == 0xCD {
                    let trap_vec = self.memory.read_byte((phys + 1) & 0xFFFFF);
                    if self.memory.read_byte((phys + 2) & 0xFFFFF) == 0xCF {
                        if let Some(handler) = self.bios_handlers[trap_vec as usize] {
                            let saved_if_tf = self.flags & 0x0300;
                            handler(self);
                            self.flags = (self.flags & !0x0300) | saved_if_tf;
                            return;
                        }
                    }
                }
            }

            // Standard IVT dispatch (EOI+IRET stub, hooked IRQ handlers, etc.)
            {
                let bda_head = self.memory.read_word(crate::vm::memory::BDA_BASE + 0x1A);
                let bda_tail = self.memory.read_word(crate::vm::memory::BDA_BASE + 0x1C);
                log::debug!("[HW-INT] vec={:02X} -> {:04X}:{:04X} IRR={:02X} ISR={:02X} ic={} SP={:04X} IF={} BDA={:02X}/{:02X}",
                           vector, new_cs, new_ip, irr, isr, self.instruction_count,
                           self.registers.sp.word(),
                           if self.check_flag(Interrupt) { 1 } else { 0 },
                           bda_head, bda_tail);
            }

            self.push_word(self.flags);
            self.unset_flag(Interrupt);
            self.unset_flag(CpuFlag::Trap);
            self.push_word(self.registers.cs.reg().word());
            self.push_word(self.registers.pc.word());
            self.registers.cs.reg_mut().set(new_cs);
            self.registers.pc.set(new_ip);
        }
    }

    /// Run periodic tasks (timer, keyboard, VGA refresh).
    /// Called from the run loop AND from blocking I/O handlers (INT 16h, HLT)
    /// to keep the system responsive during waits.
    pub fn idle_tick(&mut self) {
        self.check_timer_tick();
        self.deliver_keyboard_irq();
        if self.headless {
            self.poll_key_input();
            self.check_vga_refresh_headless();
        } else {
            self.check_vga_refresh();
        }
    }

    /// Dispatch pending hardware IRQs through the IVT, just like the main loop.
    /// If the IVT still points to our ROM stubs, uses the fast Tier 1 path.
    /// If DOS has hooked the vector, runs the guest handler via a mini
    /// execution loop so the full handler chain (DOS → BIOS) executes.
    pub fn service_bios_irqs(&mut self) {
        loop {
            let vector = if let Some(ref pic) = self.pic {
                let mut pic = pic.lock().unwrap();
                if pic.has_interrupt() {
                    pic.acknowledge()
                } else {
                    None
                }
            } else {
                None
            };

            let vector = match vector {
                Some(v) => v,
                None => break,
            };

            // Read IVT to find the actual handler target
            let ivt_offset = vector as usize * 4;
            let new_ip = self.memory.read_word(ivt_offset);
            let new_cs = self.memory.read_word(ivt_offset + 2);
            let phys = (((new_cs as usize) << 4) + new_ip as usize) & 0xFFFFF;

            if phys >= BIOS_ROM {
                let b0 = self.memory.read_byte(phys);

                // Generic IRET stub (0xCF) — no real handler, just send EOI
                if b0 == 0xCF {
                    if let Some(ref pic) = self.pic {
                        pic.lock().unwrap().eoi();
                    }
                    continue;
                }

                // ROM stub fast path: CD xx CF → call bios_handlers[xx] directly
                if b0 == 0xCD {
                    let trap_vec = self.memory.read_byte((phys + 1) & 0xFFFFF);
                    if self.memory.read_byte((phys + 2) & 0xFFFFF) == 0xCF {
                        if let Some(handler) = self.bios_handlers[trap_vec as usize] {
                            handler(self);
                            continue;
                        }
                    }
                }
            }

            // IVT target is NOT our ROM stub — DOS has hooked this interrupt.
            // Run the full guest handler chain so DOS's hook executes properly.
            self.run_guest_isr(new_cs, new_ip);
        }
    }

    /// Execute a guest interrupt service routine by pushing a sentinel return
    /// address and running the instruction loop until the handler IRETs back.
    fn run_guest_isr(&mut self, handler_cs: u16, handler_ip: u16) {
        // Sentinel: F000:0007 — an unused ROM address after the stubs.
        // We check CS:IP BEFORE calling process(), so the byte there is
        // never actually executed.
        const RET_SEG: u16 = 0xF000;
        const RET_OFF: u16 = 0x0007;

        // Save CS:IP so we can restore them after the handler completes.
        // This is critical when run_guest_isr is called from inside a Tier 1
        // handler (e.g. int16h's blocking loop via service_bios_irqs): after
        // the handler IRETs to the sentinel, CS:IP would be left at F000:0007
        // and the main loop would execute garbage instead of resuming DOS code.
        let saved_cs = self.registers.cs.reg().word();
        let saved_ip = self.registers.pc.word();
        let saved_flags = self.flags;
        let original_sp = self.registers.sp.word();

        // Push fake interrupt frame: FLAGS, sentinel CS, sentinel IP
        self.push_word(self.flags);
        self.unset_flag(CpuFlag::Interrupt);
        self.unset_flag(CpuFlag::Trap);
        self.push_word(RET_SEG);
        self.push_word(RET_OFF);

        // Jump to the hooked handler
        self.registers.cs.reg_mut().set(handler_cs);
        self.registers.pc.set(handler_ip);

        // Run guest instructions until the handler IRETs back to sentinel
        for _ in 0..500_000u32 {
            // Check sentinel BEFORE executing — never runs the byte at F000:0007
            if self.registers.cs.reg().word() == RET_SEG
                && self.registers.pc.word() == RET_OFF
                && self.registers.sp.word() == original_sp
            {
                break;
            }

            // Replicate main-loop interrupt handling
            if self.interrupt_inhibit {
                self.interrupt_inhibit = false;
            } else if self.check_flag(CpuFlag::Interrupt) {
                self.check_hw_interrupts();
            }

            process(self);
            self.instruction_count += 1;
        }

        // Restore CS:IP and FLAGS to their pre-ISR values.
        // The sentinel IRET already popped FLAGS from the stack (restoring
        // the value we pushed), but CS:IP are left at the sentinel address.
        self.registers.cs.reg_mut().set(saved_cs);
        self.registers.pc.set(saved_ip);
        self.flags = saved_flags;
    }

    fn check_timer_tick(&mut self) {
        let should_tick = if let Some(ref last_tick) = self.last_tick {
            last_tick.elapsed().as_millis() >= 55
        } else {
            false
        };

        if should_tick {
            // Increment BDA tick counter at 0x0040:006C
            let tick_addr = crate::vm::memory::BDA_BASE + 0x6C;
            let tick_lo = self.memory.read_word(tick_addr);
            let tick_hi = self.memory.read_word(tick_addr + 2);
            let tick = ((tick_hi as u32) << 16) | tick_lo as u32;
            let new_tick = tick.wrapping_add(1);
            self.memory.write_word(tick_addr, new_tick as u16);
            self.memory
                .write_word(tick_addr + 2, (new_tick >> 16) as u16);

            // Raise IRQ0 via shared PIC
            if let Some(ref pic) = self.pic {
                pic.lock().unwrap().raise_irq(0);
            }

            self.last_tick = Some(Instant::now());
        }
    }

    /// Deliver one pending scancode from the input thread via IRQ 1.
    /// Pops one entry from the crossterm buffer, latches the scancode to
    /// port 0x60, stores the ASCII for the BIOS handler, and raises IRQ 1.
    /// Only delivers if IRQ 1 is not already pending or in-service.
    pub(crate) fn deliver_keyboard_irq(&mut self) {
        // Guard: don't overwrite the latch while the previous scancode
        // hasn't been consumed yet.  The INT 09h handler clears this flag
        // when it reads port 0x60.  This covers the full window including
        // the gap between EOI (clears ISR) and IRET (restores IF).
        let data_avail = self.keyboard_data_available.as_ref()
            .map(|f| f.load(Ordering::SeqCst))
            .unwrap_or(false);

        // Also check if IRQ 1 is still pending or in-service in the PIC.
        let irq_busy = if let Some(ref pic) = self.pic {
            pic.lock().unwrap().irq_busy(1)
        } else {
            return;
        };

        if data_avail || irq_busy {
            let has_data = self.keyboard_buffer.as_ref()
                .map(|b| !b.lock().unwrap().is_empty())
                .unwrap_or(false);
            if has_data {
                eprintln!("[KB-SKIP] data_avail={} irq_busy={} ic={}",
                         data_avail, irq_busy, self.instruction_count);
            }
            return;
        }

        // Pop one scancode from crossterm buffer
        let (entry, buf_len) = if let Some(ref buf) = self.keyboard_buffer {
            let mut b = buf.lock().unwrap();
            let e = b.pop_front();
            let l = b.len();
            (e, l)
        } else {
            return;
        };

        let (scancode, ascii) = match entry {
            Some(e) => e,
            None => return,
        };

        eprintln!("[KB-DELIVER] sc={:02X} ascii={:02X} buf_remaining={} ic={}",
                   scancode, ascii, buf_len, self.instruction_count);

        // Latch scancode to port 0x60
        if let Some(ref latch) = self.keyboard_latch {
            *latch.lock().unwrap() = scancode;
        }

        // Mark data available so port 0x64 reports OBF=1
        if let Some(ref flag) = self.keyboard_data_available {
            flag.store(true, Ordering::SeqCst);
        }

        // Store ASCII for the BIOS INT 09h handler to use
        self.keyboard_pending_ascii = ascii;

        // Raise IRQ 1
        if let Some(ref pic) = self.pic {
            pic.lock().unwrap().raise_irq(1);
        }
    }

    /// Poll the key input file for new commands (headless mode).
    fn poll_key_input(&mut self) {
        let path = match &self.key_input_path {
            Some(p) => p.clone(),
            None => return,
        };

        // Respect wait commands
        if let Some(wait_until) = self.key_wait_until {
            if Instant::now() < wait_until {
                return;
            }
            self.key_wait_until = None;
        }

        // Try to open the file; if it doesn't exist yet, no-op
        let mut file = match std::fs::File::open(&path) {
            Ok(f) => f,
            Err(e) => {
                static ONCE: std::sync::Once = std::sync::Once::new();
                ONCE.call_once(|| eprintln!("[HEADLESS] key input file not found: {}", e));
                return;
            }
        };

        use std::io::{Read, Seek, SeekFrom};
        if file.seek(SeekFrom::Start(self.key_input_offset)).is_err() {
            return;
        }

        let mut new_data = String::new();
        if file.read_to_string(&mut new_data).is_err() {
            return;
        }
        if new_data.is_empty() {
            return;
        }

        eprintln!("[HEADLESS] read {} bytes from offset {}", new_data.len(), self.key_input_offset);

        // Process lines one at a time, tracking byte offset so `wait`
        // can pause and resume from the correct position.
        let base_offset = self.key_input_offset;
        let mut byte_pos = 0usize;

        for line in new_data.lines() {
            // Advance past this line (including the newline delimiter)
            let line_end = byte_pos + line.len();
            let next_pos = if new_data[line_end..].starts_with('\n') {
                line_end + 1
            } else if new_data[line_end..].starts_with("\r\n") {
                line_end + 2
            } else {
                line_end // last line, no newline
            };

            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                byte_pos = next_pos;
                continue;
            }

            let (cmd, args) = match line.split_once(' ') {
                Some((c, a)) => (c, a.trim()),
                None => (line, ""),
            };

            match cmd.to_lowercase().as_str() {
                "type" => {
                    self.headless_type_string(args);
                }
                "key" => {
                    self.headless_press_key(args);
                }
                "wait" => {
                    if let Ok(ms) = args.parse::<u64>() {
                        // Advance offset past the wait line, remaining lines
                        // will be read on the next poll after the wait expires.
                        self.key_input_offset = base_offset + next_pos as u64;
                        self.key_wait_until = Some(Instant::now() + std::time::Duration::from_millis(ms));
                        return;
                    }
                }
                "raw" => {
                    self.headless_raw_scancode(args);
                }
                "swap" => {
                    self.headless_swap_disk(args);
                }
                "eject" => {
                    self.headless_eject_disk(args);
                }
                _ => {
                    eprintln!("[HEADLESS] unknown command: {}", line);
                }
            }

            byte_pos = next_pos;
        }

        self.key_input_offset = base_offset + new_data.len() as u64;
    }

    /// Type a string by generating make+break for each character.
    fn headless_type_string(&mut self, text: &str) {
        let buf = match &self.keyboard_buffer {
            Some(b) => b.clone(),
            None => return,
        };

        let mut q = buf.lock().unwrap();
        for ch in text.chars() {
            if let Some((scancode, ascii, needs_shift)) = crate::io::keyboard::char_to_key(ch) {
                if needs_shift {
                    q.push_back((0x2A, 0)); // Left Shift make
                }
                q.push_back((scancode, ascii));     // make
                q.push_back((scancode | 0x80, 0));  // break
                if needs_shift {
                    q.push_back((0xAA, 0)); // Left Shift break
                }
            }
        }

        *self.keyboard_irq.as_ref().unwrap().lock().unwrap() = true;
    }

    /// Press a named key or key combination (e.g. "enter", "ctrl+c").
    fn headless_press_key(&mut self, key_str: &str) {
        let buf = match &self.keyboard_buffer {
            Some(b) => b.clone(),
            None => return,
        };

        let mut q = buf.lock().unwrap();

        // Parse modifier+key combinations
        let parts: Vec<&str> = key_str.split('+').collect();
        let key_name = parts.last().unwrap_or(&"");
        let has_ctrl = parts.iter().any(|p| p.eq_ignore_ascii_case("ctrl"));
        let has_alt = parts.iter().any(|p| p.eq_ignore_ascii_case("alt"));
        let has_shift = parts.iter().any(|p| p.eq_ignore_ascii_case("shift"));

        // Press modifiers
        if has_ctrl { q.push_back((0x1D, 0)); }  // Ctrl make
        if has_alt { q.push_back((0x38, 0)); }    // Alt make
        if has_shift { q.push_back((0x2A, 0)); }  // Shift make

        // Try named key first, then single character
        let key_entry = if let Some(entry) = crate::io::keyboard::parse_key_name(key_name) {
            Some(entry)
        } else if key_name.len() == 1 {
            let ch = key_name.chars().next().unwrap();
            crate::io::keyboard::char_to_key(ch).map(|(sc, mut ascii, _)| {
                if has_ctrl && ch.is_ascii_alphabetic() {
                    ascii = (ch.to_ascii_uppercase() as u8) & 0x1F;
                }
                (sc, ascii, false)
            })
        } else {
            None
        };

        if let Some((scancode, ascii, enhanced)) = key_entry {
            if enhanced { q.push_back((0xE0, 0x00)); }
            q.push_back((scancode, ascii));
            if enhanced { q.push_back((0xE0, 0x00)); }
            q.push_back((scancode | 0x80, 0));
        } else {
            eprintln!("[HEADLESS] unknown key: {}", key_str);
        }

        // Release modifiers (reverse order)
        if has_shift { q.push_back((0xAA, 0)); }
        if has_alt { q.push_back((0xB8, 0)); }
        if has_ctrl { q.push_back((0x9D, 0)); }

        *self.keyboard_irq.as_ref().unwrap().lock().unwrap() = true;
    }

    /// Inject raw scancodes from a "raw <scancode> <ascii>" command.
    fn headless_raw_scancode(&mut self, args: &str) {
        let buf = match &self.keyboard_buffer {
            Some(b) => b.clone(),
            None => return,
        };

        let parts: Vec<&str> = args.split_whitespace().collect();
        if parts.len() >= 2 {
            let scancode = u8::from_str_radix(parts[0], 16).unwrap_or(0);
            let ascii = u8::from_str_radix(parts[1], 16).unwrap_or(0);
            let mut q = buf.lock().unwrap();
            q.push_back((scancode, ascii));
            q.push_back((scancode | 0x80, 0));
            *self.keyboard_irq.as_ref().unwrap().lock().unwrap() = true;
        }
    }

    /// Swap a disk image from a headless command: "swap <drive> <path>"
    /// e.g. "swap a assets/ms-dos/Disk2.img" or "swap hd0 disk.img"
    fn headless_swap_disk(&mut self, args: &str) {
        let parts: Vec<&str> = args.splitn(2, ' ').collect();
        if parts.len() < 2 {
            eprintln!("[HEADLESS] swap: usage: swap <drive> <path>");
            return;
        }
        let drive_str = parts[0];
        let path_str = parts[1].trim();

        if let Some(hd_idx) = parse_hd(drive_str) {
            while self.hard_disks.len() <= hd_idx {
                self.hard_disks.push(None);
            }
            match DiskImage::open_or_create_hard_disk(
                Path::new(path_str),
                HD_DEFAULT_SIZE_MB,
            ) {
                Ok(disk) => {
                    self.hard_disks[hd_idx] = Some(disk);
                    eprintln!("[HEADLESS] HD{}: swapped to {}", hd_idx, path_str);
                }
                Err(e) => eprintln!("[HEADLESS] swap error: {}", e),
            }
        } else if let Some(drive_idx) = headless_parse_drive(drive_str) {
            while self.disks.len() <= drive_idx {
                self.disks.push(None);
            }
            if path_str.to_ascii_lowercase().starts_with("memory") {
                let size = if let Some(rest) = path_str
                    .strip_prefix("memory:")
                    .or_else(|| path_str.strip_prefix("MEMORY:"))
                {
                    rest.parse::<u64>().unwrap_or(1440) * 1024
                } else {
                    crate::io::disk::FLOPPY_144_SIZE
                };
                let mut img = DiskImage::new_in_memory_sized(size);
                if let Err(e) = img.format_fat() {
                    eprintln!("[HEADLESS] Warning: could not format floppy: {}", e);
                }
                self.disks[drive_idx] = Some(img);
                eprintln!("[HEADLESS] Drive {}: formatted {}KB in-memory floppy", drive_str.to_uppercase(), size / 1024);
            } else {
                match DiskImage::open_or_create(Path::new(path_str)) {
                    Ok(disk) => {
                        self.disks[drive_idx] = Some(disk);
                        eprintln!("[HEADLESS] Drive {}: swapped to {}", drive_str.to_uppercase(), path_str);
                    }
                    Err(e) => eprintln!("[HEADLESS] swap error: {}", e),
                }
            }
        }
    }

    /// Eject a disk from a headless command: "eject <drive>"
    fn headless_eject_disk(&mut self, args: &str) {
        let drive_str = args.trim();
        if let Some(hd_idx) = parse_hd(drive_str) {
            if hd_idx < self.hard_disks.len() {
                self.hard_disks[hd_idx] = None;
                eprintln!("[HEADLESS] HD{}: ejected", hd_idx);
            }
        } else if let Some(drive_idx) = headless_parse_drive(drive_str) {
            if drive_idx < self.disks.len() {
                self.disks[drive_idx] = None;
                eprintln!("[HEADLESS] Drive {}: ejected", drive_str.to_uppercase());
            }
        }
    }

    fn check_vga_refresh(&mut self) {
        let should_refresh = if let Some(ref last_refresh) = self.last_refresh {
            last_refresh.elapsed().as_millis() >= 50
        } else {
            false
        };

        if !should_refresh {
            return;
        }
        self.last_refresh = Some(Instant::now());

        use std::fmt::Write as _;

        let vga_base = crate::vm::memory::VGA_TEXT_BASE;
        let mut out = String::with_capacity(8192);
        let mut has_changes = false;
        let mut last_attr: u16 = 0xFFFF; // impossible value to force first color set

        for i in 0..2000usize {
            let offset = i * 2;
            let ch = self.memory.read_byte(vga_base + offset);
            let attr = self.memory.read_byte(vga_base + offset + 1);

            if ch == self.vga_shadow[offset] && attr == self.vga_shadow[offset + 1] {
                continue;
            }

            if !has_changes {
                out.push_str("\x1B[?25l"); // Hide cursor during update
                has_changes = true;
            }

            let row = i / 80;
            let col = i % 80;
            let _ = write!(out, "\x1B[{};{}H", row + 1, col + 1);

            // Set color if attribute changed
            if attr as u16 != last_attr {
                let fg = attr & 0x0F;
                let bg = (attr >> 4) & 0x0F; // 4 bits: treat bit 7 as intensity, not blink
                let _ = write!(
                    out,
                    "\x1B[0;{};{}m",
                    VGA_TO_ANSI_FG[fg as usize], VGA_TO_ANSI_BG[bg as usize]
                );
                last_attr = attr as u16;
            }

            out.push(CP437[ch as usize]);

            self.vga_shadow[offset] = ch;
            self.vga_shadow[offset + 1] = attr;
        }

        if has_changes {
            // Position terminal cursor at BDA cursor position
            let cursor_off = crate::vm::memory::BDA_BASE + 0x50;
            let col = self.memory.read_byte(cursor_off) as usize;
            let row = self.memory.read_byte(cursor_off + 1) as usize;
            let _ = write!(out, "\x1B[0m\x1B[{};{}H\x1B[?25h", row + 1, col + 1);

            let stdout = std::io::stdout();
            let mut lock = stdout.lock();
            let _ = std::io::Write::write_all(&mut lock, out.as_bytes());
            let _ = std::io::Write::flush(&mut lock);
        }
    }

    /// Write VGA text memory to the screen dump file (headless mode).
    fn check_vga_refresh_headless(&mut self) {
        let should_refresh = if let Some(ref last_refresh) = self.last_refresh {
            last_refresh.elapsed().as_millis() >= 50
        } else {
            false
        };

        if !should_refresh {
            return;
        }
        self.last_refresh = Some(Instant::now());

        let path = match &self.screen_dump_path {
            Some(p) => p.clone(),
            None => return,
        };

        let vga_base = crate::vm::memory::VGA_TEXT_BASE;

        // Read cursor position from BDA
        let cursor_off = crate::vm::memory::BDA_BASE + 0x50;
        let col = self.memory.read_byte(cursor_off);
        let row = self.memory.read_byte(cursor_off + 1);

        let mut out = String::with_capacity(2200);
        use std::fmt::Write as _;
        let _ = writeln!(out, "--- Frame {} | Cursor: {},{} ---", self.headless_frame, col, row);

        for r in 0..25usize {
            for c in 0..80usize {
                let offset = (r * 80 + c) * 2;
                let ch = self.memory.read_byte(vga_base + offset);
                // Map CP437 to ASCII-safe: keep printable ASCII, replace others with substitutes
                let display = cp437_to_ascii(ch);
                out.push(display);
            }
            out.push('\n');
        }

        self.headless_frame += 1;

        // Write atomically: write to temp file, then rename
        let tmp_path = path.with_extension("tmp");
        if let Ok(()) = std::fs::write(&tmp_path, out.as_bytes()) {
            let _ = std::fs::rename(&tmp_path, &path);
        }
    }

    /// Check if the F12 monitor flag is set.
    pub fn check_monitor_flag(&self) -> bool {
        self.monitor_flag
            .as_ref()
            .map_or(false, |f| f.load(Ordering::SeqCst))
    }

    pub fn enter_monitor(&mut self) {
        use std::io::Write;

        // Disable raw mode for normal terminal I/O
        let _ = crossterm::terminal::disable_raw_mode();
        let _ = write!(std::io::stdout(), "\x1B[0m\x1B[2J\x1B[H");
        let _ = std::io::stdout().flush();

        println!("=== Emulator Monitor (F12) ===");
        println!();
        println!("Floppy Drives:");
        for (i, disk) in self.disks.iter().enumerate() {
            let letter = (b'A' + i as u8) as char;
            match disk {
                Some(d) => {
                    let size_kb = d.total_bytes() / 1024;
                    println!("  {}: [loaded] ({}KB)", letter, size_kb);
                }
                None => println!("  {}: [empty]", letter),
            }
        }
        if !self.hard_disks.is_empty() {
            println!("Hard Disks:");
            for (i, disk) in self.hard_disks.iter().enumerate() {
                match disk {
                    Some(d) => {
                        let size_mb = d.total_bytes() / (1024 * 1024);
                        println!(
                            "  HD{}: [loaded] ({}MB, C/H/S={}/{}/{})",
                            i, size_mb, d.cylinders, d.heads, d.sectors_per_track
                        );
                    }
                    None => println!("  HD{}: [empty]", i),
                }
            }
        } else {
            println!("Hard Disks: (none)");
        }
        println!();
        println!("Commands:");
        println!("  swap <drive> <path>   Swap floppy image (e.g. swap a disk2.img)");
        println!("  swap <drive> memory   Insert formatted in-memory floppy (1.44MB)");
        println!("  swap <drive> memory:SIZE  Insert floppy of SIZE KB (720, 1200, 1440, 2880)");
        println!("  swap hd<n> <path>     Swap hard disk image (e.g. swap hd0 disk.img)");
        println!("  swap hd<n> memory     Insert blank in-memory hard disk");
        println!("  move <from> <to>      Move disk between drives (e.g. move b a)");
        println!("  save <drive> <path>   Save disk contents to file (drive or hd<n>)");
        println!("  eject <drive>         Eject disk from drive (drive or hd<n>)");
        println!("  put <file> <drive> [name]  Copy host file to FAT disk (e.g. put game.exe hd0)");
        println!("  put <dir> <drive>         Copy host directory to FAT disk recursively");
        println!("  resume                Resume emulation (or just press Enter)");
        println!();

        let stdin = std::io::stdin();
        loop {
            print!("> ");
            let _ = std::io::stdout().flush();
            let mut input = String::new();
            if stdin.read_line(&mut input).is_err() {
                break;
            }
            let input = input.trim();

            if input.is_empty() || input.eq_ignore_ascii_case("resume") {
                break;
            }

            let parts: Vec<&str> = input.splitn(3, ' ').collect();
            match parts[0].to_lowercase().as_str() {
                "swap" => {
                    if parts.len() < 3 {
                        println!("Usage: swap <drive> <path|memory>");
                        continue;
                    }
                    if let Some(hd_idx) = parse_hd(parts[1]) {
                        // Hard disk swap
                        while self.hard_disks.len() <= hd_idx {
                            self.hard_disks.push(None);
                        }
                        if parts[2].eq_ignore_ascii_case("memory") {
                            self.hard_disks[hd_idx] =
                                Some(DiskImage::new_in_memory_hard_disk(HD_DEFAULT_SIZE_MB));
                            println!(
                                "HD{}: blank in-memory hard disk inserted ({}MB)",
                                hd_idx, HD_DEFAULT_SIZE_MB
                            );
                        } else {
                            match DiskImage::open_or_create_hard_disk(
                                Path::new(parts[2]),
                                HD_DEFAULT_SIZE_MB,
                            ) {
                                Ok(disk) => {
                                    self.hard_disks[hd_idx] = Some(disk);
                                    println!("HD{}: {}", hd_idx, parts[2]);
                                }
                                Err(e) => println!("Error: {}", e),
                            }
                        }
                    } else if let Some(drive_idx) = parse_drive(parts[1]) {
                        // Floppy swap
                        while self.disks.len() <= drive_idx {
                            self.disks.push(None);
                        }
                        if parts[2].to_ascii_lowercase().starts_with("memory") {
                            let size = if let Some(rest) = parts[2]
                                .strip_prefix("memory:")
                                .or_else(|| parts[2].strip_prefix("MEMORY:"))
                            {
                                rest.parse::<u64>().unwrap_or(1440) * 1024
                            } else {
                                crate::io::disk::FLOPPY_144_SIZE
                            };
                            let mut img = DiskImage::new_in_memory_sized(size);
                            if let Err(e) = img.format_fat() {
                                println!("Warning: could not format floppy: {}", e);
                            }
                            self.disks[drive_idx] = Some(img);
                            println!(
                                "Drive {}: formatted {}KB in-memory floppy inserted",
                                parts[1].to_uppercase(),
                                size / 1024
                            );
                        } else {
                            match DiskImage::open_or_create(Path::new(parts[2])) {
                                Ok(disk) => {
                                    self.disks[drive_idx] = Some(disk);
                                    println!("Drive {}: {}", parts[1].to_uppercase(), parts[2]);
                                }
                                Err(e) => println!("Error: {}", e),
                            }
                        }
                    }
                }
                "move" => {
                    if parts.len() < 3 {
                        println!("Usage: move <from> <to>");
                        continue;
                    }
                    let from = match parse_drive(parts[1]) {
                        Some(d) => d,
                        None => continue,
                    };
                    let to = match parse_drive(parts[2]) {
                        Some(d) => d,
                        None => continue,
                    };
                    let max = from.max(to);
                    while self.disks.len() <= max {
                        self.disks.push(None);
                    }
                    let disk = self.disks[from].take();
                    if disk.is_some() {
                        self.disks[to] = disk;
                        println!(
                            "Moved drive {} -> {}",
                            parts[1].to_uppercase(),
                            parts[2].to_uppercase()
                        );
                    } else {
                        println!("Drive {} is empty", parts[1].to_uppercase());
                    }
                }
                "save" => {
                    if parts.len() < 3 {
                        println!("Usage: save <drive> <path>");
                        continue;
                    }
                    if let Some(hd_idx) = parse_hd(parts[1]) {
                        if let Some(ref mut disk) =
                            self.hard_disks.get_mut(hd_idx).and_then(|d| d.as_mut())
                        {
                            match disk.save_to_file(Path::new(parts[2])) {
                                Ok(()) => println!("HD{} saved to {}", hd_idx, parts[2]),
                                Err(e) => println!("Error: {}", e),
                            }
                        } else {
                            println!("HD{} is empty", hd_idx);
                        }
                    } else if let Some(drive_idx) = parse_drive(parts[1]) {
                        if let Some(ref mut disk) =
                            self.disks.get_mut(drive_idx).and_then(|d| d.as_mut())
                        {
                            match disk.save_to_file(Path::new(parts[2])) {
                                Ok(()) => println!(
                                    "Drive {} saved to {}",
                                    parts[1].to_uppercase(),
                                    parts[2]
                                ),
                                Err(e) => println!("Error: {}", e),
                            }
                        } else {
                            println!("Drive {} is empty", parts[1].to_uppercase());
                        }
                    }
                }
                "eject" => {
                    if parts.len() < 2 {
                        println!("Usage: eject <drive>");
                        continue;
                    }
                    if let Some(hd_idx) = parse_hd(parts[1]) {
                        if hd_idx < self.hard_disks.len() {
                            self.hard_disks[hd_idx] = None;
                            println!("HD{}: ejected", hd_idx);
                        }
                    } else if let Some(drive_idx) = parse_drive(parts[1]) {
                        if drive_idx < self.disks.len() {
                            self.disks[drive_idx] = None;
                            println!("Drive {}: ejected", parts[1].to_uppercase());
                        }
                    }
                }
                "put" => {
                    // Re-parse with more splits for: put <path> <drive> [dos-name]
                    let put_parts: Vec<&str> = input.splitn(4, ' ').collect();
                    if put_parts.len() < 3 {
                        println!("Usage: put <host-path> <drive> [dos-name]");
                        println!("       put <host-dir> <drive>  (copies all files)");
                        continue;
                    }
                    let host_path = put_parts[1];
                    let drive_str = put_parts[2];
                    let host = Path::new(host_path);

                    let is_dir = host.is_dir();
                    if !host.exists() {
                        println!("Error: '{}' not found", host_path);
                        continue;
                    }

                    // Macro-like closure to get disk mutably
                    let do_put = |disk: &mut DiskImage, label: &str| {
                        if is_dir {
                            match disk.copy_dir_to_fat(host) {
                                Ok(n) => println!(
                                    "Copied {} files from '{}' -> {}:\\",
                                    n, host_path, label
                                ),
                                Err(e) => println!("Error: {}", e),
                            }
                        } else {
                            let host_data = match std::fs::read(host) {
                                Ok(d) => d,
                                Err(e) => {
                                    println!("Error reading '{}': {}", host_path, e);
                                    return;
                                }
                            };
                            let dos_name = if put_parts.len() >= 4 {
                                put_parts[3].to_string()
                            } else {
                                host.file_name()
                                    .map(|n| n.to_string_lossy().to_uppercase())
                                    .unwrap_or_else(|| "FILE.DAT".to_string())
                            };
                            match disk.copy_file_to_fat(&host_data, &dos_name) {
                                Ok(()) => println!(
                                    "Copied '{}' -> {}:\\{} ({} bytes)",
                                    host_path,
                                    label,
                                    dos_name,
                                    host_data.len()
                                ),
                                Err(e) => println!("Error: {}", e),
                            }
                        }
                    };

                    if let Some(hd_idx) = parse_hd(drive_str) {
                        if let Some(ref mut disk) =
                            self.hard_disks.get_mut(hd_idx).and_then(|d| d.as_mut())
                        {
                            do_put(disk, &format!("HD{}", hd_idx));
                        } else {
                            println!("HD{} is empty", hd_idx);
                        }
                    } else if let Some(drive_idx) = parse_drive(drive_str) {
                        if let Some(ref mut disk) =
                            self.disks.get_mut(drive_idx).and_then(|d| d.as_mut())
                        {
                            do_put(disk, &drive_str.to_uppercase());
                        } else {
                            println!("Drive {} is empty", drive_str.to_uppercase());
                        }
                    }
                }
                _ => println!("Unknown command. Type 'resume' or press Enter to continue."),
            }
        }

        // Re-enable raw mode and force full VGA redraw
        let _ = crossterm::terminal::enable_raw_mode();
        self.vga_shadow.fill(0);
        let _ = write!(std::io::stdout(), "\x1B[2J\x1B[H");
        let _ = std::io::stdout().flush();

        // Clear monitor flag to unblock input thread
        if let Some(ref flag) = self.monitor_flag {
            flag.store(false, Ordering::SeqCst);
        }
    }

    /// Look up a disk by BIOS drive number (DL < 0x80 = floppy, DL >= 0x80 = hard disk).
    pub fn get_disk(&self, dl: u8) -> Option<&DiskImage> {
        if dl >= 0x80 {
            self.hard_disks
                .get((dl - 0x80) as usize)
                .and_then(|d| d.as_ref())
        } else {
            self.disks.get(dl as usize).and_then(|d| d.as_ref())
        }
    }

    pub fn get_disk_mut(&mut self, dl: u8) -> Option<&mut DiskImage> {
        if dl >= 0x80 {
            self.hard_disks
                .get_mut((dl - 0x80) as usize)
                .and_then(|d| d.as_mut())
        } else {
            self.disks.get_mut(dl as usize).and_then(|d| d.as_mut())
        }
    }

    pub fn push_word(&mut self, word: u16) {
        let address = self.registers.sp.operation(2, u16::wrapping_sub);
        self.registers.ss.write_word(address, word);
    }

    pub fn pop_word(&mut self) -> u16 {
        let address = self.registers.sp.word();
        self.registers.sp.operation(2, u16::wrapping_add);
        self.registers.ss.read_word(address)
    }

    pub fn get_segment(&mut self, segment: SegmentType) -> &mut Segment {
        match segment {
            SegmentType::ES => &mut self.registers.es,
            SegmentType::CS => &mut self.registers.cs,
            SegmentType::SS => &mut self.registers.ss,
            SegmentType::DS => &mut self.registers.ds,
        }
    }
}

/// Like parse_drive but sends errors to stderr (for headless mode).
fn headless_parse_drive(s: &str) -> Option<usize> {
    match s.to_lowercase().as_str() {
        "a" => Some(0),
        "b" => Some(1),
        "c" => Some(2),
        "d" => Some(3),
        _ => {
            if parse_hd(s).is_none() {
                eprintln!("[HEADLESS] Invalid drive '{}' (use a-d or hd0, hd1, ...)", s);
            }
            None
        }
    }
}

fn parse_drive(s: &str) -> Option<usize> {
    match s.to_lowercase().as_str() {
        "a" => Some(0),
        "b" => Some(1),
        "c" => Some(2),
        "d" => Some(3),
        _ => {
            if parse_hd(s).is_none() {
                println!("Invalid drive '{}' (use a-d or hd0, hd1, ...)", s);
            }
            None
        }
    }
}

/// Parse "hd0", "hd1", etc. Returns the HD index.
fn parse_hd(s: &str) -> Option<usize> {
    let lower = s.to_lowercase();
    if lower.starts_with("hd") {
        lower[2..].parse::<usize>().ok()
    } else {
        None
    }
}

// VGA attribute to ANSI color mapping (VGA color order differs from ANSI)
const VGA_TO_ANSI_FG: [u8; 16] = [
    30, 34, 32, 36, 31, 35, 33, 37, 90, 94, 92, 96, 91, 95, 93, 97,
];
const VGA_TO_ANSI_BG: [u8; 16] = [
    40, 44, 42, 46, 41, 45, 43, 47, // dark backgrounds (VGA 0-7)
    100, 104, 102, 106, 101, 105, 103, 107, // bright backgrounds (VGA 8-F)
];

// CP437 to Unicode mapping for proper box-drawing and extended characters
const CP437: [char; 256] = [
    // 0x00-0x1F: Control character glyphs
    ' ', '\u{263A}', '\u{263B}', '\u{2665}', '\u{2666}', '\u{2663}', '\u{2660}', '\u{2022}',
    '\u{25D8}', '\u{25CB}', '\u{25D9}', '\u{2642}', '\u{2640}', '\u{266A}', '\u{266B}', '\u{263C}',
    '\u{25BA}', '\u{25C4}', '\u{2195}', '\u{203C}', '\u{00B6}', '\u{00A7}', '\u{25AC}', '\u{21A8}',
    '\u{2191}', '\u{2193}', '\u{2192}', '\u{2190}', '\u{221F}', '\u{2194}', '\u{25B2}', '\u{25BC}',
    // 0x20-0x7F: Standard ASCII
    ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', '0', '1', '2',
    '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?', '@', 'A', 'B', 'C', 'D', 'E',
    'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', '[', '\\', ']', '^', '_', '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
    'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~',
    '\u{2302}',
    // 0x80-0xFF: Extended characters (accented, box-drawing, math, etc.)
    '\u{00C7}', '\u{00FC}', '\u{00E9}', '\u{00E2}', '\u{00E4}', '\u{00E0}', '\u{00E5}', '\u{00E7}',
    '\u{00EA}', '\u{00EB}', '\u{00E8}', '\u{00EF}', '\u{00EE}', '\u{00EC}', '\u{00C4}', '\u{00C5}',
    '\u{00C9}', '\u{00E6}', '\u{00C6}', '\u{00F4}', '\u{00F6}', '\u{00F2}', '\u{00FB}', '\u{00F9}',
    '\u{00FF}', '\u{00D6}', '\u{00DC}', '\u{00A2}', '\u{00A3}', '\u{00A5}', '\u{20A7}', '\u{0192}',
    '\u{00E1}', '\u{00ED}', '\u{00F3}', '\u{00FA}', '\u{00F1}', '\u{00D1}', '\u{00AA}', '\u{00BA}',
    '\u{00BF}', '\u{2310}', '\u{00AC}', '\u{00BD}', '\u{00BC}', '\u{00A1}', '\u{00AB}', '\u{00BB}',
    '\u{2591}', '\u{2592}', '\u{2593}', '\u{2502}', '\u{2524}', '\u{2561}', '\u{2562}', '\u{2556}',
    '\u{2555}', '\u{2563}', '\u{2551}', '\u{2557}', '\u{255D}', '\u{255C}', '\u{255B}', '\u{2510}',
    '\u{2514}', '\u{2534}', '\u{252C}', '\u{251C}', '\u{2500}', '\u{253C}', '\u{255E}', '\u{255F}',
    '\u{255A}', '\u{2554}', '\u{2569}', '\u{2566}', '\u{2560}', '\u{2550}', '\u{256C}', '\u{2567}',
    '\u{2568}', '\u{2564}', '\u{2565}', '\u{2559}', '\u{2558}', '\u{2552}', '\u{2553}', '\u{256B}',
    '\u{256A}', '\u{2518}', '\u{250C}', '\u{2588}', '\u{2584}', '\u{258C}', '\u{2590}', '\u{2580}',
    '\u{03B1}', '\u{00DF}', '\u{0393}', '\u{03C0}', '\u{03A3}', '\u{03C3}', '\u{00B5}', '\u{03C4}',
    '\u{03A6}', '\u{0398}', '\u{03A9}', '\u{03B4}', '\u{221E}', '\u{03C6}', '\u{03B5}', '\u{2229}',
    '\u{2261}', '\u{00B1}', '\u{2265}', '\u{2264}', '\u{2320}', '\u{2321}', '\u{00F7}', '\u{2248}',
    '\u{00B0}', '\u{2219}', '\u{00B7}', '\u{221A}', '\u{207F}', '\u{00B2}', '\u{25A0}', '\u{00A0}',
];

// AX   BX   CX   DX   SP   BP   SI   DI  FLAGS IP
// 0000 0000 0000 0000 ffdc 0000 0000 0000 ---- 0000:31ed

/// Map a CP437 byte to an ASCII-safe character for the headless screen dump.
/// Printable ASCII (0x20..=0x7E) passes through; common CP437 box-drawing and
/// special characters are mapped to reasonable ASCII substitutes.
fn cp437_to_ascii(ch: u8) -> char {
    match ch {
        0x20..=0x7E => ch as char,
        0x00 => ' ',
        // Box-drawing singles
        0xB3 => '|', 0xB4 => '|', 0xC3 => '|', 0xC4 => '-', 0xC5 => '+',
        0xBF => '+', 0xC0 => '+', 0xD9 => '+', 0xDA => '+',
        // Box-drawing doubles
        0xBA => '|', 0xB9 => '|', 0xCC => '|', 0xCD => '=', 0xCE => '+',
        0xBB => '+', 0xBC => '+', 0xC8 => '+', 0xC9 => '+',
        // Mixed box-drawing
        0xB5 => '|', 0xB6 => '|', 0xC6 => '|', 0xC7 => '|',
        0xD5 => '+', 0xD6 => '+', 0xB8 => '+', 0xB7 => '+',
        0xBD => '+', 0xBE => '+', 0xD3 => '+', 0xD4 => '+',
        // T-junctions
        0xC1 => '+', 0xC2 => '+', 0xCB => '+', 0xCA => '+',
        0xD0 => '+', 0xD1 => '+', 0xD2 => '+', 0xCF => '+',
        // Block elements
        0xDB => '#', 0xDC => '_', 0xDD => '|', 0xDE => '|', 0xDF => '-',
        0xB0 => '.', 0xB1 => ':', 0xB2 => '#',
        // Arrows
        0x10 => '>', 0x11 => '<', 0x1E => '^', 0x1F => 'v',
        0x18 => '^', 0x19 => 'v', 0x1A => '>', 0x1B => '<',
        // Misc
        0x07 => '*', 0x09 => ' ', 0x0D => ' ', 0x0A => ' ',
        0xFE => '#', 0xFF => ' ',
        _ => '.',
    }
}

#[inline(always)]
fn show_flag(vm: &Runtime, flag: CpuFlag, c: char) -> char {
    if vm.check_flag(flag) {
        return c;
    }
    '-'
}

impl Debug for Runtime {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let pc = self.registers.pc.word();
        write!(
            f,
            "{:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {}{}{}{} {:04x}:{:02x}{:02x}",
            self.registers.ax.word(),
            self.registers.bx.word(),
            self.registers.cx.word(),
            self.registers.dx.word(),
            self.registers.sp.word(),
            self.registers.bp.word(),
            self.registers.si.word(),
            self.registers.di.word(),
            show_flag(self, Overflow, 'O'),
            show_flag(self, Sign, 'S'),
            show_flag(self, Zero, 'Z'),
            show_flag(self, Carry, 'C'),
            pc,
            self.registers.cs.read_byte(pc),
            self.registers.cs.read_byte(pc.wrapping_add(1)),
        )?;
        Ok(())
    }
}
