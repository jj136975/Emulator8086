use std::collections::VecDeque;
use std::fmt::{Debug, Formatter};
use std::ops::{Add, DerefMut};
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use crate::a_out::executable::Executable;
use crate::io::bus::IoBus;
use crate::io::disk::DiskImage;
use crate::io::pic::Pic;
use crate::vm::instructions::process;
use crate::vm::memory::{Memory, Segment};
use crate::vm::registers::Registers;
use crate::vm::runtime::CpuFlag::{Carry, Interrupt, Overflow, Sign, Zero};
use crate::vm::runtime::Prefix::Queued;

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
            _ => panic!("Unknown segment: {}", value)
        }
    }
}

pub enum Prefix {
    Rep(bool),
    Lock,
    Seg(SegmentType),
    Queued(Box<Prefix>),
}

#[derive(PartialEq, Clone, Copy)]
pub enum ExecutionMode {
    MinixAout,
    BiosBoot,
}

pub struct Runtime {
    pub registers: Registers,
    pub memory: Box<Memory>,
    pub flags: u16,
    running: bool,
    status: u16,
    pub prefix: Option<Prefix>,
    pub mode: ExecutionMode,
    pub io_bus: Option<IoBus>,
    pub bios_handlers: [Option<fn(&mut Runtime)>; 256],
    pub disk: Option<DiskImage>,
    pub pic: Option<Arc<Mutex<Pic>>>,
    pub keyboard_buffer: Option<Arc<Mutex<VecDeque<u8>>>>,
    pub keyboard_irq: Option<Arc<Mutex<bool>>>,
    pub instruction_count: u64,
    pub trace: bool,
    last_tick: Option<Instant>,
    last_refresh: Option<Instant>,
    vga_shadow: Vec<u8>,
}

impl Runtime {
    pub fn new(exe: &Executable, args: Vec<String>) -> Self {
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new(memory.deref_mut());

        registers.cs.copy_data(0, exe.text_segment.as_slice());
        registers.ds.copy_data(0, exe.data_segment.as_slice());

        let mut vm = Self {
            registers,
            memory,
            flags: 0,
            running: true,
            status: 0,
            prefix: None,
            mode: ExecutionMode::MinixAout,
            io_bus: None,
            bios_handlers: [None; 256],
            disk: None,
            pic: None,
            keyboard_buffer: None,
            keyboard_irq: None,
            instruction_count: 0,
            trace: false,
            last_tick: None,
            last_refresh: None,
            vga_shadow: Vec::new(),
        };
        vm.set_flag(Interrupt);
        vm.init_args(args);
        vm
    }

    pub fn new_bios(disk_path: &Path) -> Self {
        let mut memory = Box::new(Memory::new());
        let registers = Registers::new_bios(memory.deref_mut());

        let disk = DiskImage::open(disk_path).expect("Failed to open disk image");

        // Create shared PIC
        let pic = Arc::new(Mutex::new(Pic::new()));

        // Create keyboard and clone shared state before moving to IoBus
        let keyboard = crate::io::keyboard::Keyboard::new();
        let kb_buffer = keyboard.shared_buffer();
        let kb_irq = keyboard.shared_irq();
        keyboard.start_input_thread();

        // Create I/O bus and register devices
        let mut io_bus = IoBus::new();
        io_bus.register(0x20, 0x21, Box::new(crate::io::pic::SharedPic::new(pic.clone())));
        io_bus.register(0x40, 0x43, Box::new(crate::io::pit::Pit::new()));
        io_bus.register(0x60, 0x60, Box::new(keyboard));
        io_bus.register(0x61, 0x61, Box::new(crate::io::port61::SystemControl::new()));
        io_bus.register(0x64, 0x64, Box::new(crate::io::keyboard::KeyboardStatus::new(kb_buffer.clone())));
        io_bus.register(0x3D4, 0x3D5, Box::new(crate::io::vga::Vga::new()));

        let mut vm = Self {
            registers,
            memory,
            flags: 0,
            running: true,
            status: 0,
            prefix: None,
            mode: ExecutionMode::BiosBoot,
            io_bus: Some(io_bus),
            bios_handlers: [None; 256],
            disk: Some(disk),
            pic: Some(pic),
            keyboard_buffer: Some(kb_buffer),
            keyboard_irq: Some(kb_irq),
            instruction_count: 0,
            trace: false,
            last_tick: Some(Instant::now()),
            last_refresh: Some(Instant::now()),
            vga_shadow: vec![0u8; 4000],
        };

        vm.set_flag(Interrupt);
        crate::bios::init::init_bios(&mut vm);
        vm
    }

    fn init_args(&mut self, args: Vec<String>) {
        let mut argv: Vec<u16> = Vec::with_capacity(args.len());
        let env = "PATH=/usr:/usr/bin";

        self.push_byte(0);
        for c in env.bytes().rev() {
            self.push_byte(c);
        }
        let env_addr = self.registers.sp.word();
        // Push strings (in reverse order so argv[0] ends up at lowest address)
        for arg in args.iter().rev() {
            self.push_byte(0);
            for c in arg.bytes().rev() {
                self.push_byte(c);
            }
            argv.push(self.registers.sp.word());
        }

        // Word-align the stack before pushing pointers
        if self.registers.sp.word() & 1 != 0 {
            self.push_byte(0);
        }
        // Push envp = [env_addr, NULL]
        self.push_word(0);
        self.push_word(env_addr);
        // Push NULL ptr of argv
        self.push_word(0);
        // Push argv addresses (argv is in reverse order from string push)
        for address in argv.iter() {
            self.push_word(*address);
        }
        // Push argc
        self.push_word(argv.len() as u16);
    }

    pub fn exit(&mut self, status: u16) {
        self.running = false;
        self.status = status;
    }

    #[inline]
    pub fn fetch_byte(&mut self) -> u8 {
        let res = self.registers.cs.read_byte(self.registers.pc.word());
        self.registers.pc.operation(1, u16::add);
        res
    }

    #[inline]
    pub fn fetch_word(&mut self) -> u16 {
        let res = self.registers.cs.read_word(self.registers.pc.word());
        self.registers.pc.operation(2, u16::add);
        res
    }

    pub fn data_segment(&mut self) -> &mut Segment {
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.get_segment(*segment);
        }
        &mut self.registers.ds
    }

    /// Picks the correct default segment for a ModR/M memory operand.
    /// BP-based addressing (rm=010, 011, or rm=110 with mod!=00) defaults to SS.
    /// All other modes default to DS. An explicit segment override prefix wins.
    pub fn effective_segment(&mut self, mod_val: u8, rm: u8) -> &mut Segment {
        if let Some(Prefix::Seg(segment)) = &self.prefix {
            return self.get_segment(*segment);
        }
        let bp_based = match rm {
            0b010 | 0b011 => true,
            0b110 => mod_val != 0b00,
            _ => false,
        };
        if bp_based { &mut self.registers.ss } else { &mut self.registers.ds }
    }

    #[inline]
    pub fn set_prefix(&mut self, prefix: Prefix) {
        self.prefix = Some(Queued(Box::new(prefix)));
    }

    #[inline]
    pub fn peek_byte(&self) -> u8 {
        self.registers.cs.read_byte(self.registers.pc.word())
    }

    #[inline]
    pub fn peek_word(&self) -> u16 {
        self.registers.cs.read_word(self.registers.pc.word())
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
        // In BIOS mode: clear terminal for VGA rendering
        if self.mode == ExecutionMode::BiosBoot {
            use std::io::Write;
            let _ = write!(std::io::stdout(), "\x1B[2J\x1B[H");
            let _ = std::io::stdout().flush();
        }

        while self.running {
            if self.trace {
                eprintln!("{:?}", self);
            }

            // In BIOS mode: check for pending hardware interrupts
            if self.mode == ExecutionMode::BiosBoot && self.check_flag(Interrupt) {
                self.check_hw_interrupts();
            }

            process(self);
            self.instruction_count += 1;

            // In BIOS mode: periodic timer tick and VGA refresh
            if self.mode == ExecutionMode::BiosBoot && self.instruction_count % 1000 == 0 {
                self.check_timer_tick();
                self.check_vga_refresh();
            }
        }

        // In BIOS mode: reset terminal on exit
        if self.mode == ExecutionMode::BiosBoot {
            use std::io::Write;
            let _ = write!(std::io::stdout(), "\x1B[0m\x1B[?25h\n");
            let _ = std::io::stdout().flush();
        }
    }

    fn check_hw_interrupts(&mut self) {
        let vector = if let Some(ref pic) = self.pic {
            pic.lock().unwrap().acknowledge()
        } else {
            None
        };

        if let Some(vector) = vector {
            let ivt_offset = vector as usize * 4;
            let new_ip = self.memory.read_word(ivt_offset);
            let new_cs = self.memory.read_word(ivt_offset + 2);
            if new_cs != 0 || new_ip != 0 {
                self.push_word(self.flags);
                self.unset_flag(Interrupt);
                self.unset_flag(CpuFlag::Trap);
                self.push_word(self.registers.cs.reg().word());
                self.push_word(self.registers.pc.word());
                self.registers.cs.reg_mut().set(new_cs);
                self.registers.pc.set(new_ip);
            }
        }
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
            self.memory.write_word(tick_addr + 2, (new_tick >> 16) as u16);

            // Raise IRQ0 via shared PIC
            if let Some(ref pic) = self.pic {
                pic.lock().unwrap().raise_irq(0);
            }

            self.last_tick = Some(Instant::now());
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
                let bg = (attr >> 4) & 0x07;
                let _ = write!(out, "\x1B[{};{}m",
                    VGA_TO_ANSI_FG[fg as usize],
                    VGA_TO_ANSI_BG[bg as usize]);
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

    pub fn push_word(&mut self, word: u16) {
        let address = self.registers.sp.operation(2, u16::wrapping_sub);
        self.registers.ss.write_word(address, word);
    }

    pub fn push_byte(&mut self, byte: u8) {
        let address = self.registers.sp.operation(1, u16::wrapping_sub);
        self.registers.ss.write_byte(address, byte);
    }

    pub fn pop_word(&mut self) -> u16 {
        let address = self.registers.sp.word();
        self.registers.sp.operation(2, u16::wrapping_add);
        self.registers.ss.read_word(address)
    }

    pub fn pop_byte(&mut self) -> u8 {
        let address = self.registers.sp.word();
        self.registers.sp.operation(1, u16::wrapping_add);
        self.registers.ss.read_byte(address)
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

// VGA attribute to ANSI color mapping (VGA color order differs from ANSI)
const VGA_TO_ANSI_FG: [u8; 16] = [30, 34, 32, 36, 31, 35, 33, 37, 90, 94, 92, 96, 91, 95, 93, 97];
const VGA_TO_ANSI_BG: [u8; 8] = [40, 44, 42, 46, 41, 45, 43, 47];

// CP437 to Unicode mapping for proper box-drawing and extended characters
const CP437: [char; 256] = [
    // 0x00-0x1F: Control character glyphs
    ' ', '\u{263A}', '\u{263B}', '\u{2665}', '\u{2666}', '\u{2663}', '\u{2660}', '\u{2022}',
    '\u{25D8}', '\u{25CB}', '\u{25D9}', '\u{2642}', '\u{2640}', '\u{266A}', '\u{266B}', '\u{263C}',
    '\u{25BA}', '\u{25C4}', '\u{2195}', '\u{203C}', '\u{00B6}', '\u{00A7}', '\u{25AC}', '\u{21A8}',
    '\u{2191}', '\u{2193}', '\u{2192}', '\u{2190}', '\u{221F}', '\u{2194}', '\u{25B2}', '\u{25BC}',
    // 0x20-0x7F: Standard ASCII
    ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
    '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_',
    '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', '\u{2302}',
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
        write!(f, "{:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {}{}{}{} {:04x}:{:02x}{:02x}",
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

