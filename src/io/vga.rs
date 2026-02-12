use std::io::Write;
use crate::io::bus::IoDevice;
use crate::vm::memory::VGA_TEXT_BASE;

const COLS: usize = 80;
const ROWS: usize = 25;
const VGA_SIZE: usize = COLS * ROWS * 2;

pub struct Vga {
    crt_index: u8,
    crt_regs: [u8; 256],
    shadow: [u8; VGA_SIZE],
}

impl Vga {
    pub fn new() -> Self {
        Self {
            crt_index: 0,
            crt_regs: [0; 256],
            shadow: [0; VGA_SIZE],
        }
    }

    pub fn cursor_position(&self) -> (u8, u8) {
        let pos = ((self.crt_regs[0x0E] as u16) << 8) | self.crt_regs[0x0F] as u16;
        let row = (pos / COLS as u16) as u8;
        let col = (pos % COLS as u16) as u8;
        (row, col)
    }

    pub fn set_cursor_position(&mut self, row: u8, col: u8) {
        let pos = row as u16 * COLS as u16 + col as u16;
        self.crt_regs[0x0E] = (pos >> 8) as u8;
        self.crt_regs[0x0F] = pos as u8;
    }

    pub fn refresh(&mut self, memory: &[u8]) {
        let vga_mem = &memory[VGA_TEXT_BASE..VGA_TEXT_BASE + VGA_SIZE];

        if vga_mem == &self.shadow[..] {
            return;
        }

        let mut output = String::with_capacity(VGA_SIZE * 4);
        output.push_str("\x1B[H"); // Home cursor

        let mut last_attr: u8 = 0xFF;
        for row in 0..ROWS {
            for col in 0..COLS {
                let offset = (row * COLS + col) * 2;
                let ch = vga_mem[offset];
                let attr = vga_mem[offset + 1];

                if attr != last_attr {
                    output.push_str(&ansi_from_attr(attr));
                    last_attr = attr;
                }

                if ch >= 0x20 && ch < 0x7F {
                    output.push(ch as char);
                } else if ch == 0 {
                    output.push(' ');
                } else {
                    output.push('.');
                }
            }
            if row < ROWS - 1 {
                output.push_str("\r\n");
            }
        }
        output.push_str("\x1B[0m"); // Reset attributes

        // Position cursor
        let (cr, cc) = self.cursor_position();
        output.push_str(&format!("\x1B[{};{}H", cr as usize + 1, cc as usize + 1));

        let stdout = std::io::stdout();
        let mut lock = stdout.lock();
        let _ = lock.write_all(output.as_bytes());
        let _ = lock.flush();

        self.shadow.copy_from_slice(vga_mem);
    }
}

impl IoDevice for Vga {
    fn port_in_byte(&mut self, port: u16) -> u8 {
        match port {
            0x3D4 => self.crt_index,
            0x3D5 => self.crt_regs[self.crt_index as usize],
            _ => 0xFF,
        }
    }

    fn port_out_byte(&mut self, port: u16, value: u8) {
        match port {
            0x3D4 => self.crt_index = value,
            0x3D5 => self.crt_regs[self.crt_index as usize] = value,
            _ => {}
        }
    }

    fn name(&self) -> &'static str {
        "VGA"
    }
}

fn ansi_from_attr(attr: u8) -> String {
    let fg = attr & 0x0F;
    let bg = (attr >> 4) & 0x07;

    let fg_code = match fg {
        0x0 => "30",    // Black
        0x1 => "34",    // Blue
        0x2 => "32",    // Green
        0x3 => "36",    // Cyan
        0x4 => "31",    // Red
        0x5 => "35",    // Magenta
        0x6 => "33",    // Brown/Yellow
        0x7 => "37",    // Light gray
        0x8 => "90",    // Dark gray
        0x9 => "94",    // Light blue
        0xA => "92",    // Light green
        0xB => "96",    // Light cyan
        0xC => "91",    // Light red
        0xD => "95",    // Light magenta
        0xE => "93",    // Yellow
        0xF => "97",    // White
        _ => "37",
    };

    let bg_code = match bg {
        0 => "40",
        1 => "44",
        2 => "42",
        3 => "46",
        4 => "41",
        5 => "45",
        6 => "43",
        7 => "47",
        _ => "40",
    };

    format!("\x1B[{};{}m", fg_code, bg_code)
}
