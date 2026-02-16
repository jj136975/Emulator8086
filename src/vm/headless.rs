use std::path::{Path, PathBuf};
use std::time::Instant;
use crate::io::disk::{DiskImage, HD_DEFAULT_SIZE_MB};


pub struct HeadlessConfig {
    pub screen_dump_path: PathBuf,
    pub key_input_path: PathBuf,
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