use std::path::Path;
use std::sync::atomic::Ordering;
use crate::io::disk::{DiskImage, HD_DEFAULT_SIZE_MB};
use crate::vm::runtime::Runtime;

pub fn enter_monitor(self: &mut Runtime) {
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
                if let Some(hd_idx) = crate::vm::runtime::parse_hd(parts[1]) {
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
                } else if let Some(drive_idx) = crate::vm::runtime::parse_drive(parts[1]) {
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
                let from = match crate::vm::runtime::parse_drive(parts[1]) {
                    Some(d) => d,
                    None => continue,
                };
                let to = match crate::vm::runtime::parse_drive(parts[2]) {
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
                if let Some(hd_idx) = crate::vm::runtime::parse_hd(parts[1]) {
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
                } else if let Some(drive_idx) = crate::vm::runtime::parse_drive(parts[1]) {
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
                if let Some(hd_idx) = crate::vm::runtime::parse_hd(parts[1]) {
                    if hd_idx < self.hard_disks.len() {
                        self.hard_disks[hd_idx] = None;
                        println!("HD{}: ejected", hd_idx);
                    }
                } else if let Some(drive_idx) = crate::vm::runtime::parse_drive(parts[1]) {
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

                if let Some(hd_idx) = crate::vm::runtime::parse_hd(drive_str) {
                    if let Some(ref mut disk) =
                        self.hard_disks.get_mut(hd_idx).and_then(|d| d.as_mut())
                    {
                        do_put(disk, &format!("HD{}", hd_idx));
                    } else {
                        println!("HD{} is empty", hd_idx);
                    }
                } else if let Some(drive_idx) = crate::vm::runtime::parse_drive(drive_str) {
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