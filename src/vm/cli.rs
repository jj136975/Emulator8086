use crate::io::disk::{DiskController, DiskImage, HD_DEFAULT_SIZE_MB, FLOPPY_144_SIZE};
use std::path::Path;

/// Parse "a"-"d" into BIOS floppy drive number (0x00-0x03).
fn parse_floppy(s: &str) -> Option<u8> {
    match s.to_lowercase().as_str() {
        "a" => Some(0x00),
        "b" => Some(0x01),
        "c" => Some(0x02),
        "d" => Some(0x03),
        _ => None,
    }
}

/// Parse "hd0", "hd1", etc. into BIOS hard disk number (0x80+).
fn parse_hd(s: &str) -> Option<u8> {
    let lower = s.to_lowercase();
    if lower.starts_with("hd") {
        lower[2..].parse::<u8>().ok().map(|n| 0x80 + n)
    } else {
        None
    }
}

/// Parse any drive string: "a"-"d" for floppies, "hd0"-"hdN" for hard disks.
fn parse_drive(s: &str) -> Option<u8> {
    parse_floppy(s)
        .or_else(|| parse_hd(s))
        .or_else(|| {
            println!("Invalid drive '{}' (use a-d or hd0, hd1, ...)", s);
            None
        })
}

/// Format a BIOS drive number for display.
fn drive_label(drive: u8) -> String {
    if drive < 0x80 {
        format!("{}:", (b'A' + drive) as char)
    } else {
        format!("HD{}", drive - 0x80)
    }
}

pub fn enter_monitor(disk_ctrl: &mut DiskController) {
    use std::io::Write;

    let _ = crossterm::terminal::disable_raw_mode();
    let _ = write!(std::io::stdout(), "\x1B[0m\x1B[2J\x1B[H");
    let _ = std::io::stdout().flush();

    println!("=== Emulator Monitor (F12) ===");
    println!();

    // List floppies
    println!("Floppy Drives:");
    for drive_num in 0x00..0x04u8 {
        let letter = (b'A' + drive_num) as char;
        match disk_ctrl.find_drive(drive_num) {
            Some(d) => {
                let size_kb = d.total_bytes() / 1024;
                let ro = if d.readonly { " [ro]" } else { "" };
                println!("  {}: [loaded] ({}KB){}", letter, size_kb, ro);
            }
            None => println!("  {}: [empty]", letter),
        }
    }

    // List hard disks
    let hd_count = disk_ctrl.drives.iter().filter(|(id, _)| *id >= 0x80).count();
    if hd_count > 0 {
        println!("Hard Disks:");
        for i in 0..4u8 {
            let drive_num = 0x80 + i;
            match disk_ctrl.find_drive(drive_num) {
                Some(d) => {
                    let size_mb = d.total_bytes() / (1024 * 1024);
                    let ro = if d.readonly { " [ro]" } else { "" };
                    println!(
                        "  HD{}: [loaded] ({}MB, C/H/S={}/{}/{}){}",
                        i, size_mb, d.cylinders, d.heads, d.sectors_per_track, ro
                    );
                }
                None => {}
            }
        }
    } else {
        println!("Hard Disks: (none)");
    }

    println!();
    println!("Commands:");
    println!("  swap <drive> <path>       Swap disk image (e.g. swap a disk2.img)");
    println!("  swap <drive> memory       Insert formatted blank disk");
    println!("  swap <drive> memory:SIZE  Insert floppy of SIZE KB (720, 1200, 1440, 2880)");
    println!("  move <from> <to>          Move disk between drives (e.g. move b a)");
    println!("  save <drive> <path>       Save disk contents to file");
    println!("  eject <drive>             Eject disk from drive");
    println!("  put <file> <drive> [name] Copy host file to FAT disk");
    println!("  put <dir> <drive>         Copy host directory to FAT disk recursively");
    println!("  resume                    Resume emulation (or just press Enter)");
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
                let drive_num = match parse_drive(parts[1]) {
                    Some(d) => d,
                    None => continue,
                };
                let is_hd = drive_num >= 0x80;

                if parts[2].to_ascii_lowercase().starts_with("memory") {
                    if is_hd {
                        let disk = DiskImage::new_in_memory_hard_disk(HD_DEFAULT_SIZE_MB);
                        disk_ctrl.detach(drive_num);
                        disk_ctrl.attach(drive_num, disk);
                        println!(
                            "{}: blank in-memory hard disk inserted ({}MB)",
                            drive_label(drive_num), HD_DEFAULT_SIZE_MB
                        );
                    } else {
                        let size = if let Some(rest) = parts[2]
                            .to_lowercase()
                            .strip_prefix("memory:")
                        {
                            rest.parse::<u64>().unwrap_or(1440) * 1024
                        } else {
                            FLOPPY_144_SIZE
                        };
                        let mut img = DiskImage::new_in_memory_sized(size, false);
                        if let Err(e) = img.format_fat() {
                            println!("Warning: could not format floppy: {}", e);
                        }
                        disk_ctrl.detach(drive_num);
                        disk_ctrl.attach(drive_num, img);
                        println!(
                            "{} formatted {}KB in-memory floppy inserted",
                            drive_label(drive_num), size / 1024
                        );
                    }
                } else {
                    let result = if is_hd {
                        DiskImage::open_or_create_hard_disk(
                            Path::new(parts[2]),
                            HD_DEFAULT_SIZE_MB,
                        )
                    } else {
                        DiskImage::open_or_create(Path::new(parts[2]), false)
                    };

                    match result {
                        Ok(disk) => {
                            disk_ctrl.detach(drive_num);
                            disk_ctrl.attach(drive_num, disk);
                            println!("{}: {}", drive_label(drive_num), parts[2]);
                        }
                        Err(e) => println!("Error: {}", e),
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
                match disk_ctrl.detach(from) {
                    Some(disk) => {
                        disk_ctrl.detach(to); // eject whatever's there
                        disk_ctrl.attach(to, disk);
                        println!(
                            "Moved {} -> {}",
                            drive_label(from),
                            drive_label(to)
                        );
                    }
                    None => println!("{} is empty", drive_label(from)),
                }
            }

            "save" => {
                if parts.len() < 3 {
                    println!("Usage: save <drive> <path>");
                    continue;
                }
                let drive_num = match parse_drive(parts[1]) {
                    Some(d) => d,
                    None => continue,
                };
                match disk_ctrl.find_drive_mut(drive_num) {
                    Some(disk) => match disk.save_to_file(Path::new(parts[2])) {
                        Ok(()) => println!("{} saved to {}", drive_label(drive_num), parts[2]),
                        Err(e) => println!("Error: {}", e),
                    },
                    None => println!("{} is empty", drive_label(drive_num)),
                }
            }

            "eject" => {
                if parts.len() < 2 {
                    println!("Usage: eject <drive>");
                    continue;
                }
                let drive_num = match parse_drive(parts[1]) {
                    Some(d) => d,
                    None => continue,
                };
                if disk_ctrl.detach(drive_num).is_some() {
                    println!("{}: ejected", drive_label(drive_num));
                } else {
                    println!("{} is empty", drive_label(drive_num));
                }
            }

            "put" => {
                let put_parts: Vec<&str> = input.splitn(4, ' ').collect();
                if put_parts.len() < 3 {
                    println!("Usage: put <host-path> <drive> [dos-name]");
                    continue;
                }
                let host_path = put_parts[1];
                let host = Path::new(host_path);

                if !host.exists() {
                    println!("Error: '{}' not found", host_path);
                    continue;
                }

                let drive_num = match parse_drive(put_parts[2]) {
                    Some(d) => d,
                    None => continue,
                };

                let label = drive_label(drive_num);
                let disk = match disk_ctrl.find_drive_mut(drive_num) {
                    Some(d) => d,
                    None => {
                        println!("{} is empty", label);
                        continue;
                    }
                };

                if host.is_dir() {
                    match disk.copy_dir_to_fat(host) {
                        Ok(n) => println!(
                            "Copied {} files from '{}' -> {}\\",
                            n, host_path, label
                        ),
                        Err(e) => println!("Error: {}", e),
                    }
                } else {
                    let host_data = match std::fs::read(host) {
                        Ok(d) => d,
                        Err(e) => {
                            println!("Error reading '{}': {}", host_path, e);
                            continue;
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
                            "Copied '{}' -> {}\\{} ({} bytes)",
                            host_path, label, dos_name, host_data.len()
                        ),
                        Err(e) => println!("Error: {}", e),
                    }
                }
            }

            _ => println!("Unknown command. Type 'resume' or press Enter to continue."),
        }
    }

    let _ = crossterm::terminal::enable_raw_mode();
    let _ = write!(std::io::stdout(), "\x1B[2J\x1B[H");
    let _ = std::io::stdout().flush();
}