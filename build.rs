//! Build script for the 8086 emulator.
//!
//! Two jobs:
//!   1. If `bios.asm` (or any included .inc) is newer than `bios.bin` AND
//!      `nasm` is on PATH, reassemble `bios.bin` from source.
//!   2. Patch the checksum byte at offset 0xFFFF so the 8-bit sum of the
//!      ROM image is 0, as required by IBM PC POST.
//!
//! The checksum patch is pure Rust; the reassembly step is best-effort
//! (skipped with a warning if `nasm` isn't available).

use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::SystemTime;

fn main() {
    // Watch the assembly sources; cargo will re-run this script when any of
    // them change. Do NOT watch bios.bin itself — we write to it below and
    // watching it would loop.
    for name in [
        "bios.asm",
        "bda.inc",
        "utils.inc",
        "int08_timer.inc",
        "int09_kbd.inc",
        "int10_video.inc",
        "int13_disk.inc",
        "int16_kbd.inc",
        "int19_boot.inc",
        "int1a_time.inc",
    ] {
        println!("cargo:rerun-if-changed=assets/rom/{}", name);
    }
    println!("cargo:rerun-if-changed=build.rs");

    let asm = Path::new("assets/rom/bios.asm");
    let bin = Path::new("assets/rom/bios.bin");

    if let Err(e) = try_rebuild_rom(asm, bin) {
        println!("cargo:warning=ROM reassembly skipped: {}", e);
    }

    match patch_checksum(bin) {
        Ok(Some((old, new))) => {
            println!(
                "cargo:warning=BIOS checksum auto-patched 0x{:02X} -> 0x{:02X}",
                old, new
            );
        }
        Ok(None) => {} // already correct
        Err(e) => panic!("BIOS checksum patch failed: {}", e),
    }
}

/// Reassemble bios.bin from bios.asm if the source is newer and nasm is
/// available. Returns Err(...) (logged as a warning, not a hard error) if
/// nasm isn't on PATH or assembly fails.
fn try_rebuild_rom(asm: &Path, bin: &Path) -> std::io::Result<()> {
    if !asm.exists() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "bios.asm missing",
        ));
    }
    if bin.exists() && !is_newer(asm, bin)? {
        return Ok(()); // bin is up to date
    }

    let status = Command::new("nasm")
        .arg("-f")
        .arg("bin")
        .arg("-I")
        .arg("assets/rom/")
        .arg(asm)
        .arg("-o")
        .arg(bin)
        .status()
        .map_err(|e| {
            std::io::Error::new(e.kind(), format!("failed to run nasm: {}", e))
        })?;
    if !status.success() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "nasm returned non-zero",
        ));
    }
    Ok(())
}

fn is_newer(a: &Path, b: &Path) -> std::io::Result<bool> {
    let a_t = a.metadata()?.modified()?;
    let b_t = b.metadata()?.modified().unwrap_or(SystemTime::UNIX_EPOCH);
    Ok(a_t > b_t)
}

/// Patch the final byte of the ROM so that the 8-bit sum of all bytes is 0.
/// Returns Some((old, new)) if a change was written, None if already correct.
fn patch_checksum(path: &Path) -> std::io::Result<Option<(u8, u8)>> {
    let mut data = fs::read(path)?;
    if data.len() != 0x10000 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "bios.bin must be exactly 64KB, got {} bytes",
                data.len()
            ),
        ));
    }

    let old = data[0xFFFF];
    data[0xFFFF] = 0;
    let partial = data.iter().fold(0u8, |acc, &b| acc.wrapping_add(b));
    let correct = 0u8.wrapping_sub(partial);
    data[0xFFFF] = correct;

    if correct == old {
        return Ok(None);
    }
    fs::write(path, &data)?;
    Ok(Some((old, correct)))
}
