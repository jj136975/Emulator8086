//! Build script for the 8086 emulator.
//!
//! Two jobs:
//!   1. If `bios.asm` or any `.inc` beside it is newer than `bios.bin` AND
//!      `nasm` is on PATH, reassemble `bios.bin` from source.
//!   2. Patch the checksum byte at offset 0xFFFF so the 8-bit sum of the
//!      ROM image is 0, as required by IBM PC POST.
//!
//! `bios.bin` is a tracked file and the emulator cannot start without it, so
//! it is only ever replaced by a complete image. The assembler writes to a
//! scratch file; if the assembly fails, the build stops and the previous ROM
//! stays in place. Without `nasm` the reassembly is skipped with a warning.
//!
//! The checksum patch is pure Rust.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;

const ROM_DIR: &str = "assets/rom";
const ROM_SIZE: usize = 0x10000;

fn main() {
    let asm = Path::new("assets/rom/bios.asm");
    let bin = Path::new("assets/rom/bios.bin");

    // Watch the assembly sources; cargo will re-run this script when any of
    // them change. Do NOT watch bios.bin itself — we write to it below and
    // watching it would loop.
    let sources = rom_sources(Path::new(ROM_DIR)).unwrap_or_else(|_| vec![asm.to_path_buf()]);
    for source in &sources {
        println!("cargo:rerun-if-changed={}", source.display());
    }
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = std::env::var_os("OUT_DIR").expect("cargo sets OUT_DIR for build scripts");
    let scratch = Path::new(&out_dir).join("bios.bin");
    match rebuild_rom(&sources, asm, bin, &scratch, run_nasm) {
        Ok(Rebuild::UpToDate | Rebuild::Assembled) => {}
        Ok(Rebuild::Skipped(reason)) => {
            println!("cargo:warning=ROM reassembly skipped: {}", reason);
        }
        Err(message) => {
            // Cargo shows a failed script's stderr, with the assembler's
            // own messages above this line.
            eprintln!("error: {}", message);
            std::process::exit(1);
        }
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

/// The ROM's assembly sources: bios.asm and every .inc file beside it.
fn rom_sources(dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut sources = Vec::new();
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if matches!(path.extension().and_then(|e| e.to_str()), Some("asm" | "inc")) {
            sources.push(path);
        }
    }
    sources.sort();
    Ok(sources)
}

/// What `rebuild_rom` did.
#[derive(Debug, PartialEq, Eq)]
enum Rebuild {
    /// The ROM image is newer than every source.
    UpToDate,
    /// The ROM image was replaced by a freshly assembled one.
    Assembled,
    /// The image could not be reassembled and is kept as it is; the text
    /// says why.
    Skipped(String),
}

/// How a run of the assembler ended.
#[derive(Debug)]
enum Assembly {
    Done,
    /// The assembler ran and rejected the source; the text describes how
    /// it ended.
    Failed(String),
    /// The assembler could not be started.
    Unavailable(String),
}

/// Reassembles `bin` from `asm` if any of `sources` is newer than it.
///
/// `assemble` is given the source and the file to write, which is `scratch`
/// and never `bin`: an assembler removes or truncates its output when it
/// finds an error. `bin` is replaced only by a complete image. An error is
/// returned, and `bin` left as it was, when the source does not assemble or
/// does not produce a ROM image; an assembler that is not installed merely
/// skips the step as long as there is an image to use.
fn rebuild_rom(
    sources: &[PathBuf],
    asm: &Path,
    bin: &Path,
    scratch: &Path,
    assemble: impl FnOnce(&Path, &Path) -> Assembly,
) -> Result<Rebuild, String> {
    if !asm.exists() {
        return Ok(Rebuild::Skipped("bios.asm missing".to_string()));
    }
    if bin.exists() {
        let mut stale = false;
        for source in sources {
            match is_newer(source, bin) {
                Ok(newer) => stale |= newer,
                Err(e) => return Ok(Rebuild::Skipped(e.to_string())),
            }
        }
        if !stale {
            return Ok(Rebuild::UpToDate);
        }
    }

    match assemble(asm, scratch) {
        Assembly::Done => {}
        Assembly::Failed(how) => {
            return Err(format!(
                "{} did not assemble ({}); {} is left as it was",
                asm.display(),
                how,
                bin.display()
            ));
        }
        Assembly::Unavailable(why) if bin.exists() => return Ok(Rebuild::Skipped(why)),
        Assembly::Unavailable(why) => {
            return Err(format!(
                "{} does not exist and cannot be assembled from {}: {}",
                bin.display(),
                asm.display(),
                why
            ));
        }
    }

    let image = fs::read(scratch)
        .map_err(|e| format!("cannot read the assembled ROM {}: {}", scratch.display(), e))?;
    if image.len() != ROM_SIZE {
        return Err(format!(
            "{} assembled to {} bytes, but a ROM image is exactly {} bytes; {} is left as it was",
            asm.display(),
            image.len(),
            ROM_SIZE,
            bin.display()
        ));
    }
    replace_file(bin, &image).map_err(|e| format!("cannot replace {}: {}", bin.display(), e))?;
    Ok(Rebuild::Assembled)
}

/// Assembles `asm` into the flat binary `output` with nasm.
fn run_nasm(asm: &Path, output: &Path) -> Assembly {
    let status = Command::new("nasm")
        .arg("-f")
        .arg("bin")
        .arg("-I")
        .arg(format!("{}/", ROM_DIR))
        .arg(asm)
        .arg("-o")
        .arg(output)
        .status();
    match status {
        Ok(status) if status.success() => Assembly::Done,
        Ok(status) => Assembly::Failed(format!("nasm ended with {}", status)),
        Err(e) => Assembly::Unavailable(format!("failed to run nasm: {}", e)),
    }
}

/// Replaces the contents of `path`. The data goes to a temporary file
/// beside it, which is then renamed over it, so that `path` holds either
/// the old or the new contents and never a part of them.
fn replace_file(path: &Path, data: &[u8]) -> std::io::Result<()> {
    let mut temporary = path.as_os_str().to_owned();
    temporary.push(".tmp");
    let temporary = PathBuf::from(temporary);

    let replaced = fs::write(&temporary, data).and_then(|()| fs::rename(&temporary, path));
    if replaced.is_err() {
        let _ = fs::remove_file(&temporary);
    }
    replaced
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
    if data.len() != ROM_SIZE {
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
    replace_file(path, &data)?;
    Ok(Some((old, correct)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    /// A scratch directory with a ROM source that is newer than the ROM
    /// image, which consists of `OLD_ROM_BYTE`s.
    struct RomDir {
        dir: PathBuf,
        asm: PathBuf,
        bin: PathBuf,
        scratch: PathBuf,
    }

    const OLD_ROM_BYTE: u8 = 0xAA;

    impl RomDir {
        fn new(name: &str) -> Self {
            let dir = std::env::temp_dir()
                .join(format!("emu8086-build-{}-{}", std::process::id(), name));
            let _ = fs::remove_dir_all(&dir);
            fs::create_dir_all(&dir).unwrap();
            let rom = Self {
                asm: dir.join("bios.asm"),
                bin: dir.join("bios.bin"),
                scratch: dir.join("scratch.bin"),
                dir,
            };
            fs::write(&rom.bin, vec![OLD_ROM_BYTE; ROM_SIZE]).unwrap();
            rom.set_image_age(Duration::from_secs(3600));
            fs::write(&rom.asm, "; source\n").unwrap();
            rom
        }

        /// Makes the ROM image `age` older than now.
        fn set_image_age(&self, age: Duration) {
            let file = fs::File::options().write(true).open(&self.bin).unwrap();
            file.set_modified(SystemTime::now() - age).unwrap();
        }

        fn rebuild(&self, assemble: impl FnOnce(&Path, &Path) -> Assembly) -> Result<Rebuild, String> {
            rebuild_rom(std::slice::from_ref(&self.asm), &self.asm, &self.bin, &self.scratch, assemble)
        }

        fn image_is_the_old_one(&self) -> bool {
            fs::read(&self.bin).is_ok_and(|data| data == vec![OLD_ROM_BYTE; ROM_SIZE])
        }

        fn file_names(&self) -> Vec<String> {
            let mut names: Vec<String> = fs::read_dir(&self.dir)
                .unwrap()
                .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
                .collect();
            names.sort();
            names
        }
    }

    impl Drop for RomDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.dir);
        }
    }

    /// What an assembler does when it finds an error: it starts its output
    /// file and removes it again.
    fn failing_assembler(_asm: &Path, output: &Path) -> Assembly {
        fs::write(output, b"partial").unwrap();
        fs::remove_file(output).unwrap();
        Assembly::Failed("nasm ended with exit code: 1".to_string())
    }

    #[test]
    fn failed_assembly_stops_the_build_and_keeps_the_previous_rom() {
        let rom = RomDir::new("failed");

        let error = rom.rebuild(failing_assembler).unwrap_err();

        assert!(error.contains("did not assemble"), "{}", error);
        assert!(error.contains("bios.asm") && error.contains("exit code: 1"), "{}", error);
        assert!(rom.image_is_the_old_one());
        assert_eq!(rom.file_names(), ["bios.asm", "bios.bin"]);
    }

    /// The assembler is never pointed at the ROM image itself.
    #[test]
    fn assembler_writes_to_the_scratch_file() {
        let rom = RomDir::new("scratch");

        let result = rom.rebuild(|asm, output| {
            assert_eq!(asm, rom.asm);
            assert_eq!(output, rom.scratch);
            fs::write(output, vec![0x55u8; ROM_SIZE]).unwrap();
            Assembly::Done
        });

        assert_eq!(result, Ok(Rebuild::Assembled));
        assert_eq!(fs::read(&rom.bin).unwrap(), vec![0x55u8; ROM_SIZE]);
        assert_eq!(rom.file_names(), ["bios.asm", "bios.bin", "scratch.bin"]);
    }

    #[test]
    fn image_of_the_wrong_size_does_not_replace_the_rom() {
        let rom = RomDir::new("size");

        let error = rom
            .rebuild(|_, output| {
                fs::write(output, [1, 2, 3]).unwrap();
                Assembly::Done
            })
            .unwrap_err();

        assert!(error.contains("3 bytes"), "{}", error);
        assert!(rom.image_is_the_old_one());
    }

    #[test]
    fn missing_assembler_skips_the_step_when_there_is_an_image() {
        let unavailable =
            |_: &Path, _: &Path| Assembly::Unavailable("failed to run nasm: not found".to_string());

        let rom = RomDir::new("no-nasm");
        assert_eq!(
            rom.rebuild(unavailable),
            Ok(Rebuild::Skipped("failed to run nasm: not found".to_string()))
        );
        assert!(rom.image_is_the_old_one());

        // Without an image there is nothing to fall back on.
        fs::remove_file(&rom.bin).unwrap();
        let error = rom.rebuild(unavailable).unwrap_err();
        assert!(error.contains("does not exist"), "{}", error);
        assert!(error.contains("failed to run nasm"), "{}", error);
    }

    #[test]
    fn up_to_date_image_is_not_reassembled() {
        let rom = RomDir::new("fresh");
        let file = fs::File::options().write(true).open(&rom.bin).unwrap();
        file.set_modified(SystemTime::now() + Duration::from_secs(3600)).unwrap();
        drop(file);

        let result = rom.rebuild(|_, _| panic!("the assembler must not run"));

        assert_eq!(result, Ok(Rebuild::UpToDate));
        assert!(rom.image_is_the_old_one());
    }

    #[test]
    fn missing_image_is_assembled() {
        let rom = RomDir::new("first");
        fs::remove_file(&rom.bin).unwrap();

        let result = rom.rebuild(|_, output| {
            fs::write(output, vec![0x55u8; ROM_SIZE]).unwrap();
            Assembly::Done
        });

        assert_eq!(result, Ok(Rebuild::Assembled));
        assert_eq!(fs::read(&rom.bin).unwrap().len(), ROM_SIZE);
    }

    #[test]
    fn checksum_patch_makes_the_image_sum_to_zero() {
        let rom = RomDir::new("checksum");
        let sum = |path: &Path| {
            fs::read(path).unwrap().iter().fold(0u8, |acc, &b| acc.wrapping_add(b))
        };

        // 0xFFFF bytes of 0xAA sum to 0x56, so the last byte has to be 0xAA.
        assert_eq!(patch_checksum(&rom.bin).unwrap(), None);
        let mut image = vec![OLD_ROM_BYTE; ROM_SIZE];
        image[0xFFFF] = 0x00;
        fs::write(&rom.bin, &image).unwrap();
        assert_eq!(patch_checksum(&rom.bin).unwrap(), Some((0x00, 0xAA)));
        assert_eq!(sum(&rom.bin), 0);
        assert_eq!(patch_checksum(&rom.bin).unwrap(), None);
        assert_eq!(rom.file_names(), ["bios.asm", "bios.bin"]);

        // An image of the wrong size is reported and not touched.
        fs::write(&rom.bin, [1, 2, 3]).unwrap();
        assert!(patch_checksum(&rom.bin).is_err());
        assert_eq!(fs::read(&rom.bin).unwrap(), [1, 2, 3]);
    }
}
