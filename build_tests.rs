//! Cargo never compiles a build script with `cfg(test)`: these tests run in
//! the crate's test binary, where `src/main.rs` includes build.rs as the
//! module `build_script`.

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
        let dir =
            std::env::temp_dir().join(format!("emu8086-build-{}-{}", std::process::id(), name));
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
        rebuild_rom(
            std::slice::from_ref(&self.asm),
            &self.asm,
            &self.bin,
            &self.scratch,
            assemble,
        )
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
    assert!(
        error.contains("bios.asm") && error.contains("exit code: 1"),
        "{}",
        error
    );
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
        Ok(Rebuild::Skipped(
            "failed to run nasm: not found".to_string()
        ))
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
    file.set_modified(SystemTime::now() + Duration::from_secs(3600))
        .unwrap();
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
        fs::read(path)
            .unwrap()
            .iter()
            .fold(0u8, |acc, &b| acc.wrapping_add(b))
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
