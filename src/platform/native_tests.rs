//! The command line, from its arguments to the machine they describe:
//! every test parses a line as `options` parses the process's own and
//! hands the result to `crate::machine`.

use std::ffi::OsString;

use clap::CommandFactory;

use super::*;
use crate::machine;
use crate::platform::Instant;
use emu8086_core::BIOS_ROM;
use emu8086_core::vm::runtime::Runtime;

fn args(line: &[&str]) -> Options {
    Cli::try_parse_from(line).unwrap().into()
}

/// The ROM is an optional positional argument: there is no `--rom`
/// option, and the help must not mention one.
#[test]
fn help_describes_the_options_that_exist() {
    let help = Cli::command().render_long_help().to_string();
    assert!(!help.contains("--rom"), "{}", help);
    assert!(help.contains("[ROM]"), "{}", help);
    assert!(help.contains(":ro"), "{}", help);
    assert!(Cli::try_parse_from(["emulator8086", "--rom", "bios.bin"]).is_err());
}

/// Every argument of the command line reaches the options under its own
/// name: none is dropped, and none lands in another's place.
#[test]
fn every_argument_reaches_the_options() {
    let options = args(&[
        "emulator8086",
        "bios.bin",
        "--disk",
        "a:boot.img",
        "--disk",
        "b:memory",
        "--hd",
        "hd0.img",
        "--boot",
        "hd0,a",
        "--trace",
    ]);
    assert_eq!(options.rom, Some(PathBuf::from("bios.bin")));
    assert_eq!(options.disk, ["a:boot.img", "b:memory"]);
    assert_eq!(options.hd, ["hd0.img"]);
    assert_eq!(options.boot.as_deref(), Some("hd0,a"));
    assert!(options.trace);
}

#[test]
fn bad_option_values_are_reported_not_guessed() {
    for bad in [
        &["emulator8086", "--disk", "e:foo.img"][..],
        &["emulator8086", "--disk", "a:one.img", "--disk", "a:two.img"],
        &["emulator8086", "--disk", "b:memory:1000"],
        &["emulator8086", "--boot", "hd200"],
        &["emulator8086", "--boot", "x"],
        &["emulator8086", "bios.bin", "--boot", "x"],
    ] {
        let error = machine(args(bad)).unwrap_err();
        assert!(!error.is_empty(), "{:?}", bad);
    }
}

/// A missing image is an error the user can read, not a panic.
#[test]
fn missing_boot_disk_is_reported_with_its_path() {
    let line = ["emulator8086", "--disk", "a:no-such-disk.img"];
    let error = machine(args(&line)).unwrap_err();
    assert!(error.contains("no-such-disk.img"), "{}", error);
}

/// Without a ROM argument the machine runs the BIOS built into the
/// binary; with one, it runs the image in that file instead.
#[test]
fn rom_argument_replaces_the_built_in_bios() {
    let rom_region = |rt: &Runtime| -> Vec<u8> {
        (0xF0000..0x100000)
            .map(|address| rt.cpu.memory.read_byte(address))
            .collect()
    };

    let (rt, _keys) = machine(args(&["emulator8086"])).unwrap();
    assert!(rom_region(&rt) == BIOS_ROM, "the built-in BIOS is loaded");

    // An 8 KB image: it ends at the top of the region, nothing below it.
    let path = std::env::temp_dir().join(format!("emu8086-cli-rom-{}.bin", std::process::id()));
    std::fs::write(&path, [0x5A; 0x2000]).unwrap();
    let line = [
        OsString::from("emulator8086"),
        path.clone().into_os_string(),
    ];
    let (rt, _keys) = machine(Cli::try_parse_from(line).unwrap().into()).unwrap();
    let _ = std::fs::remove_file(&path);
    let region = rom_region(&rt);
    assert!(region[..0xE000].iter().all(|&byte| byte == 0x00));
    assert!(region[0xE000..].iter().all(|&byte| byte == 0x5A));
}

/// A ROM file that cannot be used is an error the user can read.
#[test]
fn unusable_rom_file_is_reported_with_its_path() {
    let error = machine(args(&["emulator8086", "no-such-rom.bin"])).unwrap_err();
    assert!(error.contains("no-such-rom.bin"), "{}", error);
}

/// A text is saved in the file it is given for, and the caller is told
/// where; a file that cannot be written is an error that names it.
#[test]
fn save_text_writes_the_file_and_says_where() {
    let file = std::env::temp_dir().join(format!("emu8086-save-text-{}.txt", std::process::id()));
    let saved = save_text(&file, "the text\n");
    let written = std::fs::read_to_string(&file);
    let _ = std::fs::remove_file(&file);
    assert_eq!(saved, Ok(format!("written to {}", file.display())));
    assert_eq!(written.unwrap(), "the text\n");

    // A directory that does not exist cannot take the file.
    let nowhere = file.with_extension("missing").join("text.txt");
    let error = save_text(&nowhere, "the text\n").unwrap_err();
    let named = format!("Cannot write {}: ", nowhere.display());
    assert!(error.starts_with(&named), "{error}");
}

/// The desktop can wait: `wait` lets the time go by and says so.
#[test]
fn wait_lets_the_time_go_by() {
    let started = Instant::now();
    assert!(wait(Duration::from_millis(20)));
    assert!(started.elapsed() >= Duration::from_millis(20));
}

/// On the desktop a spawned task has run to its end when `spawn` returns:
/// what it made is where it put it.
#[test]
fn a_spawned_task_is_over_when_spawn_returns() {
    let (done, made) = std::sync::mpsc::channel();
    spawn(async move {
        let _ = done.send("made");
    });
    assert_eq!(made.try_recv(), Ok("made"));
}
