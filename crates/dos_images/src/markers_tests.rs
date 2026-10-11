//! The markers as the guest writes them and the host reads them.

use super::*;
use emu8086_core::io::disk::DiskController;
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::vm::runtime::Runtime;

/// Every marker is read back from the line its words make on the port,
/// with the blank DOS puts before a command tail or without it.
#[test]
fn every_marker_is_read_back_from_its_line() {
    for marker in [
        Marker::BuildStart,
        Marker::Module("CMD\\FIND".to_string()),
        Marker::ModuleFailed("MEMM\\EMM".to_string()),
        Marker::Copying,
        Marker::BuildDone,
        Marker::ChecksDone,
    ] {
        assert_eq!(
            Marker::parse(&format!(" {}", marker.words())),
            Some(marker.clone())
        );
        assert_eq!(Marker::parse(&marker.words()), Some(marker));
    }
}

/// A line is a marker only as a whole: what a program printed before it on
/// the same line, or after it, makes it none, and a module has a name of
/// one word.
#[test]
fn only_a_whole_line_is_a_marker() {
    for line in [
        "",
        "BIL!...FreeDOS kernel BUILD DONE",
        "BUILD DONE!",
        "BUILD",
        "MODULE",
        "MODULE ",
        "MODULE CMD\\FIND extra",
        "MODULE-FAILED",
        "setting up system to build the MS-DOS 4.01 SOURCE BAK...",
    ] {
        assert_eq!(Marker::parse(line), None, "{line:?}");
    }
}

/// E9.COM ends the line a program left open on the port, writes its command
/// tail as a line of its own, and terminates through INT 21h with return
/// code 0, whichever direction DOS left the string instructions in.
#[test]
fn e9_com_writes_its_tail_on_a_line_of_its_own() {
    const SEGMENT: u16 = 0x1000;
    const DIRECTION: u16 = 0x0400;
    let (_, keys) = std::sync::mpsc::channel();
    let mut machine = Runtime::with_drives(
        DiskController::new(),
        None,
        false,
        KeyboardController::with_receiver(keys),
    );
    machine.port_out_byte(0xE9, b'x');

    let base = (SEGMENT as usize) << 4;
    let memory = &mut machine.cpu.memory;
    for (at, byte) in E9_COM.iter().enumerate() {
        memory.write_byte(base + 0x100 + at, *byte);
    }
    // The tail as DOS hands it over: its length, then its bytes up to a CR.
    let tail = b" BUILD DONE\r";
    memory.write_byte(base + 0x80, tail.len() as u8 - 1);
    for (at, byte) in tail.iter().enumerate() {
        memory.write_byte(base + 0x81 + at, *byte);
    }
    // INT 21h leads to a HLT at 0000:0500.
    for (at, byte) in [0x00, 0x05, 0x00, 0x00].into_iter().enumerate() {
        memory.write_byte(0x21 * 4 + at, byte);
    }
    memory.write_byte(0x500, 0xF4);

    let registers = &mut machine.cpu.registers;
    for segment in [
        &mut registers.cs,
        &mut registers.ds,
        &mut registers.es,
        &mut registers.ss,
    ] {
        segment.reg_mut().set(SEGMENT);
    }
    registers.pc.set(0x100);
    registers.sp.set(0xFFFE);
    machine.cpu.flags |= DIRECTION;

    machine.run(1_000);
    assert!(machine.cpu.halted, "the program went to INT 21h");
    assert_eq!(machine.cpu.registers.ax.word(), 0x4C00);
    let console = machine.debug_console().unwrap();
    assert_eq!(console.lines().collect::<Vec<_>>(), ["x", " BUILD DONE"]);
    assert_eq!(console.line_in_progress(), "");
}
