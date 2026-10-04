//! The Port E9h panel: what the BIOS left in the log, and what follows as
//! the guest goes on writing to the port, the newest line in view.

use emu8086_core::io::console::KEPT_LINES;
use emu8086_core::vm::runtime::Runtime;

use super::*;
use crate::gui::overlay::testing::{Driver, Shell, waiting_for_a_key};

/// The panel's title, which the View menu and its window show.
const TITLE: &str = "Port E9h log";

/// The panel open over a machine whose BIOS found nothing to boot.
fn panel() -> Driver<Shell> {
    let (runtime, _keys) = waiting_for_a_key();
    Shell::with_panel(runtime, TITLE)
}

/// Writes `text` to the debug port, a byte at a time, as a guest does.
fn write(runtime: &mut Runtime, text: &str) {
    for byte in text.bytes() {
        runtime.port_out_byte(0xE9, byte);
    }
}

/// The count says how many lines the guest finished, and how many of them
/// are kept once there are more than that.
#[test]
fn the_count_says_what_was_written_and_what_is_kept() {
    let (mut runtime, _keys) = waiting_for_a_key();
    let counted = |runtime: &Runtime| count(&runtime.debug_console().expect("the machine has one"));
    assert_eq!(counted(&runtime), "1 line written");
    write(&mut runtime, "two\nthree\nand a line in progress");
    assert_eq!(counted(&runtime), "3 lines written");
    for _ in 0..KEPT_LINES {
        write(&mut runtime, "more\n");
    }
    let written = KEPT_LINES + 3;
    assert_eq!(
        counted(&runtime),
        format!("{written} lines written, the last {KEPT_LINES} kept")
    );
}

/// After the BIOS, the log holds its one line: the progress markers B, I,
/// L and F run into the copy of the message it printed.
#[test]
fn after_the_bios_the_log_holds_its_markers_and_its_message() {
    let shell = panel();
    let screen = shell.screen();
    assert!(
        screen.shows("BILFNo bootable disk - press any key to retry"),
        "{:?}",
        screen.texts()
    );
    assert!(screen.shows("1 line written"));
}

/// As the guest writes on, the newest lines come into view at the bottom,
/// with the line it is writing; the oldest leave the view.
#[test]
fn the_newest_lines_are_in_view() {
    let mut shell = panel();
    for line in 0..40 {
        write(&mut shell.target.runtime, &format!("line {line}\n"));
    }
    write(&mut shell.target.runtime, "C:\\>");
    shell.frame(Vec::new());
    let screen = shell.frame(Vec::new());
    for shown in ["line 39", "line 30", "C:\\>", "41 lines written"] {
        assert!(screen.shows(shown), "{shown:?} in {:?}", screen.texts());
    }
    for gone in ["line 0", "BILFNo bootable disk - press any key to retry"] {
        assert!(!screen.shows(gone), "{gone:?} is out of view");
    }
}
