use super::*;

fn feed(console: &mut DebugConsole, bytes: &[u8]) -> Vec<String> {
    bytes
        .iter()
        .filter_map(|byte| console.push(*byte))
        .collect()
}

/// Bytes are held until a line feed ends their line, which is then
/// passed on whole; a carriage return is dropped.
#[test]
fn output_is_collected_into_lines() {
    let mut console = DebugConsole::new();
    assert_eq!(feed(&mut console, b"BIL!"), Vec::<String>::new());
    assert_eq!(
        feed(&mut console, b"Starting MS-DOS...\r\n\r\nC:\\>"),
        vec!["BIL!Starting MS-DOS...".to_string(), String::new()]
    );
    assert_eq!(console.line, "C:\\>", "kept until its line ends");
}

#[test]
fn control_characters_are_not_passed_on() {
    let mut console = DebugConsole::new();
    assert_eq!(
        feed(&mut console, b"a\x07\x08\x1Bb\n"),
        vec!["a...b".to_string()]
    );
}

#[test]
fn a_line_without_an_end_is_logged_when_it_gets_long() {
    let mut console = DebugConsole::new();
    let lines = feed(&mut console, &[b'x'; MAX_LINE + 10]);
    assert_eq!(lines, vec!["x".repeat(MAX_LINE)]);
    assert_eq!(console.line.len(), 10);
}

/// The lines the guest finishes are kept for a reader, oldest first, and
/// the line it is writing can be read as well.
#[test]
fn finished_lines_are_kept_for_a_reader() {
    let mut console = DebugConsole::new();
    for byte in b"BIL!Starting MS-DOS...\r\n\r\nC:\\>" {
        console.write(*byte);
    }
    assert_eq!(
        console.lines().collect::<Vec<_>>(),
        ["BIL!Starting MS-DOS...", ""]
    );
    assert_eq!(console.line_in_progress(), "C:\\>");
    assert_eq!(console.lines_finished(), 2);
}

/// Only the last `KEPT_LINES` lines are kept: each line past them pushes
/// the oldest out, and the count of lines finished goes on.
#[test]
fn only_the_last_lines_are_kept() {
    let mut console = DebugConsole::new();
    for line in 0..KEPT_LINES + 10 {
        for byte in format!("line {line}\n").bytes() {
            console.write(byte);
        }
    }
    assert_eq!(console.lines().len(), KEPT_LINES);
    assert_eq!(console.lines().next(), Some("line 10"));
    let newest = format!("line {}", KEPT_LINES + 9);
    assert_eq!(console.lines().next_back(), Some(newest.as_str()));
    assert_eq!(console.lines_finished(), (KEPT_LINES + 10) as u64);
    assert_eq!(console.line_in_progress(), "");
}
