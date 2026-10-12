//! The rule of `platform` (`mod.rs`), checked against the app's own
//! sources: the suite fails when a file outside this directory asks which
//! host it is built for, reaches for a browser's own API, sleeps, starts a
//! thread, waits on a channel, a lock, a future or the terminal, or names a
//! clock of `std::time`.
//!
//! The invariant: a failure is a decision to take, not a list to extend.
//! What depends on the host gets a name in `platform` that every host
//! offers, and the file that needed it calls that name. A test source may
//! do what only a desktop can, since the tests run on a desktop and nowhere
//! else; it still does not ask which host it is on.

use std::path::{Path, PathBuf};

/// The directory that is swept, from the root of the package, and the one
/// below it that is not.
const SOURCES: &str = "src";
const HOME: &str = "platform";

/// What asking for the host, or reaching for a browser's API, looks like
/// in a source file.
const HOST_NAMES: [&str; 6] = [
    "target_arch",
    "target_family",
    "target_os",
    "wasm_bindgen",
    "web_sys",
    "js_sys",
];

/// What only some hosts can do: a browser's one thread cannot be put to
/// sleep, nor stand still until a future is ready, a channel has an answer,
/// a lock is given up or a line is typed, and it cannot start another.
const HOST_CALLS: [&str; 11] = [
    "thread::sleep",
    "thread::park",
    "thread::spawn",
    "thread::scope",
    "thread::Builder",
    "block_on",
    ".recv(",
    ".recv_timeout(",
    "Condvar",
    "Barrier",
    "io::stdin",
];

/// The clocks of `std::time`, which panic on a host std has no clock for,
/// named below `time::`; `web_time` spells its own the same, and only
/// `platform` names those. `Duration` is not one of them: it reads nothing.
const STD_CLOCKS: [&str; 3] = ["Instant", "SystemTime", "UNIX_EPOCH"];

fn is_test_source(path: &Path) -> bool {
    let name = path.file_name().and_then(|name| name.to_str());
    name.is_some_and(|name| name == "tests.rs" || name.ends_with("_tests.rs"))
}

/// The Rust files in `dir` and in every directory below it.
fn rust_sources(dir: &Path, found: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            rust_sources(&path, found);
        } else if path.extension().is_some_and(|extension| extension == "rs") {
            found.push(path);
        }
    }
}

/// How long the literal at the start of `rest` is, with its quotes, if one
/// starts there: a string, to its closing quote, past the characters it
/// escapes; a raw string, `r"..."` or `r#"..."#`, which escapes nothing, to
/// the quote followed by as many hashes as opened it; a character literal,
/// such as '"' or '\'', whole. A lifetime, such as 'a, is no literal. One
/// that never closes runs to the end.
fn literal_len(rest: &str) -> Option<usize> {
    let bytes = rest.as_bytes();
    if rest.starts_with('"') {
        let mut at = 1;
        while at < bytes.len() {
            match bytes[at] {
                b'\\' => at += 2,
                b'"' => return Some(at + 1),
                _ => at += 1,
            }
        }
        return Some(bytes.len());
    }
    if let Some(raw) = rest.strip_prefix("br").or_else(|| rest.strip_prefix('r')) {
        let hashes = raw.bytes().take_while(|b| *b == b'#').count();
        if raw.as_bytes().get(hashes) == Some(&b'"') {
            let opening = rest.len() - raw.len() + hashes + 1;
            let closing = format!("\"{}", "#".repeat(hashes));
            return Some(match rest[opening..].find(&closing) {
                Some(end) => opening + end + closing.len(),
                None => rest.len(),
            });
        }
    }
    if let Some(quoted) = rest.strip_prefix('\'') {
        if let Some(escaped) = quoted.strip_prefix('\\') {
            return escaped.find('\'').map(|end| 2 + end + 1);
        }
        let mut chars = quoted.chars();
        let first = chars.next()?;
        if chars.next() == Some('\'') {
            return Some(1 + first.len_utf8() + 1);
        }
    }
    None
}

/// How long the block comment at the start of `rest` is, with the comments
/// it holds, which Rust nests; one that never closes runs to the end.
fn block_comment_len(rest: &str) -> usize {
    let bytes = rest.as_bytes();
    let mut depth = 0;
    let mut at = 0;
    while at < bytes.len() {
        if bytes[at..].starts_with(b"/*") {
            depth += 1;
            at += 2;
        } else if bytes[at..].starts_with(b"*/") {
            depth -= 1;
            at += 2;
            if depth == 0 {
                return at;
            }
        } else {
            at += 1;
        }
    }
    bytes.len()
}

/// What `source` holds of code: all of it but its comments, a `//` to the
/// end of its line and a `/* */`, outside a string, a raw string and a
/// character literal, which stay as they are. The line ends stay too, so
/// that a line keeps its number.
fn code_of(source: &str) -> String {
    let mut code = String::with_capacity(source.len());
    let mut at = 0;
    while at < source.len() {
        let rest = &source[at..];
        let taken = if rest.starts_with("//") {
            rest.find('\n').unwrap_or(rest.len())
        } else if rest.starts_with("/*") {
            let len = block_comment_len(rest);
            code.extend(rest[..len].chars().filter(|c| *c == '\n'));
            len
        } else {
            let len =
                literal_len(rest).unwrap_or_else(|| rest.chars().next().map_or(1, char::len_utf8));
            code.push_str(&rest[..len]);
            len
        };
        at += taken;
    }
    code
}

/// What `source` does that only `platform` may: the line of each offence,
/// and what it is. Comments are not read. `is_test` says that the source is
/// a test's, which may make the calls of `HOST_CALLS`.
fn offences(source: &str, is_test: bool) -> Vec<(usize, String)> {
    let code = code_of(source);
    let line_of = |at: usize| code[..at].matches('\n').count() + 1;
    let mut found = Vec::new();

    let calls: &[&str] = if is_test { &[] } else { &HOST_CALLS };
    for name in HOST_NAMES.iter().chain(calls) {
        for (at, _) in code.match_indices(name) {
            found.push((line_of(at), name.to_string()));
        }
    }

    // A clock of std is named by its path below `time`, whatever comes
    // before it (`std::time::Instant`, `time::Instant` after `use std::time`,
    // `time::Instant` in the braces of `use std::{...}`), or in the braces
    // that follow `time::`, which rustfmt may spread over lines, or by a
    // glob of `time`.
    for clock in STD_CLOCKS {
        for (at, _) in code.match_indices(&format!("time::{clock}")) {
            found.push((line_of(at), format!("time::{clock}")));
        }
    }
    for (at, _) in code.match_indices("time::{") {
        let group = &code[at..];
        let group = &group[..group.find('}').unwrap_or(group.len())];
        for clock in STD_CLOCKS.iter().filter(|clock| group.contains(**clock)) {
            found.push((line_of(at), format!("time::{clock}")));
        }
    }
    for (at, _) in code.match_indices("time::*") {
        found.push((line_of(at), "time::*".to_string()));
    }
    found.sort();
    found
}

/// No file of the app outside `platform` asks which host it is built for,
/// names a browser's API, sleeps, starts a thread, waits on a channel, a
/// lock, a future or the terminal, or names a clock of `std::time`: what
/// depends on the host is called by the name `platform` gives it.
#[test]
fn only_platform_knows_the_host() {
    let sources = Path::new(env!("CARGO_MANIFEST_DIR")).join(SOURCES);
    let home = sources.join(HOME);
    let mut files = Vec::new();
    rust_sources(&sources, &mut files);
    for expected in ["main.rs", "pacer.rs", "pacer_tests.rs", "native.rs"] {
        assert!(
            files.iter().any(|file| file.ends_with(expected)),
            "{expected} is among the files looked at"
        );
    }

    let mut found = Vec::new();
    for file in files.iter().filter(|file| !file.starts_with(&home)) {
        let source = std::fs::read_to_string(file).unwrap();
        let shown = file.strip_prefix(&sources).unwrap_or(file).display();
        for (line, offence) in offences(&source, is_test_source(file)) {
            found.push(format!("  {shown}:{line}: {offence}"));
        }
    }
    assert!(
        found.is_empty(),
        "a file outside `src/platform` does what depends on the host. Give it a name in \
         `platform` that every host offers, and call that.\n{}",
        found.join("\n")
    );
}

/// The sweep sees an offence where there is one: this directory, which it
/// would report if it were not exempt, is full of them. A desktop sleeps,
/// blocks on the GPU's start-up and waits for a dialog on a thread of its
/// own.
#[test]
fn the_sweep_finds_what_platform_itself_does() {
    let native = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(SOURCES)
        .join(HOME)
        .join("native.rs");
    let found = offences(&std::fs::read_to_string(native).unwrap(), false);
    for expected in ["thread::sleep", "block_on", "thread::Builder"] {
        assert!(
            found.iter().any(|(_, offence)| offence == expected),
            "{expected} in {found:?}"
        );
    }
}

/// Each way of doing what only `platform` may is caught, wherever rustfmt
/// puts it and whatever a line holds before it, and reported with its
/// line.
#[test]
fn what_depends_on_the_host_is_caught() {
    for offence in [
        "#[cfg(target_arch = \"wasm32\")]",
        "#[cfg(not(target_arch = \"wasm32\"))]",
        "if cfg!(target_arch = \"wasm32\") {",
        "#[cfg_attr(target_family = \"wasm\", allow(dead_code))]",
        "#[cfg(target_os = \"windows\")]",
        "use wasm_bindgen::prelude::*;",
        "let window = web_sys::window();",
        "let now = js_sys::Date::now();",
        "std::thread::sleep(duration);",
        "thread::sleep(IDLE_POLL);",
        "std::thread::park();",
        "std::thread::spawn(move || run());",
        "std::thread::scope(|scope| scope.spawn(run));",
        "let worker = std::thread::Builder::new().spawn(run);",
        "let answer = answers.recv();",
        "let answer = answers.recv_timeout(PATIENCE);",
        "let adapter = pollster::block_on(instance.request_adapter(&options));",
        "let ready = std::sync::Condvar::new();",
        "let gate = std::sync::Barrier::new(2);",
        "std::io::stdin().read_line(&mut line);",
        "use std::time::Instant;",
        "use std::time::{Duration, Instant};",
        "use std::time::{\n    Duration,\n    SystemTime,\n    UNIX_EPOCH,\n};",
        "use std::{sync::Arc, time::Instant};",
        "use std::{\n    sync::Arc,\n    time::{Duration, SystemTime},\n};",
        "use std::time;\nlet started = time::Instant::now();",
        "use std::time::*;",
        "let started = std::time::Instant::now();",
        "let now = std::time::SystemTime::now();",
        "let site = \"https://example.org\"; let started = std::time::Instant::now();",
        "let quote = '\"'; std::thread::sleep(FRAME);",
        "let raw = r#\"say \"hi\"\"#; std::thread::sleep(FRAME);",
        "let prefix = r\"\\\\?\\\"; std::thread::sleep(FRAME);",
        "/* a */ std::thread::sleep(FRAME);",
        "let é = 1; std::thread::sleep(FRAME);",
    ] {
        assert!(
            !offences(offence, false).is_empty(),
            "not caught: {offence}"
        );
    }
    assert_eq!(
        offences("let a = 1;\nstd::thread::sleep(d); // wait\n", false),
        vec![(2usize, "thread::sleep".to_string())],
        "an offence is reported with its line"
    );
    assert_eq!(
        offences(
            "let a = 1;\n/* two\nlines */ std::thread::sleep(d);\n",
            false
        ),
        vec![(3usize, "thread::sleep".to_string())],
        "with its line past a block comment"
    );
}

/// A duration reads no clock, a clock that comes from `platform` is the
/// host's own, an answer looked for without waiting waits for nothing, and
/// a comment does nothing, after a string as well, nor a block comment,
/// over lines or with another inside it. A test may sleep, block and wait;
/// it may not ask for the host.
#[test]
fn what_every_host_can_do_is_not_caught() {
    for innocent in [
        "use std::time::Duration;",
        "use std::time::{self, Duration};",
        "const SLICE: std::time::Duration = std::time::Duration::from_millis(10);",
        "use crate::platform::{self, Instant};",
        "let started = Instant::now();",
        "let since = SystemTime::now().duration_since(UNIX_EPOCH);",
        "let answer = answers.try_recv();",
        "// std::thread::sleep would block the page: #[cfg(target_arch)] is platform's",
        "/// Never `std::time::Instant` here.",
        "let site = \"https://example.org\"; // std::thread::sleep(FRAME)",
        "let quote = '\"'; // std::thread::sleep(FRAME)",
        "let prefix = r\"\\\\?\\\"; // std::thread::sleep(FRAME)",
        "let raw = r#\"say \"hi\"\"#; // block_on",
        "fn first<'a>(words: &'a [&str]) -> &'a str { words[0] } // block_on",
        "/* std::thread::sleep(FRAME) */ let a = 1;",
        "/* outer /* inner: block_on */ std::thread::sleep(FRAME) */",
        "/*\nstd::thread::sleep(FRAME);\n*/",
        "let é = \"/* not a comment\"; // std::thread::sleep(FRAME)",
        "platform::wait(duration);",
        "platform::spawn(async move { ready.send(start_up.await) });",
    ] {
        assert!(offences(innocent, false).is_empty(), "caught: {innocent}");
    }
    assert!(offences("std::thread::sleep(FRAME);", true).is_empty());
    assert!(
        offences(
            "pollster::block_on(adapter.request_device(&descriptor))",
            true
        )
        .is_empty()
    );
    assert!(offences("let answer = answers.recv();", true).is_empty());
    assert!(!offences("#[cfg(target_arch = \"wasm32\")]", true).is_empty());
    assert!(!offences("use std::time::Instant;", true).is_empty());
}
