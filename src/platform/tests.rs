//! The rule of `platform` (`mod.rs`), checked against the app's own
//! sources: the suite fails when a file outside this directory asks which
//! host it is built for, reaches for a browser's own API, sleeps, blocks on
//! a future, or names a clock of `std::time`.
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
/// sleep, nor stand still until a future is ready.
const HOST_CALLS: [&str; 2] = ["thread::sleep", "block_on"];

/// The clocks of `std::time`, which panic on a host std has no clock for.
/// `Duration` is not one of them: it reads nothing.
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

/// What `source` does that only `platform` may: the line of each offence,
/// and what it is. Comments are not read. `is_test` says that the source is
/// a test's, which may make the calls of `HOST_CALLS`.
fn offences(source: &str, is_test: bool) -> Vec<(usize, String)> {
    let code: String = source
        .lines()
        .map(|line| line.split("//").next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");
    let line_of = |at: usize| code[..at].matches('\n').count() + 1;
    let mut found = Vec::new();

    let calls: &[&str] = if is_test { &[] } else { &HOST_CALLS };
    for name in HOST_NAMES.iter().chain(calls) {
        for (at, _) in code.match_indices(name) {
            found.push((line_of(at), name.to_string()));
        }
    }

    // A clock of std is named in the statement that names `std::time`: a
    // `use`, whose braces rustfmt may spread over lines, or a full path.
    for (at, _) in code.match_indices("std::time") {
        let statement = &code[at..];
        let statement = &statement[..statement.find(';').unwrap_or(statement.len())];
        for clock in STD_CLOCKS
            .iter()
            .filter(|clock| statement.contains(**clock))
        {
            found.push((line_of(at), format!("std::time's {clock}")));
        }
    }
    found.sort();
    found
}

/// No file of the app outside `platform` asks which host it is built for,
/// names a browser's API, sleeps, blocks on a future or names a clock of
/// `std::time`: what depends on the host is called by the name `platform`
/// gives it.
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
/// would report if it were not exempt, is full of them.
#[test]
fn the_sweep_finds_what_platform_itself_does() {
    let native = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(SOURCES)
        .join(HOME)
        .join("native.rs");
    let found = offences(&std::fs::read_to_string(native).unwrap(), false);
    for expected in HOST_CALLS {
        assert!(
            found.iter().any(|(_, offence)| offence == expected),
            "{expected} in {found:?}"
        );
    }
}

/// Each way of doing what only `platform` may is caught, wherever rustfmt
/// puts it, and reported with its line.
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
        "let adapter = pollster::block_on(instance.request_adapter(&options));",
        "use std::time::Instant;",
        "use std::time::{Duration, Instant};",
        "use std::time::{\n    Duration,\n    SystemTime,\n    UNIX_EPOCH,\n};",
        "let started = std::time::Instant::now();",
        "let now = std::time::SystemTime::now();",
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
}

/// A duration reads no clock, a clock that comes from `platform` is the
/// host's own, and a comment does nothing. A test may sleep and block; it
/// may not ask for the host.
#[test]
fn what_every_host_can_do_is_not_caught() {
    for innocent in [
        "use std::time::Duration;",
        "const SLICE: std::time::Duration = std::time::Duration::from_millis(10);",
        "use crate::platform::{self, Instant};",
        "let started = Instant::now();",
        "let since = SystemTime::now().duration_since(UNIX_EPOCH);",
        "// std::thread::sleep would block the page: #[cfg(target_arch)] is platform's",
        "/// Never `std::time::Instant` here.",
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
    assert!(!offences("#[cfg(target_arch = \"wasm32\")]", true).is_empty());
    assert!(!offences("use std::time::Instant;", true).is_empty());
}
