//! The two rules of the crate (`lib.rs`), checked against its own sources
//! and manifest: the suite fails when a file of the crate reads a clock, or
//! when the crate gets a dependency that is not listed here.
//!
//! The invariant: a failure is a decision to take, not a list to extend. A
//! clock is read by the app, which tells the machine the time. A dependency
//! goes into `DEPENDENCIES` only once it is known to open no window and no
//! GPU and to read no clock, and after it was asked for (CLAUDE.md, "Hard
//! rules").

use std::path::{Path, PathBuf};

/// What reading a clock, or waiting on one, looks like in a source file.
const CLOCK_READS: [&str; 6] = [
    "Instant",
    "SystemTime",
    "UNIX_EPOCH",
    ".elapsed()",
    "thread::sleep",
    "chrono::",
];

/// The crates this one may depend on, tests and build script included.
const DEPENDENCIES: [&str; 5] = ["fatfs", "flate2", "log", "serde", "serde_json"];

/// This file, which names the clocks in order to look for them.
const THIS_FILE: &str = "crate_rules_tests.rs";

fn crate_dir() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}

/// The Rust files in `dir` and, if `recursive`, in the directories below it.
fn rust_files(dir: &Path, recursive: bool, files: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            if recursive {
                rust_files(&path, true, files);
            }
        } else if path.extension().is_some_and(|extension| extension == "rs") {
            files.push(path);
        }
    }
}

/// The names of the dependencies in every dependency table of a manifest.
fn dependency_names(manifest: &str) -> Vec<String> {
    let mut names = Vec::new();
    let mut in_dependencies = false;
    for line in manifest.lines().map(str::trim) {
        if line.starts_with('[') {
            in_dependencies = line.ends_with("dependencies]");
        } else if in_dependencies
            && !line.starts_with('#')
            && let Some((name, _)) = line.split_once(['.', '=', ' '])
        {
            names.push(name.to_string());
        }
    }
    names
}

/// The line of `manifest` that declares the dependency `name` in `table`.
fn dependency_line<'a>(manifest: &'a str, table: &str, name: &str) -> &'a str {
    manifest
        .lines()
        .map(str::trim)
        .skip_while(|line| *line != table)
        .skip(1)
        .take_while(|line| !line.starts_with('['))
        .find(|line| line.starts_with(name))
        .unwrap_or_else(|| panic!("no {name} in {table}"))
}

/// The crate reads no clock and waits on none: no file of it, the build
/// script and the tests included, names one outside a comment. Guest time
/// is the time the owner of a machine credits it with, and the wall clock
/// is passed in.
#[test]
fn no_source_file_reads_a_clock() {
    let mut files = Vec::new();
    rust_files(crate_dir(), false, &mut files);
    rust_files(&crate_dir().join("src"), true, &mut files);
    for expected in ["build.rs", "src/lib.rs", "src/vm/runtime.rs"] {
        assert!(
            files.iter().any(|file| file.ends_with(expected)),
            "{expected} is among the files looked at"
        );
    }

    let mut found = Vec::new();
    for file in files.iter().filter(|file| !file.ends_with(THIS_FILE)) {
        let source = std::fs::read_to_string(file).unwrap();
        for (number, line) in source.lines().enumerate() {
            let code = line.split("//").next().unwrap_or("");
            for read in CLOCK_READS.iter().filter(|read| code.contains(**read)) {
                found.push(format!("{}:{}: {}", file.display(), number + 1, read));
            }
        }
    }
    assert!(found.is_empty(), "a clock is read:\n{}", found.join("\n"));
}

/// The crate depends on what is listed and on nothing else: nothing that
/// opens a window or a GPU.
#[test]
fn the_crate_depends_on_nothing_but_what_is_listed() {
    let manifest = std::fs::read_to_string(crate_dir().join("Cargo.toml")).unwrap();
    let mut names = dependency_names(&manifest);
    names.sort();
    assert_eq!(
        names, DEPENDENCIES,
        "the dependencies of the crate are not the listed ones"
    );
}

/// fatfs has a clock of its own when its `chrono` feature is on. Neither
/// this crate nor the workspace asks for the feature: the app does, for its
/// own use of fatfs.
#[test]
fn fatfs_comes_without_a_clock_of_its_own() {
    let manifest = std::fs::read_to_string(crate_dir().join("Cargo.toml")).unwrap();
    assert_eq!(
        dependency_line(&manifest, "[dependencies]", "fatfs"),
        "fatfs.workspace = true"
    );

    let workspace = std::fs::read_to_string(crate_dir().join("../../Cargo.toml")).unwrap();
    let fatfs = dependency_line(&workspace, "[workspace.dependencies]", "fatfs");
    assert!(!fatfs.contains("chrono"), "{fatfs}");
}
