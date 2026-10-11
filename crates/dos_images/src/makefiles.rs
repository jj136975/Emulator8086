//! What the build runs, read from the source's own files: the modules, in
//! the order its makefiles build them (`modules`), the batch file that
//! builds them (`build_bat`), and the files the build makes, as its CPY.BAT
//! copies them (`copied_files`).
//!
//! The source's root makefile builds by recursion: its `all` rule changes
//! into each part and runs NMAKE there, and so do the makefiles of CMD, DEV
//! and MEMM. NMAKE 1.00 runs `$(make)` within itself, each level keeping
//! its parent's makefile in memory, and under FreeDOS, which leaves a
//! program 84 KB less than MS-DOS 4.00 does, MASM runs out of memory three
//! levels down. So BUILD.BAT runs NMAKE in each directory the recursion
//! reaches, a module, in the order it reaches them, as RUNME.BAT's notes
//! build a single module. The output is the same, byte for byte.
//!
//! The invariant: the modules are the directories that the makefiles which
//! only dispatch (a `cd` or a `$(make)`, and nothing else) lead to, each
//! once, in their order. A directory whose makefile does anything else is
//! a module; nothing is built that the makefiles do not name.

use crate::markers::Marker;
use std::fmt;
use std::io;

/// The commands of the `all` rule of `makefile`, if each of them only
/// dispatches: a `cd <path>` or a `$(make)`. The rule's commands are the
/// indented lines below its line, up to the next line that is not.
pub(crate) fn dispatching(makefile: &str) -> Option<Vec<String>> {
    let mut lines = makefile.lines();
    lines.find(|line| line.trim_end() == "all:")?;
    let commands: Vec<String> = lines
        .take_while(|line| line.trim().is_empty() || line.starts_with([' ', '\t']))
        .map(str::trim)
        .filter(|command| !command.is_empty())
        .map(str::to_string)
        .collect();
    let dispatches =
        |command: &String| command == "$(make)" || command.to_ascii_lowercase().starts_with("cd ");
    (!commands.is_empty() && commands.iter().all(dispatches)).then_some(commands)
}

/// Why the modules could not be told.
#[derive(Debug)]
pub(crate) enum MakefileError {
    /// The makefile of a directory could not be read.
    Unreadable { directory: String, error: io::Error },
    /// A makefile changes into a directory above the source's root.
    Escapes { directory: String, command: String },
    /// A makefile runs NMAKE in its own directory again.
    Loops { directory: String },
    /// The makefiles lead to a module twice.
    Twice { module: String },
}

impl fmt::Display for MakefileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let named = |directory: &str| {
            if directory.is_empty() {
                "the root".to_string()
            } else {
                directory.to_string()
            }
        };
        match self {
            Self::Unreadable { directory, error } => {
                write!(
                    f,
                    "the makefile of {} cannot be read: {error}",
                    named(directory)
                )
            }
            Self::Escapes { directory, command } => write!(
                f,
                "the makefile of {} leaves the source with `{command}`",
                named(directory)
            ),
            Self::Loops { directory } => write!(
                f,
                "the makefile of {} runs NMAKE in its own directory",
                named(directory)
            ),
            Self::Twice { module } => write!(f, "the makefiles lead to {module} twice"),
        }
    }
}

/// The modules of the source, as DOS paths below its root (`CMD\FIND`), in
/// the order the makefiles that only dispatch reach them from the root.
/// `makefile` reads the makefile of a directory, given as such a path, the
/// root as the empty one.
pub(crate) fn modules(
    makefile: &mut dyn FnMut(&str) -> io::Result<String>,
) -> Result<Vec<String>, MakefileError> {
    let mut found = Vec::new();
    walk("", makefile, &mut found)?;
    Ok(found)
}

fn walk(
    directory: &str,
    makefile: &mut dyn FnMut(&str) -> io::Result<String>,
    found: &mut Vec<String>,
) -> Result<(), MakefileError> {
    let text = makefile(directory).map_err(|error| MakefileError::Unreadable {
        directory: directory.to_string(),
        error,
    })?;
    let Some(commands) = dispatching(&text) else {
        if found.iter().any(|module| module == directory) {
            return Err(MakefileError::Twice {
                module: directory.to_string(),
            });
        }
        found.push(directory.to_string());
        return Ok(());
    };
    let mut here: Vec<String> = directory
        .split('\\')
        .filter(|part| !part.is_empty())
        .map(str::to_string)
        .collect();
    for command in commands {
        if command == "$(make)" {
            let below = here.join("\\");
            if below == directory {
                return Err(MakefileError::Loops { directory: below });
            }
            walk(&below, makefile, found)?;
            continue;
        }
        for part in command[3..].trim().split('\\') {
            match part {
                "" | "." => {}
                ".." => {
                    if here.pop().is_none() {
                        return Err(MakefileError::Escapes {
                            directory: directory.to_string(),
                            command: command.clone(),
                        });
                    }
                }
                name => here.push(name.to_ascii_uppercase()),
            }
        }
    }
    Ok(())
}

/// BUILD.BAT: sets the build's environment up (SETENV.BAT), runs NMAKE in
/// each module in turn, its output appended to C:\BUILD.LOG below a line
/// that names the module, and copies what the build made into C:\BIN
/// (CPY.BAT). It tells the host where it is with E9.COM, from the root of
/// C:. Its lines end in LF: whoever writes it onto the disk gives them DOS's
/// line ends.
pub(crate) fn build_bat(modules: &[String]) -> String {
    let tell = |marker: Marker| format!("C:\\E9 {}\n", marker.words());
    let mut bat = String::from("@ECHO OFF\n");
    bat += &tell(Marker::BuildStart);
    bat += "CD \\SRC\nCALL SETENV.BAT\n";
    for module in modules {
        bat += &format!("CD \\SRC\\{module}\n");
        bat += &tell(Marker::Module(module.clone()));
        bat += &format!(
            "ECHO {} {module} >> C:\\BUILD.LOG\n",
            crate::log_scan::SECTION
        );
        bat += "NMAKE -X - >> C:\\BUILD.LOG\n";
        bat += &format!(
            "IF ERRORLEVEL 1 {}",
            tell(Marker::ModuleFailed(module.clone()))
        );
    }
    bat += "CD \\SRC\n";
    bat += &tell(Marker::Copying);
    bat += "MD C:\\BIN\nCALL CPY.BAT C:\\BIN\n";
    bat += &tell(Marker::BuildDone);
    bat
}

/// The names of the files CPY.BAT copies, which are the files the build
/// makes, in its order: each `copy <source> %1` line names one.
pub(crate) fn copied_files(cpy_bat: &str) -> Vec<String> {
    cpy_bat
        .lines()
        .filter_map(|line| {
            let mut words = line.split_whitespace();
            if !words.next()?.eq_ignore_ascii_case("copy") {
                return None;
            }
            let source = words.next()?;
            Some(source.rsplit('\\').next()?.to_ascii_uppercase())
        })
        .collect()
}

#[cfg(test)]
#[path = "makefiles_tests.rs"]
mod tests;
