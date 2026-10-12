//! BUILD.LOG as the build leaves it: what NMAKE and every tool it ran
//! printed, module by module, each module's part below the line BUILD.BAT
//! writes before it (`SECTION` and the module's name, `section_start`).
//! `errors` finds the lines that tell of an error, and `section` gives a
//! module's part.
//!
//! The invariant: a build is good only if its log tells of no error. NMAKE
//! stops at an error, and BUILD.BAT tells the host of it, but a makefile
//! may let a command fail and go on: the log is read for the errors of
//! MASM, C, LINK and NMAKE themselves, and for a tool that ran out of
//! memory. Their warnings are no errors.

use crate::fat;

/// What BUILD.BAT has ECHO write into the log before a module's part,
/// followed by a blank and the module's name.
pub(crate) const SECTION: &str = "=====";

/// A line of the log that tells of an error, and the module whose part
/// holds it.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Finding {
    pub module: String,
    pub line: String,
}

/// The lines of the log. A tool that shows its progress ends a line in a
/// CR alone, to write over it: that ends a line too.
fn lines(log: &str) -> impl Iterator<Item = &str> {
    log.split(['\r', '\n'])
        .filter(|line| !line.trim().is_empty())
}

/// The module whose part begins at `line`, if the line is the one BUILD.BAT
/// has ECHO write before it and nothing else: `SECTION`, a blank, the
/// module's name, a DOS path in capitals (`CMD\FIND`), and the blank before
/// the redirection, which ECHO writes too. A tool's own line of `=` signs
/// is none.
fn section_start(line: &str) -> Option<&str> {
    let name = line
        .strip_prefix(SECTION)?
        .strip_prefix(' ')?
        .strip_suffix(' ')?;
    name.split('\\')
        .all(|part| fat::dos_name(part).as_deref() == Some(part))
        .then_some(name)
}

/// Whether a message of `line` is `word`, a blank, and a code of the tools
/// that begins as one of `codes` does: a letter, a digit, and three digits
/// more.
fn has_message(line: &str, word: &str, codes: &[&[u8]]) -> bool {
    line.match_indices(word).any(|(at, word)| {
        let code = &line.as_bytes()[at + word.len()..];
        code.len() >= 5 && codes.contains(&&code[..2]) && code[1..5].iter().all(u8::is_ascii_digit)
    })
}

/// Whether `line` tells of an error: a fatal one, an error of MASM (A2...),
/// C (C2..., and D2... of its command line), LINK (L2...) or NMAKE
/// (U1...), or a tool out of memory.
pub(crate) fn is_error(line: &str) -> bool {
    let lower = line.to_ascii_lowercase();
    lower.contains("fatal error")
        || lower.contains("out of memory")
        || has_message(line, "error ", &[b"A2", b"C2", b"D2", b"L2", b"U1"])
}

/// The warnings of the tools in `log`: the lines with a warning of MASM
/// (A4...), C (C4..., and D4... of its command line), LINK (L4...) or LIB
/// and NMAKE (U4...).
pub(crate) fn warnings(log: &str) -> usize {
    lines(log)
        .filter(|line| has_message(line, "warning ", &[b"A4", b"C4", b"D4", b"L4", b"U4"]))
        .count()
}

/// The lines of `log` that tell of an error, each with its module. A line
/// before the first module's part is the build's own.
pub(crate) fn errors(log: &str) -> Vec<Finding> {
    let mut module = "(before the first module)";
    let mut found = Vec::new();
    for line in lines(log) {
        if let Some(name) = section_start(line) {
            module = name;
        } else if is_error(line) {
            found.push(Finding {
                module: module.to_string(),
                line: line.trim_end().to_string(),
            });
        }
    }
    found
}

/// The part of `log` that `module` wrote, its lines ended in LF, if the
/// log has one.
pub(crate) fn section(log: &str, module: &str) -> Option<String> {
    let mut lines = lines(log).skip_while(|line| section_start(line) != Some(module));
    lines.next()?;
    let part: Vec<&str> = lines
        .take_while(|line| section_start(line).is_none())
        .map(str::trim_end)
        .collect();
    Some(part.join("\n"))
}

#[cfg(test)]
#[path = "log_scan_tests.rs"]
mod tests;
