//! The pins of the inputs (`pins.env`, beside the crate's manifest), as the
//! tool reads them: compiled in, so that the report and the notices on the
//! disks name the inputs `fetch-inputs.sh` checked against the same file.
//!
//! The invariant: `pins.env` is the one place a pin is written. The tool
//! takes every pin it names from it, and never spells one itself.

/// The text of `pins.env`.
const PINS_FILE: &str = include_str!("../pins.env");

/// Whether `name` is the name of a shell's variable, in capitals: a capital
/// or an underscore, then capitals, digits and underscores. A shell that
/// sources the file takes a line of any other name for a command.
fn is_name(name: &str) -> bool {
    name.starts_with(|c: char| c.is_ascii_uppercase() || c == '_')
        && name
            .chars()
            .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_')
}

/// Whether `value` is made of what the pins need alone, for their URLs,
/// hashes, branch, tag and file names: letters, digits, `.`, `-`, `_`, `/`
/// and `:`, not beginning with a `-`. A shell that sources the file takes
/// these as they are; it would run, redirect or expand what else a value
/// held (`;`, `&`, `|`, `<`, `>`, `(`, `)`, `$`, a backtick, a quote, a
/// backslash, a `~`, a blank), and the programs it hands a value to would
/// take one that begins with a `-` for an option.
fn is_plain(value: &str) -> bool {
    !value.is_empty()
        && !value.starts_with('-')
        && value
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || ".-_/:".contains(c))
}

/// The pins the tool names, and every pin of the file.
#[derive(Debug)]
pub(crate) struct Pins {
    /// Where MS-DOS 4.00's restored source lives, its branch there, and the
    /// commit the build takes `v4.0` of.
    pub msdos_repository: &'static str,
    pub msdos_branch: &'static str,
    pub msdos_commit: &'static str,
    /// Where Doom8088's source lives, the tag of its release, the commit
    /// of the tag, and the release's zip.
    pub doom8088_repository: &'static str,
    pub doom8088_tag: &'static str,
    pub doom8088_commit: &'static str,
    pub doom8088_zip_url: &'static str,
    /// Every pin by its name, in the order of the file.
    pub all: Vec<(&'static str, &'static str)>,
}

impl Pins {
    /// The pins of `text`: a `NAME=value` a line, the blank lines and the
    /// comments (`#`) left out, each name a shell's (`is_name`) and each
    /// value plain (`is_plain`). A line that ends in CR LF ends there, as a
    /// checkout on Windows may give the file.
    pub(crate) fn parse(text: &'static str) -> Result<Self, String> {
        let mut all: Vec<(&str, &str)> = Vec::new();
        for (number, line) in text.lines().enumerate() {
            if line.trim().is_empty() || line.starts_with('#') {
                continue;
            }
            let Some((name, value)) = line.split_once('=') else {
                return Err(format!("line {} is no NAME=value: {line}", number + 1));
            };
            if !is_name(name) || !is_plain(value) {
                return Err(format!("line {} is not plain: {line}", number + 1));
            }
            if all.iter().any(|(earlier, _)| *earlier == name) {
                return Err(format!("{name} is pinned twice"));
            }
            all.push((name, value));
        }
        let get = |name: &str| {
            all.iter()
                .find(|(pinned, _)| *pinned == name)
                .map(|(_, value)| *value)
                .ok_or_else(|| format!("no {name} is pinned"))
        };
        Ok(Self {
            msdos_repository: get("MSDOS_REPOSITORY")?,
            msdos_branch: get("MSDOS_BRANCH")?,
            msdos_commit: get("MSDOS_COMMIT")?,
            doom8088_repository: get("DOOM8088_REPOSITORY")?,
            doom8088_tag: get("DOOM8088_TAG")?,
            doom8088_commit: get("DOOM8088_COMMIT")?,
            doom8088_zip_url: get("DOOM8088_ZIP_URL")?,
            all,
        })
    }

    /// The pins compiled into the tool.
    pub(crate) fn compiled_in() -> Self {
        Self::parse(PINS_FILE).expect("pins.env is read in a test of its own")
    }
}

#[cfg(test)]
#[path = "pins_tests.rs"]
mod tests;
