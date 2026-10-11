//! The pins of the inputs (`pins.env`, beside the crate's manifest), as the
//! tool reads them: compiled in, so that the report and the notices on the
//! disks name the inputs `fetch-inputs.sh` checked against the same file.
//!
//! The invariant: `pins.env` is the one place a pin is written. The tool
//! takes every pin it names from it, and never spells one itself.

/// The text of `pins.env`.
const PINS_FILE: &str = include_str!("../pins.env");

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
    /// comments (`#`) left out. A line that ends in CR LF ends there, as a
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
            let plain = |text: &str| {
                !text.is_empty()
                    && !text.contains(|c: char| c.is_whitespace() || "\"'$`\\".contains(c))
            };
            if !plain(name) || !plain(value) {
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
