//! `pins.env` as the tool and the shell read it.

use super::*;

fn is_hex(text: &str, length: usize) -> bool {
    text.len() == length
        && text
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

/// The file pins everything the tool names, and each pin is what its name
/// says: a commit or a tree is a git object's 40 hex digits, a SHA-256 its
/// 64, and a URL is one of https.
#[test]
fn the_file_pins_what_the_tool_names_each_as_its_name_says() {
    let pins = Pins::parse(PINS_FILE).unwrap();
    for (name, value) in &pins.all {
        if name.ends_with("_COMMIT") || name.ends_with("_TREE") {
            assert!(is_hex(value, 40), "{name}={value}");
        } else if name.ends_with("_SHA256") {
            assert!(is_hex(value, 64), "{name}={value}");
        } else if name.ends_with("_URL") || name.ends_with("_REPOSITORY") {
            assert!(value.starts_with("https://"), "{name}={value}");
        }
    }
    assert!(is_hex(pins.msdos_commit, 40));
    assert_eq!(pins.doom8088_tag, "v20260304");
}

/// What is downloaded is what is pinned: Doom8088's zip is the release of
/// the pinned tag, and its GPL text is the repository's at the pinned
/// commit.
#[test]
fn the_downloads_are_those_of_the_pinned_tag_and_commit() {
    let pins = Pins::parse(PINS_FILE).unwrap();
    let release = format!(
        "{}/releases/download/{}/",
        pins.doom8088_repository, pins.doom8088_tag
    );
    assert!(
        pins.doom8088_zip_url.starts_with(&release),
        "{}",
        pins.doom8088_zip_url
    );
    let license = pins
        .all
        .iter()
        .find(|(name, _)| *name == "DOOM8088_LICENSE_URL")
        .unwrap()
        .1;
    assert!(
        license.contains(&format!("/{}/LICENSE", pins.doom8088_commit)),
        "{license}"
    );
}

/// A shell sources the file as it is: it would take a line whose name is no
/// variable's for a command, and run, redirect or expand what a value holds
/// beyond letters, digits, `.`, `-`, `_`, `/` and `:`: a `;`, `&` or `|`, a
/// `<` or `>`, a parenthesis, a `$`, a backtick, a quote, a backslash, a
/// `~` at the start or after a colon, a blank. The tool refuses a line
/// whose name is of other than capitals, digits and underscores, or begins
/// with a digit, and one whose value is empty, begins with a `-` (an option
/// to the programs the script hands it to) or holds anything but those
/// characters, as it refuses a line of no pin, a pin given twice and a file
/// without a pin it names. Comments, blank lines and CR LF line endings are
/// read past.
#[test]
fn only_plain_pins_are_read() {
    let fine = "# a comment\r\n\r\nMSDOS_REPOSITORY=r\r\nMSDOS_BRANCH=b\nMSDOS_COMMIT=c\n\
                DOOM8088_REPOSITORY=d\nDOOM8088_TAG=t\nDOOM8088_COMMIT=e\nDOOM8088_ZIP_URL=z\n";
    let pins = Pins::parse(fine).unwrap();
    assert_eq!(pins.msdos_repository, "r");
    assert_eq!(pins.all.len(), 7);

    // The pins of `fine` with `line` after them, its tenth line.
    let with = |line: &str| Pins::parse(String::leak(format!("{fine}{line}\n")));
    let plain = with("_A_1=https://a.example/b_c-d.e/0:1").unwrap();
    assert_eq!(pins.all.len() + 1, plain.all.len());
    for line in [
        "A=b c", "A=\"b\"", "A='b'", "A=$HOME", "A=`b`", "A=b\\", "A=b;c", "A=b&c", "A=b&&c",
        "A=b|c", "A=<b", "A=b>c", "A=(b)", "A=~", "A=~/b", "A=b:~/c", "A=b*", "A=b?c", "A=b=c",
        "A=b\rc", "A=", "A=-b", "A=--b", "A/B=c", "A-B=c", "8A=b", "a=b", "=b",
    ] {
        assert_eq!(
            with(line).unwrap_err(),
            format!("line 10 is not plain: {line}"),
            "{line:?}"
        );
    }
    assert_eq!(with("A").unwrap_err(), "line 10 is no NAME=value: A");
    assert_eq!(
        with("MSDOS_BRANCH=c").unwrap_err(),
        "MSDOS_BRANCH is pinned twice"
    );
    assert_eq!(
        Pins::parse("A=b\n").unwrap_err(),
        "no MSDOS_REPOSITORY is pinned"
    );
}
