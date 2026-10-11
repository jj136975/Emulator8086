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

/// A shell takes a line with a blank, a quote or an expansion in it for
/// something else than the file means, so the tool refuses it, as it
/// refuses a line of no pin and a pin given twice. Comments, blank lines
/// and CR LF line endings are read past.
#[test]
fn only_plain_pins_are_read() {
    let fine = "# a comment\r\n\r\nMSDOS_REPOSITORY=r\r\nMSDOS_BRANCH=b\nMSDOS_COMMIT=c\n\
                DOOM8088_REPOSITORY=d\nDOOM8088_TAG=t\nDOOM8088_COMMIT=e\nDOOM8088_ZIP_URL=z\n";
    let pins = Pins::parse(fine).unwrap();
    assert_eq!(pins.msdos_repository, "r");
    assert_eq!(pins.all.len(), 7);

    for wrong in [
        "A=b c\n",
        "A=\"b\"\n",
        "A=$HOME\n",
        "A=b\\\n",
        "A\n",
        "=b\n",
        "A=b\nA=c\n",
    ] {
        assert!(Pins::parse(wrong).is_err(), "{wrong:?}");
    }
    assert!(
        Pins::parse("A=b\n")
            .unwrap_err()
            .contains("MSDOS_REPOSITORY")
    );
}
