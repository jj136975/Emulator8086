//! The notices, against the pins they name and DOS's screen.

use super::*;

fn notices() -> Vec<String> {
    let pins = Pins::compiled_in();
    vec![
        readme(&pins, Disk::Floppy),
        readme(&pins, Disk::Doom8088),
        doom8088_source(&pins),
    ]
}

/// A notice is ASCII text whose every line fits DOS's screen: shorter than
/// its 80 columns, as a line of 80 wraps into an empty one.
#[test]
fn every_line_of_a_notice_fits_the_screen() {
    for notice in notices() {
        assert!(notice.is_ascii());
        assert!(notice.ends_with('\n'));
        for line in notice.lines() {
            assert!(line.len() < 80, "{} columns: {line}", line.len());
            assert_eq!(line, line.trim_end(), "no blank at the end");
        }
    }
}

/// README.TXT names the source by what pins it: the repository, its
/// branch and the commit; it points at the license beside it, and says
/// what the disk holds: the floppy all but the installer, the hard disk
/// the system and Doom8088, whose notice it names.
#[test]
fn the_readme_names_the_pinned_source_and_what_the_disk_holds() {
    let pins = Pins::compiled_in();
    for disk in [Disk::Floppy, Disk::Doom8088] {
        let text = readme(&pins, disk);
        for named in [
            pins.msdos_repository,
            pins.msdos_branch,
            pins.msdos_commit,
            "LICENSE.TXT",
            "MIT License",
        ] {
            assert!(text.contains(named), "{disk:?}: {named}");
        }
    }
    assert!(readme(&pins, Disk::Floppy).contains("SELECT"));
    assert!(readme(&pins, Disk::Doom8088).contains("\\DOOM\\SOURCE.TXT"));
}

/// SOURCE.TXT names Doom8088's source by its tag and commit, its license
/// beside it, the release its files come from, and DOOM1.WAD as id
/// Software's shareware data, which the GPL does not cover.
#[test]
fn the_source_of_doom8088_is_named_by_its_tag_and_commit() {
    let pins = Pins::compiled_in();
    let text = doom8088_source(&pins);
    for named in [
        pins.doom8088_repository,
        pins.doom8088_tag,
        pins.doom8088_commit,
        "COPYING",
        "DOOM8088-VGA-v20260304.zip",
        "id Software's shareware DOOM data",
        "not covered by the GPL",
    ] {
        assert!(text.contains(named), "{named}");
    }
}
