//! CHKDSK's reports, as MS-DOS 4.00's CHKDSK writes them of the two disks.

use super::*;

const FLOPPY: &str = "\r\nVolume Serial Number is 1988-1006\r\n\r\n\
                      \x20  1457664 bytes total disk space\r\n\
                      \x20    71168 bytes in 2 hidden files\r\n\
                      \x20   952320 bytes in 60 user files\r\n\
                      \x20   434176 bytes available on disk\r\n\r\n\
                      \x20      512 bytes in each allocation unit\r\n\
                      \x20     2847 total allocation units on disk\r\n\
                      \x20      848 available allocation units on disk\r\n\r\n\
                      \x20   655360 total bytes memory\r\n\
                      \x20   592960 bytes free\r\n";

const HARD_DISK: &str = "Volume Serial Number is 1988-1007\r\n\r\n\
                         \x20  2527232 bytes total disk space\r\n\
                         \x20    73728 bytes in 2 hidden files\r\n\
                         \x20     2048 bytes in 1 directories\r\n\
                         \x20  1822720 bytes in 13 user files\r\n\
                         \x20   628736 bytes available on disk\r\n\r\n\
                         \x20     2048 bytes in each allocation unit\r\n\
                         \x20     1234 total allocation units on disk\r\n\
                         \x20      307 available allocation units on disk\r\n\r\n\
                         \x20   655360 total bytes memory\r\n\
                         \x20   595168 bytes free\r\n";

/// The report of a clean volume is its summary alone, a label's line
/// among it or not.
#[test]
fn the_summary_of_a_clean_volume_is_clean() {
    assert_eq!(unclean_lines(FLOPPY), Vec::<String>::new());
    assert_eq!(unclean_lines(HARD_DISK), Vec::<String>::new());
    let labelled = format!("Volume MSDOS400 created 10-06-1988 12:00a\r\n{FLOPPY}");
    assert_eq!(unclean_lines(&labelled), Vec::<String>::new());
}

/// Whatever CHKDSK finds wrong is a line that is no summary's, and a report
/// without the size of the volume is none.
#[test]
fn what_chkdsk_finds_wrong_is_unclean() {
    for (error, line) in [
        (
            "Errors found, F parameter not specified\r\n",
            "Errors found, F parameter not specified",
        ),
        (
            "   3 lost allocation units found in 1 chains.\r\n",
            "3 lost allocation units found in 1 chains.",
        ),
        (
            "A:\\IO.SYS\r\n   Allocation error, size adjusted\r\n",
            "A:\\IO.SYS",
        ),
        (
            "    4096 bytes in bad sectors\r\n",
            "4096 bytes in bad sectors",
        ),
        (
            "   12 bytes in 2 hidden things\r\n",
            "12 bytes in 2 hidden things",
        ),
    ] {
        let report = format!("{FLOPPY}{error}");
        assert_eq!(
            unclean_lines(&report).first().map(String::as_str),
            Some(line),
            "{error}"
        );
    }
    assert_eq!(
        unclean_lines(""),
        ["(no line of \"bytes total disk space\")"]
    );
}
