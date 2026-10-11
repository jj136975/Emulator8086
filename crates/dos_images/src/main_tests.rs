//! The command line of the tool.

use super::*;

fn args(text: &str) -> Vec<String> {
    text.split_whitespace().map(str::to_string).collect()
}

/// The four directories are named by their options, in any order.
#[test]
fn the_four_directories_are_named_in_any_order() {
    let asked = asked(args("--out o --doom8088 d --freedos f --msdos m")).unwrap();
    assert_eq!(
        asked,
        Asked::Run {
            inputs: Inputs {
                freedos: PathBuf::from("f"),
                msdos: PathBuf::from("m"),
                doom8088: PathBuf::from("d"),
            },
            out: PathBuf::from("o"),
        }
    );
    assert_eq!(asked_help("--freedos f --help"), Asked::Help);
}

fn asked_help(text: &str) -> Asked {
    asked(args(text)).unwrap()
}

/// A command line that leaves a directory out, names one twice, gives an
/// option no directory or names no option of the tool is refused, with
/// what is wrong with it.
#[test]
fn a_command_line_that_is_not_complete_or_has_more_is_refused() {
    for (text, why) in [
        ("--freedos f --msdos m --doom8088 d", "--out is not given"),
        (
            "--freedos f --freedos g --msdos m --doom8088 d --out o",
            "--freedos is given twice",
        ),
        (
            "--freedos f --msdos m --doom8088 d --out",
            "--out names no directory",
        ),
        ("--freedos f --hd x", "--hd is no option of the tool"),
    ] {
        assert_eq!(asked(args(text)), Err(why.to_string()), "{text}");
    }
}
