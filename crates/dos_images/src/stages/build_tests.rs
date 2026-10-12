//! The account of a build, told its markers by hand, at instructions and
//! guest times the test states.

use super::*;

fn modules() -> Vec<String> {
    vec!["MESSAGES".to_string(), "CMD\\FIND".to_string()]
}

/// The guest at `instructions` and `seconds` of its time.
fn at(instructions: u64, seconds: u64) -> Mark {
    Mark {
        instructions,
        time: Duration::from_secs(seconds),
        instant: Instant::now(),
    }
}

/// Tells `progress` of the marker of `line`.
fn tell(progress: &mut Progress<'_>, line: &str, now: Mark) -> Result<Option<Build>, Error> {
    let marker = Marker::parse(line).expect("a marker's line");
    progress.told(marker, line, now)
}

/// The markers of a build in BUILD.BAT's order make its account: when
/// FreeDOS started it, each module's instructions from its marker to the
/// next, the copy's last, and the build's from its start to its end. The
/// modules built are named without the copy.
#[test]
fn the_markers_in_order_make_the_account_of_the_build() {
    let modules = modules();
    let mut progress = Progress::new(&modules, at(0, 0));
    for (line, now) in [
        (" BUILD START", at(4_000_000, 1)),
        (" MODULE MESSAGES", at(5_000_000, 2)),
        (" MODULE CMD\\FIND", at(20_000_000, 3)),
        (" BUILD COPY", at(73_000_000, 9)),
    ] {
        assert_eq!(
            tell(&mut progress, line, now).unwrap().map(|_| ()),
            None,
            "{line}"
        );
    }
    let build = tell(&mut progress, " BUILD DONE", at(93_000_000, 11))
        .unwrap()
        .expect("the build is done");
    assert_eq!(build.host_up, 4_000_000);
    assert_eq!(build.instructions, 89_000_000);
    let runs: Vec<(&str, u64)> = build
        .modules
        .iter()
        .map(|run| (run.name.as_str(), run.instructions))
        .collect();
    assert_eq!(
        runs,
        [
            ("MESSAGES", 15_000_000),
            ("CMD\\FIND", 53_000_000),
            (COPYING, 20_000_000)
        ]
    );
    assert_eq!(build.module_names(), ["MESSAGES", "CMD\\FIND"]);
}

/// A marker BUILD.BAT does not send then stops the build, whatever it is:
/// a module out of the list's order, a copy before every module was built,
/// an end before the copy, a second start, a failure of another module.
#[test]
fn a_marker_out_of_turn_stops_the_build() {
    let modules = modules();
    let started = |lines: &[&str]| {
        let mut progress = Progress::new(&modules, at(0, 0));
        for line in lines {
            tell(&mut progress, line, at(1, 1)).unwrap();
        }
        progress
    };
    for (before, line) in [
        (vec![" BUILD START"], " MODULE CMD\\FIND"),
        (vec![" BUILD START", " MODULE MESSAGES"], " BUILD COPY"),
        (vec![" BUILD START", " MODULE MESSAGES"], " BUILD DONE"),
        (vec![" BUILD START"], " BUILD START"),
        (
            vec![" BUILD START", " MODULE MESSAGES"],
            " MODULE-FAILED CMD\\FIND",
        ),
        (vec![], " MODULE MESSAGES"),
        (vec![], " CHECKS DONE"),
    ] {
        let mut progress = started(&before);
        assert_eq!(
            tell(&mut progress, line, at(2, 2)).unwrap_err(),
            Error::OutOfTurn(line.trim().to_string()),
            "{before:?} {line}"
        );
    }
}

/// A module that NMAKE failed in stops the build, naming the module, which
/// the account still holds as the one being built.
#[test]
fn a_module_nmake_failed_in_stops_the_build() {
    let modules = modules();
    let mut progress = Progress::new(&modules, at(0, 0));
    tell(&mut progress, " BUILD START", at(1, 1)).unwrap();
    tell(&mut progress, " MODULE MESSAGES", at(2, 1)).unwrap();
    assert_eq!(
        tell(&mut progress, " MODULE-FAILED MESSAGES", at(3, 2)).unwrap_err(),
        Error::ModuleFailed("MESSAGES".to_string())
    );
    assert_eq!(progress.module(), Some("MESSAGES"));
}

/// Each budget of guest time is kept: the start's from power-on, a
/// module's or the copy's from its own start, and the build's from the
/// start of BUILD.BAT.
#[test]
fn each_budget_of_guest_time_is_kept() {
    let seconds = Duration::from_secs;
    let modules = modules();
    let mut progress = Progress::new(&modules, at(0, 0));
    assert_eq!(progress.spent(seconds(9)), None);
    assert_eq!(progress.spent(seconds(10)), Some(Error::NoStart));

    tell(&mut progress, " BUILD START", at(1, 5)).unwrap();
    assert_eq!(progress.spent(seconds(300)), None, "no module runs yet");
    tell(&mut progress, " MODULE MESSAGES", at(2, 100)).unwrap();
    assert_eq!(progress.spent(seconds(499)), None);
    assert_eq!(
        progress.spent(seconds(500)),
        Some(Error::ModuleTooLong("MESSAGES".to_string()))
    );
    tell(&mut progress, " MODULE CMD\\FIND", at(3, 2_800)).unwrap();
    assert_eq!(progress.spent(seconds(3_004)), None);
    assert_eq!(progress.spent(seconds(3_005)), Some(Error::TooLong));

    let mut progress = Progress::new(&modules, at(0, 0));
    for line in [" BUILD START", " MODULE MESSAGES", " MODULE CMD\\FIND"] {
        tell(&mut progress, line, at(1, 1)).unwrap();
    }
    tell(&mut progress, " BUILD COPY", at(2, 10)).unwrap();
    assert_eq!(
        progress.spent(seconds(410)),
        Some(Error::ModuleTooLong(COPYING.to_string()))
    );
    assert_eq!(progress.module(), None, "the copy is no module");
}
