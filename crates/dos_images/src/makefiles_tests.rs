//! The modules, BUILD.BAT and the files of CPY.BAT, from makefiles and batch
//! files written as the source writes them (CR LF, tabs or blanks before a
//! command, the paths of `cd` in lower case).

use super::*;
use std::collections::HashMap;

/// A source of makefiles held in memory, by the directory each is in.
fn source(makefiles: &[(&str, &str)]) -> impl FnMut(&str) -> io::Result<String> {
    let makefiles: HashMap<String, String> = makefiles
        .iter()
        .map(|(directory, text)| (directory.to_string(), text.replace('\n', "\r\n")))
        .collect();
    move |directory| {
        makefiles
            .get(directory)
            .cloned()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "no MAKEFILE"))
    }
}

const ROOT: &str = "#**** Root level Makefile ****\n\nmake\t=nmake\n\nall:\n\
                    \tcd messages\n\t$(make)\n\tcd ..\\cmd\n\t$(make)\n        cd ..\\memm\n        $(make)\n\tcd ..\n\n\n";
const CMD: &str =
    "make\t=nmake\n\nall:\n\tcd command\n\t$(make)\n\tcd ..\\find\n\t$(make)\n\tcd ..\n";
const MEMM: &str = "all:\n\tcd emm\n\t$(make)\n\tcd ..\\memm\n\t$(make)\n\tcd ..\n";
const LEAF: &str =
    "include ..\\..\\inc\\defines.mak\n\nall: find.exe\n\nfind.exe: find.obj\n\tlink find;\n";

/// A makefile only dispatches when every command of its `all` rule is a
/// `cd` or a `$(make)`; its commands end where the next rule begins.
#[test]
fn a_makefile_dispatches_when_its_all_rule_only_changes_directory_and_recurses() {
    assert_eq!(
        dispatching(&CMD.replace('\n', "\r\n")).unwrap(),
        ["cd command", "$(make)", "cd ..\\find", "$(make)", "cd .."]
    );
    assert_eq!(
        dispatching(LEAF),
        None,
        "all: has a target, and a rule builds"
    );
    assert_eq!(dispatching("all:\n\tcd x\n\tmasm x;\n"), None);
    assert_eq!(dispatching("all:\n\n"), None, "nothing to dispatch to");
    assert_eq!(
        dispatching("all:\n\tcd x\n\t$(make)\n\nclean:\n\tdel *.obj\n").unwrap(),
        ["cd x", "$(make)"]
    );
}

/// The modules are the directories the dispatching makefiles reach, in
/// their order, each as a DOS path in capitals below the root.
#[test]
fn the_modules_are_the_directories_the_dispatching_makefiles_reach_in_order() {
    let mut makefiles = source(&[
        ("", ROOT),
        ("MESSAGES", LEAF),
        ("CMD", CMD),
        ("CMD\\COMMAND", LEAF),
        ("CMD\\FIND", LEAF),
        ("MEMM", MEMM),
        ("MEMM\\EMM", LEAF),
        ("MEMM\\MEMM", LEAF),
    ]);
    assert_eq!(
        modules(&mut makefiles).unwrap(),
        [
            "MESSAGES",
            "CMD\\COMMAND",
            "CMD\\FIND",
            "MEMM\\EMM",
            "MEMM\\MEMM"
        ]
    );
}

/// A makefile that cannot be read, a `cd` above the root, a recursion into
/// a makefile's own directory and a module reached twice each stop the
/// reading, naming where.
#[test]
fn the_makefiles_are_refused_when_they_lead_nowhere_or_twice() {
    let mut missing = source(&[("", ROOT), ("MESSAGES", LEAF)]);
    assert!(matches!(
        modules(&mut missing),
        Err(MakefileError::Unreadable { directory, .. }) if directory == "CMD"
    ));

    let mut escaping = source(&[("", "all:\n\tcd ..\\..\\x\n\t$(make)\n")]);
    assert!(matches!(
        modules(&mut escaping),
        Err(MakefileError::Escapes { directory, .. }) if directory.is_empty()
    ));

    let mut looping = source(&[("", "all:\n\t$(make)\n")]);
    assert!(matches!(
        modules(&mut looping),
        Err(MakefileError::Loops { .. })
    ));

    let mut twice = source(&[
        ("", "all:\n\tcd x\n\t$(make)\n\tcd ..\\x\n\t$(make)\n"),
        ("X", LEAF),
    ]);
    assert!(matches!(
        modules(&mut twice),
        Err(MakefileError::Twice { module }) if module == "X"
    ));
}

/// BUILD.BAT sets the environment up, then for each module changes into
/// it, tells the host, marks the log, runs NMAKE into the log and tells the
/// host when it failed, and at the end copies what was built into C:\BIN.
#[test]
fn build_bat_runs_nmake_in_each_module_and_tells_the_host() {
    let bat = build_bat(&["MESSAGES".to_string(), "CMD\\FIND".to_string()]);
    assert_eq!(
        bat,
        "@ECHO OFF\n\
         C:\\E9 BUILD START\n\
         CD \\SRC\n\
         CALL SETENV.BAT\n\
         CD \\SRC\\MESSAGES\n\
         C:\\E9 MODULE MESSAGES\n\
         ECHO ===== MESSAGES >> C:\\BUILD.LOG\n\
         NMAKE -X - >> C:\\BUILD.LOG\n\
         IF ERRORLEVEL 1 C:\\E9 MODULE-FAILED MESSAGES\n\
         CD \\SRC\\CMD\\FIND\n\
         C:\\E9 MODULE CMD\\FIND\n\
         ECHO ===== CMD\\FIND >> C:\\BUILD.LOG\n\
         NMAKE -X - >> C:\\BUILD.LOG\n\
         IF ERRORLEVEL 1 C:\\E9 MODULE-FAILED CMD\\FIND\n\
         CD \\SRC\n\
         C:\\E9 BUILD COPY\n\
         MD C:\\BIN\n\
         CALL CPY.BAT C:\\BIN\n\
         C:\\E9 BUILD DONE\n"
    );
}

/// The files of CPY.BAT are the names of what each `copy` line copies, in
/// capitals, in its order; other lines name none.
#[test]
fn the_files_of_cpy_bat_are_what_it_copies() {
    let cpy = "\r\ncopy .\\bios\\io.sys %1\r\ncopy .\\cmd\\fdisk\\fdisk.exe %1\r\n\
               rem not a copy\r\nCOPY .\\dev\\printer\\4201\\4201.cpi %1\r\n\r\n";
    assert_eq!(copied_files(cpy), ["IO.SYS", "FDISK.EXE", "4201.CPI"]);
}
