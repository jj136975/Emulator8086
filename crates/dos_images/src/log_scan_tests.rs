//! BUILD.LOG read as the build writes it: CR LF lines, a progress line
//! ended by a CR alone, and the lines of the real build's tools.

use super::*;

const LOG: &str = "===== MESSAGES \r\n\
                   \tbuildmsg usa-ms.msg\r\n\
                   ===== MAPPER \r\n\
                   LINK : warning L4021: no stack segment\r\n\
                   Pass 1\rPass 2\r\n\
                   ***** Possible stack size error in DOS_Open ***** \r\n\
                   ===== CMD\\IFSFUNC \r\n\
                   \tmasm -Mx -t ifsinit.asm,ifsinit.obj;\r\n\
                   ..\\..\\inc\\PARSE.ASM(2203): Out of memory\r\n\
                   NMAKE : fatal error U1077: 'MASM' : return code 248\r\n\
                   ===== CMD\\IFSFUNCX \r\n\
                   done\r\n";

/// What MASM, C, LINK and NMAKE report as an error, and a tool out of
/// memory, is an error; their warnings, a source's own notes and a file
/// named for errors are none.
#[test]
fn errors_are_told_from_warnings_and_notes() {
    for error in [
        "IFSINIT.ASM(12): error A2009: Symbol not defined: FOO",
        "fc.c(3) : error C2065: 'y' : undefined",
        "foo.c(1) : fatal error C1001: Internal Compiler Error",
        "LINK : error L2029: '_main' : unresolved external",
        "LINK : fatal error L1093: object not found",
        "NMAKE : error U1034: syntax error : separator missing",
        "NMAKE : fatal error U1077: 'MASM' : return code 248",
        "..\\..\\inc\\PARSE.ASM(2203): Out of memory",
    ] {
        assert!(is_error(error), "{error}");
    }
    for fine in [
        "LINK : warning L4021: no stack segment",
        "memm.asm(10): warning A4100: Impure memory reference",
        "LIB : warning U4155: services : module not in library",
        "***** Possible stack size error in DOS_Open *****",
        "SHARE_ERROR... pass 1",
        "\tmasm -Mx -t  -I. error.asm,error.obj;",
        "Object Modules [.OBJ]: FC.OBJ ERROR.OBJ",
        "error.c",
        "error A2",
    ] {
        assert!(!is_error(fine), "{fine}");
    }
}

/// The warnings of the tools are counted, each line once; a source's own
/// warning, printed as a note, is none of theirs.
#[test]
fn the_warnings_of_the_tools_are_counted() {
    let log = "LINK : warning L4021: no stack segment\r\n\
               memm.asm(10): warning A4100: Impure memory reference\r\n\
               LIB : warning U4155: services : module not in library\r\n\
               WARNING!!! Debug fields are being included!!!\r\n\
               warning A4\r\n";
    assert_eq!(warnings(log), 3);
    assert_eq!(warnings(LOG), 1);
}

/// Each error of the log is found with the module whose part holds it.
#[test]
fn each_error_is_found_in_its_module() {
    assert_eq!(
        errors(LOG),
        [
            Finding {
                module: "CMD\\IFSFUNC".to_string(),
                line: "..\\..\\inc\\PARSE.ASM(2203): Out of memory".to_string(),
            },
            Finding {
                module: "CMD\\IFSFUNC".to_string(),
                line: "NMAKE : fatal error U1077: 'MASM' : return code 248".to_string(),
            },
        ]
    );
    assert_eq!(
        errors("NMAKE : fatal error U1052\r\n")[0].module,
        "(before the first module)"
    );
}

/// A module's part is the lines between its line and the next module's,
/// found by the module's whole name; a module the log has no part of has
/// none.
#[test]
fn a_modules_part_is_the_lines_below_its_line() {
    assert_eq!(
        section(LOG, "CMD\\IFSFUNC").unwrap(),
        "\tmasm -Mx -t ifsinit.asm,ifsinit.obj;\n\
         ..\\..\\inc\\PARSE.ASM(2203): Out of memory\n\
         NMAKE : fatal error U1077: 'MASM' : return code 248"
    );
    assert_eq!(
        section(LOG, "MAPPER").unwrap(),
        "LINK : warning L4021: no stack segment\nPass 1\nPass 2\n\
         ***** Possible stack size error in DOS_Open *****"
    );
    assert_eq!(section(LOG, "CMD\\IFSFUNCX").unwrap(), "done");
    assert_eq!(section(LOG, "CMD\\IFS"), None);
    assert_eq!(section("", "MESSAGES"), None);
}

/// The line BUILD.BAT has ECHO write into the log before a module, all
/// that ECHO writes of it up to the redirection, begins the module's part.
#[test]
fn the_line_build_bat_echoes_begins_the_modules_part() {
    let bat = crate::makefiles::build_bat(&["CMD\\FIND".to_string()]);
    let echoed = bat
        .lines()
        .find_map(|line| line.strip_prefix("ECHO ")?.strip_suffix(">> C:\\BUILD.LOG"))
        .expect("BUILD.BAT echoes a line into the log");
    assert_eq!(section_start(echoed), Some("CMD\\FIND"));
}

/// No other line begins a part: a tool's own line of `=` signs, or one
/// that begins as BUILD.BAT's line does but is not one, is a line of the
/// part it is in, and an error below it is that module's.
#[test]
fn no_other_line_begins_a_part() {
    let log = "===== CMD\\FIND \r\n\
               ==========\r\n\
               ===== Pass 2 =====\r\n\
               ===== ===== \r\n\
               ===== cmd\\fc \r\n\
               =====CMD\\FC \r\n\
               ===== CMD\\FC\r\n\
               ===== CMD\\FC  \r\n\
               \x20===== CMD\\FC \r\n\
               fc.c(3) : error C2065: 'y' : undefined\r\n";
    assert_eq!(
        errors(log),
        [Finding {
            module: "CMD\\FIND".to_string(),
            line: "fc.c(3) : error C2065: 'y' : undefined".to_string(),
        }]
    );
    assert_eq!(section(log, "CMD\\FIND").unwrap().lines().count(), 9);
    assert_eq!(section(log, "CMD\\FC"), None);
    assert_eq!(section(log, "====="), None);
}
