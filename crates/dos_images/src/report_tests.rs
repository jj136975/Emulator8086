//! The report, of a run told by hand.

use super::*;
use crate::stages::build::ModuleRun;

fn report() -> Report {
    Report {
        pins: Pins::compiled_in(),
        build: Build {
            host_up: 3_180_722,
            modules: vec![
                ModuleRun {
                    name: "MESSAGES".to_string(),
                    instructions: 12_300_000,
                    seconds: 0.11,
                },
                ModuleRun {
                    name: "(CPY.BAT)".to_string(),
                    instructions: 4_000_000,
                    seconds: 0.04,
                },
            ],
            instructions: 16_300_000,
            seconds: 0.15,
        },
        built: Built {
            files: vec![("IO.SYS".to_string(), vec![0; 33_321])],
            boot_sector: [0; 512],
            master_boot_record: [0; 512],
            log_bytes: 184_981,
            warnings: 60,
        },
        floppy: Floppy {
            bytes: vec![0; 1_474_560],
            files: 62,
            free: 434_176,
            prompt_at: 1_000_000,
            chkdsk:
                "Volume Serial Number is 1988-1006\r\n\r\n   1457664 bytes total disk space\r\n"
                    .to_string(),
        },
        doom_disk: DoomDisk {
            bytes: vec![0; 2_580_480],
            free: 628_736,
            mode_13h_at: 3_000_000,
            title_at: 23_000_000,
            demo_at: 333_000_000,
            chkdsk: "   2527232 bytes total disk space\r\n".to_string(),
        },
        seconds: 150.0,
    }
}

/// A number is written with its digits in groups of three.
#[test]
fn numbers_are_grouped_by_thousands() {
    assert_eq!(grouped(0), "0");
    assert_eq!(grouped(999), "999");
    assert_eq!(grouped(1_000), "1,000");
    assert_eq!(grouped(11_373_000_000), "11,373,000,000");
}

/// The report names every pin, every module with its instructions, and
/// both disks with how they booted and what CHKDSK said of them.
#[test]
fn the_report_tells_the_inputs_the_build_and_the_disks() {
    let report = report();
    let text = report.to_string();
    for (name, value) in &report.pins.all {
        assert!(text.contains(&format!("{name:<24} {value}")), "{name}");
    }
    assert!(text.contains("after 3,180,722 instructions. It built 1 modules"));
    assert!(text.contains("  MESSAGES              12,300,000      0.11"));
    assert!(text.contains("  (CPY.BAT)              4,000,000      0.04"));
    assert!(text.contains("of 184,981 bytes"));
    assert!(text.contains("disks/msdos400.img: 1,474,560 bytes, 62 files, 434,176 bytes free"));
    assert!(text.contains(
        "\n    Volume Serial Number is 1988-1006\n       1457664 bytes total disk space\n"
    ));
    assert!(text.contains("disks/doom8088.img: 2,580,480 bytes, 628,736 bytes free"));
    assert!(text.contains("starts its demo after 333,000,000"));
    assert!(text.ends_with("The tool took 150.0 s of the host's.\n"));
}
