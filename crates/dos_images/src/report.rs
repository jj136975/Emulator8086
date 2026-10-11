//! Stage 7: the report, `report.txt` of the output directory. What the
//! inputs were (every pin), how the build went module by module, what it
//! made, and the two disks: how they booted, and what CHKDSK said of them.
//! The Pages workflow shows it as the summary of its run.
//!
//! The invariant: the report tells what this run did. Its instructions are
//! the same on every run; its seconds are the host's, and differ.

use crate::pins::Pins;
use crate::stages::{Build, Built, DoomDisk, FLOPPY_IMAGE, Floppy, HARD_DISK_IMAGE};
use std::fmt;

/// What the tool did.
pub(crate) struct Report {
    pub pins: Pins,
    pub build: Build,
    pub built: Built,
    pub floppy: Floppy,
    pub doom_disk: DoomDisk,
    /// The host's seconds for all of it.
    pub seconds: f64,
}

/// `number` with its digits in groups of three.
pub(crate) fn grouped(number: u64) -> String {
    let digits = number.to_string();
    let mut text = String::new();
    for (at, digit) in digits.chars().enumerate() {
        if at > 0 && (digits.len() - at).is_multiple_of(3) {
            text.push(',');
        }
        text.push(digit);
    }
    text
}

/// `text` with each line indented by four blanks.
fn indented(text: &str) -> String {
    text.lines()
        .map(str::trim_end)
        .filter(|line| !line.is_empty())
        .map(|line| format!("    {line}\n"))
        .collect()
}

impl fmt::Display for Report {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (build, built, floppy, doom) =
            (&self.build, &self.built, &self.floppy, &self.doom_disk);
        writeln!(f, "MS-DOS 4.00, built from its source in Emulator8086")?;
        writeln!(f)?;
        writeln!(f, "The inputs, as crates/dos_images/pins.env pins them:")?;
        for (name, value) in &self.pins.all {
            writeln!(f, "  {name:<24} {value}")?;
        }
        writeln!(f)?;
        writeln!(
            f,
            "The build: FreeDOS 1.4 started BUILD.BAT after {} instructions. It built {} \
             modules and copied what they made in {} instructions and {:.1} s of the host's, \
             {:.0} million instructions a second:",
            grouped(build.host_up),
            build.modules.len() - 1,
            grouped(build.instructions),
            build.seconds,
            build.instructions as f64 / build.seconds.max(1e-9) / 1e6
        )?;
        writeln!(
            f,
            "  {:<16} {:>15} {:>9}",
            "module", "instructions", "seconds"
        )?;
        for run in &build.modules {
            writeln!(
                f,
                "  {:<16} {:>15} {:>9.2}",
                run.name,
                grouped(run.instructions),
                run.seconds
            )?;
        }
        writeln!(f)?;
        writeln!(
            f,
            "What it made: the {} files of CPY.BAT, in bin/. BUILD.LOG, of {} bytes, tells of \
             no error, and of {} warnings of the tools.",
            built.files.len(),
            grouped(built.log_bytes as u64),
            built.warnings
        )?;
        writeln!(f)?;
        writeln!(
            f,
            "{FLOPPY_IMAGE}: {} bytes, {} files, {} bytes free. MS-DOS 4.00 shows its prompt \
             after {} instructions. CHKDSK finds it clean:",
            grouped(floppy.bytes.len() as u64),
            floppy.files,
            grouped(floppy.free),
            grouped(floppy.prompt_at)
        )?;
        write!(f, "{}", indented(&floppy.chkdsk))?;
        writeln!(f)?;
        writeln!(
            f,
            "{HARD_DISK_IMAGE}: {} bytes, {} bytes free. Doom8088 sets Mode 13h after {} \
             instructions, shows its title picture after {} and starts its demo after {}. \
             CHKDSK finds it clean:",
            grouped(doom.bytes.len() as u64),
            grouped(doom.free),
            grouped(doom.mode_13h_at),
            grouped(doom.title_at),
            grouped(doom.demo_at)
        )?;
        write!(f, "{}", indented(&doom.chkdsk))?;
        writeln!(f)?;
        writeln!(f, "The tool took {:.1} s of the host's.", self.seconds)
    }
}

#[cfg(test)]
#[path = "report_tests.rs"]
mod tests;
