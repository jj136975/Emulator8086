//! The notices the disks carry beside what they hold: README.TXT, which
//! says what MS-DOS 4.00 on a disk is, where its source lives and under
//! which license (LICENSE.TXT beside it); and SOURCE.TXT beside Doom8088,
//! which names its source, its license (COPYING beside it) and what
//! DOOM1.WAD is. The lines of a notice end in LF; whoever writes one onto
//! a disk gives them DOS's line ends (`fat::dos_text`).
//!
//! The invariant: what a notice says of an input is what `pins.env` pins,
//! and every line fits the 80 columns of DOS's screen.

use crate::pins::Pins;

/// Where Emulator8086 lives, whose CI builds the disks.
const EMULATOR: &str = "https://github.com/jj136975/Emulator8086";
/// Where Microsoft published the source of MS-DOS.
const MICROSOFT: &str = "https://github.com/microsoft/MS-DOS";

/// Which of the site's disks a README.TXT is for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Disk {
    /// The MS-DOS 4.00 floppy, which holds what the build makes but the
    /// installer.
    Floppy,
    /// The hard disk that starts Doom8088, which holds MS-DOS 4.00's system
    /// and a few of its programs.
    Doom8088,
}

/// README.TXT of the root of a disk.
pub(crate) fn readme(pins: &Pins, disk: Disk) -> String {
    let mut text = format!(
        "MS-DOS 4.00

This disk holds MS-DOS 4.00, built from the source code that Microsoft and
IBM published under the MIT License (LICENSE.TXT on this disk), at
{MICROSOFT}. The source built is the community's
restoration of the published code, which mends the files that came out
damaged:

  {repository}
  branch {branch}, directory v4.0, commit
  {commit}

Emulator8086, a PC emulator, built it inside its own emulated 8086, under
FreeDOS 1.4, with the tools that come with the source, in the continuous
integration of its repository: {EMULATOR}
Of the 62 files the build makes, 61 are byte for byte those of the MS-DOS
4.00 release of 1988-10-06. The 62nd, XMAEM.SYS, is not on its disks.
",
        repository = pins.msdos_repository,
        branch = pins.msdos_branch,
        commit = pins.msdos_commit,
    );
    text += match disk {
        Disk::Floppy => {
            "
This disk holds all of them but SELECT, the installer, which is made for
the release's own disks.
"
        }
        Disk::Doom8088 => {
            "
This disk holds the system of MS-DOS, COMMAND.COM, MEM.EXE and CHKDSK.COM.
The directory \\DOOM holds Doom8088, which AUTOEXEC.BAT starts: it is free
software under the GNU General Public License (\\DOOM\\SOURCE.TXT).
"
        }
    };
    text
}

/// SOURCE.TXT, beside Doom8088 in \DOOM.
pub(crate) fn doom8088_source(pins: &Pins) -> String {
    let zip = pins
        .doom8088_zip_url
        .rsplit('/')
        .next()
        .unwrap_or(pins.doom8088_zip_url);
    format!(
        "Doom8088

D8M13H.EXE is Doom8088, Frenkel Smeijers's port of DOOM to the 8088 and the
8086, built for the VGA's Mode 13h. It is free software under the GNU
General Public License, version 2 (COPYING in this directory). Its source
code is at

  {repository}
  tag {tag}, commit {commit}

D8M13H.EXE, DOOM1.WAD and README.TXT are those of the release's zip,
{zip}:

  {repository}/releases/tag/{tag}

COPYING is the repository's LICENSE at the tag. README.TXT and COPYING
have DOS's line ends here.

DOOM1.WAD is id Software's shareware DOOM data, as Doom8088's release ships
it, prepared for Doom8088. It is id Software's, not covered by the GPL.
",
        repository = pins.doom8088_repository,
        tag = pins.doom8088_tag,
        commit = pins.doom8088_commit,
    )
}

#[cfg(test)]
#[path = "notices_tests.rs"]
mod tests;
