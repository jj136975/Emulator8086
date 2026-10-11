//! The times the tool stamps what it writes onto a FAT volume with, and the
//! serials of the volumes it formats.
//!
//! The invariant: nothing the tool writes onto a disk takes the time of the
//! host. fatfs stamps every file and directory it creates with what the
//! time provider of the mount says (`FsOptions::time_provider`), so every
//! volume the tool writes to is mounted with one of these stamps (`fat`),
//! and the same inputs make the same disks, byte for byte.

use fatfs::{Date, DateTime, FsOptions, Time, TimeProvider};

/// A time that stands still, for fatfs to stamp files with.
#[derive(Debug)]
pub(crate) struct Stamp(DateTime);

impl TimeProvider for Stamp {
    fn get_current_date(&self) -> Date {
        self.0.date
    }

    fn get_current_date_time(&self) -> DateTime {
        self.0
    }
}

impl Stamp {
    /// The options a volume is mounted with to be written at this time.
    pub(crate) fn options(&'static self) -> FsOptions {
        FsOptions::new().time_provider(self)
    }
}

const fn at(year: u16, month: u16, day: u16) -> Stamp {
    Stamp(DateTime {
        date: Date { year, month, day },
        time: Time {
            hour: 0,
            min: 0,
            sec: 0,
            millis: 0,
        },
    })
}

/// The day of the MS-DOS 4.00 release whose files the build makes again,
/// 1988-10-06: the sources on the build disk, and the files of MS-DOS on
/// the disks the tool makes. The build's own outputs are newer than the
/// sources, as on a fresh tree in 1988: the guest's clock starts the day
/// after (`build_disk`).
pub(crate) static MSDOS: Stamp = at(1988, 10, 6);

/// The day of Doom8088's release v20260304, 2026-03-04: its files on the
/// hard disk.
pub(crate) static DOOM8088: Stamp = at(2026, 3, 4);

/// The serials of the volumes the tool formats, as DOS shows them: the
/// floppy is 1988-1006, the hard disk 1988-1007 and the build disk
/// 1988-1005.
pub(crate) const FLOPPY_SERIAL: u32 = 0x1988_1006;
pub(crate) const HARD_DISK_SERIAL: u32 = 0x1988_1007;
pub(crate) const BUILD_DISK_SERIAL: u32 = 0x1988_1005;
