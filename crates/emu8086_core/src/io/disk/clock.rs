//! The host's wall clock, as the host-side FAT code is handed it: the time
//! files are stamped with, and the count a new volume serial comes from.
//!
//! The invariant: nothing in this crate reads a clock. The machine has to
//! build where there is none to read (a browser), and its tests have to
//! give the same result on every run. So whoever formats a disk or copies a
//! file onto one passes the clock in: the app its own, a test `FixedClock`.

/// The host's wall clock. The app implements it; the FAT code asks it, and
/// nothing else, what time it is.
pub trait WallClock {
    /// The options a FAT volume is mounted with while it is written to.
    ///
    /// fatfs stamps every file it creates or changes with the local date
    /// and time its mount options' time provider reports, so the options
    /// are how a clock reaches it. An implementation sets a provider of its
    /// own (`FsOptions::time_provider`), or returns the default options of
    /// a fatfs built with its `chrono` feature, whose provider reads the
    /// host's local time: fatfs 0.3 hands that provider out in no other
    /// way.
    fn fat_options(&self) -> fatfs::FsOptions;

    /// The time as nanoseconds since the Unix epoch. A new volume's serial
    /// is derived from it, as DOS derives one from the time of the format.
    fn unix_nanos(&self) -> u64;
}

/// The clock of the tests. It stands still, at the date and time the files
/// of MS-DOS 6.22 carry.
#[cfg(test)]
#[derive(Debug)]
pub(crate) struct FixedClock;

#[cfg(test)]
impl FixedClock {
    /// 1994-05-31 06:22:00: what every file written by this clock is
    /// stamped with.
    pub(crate) const NOW: fatfs::DateTime = fatfs::DateTime {
        date: fatfs::Date {
            year: 1994,
            month: 5,
            day: 31,
        },
        time: fatfs::Time {
            hour: 6,
            min: 22,
            sec: 0,
            millis: 0,
        },
    };
}

#[cfg(test)]
impl fatfs::TimeProvider for FixedClock {
    fn get_current_date(&self) -> fatfs::Date {
        Self::NOW.date
    }

    fn get_current_date_time(&self) -> fatfs::DateTime {
        Self::NOW
    }
}

#[cfg(test)]
impl WallClock for FixedClock {
    fn fat_options(&self) -> fatfs::FsOptions {
        fatfs::FsOptions::new().time_provider(&FixedClock)
    }

    fn unix_nanos(&self) -> u64 {
        // `NOW`, taken as UTC.
        770_365_320_000_000_000
    }
}
