//! The host's wall clock, as the machine's FAT code asks for it
//! (`emu8086_core::io::disk::WallClock`). The core reads no clock: a disk
//! formatted or a file copied from the host gets its time from here.
//!
//! The invariant: files are stamped with the host's local time, and how a
//! host tells its local time is the host's business
//! (`platform::fat_options`). Mount options made here by hand would stamp
//! every file with whatever fatfs falls back on.

use emu8086_core::io::disk::WallClock;

use crate::platform::{self, SystemTime, UNIX_EPOCH};

/// The clock of the host this process runs on.
pub struct HostClock;

impl WallClock for HostClock {
    fn fat_options(&self) -> fatfs::FsOptions {
        platform::fat_options()
    }

    fn unix_nanos(&self) -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|since_epoch| since_epoch.as_nanos() as u64)
            .unwrap_or(0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    /// Files are stamped with the present, not with the 1980-01-01 that
    /// fatfs falls back on when it is built without `chrono`.
    #[test]
    fn host_clock_stamps_files_with_the_present() {
        let mut volume = Cursor::new(vec![0u8; 1_474_560]);
        fatfs::format_volume(&mut volume, fatfs::FormatVolumeOptions::new()).unwrap();
        let fs = fatfs::FileSystem::new(&mut volume, HostClock.fat_options()).unwrap();
        fs.root_dir().create_file("NOW.TXT").unwrap();
        let entry = fs.root_dir().iter().next().unwrap().unwrap();
        let stamp = entry.created();

        // The year by the other reading of the same clock. Local time and
        // UTC are at most a day apart, so the two years are at most one.
        const SECONDS_PER_YEAR: u64 = 31_556_952;
        let year = 1970 + HostClock.unix_nanos() / 1_000_000_000 / SECONDS_PER_YEAR;
        assert!(
            u64::from(stamp.date.year).abs_diff(year) <= 1,
            "stamped {stamp:?} in {year}"
        );
    }
}
