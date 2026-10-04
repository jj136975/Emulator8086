//! The host's wall clock, as the machine's FAT code asks for it
//! (`emu8086_core::io::disk::WallClock`). The core reads no clock: a disk
//! formatted or a file copied from the host gets its time from here.
//!
//! The invariant: files are stamped with the host's local time. It comes
//! from fatfs's own time provider, which reads it through chrono and is
//! there only in a fatfs built with its `chrono` feature. This crate's
//! manifest turns that feature on; without it every file would be stamped
//! 1980-01-01 00:00.

use std::time::{SystemTime, UNIX_EPOCH};

use emu8086_core::io::disk::WallClock;

/// The clock of the host this process runs on.
pub struct HostClock;

impl WallClock for HostClock {
    fn fat_options(&self) -> fatfs::FsOptions {
        // The options fatfs makes itself carry its own time provider.
        fatfs::FsOptions::new()
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
