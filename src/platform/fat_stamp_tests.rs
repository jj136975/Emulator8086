//! The stamp of a date, field by field, and on a file of a real volume.

use std::io::Cursor;

use super::*;

fn local(year: u32, month: u32, day: u32, hour: u32, minute: u32, second: u32) -> Local {
    Local {
        year,
        month,
        day,
        hour,
        minute,
        second,
        millisecond: 0,
    }
}

fn fields(stamp: fatfs::DateTime) -> [u16; 6] {
    [
        stamp.date.year,
        stamp.date.month,
        stamp.date.day,
        stamp.time.hour,
        stamp.time.min,
        stamp.time.sec,
    ]
}

/// The fields go over as they are, but for the month, which `Date` counts
/// from 0 and FAT from 1.
#[test]
fn a_date_becomes_its_stamp() {
    let stamp = date_time(Local {
        millisecond: 420,
        ..local(2026, 9, 4, 23, 5, 59)
    });
    assert_eq!(fields(stamp), [2026, 10, 4, 23, 5, 59]);
    assert_eq!(stamp.time.millis, 420);

    let first = date_time(local(1980, 0, 1, 0, 0, 0));
    assert_eq!(fields(first), [1980, 1, 1, 0, 0, 0]);
    let last = date_time(local(2107, 11, 31, 23, 59, 59));
    assert_eq!(fields(last), [2107, 12, 31, 23, 59, 59]);
}

/// A clock outside the years FAT can hold is stamped with the nearest
/// moment it has, and a field out of its range is held to it.
#[test]
fn a_date_fat_cannot_hold_is_the_nearest_it_can() {
    let unix_epoch = date_time(local(1970, 0, 1, 12, 0, 0));
    assert_eq!(fields(unix_epoch), [1980, 1, 1, 0, 0, 0]);
    // What the getters of an invalid `Date` come to.
    let invalid = date_time(local(0, 0, 0, 0, 0, 0));
    assert_eq!(fields(invalid), [1980, 1, 1, 0, 0, 0]);

    let too_late = date_time(local(2108, 0, 1, 0, 0, 0));
    assert_eq!(fields(too_late), [2107, 12, 31, 23, 59, 59]);

    let out_of_range = date_time(local(2026, u32::MAX, 0, 24, 60, 60));
    assert_eq!(fields(out_of_range), [2026, 12, 1, 23, 59, 59]);
}

/// The stamp is what a file gets: a volume mounted with a clock that
/// reports it stamps a new file with it.
#[test]
fn the_stamp_is_what_a_file_gets() {
    #[derive(Debug)]
    struct Clock;
    impl fatfs::TimeProvider for Clock {
        fn get_current_date(&self) -> fatfs::Date {
            self.get_current_date_time().date
        }
        fn get_current_date_time(&self) -> fatfs::DateTime {
            // An even second: a FAT entry keeps the time it was last
            // written to the two seconds.
            date_time(local(2026, 9, 4, 13, 37, 42))
        }
    }

    let mut volume = Cursor::new(vec![0u8; 1_474_560]);
    fatfs::format_volume(&mut volume, fatfs::FormatVolumeOptions::new()).unwrap();
    let options = fatfs::FsOptions::new().time_provider(&Clock);
    let fs = fatfs::FileSystem::new(&mut volume, options).unwrap();
    fs.root_dir().create_file("NOW.TXT").unwrap();
    let entry = fs.root_dir().iter().next().unwrap().unwrap();
    assert_eq!(fields(entry.modified()), [2026, 10, 4, 13, 37, 42]);
}
