//! A local date and time, as a browser's `Date` tells it, as the stamp a
//! FAT volume puts on a file. It knows nothing of a browser: the fields are
//! handed in, which is what lets the tests run it on a desktop.
//!
//! The invariant: the stamp is one FAT can hold. A directory entry keeps
//! the year as its distance from 1980 in seven bits, so the years 1980 to
//! 2107 and no other, and fatfs packs what it is given without looking: a
//! year before 1980 underflows there. A clock outside that range is stamped
//! with the nearest moment FAT has.

/// The first and the last year a FAT stamp can hold.
const FIRST_YEAR: u32 = 1980;
const LAST_YEAR: u32 = 2107;

/// A local date and time by its fields, as the getters of `Date` give them.
#[derive(Clone, Copy, Debug)]
pub struct Local {
    pub year: u32,
    /// The month, counted from 0 for January.
    pub month: u32,
    /// The day of the month, counted from 1.
    pub day: u32,
    pub hour: u32,
    pub minute: u32,
    pub second: u32,
    pub millisecond: u32,
}

/// The FAT stamp of `local`.
pub fn date_time(local: Local) -> fatfs::DateTime {
    let stamp = |year, month, day, hour, min, sec, millis| fatfs::DateTime {
        date: fatfs::Date { year, month, day },
        time: fatfs::Time {
            hour,
            min,
            sec,
            millis,
        },
    };
    if local.year < FIRST_YEAR {
        return stamp(FIRST_YEAR as u16, 1, 1, 0, 0, 0, 0);
    }
    if local.year > LAST_YEAR {
        return stamp(LAST_YEAR as u16, 12, 31, 23, 59, 59, 990);
    }
    // Each field is held to its range: fatfs packs them into bit fields
    // without a check, and one out of its range would spill into the next.
    stamp(
        local.year as u16,
        local.month.saturating_add(1).clamp(1, 12) as u16,
        local.day.clamp(1, 31) as u16,
        local.hour.min(23) as u16,
        local.minute.min(59) as u16,
        local.second.min(59) as u16,
        local.millisecond.min(999) as u16,
    )
}

#[cfg(test)]
#[path = "fat_stamp_tests.rs"]
mod tests;
