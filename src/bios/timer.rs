use crate::bios::bda;
use crate::vm::instructions::control;
use crate::vm::runtime::{CpuFlag, Runtime};

/// INT 08h — Timer tick IRQ handler (IRQ 0, slot 0xF0)
///
/// The actual tick counter increment is done by `check_timer_tick()` in runtime.rs
/// before the IRQ fires. This handler calls the INT 1Ch user timer hook
/// (used by TSRs, DOS idle, and programs needing periodic callbacks),
/// then sends EOI.
pub fn int08h(vm: &mut Runtime) {
    // INT 1Ch — user timer hook (default IVT points to IRET stub)
    control::dispatch_int(vm, 0x1C);

    if let Some(ref pic) = vm.pic {
        pic.lock().unwrap().eoi();
    }
}

/// INT 1Ah — Time of Day services
pub fn int1ah(vm: &mut Runtime) {
    let ah = vm.registers.ax.high();
    match ah {
        // AH=00: Read tick count
        0x00 => {
            let tick_lo = vm.memory.read_word(bda::TICK_COUNT);
            let tick_hi = vm.memory.read_word(bda::TICK_COUNT + 2);
            vm.registers.cx.set(tick_hi);
            vm.registers.dx.set(tick_lo);
            let midnight = vm.memory.read_byte(bda::TICK_OVERFLOW);
            vm.registers.ax.set_low(midnight);
            vm.memory.write_byte(bda::TICK_OVERFLOW, 0);
        }
        // AH=01: Set tick count
        0x01 => {
            let cx = vm.registers.cx.word();
            let dx = vm.registers.dx.word();
            vm.memory.write_word(bda::TICK_COUNT, dx);
            vm.memory.write_word(bda::TICK_COUNT + 2, cx);
            vm.memory.write_byte(bda::TICK_OVERFLOW, 0);
        }
        // AH=02: Get RTC time (BCD)
        0x02 => {
            let (hour, min, sec) = local_hms();
            vm.registers.cx.set_high(to_bcd(hour));
            vm.registers.cx.set_low(to_bcd(min));
            vm.registers.dx.set_high(to_bcd(sec));
            vm.registers.dx.set_low(0);
            vm.unset_flag(CpuFlag::Carry);
        }
        // AH=04: Get RTC date (BCD)
        0x04 => {
            let (year, month, day) = local_ymd();
            vm.registers.cx.set_high(to_bcd((year / 100) as u8));
            vm.registers.cx.set_low(to_bcd((year % 100) as u8));
            vm.registers.dx.set_high(to_bcd(month));
            vm.registers.dx.set_low(to_bcd(day));
            vm.unset_flag(CpuFlag::Carry);
        }
        _ => {
            vm.set_flag(CpuFlag::Carry);
        }
    }
}

fn to_bcd(val: u8) -> u8 {
    ((val / 10) << 4) | (val % 10)
}

/// Get local time as (hour, minute, second) using platform APIs.
fn local_hms() -> (u8, u8, u8) {
    #[cfg(windows)]
    {
        use std::mem::MaybeUninit;
        #[repr(C)]
        struct SystemTime {
            year: u16,
            month: u16,
            day_of_week: u16,
            day: u16,
            hour: u16,
            minute: u16,
            second: u16,
            milliseconds: u16,
        }
        extern "system" {
            fn GetLocalTime(st: *mut SystemTime);
        }
        let mut st = MaybeUninit::<SystemTime>::uninit();
        unsafe {
            GetLocalTime(st.as_mut_ptr());
            let st = st.assume_init();
            (st.hour as u8, st.minute as u8, st.second as u8)
        }
    }
    #[cfg(not(windows))]
    {
        // Fallback: use libc localtime
        use std::time::{SystemTime, UNIX_EPOCH};
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        extern "C" {
            fn localtime(time: *const i64) -> *const Tm;
        }
        #[repr(C)]
        struct Tm {
            tm_sec: i32,
            tm_min: i32,
            tm_hour: i32,
            tm_mday: i32,
            tm_mon: i32,
            tm_year: i32,
            // ... more fields we don't need
        }
        let time = secs as i64;
        unsafe {
            let tm = localtime(&time);
            if tm.is_null() {
                return (0, 0, 0);
            }
            ((*tm).tm_hour as u8, (*tm).tm_min as u8, (*tm).tm_sec as u8)
        }
    }
}

/// Get local date as (year, month, day) using platform APIs.
fn local_ymd() -> (u16, u8, u8) {
    #[cfg(windows)]
    {
        use std::mem::MaybeUninit;
        #[repr(C)]
        struct SystemTime {
            year: u16,
            month: u16,
            day_of_week: u16,
            day: u16,
            hour: u16,
            minute: u16,
            second: u16,
            milliseconds: u16,
        }
        extern "system" {
            fn GetLocalTime(st: *mut SystemTime);
        }
        let mut st = MaybeUninit::<SystemTime>::uninit();
        unsafe {
            GetLocalTime(st.as_mut_ptr());
            let st = st.assume_init();
            (st.year, st.month as u8, st.day as u8)
        }
    }
    #[cfg(not(windows))]
    {
        use std::time::{SystemTime, UNIX_EPOCH};
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        extern "C" {
            fn localtime(time: *const i64) -> *const Tm;
        }
        #[repr(C)]
        struct Tm {
            tm_sec: i32,
            tm_min: i32,
            tm_hour: i32,
            tm_mday: i32,
            tm_mon: i32,
            tm_year: i32,
        }
        let time = secs as i64;
        unsafe {
            let tm = localtime(&time);
            if tm.is_null() {
                return (2026, 1, 1);
            }
            (
                ((*tm).tm_year + 1900) as u16,
                ((*tm).tm_mon + 1) as u8,
                (*tm).tm_mday as u8,
            )
        }
    }
}
