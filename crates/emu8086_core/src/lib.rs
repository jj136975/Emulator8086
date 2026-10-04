//! The machine, as a library: `vm` is the CPU, its memory and the `Runtime`
//! that wires the devices of `io` to them and runs them, and `BIOS_ROM` is
//! the BIOS a machine runs unless it is given another. The crate-level test
//! modules and the build script's tests are declared here.
//!
//! The invariants, which let the crate build and run where there is neither
//! a window nor a clock to read (a browser, a test):
//!
//! - Nothing here opens a window or touches a GPU, and no dependency of
//!   this crate does. Keys arrive as scancodes down a channel and the
//!   picture leaves as a frame in memory (`io::raster`); the app, the root
//!   package of the workspace, shows the one and sends the other.
//! - Nothing here reads a clock or waits for one. The owner of a machine
//!   measures host time and tells the machine how much has passed
//!   (`Runtime::credit_pit_time`), and hands its wall clock to whatever
//!   formats a disk or copies a file onto one (`io::disk::WallClock`).

pub mod io;
mod utils;
pub mod vm;

#[cfg(test)]
mod cpu_tests;

#[cfg(test)]
mod bios_inspection_tests;

#[cfg(test)]
mod bios_tests;

#[cfg(test)]
mod crate_rules_tests;

/// The build script, compiled into the test binary so that its tests run
/// with the others.
#[cfg(test)]
#[allow(dead_code)]
#[path = "../build.rs"]
mod build_script;

/// The BIOS built into the crate: the image `build.rs` brings up to date
/// with its sources in `assets/rom` before the crate compiles.
pub const BIOS_ROM: &[u8] = include_bytes!("../../../assets/rom/bios.bin");
