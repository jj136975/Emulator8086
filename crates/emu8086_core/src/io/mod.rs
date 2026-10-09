//! The machine's devices, and the screen as pixels. Every device answers
//! its ports through `bus::IoDevice` and is put on the port bus when the
//! machine is built. `raster` turns what the VGA holds into an RGBA frame,
//! with the glyphs of `font_cp437`. Around the disks (`disk`) stand the two
//! ways the host has to them: `disk_spec`, the drives and images a start
//! names, and `disk_ops`, what a user does to the drives while the guest
//! runs.
//!
//! The invariant: nothing here knows of a window or a GPU. Keys arrive as
//! scancodes down a channel and the picture leaves as a frame in memory;
//! what shows the one and sends the other is the app. That is what lets
//! the tests cover every device and every pixel.
//!
//! A module is public when the app reaches into it: for the disks, the
//! keyboard, the picture, the interrupt controller the `Cpu` holds, and the
//! timer and the debug port, which the app only reads. The port bus and the
//! devices only it talks to stay inside the crate.

pub(crate) mod bus;
pub mod console;
pub mod disk;
pub mod disk_ops;
pub mod disk_spec;
pub(crate) mod dma;
pub(crate) mod fdc;
pub(crate) mod font_cp437;
pub(crate) mod hdc;
pub mod keyboard;
pub mod pic;
pub mod pit;
pub mod raster;
pub mod vga;
