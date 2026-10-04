//! The machine's devices, and the screen as pixels. Every device answers
//! its ports through `bus::IoDevice` and is put on the port bus when the
//! machine is built. `raster` turns what the VGA holds into an RGBA frame,
//! with the glyphs of `font_cp437`.
//!
//! The invariant: nothing here knows of a window or a GPU. Keys arrive as
//! scancodes down a channel and the picture leaves as a frame in memory;
//! what shows the one and sends the other is `crate::gui`. That is what
//! lets the tests cover every device and every pixel.

pub mod bus;
pub mod console;
pub mod disk;
pub mod disk_spec;
pub mod dma;
pub mod fdc;
pub mod font_cp437;
pub mod hdc;
pub mod keyboard;
pub mod pic;
pub mod pit;
pub mod raster;
pub mod vga;
