//! The machine's devices and its two frontends. Every device answers its
//! ports through `bus::IoDevice` and is put on the port bus when the
//! machine is built. The frontends draw the screen and turn host keys into
//! scancodes: the terminal one through crossterm, the window one
//! (`--features gui`) through winit and wgpu.
//!
//! The invariant: a frontend module is compiled only into the builds that
//! use it, or the other build warns of dead code. The window's modules
//! need the `gui` feature's dependencies; the parts of either frontend that
//! run without a terminal or a GPU (`renderer_term`, `raster`,
//! `font_cp437`) also compile into every test build, so the default
//! `cargo test` covers both.

pub mod bus;
pub mod console;
pub mod disk;
pub mod disk_spec;
pub mod dma;
pub mod fdc;
pub mod hdc;
pub mod keyboard;
pub mod pic;
pub mod pit;
#[cfg(any(not(feature = "gui"), test))]
pub mod renderer_term;
pub mod vga;

#[cfg(any(feature = "gui", test))]
pub mod font_cp437;
#[cfg(feature = "gui")]
pub mod keyboard_winit;
#[cfg(any(feature = "gui", test))]
pub mod raster;
#[cfg(feature = "gui")]
pub mod renderer_wgpu;
