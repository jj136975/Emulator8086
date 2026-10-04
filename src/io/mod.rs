//! The machine's devices and the two halves of its window frontend. Every
//! device answers its ports through `bus::IoDevice` and is put on the port
//! bus when the machine is built. The frontend draws the screen (`raster`
//! and `font_cp437` on the CPU, `renderer_wgpu` on the GPU) and turns host
//! keys into scancodes (`keyboard_winit`).
//!
//! The invariant: only `renderer_wgpu` and `keyboard_winit` know of the
//! window and the GPU. The devices and the rasterisers run without either,
//! which is what lets the tests cover them.

pub mod bus;
pub mod console;
pub mod disk;
pub mod disk_spec;
pub mod dma;
pub mod fdc;
pub mod font_cp437;
pub mod hdc;
pub mod keyboard;
pub mod keyboard_winit;
pub mod pic;
pub mod pit;
pub mod raster;
pub mod renderer_wgpu;
pub mod vga;
