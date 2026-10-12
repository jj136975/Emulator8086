//! The window's renderer. Two things are drawn, and neither needs a window:
//! the emulator's picture (`blit`: `FrameBlit`, one texture that takes the
//! frame `raster` draws on the CPU and a fullscreen-triangle blit that
//! draws it as large as its target allows with its shape kept, centred,
//! its pixels sharp at any scale of one or more, and leaves the rest of
//! the target black) and the overlay (`overlay_pass`: egui's meshes
//! over the picture). `record_frame`, here, puts both into one render pass,
//! the picture first. `surface` is the window's side: `Renderer` holds the
//! wgpu surface on the winit window and records one such frame into it per
//! redraw, and each redraw reports how long the surface kept it waiting for
//! that frame, apart from its drawing (`Drawn`).
//!
//! The invariants: the frame holds display-encoded DAC colours that must
//! reach the screen unchanged, so the texture's encoding follows the
//! target's (`blit::framebuffer_format`). A mismatch decodes without
//! encoding again, or encodes what was never decoded: too dark or too
//! bright. And without an overlay to paint, a frame is the picture and
//! nothing else: egui's renderer is not touched.

mod blit;
mod overlay_pass;
mod surface;

pub use surface::{Drawn, Renderer};

use blit::FrameBlit;
use overlay_pass::OverlayPass;

use super::overlay::Paint;
use emu8086_core::io::raster;

const TEXT_WIDTH: u32 = raster::TEXT_WIDTH as u32;
const TEXT_HEIGHT: u32 = raster::TEXT_HEIGHT as u32;
const GFX_WIDTH: u32 = raster::GFX_WIDTH as u32;
const GFX_HEIGHT: u32 = raster::GFX_HEIGHT as u32;

/// Initial window size in physical pixels: the text frame doubled, which
/// is also the mode 13h frame times four, so both fill the window.
pub const DEFAULT_WINDOW_WIDTH: u32 = 2 * TEXT_WIDTH;
pub const DEFAULT_WINDOW_HEIGHT: u32 = 2 * TEXT_HEIGHT;

/// The limits the renderer asks of a device whose adapter offers `offered`:
/// wgpu's defaults where the adapter has them. An adapter that has not, as
/// the WebGL2 a browser without WebGPU is drawn with, is asked for the
/// least wgpu itself runs on, with textures as large as the adapter makes
/// them: the surface is one, as large as the window.
fn required_limits(offered: &wgpu::Limits) -> wgpu::Limits {
    let defaults = wgpu::Limits::default();
    if defaults.check_limits(offered) {
        defaults
    } else {
        wgpu::Limits::downlevel_webgl2_defaults().using_resolution(offered.clone())
    }
}

/// What the renderer asks of a device of `adapter`: no optional feature,
/// and the limits the adapter can give (`required_limits`).
fn device_descriptor(adapter: &wgpu::Adapter) -> wgpu::DeviceDescriptor<'static> {
    wgpu::DeviceDescriptor {
        label: Some("VGA Device"),
        required_features: wgpu::Features::empty(),
        required_limits: required_limits(&adapter.limits()),
        ..Default::default()
    }
}

/// Records one frame into `target`: a single render pass that starts from
/// black, holds the emulator's picture and, over it, the overlay's `paint`
/// if there is one. Returns the command buffers to submit, in order; the
/// textures `paint` gives up are released after the submit
/// (`OverlayPass::release`).
fn record_frame(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    blit: &FrameBlit,
    overlay: &mut OverlayPass,
    mut paint: Option<&mut Paint>,
    target: &wgpu::TextureView,
) -> Vec<wgpu::CommandBuffer> {
    let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
        label: Some("Frame Encoder"),
    });
    let size = blit.target_size();
    let mut commands = match paint.as_deref_mut() {
        Some(paint) => overlay.prepare(device, queue, &mut encoder, paint, size),
        None => Vec::new(),
    };
    {
        // egui's renderer takes a pass that does not borrow its encoder.
        // The compiler then no longer sees that the encoder must not be
        // used while the pass is open: this block is the pass, and the
        // encoder is next used after it.
        let mut pass = encoder
            .begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Frame Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: target,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                ..Default::default()
            })
            .forget_lifetime();
        blit.draw(&mut pass);
        if let Some(paint) = paint.as_deref() {
            overlay.draw(&mut pass, paint, size);
        }
    }
    commands.push(encoder.finish());
    commands
}

#[cfg(test)]
mod tests;
