//! The overlay on the GPU: `OverlayPass`, egui's renderer for wgpu, which
//! uploads what a pass of the overlay left to paint and draws its meshes
//! into a render pass that already holds the picture.
//!
//! The invariant: a texture egui gives up is freed only once the frame that
//! may still draw from it is submitted (`OverlayPass::release`).

use crate::gui::overlay::Paint;

/// The overlay on the GPU: egui's own renderer for wgpu, which keeps the
/// textures egui draws from (its font atlas) and draws its meshes into a
/// pass that already holds the picture.
pub(super) struct OverlayPass {
    renderer: egui_wgpu::Renderer,
}

impl OverlayPass {
    /// An overlay drawn into targets of `target_format`.
    pub(super) fn new(device: &wgpu::Device, target_format: wgpu::TextureFormat) -> Self {
        let options = egui_wgpu::RendererOptions::default();
        Self {
            renderer: egui_wgpu::Renderer::new(device, target_format, options),
        }
    }

    /// Uploads what `paint` is drawn from, before the pass begins: the
    /// textures egui changed, then the meshes. `size` is that of the
    /// target, in pixels. Returns the command buffers of egui's paint
    /// callbacks, to be submitted ahead of the frame's.
    pub(super) fn prepare(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        encoder: &mut wgpu::CommandEncoder,
        paint: &mut Paint,
        size: [u32; 2],
    ) -> Vec<wgpu::CommandBuffer> {
        for (id, changes) in paint.textures.set.drain() {
            for change in &changes {
                self.renderer.update_texture(device, queue, id, change);
            }
        }
        let screen = screen_descriptor(paint, size);
        self.renderer
            .update_buffers(device, queue, encoder, &paint.primitives, &screen)
    }

    /// Draws the meshes of `paint` into `pass`, over what it holds.
    pub(super) fn draw(&self, pass: &mut wgpu::RenderPass<'static>, paint: &Paint, size: [u32; 2]) {
        let screen = screen_descriptor(paint, size);
        self.renderer.render(pass, &paint.primitives, &screen);
    }

    /// Frees the textures egui gave up with `paint`. Only once the frame
    /// that may still draw from them is submitted.
    pub(super) fn release(&mut self, paint: &mut Paint) {
        for id in paint.textures.free.drain() {
            self.renderer.free_texture(&id);
        }
    }
}

fn screen_descriptor(paint: &Paint, size: [u32; 2]) -> egui_wgpu::ScreenDescriptor {
    egui_wgpu::ScreenDescriptor {
        size_in_pixels: size,
        pixels_per_point: paint.pixels_per_point,
    }
}
