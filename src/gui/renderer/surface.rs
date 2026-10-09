//! The window's side of the renderer: `Renderer` holds the wgpu surface on
//! the winit window, configures it (`window_format`, vsync, two frames in
//! flight) and records one frame into it per redraw (`record_frame`), and
//! each redraw reports how long the surface kept it waiting for that frame,
//! apart from the drawing (`Drawn`).
//!
//! The invariant: the overlay's paint is taken only once the surface has
//! given a frame to draw on. The texture changes it carries come once, and
//! stay in the overlay until a frame takes them.

use std::sync::Arc;
use std::time::Duration;
use winit::event_loop::OwnedDisplayHandle;
use winit::window::Window;

use super::{FrameBlit, OverlayPass, device_descriptor, record_frame};
use crate::gui::overlay::Overlay;
use crate::platform::Instant;
use emu8086_core::io::raster::BlinkPhase;
use emu8086_core::io::vga::VgaDevice;
use emu8086_core::vm::memory::Memory;

/// The format the window is drawn in: one of the two plain 8-bit formats if
/// the surface has one, else whatever it offers first. egui blends the
/// encoded colours a plain target stores; on an sRGB target the GPU blends
/// them decoded, and the edges of its text and its shadows come out
/// differently. The picture is the same on either (`framebuffer_format`).
pub(super) fn window_format(formats: &[wgpu::TextureFormat]) -> wgpu::TextureFormat {
    use wgpu::TextureFormat::{Bgra8Unorm, Rgba8Unorm};
    formats
        .iter()
        .copied()
        .find(|format| matches!(format, Bgra8Unorm | Rgba8Unorm))
        .unwrap_or(formats[0])
}

/// What one `Renderer::render` took: the time the surface kept it waiting
/// for a frame to draw on, and the drawing after that, presenting the frame
/// included.
#[derive(Clone, Copy, Debug)]
pub struct Drawn {
    pub waited: Duration,
    pub drawing: Duration,
}

pub struct Renderer {
    device: wgpu::Device,
    queue: wgpu::Queue,
    surface: wgpu::Surface<'static>,
    surface_config: wgpu::SurfaceConfiguration,
    blit: FrameBlit,
    overlay: OverlayPass,

    /// Reference point for the blink phase of cursor and text.
    started: Instant,
}

impl Renderer {
    /// Starts a renderer for `window`; `display` is the event loop's
    /// display handle, the one `window` was created on. The adapter and
    /// the device are awaited: a host that may block has them at once,
    /// another only after it went back to its event loop
    /// (`platform::spawn`), and until then the window has no renderer.
    pub async fn new(window: Arc<Window>, display: OwnedDisplayHandle) -> Self {
        // Every backend the build has. The GL backend presents through the
        // display the instance was given, so it gets the event loop's. A
        // browser has two, WebGPU and WebGL2, and an instance is of one for
        // good: whether WebGPU works there is found out first, by asking it
        // for an adapter. Elsewhere there is nothing to find out.
        let instance = wgpu::util::new_instance_with_webgpu_detection(
            wgpu::InstanceDescriptor::new_with_display_handle(Box::new(display)),
        )
        .await;

        let surface = instance.create_surface(window.clone()).unwrap();

        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
                apply_limit_buckets: false,
            })
            .await
            .expect("Failed to find GPU adapter");

        let info = adapter.get_info();
        log::info!("Drawing with {} ({})", info.name, info.backend);

        let (device, queue) = adapter
            .request_device(&device_descriptor(&adapter))
            .await
            .expect("Failed to create GPU device");

        // Measured after the waits: the window may have got its size, or
        // another one, while the device was on its way.
        let size = window.inner_size();
        let surface_caps = surface.get_capabilities(&adapter);
        let surface_format = window_format(&surface_caps.formats);

        let surface_config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            // For an 8-bit format `Auto` is sRGB at standard range, the
            // encoding the frame's DAC colours are in.
            color_space: wgpu::SurfaceColorSpace::Auto,
            width: size.width.max(1),
            height: size.height.max(1),
            present_mode: wgpu::PresentMode::AutoVsync,
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &surface_config);

        let blit = FrameBlit::new(
            &device,
            surface_format,
            surface_config.width,
            surface_config.height,
        );
        let overlay = OverlayPass::new(&device, surface_format);

        Self {
            device,
            queue,
            surface,
            surface_config,
            blit,
            overlay,
            started: Instant::now(),
        }
    }

    /// The longest side a texture can have on this device: the limit of
    /// egui's font atlas.
    pub fn max_texture_side(&self) -> usize {
        self.device.limits().max_texture_dimension_2d as usize
    }

    /// Width and height the surface is configured for, in pixels.
    pub fn size(&self) -> (u32, u32) {
        (self.surface_config.width, self.surface_config.height)
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        if width == 0 || height == 0 {
            return;
        }
        self.surface_config.width = width;
        self.surface_config.height = height;
        self.surface.configure(&self.device, &self.surface_config);
        self.blit.resize(&self.queue, width, height);
    }

    /// Draws one frame in the window: the VGA's picture as it is now and,
    /// over it, what the last pass of `overlay` left to paint, if an overlay
    /// is given. Its paint is taken only once there is a frame to draw on:
    /// the texture changes it carries come once and are kept in the overlay
    /// until then. Returns how long the surface kept it waiting for that
    /// frame, and how long the rest took.
    pub fn render(
        &mut self,
        vga: &VgaDevice,
        memory: &Memory,
        overlay: Option<&mut Overlay>,
    ) -> Drawn {
        let asked = Instant::now();
        let current = self.surface.get_current_texture();
        let waited = asked.elapsed();
        let drawing = Instant::now();
        self.draw_on(current, vga, memory, overlay);
        Drawn {
            waited,
            drawing: drawing.elapsed(),
        }
    }

    /// Draws on the frame `current` gave the surface, if it gave one: a lost
    /// surface is configured again for the next frame, and a hidden window
    /// has nothing to draw on.
    fn draw_on(
        &mut self,
        current: wgpu::CurrentSurfaceTexture,
        vga: &VgaDevice,
        memory: &Memory,
        overlay: Option<&mut Overlay>,
    ) {
        let output = match current {
            // A suboptimal frame is drawn all the same: the resize that
            // made it so configures the surface again.
            wgpu::CurrentSurfaceTexture::Success(frame)
            | wgpu::CurrentSurfaceTexture::Suboptimal(frame) => frame,
            wgpu::CurrentSurfaceTexture::Lost | wgpu::CurrentSurfaceTexture::Outdated => {
                self.surface.configure(&self.device, &self.surface_config);
                return;
            }
            // A hidden window has nothing to draw on; it is not an error.
            wgpu::CurrentSurfaceTexture::Occluded => return,
            status @ (wgpu::CurrentSurfaceTexture::Timeout
            | wgpu::CurrentSurfaceTexture::Validation) => {
                // Through the log, which every host shows somewhere: a
                // browser has no stderr.
                log::error!("Surface error: {:?}", status);
                return;
            }
        };

        // Only now that there is a frame to draw on: what is written to the
        // queue stays queued until a submit, which a frame that is not
        // drawn never makes.
        let phase = BlinkPhase::at(self.started.elapsed());
        self.blit
            .upload(&self.device, &self.queue, vga, memory, phase);

        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());
        let mut paint = overlay.map(Overlay::take_paint);
        let frame = record_frame(
            &self.device,
            &self.queue,
            &self.blit,
            &mut self.overlay,
            paint.as_mut(),
            &view,
        );
        self.queue.submit(frame);
        if let Some(paint) = &mut paint {
            self.overlay.release(paint);
        }
        self.queue.present(output);
    }
}
