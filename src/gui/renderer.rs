//! The window's renderer. Two things are drawn, and neither needs a window:
//! `FrameBlit` is the emulator's picture, one texture that takes the frame
//! `raster` draws on the CPU and a fullscreen-triangle blit that scales it
//! by a whole number, centres it in its target and leaves the rest of the
//! target black; `OverlayPass` is the overlay, egui's meshes over the
//! picture. `record_frame` puts both into one render pass, the picture
//! first. `Renderer` holds the wgpu surface on the winit window and records
//! one such frame into it per redraw.
//!
//! The invariants: the frame holds display-encoded DAC colours that must
//! reach the screen unchanged, so the texture's encoding follows the
//! target's (`framebuffer_format`). A mismatch decodes without encoding
//! again, or encodes what was never decoded: too dark or too bright. And
//! without an overlay to paint, a frame is the picture and nothing else:
//! egui's renderer is not touched.

use std::sync::Arc;
use wgpu::util::DeviceExt;
use winit::event_loop::OwnedDisplayHandle;
use winit::window::Window;

use super::overlay::{Overlay, Paint};
use crate::platform::Instant;
use emu8086_core::io::raster::{self, BlinkPhase};
use emu8086_core::io::vga::{VgaDevice, VideoMode};
use emu8086_core::vm::memory::Memory;

const TEXT_WIDTH: u32 = raster::TEXT_WIDTH as u32;
const TEXT_HEIGHT: u32 = raster::TEXT_HEIGHT as u32;
const GFX_WIDTH: u32 = raster::GFX_WIDTH as u32;
const GFX_HEIGHT: u32 = raster::GFX_HEIGHT as u32;

/// Initial window size in physical pixels: the text frame doubled, which
/// is also the mode 13h frame times four, so both fill the window.
pub const DEFAULT_WINDOW_WIDTH: u32 = 2 * TEXT_WIDTH;
pub const DEFAULT_WINDOW_HEIGHT: u32 = 2 * TEXT_HEIGHT;

/// Compute an integer-scaled viewport centered in the window.
///
/// Returns (offset_x, offset_y, scaled_w, scaled_h) in pixels as f32.
///
/// Scale is at least 1 — when the window is smaller than the framebuffer we
/// still draw at 1:1 and let the window clip. Offsets use signed arithmetic
/// so that sub-framebuffer windows produce a negative offset (centered
/// overflow) rather than a u32 underflow.
pub(crate) fn compute_viewport_math(
    win_width: u32,
    win_height: u32,
    fb_width: u32,
    fb_height: u32,
) -> (f32, f32, f32, f32) {
    let fb_w = fb_width.max(1);
    let fb_h = fb_height.max(1);
    let scale_x = win_width / fb_w;
    let scale_y = win_height / fb_h;
    let scale = scale_x.min(scale_y).max(1);

    let scaled_w = fb_w * scale;
    let scaled_h = fb_h * scale;
    // Signed math so that win < scaled produces a negative offset instead
    // of a u32 wrap to ~4 billion pixels.
    let offset_x = (win_width as i32 - scaled_w as i32) / 2;
    let offset_y = (win_height as i32 - scaled_h as i32) / 2;

    (
        offset_x as f32,
        offset_y as f32,
        scaled_w as f32,
        scaled_h as f32,
    )
}

/// Format of the frame texture for a given target format. The frame holds
/// DAC values, which are display-encoded. With an sRGB target the texture
/// is sRGB too (decoded on sampling, encoded again on output); with any
/// other target it must be plain, or the picture would be decoded only.
fn framebuffer_format(target_format: wgpu::TextureFormat) -> wgpu::TextureFormat {
    if target_format.is_srgb() {
        wgpu::TextureFormat::Rgba8UnormSrgb
    } else {
        wgpu::TextureFormat::Rgba8Unorm
    }
}

fn create_framebuffer_texture(
    device: &wgpu::Device,
    format: wgpu::TextureFormat,
    width: u32,
    height: u32,
) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("VGA Framebuffer"),
        size: wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        view_formats: &[],
    })
}

/// The bind group of the blit: the frame texture, its sampler and the
/// uniforms. It holds a view of the texture, so a new texture needs a new
/// bind group.
fn create_blit_bind_group(
    device: &wgpu::Device,
    layout: &wgpu::BindGroupLayout,
    texture: &wgpu::Texture,
    sampler: &wgpu::Sampler,
    uniform_buffer: &wgpu::Buffer,
) -> wgpu::BindGroup {
    let texture_view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("Blit BG"),
        layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&texture_view),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(sampler),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: uniform_buffer.as_entire_binding(),
            },
        ],
    })
}

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

/// Uniform buffer for the fullscreen blit shader.
#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct BlitUniforms {
    /// The viewport in pixels: offset_x, offset_y, scaled_w, scaled_h.
    viewport: [f32; 4],
    /// Size of the target, which for the surface is the window's.
    window_size: [f32; 2],
    _pad: [f32; 2],
}

impl BlitUniforms {
    /// The uniforms that put a frame of `fb_width` x `fb_height` on a target
    /// of `width` x `height`.
    fn new(width: u32, height: u32, fb_width: u32, fb_height: u32) -> Self {
        let (vp_x, vp_y, vp_w, vp_h) = compute_viewport_math(width, height, fb_width, fb_height);
        Self {
            viewport: [vp_x, vp_y, vp_w, vp_h],
            window_size: [width as f32, height as f32],
            _pad: [0.0; 2],
        }
    }
}

const SHADER_SRC: &str = r#"
struct Uniforms {
    viewport: vec4<f32>,    // offset_x, offset_y, scaled_w, scaled_h
    window_size: vec2<f32>,
    _pad: vec2<f32>,
}

@group(0) @binding(0) var tex: texture_2d<f32>;
@group(0) @binding(1) var tex_sampler: sampler;
@group(0) @binding(2) var<uniform> uniforms: Uniforms;

struct VsOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
}

@vertex
fn vs_main(@builtin(vertex_index) vi: u32) -> VsOutput {
    // Fullscreen triangle
    var pos = array<vec2<f32>, 3>(
        vec2<f32>(-1.0, -1.0),
        vec2<f32>(3.0, -1.0),
        vec2<f32>(-1.0, 3.0),
    );
    var uv = array<vec2<f32>, 3>(
        vec2<f32>(0.0, 1.0),
        vec2<f32>(2.0, 1.0),
        vec2<f32>(0.0, -1.0),
    );
    var out: VsOutput;
    out.position = vec4<f32>(pos[vi], 0.0, 1.0);
    out.uv = uv[vi];
    return out;
}

@fragment
fn fs_main(in: VsOutput) -> @location(0) vec4<f32> {
    // uv spans the window from 0 (top left) to 1 (bottom right).
    let pixel = in.uv * uniforms.window_size;
    let vp = uniforms.viewport; // offset_x, offset_y, scaled_w, scaled_h

    if pixel.x < vp.x || pixel.x >= vp.x + vp.z ||
       pixel.y < vp.y || pixel.y >= vp.y + vp.w {
        return vec4<f32>(0.0, 0.0, 0.0, 1.0); // black border
    }

    // The texture has a single level, and naming it keeps the lookup legal
    // after the conditional return above.
    let tex_uv = (pixel - vp.xy) / vp.zw;
    return textureSampleLevel(tex, tex_sampler, tex_uv, 0.0);
}
"#;

/// The emulator's frame on the GPU: the texture the rasterised frame is
/// uploaded to and the pipeline that draws it into a target of one format
/// and one size. It never sees the surface: `record_frame` draws it into
/// any texture view, which is what lets a test draw into a texture and read
/// it back.
struct FrameBlit {
    pipeline: wgpu::RenderPipeline,
    texture: wgpu::Texture,
    sampler: wgpu::Sampler,
    bind_group_layout: wgpu::BindGroupLayout,
    bind_group: wgpu::BindGroup,
    uniform_buffer: wgpu::Buffer,

    // CPU-side framebuffer (RGBA)
    framebuffer: Vec<u8>,
    fb_width: u32,
    fb_height: u32,
    fb_format: wgpu::TextureFormat,

    /// The video mode the frame texture is sized for.
    current_mode: VideoMode,

    // Target size in physical pixels, never zero.
    target_width: u32,
    target_height: u32,
}

impl FrameBlit {
    /// A blit into targets of `target_format` that are `width` x `height`
    /// pixels.
    fn new(
        device: &wgpu::Device,
        target_format: wgpu::TextureFormat,
        width: u32,
        height: u32,
    ) -> Self {
        // Sized for text mode, the mode `current_mode` starts in.
        let fb_width = TEXT_WIDTH;
        let fb_height = TEXT_HEIGHT;
        let framebuffer = vec![0u8; (fb_width * fb_height * 4) as usize];
        let fb_format = framebuffer_format(target_format);

        let texture = create_framebuffer_texture(device, fb_format, fb_width, fb_height);

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Nearest Sampler"),
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        // The same integer-scaled viewport that a resize computes, so the
        // first frames are not stretched to the target.
        let (target_width, target_height) = (width.max(1), height.max(1));
        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Blit Uniforms"),
            contents: bytemuck::bytes_of(&BlitUniforms::new(
                target_width,
                target_height,
                fb_width,
                fb_height,
            )),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Blit BGL"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 2,
                    visibility: wgpu::ShaderStages::VERTEX_FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            ],
        });

        let bind_group = create_blit_bind_group(
            device,
            &bind_group_layout,
            &texture,
            &sampler,
            &uniform_buffer,
        );

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Blit Pipeline Layout"),
            bind_group_layouts: &[Some(&bind_group_layout)],
            immediate_size: 0,
        });

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Blit Shader"),
            source: wgpu::ShaderSource::Wgsl(SHADER_SRC.into()),
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Blit Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                buffers: &[],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: target_format,
                    blend: Some(wgpu::BlendState::REPLACE),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview_mask: None,
            cache: None,
        });

        Self {
            pipeline,
            texture,
            sampler,
            bind_group_layout,
            bind_group,
            uniform_buffer,
            framebuffer,
            fb_width,
            fb_height,
            fb_format,
            current_mode: VideoMode::Text80x25,
            target_width,
            target_height,
        }
    }

    /// The target is `width` x `height` pixels from now on.
    fn resize(&mut self, queue: &wgpu::Queue, width: u32, height: u32) {
        self.target_width = width.max(1);
        self.target_height = height.max(1);
        self.update_uniforms(queue);
    }

    fn update_uniforms(&self, queue: &wgpu::Queue) {
        queue.write_buffer(
            &self.uniform_buffer,
            0,
            bytemuck::bytes_of(&BlitUniforms::new(
                self.target_width,
                self.target_height,
                self.fb_width,
                self.fb_height,
            )),
        );
    }

    /// Recreate the framebuffer texture when mode changes.
    fn recreate_texture(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        width: u32,
        height: u32,
    ) {
        self.fb_width = width;
        self.fb_height = height;
        self.framebuffer.resize((width * height * 4) as usize, 0);

        self.texture = create_framebuffer_texture(device, self.fb_format, width, height);
        self.bind_group = create_blit_bind_group(
            device,
            &self.bind_group_layout,
            &self.texture,
            &self.sampler,
            &self.uniform_buffer,
        );

        self.update_uniforms(queue);
    }

    /// Rasterises the screen as it looks in blink phase `phase` and uploads
    /// the frame to the texture.
    fn upload(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        vga: &VgaDevice,
        memory: &Memory,
        phase: BlinkPhase,
    ) {
        if vga.video_mode != self.current_mode {
            self.current_mode = vga.video_mode;
            match self.current_mode {
                VideoMode::Text80x25 => {
                    self.recreate_texture(device, queue, TEXT_WIDTH, TEXT_HEIGHT)
                }
                VideoMode::Mode13h => self.recreate_texture(device, queue, GFX_WIDTH, GFX_HEIGHT),
            }
        }

        match self.current_mode {
            VideoMode::Text80x25 => {
                raster::rasterize_text(&mut self.framebuffer, vga, memory, phase);
            }
            VideoMode::Mode13h => raster::rasterize_mode13h(&mut self.framebuffer, vga, memory),
        }

        queue.write_texture(
            wgpu::TexelCopyTextureInfo {
                texture: &self.texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            &self.framebuffer,
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(self.fb_width * 4),
                rows_per_image: Some(self.fb_height),
            },
            wgpu::Extent3d {
                width: self.fb_width,
                height: self.fb_height,
                depth_or_array_layers: 1,
            },
        );
    }

    /// Draws the uploaded frame over the whole of `pass`, whose target is of
    /// the format and the size the blit was told.
    fn draw(&self, pass: &mut wgpu::RenderPass<'_>) {
        pass.set_pipeline(&self.pipeline);
        pass.set_bind_group(0, &self.bind_group, &[]);
        pass.draw(0..3, 0..1);
    }

    /// Width and height of the target, in pixels.
    fn target_size(&self) -> [u32; 2] {
        [self.target_width, self.target_height]
    }
}

/// The overlay on the GPU: egui's own renderer for wgpu, which keeps the
/// textures egui draws from (its font atlas) and draws its meshes into a
/// pass that already holds the picture.
struct OverlayPass {
    renderer: egui_wgpu::Renderer,
}

impl OverlayPass {
    /// An overlay drawn into targets of `target_format`.
    fn new(device: &wgpu::Device, target_format: wgpu::TextureFormat) -> Self {
        let options = egui_wgpu::RendererOptions::default();
        Self {
            renderer: egui_wgpu::Renderer::new(device, target_format, options),
        }
    }

    /// Uploads what `paint` is drawn from, before the pass begins: the
    /// textures egui changed, then the meshes. `size` is that of the
    /// target, in pixels. Returns the command buffers of egui's paint
    /// callbacks, to be submitted ahead of the frame's.
    fn prepare(
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
    fn draw(&self, pass: &mut wgpu::RenderPass<'static>, paint: &Paint, size: [u32; 2]) {
        let screen = screen_descriptor(paint, size);
        self.renderer.render(pass, &paint.primitives, &screen);
    }

    /// Frees the textures egui gave up with `paint`. Only once the frame
    /// that may still draw from them is submitted.
    fn release(&mut self, paint: &mut Paint) {
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

/// The format the window is drawn in: one of the two plain 8-bit formats if
/// the surface has one, else whatever it offers first. egui blends the
/// encoded colours a plain target stores; on an sRGB target the GPU blends
/// them decoded, and the edges of its text and its shadows come out
/// differently. The picture is the same on either (`framebuffer_format`).
fn window_format(formats: &[wgpu::TextureFormat]) -> wgpu::TextureFormat {
    use wgpu::TextureFormat::{Bgra8Unorm, Rgba8Unorm};
    formats
        .iter()
        .copied()
        .find(|format| matches!(format, Bgra8Unorm | Rgba8Unorm))
        .unwrap_or(formats[0])
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
    /// until then.
    pub fn render(&mut self, vga: &VgaDevice, memory: &Memory, overlay: Option<&mut Overlay>) {
        let output = match self.surface.get_current_texture() {
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

#[cfg(test)]
#[path = "renderer_tests.rs"]
mod tests;
