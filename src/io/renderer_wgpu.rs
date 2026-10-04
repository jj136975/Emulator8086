use std::sync::Arc;
use std::time::Instant;
use wgpu::util::DeviceExt;
use winit::window::Window;

use crate::io::raster;
use crate::io::vga::{VgaDevice, VideoMode};
use crate::vm::memory::Memory;

const TEXT_WIDTH: u32 = raster::TEXT_WIDTH as u32;
const TEXT_HEIGHT: u32 = raster::TEXT_HEIGHT as u32;
const GFX_WIDTH: u32 = raster::GFX_WIDTH as u32;
const GFX_HEIGHT: u32 = raster::GFX_HEIGHT as u32;

/// Initial window size in physical pixels: the text frame doubled, which
/// is also the mode 13h frame times four, so both fill the window.
pub const DEFAULT_WINDOW_WIDTH: u32 = 2 * TEXT_WIDTH;
pub const DEFAULT_WINDOW_HEIGHT: u32 = 2 * TEXT_HEIGHT;

#[cfg(test)]
mod inspection_tests {
    use super::*;

    /// Regression: the default window was 960x600, which holds the 640x400
    /// text frame only at scale 1 and left more than half of it black.
    #[test]
    fn default_window_is_filled_by_both_frame_sizes() {
        let filled = (
            0.0,
            0.0,
            DEFAULT_WINDOW_WIDTH as f32,
            DEFAULT_WINDOW_HEIGHT as f32,
        );
        assert_eq!(
            compute_viewport_math(
                DEFAULT_WINDOW_WIDTH,
                DEFAULT_WINDOW_HEIGHT,
                TEXT_WIDTH,
                TEXT_HEIGHT
            ),
            filled
        );
        assert_eq!(
            compute_viewport_math(
                DEFAULT_WINDOW_WIDTH,
                DEFAULT_WINDOW_HEIGHT,
                GFX_WIDTH,
                GFX_HEIGHT
            ),
            filled
        );
    }

    /// Regression: the frame texture was always sRGB. On a surface without
    /// an sRGB format the decoded values were written back unencoded and
    /// the picture came out too dark.
    #[test]
    fn frame_texture_matches_the_colour_encoding_of_the_surface() {
        use wgpu::TextureFormat::*;
        assert_eq!(framebuffer_format(Bgra8UnormSrgb), Rgba8UnormSrgb);
        assert_eq!(framebuffer_format(Rgba8UnormSrgb), Rgba8UnormSrgb);
        assert_eq!(framebuffer_format(Bgra8Unorm), Rgba8Unorm);
        assert_eq!(framebuffer_format(Rgba8Unorm), Rgba8Unorm);
    }

    /// The blit shader parses and validates, including the check that
    /// implicit-derivative sampling is not used after a conditional return.
    #[test]
    fn blit_shader_is_valid_wgsl() {
        use wgpu::naga;
        let module = naga::front::wgsl::parse_str(SHADER_SRC).expect("shader parses");
        naga::valid::Validator::new(
            naga::valid::ValidationFlags::all(),
            naga::valid::Capabilities::empty(),
        )
        .validate(&module)
        .expect("shader validates");
        assert!(!SHADER_SRC.contains("textureSample("));
    }

    /// Regression: when the window is at least as large as the framebuffer,
    /// the viewport is integer-scaled and centered.
    #[test]
    fn viewport_integer_scales_and_centers() {
        // 960x600 window, 320x200 framebuffer (mode 13h). Integer scale = 3.
        let (ox, oy, w, h) = compute_viewport_math(960, 600, 320, 200);
        assert_eq!(w, 960.0); // 320 * 3
        assert_eq!(h, 600.0); // 200 * 3
        assert_eq!(ox, 0.0);
        assert_eq!(oy, 0.0);
    }

    /// Regression: asymmetric windows letterbox on the limiting axis.
    #[test]
    fn viewport_letterboxes_on_constrained_axis() {
        // Window is much wider than tall; scale limited by height.
        let (ox, oy, w, h) = compute_viewport_math(2000, 400, 320, 200);
        // height allows scale 2, width would allow 6 — min is 2.
        assert_eq!(w, 640.0);
        assert_eq!(h, 400.0);
        assert_eq!(ox, (2000.0 - 640.0) / 2.0);
        assert_eq!(oy, 0.0);
    }

    /// Regression: when the window is smaller than the framebuffer, the
    /// viewport offset is a plain negative number (no u32 underflow that
    /// would push the viewport off-screen).
    #[test]
    fn viewport_underflow_when_window_smaller_than_framebuffer() {
        // 200x150 window, 320x200 framebuffer.
        let (ox, oy, w, h) = compute_viewport_math(200, 150, 320, 200);
        // scale clamped to 1; framebuffer drawn at native size, centered
        // with negative offsets (visible part is clipped by the window).
        assert_eq!(w, 320.0);
        assert_eq!(h, 200.0);
        assert_eq!(ox, (200.0 - 320.0) / 2.0); // -60.0
        assert_eq!(oy, (150.0 - 200.0) / 2.0); // -25.0
        assert!(ox < 0.0, "offset_x should be negative, got {}", ox);
        assert!(oy < 0.0, "offset_y should be negative, got {}", oy);
    }

    /// Regression: degenerate zero-sized framebuffer doesn't divide by zero.
    #[test]
    fn viewport_handles_zero_framebuffer_dimensions() {
        let (_, _, w, h) = compute_viewport_math(960, 600, 0, 0);
        // With fb clamped to 1, the scale saturates huge but we still get
        // finite numbers out instead of a panic.
        assert!(w.is_finite());
        assert!(h.is_finite());
    }
}

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

/// Format of the frame texture for a given surface format. The frame holds
/// DAC values, which are display-encoded. With an sRGB surface the texture
/// is sRGB too (decoded on sampling, encoded again on output); with any
/// other surface it must be plain, or the picture would be decoded only.
fn framebuffer_format(surface_format: wgpu::TextureFormat) -> wgpu::TextureFormat {
    if surface_format.is_srgb() {
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

/// Uniform buffer for the fullscreen blit shader.
#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct BlitUniforms {
    /// Viewport offset (in pixels) and scale factor, packed as [offset_x, offset_y, scaled_w, scaled_h].
    viewport: [f32; 4],
    /// Window size.
    window_size: [f32; 2],
    _pad: [f32; 2],
}

pub struct WgpuRenderer {
    device: wgpu::Device,
    queue: wgpu::Queue,
    surface: wgpu::Surface<'static>,
    surface_config: wgpu::SurfaceConfiguration,
    pipeline: wgpu::RenderPipeline,
    texture: wgpu::Texture,
    texture_view: wgpu::TextureView,
    sampler: wgpu::Sampler,
    bind_group_layout: wgpu::BindGroupLayout,
    bind_group: wgpu::BindGroup,
    uniform_buffer: wgpu::Buffer,

    // CPU-side framebuffer (RGBA)
    framebuffer: Vec<u8>,
    fb_width: u32,
    fb_height: u32,
    fb_format: wgpu::TextureFormat,

    // Current mode tracking
    current_mode: VideoMode,

    // Window dimensions
    win_width: u32,
    win_height: u32,

    /// Reference point for the blink phase of cursor and text.
    started: Instant,
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
    // Map pixel position to the viewport area
    let pixel = in.uv * uniforms.window_size;
    let vp = uniforms.viewport; // offset_x, offset_y, scaled_w, scaled_h

    // Check if pixel is inside the viewport
    if pixel.x < vp.x || pixel.x >= vp.x + vp.z ||
       pixel.y < vp.y || pixel.y >= vp.y + vp.w {
        return vec4<f32>(0.0, 0.0, 0.0, 1.0); // black border
    }

    // Map to texture UV. The texture has a single level, and naming it
    // keeps the lookup legal after the conditional return above.
    let tex_uv = (pixel - vp.xy) / vp.zw;
    return textureSampleLevel(tex, tex_sampler, tex_uv, 0.0);
}
"#;

impl WgpuRenderer {
    pub fn new(window: Arc<Window>) -> Self {
        let size = window.inner_size();

        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });

        let surface = instance.create_surface(window.clone()).unwrap();

        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .expect("Failed to find GPU adapter");

        let (device, queue) = pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                label: Some("VGA Device"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::default(),
                ..Default::default()
            },
            None,
        ))
        .expect("Failed to create GPU device");

        let surface_caps = surface.get_capabilities(&adapter);
        let surface_format = surface_caps
            .formats
            .iter()
            .find(|f| f.is_srgb())
            .copied()
            .unwrap_or(surface_caps.formats[0]);

        let surface_config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width.max(1),
            height: size.height.max(1),
            present_mode: wgpu::PresentMode::AutoVsync,
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &surface_config);

        // Create framebuffer texture (start with text mode size)
        let fb_width = TEXT_WIDTH;
        let fb_height = TEXT_HEIGHT;
        let framebuffer = vec![0u8; (fb_width * fb_height * 4) as usize];
        let fb_format = framebuffer_format(surface_format);

        let texture = create_framebuffer_texture(&device, fb_format, fb_width, fb_height);
        let texture_view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Nearest Sampler"),
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        // The same integer-scaled viewport that a resize computes, so the
        // first frames are not stretched to the window.
        let (win_width, win_height) = (size.width.max(1), size.height.max(1));
        let (vp_x, vp_y, vp_w, vp_h) =
            compute_viewport_math(win_width, win_height, fb_width, fb_height);
        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Blit Uniforms"),
            contents: bytemuck::cast_slice(&[BlitUniforms {
                viewport: [vp_x, vp_y, vp_w, vp_h],
                window_size: [win_width as f32, win_height as f32],
                _pad: [0.0; 2],
            }]),
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

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Blit BG"),
            layout: &bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: uniform_buffer.as_entire_binding(),
                },
            ],
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Blit Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
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
                    format: surface_format,
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
            multiview: None,
            cache: None,
        });

        Self {
            device,
            queue,
            surface,
            surface_config,
            pipeline,
            texture,
            texture_view,
            sampler,
            bind_group_layout,
            bind_group,
            uniform_buffer,
            framebuffer,
            fb_width,
            fb_height,
            fb_format,
            current_mode: VideoMode::Text80x25,
            win_width,
            win_height,
            started: Instant::now(),
        }
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        if width == 0 || height == 0 {
            return;
        }
        self.win_width = width;
        self.win_height = height;
        self.surface_config.width = width;
        self.surface_config.height = height;
        self.surface.configure(&self.device, &self.surface_config);
        self.update_uniforms();
    }

    fn update_uniforms(&self) {
        let (vp_x, vp_y, vp_w, vp_h) = self.compute_viewport();
        self.queue.write_buffer(
            &self.uniform_buffer,
            0,
            bytemuck::cast_slice(&[BlitUniforms {
                viewport: [vp_x, vp_y, vp_w, vp_h],
                window_size: [self.win_width as f32, self.win_height as f32],
                _pad: [0.0; 2],
            }]),
        );
    }

    /// Compute integer-scaled viewport centered in window.
    fn compute_viewport(&self) -> (f32, f32, f32, f32) {
        compute_viewport_math(
            self.win_width,
            self.win_height,
            self.fb_width,
            self.fb_height,
        )
    }

    /// Recreate the framebuffer texture when mode changes.
    fn recreate_texture(&mut self, width: u32, height: u32) {
        self.fb_width = width;
        self.fb_height = height;
        self.framebuffer.resize((width * height * 4) as usize, 0);

        self.texture = create_framebuffer_texture(&self.device, self.fb_format, width, height);
        self.texture_view = self
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        // Rebuild bind group with new texture view
        self.bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Blit BG"),
            layout: &self.bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&self.texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&self.sampler),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: self.uniform_buffer.as_entire_binding(),
                },
            ],
        });

        self.update_uniforms();
    }

    /// Render current VGA state to the window.
    pub fn render(&mut self, vga: &VgaDevice, memory: &Memory) {
        // Handle mode changes
        if vga.video_mode != self.current_mode {
            self.current_mode = vga.video_mode;
            match self.current_mode {
                VideoMode::Text80x25 => self.recreate_texture(TEXT_WIDTH, TEXT_HEIGHT),
                VideoMode::Mode13h => self.recreate_texture(GFX_WIDTH, GFX_HEIGHT),
            }
        }

        // Rasterize to CPU framebuffer
        match self.current_mode {
            VideoMode::Text80x25 => {
                let phase = raster::BlinkPhase::at(self.started.elapsed());
                raster::rasterize_text(&mut self.framebuffer, vga, memory, phase);
            }
            VideoMode::Mode13h => raster::rasterize_mode13h(&mut self.framebuffer, vga, memory),
        }

        // Upload to GPU texture
        self.queue.write_texture(
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

        // Render to screen
        let output = match self.surface.get_current_texture() {
            Ok(t) => t,
            Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                self.surface.configure(&self.device, &self.surface_config);
                return;
            }
            Err(e) => {
                eprintln!("Surface error: {}", e);
                return;
            }
        };

        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Blit Encoder"),
            });

        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Blit Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                ..Default::default()
            });
            pass.set_pipeline(&self.pipeline);
            pass.set_bind_group(0, &self.bind_group, &[]);
            pass.draw(0..3, 0..1);
        }

        self.queue.submit(std::iter::once(encoder.finish()));
        output.present();
    }
}
