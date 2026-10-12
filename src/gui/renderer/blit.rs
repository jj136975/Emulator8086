//! The emulator's picture on the GPU: `FrameBlit`, the texture the frame
//! `raster` draws on the CPU is uploaded to, and the fullscreen-triangle
//! blit that draws it into a target of one format and one size, as large
//! as the target allows with its shape kept and centred (`viewport`), its
//! pixels sharp at any scale of one or more and blended below, the rest of
//! the target black.
//!
//! The invariant: the texture's encoding follows the target's
//! (`framebuffer_format`). The frame holds display-encoded DAC colours that
//! must reach the target unchanged.

use wgpu::util::DeviceExt;

use super::{GFX_HEIGHT, GFX_WIDTH, TEXT_HEIGHT, TEXT_WIDTH};
use emu8086_core::io::raster::{self, BlinkPhase};
use emu8086_core::io::vga::{VgaDevice, VideoMode};
use emu8086_core::vm::memory::Memory;

/// Where a frame of `frame_width` x `frame_height` pixels goes on a target
/// of `target_width` x `target_height`: as large as the target allows with
/// the frame's shape kept, a whole number of pixels wide and high, in the
/// middle of the target. A frame larger than the target is shrunk to fit,
/// whole. Returns (offset_x, offset_y, width, height), in pixels of the
/// target, as f32. The offset is rounded down: a border of an odd width
/// leaves its extra pixel on the right, or below.
pub(super) fn viewport(
    target_width: u32,
    target_height: u32,
    frame_width: u32,
    frame_height: u32,
) -> (f32, f32, f32, f32) {
    let frame_width = frame_width.max(1) as f32;
    let frame_height = frame_height.max(1) as f32;
    let scale = (target_width as f32 / frame_width).min(target_height as f32 / frame_height);
    let width = (frame_width * scale).round().max(1.0);
    let height = (frame_height * scale).round().max(1.0);
    let offset_x = ((target_width as f32 - width) / 2.0).floor();
    let offset_y = ((target_height as f32 - height) / 2.0).floor();
    (offset_x, offset_y, width, height)
}

/// Format of the frame texture for a given target format. The frame holds
/// DAC values, which are display-encoded. With an sRGB target the texture
/// is sRGB too (decoded on sampling, encoded again on output); with any
/// other target it must be plain, or the picture would be decoded only.
pub(super) fn framebuffer_format(target_format: wgpu::TextureFormat) -> wgpu::TextureFormat {
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

/// Uniform buffer for the fullscreen blit shader.
#[repr(C)]
#[derive(Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct BlitUniforms {
    /// The viewport in pixels of the target: offset_x, offset_y, width,
    /// height (`viewport`).
    viewport: [f32; 4],
    /// Size of the target, which for the surface is the window's.
    window_size: [f32; 2],
    /// Size of the frame, in its own pixels.
    frame_size: [f32; 2],
}

impl BlitUniforms {
    /// The uniforms that put a frame of `fb_width` x `fb_height` on a target
    /// of `width` x `height`.
    fn new(width: u32, height: u32, fb_width: u32, fb_height: u32) -> Self {
        let (vp_x, vp_y, vp_w, vp_h) = viewport(width, height, fb_width, fb_height);
        Self {
            viewport: [vp_x, vp_y, vp_w, vp_h],
            window_size: [width as f32, height as f32],
            frame_size: [fb_width.max(1) as f32, fb_height.max(1) as f32],
        }
    }
}

pub(super) const SHADER_SRC: &str = r#"
struct Uniforms {
    viewport: vec4<f32>,    // offset_x, offset_y, width, height, in pixels of the target
    window_size: vec2<f32>,
    frame_size: vec2<f32>,  // the frame's pixels across and down
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
    let vp = uniforms.viewport; // offset_x, offset_y, width, height

    if pixel.x < vp.x || pixel.x >= vp.x + vp.z ||
       pixel.y < vp.y || pixel.y >= vp.y + vp.w {
        return vec4<f32>(0.0, 0.0, 0.0, 1.0); // black border
    }

    // The pixel's place in the frame, in the frame's own pixels, and how
    // many of those one pixel of the target spans: under one where the
    // frame is enlarged, over one where it is shrunk.
    let texel = (pixel - vp.xy) / vp.zw * uniforms.frame_size;
    let span = uniforms.frame_size / vp.zw;
    // Sampled at the centre of its frame pixel, but across the seam between
    // two, for the width of one target pixel, where the two are blended: the
    // frame's pixels stay sharp at any scale, their edges without stairs,
    // and a frame shrunk below its size blends its neighbours throughout.
    let seam = floor(texel + 0.5);
    let snapped = seam + clamp((texel - seam) / span, vec2<f32>(-0.5), vec2<f32>(0.5));
    // The texture has a single level, and naming it keeps the lookup legal
    // after the conditional return above.
    return textureSampleLevel(tex, tex_sampler, snapped / uniforms.frame_size, 0.0);
}
"#;

/// The emulator's frame on the GPU: the texture the rasterised frame is
/// uploaded to and the pipeline that draws it into a target of one format
/// and one size. It never sees the surface: `record_frame` draws it into
/// any texture view, which is what lets a test draw into a texture and read
/// it back.
pub(super) struct FrameBlit {
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
    pub(super) fn new(
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

        // Linear, for the blend across a seam between two frame pixels that
        // the shader asks for; everywhere else it samples a pixel's centre.
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Blit Sampler"),
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        // The same viewport that a resize computes, so the first frames are
        // not stretched to the target.
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
    pub(super) fn resize(&mut self, queue: &wgpu::Queue, width: u32, height: u32) {
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
    pub(super) fn upload(
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
    pub(super) fn draw(&self, pass: &mut wgpu::RenderPass<'_>) {
        pass.set_pipeline(&self.pipeline);
        pass.set_bind_group(0, &self.bind_group, &[]);
        pass.draw(0..3, 0..1);
    }

    /// Width and height of the target, in pixels.
    pub(super) fn target_size(&self) -> [u32; 2] {
        [self.target_width, self.target_height]
    }
}
