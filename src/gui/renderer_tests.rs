//! The viewport and texture format helpers and the shader, which naga
//! parses and validates, are tested without a GPU. The last test runs the
//! pipeline on a real one: it asks for an adapter without a surface, draws
//! `FrameBlit` into a texture and reads the pixels back.
//!
//! The invariant: no test needs a window, and none fails for want of a GPU.
//! On a machine without an adapter the GPU test says so and passes.

use std::time::Duration;

use super::*;
use crate::io::bus::IoDevice;
use crate::vm::runtime::Runtime;

/// The default window is a whole multiple of both frame sizes, so the text
/// frame and the mode 13h frame each fill it without a border.
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

/// The frame texture is sRGB exactly when the surface is. On a surface
/// without sRGB, an sRGB texture's values would be decoded on sampling and
/// written back unencoded, and the picture would come out too dark.
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

/// When the window is at least as large as the framebuffer, the viewport is
/// integer-scaled and centered.
#[test]
fn viewport_integer_scales_and_centers() {
    // 960x600 window, 320x200 framebuffer (mode 13h). Integer scale = 3.
    let (ox, oy, w, h) = compute_viewport_math(960, 600, 320, 200);
    assert_eq!(w, 960.0); // 320 * 3
    assert_eq!(h, 600.0); // 200 * 3
    assert_eq!(ox, 0.0);
    assert_eq!(oy, 0.0);
}

/// An asymmetric window letterboxes on the limiting axis.
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

/// When the window is smaller than the framebuffer, the viewport offset is
/// a plain negative number, not a u32 underflow that would push the
/// viewport off-screen.
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

/// A zero-sized framebuffer does not divide by zero.
#[test]
fn viewport_handles_zero_framebuffer_dimensions() {
    let (_, _, w, h) = compute_viewport_math(960, 600, 0, 0);
    // With fb clamped to 1, the scale saturates huge but we still get
    // finite numbers out instead of a panic.
    assert!(w.is_finite());
    assert!(h.is_finite());
}

// ---------------------------------------------------------------------------
// On a GPU
// ---------------------------------------------------------------------------

/// Both halves of both blinks visible, so a text frame shows all it holds.
const ALL_ON: BlinkPhase = BlinkPhase {
    cursor_on: true,
    text_on: true,
};

/// A device asked for as the window asks for its own, but without a
/// surface. `None`, with the reason printed, on a machine that has no
/// adapter or whose adapter gives no device.
fn gpu() -> Option<(wgpu::Device, wgpu::Queue)> {
    // The environment may narrow the choice (`WGPU_BACKEND=vulkan`), so
    // that one machine can run the test on each backend it has.
    let instance =
        wgpu::Instance::new(wgpu::InstanceDescriptor::new_without_display_handle_from_env());
    let options = wgpu::RequestAdapterOptions::default();
    let adapter = match pollster::block_on(instance.request_adapter(&options)) {
        Ok(adapter) => adapter,
        Err(error) => {
            eprintln!("skipped: no GPU adapter: {error}");
            return None;
        }
    };
    let info = adapter.get_info();
    eprintln!(
        "GPU test on {} ({}, {:?})",
        info.name, info.backend, info.device_type
    );
    match pollster::block_on(adapter.request_device(&device_descriptor())) {
        Ok(device_and_queue) => Some(device_and_queue),
        Err(error) => {
            eprintln!("skipped: the adapter gives no device: {error}");
            None
        }
    }
}

/// A text screen that shows every character and every attribute, with the
/// cursor on it.
fn text_screen() -> (Runtime, VgaDevice) {
    let mut rt = Runtime::new_test();
    for cell in 0..80 * 25 {
        rt.cpu.memory.write_byte(0xB8000 + cell * 2, cell as u8);
        // 7 has no factor in common with 256: every attribute comes up.
        rt.cpu
            .memory
            .write_byte(0xB8000 + cell * 2 + 1, (cell * 7) as u8);
    }
    let mut vga = VgaDevice::new();
    let cursor: u16 = 12 * 80 + 40;
    vga.port_out_byte(0x3D4, 0x0E, &mut rt.cpu);
    vga.port_out_byte(0x3D5, (cursor >> 8) as u8, &mut rt.cpu);
    vga.port_out_byte(0x3D4, 0x0F, &mut rt.cpu);
    vga.port_out_byte(0x3D5, cursor as u8, &mut rt.cpu);
    (rt, vga)
}

/// Switches to mode 13h, gives every DAC entry a colour of its own and
/// paints stripes of all 256 that no flip or mirror maps onto themselves.
fn show_mode_13h_stripes(rt: &mut Runtime, vga: &mut VgaDevice) {
    vga.port_out_byte(0x3CE, 0x05, &mut rt.cpu);
    vga.port_out_byte(0x3CF, 0x40, &mut rt.cpu); // 256-colour shift mode
    vga.port_out_byte(0x3C8, 0, &mut rt.cpu);
    for index in 0..=255u8 {
        for component in [index & 0x3F, index >> 2, 0x3F - (index & 0x3F)] {
            vga.port_out_byte(0x3C9, component, &mut rt.cpu);
        }
    }
    for y in 0..raster::GFX_HEIGHT {
        for x in 0..raster::GFX_WIDTH {
            rt.cpu
                .memory
                .write_byte(0xA0000 + y * raster::GFX_WIDTH + x, (x + 3 * y) as u8);
        }
    }
}

/// Draws the blit's frame into a new `width` x `height` texture of `format`
/// and returns what the texture then holds: RGBA, row after row.
fn draw_and_read_back(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    blit: &FrameBlit,
    format: wgpu::TextureFormat,
    (width, height): (u32, u32),
) -> Vec<u8> {
    let size = wgpu::Extent3d {
        width,
        height,
        depth_or_array_layers: 1,
    };
    let target = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("Test Target"),
        size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
        view_formats: &[],
    });
    let view = target.create_view(&wgpu::TextureViewDescriptor::default());

    // A copy from a texture wants each row of the buffer padded to a
    // multiple of 256 bytes.
    let row_bytes = width * 4;
    let padded_row_bytes = row_bytes.next_multiple_of(wgpu::COPY_BYTES_PER_ROW_ALIGNMENT);
    let buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("Test Readback"),
        size: u64::from(padded_row_bytes) * u64::from(height),
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });

    let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
    blit.draw(&mut encoder, &view);
    encoder.copy_texture_to_buffer(
        wgpu::TexelCopyTextureInfo {
            texture: &target,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: wgpu::TextureAspect::All,
        },
        wgpu::TexelCopyBufferInfo {
            buffer: &buffer,
            layout: wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(padded_row_bytes),
                rows_per_image: Some(height),
            },
        },
        size,
    );
    queue.submit([encoder.finish()]);

    let (mapped, was_mapped) = std::sync::mpsc::channel();
    buffer.map_async(wgpu::MapMode::Read, .., move |result| {
        let _ = mapped.send(result);
    });
    device
        .poll(wgpu::PollType::Wait {
            submission_index: None,
            timeout: Some(Duration::from_secs(30)),
        })
        .expect("the GPU finishes the frame");
    was_mapped
        .recv()
        .expect("the map callback runs during the poll")
        .expect("the buffer maps");

    let padded = buffer.get_mapped_range(..).expect("the buffer is mapped");
    padded
        .chunks(padded_row_bytes as usize)
        .flat_map(|row| &row[..row_bytes as usize])
        .copied()
        .collect()
}

/// What a `width` x `height` target is to hold: `frame`, each of its pixels
/// `scale` target pixels wide and high, its top left corner at `offset`,
/// and opaque black wherever the frame does not reach.
fn frame_on_target(
    frame: &[u8],
    (fb_width, fb_height): (usize, usize),
    (width, height): (u32, u32),
    scale: i32,
    (offset_x, offset_y): (i32, i32),
) -> Vec<u8> {
    let mut expected = Vec::with_capacity((width * height * 4) as usize);
    for y in 0..height as i32 {
        for x in 0..width as i32 {
            // Rounded down, so that a pixel left of or above the frame gets
            // a negative coordinate, not 0.
            let frame_x = (x - offset_x).div_euclid(scale);
            let frame_y = (y - offset_y).div_euclid(scale);
            if (0..fb_width as i32).contains(&frame_x) && (0..fb_height as i32).contains(&frame_y) {
                let at = (frame_y as usize * fb_width + frame_x as usize) * 4;
                expected.extend_from_slice(&frame[at..at + 4]);
            } else {
                expected.extend_from_slice(&[0, 0, 0, 255]);
            }
        }
    }
    expected
}

/// Fails with the first pixel that differs and the number that do, rather
/// than with two images of a megabyte each.
fn assert_same_pixels(drawn: &[u8], expected: &[u8], width: u32, what: &str) {
    assert_eq!(drawn.len(), expected.len(), "{what}: image size");
    let mut differing = drawn
        .chunks(4)
        .zip(expected.chunks(4))
        .enumerate()
        .filter(|(_, (drawn, expected))| drawn != expected);
    if let Some((index, (drawn, expected))) = differing.next() {
        let (x, y) = (index as u32 % width, index as u32 / width);
        panic!(
            "{what}: {} pixels differ, the first at {x},{y}: drawn {drawn:?}, expected {expected:?}",
            differing.count() + 1
        );
    }
}

/// The pipeline puts the frame `raster` drew on its target byte for byte:
/// every frame pixel is a square of whole target pixels, the picture sits
/// in the middle (cut at the edges of a target smaller than the frame), the
/// rest of the target is opaque black, and the colours are those of the
/// frame whether the target is sRGB or not. A change of video mode on the
/// same blit swaps the frame texture for one of the new size.
#[test]
fn blit_draws_the_rasterised_frame_scaled_and_centred() {
    let Some((device, queue)) = gpu() else {
        return;
    };
    const TEXT: (usize, usize) = (raster::TEXT_WIDTH, raster::TEXT_HEIGHT);
    const GFX: (usize, usize) = (raster::GFX_WIDTH, raster::GFX_HEIGHT);

    for format in [
        wgpu::TextureFormat::Rgba8UnormSrgb,
        wgpu::TextureFormat::Rgba8Unorm,
    ] {
        let (mut rt, mut vga) = text_screen();
        let mut text_frame = vec![0; TEXT.0 * TEXT.1 * 4];
        raster::rasterize_text(&mut text_frame, &vga, &rt.cpu.memory, ALL_ON);

        // Twice the text frame, with a border of 10 pixels left and right
        // and of 15 above and below.
        let target = (1300, 830);
        let mut blit = FrameBlit::new(&device, format, target.0, target.1);
        blit.upload(&device, &queue, &vga, &rt.cpu.memory, ALL_ON);
        assert_same_pixels(
            &draw_and_read_back(&device, &queue, &blit, format, target),
            &frame_on_target(&text_frame, TEXT, target, 2, (10, 15)),
            target.0,
            &format!("text frame doubled, {format:?}"),
        );

        // A target smaller than the frame shows the middle of it.
        let target = (500, 300);
        blit.resize(&queue, target.0, target.1);
        assert_same_pixels(
            &draw_and_read_back(&device, &queue, &blit, format, target),
            &frame_on_target(&text_frame, TEXT, target, 1, (-70, -50)),
            target.0,
            &format!("text frame cut, {format:?}"),
        );

        // Mode 13h at three times its size. The borders are odd numbers of
        // pixels (41 and 101), so the picture sits half a pixel off centre:
        // 20 pixels from the left, 50 from the top.
        show_mode_13h_stripes(&mut rt, &mut vga);
        let mut gfx_frame = vec![0; GFX.0 * GFX.1 * 4];
        raster::rasterize_mode13h(&mut gfx_frame, &vga, &rt.cpu.memory);
        let target = (1001, 701);
        blit.resize(&queue, target.0, target.1);
        blit.upload(&device, &queue, &vga, &rt.cpu.memory, ALL_ON);
        assert_same_pixels(
            &draw_and_read_back(&device, &queue, &blit, format, target),
            &frame_on_target(&gfx_frame, GFX, target, 3, (20, 50)),
            target.0,
            &format!("mode 13h frame tripled, {format:?}"),
        );
    }
}
