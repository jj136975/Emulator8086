//! The viewport and format helpers and the shader, which naga parses and
//! validates, are tested without a GPU. Three tests run on a real one: they
//! ask for an adapter without a surface, record frames into a texture as
//! the window records them into its surface (`record_frame`) and read the
//! pixels back: the picture alone, the picture with the overlay over it,
//! and both again on a device that has no more than WebGL2 guarantees.
//!
//! The invariant: no test needs a window, and none fails for want of a GPU.
//! On a machine without an adapter the GPU tests say so and pass.

use std::cell::Ref;
use std::time::Duration;

use super::blit::{SHADER_SRC, framebuffer_format, viewport};
use super::surface::window_format;
use super::*;
use crate::gui::meter::Meter;
use crate::gui::overlay::Overlay;
use crate::gui::overlay::testing::{SCREEN, input};
use crate::wall_clock::HostClock;
use emu8086_core::io::keyboard::KeyboardController;
use emu8086_core::io::raster::BlinkPhase;
use emu8086_core::io::vga::VgaDevice;
use emu8086_core::vm::runtime::Runtime;

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
        viewport(
            DEFAULT_WINDOW_WIDTH,
            DEFAULT_WINDOW_HEIGHT,
            TEXT_WIDTH,
            TEXT_HEIGHT
        ),
        filled
    );
    assert_eq!(
        viewport(
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

/// A device is asked for wgpu's default limits where its adapter has them,
/// however much more it offers. An adapter that has not is asked for what
/// WebGL2 guarantees, with textures as large as it makes them, and what is
/// asked for is always on offer.
#[test]
fn a_device_is_asked_for_limits_its_adapter_has() {
    let defaults = wgpu::Limits::default();
    assert_eq!(required_limits(&defaults), defaults);
    let generous = wgpu::Limits {
        max_texture_dimension_2d: 32768,
        max_bind_groups: 8,
        ..defaults.clone()
    };
    assert_eq!(required_limits(&generous), defaults);

    // WebGL2 as a browser offers it: its guarantees, with larger textures.
    let webgl2 = wgpu::Limits {
        max_texture_dimension_1d: 4096,
        max_texture_dimension_2d: 4096,
        ..wgpu::Limits::downlevel_webgl2_defaults()
    };
    assert!(!defaults.check_limits(&webgl2), "it has not the defaults");
    let asked = required_limits(&webgl2);
    assert_eq!(asked.max_texture_dimension_2d, 4096);
    assert_eq!(asked, webgl2);
    assert!(asked.check_limits(&webgl2));
}

/// A frame fills a target that is a whole multiple of it, and a target of
/// another shape as far as the frame's shape allows, the rest of the target
/// shared between the two borders.
#[test]
fn the_frame_fills_the_target_as_far_as_its_shape_allows() {
    // Three times the mode 13h frame, exactly.
    assert_eq!(viewport(960, 600, 320, 200), (0.0, 0.0, 960.0, 600.0));
    // Much wider than tall: the height allows twice, the width six times.
    assert_eq!(viewport(2000, 400, 320, 200), (680.0, 0.0, 640.0, 400.0));
    // Square: the width allows 3.125 times, which the height has room for.
    assert_eq!(viewport(1000, 1000, 320, 200), (0.0, 187.0, 1000.0, 625.0));
    // A 1280 by 720 window: the height allows 1.8 times the text frame.
    assert_eq!(viewport(1280, 720, 640, 400), (64.0, 0.0, 1152.0, 720.0));
}

/// A target smaller than the frame shows the whole of it, shrunk to fit, a
/// whole number of pixels high and wide.
#[test]
fn a_frame_larger_than_the_target_is_shrunk_to_fit() {
    // The width allows 0.625 of the mode 13h frame, the height 0.75.
    assert_eq!(viewport(200, 150, 320, 200), (0.0, 12.0, 200.0, 125.0));
    // Half the text frame, with a border of 10 pixels left and right.
    assert_eq!(viewport(340, 200, 640, 400), (10.0, 0.0, 320.0, 200.0));
    // A phone held upright: the width allows 0.609 of the text frame,
    // 243.75 pixels high, which is 244 whole ones.
    assert_eq!(viewport(390, 844, 640, 400), (0.0, 300.0, 390.0, 244.0));
}

/// A frame of no size divides nothing by zero, and the viewport is never
/// of no size, on a target of one pixel either: the shader divides by it.
#[test]
fn viewport_handles_zero_frame_dimensions() {
    let (_, _, w, h) = viewport(960, 600, 0, 0);
    assert!(w.is_finite() && h.is_finite());
    assert_eq!(viewport(1, 1, 320, 200), (0.0, 0.0, 1.0, 1.0));
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
    gpu_asked_for(required_limits)
}

/// As `gpu`, but the device is asked for the limits `limits` makes of those
/// the adapter offers.
fn gpu_asked_for(
    limits: impl Fn(&wgpu::Limits) -> wgpu::Limits,
) -> Option<(wgpu::Device, wgpu::Queue)> {
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
    let descriptor = wgpu::DeviceDescriptor {
        required_limits: limits(&adapter.limits()),
        ..device_descriptor(&adapter)
    };
    match pollster::block_on(adapter.request_device(&descriptor)) {
        Ok(device_and_queue) => Some(device_and_queue),
        Err(error) => {
            eprintln!("skipped: the adapter gives no device: {error}");
            None
        }
    }
}

/// The limits a device is asked for when its adapter is WebGL2, made of
/// what any adapter offers: what WebGL2 guarantees and no more, with
/// textures as large as this adapter makes them.
fn as_on_webgl2(offered: &wgpu::Limits) -> wgpu::Limits {
    let webgl2 = wgpu::Limits::downlevel_webgl2_defaults().using_resolution(offered.clone());
    required_limits(&webgl2)
}

/// A machine without disks or ROM, whose text screen shows every character
/// and every attribute, with the cursor on it. The screen is programmed as
/// a guest would: through the machine's ports.
fn text_screen() -> Runtime {
    let (_keys, key_events) = std::sync::mpsc::channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    let mut rt = Runtime::new(vec![], vec![], None, false, keyboard, &HostClock)
        .expect("a machine without disks has nothing to open");
    for cell in 0..80 * 25 {
        rt.cpu.memory.write_byte(0xB8000 + cell * 2, cell as u8);
        // 7 has no factor in common with 256: every attribute comes up.
        rt.cpu
            .memory
            .write_byte(0xB8000 + cell * 2 + 1, (cell * 7) as u8);
    }
    let cursor: u16 = 12 * 80 + 40;
    rt.port_out_byte(0x3D4, 0x0E);
    rt.port_out_byte(0x3D5, (cursor >> 8) as u8);
    rt.port_out_byte(0x3D4, 0x0F);
    rt.port_out_byte(0x3D5, cursor as u8);
    rt
}

/// The machine's VGA, as the window borrows it to draw a frame.
fn vga(rt: &Runtime) -> Ref<'_, VgaDevice> {
    rt.vga.as_ref().expect("the machine has a VGA").borrow()
}

/// Switches to mode 13h, gives every DAC entry a colour of its own and
/// paints stripes of all 256 that no flip or mirror maps onto themselves.
fn show_mode_13h_stripes(rt: &mut Runtime) {
    rt.port_out_byte(0x3CE, 0x05);
    rt.port_out_byte(0x3CF, 0x40); // 256-colour shift mode
    rt.port_out_byte(0x3C8, 0);
    for index in 0..=255u8 {
        for component in [index & 0x3F, index >> 2, 0x3F - (index & 0x3F)] {
            rt.port_out_byte(0x3C9, component);
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

/// Draws a frame into a new `width` x `height` texture of `format`, as the
/// window draws one into its surface: the blit's picture and, over it,
/// `paint` if there is one. Returns what the texture then holds: RGBA, row
/// after row.
fn draw_and_read_back(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    blit: &FrameBlit,
    overlay: &mut OverlayPass,
    mut paint: Option<Paint>,
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

    // The frame as the window records it, then the copy that reads it back.
    let mut commands = record_frame(device, queue, blit, overlay, paint.as_mut(), &view);
    let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
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
    commands.push(encoder.finish());
    queue.submit(commands);
    if let Some(paint) = &mut paint {
        overlay.release(paint);
    }

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

/// The frame pixels a target pixel covers along one axis, each with the
/// share of the target pixel it covers: pixel `k` of a target on which the
/// frame, `frame_len` pixels long on this axis, starts at `offset` and is
/// `scale` times its size. None where the frame does not reach.
fn covered(k: u32, offset: u32, scale: f64, frame_len: usize) -> Vec<(usize, f64)> {
    let Some(k) = k.checked_sub(offset) else {
        return Vec::new();
    };
    let (start, end) = (f64::from(k) / scale, f64::from(k + 1) / scale);
    let first = start.floor() as usize;
    let last = (end.ceil() as usize).min(frame_len);
    (first..last)
        .map(|texel| {
            let share = (end.min(texel as f64 + 1.0) - start.max(texel as f64)) * scale;
            (texel, share)
        })
        .filter(|(_, share)| *share > 1e-9)
        .collect()
}

/// What a `width` x `height` target is to hold of `frame`, `scale` times
/// its size with its top left corner at `offset`: each target pixel the
/// mean of the frame pixels it covers, each in the share of the target
/// pixel it covers (a box filter), taken in linear light on an sRGB
/// target, and opaque black wherever the frame does not reach. At a whole
/// multiple that is the frame pixel itself; shrunk to half, the mean of
/// four.
fn frame_fitted_on_target(
    frame: &[u8],
    (fb_width, fb_height): (usize, usize),
    (width, height): (u32, u32),
    (offset_x, offset_y): (u32, u32),
    scale: f64,
    srgb: bool,
) -> Vec<u8> {
    let decode = |byte: u8| {
        let c = f64::from(byte) / 255.0;
        if !srgb {
            c
        } else if c <= 0.04045 {
            c / 12.92
        } else {
            ((c + 0.055) / 1.055).powf(2.4)
        }
    };
    let encode = |light: f64| {
        let c = if !srgb {
            light
        } else if light <= 0.003_130_8 {
            light * 12.92
        } else {
            1.055 * light.powf(1.0 / 2.4) - 0.055
        };
        (c * 255.0).round().clamp(0.0, 255.0) as u8
    };
    let mut expected = Vec::with_capacity((width * height * 4) as usize);
    for y in 0..height {
        let rows = covered(y, offset_y, scale, fb_height);
        for x in 0..width {
            let columns = covered(x, offset_x, scale, fb_width);
            if rows.is_empty() || columns.is_empty() {
                expected.extend_from_slice(&[0, 0, 0, 255]);
                continue;
            }
            let shares: f64 = rows
                .iter()
                .flat_map(|(_, row)| columns.iter().map(move |(_, column)| row * column))
                .sum();
            for channel in 0..3 {
                let mean = rows
                    .iter()
                    .flat_map(|(frame_y, row)| {
                        columns.iter().map(move |(frame_x, column)| {
                            decode(frame[(frame_y * fb_width + frame_x) * 4 + channel])
                                * row
                                * column
                        })
                    })
                    .sum::<f64>()
                    / shares;
                expected.push(encode(mean));
            }
            expected.push(255);
        }
    }
    expected
}

/// Fails with the first pixel that differs and the number that do, rather
/// than with two images of a megabyte each.
fn assert_same_pixels(drawn: &[u8], expected: &[u8], width: u32, what: &str) {
    assert_close_pixels(drawn, expected, width, 0, what);
}

/// As `assert_same_pixels`, but a channel may differ by up to `tolerance`.
fn assert_close_pixels(drawn: &[u8], expected: &[u8], width: u32, tolerance: u8, what: &str) {
    assert_eq!(drawn.len(), expected.len(), "{what}: image size");
    let mut differing =
        drawn
            .chunks(4)
            .zip(expected.chunks(4))
            .enumerate()
            .filter(|(_, (drawn, expected))| {
                drawn
                    .iter()
                    .zip(*expected)
                    .any(|(a, b)| a.abs_diff(*b) > tolerance)
            });
    if let Some((index, (drawn, expected))) = differing.next() {
        let (x, y) = (index as u32 % width, index as u32 / width);
        panic!(
            "{what}: {} pixels differ, the first at {x},{y}: drawn {drawn:?}, expected {expected:?}",
            differing.count() + 1
        );
    }
}

/// The pipeline puts the frame `raster` drew on its target: on a target
/// that is a whole multiple of the frame but for a border, byte for byte,
/// every frame pixel a square of whole target pixels; the picture sits in
/// the middle, the rest of the target is opaque black, and the colours are
/// those of the frame whether the target is sRGB or not. A target smaller
/// than the frame shows the whole of it, shrunk: each target pixel the mean
/// of the frame pixels it covers. A change of video mode on the same blit
/// swaps the frame texture for one of the new size.
#[test]
fn blit_draws_the_rasterised_frame_scaled_and_centred() {
    blit_puts_the_frame_on_its_target(gpu());
}

/// Draws text and mode 13h frames on `gpu` and compares every pixel read
/// back with the frame `raster` drew. Without a GPU there is nothing to
/// compare.
fn blit_puts_the_frame_on_its_target(gpu: Option<(wgpu::Device, wgpu::Queue)>) {
    let Some((device, queue)) = gpu else {
        return;
    };
    const TEXT: (usize, usize) = (raster::TEXT_WIDTH, raster::TEXT_HEIGHT);
    const GFX: (usize, usize) = (raster::GFX_WIDTH, raster::GFX_HEIGHT);

    for format in [
        wgpu::TextureFormat::Rgba8UnormSrgb,
        wgpu::TextureFormat::Rgba8Unorm,
    ] {
        let mut rt = text_screen();
        let mut text_frame = vec![0; TEXT.0 * TEXT.1 * 4];
        raster::rasterize_text(&mut text_frame, &vga(&rt), &rt.cpu.memory, ALL_ON);

        // Twice the text frame, which the height allows exactly, with a
        // border of 10 pixels left and right.
        let target = (1300, 800);
        let mut blit = FrameBlit::new(&device, format, target.0, target.1);
        // No overlay is painted here: the frames are the picture alone.
        let mut overlay = OverlayPass::new(&device, format);
        blit.upload(&device, &queue, &vga(&rt), &rt.cpu.memory, ALL_ON);
        assert_same_pixels(
            &draw_and_read_back(&device, &queue, &blit, &mut overlay, None, format, target),
            &frame_on_target(&text_frame, TEXT, target, 2, (10, 0)),
            target.0,
            &format!("text frame doubled, {format:?}"),
        );

        // The text frame at 1.8 times, as a 1280 by 720 window shows it,
        // but for a border of 32 pixels left and right. Every so often the
        // seam between two frame pixels crosses a target pixel, which takes
        // the two in the shares they cover; everywhere else a target pixel
        // is one frame pixel, whole. An sRGB target blends in linear light,
        // and the GPU's filtering and encoding round their own way, a step
        // or two.
        let target = (1216, 720);
        blit.resize(&queue, target.0, target.1);
        assert_close_pixels(
            &draw_and_read_back(&device, &queue, &blit, &mut overlay, None, format, target),
            &frame_fitted_on_target(&text_frame, TEXT, target, (32, 0), 1.8, format.is_srgb()),
            target.0,
            2,
            &format!("text frame at 1.8 times, {format:?}"),
        );

        // A target smaller than the frame shows the whole of it, shrunk:
        // here to half, with a border of 10 pixels left and right. Each
        // target pixel is then the mean of four.
        let target = (340, 200);
        blit.resize(&queue, target.0, target.1);
        assert_close_pixels(
            &draw_and_read_back(&device, &queue, &blit, &mut overlay, None, format, target),
            &frame_fitted_on_target(&text_frame, TEXT, target, (10, 0), 0.5, format.is_srgb()),
            target.0,
            2,
            &format!("text frame halved, {format:?}"),
        );

        // Mode 13h at three times its size, which the height allows
        // exactly. The border is an odd number of pixels (41), so the
        // picture sits half a pixel off centre: 20 pixels from the left.
        show_mode_13h_stripes(&mut rt);
        let mut gfx_frame = vec![0; GFX.0 * GFX.1 * 4];
        raster::rasterize_mode13h(&mut gfx_frame, &vga(&rt), &rt.cpu.memory);
        let target = (1001, 600);
        blit.resize(&queue, target.0, target.1);
        blit.upload(&device, &queue, &vga(&rt), &rt.cpu.memory, ALL_ON);
        assert_same_pixels(
            &draw_and_read_back(&device, &queue, &blit, &mut overlay, None, format, target),
            &frame_on_target(&gfx_frame, GFX, target, 3, (20, 0)),
            target.0,
            &format!("mode 13h frame tripled, {format:?}"),
        );
    }
}

/// The pixel at `x`, `y` of an image `width` pixels wide.
fn pixel(image: &[u8], width: u32, x: u32, y: u32) -> [u8; 4] {
    let at = ((y * width + x) * 4) as usize;
    [image[at], image[at + 1], image[at + 2], image[at + 3]]
}

/// The pixels of the `columns` x `rows` rectangle in which `drawn` and
/// `picture`, both `width` pixels wide, differ.
fn differing(
    drawn: &[u8],
    picture: &[u8],
    width: u32,
    columns: std::ops::Range<u32>,
    rows: std::ops::Range<u32>,
) -> usize {
    rows.flat_map(|y| columns.clone().map(move |x| (x, y)))
        .filter(|&(x, y)| pixel(drawn, width, x, y) != pixel(picture, width, x, y))
        .count()
}

/// The overlay is drawn over the picture, in the same frame and only where
/// it is. While the hint shows, the frame is the picture but for the hint's
/// corner. Hidden, the overlay leaves nothing to paint and the frame is the
/// picture exactly. Shown, its menu bar covers the top of the target in the
/// colour of the style, and all that lies below the bar is still the
/// picture, pixel for pixel.
#[test]
fn the_overlay_is_drawn_over_the_picture_and_only_where_it_is() {
    overlay_lies_over_the_picture(gpu());
}

/// What WebGL2 guarantees is enough for the frame. A device that was asked
/// for no more refuses whatever exceeds it, whichever adapter is under it:
/// on such a device the picture and the overlay are drawn as on any other.
#[test]
fn the_frame_is_drawn_within_the_limits_of_webgl2() {
    let gpu = gpu_asked_for(as_on_webgl2);
    if let Some((device, _)) = &gpu {
        let limits = device.limits();
        assert_eq!(
            limits.max_storage_buffers_per_shader_stage, 0,
            "WebGL2 has no storage buffers: {limits:?}"
        );
    }
    blit_puts_the_frame_on_its_target(gpu.clone());
    overlay_lies_over_the_picture(gpu);
}

/// Draws the picture with the overlay over it on `gpu`, in each state of
/// the overlay, and compares what is read back with the picture alone.
/// Without a GPU there is nothing to compare.
fn overlay_lies_over_the_picture(gpu: Option<(wgpu::Device, wgpu::Queue)>) {
    let Some((device, queue)) = gpu else {
        return;
    };
    const TEXT: (usize, usize) = (raster::TEXT_WIDTH, raster::TEXT_HEIGHT);
    // The text frame at its own size fills the window the overlay's tests
    // pretend to have.
    let target = (SCREEN[0], SCREEN[1]);
    let (width, height) = target;
    assert_eq!(target, (TEXT_WIDTH, TEXT_HEIGHT));

    for format in [
        wgpu::TextureFormat::Rgba8UnormSrgb,
        wgpu::TextureFormat::Rgba8Unorm,
    ] {
        let mut rt = text_screen();
        let mut picture = vec![0; TEXT.0 * TEXT.1 * 4];
        raster::rasterize_text(&mut picture, &vga(&rt), &rt.cpu.memory, ALL_ON);
        let mut blit = FrameBlit::new(&device, format, width, height);
        blit.upload(&device, &queue, &vga(&rt), &rt.cpu.memory, ALL_ON);
        let mut pass = OverlayPass::new(&device, format);
        let mut overlay = Overlay::on(false);

        // A frame as the window draws it: the overlay is painted only if it
        // has anything to show.
        let mut frame = |overlay: &mut Overlay| {
            let paint = overlay.draws().then(|| overlay.take_paint());
            draw_and_read_back(&device, &queue, &blit, &mut pass, paint, format, target)
        };

        // A pass of the overlay at input time `time`, with no events, over
        // the machine whose picture this is, running.
        let meter = Meter::default();
        let mut run = |overlay: &mut Overlay, time: f64| {
            overlay.run(input(time, Vec::new()), &mut rt, false, None, &meter);
        };

        // The first seconds: the hint, in the bottom left corner. It is
        // laid out in one pass, shown from the next and fades in.
        for time in [0.0, 1.0, 2.0] {
            run(&mut overlay, time);
        }
        let drawn = frame(&mut overlay);
        assert!(
            differing(&drawn, &picture, width, 0..200, height - 60..height) > 100,
            "the hint is drawn, {format:?}"
        );
        assert_eq!(
            differing(&drawn, &picture, width, 0..width, 0..height - 60)
                + differing(&drawn, &picture, width, 200..width, height - 60..height),
            0,
            "and nothing but the hint, {format:?}"
        );

        // Hidden and past the hint: nothing of the overlay is drawn.
        overlay.toggle();
        overlay.toggle();
        assert!(!overlay.draws());
        assert_same_pixels(
            &frame(&mut overlay),
            &picture,
            width,
            &format!("hidden overlay, {format:?}"),
        );

        // Shown: the menu bar along the top.
        overlay.toggle();
        run(&mut overlay, 3.0);
        run(&mut overlay, 3.1);
        let bar = overlay
            .context()
            .global_style()
            .visuals
            .panel_fill
            .to_array();
        let drawn = frame(&mut overlay);
        // Inside the bar, right of its menus and left of the guest's speed
        // at its right end.
        for (x, y) in [(300, 3), (width / 2, 8), (width - 200, 6)] {
            let drawn = pixel(&drawn, width, x, y);
            // An sRGB target takes the colour decoded and stores it encoded
            // again: one step of rounding either way.
            let close = drawn.iter().zip(&bar).all(|(a, b)| a.abs_diff(*b) <= 2);
            assert!(
                close,
                "{x},{y} is {drawn:?}, the bar is {bar:?}, {format:?}"
            );
        }
        assert_eq!(
            differing(&drawn, &picture, width, 0..width, 40..height),
            0,
            "below the bar the picture is untouched, {format:?}"
        );
    }
}

/// The window is drawn in a plain 8-bit format when the surface has one,
/// wherever it stands in the surface's list, and in the first format of
/// the list when it has none.
#[test]
fn window_format_is_a_plain_one_when_the_surface_has_it() {
    use wgpu::TextureFormat::*;
    assert_eq!(window_format(&[Bgra8UnormSrgb, Bgra8Unorm]), Bgra8Unorm);
    assert_eq!(
        window_format(&[Rgba8UnormSrgb, Rgba16Float, Rgba8Unorm]),
        Rgba8Unorm
    );
    assert_eq!(
        window_format(&[Bgra8UnormSrgb, Rgba16Float]),
        Bgra8UnormSrgb
    );
}
