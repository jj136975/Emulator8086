//! No test here opens a GPU device: they cover the viewport and texture
//! format helpers, and naga parses and validates the shader without one.

use super::*;

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
