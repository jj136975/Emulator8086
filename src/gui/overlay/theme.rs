//! The overlay's look: the colours (`PALETTE`) and the sizes (`METRICS`) as
//! named slots, and `apply`, the one place where they are put onto egui's
//! style. Nothing else in the overlay chooses a colour: it names a slot
//! (`PALETTE.error`), and what egui draws by itself follows the style.
//!
//! The invariant: this is the only file under `src/gui/overlay/` that spells
//! a colour. A colour built from numbers anywhere else, or one of
//! `Color32`'s named constants, would not follow a change made here; the
//! test beside this file reads the sources of the overlay and fails on one.

use egui::{Color32, CornerRadius, Margin, Shadow, Stroke, Vec2, vec2};

/// The colours of the overlay, each named for what it is used for.
pub struct Palette {
    /// Behind all the overlay draws: the menu bar, the windows, the menus.
    pub surface: Color32,
    /// Behind what is typed into or scrolled through, darker than the
    /// surface it lies in.
    pub well: Color32,
    /// Every other row of a striped table, barely apart from the surface.
    pub stripe: Color32,
    /// Outlines of windows and menus, and separators.
    pub outline: Color32,
    /// What a window or a menu casts on the picture below it.
    pub shadow: Color32,
    /// Running text.
    pub text: Color32,
    /// Text that stands out: a heading, the widget under the pointer.
    pub text_strong: Color32,
    /// A button or a checkbox at rest.
    pub widget: Color32,
    /// The widget under the pointer.
    pub widget_hovered: Color32,
    /// The widget being pressed.
    pub widget_pressed: Color32,
    /// What is selected, and the menu that is open.
    pub accent: Color32,
    /// A link.
    pub link: Color32,
    /// A state the user should not overlook, such as a paused machine.
    pub warning: Color32,
    /// What failed.
    pub error: Color32,
    /// A bit that is set, a line that is active: an interrupt requested or
    /// in service, an output or a gate that is high.
    pub on: Color32,
    /// A bit that is clear, a line at rest: quieter than running text.
    pub off: Color32,
    /// A line held back whatever it asks: an interrupt that is masked.
    pub masked: Color32,
}

/// The sizes of the overlay, in points.
pub struct Metrics {
    /// Between two widgets, across and down.
    pub item_gap: Vec2,
    /// Around the text of a button.
    pub button_padding: Vec2,
    /// Between the edge of a window and its contents.
    pub window_margin: i8,
    /// Between the edge of a menu and its entries.
    pub menu_margin: i8,
    /// Corner radius of windows and menus.
    pub corner: u8,
    /// Corner radius of buttons and checkboxes.
    pub widget_corner: u8,
    /// Width of outlines and separators.
    pub line: f32,
    /// Width of the strokes that mark the widget under the pointer or
    /// being pressed, and of the text cursor.
    pub strong_line: f32,
    /// From the edge of the window to what floats over the picture: the
    /// hint and the messages.
    pub edge: f32,
    /// Offset and blur of the shadow under windows and menus.
    pub shadow_offset: [i8; 2],
    pub shadow_blur: u8,
}

pub const PALETTE: Palette = Palette {
    surface: Color32::from_rgb(30, 32, 38),
    well: Color32::from_rgb(18, 19, 23),
    stripe: Color32::from_rgb(38, 41, 49),
    outline: Color32::from_rgb(72, 77, 90),
    shadow: Color32::from_black_alpha(110),
    text: Color32::from_rgb(200, 204, 212),
    text_strong: Color32::from_rgb(245, 246, 250),
    widget: Color32::from_rgb(52, 56, 67),
    widget_hovered: Color32::from_rgb(68, 74, 89),
    widget_pressed: Color32::from_rgb(43, 46, 56),
    accent: Color32::from_rgb(56, 104, 184),
    link: Color32::from_rgb(110, 170, 255),
    warning: Color32::from_rgb(240, 170, 60),
    error: Color32::from_rgb(240, 96, 96),
    on: Color32::from_rgb(112, 204, 132),
    off: Color32::from_rgb(104, 110, 124),
    masked: Color32::from_rgb(196, 136, 96),
};

pub const METRICS: Metrics = Metrics {
    item_gap: vec2(8.0, 4.0),
    button_padding: vec2(6.0, 2.0),
    window_margin: 8,
    menu_margin: 6,
    corner: 5,
    widget_corner: 3,
    line: 1.0,
    strong_line: 1.5,
    edge: 12.0,
    shadow_offset: [4, 8],
    shadow_blur: 12,
};

/// Puts the theme on `ctx`. egui keeps one style for a dark host and one
/// for a light one and picks by the host's setting: both become this style,
/// so the overlay looks the same whatever the host is set to.
pub fn apply(ctx: &egui::Context) {
    ctx.set_theme(egui::Theme::Dark);
    ctx.all_styles_mut(style);
}

/// Writes the slots into every part of `style` that carries a colour or one
/// of the sizes above. What is left alone is egui's dark default.
fn style(style: &mut egui::Style) {
    let palette = &PALETTE;
    let metrics = &METRICS;
    let outline = Stroke::new(metrics.line, palette.outline);
    let shadow = Shadow {
        offset: metrics.shadow_offset,
        blur: metrics.shadow_blur,
        spread: 0,
        color: palette.shadow,
    };
    // One state of a widget: its fill, its outline and the colour of its
    // text and marks.
    let widget = |fill: Color32, outline: Stroke, mark: Stroke| egui::style::WidgetVisuals {
        bg_fill: fill,
        weak_bg_fill: fill,
        bg_stroke: outline,
        corner_radius: CornerRadius::same(metrics.widget_corner),
        fg_stroke: mark,
        expansion: 0.0,
    };

    style.spacing.item_spacing = metrics.item_gap;
    style.spacing.button_padding = metrics.button_padding;
    style.spacing.window_margin = Margin::same(metrics.window_margin);
    style.spacing.menu_margin = Margin::same(metrics.menu_margin);

    let mut visuals = egui::Visuals::dark();
    visuals.panel_fill = palette.surface;
    visuals.window_fill = palette.surface;
    visuals.window_stroke = outline;
    visuals.window_corner_radius = CornerRadius::same(metrics.corner);
    visuals.menu_corner_radius = CornerRadius::same(metrics.corner);
    visuals.window_shadow = shadow;
    visuals.popup_shadow = shadow;
    visuals.extreme_bg_color = palette.well;
    visuals.code_bg_color = palette.well;
    visuals.faint_bg_color = palette.stripe;
    visuals.hyperlink_color = palette.link;
    visuals.warn_fg_color = palette.warning;
    visuals.error_fg_color = palette.error;
    visuals.selection.bg_fill = palette.accent;
    visuals.selection.stroke = Stroke::new(metrics.line, palette.text_strong);
    visuals.text_cursor.stroke = Stroke::new(metrics.strong_line, palette.text_strong);
    visuals.ime_composition.active_underline_stroke =
        Stroke::new(metrics.strong_line, palette.text_strong);
    visuals.ime_composition.inactive_underline_stroke =
        Stroke::new(metrics.strong_line, palette.text);

    visuals.widgets.noninteractive = widget(
        palette.surface,
        outline,
        Stroke::new(metrics.line, palette.text),
    );
    visuals.widgets.inactive = widget(
        palette.widget,
        Stroke::NONE,
        Stroke::new(metrics.line, palette.text),
    );
    visuals.widgets.hovered = widget(
        palette.widget_hovered,
        outline,
        Stroke::new(metrics.strong_line, palette.text_strong),
    );
    visuals.widgets.active = widget(
        palette.widget_pressed,
        Stroke::new(metrics.line, palette.text_strong),
        Stroke::new(metrics.strong_line, palette.text_strong),
    );
    visuals.widgets.open = widget(
        palette.accent,
        outline,
        Stroke::new(metrics.line, palette.text_strong),
    );
    style.visuals = visuals;
}

#[cfg(test)]
#[path = "theme_tests.rs"]
mod tests;
