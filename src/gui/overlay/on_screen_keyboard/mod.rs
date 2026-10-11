//! The On-screen keyboard panel: the PC/XT keyboard drawn on the screen,
//! for a user who has no keyboard at hand, as on a phone. Its keys are
//! those of an XT printed for the US (`layout`, THE key table), and each
//! sends what the physical key in its place sends (`gui::keys`): its make
//! code going down and its break coming up, so the guest's layout decides
//! what it types. A finger or the primary button of the pointer presses a
//! key; each finger holds its own, so that several keys are held at once.
//! A key held repeats as an XT's does, Shift, Ctrl and Alt latch when
//! tapped alone (`presses`), and the lock keys are lit from the BIOS's
//! keyboard flags, read from guest memory.
//!
//! The panel presses no key itself: it asks the app (`Command::Key`), which
//! sends the codes through the guest's keyboard (`gui::keys::GuestKeys`).
//!
//! The invariants:
//!
//! - No key takes the keyboard's focus. A widget with the focus gives the
//!   overlay the keyboard, and the app then lets go of every key the guest
//!   holds, the chord a key is part of with it.
//! - Every event of a frame counts once. egui may run the panel twice for
//!   one frame; the second run is given no events.
//! - What the panel holds is the screen's only while the panel is drawn. A
//!   pass that does not draw it holds none of its keys, which the app lets
//!   go of, and the panel drawn again remembers nothing from before.

mod layout;
mod presses;

use std::sync::Arc;

use egui::{
    Color32, Context, Event, FontId, Galley, LayerId, PointerButton, Pos2, Rect, Response, Sense,
    StrokeKind, TouchPhase, Ui, Vec2, WidgetInfo, WidgetType, pos2, vec2,
};

use super::theme::{METRICS, PALETTE};
use super::{Command, Cx, keyboard_taken, with_echoes};
use presses::{Holder, Presses};

/// Where the BIOS keeps its keyboard flags, the state of the lock keys
/// among them: 0040:0017h (`BDA_KBD_FLAGS1` in assets/rom/bda.inc).
const BIOS_KEYBOARD_FLAGS: usize = 0x0417;

/// How tall a legend is, as a share of the smaller side of a key that
/// types a letter. A legend that would not fit its key is made smaller.
const LEGEND_SHARE: f32 = 0.42;

/// What the panel keeps from pass to pass, in egui's memory.
#[derive(Clone, Default)]
struct State {
    /// Where the last pass drew the keys: the cell of each, by make code.
    cells: Vec<(u8, Rect)>,
    /// The layer it drew them on: the panel's window.
    layer: Option<LayerId>,
    presses: Presses,
}

impl State {
    /// The key that a finger or the pointer coming down at `at` lands on,
    /// as the last pass drew the keyboard: the one whose cell holds `at`,
    /// unless something else of the overlay lies over the panel there.
    fn key_at(&self, ctx: &Context, at: Pos2) -> Option<u8> {
        if ctx.layer_id_at(at) != Some(self.layer?) {
            return None;
        }
        self.cells
            .iter()
            .find(|(_, cell)| cell.contains(at))
            .map(|(make, _)| *make)
    }
}

/// How a key looks.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Look {
    Up,
    /// Held down by a finger or the pointer.
    Held,
    /// A modifier latched.
    Latched,
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    let id = ui.make_persistent_id("on_screen_keyboard");
    // A pass that did not draw the panel held none of its keys, and the
    // app let go of them: what the screen showed before that is gone.
    let mut state = if cx.drawn_before {
        ui.data(|data| data.get_temp::<State>(id))
            .unwrap_or_default()
    } else {
        State::default()
    };

    let (now, events) = ui.input(|input| (input.time, input.events.clone()));
    // While the overlay has the keyboard the keys are its own: nothing
    // holds a key of the guest from here, and nothing presses one. Nor
    // once the window has lost the keyboard, whose releases go elsewhere.
    if keyboard_taken(ui.ctx()) || events.contains(&Event::WindowFocused(false)) {
        state.presses.let_go(cx.commands);
    } else {
        take_events(&mut state, &events, ui.ctx(), now, cx.commands);
        state.presses.repeat(now, cx.commands);
    }

    let flags = cx.runtime.cpu.memory.read_byte(BIOS_KEYBOARD_FLAGS);
    draw(ui, &mut state, flags);
    ui.data_mut(|data| data.insert_temp(id, state));
}

/// Takes up the events of the frame, in their order, at input time `now`.
/// A finger that comes down on a key presses it, and so does the primary
/// button of the pointer; the key comes up when that finger lifts or the
/// host takes it away, or when that button is released, wherever the
/// finger or the pointer is by then. A press or a release of the button
/// that is a finger's echo (`with_echoes`) does nothing here: the finger's
/// touch did it.
fn take_events(
    state: &mut State,
    events: &[Event],
    ctx: &Context,
    now: f64,
    commands: &mut Vec<Command>,
) {
    for (event, echo) in with_echoes(events) {
        match event {
            Event::Touch {
                device_id,
                id,
                phase,
                pos,
                ..
            } => {
                let finger = Holder::Finger(*device_id, *id);
                match phase {
                    TouchPhase::Start => {
                        if let Some(make) = state.key_at(ctx, *pos) {
                            state.presses.press(finger, make, now, commands);
                        }
                    }
                    TouchPhase::Move => {}
                    TouchPhase::End | TouchPhase::Cancel => {
                        state.presses.release(finger, commands);
                    }
                }
            }
            Event::PointerButton {
                pos,
                button: PointerButton::Primary,
                pressed,
                ..
            } if !echo => {
                if !pressed {
                    state.presses.release(Holder::Pointer, commands);
                } else if let Some(make) = state.key_at(ctx, *pos) {
                    state.presses.press(Holder::Pointer, make, now, commands);
                }
            }
            _ => {}
        }
    }
}

/// The width of a key that types a letter and the height of a row, in
/// points: the keyboard takes the width the screen leaves a window, and its
/// rows half the screen's height, each between the smallest and the
/// largest a key is in the theme.
fn key_size(ui: &Ui) -> (f32, f32) {
    let screen = ui.ctx().content_rect().size();
    // The window's frame around the keys: its margins and its outline, and
    // a point to spare for rounding.
    let stroke = ui.visuals().window_stroke.width;
    let frame = ui.style().spacing.window_margin.sum() + Vec2::splat(2.0 * stroke + 1.0);
    let unit = (screen.x - frame.x) / layout::KEYS_ACROSS;
    let height = screen.y / 2.0 / layout::ROWS.len() as f32;
    let (smallest, largest) = (METRICS.key_smallest, METRICS.key_largest);
    (
        unit.clamp(smallest, largest),
        height.clamp(smallest, largest),
    )
}

/// Lays the keys out for the screen as it is, draws each as it is, tells a
/// screen reader of each as a button, and keeps where each lies for the
/// next pass. `flags` are the BIOS's keyboard flags, which light the lock
/// keys.
fn draw(ui: &mut Ui, state: &mut State, flags: u8) {
    let (unit, height) = key_size(ui);
    let size = vec2(
        unit * layout::KEYS_ACROSS,
        height * layout::ROWS.len() as f32,
    );
    let (area, _) = ui.allocate_exact_size(size, Sense::hover());
    let legend_size = unit.min(height) * LEGEND_SHARE;
    state.cells = layout::cells(area.min, unit, height);
    for &(make, cell) in &state.cells {
        let Some((name, legend)) = layout::label(make) else {
            continue;
        };
        let key = cell.shrink(METRICS.key_gap / 2.0);
        // Not focusable, and a drag that starts on a key is the key's: the
        // window under it is not moved.
        let response = ui.interact(key, ui.id().with(make), Sense::CLICK | Sense::DRAG);
        let lit = layout::lock_bit(make).map(|bit| flags & bit != 0);
        let look = if state.presses.is_latched(make) {
            Look::Latched
        } else if state.presses.is_held(make) {
            Look::Held
        } else {
            Look::Up
        };
        // A lock key is pressed, to a screen reader, while its lock is on;
        // any other key while it is down.
        let pressed = lit.unwrap_or(look != Look::Up);
        response.widget_info(|| WidgetInfo::selected(WidgetType::Button, true, pressed, name));
        paint(ui, &response, key, legend, look, lit, legend_size);
    }
    state.layer = Some(ui.layer_id());
}

/// Paints `key` with its `legend`, as it looks: a button of the theme while
/// it is up, the colour of a key held down or of a latched modifier
/// otherwise. A lock key has a light in its top right corner, on while
/// `lit`, and its legend in the room below the light.
fn paint(
    ui: &Ui,
    response: &Response,
    key: Rect,
    legend: &str,
    look: Look,
    lit: Option<bool>,
    legend_size: f32,
) {
    let visuals = ui.style().interact(response);
    let (fill, text) = match look {
        Look::Up => (visuals.weak_bg_fill, visuals.fg_stroke.color),
        Look::Held => (PALETTE.key_held, PALETTE.text_strong),
        Look::Latched => (PALETTE.key_latched, PALETTE.text_strong),
    };
    let painter = ui.painter();
    painter.rect(
        key,
        METRICS.widget_corner,
        fill,
        visuals.bg_stroke,
        StrokeKind::Inside,
    );

    let mut room = key.shrink(METRICS.key_gap);
    if let Some(lit) = lit {
        let corner = pos2(room.right() - METRICS.key_light.x, room.top());
        let light = if lit { PALETTE.key_lit } else { PALETTE.off };
        painter.rect_filled(
            Rect::from_min_size(corner, METRICS.key_light),
            METRICS.widget_corner,
            light,
        );
        room.min.y += METRICS.key_light.y + METRICS.key_gap;
    }

    let lines = legend_lines(ui, legend, legend_size, room.size(), text);
    let mut top = room.center().y - lines.iter().map(|line| line.size().y).sum::<f32>() / 2.0;
    for line in lines {
        let size = line.size();
        painter.galley(pos2(room.center().x - size.x / 2.0, top), line, text);
        top += size.y;
    }
}

/// The lines of `legend` laid out in `colour` to fit `room`: `size` points
/// tall, or less where they would not fit.
fn legend_lines(ui: &Ui, legend: &str, size: f32, room: Vec2, colour: Color32) -> Vec<Arc<Galley>> {
    let lay_out = |size: f32| -> Vec<Arc<Galley>> {
        legend
            .lines()
            .map(|line| {
                ui.painter()
                    .layout_no_wrap(line.to_owned(), FontId::proportional(size), colour)
            })
            .collect()
    };
    let lines = lay_out(size);
    let width = lines.iter().map(|line| line.size().x).fold(0.0, f32::max);
    let height: f32 = lines.iter().map(|line| line.size().y).sum();
    let fit = (room.x / width).min(room.y / height);
    if fit < 1.0 {
        lay_out(size * fit)
    } else {
        lines
    }
}

#[cfg(test)]
mod tests;
