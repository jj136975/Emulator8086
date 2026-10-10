//! A window's events as egui's input, written out by hand for the host on
//! which egui-winit does not build: a browser. (egui-winit 0.36.2 does not
//! compile for wasm32: it implements `egui::DroppedFile::bytes`, which egui
//! declares only off wasm32.) `EguiEvents` does there what
//! `egui_winit::State` does on a desktop, for what the overlay uses: the
//! pointer with its buttons and its wheel, the fingers on a touch screen,
//! the keys with their modifiers and the text they type, the focus, and the
//! cursor a pass asks for. It knows winit's events and egui's input and
//! nothing of a browser, which is what lets the tests drive it on a desktop.
//!
//! A finger is a touch of egui's, and the first finger is the pointer as
//! well, as egui-winit makes it: it moves the pointer and presses its
//! primary button, so that a tap is a click. The pointer's events of a
//! finger follow its touch at once.
//!
//! Left out: the IME, dropped files, the clipboard, and the physical key
//! beside the logical one.
//!
//! The invariant: egui's points are the window's physical pixels divided by
//! the pixels per point of the pass, which is the window's scale factor
//! times egui's zoom. A position of the pointer or of a finger and the size
//! of the screen are converted by that one factor (`pixels_per_point`): a
//! click or a touch lands on what the picture shows under it.

use egui::{
    Event, Modifiers, MouseWheelUnit, PointerButton, Pos2, RawInput, Rect, TouchDeviceId, TouchId,
    ViewportId,
};
use winit::dpi::{PhysicalPosition, PhysicalSize};
use winit::event::{
    ElementState, Force, MouseButton, MouseScrollDelta, Touch, TouchPhase, WindowEvent,
};
use winit::keyboard::{Key, ModifiersState, NamedKey};
use winit::window::CursorIcon;

use super::Instant;

/// What a pass asks of the window's cursor.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CursorWish {
    /// Show this icon.
    Show(CursorIcon),
    /// Show no cursor.
    Hide,
}

/// Gathers a window's events as the input of egui's next pass.
pub struct EguiEvents {
    ctx: egui::Context,
    /// When egui's time began: the time of a pass is counted from here.
    started: Instant,
    /// What was gathered since the last `take`.
    input: RawInput,
    /// The modifiers held, as the window last reported them. Every key and
    /// pointer event carries a copy.
    modifiers: Modifiers,
    /// Where the pointer is, in points. `None` while it is outside the
    /// window.
    pointer: Option<Pos2>,
    /// The finger that is the pointer as well, by winit's id of it: the one
    /// that came down while no finger was.
    pointer_finger: Option<u64>,
    /// The window's scale factor: physical pixels per logical pixel.
    scale_factor: f32,
    /// The cursor the window was last asked to show.
    cursor: Option<egui::CursorIcon>,
}

impl EguiEvents {
    /// The input of the passes of `ctx`, from a window whose scale factor
    /// is `scale_factor`.
    pub fn new(ctx: egui::Context, scale_factor: f64) -> Self {
        Self {
            ctx,
            started: Instant::now(),
            // The window says when it has the keyboard.
            input: RawInput {
                focused: false,
                ..Default::default()
            },
            modifiers: Modifiers::default(),
            pointer: None,
            pointer_finger: None,
            scale_factor: scale_factor as f32,
            cursor: None,
        }
    }

    /// The longest side a texture of the overlay may have: the next pass is
    /// told.
    pub fn set_max_texture_side(&mut self, side: usize) {
        self.input.max_texture_side = Some(side);
    }

    /// Physical pixels per point of the passes.
    fn pixels_per_point(&self) -> f32 {
        self.scale_factor * self.ctx.zoom_factor()
    }

    /// Makes `event` part of the next pass's input. Returns whether egui
    /// keeps the event for itself: it has a use for the pointer where the
    /// pointer is, or one of its widgets has the keyboard.
    pub fn on_window_event(&mut self, event: &WindowEvent) -> bool {
        match event {
            WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
                self.scale_factor = *scale_factor as f32;
                false
            }
            WindowEvent::CursorMoved { position, .. } => {
                self.pointer_moved(*position);
                self.ctx.egui_is_using_pointer()
            }
            WindowEvent::CursorLeft { .. } => {
                self.pointer = None;
                self.input.events.push(Event::PointerGone);
                false
            }
            WindowEvent::MouseInput { state, button, .. } => {
                self.pointer_button(*state, *button);
                self.ctx.egui_wants_pointer_input()
            }
            WindowEvent::MouseWheel { delta, phase, .. } => {
                self.wheel(*delta, *phase);
                self.ctx.egui_wants_pointer_input()
            }
            WindowEvent::Touch(touch) => {
                self.touch(touch);
                // A finger that moves is egui's only while egui uses the
                // pointer, as a moving pointer is.
                match touch.phase {
                    TouchPhase::Started | TouchPhase::Ended | TouchPhase::Cancelled => {
                        self.ctx.egui_wants_pointer_input()
                    }
                    TouchPhase::Moved => self.ctx.egui_is_using_pointer(),
                }
            }
            WindowEvent::ModifiersChanged(modifiers) => {
                self.modifiers_changed(modifiers.state());
                false
            }
            WindowEvent::KeyboardInput {
                event,
                is_synthetic,
                ..
            } => {
                // A key the host reports as down because the window got
                // the keyboard was not pressed for this window.
                if *is_synthetic && event.state == ElementState::Pressed {
                    false
                } else {
                    self.key(&event.logical_key, event.text.as_deref(), event.state)
                }
            }
            WindowEvent::Focused(focused) => {
                self.focused(*focused);
                false
            }
            _ => false,
        }
    }

    /// The pointer is at `position`, in physical pixels from the top left
    /// corner of the window.
    fn pointer_moved(&mut self, position: PhysicalPosition<f64>) {
        let pixels_per_point = self.pixels_per_point();
        let position = egui::pos2(
            position.x as f32 / pixels_per_point,
            position.y as f32 / pixels_per_point,
        );
        self.pointer = Some(position);
        self.input.events.push(Event::PointerMoved(position));
    }

    /// A button of the pointer went down or came up where the pointer is.
    /// A button egui has no name for, or one of a pointer the window has
    /// not placed yet, is nobody's.
    fn pointer_button(&mut self, state: ElementState, button: MouseButton) {
        if let Some(pos) = self.pointer
            && let Some(button) = egui_button(button)
        {
            self.input.events.push(Event::PointerButton {
                pos,
                button,
                pressed: state == ElementState::Pressed,
                modifiers: self.modifiers,
            });
        }
    }

    /// A finger came down, moved, lifted or was taken away by the host.
    /// egui is told of every finger, by the device it is on and its own id.
    /// The finger that is the pointer moves it too, presses its primary
    /// button when it comes down and releases it when it lifts, and then
    /// the pointer is gone: a finger that has lifted hovers over nothing. A
    /// finger that comes down while no finger is the pointer becomes it; in
    /// the meantime the pointer follows any finger's moves and lift, as
    /// egui-winit has it.
    fn touch(&mut self, touch: &Touch) {
        let pixels_per_point = self.pixels_per_point();
        self.input.events.push(Event::Touch {
            device_id: TouchDeviceId(egui::epaint::util::hash(touch.device_id)),
            id: TouchId::from(touch.id),
            phase: egui_phase(touch.phase),
            pos: egui::pos2(
                touch.location.x as f32 / pixels_per_point,
                touch.location.y as f32 / pixels_per_point,
            ),
            force: touch.force.map(|force| match force {
                Force::Normalized(force) => force as f32,
                Force::Calibrated {
                    force,
                    max_possible_force,
                    ..
                } => (force / max_possible_force) as f32,
            }),
        });

        if self.pointer_finger.is_some_and(|finger| finger != touch.id) {
            return;
        }
        match touch.phase {
            TouchPhase::Started => {
                self.pointer_finger = Some(touch.id);
                self.pointer_moved(touch.location);
                self.pointer_button(ElementState::Pressed, MouseButton::Left);
            }
            TouchPhase::Moved => self.pointer_moved(touch.location),
            TouchPhase::Ended => {
                self.pointer_finger = None;
                self.pointer_button(ElementState::Released, MouseButton::Left);
                self.pointer = None;
                self.input.events.push(Event::PointerGone);
            }
            TouchPhase::Cancelled => {
                self.pointer_finger = None;
                self.pointer = None;
                self.input.events.push(Event::PointerGone);
            }
        }
    }

    /// The wheel turned: by lines, or by physical pixels.
    fn wheel(&mut self, delta: MouseScrollDelta, phase: TouchPhase) {
        let (unit, delta) = match delta {
            MouseScrollDelta::LineDelta(x, y) => (MouseWheelUnit::Line, egui::vec2(x, y)),
            MouseScrollDelta::PixelDelta(PhysicalPosition { x, y }) => (
                MouseWheelUnit::Point,
                egui::vec2(x as f32, y as f32) / self.pixels_per_point(),
            ),
        };
        self.input.events.push(Event::MouseWheel {
            unit,
            delta,
            phase: egui_phase(phase),
            modifiers: self.modifiers,
        });
    }

    /// The modifiers held are `state` from now on. Ctrl is the key of
    /// egui's commands: a page cannot tell at build time that it will run
    /// on a Mac, where it would be the command key.
    fn modifiers_changed(&mut self, state: ModifiersState) {
        self.modifiers = Modifiers {
            alt: state.alt_key(),
            ctrl: state.control_key(),
            shift: state.shift_key(),
            mac_cmd: false,
            command: state.control_key(),
        };
        self.input
            .events
            .push(Event::ModifiersChanged(self.modifiers));
    }

    /// A key went down or came up: `logical` is the key by what it means in
    /// the host's layout, `text` what it types. Returns whether egui keeps
    /// it: one of its widgets has the keyboard, or the key is Tab, which
    /// gives a widget the keyboard whether one had it or not.
    fn key(&mut self, logical: &Key, text: Option<&str>, state: ElementState) -> bool {
        let pressed = state == ElementState::Pressed;
        if let Some(key) = egui_key(logical) {
            self.input.events.push(Event::Key {
                key,
                physical_key: None,
                pressed,
                // egui works it out from the keys it knows to be down.
                repeat: false,
                modifiers: self.modifiers,
            });
        }

        // The text a press types, unless the press is a command (Ctrl+A):
        // the letter of a command is not typed.
        let is_command = self.modifiers.ctrl || self.modifiers.command;
        if pressed
            && !is_command
            && let Some(text) = text.or_else(|| logical.to_text())
            && !text.is_empty()
            && text.chars().all(is_printable)
        {
            self.input.events.push(Event::Text(text.to_owned()));
        }

        self.ctx.egui_wants_keyboard_input() || *logical == Key::Named(NamedKey::Tab)
    }

    /// The window got the keyboard, or lost it.
    fn focused(&mut self, focused: bool) {
        self.input.focused = focused;
        if !focused {
            // The releases of the modifiers held now go elsewhere.
            self.modifiers = Modifiers::default();
        }
        self.input.events.push(Event::WindowFocused(focused));
    }

    /// The input of a pass that is run now, in a window of `size` physical
    /// pixels whose scale factor is `scale_factor`: all that was gathered
    /// since the last call, with the time and the size of the screen.
    pub fn take(&mut self, size: PhysicalSize<u32>, scale_factor: f64) -> RawInput {
        self.scale_factor = scale_factor as f32;
        self.input.time = Some(self.started.elapsed().as_secs_f64());

        let size = egui::vec2(size.width as f32, size.height as f32) / self.pixels_per_point();
        // A window without a size has no screen: egui keeps its last one.
        self.input.screen_rect =
            (size.x > 0.0 && size.y > 0.0).then(|| Rect::from_min_size(Pos2::ZERO, size));
        self.input
            .viewports
            .entry(ViewportId::ROOT)
            .or_default()
            .native_pixels_per_point = Some(self.scale_factor);
        self.input.take()
    }

    /// Drops what was gathered: no pass will take it.
    pub fn discard(&mut self) {
        self.input.events.clear();
    }

    /// What the window's cursor has to become after a pass that asked for
    /// `icon`, or `None` if it is that already: a cursor set again on every
    /// frame flickers on some hosts. While the pointer is outside the
    /// window nothing is asked, and the next pass with the pointer inside
    /// asks afresh.
    pub fn cursor_wish(&mut self, icon: egui::CursorIcon) -> Option<CursorWish> {
        if self.pointer.is_none() {
            self.cursor = None;
            return None;
        }
        if self.cursor == Some(icon) {
            return None;
        }
        self.cursor = Some(icon);
        Some(match winit_cursor(icon) {
            Some(icon) => CursorWish::Show(icon),
            None => CursorWish::Hide,
        })
    }
}

/// Whether `character` is typed text. winit reports the keys that type
/// nothing (Backspace, Escape, the function keys) as control characters or
/// in Unicode's private use areas.
fn is_printable(character: char) -> bool {
    let is_private_use = ('\u{e000}'..='\u{f8ff}').contains(&character)
        || ('\u{f0000}'..='\u{ffffd}').contains(&character)
        || ('\u{100000}'..='\u{10fffd}').contains(&character);
    !is_private_use && !character.is_ascii_control()
}

fn egui_phase(phase: TouchPhase) -> egui::TouchPhase {
    match phase {
        TouchPhase::Started => egui::TouchPhase::Start,
        TouchPhase::Moved => egui::TouchPhase::Move,
        TouchPhase::Ended => egui::TouchPhase::End,
        TouchPhase::Cancelled => egui::TouchPhase::Cancel,
    }
}

fn egui_button(button: MouseButton) -> Option<PointerButton> {
    match button {
        MouseButton::Left => Some(PointerButton::Primary),
        MouseButton::Right => Some(PointerButton::Secondary),
        MouseButton::Middle => Some(PointerButton::Middle),
        MouseButton::Back => Some(PointerButton::Extra1),
        MouseButton::Forward => Some(PointerButton::Extra2),
        MouseButton::Other(_) => None,
    }
}

/// The key of egui that `key` is, if egui has one for it. A key that types
/// a character is looked up by that character, as egui names its keys.
fn egui_key(key: &Key) -> Option<egui::Key> {
    match key {
        Key::Named(named) => egui_named_key(*named),
        Key::Character(character) => egui::Key::from_name(character.as_str()),
        Key::Unidentified(_) | Key::Dead(_) => None,
    }
}

fn egui_named_key(key: NamedKey) -> Option<egui::Key> {
    use egui::Key;
    Some(match key {
        NamedKey::Enter => Key::Enter,
        NamedKey::Tab => Key::Tab,
        NamedKey::Space => Key::Space,
        NamedKey::Escape => Key::Escape,
        NamedKey::Backspace => Key::Backspace,
        NamedKey::Delete => Key::Delete,
        NamedKey::Insert => Key::Insert,
        NamedKey::ArrowDown => Key::ArrowDown,
        NamedKey::ArrowLeft => Key::ArrowLeft,
        NamedKey::ArrowRight => Key::ArrowRight,
        NamedKey::ArrowUp => Key::ArrowUp,
        NamedKey::Home => Key::Home,
        NamedKey::End => Key::End,
        NamedKey::PageUp => Key::PageUp,
        NamedKey::PageDown => Key::PageDown,
        NamedKey::Cut => Key::Cut,
        NamedKey::Copy => Key::Copy,
        NamedKey::Paste => Key::Paste,
        NamedKey::F1 => Key::F1,
        NamedKey::F2 => Key::F2,
        NamedKey::F3 => Key::F3,
        NamedKey::F4 => Key::F4,
        NamedKey::F5 => Key::F5,
        NamedKey::F6 => Key::F6,
        NamedKey::F7 => Key::F7,
        NamedKey::F8 => Key::F8,
        NamedKey::F9 => Key::F9,
        NamedKey::F10 => Key::F10,
        NamedKey::F11 => Key::F11,
        NamedKey::F12 => Key::F12,
        _ => return None,
    })
}

/// The cursor of the window that egui's `icon` is; `None` is no cursor at
/// all.
fn winit_cursor(icon: egui::CursorIcon) -> Option<CursorIcon> {
    use egui::CursorIcon as Icon;
    Some(match icon {
        Icon::None => return None,
        Icon::Default => CursorIcon::Default,
        Icon::ContextMenu => CursorIcon::ContextMenu,
        Icon::Help => CursorIcon::Help,
        Icon::PointingHand => CursorIcon::Pointer,
        Icon::Progress => CursorIcon::Progress,
        Icon::Wait => CursorIcon::Wait,
        Icon::Cell => CursorIcon::Cell,
        Icon::Crosshair => CursorIcon::Crosshair,
        Icon::Text => CursorIcon::Text,
        Icon::VerticalText => CursorIcon::VerticalText,
        Icon::Alias => CursorIcon::Alias,
        Icon::Copy => CursorIcon::Copy,
        Icon::Move => CursorIcon::Move,
        Icon::NoDrop => CursorIcon::NoDrop,
        Icon::NotAllowed => CursorIcon::NotAllowed,
        Icon::Grab => CursorIcon::Grab,
        Icon::Grabbing => CursorIcon::Grabbing,
        Icon::AllScroll => CursorIcon::AllScroll,
        Icon::ResizeHorizontal => CursorIcon::EwResize,
        Icon::ResizeNeSw => CursorIcon::NeswResize,
        Icon::ResizeNwSe => CursorIcon::NwseResize,
        Icon::ResizeVertical => CursorIcon::NsResize,
        Icon::ResizeEast => CursorIcon::EResize,
        Icon::ResizeSouthEast => CursorIcon::SeResize,
        Icon::ResizeSouth => CursorIcon::SResize,
        Icon::ResizeSouthWest => CursorIcon::SwResize,
        Icon::ResizeWest => CursorIcon::WResize,
        Icon::ResizeNorthWest => CursorIcon::NwResize,
        Icon::ResizeNorth => CursorIcon::NResize,
        Icon::ResizeNorthEast => CursorIcon::NeResize,
        Icon::ResizeColumn => CursorIcon::ColResize,
        Icon::ResizeRow => CursorIcon::RowResize,
        Icon::ZoomIn => CursorIcon::ZoomIn,
        Icon::ZoomOut => CursorIcon::ZoomOut,
    })
}

#[cfg(test)]
#[path = "egui_events_tests.rs"]
mod tests;
