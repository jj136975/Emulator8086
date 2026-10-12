//! `EguiEvents` on a desktop: winit's events are made by hand, as a window
//! would send them, and the tests read the input that comes out and what
//! passes of egui make of it.
//!
//! The invariant: no window. Whatever needs one (its size, its scale
//! factor) is stated by the test. A key event is the one event of winit a
//! test cannot make, so keys go in through `EguiEvents::key`, which is all
//! the arm of `on_window_event` calls.

use egui::accesskit::Role;
use winit::event::{DeviceId, Modifiers as WinitModifiers};

use super::*;

/// A window of 800 x 600 physical pixels.
const SIZE: PhysicalSize<u32> = PhysicalSize::new(800, 600);

fn events(scale_factor: f64) -> (EguiEvents, egui::Context) {
    let ctx = egui::Context::default();
    (EguiEvents::new(ctx.clone(), scale_factor), ctx)
}

fn moved(x: f64, y: f64) -> WindowEvent {
    WindowEvent::CursorMoved {
        device_id: DeviceId::dummy(),
        position: PhysicalPosition::new(x, y),
    }
}

fn button(state: ElementState, button: MouseButton) -> WindowEvent {
    WindowEvent::MouseInput {
        device_id: DeviceId::dummy(),
        state,
        button,
    }
}

fn wheel(delta: MouseScrollDelta) -> WindowEvent {
    WindowEvent::MouseWheel {
        device_id: DeviceId::dummy(),
        delta,
        phase: TouchPhase::Moved,
    }
}

fn modifiers(state: ModifiersState) -> WindowEvent {
    WindowEvent::ModifiersChanged(WinitModifiers::from(state))
}

/// Finger `id` in `phase` at `x`, `y` physical pixels, pressing with
/// `force`.
fn finger_with(id: u64, phase: TouchPhase, x: f64, y: f64, force: Option<Force>) -> WindowEvent {
    WindowEvent::Touch(Touch {
        device_id: DeviceId::dummy(),
        phase,
        location: PhysicalPosition::new(x, y),
        force,
        id,
    })
}

/// Finger `id` in `phase` at `x`, `y` physical pixels, on a screen that
/// does not tell the force.
fn finger(id: u64, phase: TouchPhase, x: f64, y: f64) -> WindowEvent {
    finger_with(id, phase, x, y, None)
}

/// What egui is told of finger `id` in `phase` at `pos`, in points.
fn touched(id: u64, phase: egui::TouchPhase, pos: Pos2) -> Event {
    Event::Touch {
        device_id: TouchDeviceId(egui::epaint::util::hash(DeviceId::dummy())),
        id: TouchId(id),
        phase,
        pos,
        force: None,
    }
}

/// The primary button of the pointer going down or coming up at `pos`.
fn primary(pos: Pos2, pressed: bool) -> Event {
    Event::PointerButton {
        pos,
        button: PointerButton::Primary,
        pressed,
        modifiers: Modifiers::NONE,
    }
}

fn named(key: NamedKey) -> Key {
    Key::Named(key)
}

fn character(text: &str) -> Key {
    Key::Character(text.into())
}

/// The events gathered since the last take.
fn taken(events: &mut EguiEvents, scale_factor: f64) -> Vec<Event> {
    events.take(SIZE, scale_factor).events
}

/// Runs a pass of `ctx` over `input`. Nothing draws here: the texture
/// changes of the pass are dropped, which egui wants to be told.
fn run(
    ctx: &egui::Context,
    input: RawInput,
    ui: impl FnMut(&mut egui::Ui),
) -> egui::PlatformOutput {
    let mut output = ctx.run_ui(input, ui);
    output.textures_delta.clear();
    output.platform_output
}

/// The pointer's position reaches egui in points: the physical pixels of
/// the window divided by its scale factor, and by egui's zoom if it has
/// one. The size of the screen is converted the same way.
#[test]
fn positions_and_the_screen_are_in_points() {
    let (mut events, ctx) = events(2.0);
    events.on_window_event(&moved(200.0, 100.0));
    let input = events.take(SIZE, 2.0);
    assert_eq!(input.events, [Event::PointerMoved(egui::pos2(100.0, 50.0))]);
    assert_eq!(
        input.screen_rect,
        Some(Rect::from_min_size(Pos2::ZERO, egui::vec2(400.0, 300.0)))
    );
    let viewport = &input.viewports[&ViewportId::ROOT];
    assert_eq!(viewport.native_pixels_per_point, Some(2.0));

    // A zoom of the UI takes effect with the pass after it is set.
    ctx.set_zoom_factor(2.0);
    run(&ctx, input, |_| {});
    run(&ctx, events.take(SIZE, 2.0), |_| {});
    assert_eq!(ctx.pixels_per_point(), 4.0);
    events.on_window_event(&moved(200.0, 100.0));
    let input = events.take(SIZE, 2.0);
    assert_eq!(input.events, [Event::PointerMoved(egui::pos2(50.0, 25.0))]);
    assert_eq!(
        input.screen_rect,
        Some(Rect::from_min_size(Pos2::ZERO, egui::vec2(200.0, 150.0)))
    );
}

/// A change of the window's scale factor moves the points with it, and a
/// window without a size has no screen to tell egui of.
#[test]
fn the_scale_factor_follows_the_window() {
    let (mut events, _ctx) = events(1.0);
    events.on_window_event(&moved(300.0, 150.0));
    assert_eq!(
        taken(&mut events, 1.0),
        [Event::PointerMoved(egui::pos2(300.0, 150.0))]
    );

    // The window says so itself at the next take: here, 1.5.
    let input = events.take(SIZE, 1.5);
    assert_eq!(
        input.viewports[&ViewportId::ROOT].native_pixels_per_point,
        Some(1.5)
    );
    events.on_window_event(&moved(300.0, 150.0));
    assert_eq!(
        taken(&mut events, 1.5),
        [Event::PointerMoved(egui::pos2(200.0, 100.0))]
    );

    let input = events.take(PhysicalSize::new(0, 0), 1.5);
    assert_eq!(input.screen_rect, None);
}

/// A button goes to egui where the pointer is, under egui's name for it. A
/// button of a pointer that was never placed, or that has left the window,
/// is dropped, and so is one egui has no name for.
#[test]
fn buttons_are_pressed_where_the_pointer_is() {
    let (mut events, _ctx) = events(1.0);
    events.on_window_event(&button(ElementState::Pressed, MouseButton::Left));
    assert_eq!(taken(&mut events, 1.0), [], "no pointer yet");

    events.on_window_event(&moved(40.0, 30.0));
    events.on_window_event(&button(ElementState::Pressed, MouseButton::Left));
    events.on_window_event(&button(ElementState::Released, MouseButton::Left));
    events.on_window_event(&button(ElementState::Pressed, MouseButton::Right));
    events.on_window_event(&button(ElementState::Pressed, MouseButton::Other(9)));
    let pos = egui::pos2(40.0, 30.0);
    let pressed = |button, pressed| Event::PointerButton {
        pos,
        button,
        pressed,
        modifiers: Modifiers::NONE,
    };
    assert_eq!(
        taken(&mut events, 1.0),
        [
            Event::PointerMoved(pos),
            pressed(PointerButton::Primary, true),
            pressed(PointerButton::Primary, false),
            pressed(PointerButton::Secondary, true),
        ]
    );

    events.on_window_event(&WindowEvent::CursorLeft {
        device_id: DeviceId::dummy(),
    });
    events.on_window_event(&button(ElementState::Pressed, MouseButton::Left));
    assert_eq!(taken(&mut events, 1.0), [Event::PointerGone]);
}

/// The wheel reaches egui in the unit the window reports it in: lines as
/// they are, physical pixels as points.
#[test]
fn the_wheel_turns_by_lines_or_by_points() {
    let (mut events, _ctx) = events(2.0);
    events.on_window_event(&wheel(MouseScrollDelta::LineDelta(0.0, -3.0)));
    events.on_window_event(&wheel(MouseScrollDelta::PixelDelta(PhysicalPosition::new(
        10.0, 40.0,
    ))));
    let turned = |unit, delta| Event::MouseWheel {
        unit,
        delta,
        phase: egui::TouchPhase::Move,
        modifiers: Modifiers::NONE,
    };
    assert_eq!(
        taken(&mut events, 2.0),
        [
            turned(MouseWheelUnit::Line, egui::vec2(0.0, -3.0)),
            turned(MouseWheelUnit::Point, egui::vec2(5.0, 20.0)),
        ]
    );
}

/// Every finger reaches egui as a touch, in points. The first finger is the
/// pointer too: it moves the pointer and presses the primary button where it
/// comes down, moves the pointer where it moves, and releases the button
/// where the pointer is when it lifts, after which the pointer is gone. A
/// finger the host takes away releases nothing: the pointer is gone.
#[test]
fn the_first_finger_is_a_touch_and_the_pointer() {
    let (mut events, _ctx) = events(2.0);
    let (down, moved) = (egui::pos2(100.0, 50.0), egui::pos2(110.0, 60.0));
    events.on_window_event(&finger(7, TouchPhase::Started, 200.0, 100.0));
    assert_eq!(
        taken(&mut events, 2.0),
        [
            touched(7, egui::TouchPhase::Start, down),
            Event::PointerMoved(down),
            primary(down, true),
        ]
    );

    events.on_window_event(&finger(7, TouchPhase::Moved, 220.0, 120.0));
    assert_eq!(
        taken(&mut events, 2.0),
        [
            touched(7, egui::TouchPhase::Move, moved),
            Event::PointerMoved(moved),
        ]
    );

    // Where the finger lifts is a touch's own: the pointer is released
    // where it last was.
    events.on_window_event(&finger(7, TouchPhase::Ended, 230.0, 130.0));
    assert_eq!(
        taken(&mut events, 2.0),
        [
            touched(7, egui::TouchPhase::End, egui::pos2(115.0, 65.0)),
            primary(moved, false),
            Event::PointerGone,
        ]
    );

    events.on_window_event(&finger(8, TouchPhase::Started, 40.0, 40.0));
    events.on_window_event(&finger(8, TouchPhase::Cancelled, 40.0, 40.0));
    let pos = egui::pos2(20.0, 20.0);
    assert_eq!(
        taken(&mut events, 2.0),
        [
            touched(8, egui::TouchPhase::Start, pos),
            Event::PointerMoved(pos),
            primary(pos, true),
            touched(8, egui::TouchPhase::Cancel, pos),
            Event::PointerGone,
        ]
    );
}

/// What no pass is run for is dropped, but for the releases of the
/// pointer's buttons, the last of each: the lift of a finger whose press a
/// pass took reaches egui with the next pass, and the mouse's last release
/// with it, while the presses, the moves, the touches and the press of a
/// button still down are gone.
#[test]
fn what_no_pass_takes_is_dropped_but_for_the_releases() {
    let (mut events, _ctx) = events(1.0);
    events.on_window_event(&finger(1, TouchPhase::Started, 10.0, 10.0));
    taken(&mut events, 1.0);

    events.on_window_event(&finger(1, TouchPhase::Ended, 10.0, 10.0));
    events.on_window_event(&moved(30.0, 30.0));
    events.on_window_event(&button(ElementState::Pressed, MouseButton::Left));
    events.on_window_event(&button(ElementState::Released, MouseButton::Left));
    events.on_window_event(&button(ElementState::Pressed, MouseButton::Right));
    events.discard();
    assert_eq!(
        taken(&mut events, 1.0),
        [primary(egui::pos2(30.0, 30.0), false)]
    );
}

/// A finger that comes down while the first one is still down is a touch of
/// its own and nothing more: the pointer stays with the first, until that
/// one lifts.
#[test]
fn the_other_fingers_are_touches_only() {
    let (mut events, _ctx) = events(1.0);
    events.on_window_event(&finger(1, TouchPhase::Started, 10.0, 10.0));
    taken(&mut events, 1.0);

    events.on_window_event(&finger(2, TouchPhase::Started, 50.0, 60.0));
    events.on_window_event(&finger(2, TouchPhase::Moved, 55.0, 65.0));
    events.on_window_event(&finger(2, TouchPhase::Ended, 55.0, 65.0));
    events.on_window_event(&finger(3, TouchPhase::Started, 90.0, 90.0));
    events.on_window_event(&finger(3, TouchPhase::Cancelled, 90.0, 90.0));
    assert_eq!(
        taken(&mut events, 1.0),
        [
            touched(2, egui::TouchPhase::Start, egui::pos2(50.0, 60.0)),
            touched(2, egui::TouchPhase::Move, egui::pos2(55.0, 65.0)),
            touched(2, egui::TouchPhase::End, egui::pos2(55.0, 65.0)),
            touched(3, egui::TouchPhase::Start, egui::pos2(90.0, 90.0)),
            touched(3, egui::TouchPhase::Cancel, egui::pos2(90.0, 90.0)),
        ]
    );

    events.on_window_event(&finger(1, TouchPhase::Ended, 10.0, 10.0));
    let pos = egui::pos2(10.0, 10.0);
    assert_eq!(
        taken(&mut events, 1.0),
        [
            touched(1, egui::TouchPhase::End, pos),
            primary(pos, false),
            Event::PointerGone,
        ]
    );
}

/// The force of a finger reaches egui between 0 and 1: as the host reports
/// it where it is normalized, as a share of the most there can be where it
/// is calibrated, and not at all where the screen does not tell it.
#[test]
fn the_force_of_a_finger_is_a_share_of_the_most() {
    let (mut events, _ctx) = events(1.0);
    let calibrated = Force::Calibrated {
        force: 3.0,
        max_possible_force: 4.0,
        altitude_angle: None,
    };
    for (force, expected) in [
        (Some(Force::Normalized(0.25)), Some(0.25)),
        (Some(calibrated), Some(0.75)),
        (None, None),
    ] {
        events.on_window_event(&finger_with(5, TouchPhase::Moved, 1.0, 1.0, force));
        let input = taken(&mut events, 1.0);
        let Some(Event::Touch { force, .. }) = input.first() else {
            panic!("no touch in {input:?}");
        };
        assert_eq!(*force, expected);
    }
}

/// What a pass of egui makes of a finger: a tap on a button drawn by a pass
/// clicks it, and while the finger presses the button, egui uses the
/// pointer: the finger's moves and its lift are egui's. A finger on the
/// rest of the screen is not. (Whether egui keeps a finger that comes down
/// is answered, as egui-winit answers it, for where the last pass left the
/// pointer: after a finger has lifted, that is nowhere.)
#[test]
fn a_tap_clicks_what_the_finger_lands_on() {
    let (mut events, ctx) = events(2.0);
    ctx.enable_accesskit();
    let mut clicks = 0;
    let pass = |events: &mut EguiEvents, clicks: &mut u32| {
        let output = run(&ctx, events.take(SIZE, 2.0), |ui| {
            egui::Window::new("Tool")
                .fixed_pos(egui::pos2(100.0, 100.0))
                .show(ui.ctx(), |ui| {
                    if ui.button("Press").clicked() {
                        *clicks += 1;
                    }
                });
        });
        let update = output.accesskit_update.unwrap();
        update
            .nodes
            .iter()
            .find(|(_, node)| node.role() == Role::Button && node.label() == Some("Press"))
            .and_then(|(_, node)| node.bounds())
    };
    pass(&mut events, &mut clicks);
    let at = pass(&mut events, &mut clicks).expect("the button is on the screen");
    // The middle of the button in physical pixels: twice its points.
    let centre = (at.x0 + at.x1, at.y0 + at.y1);

    // On the bare screen.
    events.on_window_event(&finger(1, TouchPhase::Started, 1500.0, 1100.0));
    pass(&mut events, &mut clicks);
    assert!(!events.on_window_event(&finger(1, TouchPhase::Moved, 1400.0, 1000.0)));
    assert!(!events.on_window_event(&finger(1, TouchPhase::Ended, 1400.0, 1000.0)));
    pass(&mut events, &mut clicks);
    assert_eq!(clicks, 0);

    // On the button.
    events.on_window_event(&finger(2, TouchPhase::Started, centre.0, centre.1));
    pass(&mut events, &mut clicks);
    assert!(
        events.on_window_event(&finger(2, TouchPhase::Moved, centre.0, centre.1)),
        "the finger presses the button"
    );
    assert!(events.on_window_event(&finger(2, TouchPhase::Ended, centre.0, centre.1)));
    pass(&mut events, &mut clicks);
    assert_eq!(clicks, 1);
}

/// A key reaches egui as egui's key, down and up, and a press that types
/// something as its text too. A key egui has no name for types its text
/// and is no key; one that types nothing is a key and no text.
#[test]
fn keys_and_the_text_they_type() {
    let (mut events, _ctx) = events(1.0);
    let key = |key, pressed| Event::Key {
        key,
        physical_key: None,
        pressed,
        repeat: false,
        modifiers: Modifiers::NONE,
    };

    events.key(&character("a"), Some("a"), ElementState::Pressed);
    events.key(&character("a"), Some("a"), ElementState::Released);
    assert_eq!(
        taken(&mut events, 1.0),
        [
            key(egui::Key::A, true),
            Event::Text("a".to_owned()),
            key(egui::Key::A, false),
        ]
    );

    events.key(&character("é"), Some("é"), ElementState::Pressed);
    assert_eq!(taken(&mut events, 1.0), [Event::Text("é".to_owned())]);

    // What winit reports for the keys that type nothing a field could
    // hold: a control character, or no text at all.
    events.key(&named(NamedKey::Enter), Some("\r"), ElementState::Pressed);
    events.key(&named(NamedKey::Backspace), None, ElementState::Pressed);
    events.key(&named(NamedKey::ArrowLeft), None, ElementState::Pressed);
    events.key(&named(NamedKey::Space), Some(" "), ElementState::Pressed);
    assert_eq!(
        taken(&mut events, 1.0),
        [
            key(egui::Key::Enter, true),
            key(egui::Key::Backspace, true),
            key(egui::Key::ArrowLeft, true),
            key(egui::Key::Space, true),
            Event::Text(" ".to_owned()),
        ]
    );

    // A key neither egui nor a text field has a use for.
    events.key(&named(NamedKey::CapsLock), None, ElementState::Pressed);
    events.key(&Key::Dead(Some('^')), None, ElementState::Pressed);
    assert_eq!(taken(&mut events, 1.0), []);
}

/// The modifiers the window reports are on every key and button from then
/// on, and egui is told of each change. Ctrl is egui's command key, and the
/// letter of a command is not typed. A window that loses the keyboard
/// holds no modifier anymore.
#[test]
fn modifiers_are_on_the_events_that_follow() {
    let (mut events, _ctx) = events(1.0);
    let ctrl = Modifiers {
        ctrl: true,
        command: true,
        ..Modifiers::NONE
    };
    events.on_window_event(&modifiers(ModifiersState::CONTROL));
    events.key(&character("a"), Some("a"), ElementState::Pressed);
    events.on_window_event(&moved(10.0, 10.0));
    events.on_window_event(&button(ElementState::Pressed, MouseButton::Left));
    assert_eq!(
        taken(&mut events, 1.0),
        [
            Event::ModifiersChanged(ctrl),
            Event::Key {
                key: egui::Key::A,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: ctrl,
            },
            Event::PointerMoved(egui::pos2(10.0, 10.0)),
            Event::PointerButton {
                pos: egui::pos2(10.0, 10.0),
                button: PointerButton::Primary,
                pressed: true,
                modifiers: ctrl,
            },
        ],
        "and no text: Ctrl+A is a command"
    );

    let shift = Modifiers {
        shift: true,
        ..Modifiers::NONE
    };
    events.on_window_event(&modifiers(ModifiersState::SHIFT));
    events.key(&character("A"), Some("A"), ElementState::Pressed);
    assert_eq!(
        taken(&mut events, 1.0),
        [
            Event::ModifiersChanged(shift),
            Event::Key {
                key: egui::Key::A,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: shift,
            },
            Event::Text("A".to_owned()),
        ]
    );

    events.on_window_event(&WindowEvent::Focused(false));
    events.key(&character("b"), Some("b"), ElementState::Pressed);
    let input = events.take(SIZE, 1.0);
    assert!(!input.focused);
    assert_eq!(
        input.events,
        [
            Event::WindowFocused(false),
            Event::Key {
                key: egui::Key::B,
                physical_key: None,
                pressed: true,
                repeat: false,
                modifiers: Modifiers::NONE,
            },
            Event::Text("b".to_owned()),
        ]
    );
    events.on_window_event(&WindowEvent::Focused(true));
    assert!(events.take(SIZE, 1.0).focused);
}

/// The input of a pass holds what was gathered since the pass before, once:
/// the events are taken, the time moves on, and what was dropped is not
/// replayed.
#[test]
fn a_take_empties_what_was_gathered() {
    let (mut events, _ctx) = events(1.0);
    events.set_max_texture_side(4096);
    events.on_window_event(&moved(1.0, 2.0));
    let first = events.take(SIZE, 1.0);
    assert_eq!(first.events.len(), 1);
    assert_eq!(first.max_texture_side, Some(4096));

    let second = events.take(SIZE, 1.0);
    assert_eq!(second.events, []);
    assert!(
        second.time >= first.time,
        "{:?} then {:?}",
        first.time,
        second.time
    );

    events.on_window_event(&moved(3.0, 4.0));
    events.discard();
    assert_eq!(events.take(SIZE, 1.0).events, []);
}

/// What a pass of egui makes of the input: a button drawn by a pass is
/// clicked by the events of a press and a release over it, and while the
/// pointer is over an egui window the events are egui's to keep; over the
/// rest of the screen they are not.
#[test]
fn egui_clicks_what_the_pointer_presses() {
    let (mut events, ctx) = events(2.0);
    ctx.enable_accesskit();
    let mut clicks = 0;
    let pass = |events: &mut EguiEvents, clicks: &mut u32| {
        let output = run(&ctx, events.take(SIZE, 2.0), |ui| {
            egui::Window::new("Tool")
                .fixed_pos(egui::pos2(100.0, 100.0))
                .show(ui.ctx(), |ui| {
                    if ui.button("Press").clicked() {
                        *clicks += 1;
                    }
                });
        });
        // Where the button is, in points, as egui tells a screen reader.
        let update = output.accesskit_update.unwrap();
        update
            .nodes
            .iter()
            .find(|(_, node)| node.role() == Role::Button && node.label() == Some("Press"))
            .and_then(|(_, node)| node.bounds())
    };

    // Laid out in one pass, on the screen in the next.
    pass(&mut events, &mut clicks);
    let at = pass(&mut events, &mut clicks).expect("the button is on the screen");
    let centre = ((at.x0 + at.x1) / 2.0, (at.y0 + at.y1) / 2.0);

    // The window reports the pointer in physical pixels: twice the points.
    assert!(!events.on_window_event(&moved(1500.0, 1100.0)));
    pass(&mut events, &mut clicks);
    assert!(
        !events.on_window_event(&button(ElementState::Pressed, MouseButton::Left)),
        "a press on the bare screen is not egui's"
    );
    events.on_window_event(&button(ElementState::Released, MouseButton::Left));
    pass(&mut events, &mut clicks);
    assert_eq!(clicks, 0);

    events.on_window_event(&moved(centre.0 * 2.0, centre.1 * 2.0));
    pass(&mut events, &mut clicks);
    assert!(
        events.on_window_event(&button(ElementState::Pressed, MouseButton::Left)),
        "a press over a window of egui is egui's"
    );
    pass(&mut events, &mut clicks);
    events.on_window_event(&button(ElementState::Released, MouseButton::Left));
    pass(&mut events, &mut clicks);
    assert_eq!(clicks, 1);
}

/// A key is egui's while one of its widgets has the keyboard, and Tab is
/// always: it is what gives a widget the keyboard.
#[test]
fn a_key_is_eguis_while_a_widget_has_the_keyboard() {
    let (mut events, ctx) = events(1.0);
    let mut text = String::new();
    let pass = |events: &mut EguiEvents, text: &mut String| {
        run(&ctx, events.take(SIZE, 1.0), |ui| {
            ui.text_edit_singleline(text);
        });
    };
    pass(&mut events, &mut text);

    assert!(!events.key(&character("a"), Some("a"), ElementState::Pressed));
    pass(&mut events, &mut text);
    assert_eq!(text, "", "no widget has the keyboard");

    assert!(events.key(&named(NamedKey::Tab), Some("\t"), ElementState::Pressed));
    events.key(&named(NamedKey::Tab), Some("\t"), ElementState::Released);
    pass(&mut events, &mut text);
    assert!(
        ctx.egui_wants_keyboard_input(),
        "Tab gave the field the focus"
    );

    assert!(events.key(&character("h"), Some("h"), ElementState::Pressed));
    events.key(&character("h"), Some("h"), ElementState::Released);
    events.key(&character("i"), Some("i"), ElementState::Pressed);
    events.key(&character("i"), Some("i"), ElementState::Released);
    pass(&mut events, &mut text);
    assert_eq!(text, "hi");

    events.key(&named(NamedKey::Backspace), None, ElementState::Pressed);
    events.key(&named(NamedKey::Backspace), None, ElementState::Released);
    pass(&mut events, &mut text);
    assert_eq!(text, "h");
}

/// The window is asked for a cursor when the one a pass wants is not the
/// one it shows, and only while the pointer is in the window. egui's "no
/// cursor" hides it.
#[test]
fn the_cursor_is_asked_for_when_it_changes() {
    let (mut events, _ctx) = events(1.0);
    assert_eq!(
        events.cursor_wish(egui::CursorIcon::Text),
        None,
        "no pointer"
    );

    events.on_window_event(&moved(5.0, 5.0));
    assert_eq!(
        events.cursor_wish(egui::CursorIcon::Text),
        Some(CursorWish::Show(CursorIcon::Text))
    );
    assert_eq!(events.cursor_wish(egui::CursorIcon::Text), None, "shown");
    assert_eq!(
        events.cursor_wish(egui::CursorIcon::PointingHand),
        Some(CursorWish::Show(CursorIcon::Pointer))
    );
    assert_eq!(
        events.cursor_wish(egui::CursorIcon::None),
        Some(CursorWish::Hide)
    );

    // A pointer that left and came back finds whatever the host showed
    // meanwhile: the cursor is asked for again.
    events.on_window_event(&WindowEvent::CursorLeft {
        device_id: DeviceId::dummy(),
    });
    assert_eq!(events.cursor_wish(egui::CursorIcon::Default), None);
    events.on_window_event(&moved(5.0, 5.0));
    assert_eq!(
        events.cursor_wish(egui::CursorIcon::Default),
        Some(CursorWish::Show(CursorIcon::Default))
    );
}
