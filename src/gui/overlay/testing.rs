//! Driving the overlay by hand, for the tests of the shell and of the
//! session around it: frames of hand-made input, and what each pass left on
//! the screen as egui describes it to a screen reader (the texts, and where
//! each control is). No window and no GPU.
//!
//! The invariant: a test reaches the overlay only the way a user does. A
//! control is found by its role and its label among what the last pass
//! showed, and clicked with the pointer at its centre: a control that is not
//! on the screen, or lies under another, cannot be clicked.

use egui::accesskit::{Node, Role};
use egui::{Event, Modifiers, PointerButton, Pos2, RawInput, Rect, pos2, vec2};

/// Size of the window the tests pretend to draw in: in points, and since
/// they draw at one pixel per point, in pixels.
pub const SCREEN: [u32; 2] = [640, 400];

/// Seconds from one frame to the next.
const FRAME_SECONDS: f64 = 1.0 / 60.0;

/// The input of a frame at input time `time`: `events`, in the window the
/// tests pretend to have.
pub fn input(time: f64, events: Vec<Event>) -> RawInput {
    RawInput {
        screen_rect: Some(Rect::from_min_size(
            Pos2::ZERO,
            vec2(SCREEN[0] as f32, SCREEN[1] as f32),
        )),
        time: Some(time),
        events,
        ..Default::default()
    }
}

/// The event of `key` going down or coming up, with no modifier held.
pub fn key(key: egui::Key, pressed: bool) -> Event {
    Event::Key {
        key,
        physical_key: None,
        pressed,
        repeat: false,
        modifiers: Modifiers::NONE,
    }
}

/// What a pass left on the screen.
#[derive(Default)]
pub struct Screen {
    nodes: Vec<Node>,
}

impl Screen {
    fn of(output: &egui::PlatformOutput) -> Self {
        let update = output
            .accesskit_update
            .as_ref()
            .expect("the driver turned the description of the screen on");
        Self {
            nodes: update.nodes.iter().map(|(_, node)| node.clone()).collect(),
        }
    }

    /// The control of `role` that is labelled `label`.
    fn find(&self, role: Role, label: &str) -> Option<&Node> {
        self.nodes
            .iter()
            .find(|node| node.role() == role && node.label() == Some(label))
    }

    /// Whether a control of `role` labelled `label` is on the screen.
    pub fn has(&self, role: Role, label: &str) -> bool {
        self.find(role, label).is_some()
    }

    /// Whether `text` is shown as running text.
    pub fn shows(&self, text: &str) -> bool {
        self.texts().contains(&text)
    }

    /// Every running text on the screen.
    pub fn texts(&self) -> Vec<&str> {
        self.nodes
            .iter()
            .filter(|node| node.role() == Role::Label)
            .filter_map(Node::value)
            .collect()
    }

    /// Where the control of `role` labelled `label` is.
    pub fn rect(&self, role: Role, label: &str) -> Rect {
        let bounds = self
            .find(role, label)
            .and_then(Node::bounds)
            .unwrap_or_else(|| {
                panic!(
                    "no {role:?} {label:?} on the screen, which shows {:?}",
                    self.labels()
                )
            });
        Rect::from_min_max(
            pos2(bounds.x0 as f32, bounds.y0 as f32),
            pos2(bounds.x1 as f32, bounds.y1 as f32),
        )
    }

    /// The label of every control on the screen, for the message of a
    /// failure.
    fn labels(&self) -> Vec<&str> {
        self.nodes.iter().filter_map(Node::label).collect()
    }
}

/// Something that runs passes of the overlay: the shell by itself, or the
/// session around it.
pub trait Passes {
    /// The context the passes run in.
    fn context(&self) -> &egui::Context;

    /// Runs one pass over `input` and returns what it asks of the window,
    /// which holds the description of the screen.
    fn pass(&mut self, input: RawInput) -> egui::PlatformOutput;
}

/// Drives `target` frame by frame, a sixtieth of a second apart.
pub struct Driver<T> {
    pub target: T,
    time: f64,
    screen: Screen,
}

impl<T: Passes> Driver<T> {
    pub fn new(target: T) -> Self {
        target.context().enable_accesskit();
        Self {
            target,
            time: 0.0,
            screen: Screen::default(),
        }
    }

    /// What the last frame left on the screen.
    pub fn screen(&self) -> &Screen {
        &self.screen
    }

    /// Runs one frame with `events`.
    pub fn frame(&mut self, events: Vec<Event>) -> &Screen {
        let output = self.target.pass(input(self.time, events));
        self.time += FRAME_SECONDS;
        self.screen = Screen::of(&output);
        &self.screen
    }

    /// Lets `seconds` go by without a frame, then runs one.
    pub fn wait(&mut self, seconds: f64) -> &Screen {
        self.time += seconds;
        self.frame(Vec::new())
    }

    /// Clicks the control of `role` labelled `label`: the press in one
    /// frame, the release in the next, and one more in which what the click
    /// opened or closed is laid out.
    pub fn click(&mut self, role: Role, label: &str) -> &Screen {
        let at = self.screen.rect(role, label).center();
        let button = |pressed| Event::PointerButton {
            pos: at,
            button: PointerButton::Primary,
            pressed,
            modifiers: Modifiers::NONE,
        };
        self.frame(vec![Event::PointerMoved(at), button(true)]);
        self.frame(vec![button(false)]);
        self.frame(Vec::new())
    }
}
