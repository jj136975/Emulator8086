//! Driving the overlay by hand, for the tests of the shell, of its panels
//! and of the session around it: frames of hand-made input, keys, the
//! pointer and the fingers of a touch screen as the hosts hand them to egui,
//! and what each pass left on the screen as egui describes it to a screen
//! reader (the texts, and where each control is). No window and no GPU.
//! `Shell` is the shell alone, lent a machine; `waiting_for_a_key` is a
//! machine in a state the BIOS put it in.
//!
//! The invariant: a test reaches the overlay only the way a user does. A
//! control is found by its role and its label among what the last pass
//! showed, and clicked with the pointer at its centre: a control that is not
//! on the screen, or lies under another, cannot be clicked.

use std::sync::mpsc::{Sender, channel};

use egui::accesskit::{Node, Role, Toggled};
use egui::{
    Event, Modifiers, PointerButton, Pos2, RawInput, Rect, TouchDeviceId, TouchId, TouchPhase,
    pos2, vec2,
};

use super::{Command, Overlay};
use crate::gui::meter::Meter;
use crate::site::SiteFile;
use crate::wall_clock::HostClock;
use emu8086_core::io::keyboard::{EmulatorEvent, KeyboardController};
use emu8086_core::vm::runtime::Runtime;
use emu8086_core::{BIOS_ROM, BIOS_ROM_NAME};

/// Size of the window the tests pretend to draw in: in points, and since
/// they draw at one pixel per point, in pixels.
pub const SCREEN: [u32; 2] = [640, 400];

/// Batches of instructions the BIOS is given to reach its wait for a key.
const BOOT_BATCHES: usize = 2000;

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

/// Finger `id` of a touch screen in `phase` at `pos`.
pub fn touch(id: u64, phase: TouchPhase, pos: Pos2) -> Event {
    Event::Touch {
        device_id: TouchDeviceId(1),
        id: TouchId(id),
        phase,
        pos,
        force: None,
    }
}

/// The primary button of the pointer going down or coming up at `pos`.
pub fn primary(pos: Pos2, pressed: bool) -> Event {
    Event::PointerButton {
        pos,
        button: PointerButton::Primary,
        pressed,
        modifiers: Modifiers::NONE,
    }
}

/// The label of the button that opens a menu within a menu, as egui tells
/// a screen reader of it: the menu's `title`, and the arrow at its end.
pub fn submenu(title: &str) -> String {
    format!("{title} ⏵")
}

/// What a host hands egui when the first finger, which is the pointer as
/// well, comes down at `pos`: its touch, then the pointer moved there and
/// its primary button pressed.
pub fn first_finger_down(id: u64, pos: Pos2) -> Vec<Event> {
    vec![
        touch(id, TouchPhase::Start, pos),
        Event::PointerMoved(pos),
        primary(pos, true),
    ]
}

/// What a host hands egui when the first finger lifts at `pos`: its touch,
/// then the primary button released and the pointer gone.
pub fn first_finger_up(id: u64, pos: Pos2) -> Vec<Event> {
    vec![
        touch(id, TouchPhase::End, pos),
        primary(pos, false),
        Event::PointerGone,
    ]
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

    /// The running texts level with `text`, left to right, `text` among
    /// them: the row of a table it is in, as a reader sees it. A text is on
    /// the row when it spans the height of the middle of `text`. Empty when
    /// `text` is not on the screen.
    pub fn row(&self, text: &str) -> Vec<&str> {
        let labels = || {
            self.nodes
                .iter()
                .filter(|node| node.role() == Role::Label)
                .filter_map(|node| Some((node.value()?, node.bounds()?)))
        };
        let Some((_, first)) = labels().find(|(value, _)| *value == text) else {
            return Vec::new();
        };
        let middle = (first.y0 + first.y1) / 2.0;
        let mut row: Vec<_> = labels()
            .filter(|(_, bounds)| bounds.y0 <= middle && middle <= bounds.y1)
            .collect();
        row.sort_by(|(_, a), (_, b)| a.x0.total_cmp(&b.x0));
        row.into_iter().map(|(value, _)| value).collect()
    }

    /// The running texts level with the control of `role` labelled `label`,
    /// left to right: the row of a table that starts with a control, as
    /// `row` is the row of a text. Empty when the control is not on the
    /// screen.
    pub fn row_of(&self, role: Role, label: &str) -> Vec<&str> {
        let Some(control) = self.find(role, label).and_then(Node::bounds) else {
            return Vec::new();
        };
        let middle = (control.y0 + control.y1) / 2.0;
        let mut row: Vec<_> = self
            .nodes
            .iter()
            .filter(|node| node.role() == Role::Label)
            .filter_map(|node| Some((node.value()?, node.bounds()?)))
            .filter(|(_, bounds)| bounds.y0 <= middle && middle <= bounds.y1)
            .collect();
        row.sort_by(|(_, a), (_, b)| a.x0.total_cmp(&b.x0));
        row.into_iter().map(|(value, _)| value).collect()
    }

    /// Whether the checkbox labelled `label` is ticked.
    pub fn ticked(&self, label: &str) -> bool {
        let checkbox = self
            .find(Role::CheckBox, label)
            .unwrap_or_else(|| panic!("no checkbox {label:?} on the screen"));
        checkbox.toggled() == Some(Toggled::True)
    }

    /// Whether the button labelled `label` is pressed: a button that is
    /// down or up, and is down.
    pub fn pressed(&self, label: &str) -> bool {
        let button = self
            .find(Role::Button, label)
            .unwrap_or_else(|| panic!("no button {label:?} on the screen"));
        button.toggled() == Some(Toggled::True)
    }

    /// Whether the radio button labelled `label` is the one chosen.
    pub fn chosen(&self, label: &str) -> bool {
        let radio = self
            .find(Role::RadioButton, label)
            .unwrap_or_else(|| panic!("no radio button {label:?} on the screen"));
        radio.toggled() == Some(Toggled::True)
    }

    /// Whether the control of `role` labelled `label` can be used: it is on
    /// the screen, and not greyed out.
    pub fn enabled(&self, role: Role, label: &str) -> bool {
        self.find(role, label)
            .is_some_and(|control| !control.is_disabled())
    }

    /// Where the running text `text` is.
    pub fn text_rect(&self, text: &str) -> Rect {
        let bounds = self
            .nodes
            .iter()
            .filter(|node| node.role() == Role::Label && node.value() == Some(text))
            .find_map(Node::bounds)
            .unwrap_or_else(|| panic!("no text {text:?} on the screen: {:?}", self.texts()));
        Rect::from_min_max(
            pos2(bounds.x0 as f32, bounds.y0 as f32),
            pos2(bounds.x1 as f32, bounds.y1 as f32),
        )
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

    /// The input of the next frame, with `events`, for a test that runs
    /// that frame on the target itself. The clock moves on as for `frame`.
    pub fn next_input(&mut self, events: Vec<Event>) -> RawInput {
        let input = input(self.time, events);
        self.time += FRAME_SECONDS;
        input
    }

    /// Runs one frame with `events`.
    pub fn frame(&mut self, events: Vec<Event>) -> &Screen {
        let input = self.next_input(events);
        let output = self.target.pass(input);
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

    /// Ticks or unticks the checkbox of the panel titled `title` in the
    /// View menu, and gives its window the time to come or to go.
    pub fn toggle_in_view_menu(&mut self, title: &str) -> &Screen {
        self.click(Role::Button, "View");
        self.click(Role::CheckBox, title);
        self.wait(1.0)
    }

    /// Types `text` into the text field labelled `label` in place of what
    /// it held, and presses Enter: a click gives the field the focus, Ctrl+A
    /// selects what it holds.
    pub fn fill(&mut self, label: &str, text: &str) -> &Screen {
        self.click(Role::TextInput, label);
        let select_all = Event::Key {
            key: egui::Key::A,
            physical_key: None,
            pressed: true,
            repeat: false,
            modifiers: Modifiers::COMMAND,
        };
        self.frame(vec![select_all, Event::Text(text.to_string())]);
        self.frame(vec![key(egui::Key::Enter, true)]);
        self.frame(vec![key(egui::Key::Enter, false)]);
        self.frame(Vec::new())
    }
}

/// The shell by itself, lent a machine: what the tests of the shell and of
/// its panels drive. Nobody carries out the commands its passes ask for:
/// `paused` is what the test says, and so are `starting` and `meter`, which
/// no frame feeds.
pub struct Shell {
    pub overlay: Overlay,
    pub runtime: Runtime,
    pub paused: bool,
    pub starting: Option<&'static SiteFile>,
    pub meter: Meter,
    pub commands: Vec<Command>,
}

impl Shell {
    /// The shell as the app starts it, hidden, lent `runtime`, on the host
    /// the tests run on: a desktop, which came from no site.
    pub fn new(runtime: Runtime) -> Self {
        Self::with(Overlay::new(), runtime)
    }

    /// The shell, hidden, lent `runtime`, on a host that came from a site,
    /// or not: `has_site`.
    pub fn on(runtime: Runtime, has_site: bool) -> Self {
        Self::with(Overlay::on(has_site), runtime)
    }

    fn with(overlay: Overlay, runtime: Runtime) -> Self {
        Self {
            overlay,
            runtime,
            paused: false,
            starting: None,
            meter: Meter::default(),
            commands: Vec::new(),
        }
    }

    /// A shell shown over `runtime`, its first frame run, with the panel
    /// titled `title` open.
    pub fn with_panel(runtime: Runtime, title: &str) -> Driver<Self> {
        let mut shell = Self::new(runtime);
        shell.overlay.toggle();
        let mut driver = Driver::new(shell);
        driver.frame(Vec::new());
        driver.toggle_in_view_menu(title);
        driver
    }
}

impl Passes for Shell {
    fn context(&self) -> &egui::Context {
        self.overlay.context()
    }

    fn pass(&mut self, input: RawInput) -> egui::PlatformOutput {
        let commands = self.overlay.run(
            input,
            &mut self.runtime,
            self.paused,
            self.starting,
            &self.meter,
        );
        self.commands.extend(commands);
        self.overlay.take_platform_output()
    }
}

/// A machine without disks whose BIOS has run its power-on self test and
/// found nothing to boot: it said so and waits in HLT, interrupts enabled,
/// for a key, typed through the sender. It gets there without any time
/// passing, and every run gets there the same way.
pub fn waiting_for_a_key() -> (Runtime, Sender<EmulatorEvent>) {
    let (keys, key_events) = channel();
    let mut runtime = Runtime::new(
        Vec::new(),
        Vec::new(),
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .expect("a machine without disks has nothing to open");
    runtime.load_rom_image(BIOS_ROM, BIOS_ROM_NAME).unwrap();
    for _ in 0..BOOT_BATCHES {
        runtime.run(50_000);
        if runtime.cpu.halted {
            return (runtime, keys);
        }
    }
    panic!("the BIOS did not come to wait for a key");
}
