//! The overlay: the tools drawn over the emulator's picture with egui. This
//! file is the shell: whether the overlay is shown, the hint of the first
//! seconds, the menu bar, the windows of the panels and the messages.
//! `panels` is THE panel table, with a file per panel beside it (`start`,
//! `about`, `disks`, `on_screen_keyboard`, `stats`, `memory`, and one per
//! device: `pic`, `pit`, `vga`, `keyboard`, `debug_port`); `load_rom` is
//! the Machine menu's choice of ROMs; `messages` is where a tool says what
//! it did; `theme` is the look. What the panels read alike of egui's input
//! is here too: whether the overlay has the keyboard (`keyboard_taken`),
//! and which of the pointer's events are a finger's echo (`with_echoes`).
//!
//! The shell knows egui and the machine and nothing else: no window, no GPU,
//! no file, no clock, no thread, and of the host only what its switch is
//! called (`platform::OVERLAY_SWITCH`) and whether it came from a site
//! (`platform::HAS_SITE`), whose panel only such a host offers. The window
//! gathers egui's input and calls `run`, which is lent the machine and what
//! the app measured of it (`gui::meter`) for the length of the pass and
//! returns what the user asked the app to do (`Command`); the renderer
//! takes what to draw (`take_paint`). The only time there is, is the time
//! of that input.
//!
//! The invariant: a hidden overlay costs nothing. `draws` says whether there
//! is anything to show; while it says no, the window runs no pass and the
//! renderer is given nothing to paint. Hidden, the overlay shows one thing,
//! and only for the first seconds: the hint that names its switch.

mod about;
mod debug_port;
mod disks;
mod keyboard;
mod load_rom;
mod memory;
mod messages;
mod on_screen_keyboard;
mod panels;
mod pic;
mod pit;
mod start;
mod stats;
pub mod theme;
mod vga;

pub use disks::DiskCommand;
pub use load_rom::RomCommand;
pub use messages::Messages;
pub use panels::Panel;
pub use start::StartCommand;

use egui::{
    Align, Align2, Area, Event, Frame, Id, Layout, MenuBar, Order, PointerButton, Pos2, Rect, Vec2,
    Window, vec2,
};

use super::meter::Meter;
use crate::platform::{self, OVERLAY_SWITCH};
use crate::site::SiteFile;
use emu8086_core::vm::runtime::Runtime;
use theme::{METRICS, PALETTE};

/// What the hint says: how the overlay is shown on this host, which is the
/// host's to say (`platform::OVERLAY_SWITCH`).
fn hint_text() -> String {
    format!("{}: tools", OVERLAY_SWITCH.name)
}

/// How long the hint stays after the first frame, in seconds.
const HINT_SECONDS: f64 = 5.0;
/// The mark of a paused machine, at the right end of the menu bar.
const PAUSED: &str = "Paused";

/// What a tool asks the app to do with the machine. The shell only asks:
/// the app carries the command out and says what came of it through
/// `Overlay::messages`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    /// Stop running the guest. The window goes on drawing.
    Pause,
    /// Run the guest again, from where it stopped.
    Resume,
    /// Reboot the guest.
    Reset,
    /// Restart the machine with another ROM image, as Machine > Load ROM
    /// asks.
    Rom(RomCommand),
    /// Put the state of the display where it can be read.
    DumpVga,
    /// Do to a drive what the Disks panel asks.
    Disk(DiskCommand),
    /// Start the machine with a disk of the site, as the Start panel asks:
    /// the disk into its drive, the bootstrap trying that drive first, and
    /// the machine restarted.
    Start(StartCommand),
    /// Press (`pressed`) or release the key of the on-screen keyboard whose
    /// make code is `make`, for the guest. A press of a key held down is a
    /// repeat.
    Key { make: u8, pressed: bool },
}

/// What the menu bar and the panels work on during a pass: the machine, and
/// the two ways out of the overlay.
pub struct Cx<'a> {
    /// The machine, the shell's for the length of the pass.
    pub runtime: &'a mut Runtime,
    /// Whether the app is holding the guest still.
    pub paused: bool,
    /// The disk of the site the app is fetching to start the machine with,
    /// while it is: the Start panel waits for it.
    pub starting: Option<&'static SiteFile>,
    /// What the app measured of the emulation, frame by frame.
    pub meter: &'a Meter,
    /// Whether the pass before this one drew the panel being drawn: not
    /// when it was closed, collapsed, or hidden with the overlay since.
    pub drawn_before: bool,
    /// What the app is asked to do once the pass is over.
    pub commands: &'a mut Vec<Command>,
    /// Where to say what was done, or why it could not be.
    pub messages: &'a mut Messages,
}

/// What a pass left to be drawn, ready for the GPU.
pub struct Paint {
    /// The meshes, each with the rectangle it is clipped to.
    pub primitives: Vec<egui::ClippedPrimitive>,
    /// The textures egui changed since the last paint was taken: those in
    /// `set` are uploaded before drawing, those in `free` released after
    /// it. egui asserts that no change is dropped: whoever takes a `Paint`
    /// drains both.
    pub textures: egui::TexturesDelta,
    /// Physical pixels per point of the pass.
    pub pixels_per_point: f32,
}

/// The hint that names the overlay's switch, shown while the overlay is not.
enum Hint {
    /// No pass has told the time yet.
    Unstarted,
    /// Shown until this input time.
    Until(f64),
    /// Gone for good: its time is over, or the switch was found.
    Over,
}

pub struct Overlay {
    ctx: egui::Context,
    visible: bool,
    hint: Hint,
    /// Whether the host came from a site: only then are the panels that
    /// need one offered (`Panel::needs_site`).
    has_site: bool,
    /// The panels whose windows are open.
    open: Vec<Panel>,
    /// The panels whose bodies the last pass drew: those open, but for a
    /// window collapsed to its title bar.
    drawn: Vec<Panel>,
    /// What the menu bar left of the screen below it in the last pass that
    /// drew it.
    below_bar: Rect,
    messages: Messages,
    /// What the last pass drew, until `take_paint` hands it on.
    shapes: Vec<egui::epaint::ClippedShape>,
    pixels_per_point: f32,
    /// The texture changes of every pass since the last `take_paint`.
    textures: egui::TexturesDelta,
    /// What the last pass asks of the window: the cursor, the IME.
    platform: egui::PlatformOutput,
}

impl Overlay {
    /// A hidden overlay, in the theme, with no panel open, on the host the
    /// app runs on.
    pub fn new() -> Self {
        Self::on(platform::HAS_SITE)
    }

    /// The overlay on a host that came from a site, or not: `has_site`.
    pub fn on(has_site: bool) -> Self {
        let ctx = egui::Context::default();
        theme::apply(&ctx);
        // The keyboard is shared with the guest. egui's own shortcuts, to
        // zoom the UI (Ctrl with + or -) and to quit (Ctrl+Q), would act on
        // keys the guest gets as well.
        ctx.options_mut(|options| {
            options.zoom_with_keyboard = false;
            options.quit_shortcuts.clear();
        });
        // So would Ctrl+C on text selected in a label, and there is no
        // clipboard to copy it to: the text of the overlay is not selected.
        ctx.all_styles_mut(|style| style.interaction.selectable_labels = false);
        Self {
            ctx,
            visible: false,
            hint: Hint::Unstarted,
            has_site,
            open: Vec::new(),
            drawn: Vec::new(),
            below_bar: Rect::EVERYTHING,
            messages: Messages::default(),
            shapes: Vec::new(),
            pixels_per_point: 1.0,
            textures: egui::TexturesDelta::default(),
            platform: egui::PlatformOutput::default(),
        }
    }

    /// The context the passes run in, for whoever feeds it the window's
    /// events.
    pub fn context(&self) -> &egui::Context {
        &self.ctx
    }

    pub fn visible(&self) -> bool {
        self.visible
    }

    /// Shows the overlay, or hides it. Whoever does either has found the
    /// switch: the hint is over.
    pub fn toggle(&mut self) {
        if self.visible {
            self.hide();
        } else {
            self.visible = true;
        }
        self.hint = Hint::Over;
    }

    /// Hides the overlay, as the app does by itself once its work is done:
    /// the user did not work the switch for it.
    pub fn hide(&mut self) {
        self.visible = false;
        // A hidden overlay runs no pass, and draws no panel meanwhile.
        self.drawn.clear();
    }

    /// Opens the window of `panel`, as its checkbox in the View menu does.
    /// A panel the host does not offer stays shut.
    pub fn open(&mut self, panel: Panel) {
        if self.offers(panel) && !self.open.contains(&panel) {
            self.open.push(panel);
        }
    }

    /// Closes the window of `panel`, as its close button does.
    pub fn close(&mut self, panel: Panel) {
        self.open.retain(|open| *open != panel);
    }

    /// Whether the View menu offers `panel` on this host.
    fn offers(&self, panel: Panel) -> bool {
        self.has_site || !panel.needs_site()
    }

    /// Whether there is anything to show: the overlay, or the hint. While
    /// there is not, no pass is to be run and nothing painted.
    pub fn draws(&self) -> bool {
        self.visible || !matches!(self.hint, Hint::Over)
    }

    /// Whether the keys belong to the overlay: it is shown, and one of its
    /// widgets has the keyboard focus, as a field being typed in has, or
    /// one of its menus is open, which Escape closes.
    pub fn wants_keyboard(&self) -> bool {
        self.visible && keyboard_taken(&self.ctx)
    }

    /// Whether the overlay is shown and its last pass drew the body of
    /// `panel`: the panel is open and its window not collapsed.
    pub fn shows(&self, panel: Panel) -> bool {
        self.visible && self.drawn.contains(&panel)
    }

    /// Whether the overlay shows something at `at`, in points, as its last
    /// pass laid it out: the menu bar, a window, an open menu or a message.
    /// Anywhere else is the picture. A hidden overlay covers nothing: the
    /// hint is not one to touch.
    pub fn covers(&self, at: Pos2) -> bool {
        if !self.visible {
            return false;
        }
        if self.messages.covers(at) {
            return true;
        }
        match self.ctx.layer_id_at(at) {
            // The menu bar is drawn on egui's background layer, which is
            // where the picture is too: there, the bar is what lies outside
            // the room it left below it.
            Some(layer) if layer.order == Order::Background => !self.below_bar.contains(at),
            Some(_) => true,
            None => false,
        }
    }

    /// Where the app says what came of a command.
    pub fn messages(&mut self) -> &mut Messages {
        &mut self.messages
    }

    /// Runs one pass over `input`, on the machine as it is: `paused` says
    /// whether the app is holding the guest still, `starting` which disk of
    /// the site it is fetching to start the machine with, if it is, `meter`
    /// what it measured of the emulation. Returns what the user asked the
    /// app to do, in the order asked.
    pub fn run(
        &mut self,
        input: egui::RawInput,
        runtime: &mut Runtime,
        paused: bool,
        starting: Option<&'static SiteFile>,
        meter: &Meter,
    ) -> Vec<Command> {
        // egui may run the UI twice for one frame, to lay out what it
        // shows for the first time: the commands of both runs count, and a
        // click is in only one of them.
        let mut commands = Vec::new();
        let ctx = self.ctx.clone();
        let output = ctx.run_ui(input, |ui| {
            self.ui(ui, runtime, paused, starting, meter, &mut commands)
        });
        self.shapes = output.shapes;
        self.pixels_per_point = output.pixels_per_point;
        self.textures.append(output.textures_delta);
        self.platform = output.platform_output;
        commands
    }

    /// What the last pass drew, as meshes, with every texture change since
    /// the last call. Only after a pass: there is nothing to tessellate
    /// before the first.
    pub fn take_paint(&mut self) -> Paint {
        let shapes = std::mem::take(&mut self.shapes);
        Paint {
            primitives: self.ctx.tessellate(shapes, self.pixels_per_point),
            textures: std::mem::take(&mut self.textures),
            pixels_per_point: self.pixels_per_point,
        }
    }

    /// What the last pass asks of the window it is shown in.
    pub fn take_platform_output(&mut self) -> egui::PlatformOutput {
        std::mem::take(&mut self.platform)
    }

    fn ui(
        &mut self,
        ui: &mut egui::Ui,
        runtime: &mut Runtime,
        paused: bool,
        starting: Option<&'static SiteFile>,
        meter: &Meter,
        commands: &mut Vec<Command>,
    ) {
        let now = ui.ctx().input(|input| input.time);
        if let Hint::Unstarted = self.hint {
            self.hint = Hint::Until(now + HINT_SECONDS);
        }
        if let Hint::Until(end) = self.hint
            && now >= end
        {
            self.hint = Hint::Over;
        }
        let drawn_before = std::mem::take(&mut self.drawn);
        if !self.visible {
            if let Hint::Until(_) = self.hint {
                hint(ui.ctx());
            }
            return;
        }

        let offered: Vec<Panel> = Panel::ALL
            .into_iter()
            .filter(|panel| self.offers(*panel))
            .collect();
        let mut cx = Cx {
            runtime,
            paused,
            starting,
            meter,
            drawn_before: false,
            commands,
            messages: &mut self.messages,
        };
        let open = &mut self.open;
        egui::Panel::top("overlay_menu_bar").show(ui, |ui| {
            MenuBar::new().ui(ui, |ui| menu_bar(ui, &mut cx, &offered, open));
        });

        // What the menu bar leaves of the window: the panels stay in it,
        // and the messages go to its corner.
        let below_bar = ui.available_rect_before_wrap();
        self.below_bar = below_bar;
        for (index, panel) in offered.into_iter().enumerate() {
            let mut is_open = open.contains(&panel);
            // A window takes the size of what its panel shows: a tool over
            // the picture hides no more of it than it has to. A panel that
            // shows more than fits scrolls it itself.
            //
            // It first opens a step from where the panel before it in the
            // View menu opens, its title bar in view: egui would put it
            // where there is room, and over a small picture there is none,
            // so it would put them all in one place, the last one hiding
            // the others. egui measures a new window in an unseen frame
            // first, taking it meanwhile for the size it was given, and
            // keeps it inside the screen at that size: given none, it takes
            // one so wide that every step is pushed back to the same place.
            let step = Vec2::splat(METRICS.edge + METRICS.cascade * index as f32);
            cx.drawn_before = drawn_before.contains(&panel);
            let window = Window::new(panel.title())
                .open(&mut is_open)
                .constrain_to(below_bar)
                .default_pos(below_bar.left_top() + step)
                .default_size(Vec2::ZERO)
                .auto_sized()
                .show(ui.ctx(), |ui| panel.body(ui, &mut cx));
            // A window that is closed shows nothing, and a collapsed one
            // only its title bar.
            if window.is_some_and(|window| window.inner.is_some()) {
                self.drawn.push(panel);
            }
            // The window's own button closed it.
            if !is_open {
                open.retain(|shown| *shown != panel);
            }
        }

        let corner = below_bar.right_top() + vec2(-METRICS.edge, METRICS.edge);
        cx.messages.show(ui.ctx(), corner, now);
    }
}

impl Drop for Overlay {
    fn drop(&mut self) {
        // egui asserts that a texture change is applied or cleared, never
        // dropped. An overlay nobody painted, as in a test without a GPU,
        // still holds the changes of its passes.
        self.textures.clear();
    }
}

/// Whether the overlay over `ctx`, if it is shown, has the keyboard: one of
/// its widgets has the focus, or one of its menus is open.
fn keyboard_taken(ctx: &egui::Context) -> bool {
    ctx.egui_wants_keyboard_input() || egui::Popup::is_any_open(ctx)
}

/// The events of a frame, each with whether it is a finger's echo. The
/// first finger is egui's pointer as well, and the hosts put the pointer's
/// events of a finger right after its touch (egui-winit, and
/// `platform::egui_events` in a browser): a press or a release of the
/// primary button that follows a touch, with nothing but moves of the
/// pointer between them, is that touch's echo.
fn with_echoes(events: &[Event]) -> impl Iterator<Item = (&Event, bool)> {
    let mut after_touch = false;
    events.iter().map(move |event| {
        let echo = after_touch
            && matches!(
                event,
                Event::PointerButton {
                    button: PointerButton::Primary,
                    ..
                }
            );
        after_touch = match event {
            Event::Touch { .. } => true,
            Event::PointerMoved(_) => after_touch,
            _ => false,
        };
        (event, echo)
    })
}

/// The menu bar: the Machine menu, with the ROMs the machine can restart
/// with in a menu of their own (`load_rom`), the View menu with a checkbox
/// for every panel the host offers (`offered`), and at the right end the
/// guest's speed, beside the mark of a paused machine.
fn menu_bar(ui: &mut egui::Ui, cx: &mut Cx<'_>, offered: &[Panel], open: &mut Vec<Panel>) {
    ui.menu_button("Machine", |ui| {
        let (label, command) = if cx.paused {
            ("Resume", Command::Resume)
        } else {
            ("Pause", Command::Pause)
        };
        if ui.button(label).clicked() {
            cx.commands.push(command);
        }
        if ui.button("Reset").clicked() {
            cx.commands.push(Command::Reset);
        }
        load_rom::menu(ui, cx);
        ui.separator();
        if ui.button("Dump VGA state").clicked() {
            cx.commands.push(Command::DumpVga);
        }
    });
    ui.menu_button("View", |ui| {
        for &panel in offered {
            let mut is_open = open.contains(&panel);
            if ui.checkbox(&mut is_open, panel.title()).changed() {
                if is_open {
                    open.push(panel);
                } else {
                    open.retain(|shown| *shown != panel);
                }
            }
        }
    });
    ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
        ui.label(stats::readout(cx.meter, cx.paused))
            .on_hover_text("Instructions per second, over the guest's last second of running");
        if cx.paused {
            ui.colored_label(PALETTE.warning, PAUSED);
        }
    });
}

/// The hint, in the bottom left corner of the picture.
fn hint(ctx: &egui::Context) {
    Area::new(Id::new("overlay_hint"))
        .order(Order::Foreground)
        .anchor(Align2::LEFT_BOTTOM, vec2(METRICS.edge, -METRICS.edge))
        // The hint is read, not used: clicks go to what lies under it.
        .interactable(false)
        .show(ctx, |ui| {
            Frame::popup(ui.style()).show(ui, |ui| ui.label(hint_text()));
        });
}

#[cfg(test)]
pub(super) mod testing;

#[cfg(test)]
mod tests;
