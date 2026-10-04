//! The overlay: the tools drawn over the emulator's picture with egui. This
//! file is the shell: whether the overlay is shown, the hint of the first
//! seconds, the menu bar and the messages. `messages` is where a tool says
//! what it did; `theme` is the look.
//!
//! The shell knows egui and nothing else: no window, no GPU, no file, no
//! clock. The window gathers egui's input and calls `run`, which returns
//! what the user asked the app to do (`Command`); the renderer takes what to
//! draw (`take_paint`). The only time there is, is the time of that input.
//!
//! The invariant: a hidden overlay costs nothing. `draws` says whether there
//! is anything to show; while it says no, the window runs no pass and the
//! renderer is given nothing to paint. Hidden, the overlay shows one thing,
//! and only for the first seconds: the hint that names its key.

mod messages;
pub mod theme;

pub use messages::Messages;

use egui::{Align2, Area, Frame, Id, MenuBar, Order, vec2};

use theme::METRICS;

/// What the hint says. The key is the window's (`gui::OVERLAY_KEY`).
const HINT: &str = "F12: tools";
/// How long the hint stays after the first frame, in seconds.
const HINT_SECONDS: f64 = 5.0;

/// What a tool asks the app to do with the machine. The shell only asks:
/// the app carries the command out and says what came of it through
/// `Overlay::messages`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    /// Reboot the guest.
    Reset,
    /// Put the state of the display where it can be read.
    DumpVga,
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

/// The hint that names the overlay's key, shown while the overlay is not.
enum Hint {
    /// No pass has told the time yet.
    Unstarted,
    /// Shown until this input time.
    Until(f64),
    /// Gone for good: its time is over, or the key was found.
    Over,
}

pub struct Overlay {
    ctx: egui::Context,
    visible: bool,
    hint: Hint,
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
    /// A hidden overlay, in the theme.
    pub fn new() -> Self {
        let ctx = egui::Context::default();
        theme::apply(&ctx);
        // The keyboard is shared with the guest. egui's own shortcuts, to
        // zoom the UI (Ctrl with + or -) and to quit (Ctrl+Q), would act on
        // keys the guest gets as well.
        ctx.options_mut(|options| {
            options.zoom_with_keyboard = false;
            options.quit_shortcuts.clear();
        });
        Self {
            ctx,
            visible: false,
            hint: Hint::Unstarted,
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
    /// key: the hint is over.
    pub fn toggle(&mut self) {
        self.visible = !self.visible;
        self.hint = Hint::Over;
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
        self.visible
            && (self.ctx.egui_wants_keyboard_input() || egui::Popup::is_any_open(&self.ctx))
    }

    /// Where the app says what came of a command.
    pub fn messages(&mut self) -> &mut Messages {
        &mut self.messages
    }

    /// Runs one pass over `input`. Returns what the user asked the app to
    /// do, in the order asked.
    pub fn run(&mut self, input: egui::RawInput) -> Vec<Command> {
        // egui may run the UI twice for one frame, to lay out what it
        // shows for the first time: the commands of both runs count, and a
        // click is in only one of them.
        let mut commands = Vec::new();
        let ctx = self.ctx.clone();
        let output = ctx.run_ui(input, |ui| self.ui(ui, &mut commands));
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

    fn ui(&mut self, ui: &mut egui::Ui, commands: &mut Vec<Command>) {
        let now = ui.ctx().input(|input| input.time);
        if let Hint::Unstarted = self.hint {
            self.hint = Hint::Until(now + HINT_SECONDS);
        }
        if let Hint::Until(end) = self.hint
            && now >= end
        {
            self.hint = Hint::Over;
        }
        if !self.visible {
            if let Hint::Until(_) = self.hint {
                hint(ui.ctx());
            }
            return;
        }

        egui::Panel::top("overlay_menu_bar").show(ui, |ui| {
            MenuBar::new().ui(ui, |ui| menu_bar(ui, commands));
        });

        // The messages go below the menu bar, in what it leaves of the
        // window.
        let below_bar = ui.available_rect_before_wrap();
        let corner = below_bar.right_top() + vec2(-METRICS.edge, METRICS.edge);
        self.messages.show(ui.ctx(), corner, now);
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

/// The menu bar: the Machine menu.
fn menu_bar(ui: &mut egui::Ui, commands: &mut Vec<Command>) {
    ui.menu_button("Machine", |ui| {
        if ui.button("Reset").clicked() {
            commands.push(Command::Reset);
        }
        if ui.button("Dump VGA state").clicked() {
            commands.push(Command::DumpVga);
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
            Frame::popup(ui.style()).show(ui, |ui| ui.label(HINT));
        });
}

#[cfg(test)]
pub(super) mod testing;

#[cfg(test)]
mod tests;
