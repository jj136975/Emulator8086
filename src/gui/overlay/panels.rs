//! THE panel table. A panel is a tool in a window of its own over the
//! picture, which the View menu opens and closes. Each one is a row here (a
//! variant of `Panel`, its place in `ALL`, its title, the arm that draws its
//! body) and a file of its own for that body.
//!
//! Adding a panel:
//!
//! 1. A variant of `Panel`, listed in `ALL`. The order of `ALL` is the order
//!    of the View menu.
//! 2. Its arm in `title` and in `body`: the compiler asks for both.
//! 3. The body, in a file beside this one:
//!    `pub(super) fn body(ui: &mut egui::Ui, cx: &mut Cx<'_>)`. It reaches
//!    the machine through `cx.runtime`, asks the app for what only the app
//!    can do through `cx.commands`, and says what it did or why it failed
//!    through `cx.messages`. A colour is a slot of `theme::PALETTE`.
//!
//! The checkbox in the View menu, the window, its title bar and its close
//! button all follow from `ALL`: the shell knows no panel by name.
//!
//! The invariant: `ALL` holds every variant, once. It is the one list the
//! compiler cannot complete by itself, but it does notice: a variant left
//! out of `ALL` is built nowhere, which is a warning in a build that allows
//! none. The test of the table names every variant too, and does not
//! compile until a new one is named there:
//! `every_panel_has_a_title_a_checkbox_and_a_window`.

use super::{Cx, about, debug_port, keyboard, memory, pic, pit, stats, vga};

/// The panels of the overlay.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Panel {
    About,
    Stats,
    Memory,
    Interrupts,
    Timer,
    Video,
    Keyboard,
    DebugPort,
}

impl Panel {
    /// Every panel, in the order of the View menu.
    pub const ALL: [Self; 8] = [
        Self::About,
        Self::Stats,
        Self::Memory,
        Self::Interrupts,
        Self::Timer,
        Self::Video,
        Self::Keyboard,
        Self::DebugPort,
    ];

    /// What the View menu and the window's title bar call the panel. The
    /// window is found again by its title from frame to frame, so two
    /// panels never share one.
    pub fn title(self) -> &'static str {
        match self {
            Self::About => "About",
            Self::Stats => "Stats",
            Self::Memory => "Memory",
            Self::Interrupts => "Interrupts (8259A)",
            Self::Timer => "Timer (8253)",
            Self::Video => "Video (VGA)",
            Self::Keyboard => "Keyboard (8255)",
            Self::DebugPort => "Port E9h log",
        }
    }

    /// Draws the panel's contents into its window.
    pub(super) fn body(self, ui: &mut egui::Ui, cx: &mut Cx<'_>) {
        match self {
            Self::About => about::body(ui, cx),
            Self::Stats => stats::body(ui, cx),
            Self::Memory => memory::body(ui, cx),
            Self::Interrupts => pic::body(ui, cx),
            Self::Timer => pit::body(ui, cx),
            Self::Video => vga::body(ui, cx),
            Self::Keyboard => keyboard::body(ui, cx),
            Self::DebugPort => debug_port::body(ui, cx),
        }
    }
}
