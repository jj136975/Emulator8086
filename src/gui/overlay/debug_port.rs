//! The Port E9h panel: the lines the guest wrote to the debug port, the
//! newest at the bottom, and the line it is writing. The BIOS writes its
//! progress markers there, and a copy of what it prints through INT 10h's
//! teletype. It only reads.
//!
//! The invariant: a frame lays out the lines on the screen and no others
//! (`ScrollArea::show_rows`), however many the console keeps.

use std::iter::once;

use egui::{Label, RichText, ScrollArea, TextStyle, Ui};

use emu8086_core::io::console::DebugConsole;

use super::Cx;

/// Lines on the screen at a time.
const LINES_SHOWN: usize = 12;
/// Characters of a line on the screen; a longer one scrolls sideways.
const COLUMNS_SHOWN: usize = 56;

/// How many lines the guest finished, and how many of them are kept.
fn count(console: &DebugConsole) -> String {
    let kept = console.lines().len();
    match console.lines_finished() {
        0 => "No line written yet".to_string(),
        1 => "1 line written".to_string(),
        written if written > kept as u64 => {
            format!("{written} lines written, the last {kept} kept")
        }
        written => format!("{written} lines written"),
    }
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    let Some(console) = cx.runtime.debug_console() else {
        ui.label("This machine has no debug port");
        return;
    };
    let in_progress = console.line_in_progress();
    // The line being written is the last row, from its first character.
    let rows = console.lines().len() + usize::from(!in_progress.is_empty());

    ui.scope(|ui| {
        // The rows touch, as the lines of a terminal do.
        ui.spacing_mut().item_spacing.y = 0.0;
        let row_height = ui.text_style_height(&TextStyle::Monospace);
        let font = TextStyle::Monospace.resolve(ui.style());
        let column_width = ui.fonts_mut(|fonts| fonts.glyph_width(&font, '0'));
        ScrollArea::both()
            .id_salt("debug_port_lines")
            .max_height(LINES_SHOWN as f32 * row_height)
            .max_width(COLUMNS_SHOWN as f32 * column_width)
            .auto_shrink(false)
            .stick_to_bottom(true)
            .show_rows(ui, row_height, rows, |ui, shown| {
                let lines = console.lines().chain(once(in_progress)).take(rows);
                for line in lines.skip(shown.start).take(shown.len()) {
                    ui.add(Label::new(RichText::new(line).monospace()).extend());
                }
            });
    });
    ui.label(count(&console));
}

#[cfg(test)]
#[path = "debug_port_tests.rs"]
mod tests;
