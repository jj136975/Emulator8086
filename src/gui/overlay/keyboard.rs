//! The Keyboard panel: the PC/XT keyboard interface on the 8255 as its
//! ports show it: the byte in port A, whether the keyboard waits for the
//! guest to acknowledge it, the scancodes queued behind it, and port B's
//! bits by name. It only reads.
//!
//! The invariant: the panel reads the interface's view
//! (`KeyboardController::view`), never a port: a read of port 61h flips
//! its bit 4 for the guest.

use egui::{Grid, RichText, Ui};

use emu8086_core::io::keyboard::KeyboardView;

use super::Cx;
use super::theme::PALETTE;

/// Port B's bits, PB0 first, each named for what it does when it is set,
/// as the PC/XT's technical reference describes them.
const PORT_B: [&str; 8] = [
    "timer 2 gate",
    "speaker data",
    "spare",
    "read high switches",
    "RAM parity check off",
    "I/O channel check off",
    "keyboard clock enabled",
    "keyboard held clear",
];

/// What the byte in port A says, read as a scancode of set 1: the prefix
/// of an extended key, a key's make code, or its break code, the make code
/// with bit 7 set. A zero that waits for no acknowledge is no key at all:
/// the acknowledge of the last one cleared the port.
fn scancode(byte: u8, awaiting: bool) -> String {
    match byte {
        0x00 if !awaiting => "00h, empty".to_string(),
        0xE0 | 0xE1 => format!("{byte:02X}h, prefix"),
        _ if byte & 0x80 != 0 => format!("{byte:02X}h, break of {:02X}h", byte & 0x7F),
        _ => format!("{byte:02X}h, make"),
    }
}

/// The state of the interface, each part named and set out in words, in
/// the colour of what it says: active, held back, or at rest.
fn state(view: &KeyboardView) -> Vec<(&'static str, RichText)> {
    let on = |text: &str| RichText::new(text).color(PALETTE.on);
    let held = |text: &str| RichText::new(text).color(PALETTE.masked);
    let plain = |text: &str| RichText::new(text);
    let mut state = vec![
        (
            "Port A (60h)",
            RichText::new(scancode(view.port_a, view.awaiting_acknowledge)).monospace(),
        ),
        (
            "Acknowledge",
            if view.awaiting_acknowledge {
                on("awaited")
            } else {
                plain("not awaited")
            },
        ),
        (
            "Queued",
            plain(&match view.queued {
                0 => "nothing".to_string(),
                1 => "1 scancode".to_string(),
                queued => format!("{queued} scancodes"),
            }),
        ),
        (
            "Keyboard",
            if view.held_clear {
                held("held clear")
            } else {
                plain("enabled")
            },
        ),
        (
            "Clock",
            if view.clock_held_low {
                held("held low")
            } else {
                plain("running")
            },
        ),
        (
            "Port B (61h)",
            RichText::new(format!("{:02X}h", view.port_b)).monospace(),
        ),
    ];
    if view.reset_pending {
        state.push(("Reset", on("AAh due at the next poll")));
    }
    state
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    let Some(keyboard) = cx.runtime.keyboard() else {
        ui.label("This machine has no keyboard");
        return;
    };
    let view = keyboard.view();

    ui.horizontal_top(|ui| {
        Grid::new("keyboard_state").num_columns(2).show(ui, |ui| {
            for (name, value) in state(&view) {
                ui.label(name);
                ui.label(value);
                ui.end_row();
            }
        });
        ui.add_space(ui.spacing().item_spacing.x * 2.0);
        Grid::new("keyboard_port_b")
            .num_columns(3)
            .striped(true)
            .show(ui, |ui| {
                for (bit, name) in PORT_B.iter().enumerate() {
                    let set = view.port_b & 1 << bit != 0;
                    ui.monospace(format!("PB{bit}"));
                    ui.label(*name);
                    ui.label(if set {
                        RichText::new("1").monospace().color(PALETTE.on)
                    } else {
                        RichText::new("0").monospace().color(PALETTE.off)
                    });
                    ui.end_row();
                }
            });
    });
}

#[cfg(test)]
#[path = "keyboard_tests.rs"]
mod tests;
