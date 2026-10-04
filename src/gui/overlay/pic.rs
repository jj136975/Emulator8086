//! The 8259A panel: the interrupt controller as its datasheet describes it.
//! Its eight lines, each named for what the PC/XT wires to it, with their
//! bits in the request, in-service and mask registers; the vector base its
//! initialisation set, and the modes in force. It only reads.
//!
//! The invariant: the panel reads the controller's view (`Pic::view`), never
//! a port. A read of port 20h would answer a poll the guest is waiting for.

use egui::{Grid, RichText, Ui};

use emu8086_core::io::pic::PicView;

use super::Cx;
use super::theme::PALETTE;

/// What the PC/XT wires to each line, IRQ0 to IRQ7, as its technical
/// reference lists them.
const DEVICES: [&str; 8] = [
    "timer",
    "keyboard",
    "reserved",
    "COM2",
    "COM1",
    "hard disk",
    "floppy",
    "printer",
];

/// What a cell of the table shows for a bit that is clear.
const CLEAR: &str = "—";

/// One line of the controller, as a row of the panel's table.
#[derive(Debug, PartialEq, Eq)]
struct Line {
    name: String,
    device: &'static str,
    vector: String,
    requested: bool,
    in_service: bool,
    masked: bool,
}

/// The eight lines, IRQ0 first.
fn lines(view: &PicView) -> Vec<Line> {
    (0..8u8)
        .map(|irq| {
            let bit = 1 << irq;
            Line {
                name: format!("IRQ{irq}"),
                device: DEVICES[irq as usize],
                vector: format!("{:02X}h", view.vector_base.wrapping_add(irq)),
                requested: view.request & bit != 0,
                in_service: view.in_service & bit != 0,
                masked: view.mask & bit != 0,
            }
        })
        .collect()
}

/// The modes in force, each named and set out in words: what ICW1 to ICW4
/// and the operation command words left in the controller.
fn modes(view: &PicView) -> Vec<(&'static str, String)> {
    let mut modes = vec![
        ("Vector base", format!("{:02X}h", view.vector_base)),
        (
            "Triggering",
            if view.level_triggered {
                "level"
            } else {
                "edge"
            }
            .to_string(),
        ),
        (
            "End of interrupt",
            match (view.auto_eoi, view.rotate_on_auto_eoi) {
                (false, _) => "by command",
                (true, false) => "automatic",
                (true, true) => "automatic, rotating",
            }
            .to_string(),
        ),
        (
            "Nesting",
            if view.special_mask {
                "special mask"
            } else {
                "fully nested"
            }
            .to_string(),
        ),
        ("Priority", format!("IRQ{} highest", view.highest_priority)),
        (
            "Port 20h reads",
            if view.reads_in_service { "ISR" } else { "IRR" }.to_string(),
        ),
        (
            "Set up for",
            if view.single {
                "a single controller"
            } else {
                "a cascade, without slave"
            }
            .to_string(),
        ),
    ];
    if let Some(word) = view.awaiting_icw {
        modes.push(("Initialisation", format!("waiting for ICW{word}")));
    }
    if view.poll {
        modes.push(("Poll", "waiting for its read".to_string()));
    }
    modes
}

/// A cell for a bit: `set` in the colour of an active line, or the mark of
/// a clear bit.
fn bit(set: bool, set_text: &str) -> RichText {
    if set {
        RichText::new(set_text).color(PALETTE.on)
    } else {
        RichText::new(CLEAR).color(PALETTE.off)
    }
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    let view = cx.runtime.cpu.pic.view();

    Grid::new("pic_modes").num_columns(4).show(ui, |ui| {
        for (index, (name, setting)) in modes(&view).into_iter().enumerate() {
            ui.label(name);
            ui.strong(setting);
            if index % 2 == 1 {
                ui.end_row();
            }
        }
    });
    // A gap, not a separator: a separator spans the window, and the window
    // takes the size of its contents.
    ui.add_space(ui.spacing().item_spacing.y * 2.0);

    Grid::new("pic_lines")
        .num_columns(6)
        .striped(true)
        .show(ui, |ui| {
            for heading in ["Line", "Device", "Vector"] {
                ui.strong(heading);
            }
            // Each register heads the column of its bits, with its byte.
            for (register, byte) in registers(&view) {
                ui.strong(format!("{register} {byte:02X}h"));
            }
            ui.end_row();
            for line in lines(&view) {
                ui.label(&line.name);
                ui.label(line.device);
                ui.monospace(&line.vector);
                ui.label(bit(line.requested, "requested"));
                ui.label(bit(line.in_service, "in service"));
                if line.masked {
                    ui.colored_label(PALETTE.masked, "masked");
                } else {
                    ui.label("unmasked");
                }
                ui.end_row();
            }
        });
}

/// The three registers, by their datasheet names, with their bytes.
fn registers(view: &PicView) -> [(&'static str, u8); 3] {
    [
        ("IRR", view.request),
        ("ISR", view.in_service),
        ("IMR", view.mask),
    ]
}

#[cfg(test)]
#[path = "pic_tests.rs"]
mod tests;
