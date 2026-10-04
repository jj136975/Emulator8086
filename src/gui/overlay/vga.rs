//! The VGA panel: the display as the VGA's registers set it, the way a
//! reader of the adapter's documentation looks for it first: the video
//! mode, where the displayed page starts, the cursor and its shape, what
//! bit 7 of an attribute does, the registers these come from, and the 256
//! entries of the DAC as swatches of their colours. It only reads.
//!
//! The invariant: the panel reads the adapter's state
//! (`VgaDevice::registers`, `VgaDevice::dac_colour`), never a port. A read
//! of 3C9h moves the guest's DAC read index, and a read of 3DAh resets the
//! attribute controller's flip-flop.

use egui::{Color32, Grid, Rect, Sense, Stroke, StrokeKind, Ui, Vec2, vec2};

use emu8086_core::io::vga::{VgaDevice, VgaRegisters, VideoMode};
use emu8086_core::vm::memory::Memory;

use super::Cx;
use super::theme::{METRICS, PALETTE};

/// Where the BIOS keeps the number of the video mode it set, 0040:0049.
const BIOS_MODE: usize = 0x0449;
/// Entries of the DAC to a row of swatches.
const DAC_ROW: usize = 16;

/// The state of the display, each part named and set out in words.
fn state(vga: &VgaDevice, memory: &Memory) -> Vec<(&'static str, String)> {
    let cols = vga.cols();
    let text = vga.video_mode == VideoMode::Text80x25;
    let mode = if text {
        format!("text, {cols} columns")
    } else {
        "graphics, 320x200, 256 colours".to_string()
    };
    let cursor = match vga.visible_cursor_cell() {
        _ if !text => "not drawn in graphics".to_string(),
        Some(cell) => format!("row {}, column {}", cell / cols, cell % cols),
        None if vga.cursor_shape().is_none() => "hidden".to_string(),
        None => "off the page".to_string(),
    };
    let shape = match vga.cursor_shape() {
        Some((start, end)) => format!("lines {start} to {end}"),
        None => "hidden".to_string(),
    };
    let bit_7 = if vga.blink_mode {
        "blinks the character"
    } else {
        "brightens the background"
    };
    vec![
        ("Mode", mode),
        ("BIOS mode", format!("{:02X}h", memory.read_byte(BIOS_MODE))),
        ("Display start", format!("{:04X}h", vga.start_address())),
        ("Cursor", cursor),
        ("Cursor shape", shape),
        ("Attribute bit 7", bit_7.to_string()),
    ]
}

/// The registers a reader of the VGA looks at first: where each is, by
/// its port or its group and index, its name, and what it holds.
fn registers(registers: &VgaRegisters) -> [(&'static str, &'static str, u8); 11] {
    [
        ("3C2h", "Miscellaneous output", registers.misc_output),
        ("3D8h", "CGA mode control", registers.cga_mode),
        ("SR02", "Map mask", registers.sequencer[2]),
        ("SR04", "Memory mode", registers.sequencer[4]),
        ("GR05", "Graphics mode", registers.graphics[5]),
        ("GR06", "Miscellaneous", registers.graphics[6]),
        ("AR10", "Attribute mode control", registers.attribute[0x10]),
        ("CR09", "Maximum scan line", registers.crtc[0x09]),
        ("CR13", "Offset", registers.crtc[0x13]),
        ("CR17", "CRTC mode control", registers.crtc[0x17]),
        ("3C6h", "DAC pixel mask", registers.dac_mask),
    ]
}

/// The 256 entries of the DAC as swatches of their colours, sixteen to a
/// row from entry 0 at the top left. The entry under the pointer is
/// outlined and named, with its three 6-bit components.
fn dac(ui: &mut Ui, vga: &VgaDevice) {
    let pitch = METRICS.swatch + METRICS.line;
    let side = DAC_ROW as f32 * pitch - METRICS.line;
    let (area, response) = ui.allocate_exact_size(Vec2::splat(side), Sense::hover());
    let swatch = |index: usize| {
        let cell = vec2((index % DAC_ROW) as f32, (index / DAC_ROW) as f32);
        Rect::from_min_size(area.min + cell * pitch, Vec2::splat(METRICS.swatch))
    };
    let painter = ui.painter();
    for index in 0..=u8::MAX {
        let [r, g, b] = vga.dac_colour(index);
        painter.rect_filled(swatch(index.into()), 0.0, Color32::from_rgb(r, g, b));
    }

    let Some(pointer) = response.hover_pos() else {
        return;
    };
    let cell = ((pointer - area.min) / pitch).floor();
    let last = DAC_ROW - 1;
    let index = (cell.y as usize).min(last) * DAC_ROW + (cell.x as usize).min(last);
    painter.rect_stroke(
        swatch(index),
        0.0,
        Stroke::new(METRICS.strong_line, PALETTE.text_strong),
        StrokeKind::Outside,
    );
    let [r, g, b] = vga.dac_palette[index];
    response.on_hover_text_at_pointer(format!("Entry {index:02X}h: {r:02X} {g:02X} {b:02X}"));
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    let Some(vga) = cx.runtime.vga.as_ref() else {
        ui.label("This machine has no VGA");
        return;
    };
    let vga = vga.borrow();
    let memory = &cx.runtime.cpu.memory;
    let gap = ui.spacing().item_spacing;

    ui.horizontal_top(|ui| {
        ui.vertical(|ui| {
            Grid::new("vga_state").num_columns(2).show(ui, |ui| {
                for (name, value) in state(&vga, memory) {
                    ui.label(name);
                    ui.strong(value);
                    ui.end_row();
                }
            });
            ui.add_space(gap.y * 2.0);
            ui.strong("DAC");
            dac(ui, &vga);
        });
        ui.add_space(gap.x * 2.0);
        Grid::new("vga_registers")
            .num_columns(3)
            .striped(true)
            .show(ui, |ui| {
                for (place, name, value) in registers(&vga.registers()) {
                    ui.monospace(place);
                    ui.label(name);
                    ui.monospace(format!("{value:02X}h"));
                    ui.end_row();
                }
            });
    });
}

#[cfg(test)]
#[path = "vga_tests.rs"]
mod tests;
