//! The Memory panel: a hex view of guest memory, sixteen bytes to a row
//! with their characters beside them, as DEBUG's `d` shows them. The
//! address field takes a physical address (`B8000`) or a segment and an
//! offset (`B800:0000`); buttons jump to where the register pairs point,
//! whose values they show, and to the landmarks of the first megabyte. The
//! byte a jump went to is marked. It only reads.
//!
//! The rows are addressed the way the view was entered: a segment's rows by
//! their offset (`B800:0010`), over the segment's 64 KB; physical rows by
//! their address (`B8010`), over the megabyte.
//!
//! The invariant: a frame reads the rows on the screen and no others. The
//! scroll area hands the panel the rows it shows (`ScrollArea::show_rows`):
//! a few hundred bytes a frame wherever the view stands, while the guest
//! runs on between the frames.

use std::fmt;

use egui::text::{LayoutJob, TextFormat};
use egui::{Key, Label, ScrollArea, TextEdit, TextStyle, Ui};

use emu8086_core::vm::memory::{Memory, Segment};
use emu8086_core::vm::registers::Registers;

use super::Cx;
use super::theme::PALETTE;

/// Bytes to a row: a paragraph, so that the rows of a segment start at the
/// offsets a segment register can point to.
const ROW_BYTES: u32 = 16;
/// Rows on the screen at a time.
const ROWS_SHOWN: usize = 16;
/// The 8086 addresses a megabyte; a physical address wraps at its end.
const MEGABYTE: u32 = 1 << 20;
/// What a segment spans from its base.
const SEGMENT_BYTES: u32 = 1 << 16;
/// The name of the address field, and what it shows while it is empty,
/// which sets its width.
const FIELD: &str = "Address";
const FIELD_HINT: &str = "B8000 or B800:0000";
/// Characters the address field takes, spaces included.
const FIELD_CHARS: usize = 16;

/// The places of the first megabyte a reader looks at most: what the
/// button says, the segment it jumps to, and what is found there.
const LANDMARKS: [(&str, u16, &str); 5] = [
    ("IVT", 0x0000, "The interrupt vector table, 0000:0000"),
    ("BDA", 0x0040, "The BIOS data area, 0040:0000"),
    ("Graphics", 0xA000, "The VGA's graphics memory, A000:0000"),
    ("Text", 0xB800, "The VGA's text memory, B800:0000"),
    ("ROM", 0xF000, "The BIOS ROM, F000:0000"),
];

/// An address of the first megabyte, as the field takes it or a register
/// pair points.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Address {
    /// A physical address, below 100000h.
    Physical(u32),
    /// An offset into a segment.
    Segmented { segment: u16, offset: u16 },
}

impl Address {
    /// The physical address. A segment's base and offset add up to 20 bits
    /// and wrap at the end of the megabyte, as on the 8086.
    pub(super) fn physical(self) -> u32 {
        match self {
            Self::Physical(physical) => physical,
            Self::Segmented { segment, offset } => {
                ((u32::from(segment) << 4) + u32::from(offset)) % MEGABYTE
            }
        }
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Physical(physical) => write!(f, "{physical:05X}"),
            Self::Segmented { segment, offset } => write!(f, "{segment:04X}:{offset:04X}"),
        }
    }
}

/// Reads an address as the field takes it, in hexadecimal: `B8000`, or a
/// segment and an offset, `B800:0000`. The error says what is wrong with
/// `text`, for the user.
pub(super) fn parse_address(text: &str) -> Result<Address, String> {
    let typed = text.trim();
    let not_one = |reason: String| format!("{typed} is not an address: {reason}");
    // A number of at most `most` digits, `what` for the error.
    let number = |digits: &str, most: usize, what: &str| -> Result<u32, String> {
        if digits.is_empty() {
            return Err(not_one(format!("the {what} is missing")));
        }
        if let Some(stray) = digits.chars().find(|c| !c.is_ascii_hexdigit()) {
            return Err(not_one(format!("{stray} is not a hexadecimal digit")));
        }
        if digits.len() > most {
            return Err(not_one(format!("the {what} has more than {most} digits")));
        }
        u32::from_str_radix(digits, 16).map_err(|error| not_one(error.to_string()))
    };

    if typed.is_empty() {
        return Err(format!("Type an address: {FIELD_HINT}"));
    }
    match typed.split_once(':') {
        Some((segment, offset)) => Ok(Address::Segmented {
            segment: number(segment.trim(), 4, "segment")? as u16,
            offset: number(offset.trim(), 4, "offset")? as u16,
        }),
        None => Ok(Address::Physical(number(typed, 5, "physical address")?)),
    }
}

/// The register pairs that point into memory, named as DEBUG names them,
/// and where each points.
fn pointers(registers: &Registers) -> [(&'static str, Address); 4] {
    let pair = |segment: &Segment, offset: u16| Address::Segmented {
        segment: segment.reg().word(),
        offset,
    };
    [
        ("CS:IP", pair(&registers.cs, registers.pc.word())),
        ("SS:SP", pair(&registers.ss, registers.sp.word())),
        ("DS:SI", pair(&registers.ds, registers.si.word())),
        ("ES:DI", pair(&registers.es, registers.di.word())),
    ]
}

/// What the panel keeps from frame to frame, in egui's memory.
#[derive(Clone, Debug, PartialEq, Eq)]
struct View {
    /// What the address field holds.
    field: String,
    /// The segment the rows are counted in, or none for physical rows.
    segment: Option<u16>,
    /// The physical address of the byte the last jump went to.
    mark: Option<u32>,
    /// A jump the scroll area is yet to make: the row to bring to the top.
    scroll_to: Option<usize>,
}

impl Default for View {
    /// The vector table, until the reader goes elsewhere.
    fn default() -> Self {
        Self {
            field: String::new(),
            segment: Some(0x0000),
            mark: None,
            scroll_to: None,
        }
    }
}

impl View {
    /// Shows the rows around `address`, its row at the top, and marks it.
    fn go(&mut self, address: Address) {
        let (segment, row) = match address {
            Address::Physical(physical) => (None, physical / ROW_BYTES),
            Address::Segmented { segment, offset } => {
                (Some(segment), u32::from(offset) / ROW_BYTES)
            }
        };
        self.segment = segment;
        self.mark = Some(address.physical());
        self.scroll_to = Some(row as usize);
    }

    /// How many rows the view scrolls over.
    fn rows(&self) -> usize {
        let bytes = match self.segment {
            Some(_) => SEGMENT_BYTES,
            None => MEGABYTE,
        };
        (bytes / ROW_BYTES) as usize
    }

    /// The address row `row` starts at.
    fn row_address(&self, row: usize) -> Address {
        let start = row as u32 * ROW_BYTES;
        match self.segment {
            Some(segment) => Address::Segmented {
                segment,
                offset: start as u16,
            },
            None => Address::Physical(start),
        }
    }
}

/// Where the two digits of byte `index` stand in the text of a row whose
/// address is `address_chars` long, and where its character stands.
fn columns(address_chars: usize, index: usize) -> (usize, usize) {
    let hex = address_chars + 2 + index * 3;
    let character = address_chars + 2 + ROW_BYTES as usize * 3 + 1 + index;
    (hex, character)
}

/// The row of memory that starts at `start`: its address, its bytes in
/// hexadecimal with a dash between the two halves, and their characters,
/// a dot for a byte that is no printable ASCII. A row of a segment that
/// runs past the end of the megabyte wraps to its start, as on the 8086.
fn row_text(memory: &Memory, start: Address) -> String {
    let base = start.physical();
    let bytes: Vec<u8> = (0..ROW_BYTES)
        .map(|index| memory.read_byte(((base + index) % MEGABYTE) as usize))
        .collect();
    let mut text = format!("{start} ");
    for (index, byte) in bytes.iter().enumerate() {
        let gap = if index == ROW_BYTES as usize / 2 {
            '-'
        } else {
            ' '
        };
        text.push(gap);
        text.push_str(&format!("{byte:02X}"));
    }
    text.push_str("  ");
    text.extend(bytes.iter().map(|&byte| {
        if (0x20..0x7F).contains(&byte) {
            byte as char
        } else {
            '.'
        }
    }));
    text
}

/// The text of a row laid out for the screen in the format `plain`, and
/// the marked byte, if it is in the row, drawn as a selection: its digits
/// and its character.
fn row_job(
    text: &str,
    address_chars: usize,
    marked: Option<usize>,
    plain: TextFormat,
) -> LayoutJob {
    let selected = TextFormat {
        color: PALETTE.text_strong,
        background: PALETTE.accent,
        ..plain.clone()
    };
    let mut job = LayoutJob::default();
    let Some(index) = marked else {
        job.append(text, 0.0, plain);
        return job;
    };
    let (hex, character) = columns(address_chars, index);
    for (span, format) in [
        (&text[..hex], &plain),
        (&text[hex..hex + 2], &selected),
        (&text[hex + 2..character], &plain),
        (&text[character..character + 1], &selected),
        (&text[character + 1..], &plain),
    ] {
        job.append(span, 0.0, format.clone());
    }
    job
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    let id = ui.make_persistent_id("memory_view");
    let mut view = ui.data_mut(|data| data.get_temp_mut_or_default::<View>(id).clone());

    ui.horizontal(|ui| {
        ui.label(FIELD);
        let field = ui.add(
            TextEdit::singleline(&mut view.field)
                .hint_text(FIELD_HINT)
                .font(TextStyle::Monospace)
                .char_limit(FIELD_CHARS)
                .desired_width(FIELD_HINT.len() as f32 * monospace_width(ui)),
        );
        // A screen reader names the field as the label beside it does.
        ui.ctx()
            .accesskit_node_builder(field.id, |node| node.set_label(FIELD));
        let entered = field.lost_focus() && ui.input(|input| input.key_pressed(Key::Enter));
        if ui.button("Go").clicked() || entered {
            match parse_address(&view.field) {
                Ok(address) => view.go(address),
                Err(error) => cx.messages.failed(error),
            }
        }
        ui.separator();
        for (name, segment, place) in LANDMARKS {
            if ui.button(name).on_hover_text(place).clicked() {
                view.go(Address::Segmented { segment, offset: 0 });
            }
        }
    });

    ui.horizontal(|ui| {
        for (name, address) in pointers(&cx.runtime.cpu.registers) {
            if ui
                .button(name)
                .on_hover_text(format!("Show where {name} points"))
                .clicked()
            {
                view.go(address);
            }
            ui.monospace(address.to_string());
        }
    });
    // A gap, not a separator: a separator spans the window, and the window
    // takes the size of its contents.
    ui.add_space(ui.spacing().item_spacing.y * 2.0);

    // The rows touch, as the lines of a dump do.
    ui.spacing_mut().item_spacing.y = 0.0;
    let row_height = ui.text_style_height(&TextStyle::Monospace);
    let mut rows = ScrollArea::vertical()
        .id_salt("memory_rows")
        .max_height(ROWS_SHOWN as f32 * row_height);
    if let Some(row) = view.scroll_to.take() {
        rows = rows.vertical_scroll_offset(row as f32 * row_height);
    }
    let memory = &cx.runtime.cpu.memory;
    let plain = TextFormat::simple(
        TextStyle::Monospace.resolve(ui.style()),
        ui.visuals().text_color(),
    );
    rows.show_rows(ui, row_height, view.rows(), |ui, shown| {
        for row in shown {
            let start = view.row_address(row);
            let address_chars = start.to_string().len();
            let marked = view.mark.and_then(|mark| {
                let index = mark.wrapping_sub(start.physical()) % MEGABYTE;
                (index < ROW_BYTES).then_some(index as usize)
            });
            let text = row_text(memory, start);
            let job = row_job(&text, address_chars, marked, plain.clone());
            ui.add(Label::new(job).extend());
        }
    });

    ui.data_mut(|data| data.insert_temp(id, view));
}

/// The width of a character of the monospace font.
fn monospace_width(ui: &Ui) -> f32 {
    let font = TextStyle::Monospace.resolve(ui.style());
    ui.fonts_mut(|fonts| fonts.glyph_width(&font, '0'))
}

#[cfg(test)]
#[path = "memory_tests.rs"]
mod tests;
