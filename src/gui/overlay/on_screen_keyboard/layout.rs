//! THE key table of the on-screen keyboard: the 83 keys of the PC/XT
//! keyboard by their make codes, where each lies, and what is printed on
//! it, as on a US keyboard. The keys are laid out as on a keyboard without
//! a numeric pad: the main block with Esc and F1-F10 above the digits, and
//! beside it the cursor keys, the arrows in an inverted T. An XT has one
//! code for an arrow and a digit of its keypad, so an arrow is the keypad's
//! 8, 4, 6 or 2, as `gui::keys` maps the host's keys; the keys of the XT's
//! keypad that such a keyboard has no place for lie above the arrows, and
//! Num Lock, Scroll Lock and PrtSc (the keypad's `*`) on the top row.
//!
//! The invariant: each make code from 01h to 53h is in the table once, and
//! every row is `WIDTH` quarters long. The tests read the table for both.

use egui::{Pos2, Rect, pos2, vec2};

/// What a row holds where there is no key.
const GAP: u8 = 0;

/// How long each row is, in quarters of the width of a key that types a
/// letter: the main block (60), a gap (2) and the cursor keys (12).
pub(super) const WIDTH: u32 = 74;

/// How many keys that type a letter would fill a row.
pub(super) const KEYS_ACROSS: f32 = WIDTH as f32 / 4.0;

/// THE key table: the rows, top to bottom, each a run of (make code,
/// width in quarters of a key that types a letter) from the left edge. A
/// make code of `GAP` is room without a key.
pub(super) const ROWS: [&[(u8, u8)]; 6] = [
    // Esc, F1-F10; PrtSc, Scroll Lock, Num Lock.
    &[
        (0x01, 4),
        (GAP, 4),
        (0x3B, 4),
        (0x3C, 4),
        (0x3D, 4),
        (0x3E, 4),
        (GAP, 2),
        (0x3F, 4),
        (0x40, 4),
        (0x41, 4),
        (0x42, 4),
        (GAP, 2),
        (0x43, 4),
        (0x44, 4),
        (GAP, 10),
        (0x37, 4),
        (0x46, 4),
        (0x45, 4),
    ],
    // ` 1-0 - = Backspace; Ins Home PgUp.
    &[
        (0x29, 4),
        (0x02, 4),
        (0x03, 4),
        (0x04, 4),
        (0x05, 4),
        (0x06, 4),
        (0x07, 4),
        (0x08, 4),
        (0x09, 4),
        (0x0A, 4),
        (0x0B, 4),
        (0x0C, 4),
        (0x0D, 4),
        (0x0E, 8),
        (GAP, 2),
        (0x52, 4),
        (0x47, 4),
        (0x49, 4),
    ],
    // Tab Q-P [ ] \; Del End PgDn.
    &[
        (0x0F, 6),
        (0x10, 4),
        (0x11, 4),
        (0x12, 4),
        (0x13, 4),
        (0x14, 4),
        (0x15, 4),
        (0x16, 4),
        (0x17, 4),
        (0x18, 4),
        (0x19, 4),
        (0x1A, 4),
        (0x1B, 4),
        (0x2B, 6),
        (GAP, 2),
        (0x53, 4),
        (0x4F, 4),
        (0x51, 4),
    ],
    // Caps Lock A-L ; ' Enter; the keypad's - 5 +.
    &[
        (0x3A, 7),
        (0x1E, 4),
        (0x1F, 4),
        (0x20, 4),
        (0x21, 4),
        (0x22, 4),
        (0x23, 4),
        (0x24, 4),
        (0x25, 4),
        (0x26, 4),
        (0x27, 4),
        (0x28, 4),
        (0x1C, 9),
        (GAP, 2),
        (0x4A, 4),
        (0x4C, 4),
        (0x4E, 4),
    ],
    // Shift Z-M , . / Shift; Up.
    &[
        (0x2A, 9),
        (0x2C, 4),
        (0x2D, 4),
        (0x2E, 4),
        (0x2F, 4),
        (0x30, 4),
        (0x31, 4),
        (0x32, 4),
        (0x33, 4),
        (0x34, 4),
        (0x35, 4),
        (0x36, 11),
        (GAP, 6),
        (0x48, 4),
        (GAP, 4),
    ],
    // Ctrl Alt Space; Left Down Right.
    &[
        (0x1D, 8),
        (0x38, 8),
        (0x39, 44),
        (GAP, 2),
        (0x4B, 4),
        (0x50, 4),
        (0x4D, 4),
    ],
];

/// The key whose make code is `make`: its name, which a screen reader is
/// told, and its legend, what is printed on it, a line each (the shifted
/// sign above the plain one). A name is what the key bears, unless two keys
/// bear the same (the Shifts, the minus and plus of the keypad) or what it
/// bears is a sign of no word (the arrows): then it says which. The arrows
/// are triangles, which egui's own fonts have where they have no arrow.
/// `None` for a code no key has.
pub(super) fn label(make: u8) -> Option<(&'static str, &'static str)> {
    Some(match make {
        0x01 => ("Esc", "Esc"),
        0x02 => ("1", "!\n1"),
        0x03 => ("2", "@\n2"),
        0x04 => ("3", "#\n3"),
        0x05 => ("4", "$\n4"),
        0x06 => ("5", "%\n5"),
        0x07 => ("6", "^\n6"),
        0x08 => ("7", "&\n7"),
        0x09 => ("8", "*\n8"),
        0x0A => ("9", "(\n9"),
        0x0B => ("0", ")\n0"),
        0x0C => ("-", "_\n-"),
        0x0D => ("=", "+\n="),
        0x0E => ("Backspace", "Backspace"),
        0x0F => ("Tab", "Tab"),
        0x10 => ("Q", "Q"),
        0x11 => ("W", "W"),
        0x12 => ("E", "E"),
        0x13 => ("R", "R"),
        0x14 => ("T", "T"),
        0x15 => ("Y", "Y"),
        0x16 => ("U", "U"),
        0x17 => ("I", "I"),
        0x18 => ("O", "O"),
        0x19 => ("P", "P"),
        0x1A => ("[", "{\n["),
        0x1B => ("]", "}\n]"),
        0x1C => ("Enter", "Enter"),
        0x1D => ("Ctrl", "Ctrl"),
        0x1E => ("A", "A"),
        0x1F => ("S", "S"),
        0x20 => ("D", "D"),
        0x21 => ("F", "F"),
        0x22 => ("G", "G"),
        0x23 => ("H", "H"),
        0x24 => ("J", "J"),
        0x25 => ("K", "K"),
        0x26 => ("L", "L"),
        0x27 => (";", ":\n;"),
        0x28 => ("'", "\"\n'"),
        0x29 => ("`", "~\n`"),
        0x2A => ("Left Shift", "Shift"),
        0x2B => ("\\", "|\n\\"),
        0x2C => ("Z", "Z"),
        0x2D => ("X", "X"),
        0x2E => ("C", "C"),
        0x2F => ("V", "V"),
        0x30 => ("B", "B"),
        0x31 => ("N", "N"),
        0x32 => ("M", "M"),
        0x33 => (",", "<\n,"),
        0x34 => (".", ">\n."),
        0x35 => ("/", "?\n/"),
        0x36 => ("Right Shift", "Shift"),
        0x37 => ("PrtSc", "PrtSc\n*"),
        0x38 => ("Alt", "Alt"),
        0x39 => ("Space", ""),
        0x3A => ("Caps Lock", "Caps\nLock"),
        0x3B => ("F1", "F1"),
        0x3C => ("F2", "F2"),
        0x3D => ("F3", "F3"),
        0x3E => ("F4", "F4"),
        0x3F => ("F5", "F5"),
        0x40 => ("F6", "F6"),
        0x41 => ("F7", "F7"),
        0x42 => ("F8", "F8"),
        0x43 => ("F9", "F9"),
        0x44 => ("F10", "F10"),
        0x45 => ("Num Lock", "Num\nLock"),
        0x46 => ("Scroll Lock", "Scroll\nLock"),
        0x47 => ("Home", "Home"),
        0x48 => ("Up", "⏶"),
        0x49 => ("PgUp", "PgUp"),
        0x4A => ("Keypad -", "-"),
        0x4B => ("Left", "⏴"),
        0x4C => ("Keypad 5", "5"),
        0x4D => ("Right", "⏵"),
        0x4E => ("Keypad +", "+"),
        0x4F => ("End", "End"),
        0x50 => ("Down", "⏷"),
        0x51 => ("PgDn", "PgDn"),
        0x52 => ("Ins", "Ins"),
        0x53 => ("Del", "Del"),
        _ => return None,
    })
}

/// Whether the key `make` is a modifier, which latches when tapped alone:
/// a Shift, Ctrl or Alt.
pub(super) fn is_modifier(make: u8) -> bool {
    matches!(make, 0x2A | 0x36 | 0x1D | 0x38)
}

/// The bit of the BIOS's keyboard flags (0040:0017h) that holds the state
/// of the lock key `make`: Caps Lock is bit 6, Num Lock bit 5, Scroll Lock
/// bit 4. `None` for any other key.
pub(super) fn lock_bit(make: u8) -> Option<u8> {
    match make {
        0x3A => Some(0x40),
        0x45 => Some(0x20),
        0x46 => Some(0x10),
        _ => None,
    }
}

/// Where each key lies, by make code, with the top left corner of the
/// keyboard at `origin`, a key that types a letter `unit` points wide and a
/// row `height` points high: the cell the key fills, its share of the gaps
/// between keys included. The cells of a row touch, and so do the rows.
pub(super) fn cells(origin: Pos2, unit: f32, height: f32) -> Vec<(u8, Rect)> {
    let quarter = unit / 4.0;
    let mut cells = Vec::new();
    for (row, keys) in ROWS.iter().enumerate() {
        let top = origin.y + row as f32 * height;
        let mut across = 0;
        for &(make, quarters) in keys.iter() {
            if make != GAP {
                let left = origin.x + across as f32 * quarter;
                let size = vec2(f32::from(quarters) * quarter, height);
                cells.push((make, Rect::from_min_size(pos2(left, top), size)));
            }
            across += u32::from(quarters);
        }
    }
    cells
}
