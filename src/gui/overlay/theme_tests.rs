//! The theme is on egui's style, and nothing but `theme.rs` spells a colour.
//! The second is a rule no header can hold by itself: it is checked against
//! the sources of the overlay, so that a file added later which picks a
//! colour by hand fails the suite.
//!
//! What counts as spelling a colour: a constructor of `Color32`, `Rgba` or
//! `Hsva` whose arguments are all literals, or one of their named constants.
//! A colour computed from values (`Color32::from_rgb(r, g, b)`) is arithmetic,
//! not a choice, and passes.
//!
//! The invariant: a test source may spell colours, since nothing it writes is
//! drawn, and so may the theme. Every other file below `src/gui/overlay` is
//! swept, however deep.

use std::path::{Path, PathBuf};

use super::*;

/// The directory that is swept, from the root of the package.
const OVERLAY: &str = "src/gui/overlay";

/// The file that decides what the colours are.
const HOME: &str = "theme.rs";

/// The functions that make a colour from components.
const CONSTRUCTORS: [&str; 17] = [
    "from_rgb",
    "from_rgb_additive",
    "from_rgba_premultiplied",
    "from_rgba_unmultiplied",
    "from_rgba_unmultiplied_const",
    "from_gray",
    "from_black_alpha",
    "from_white_alpha",
    "from_additive_luminance",
    "from_luminance_alpha",
    "from_srgb",
    "from_srgba_premultiplied",
    "from_srgba_unmultiplied",
    "from_additive_srgb",
    "from_additive_rgb",
    "from_hex",
    "Hsva::new",
];

/// The types whose named constants are colours.
const COLOUR_TYPES: [&str; 3] = ["Color32::", "Rgba::", "Hsva::"];

/// The two constants that carry no colour: the absence of paint, and egui's
/// own mark for what should never be drawn.
const ALLOWED_CONSTANTS: [&str; 2] = ["TRANSPARENT", "PLACEHOLDER"];

fn is_test_source(path: &Path) -> bool {
    let name = path.file_name().and_then(|name| name.to_str());
    name.is_some_and(|name| name == "tests.rs" || name.ends_with("_tests.rs"))
}

/// The Rust files in `dir` and in every directory below it.
fn rust_sources(dir: &Path, found: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            rust_sources(&path, found);
        } else if path.extension().is_some_and(|extension| extension == "rs") {
            found.push(path);
        }
    }
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// A number or a string written out: `27`, `0x1B`, `0.5`, `255u8`, `"#fff"`.
/// A number starts with a digit, which no name does.
fn is_literal(token: &str) -> bool {
    let number = token.strip_prefix('-').unwrap_or(token);
    token.starts_with('"')
        || (number.starts_with(|c: char| c.is_ascii_digit())
            && number.chars().all(|c| is_identifier_char(c) || c == '.'))
}

/// Whether the arguments of a call are all written out, none of them a name
/// or an expression. An array of literals counts: `from_srgb([1, 2, 3])`.
fn all_literal(arguments: &str) -> bool {
    let mut tokens = arguments
        .split(',')
        .map(|token| token.trim_matches(|c: char| c.is_whitespace() || c == '[' || c == ']'))
        .filter(|token| !token.is_empty())
        .peekable();
    tokens.peek().is_some() && tokens.all(is_literal)
}

/// The named constant of a colour type at the start of `tail`, the text
/// that follows `Color32::`, if it is one that carries a colour.
fn colour_constant(tail: &str) -> Option<&str> {
    let end = tail
        .find(|c: char| !(c.is_ascii_uppercase() || c == '_'))
        .unwrap_or(tail.len());
    let name = &tail[..end];
    // Two capitals or more, and no more of a name after them: a constant,
    // not the first letter of a type or of a function.
    let ends_there = !tail[end..].starts_with(is_identifier_char);
    (name.len() > 1 && ends_there && !ALLOWED_CONSTANTS.contains(&name)).then_some(name)
}

/// The colours `source` spells: the line of each, and how it is spelled.
/// Comments are not read. A call is read to its closing parenthesis, on
/// whichever line that is, since rustfmt puts long arguments on lines of
/// their own.
fn colours_spelled(source: &str) -> Vec<(usize, String)> {
    let code: String = source
        .lines()
        .map(|line| line.split("//").next().unwrap_or(""))
        .collect::<Vec<_>>()
        .join("\n");
    let line_of = |at: usize| code[..at].matches('\n').count() + 1;
    let mut found = Vec::new();

    for constructor in CONSTRUCTORS {
        let call = format!("{constructor}(");
        for (at, _) in code.match_indices(&call) {
            // `from_rgb(` is also the end of `not_from_rgb(`.
            if code[..at].ends_with(is_identifier_char) {
                continue;
            }
            let arguments = &code[at + call.len()..];
            let arguments = &arguments[..arguments.find(')').unwrap_or(arguments.len())];
            if all_literal(arguments) {
                found.push((line_of(at), format!("{constructor}({})", arguments.trim())));
            }
        }
    }
    for colour_type in COLOUR_TYPES {
        for (at, _) in code.match_indices(colour_type) {
            if let Some(name) = colour_constant(&code[at + colour_type.len()..]) {
                found.push((line_of(at), format!("{colour_type}{name}")));
            }
        }
    }
    found.sort();
    found
}

/// No file of the overlay but the theme spells a colour: the shell and the
/// panels name a slot of the palette, so that a change of the theme reaches
/// all they draw.
#[test]
fn only_the_theme_spells_a_colour() {
    let overlay = Path::new(env!("CARGO_MANIFEST_DIR")).join(OVERLAY);
    let mut sources = Vec::new();
    rust_sources(&overlay, &mut sources);
    for expected in ["mod.rs", HOME] {
        assert!(
            sources.iter().any(|source| source.ends_with(expected)),
            "{expected} is among the files looked at"
        );
    }

    let mut offences = Vec::new();
    for path in &sources {
        if path.ends_with(HOME) || is_test_source(path) {
            continue;
        }
        let source = std::fs::read_to_string(path).unwrap();
        let shown = path.strip_prefix(&overlay).unwrap_or(path).display();
        for (line, colour) in colours_spelled(&source) {
            offences.push(format!("  {shown}:{line}: {colour}"));
        }
    }
    assert!(
        offences.is_empty(),
        "a colour is spelled outside `overlay/theme.rs`, where a change of the theme does not \
         reach it. Name a slot of `theme::PALETTE` instead, or add the slot there.\n{}",
        offences.join("\n")
    );
}

/// The sweep sees colours where there are some: in the theme itself, which
/// it would report slot by slot if that file were not exempt.
#[test]
fn the_sweep_finds_the_colours_of_the_theme_itself() {
    let home = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(OVERLAY)
        .join(HOME);
    let spelled = colours_spelled(&std::fs::read_to_string(home).unwrap());
    assert!(
        spelled.len() >= 10,
        "only {} colours found in the theme: {spelled:?}",
        spelled.len()
    );
}

/// What a panel would write to pick a colour by hand is caught, in each of
/// its spellings and wherever rustfmt puts the arguments.
#[test]
fn a_colour_picked_by_hand_is_caught() {
    for offence in [
        "let colour = egui::Color32::from_rgb(255, 210, 60);",
        "fill: Color32::from_gray(27),",
        "Stroke::new(1.0, Color32::BLACK)",
        "color: Color32::from_black_alpha(96),",
        "let veil = Color32::from_white_alpha(12);",
        "let c = Color32::from_rgba_unmultiplied(0x10, 0x20, 0x30, 0x80);",
        "let c = Color32::from_rgba_unmultiplied_const(1, 2, 3, 4);",
        "let c = Color32::from_rgb_additive(1, 2, 3);",
        "let c = Color32::from_hex(\"#ff8000\");",
        "let c = Rgba::from_rgb(1.0, 0.0, 0.0);",
        "let c = Rgba::from_srgba_unmultiplied([255u8, 0, 0, 255]);",
        "let c = Hsva::new(0.5, 1.0, 1.0, 1.0);",
        "let c = Hsva::WHITE;",
        // A constant that ends its line, as rustfmt leaves the two arms of
        // a conditional.
        "    Color32::LIGHT_YELLOW",
        "\t\tRgba::BLACK",
        // Arguments on lines of their own.
        "let c = Color32::from_rgba_premultiplied(\n    12,\n    34,\n    56,\n    78,\n);",
    ] {
        assert!(
            !colours_spelled(offence).is_empty(),
            "not caught: {offence}"
        );
    }
    assert_eq!(
        colours_spelled("let a = 1;\nlet c = Color32::RED; // red\n"),
        vec![(2usize, "Color32::RED".to_string())],
        "an offence is reported with its line"
    );
}

/// A colour computed from values, a slot of the palette and the two
/// constants without a colour pass, and so does whatever a comment says.
#[test]
fn arithmetic_and_named_slots_are_not_colours_picked_by_hand() {
    for innocent in [
        "let c = Color32::from_rgb(r, g, b);",
        "let c = Color32::from_rgb(a, b, c);",
        "let c = Color32::from_gray(level(index / 36));",
        "let c = Color32::from_rgb(level, 0, 0);",
        "let c = Color32::from_rgb();",
        "ui.colored_label(PALETTE.error, text);",
        "let nothing = Color32::TRANSPARENT;",
        "let unset = egui::Color32::PLACEHOLDER;",
        "let c: Color32 = palette.accent;",
        "use egui::Color32;",
        "// Color32::RED is what this used to be: Color32::from_rgb(255, 0, 0)",
        "/// Never `Color32::WHITE` here.",
        "let c = not_from_rgb(1, 2, 3);",
    ] {
        assert!(colours_spelled(innocent).is_empty(), "caught: {innocent}");
    }
}

/// The theme is on both of egui's styles, the one for a dark host and the
/// one for a light host, so that the overlay looks the same on either.
#[test]
fn the_theme_is_on_both_of_eguis_styles() {
    let ctx = egui::Context::default();
    apply(&ctx);

    let dark = ctx.style_of(egui::Theme::Dark);
    let light = ctx.style_of(egui::Theme::Light);
    for style in [&dark, &light] {
        let visuals = &style.visuals;
        assert_eq!(visuals.panel_fill, PALETTE.surface);
        assert_eq!(visuals.window_fill, PALETTE.surface);
        assert_eq!(visuals.extreme_bg_color, PALETTE.well);
        assert_eq!(visuals.error_fg_color, PALETTE.error);
        assert_eq!(visuals.widgets.noninteractive.fg_stroke.color, PALETTE.text);
        assert_eq!(visuals.widgets.inactive.bg_fill, PALETTE.widget);
        assert_eq!(visuals.selection.bg_fill, PALETTE.accent);
        assert_eq!(style.spacing.item_spacing, METRICS.item_gap);
    }
    assert!(dark.visuals == light.visuals, "one look for both hosts");
    assert_eq!(ctx.theme(), egui::Theme::Dark);
}
