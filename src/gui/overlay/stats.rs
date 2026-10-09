//! The Stats panel: whether the emulation keeps up with real time, and
//! where the host's time goes. First the answer in words; then the guest's
//! speed, with the last minute of it as a chart; how the host's frames
//! divide their time between running the guest, letting time go by for it,
//! the overlay, the drawing and the wait for the display; how the guest's
//! time keeps up with the host's (its clock, its timer's interrupts against
//! the rate programmed, what it is owed, what it lost, what it spent halted);
//! and the interrupts it takes, line by line. It only reads.
//!
//! The invariant: the panel shows what the meter measured (`Meter`), never
//! a count read off the machine on the spot, so that its figures are
//! averages over the same spans; and a number keeps its width from one
//! average to the next, so that the window, which takes the size of what
//! it shows, does not jitter as the figures change.

use std::time::Duration;

use egui::{Color32, Grid, Mesh, Pos2, Rect, RichText, Sense, Stroke, Ui, Vec2, pos2, vec2};

use super::Cx;
use super::pic::DEVICES;
use super::theme::{METRICS, PALETTE};
use crate::gui::meter::{Frames, HISTORY, LINES, Meter, RATE_SECONDS, Second};

/// What a figure shows while there is none: before the first average, or
/// for a span in which nothing was counted.
const NOTHING: &str = "—";

/// What the top of the speed's chart stands for, over the fastest second
/// on it: a steady speed is drawn below the top edge, not along it.
const HEADROOM: f64 = 1.25;

/// The parts of a frame's host time, as the panel names them, with their
/// colours and what each holds.
const PARTS: [(&str, Color32, &str); 5] = [
    (
        "Running the guest",
        PALETTE.frame_running,
        "The slices of host time the guest ran in",
    ),
    (
        "Idle (halted or paused)",
        PALETTE.frame_idle,
        "Host time a slice let go by: the guest was halted with nothing due, or paused. \
         A browser cannot wait: there the slice ends instead, and the time until the next \
         frame is waiting for the display",
    ),
    (
        "Overlay",
        PALETTE.frame_overlay,
        "The overlay's passes: laying it out, and carrying out what its tools asked for",
    ),
    (
        "Drawing",
        PALETTE.frame_drawing,
        "The picture and the overlay drawn into the window, presenting included",
    ),
    (
        "Waiting for the display",
        PALETTE.frame_waiting,
        "The wait for the window to be given a frame to draw on, and whatever nothing else \
         measured: in a browser, the wait for the next animation frame",
    ),
];

/// Instructions per second in millions, to two decimals in six places:
/// ` 12.34`.
fn millions(per_second: f64) -> String {
    format!("{:>6.2}", per_second / 1e6)
}

/// A span of time to a tenth of a millisecond in nine places, `  16.7 ms`;
/// from ten seconds on, to a tenth of a second: `  12.5  s`.
fn span(time: Duration) -> String {
    let millis = time.as_secs_f64() * 1000.0;
    if millis < 9_999.95 {
        format!("{millis:>6.1} ms")
    } else {
        format!("{:>6.1}  s", millis / 1000.0)
    }
}

/// A share, 1 being all, to a tenth of a percent in seven places:
/// ` 57.6 %`.
fn percent(share: f64) -> String {
    format!("{:>5.1} %", share * 100.0)
}

/// A frequency to a hundredth of a hertz in ten places: `  18.21 Hz`.
fn hertz(hz: f64) -> String {
    format!("{hz:>7.2} Hz")
}

/// A rate per second to a tenth, in seven places: `   18.2`.
fn rate(per_second: f64) -> String {
    format!("{per_second:>7.1}")
}

/// A count with its thousands apart: `1,234,567`.
fn grouped(count: u64) -> String {
    let digits = count.to_string();
    let mut grouped = String::with_capacity(digits.len() * 4 / 3);
    for (index, digit) in digits.chars().enumerate() {
        if index > 0 && (digits.len() - index).is_multiple_of(3) {
            grouped.push(',');
        }
        grouped.push(digit);
    }
    grouped
}

/// A span of host time as a clock shows it: `1:02:03`.
fn clock(time: Duration) -> String {
    let seconds = time.as_secs();
    format!(
        "{}:{:02}:{:02}",
        seconds / 3600,
        seconds / 60 % 60,
        seconds % 60
    )
}

/// A figure as the panel shows it: in a monospace font, so that it keeps
/// its width; the mark of nothing while there is none.
fn figure(text: Option<String>) -> RichText {
    RichText::new(text.as_deref().unwrap_or(NOTHING)).monospace()
}

/// The answer at a glance: whether the guest kept up with real time over
/// its last second of running, and if it did not, by how much. A paused
/// guest keeps up with nothing: its figures are those it ran with last.
fn verdict(second: Option<&Second>, paused: bool) -> RichText {
    if paused {
        return RichText::new("Paused: the guest's figures are from before the pause")
            .color(PALETTE.warning);
    }
    let Some(second) = second else {
        return RichText::new("Measuring").color(PALETTE.off);
    };
    if second.kept_up() {
        RichText::new("Keeping up with real time").color(PALETTE.on)
    } else if second.dropped > 0 {
        let dropped = span(second.dropped_time());
        RichText::new(format!(
            "Falling behind: {} of time dropped in the last second",
            dropped.trim_start()
        ))
        .color(PALETTE.warning)
    } else {
        let rate = percent(second.clock_rate());
        RichText::new(format!(
            "Falling behind: the guest's clock ran at {} of real time",
            rate.trim_start()
        ))
        .color(PALETTE.warning)
    }
}

/// The guest's speed for the menu bar, compact: its last second of
/// running, in millions of instructions to a tenth in five places, then
/// the unit. While the guest is paused it is the speed it ran at before
/// the pause, in the colour of a line at rest.
pub(super) fn readout(meter: &Meter, paused: bool) -> RichText {
    let text = match meter.speed() {
        Some(speed) => format!("{:>5.1} M instr/s", speed / 1e6),
        None => format!("{NOTHING:>5} M instr/s"),
    };
    let readout = RichText::new(text).monospace();
    if paused {
        readout.color(PALETTE.off)
    } else {
        readout
    }
}

pub(super) fn body(ui: &mut Ui, cx: &mut Cx<'_>) {
    let meter = cx.meter;
    // Gaps, not separators, between the parts: a separator spans the
    // window, and the window takes the size of its contents.
    let gap = ui.spacing().item_spacing.y * 2.0;

    ui.label(verdict(meter.second(), cx.paused));
    ui.add_space(gap);
    speed(ui, meter);
    ui.add_space(gap);
    frames(ui, meter.frames());
    ui.add_space(gap);
    guest_time(ui, meter);
    ui.add_space(gap);
    interrupts(ui, meter);
}

/// The guest's speed over its last second of running, the last minute of
/// it as a chart, and what it has done since power-on.
fn speed(ui: &mut Ui, meter: &Meter) {
    Grid::new("stats_speed").num_columns(2).show(ui, |ui| {
        ui.label("Speed")
            .on_hover_text("Instructions per second, over the guest's last second of running");
        let speed = meter
            .speed()
            .map(|speed| format!("{} M instr/s", millions(speed)));
        ui.label(figure(speed));
        ui.end_row();

        ui.label("Last minute");
        chart(ui, meter);
        ui.end_row();

        ui.label("Executed");
        let executed = meter
            .counts()
            .map(|counts| format!("{} instructions", grouped(counts.instructions)));
        ui.label(figure(executed));
        ui.end_row();

        ui.label("Uptime");
        ui.label(figure(Some(clock(meter.uptime()))));
        ui.end_row();
    });
}

/// The horizontal distance from one second to the next on a chart in
/// `area`: a minute spans its width.
fn chart_step(area: Rect) -> f32 {
    area.width() / (HISTORY - 1) as f32
}

/// Where each speed goes on a chart in `area`: the newest at its right
/// edge and each one before it a step further left; a standstill at its
/// foot, and at its top `HEADROOM` times the fastest.
fn chart_points(speeds: &[f64], area: Rect) -> Vec<Pos2> {
    let step = chart_step(area);
    let top = speeds.iter().copied().fold(0.0, f64::max) * HEADROOM;
    let newest = speeds.len().saturating_sub(1);
    speeds
        .iter()
        .enumerate()
        .map(|(index, speed)| {
            let height = if top > 0.0 { (speed / top) as f32 } else { 0.0 };
            pos2(
                area.right() - (newest - index) as f32 * step,
                area.bottom() - height * area.height(),
            )
        })
        .collect()
}

/// The area under the line through `points`, down to `foot`: a quad under
/// each of its segments.
fn area_under(points: &[Pos2], foot: f32) -> Mesh {
    let mut mesh = Mesh::default();
    for pair in points.windows(2) {
        let first = mesh.vertices.len() as u32;
        for corner in [
            pair[0],
            pair[1],
            pos2(pair[1].x, foot),
            pos2(pair[0].x, foot),
        ] {
            mesh.colored_vertex(corner, PALETTE.chart_fill);
        }
        mesh.add_triangle(first, first + 1, first + 2);
        mesh.add_triangle(first, first + 2, first + 3);
    }
    mesh
}

/// The guest's speed in each of its last sixty seconds of running, the
/// newest at the right, as a line over the area under it, scaled to the
/// fastest of them (`chart_points`). The second under the pointer is told
/// in words.
fn chart(ui: &mut Ui, meter: &Meter) {
    let (rect, response) = ui.allocate_exact_size(METRICS.chart, Sense::hover());
    let painter = ui.painter();
    painter.rect_filled(rect, METRICS.widget_corner, PALETTE.well);

    let speeds: Vec<f64> = meter.seconds().map(Second::speed).collect();
    let area = rect.shrink(METRICS.strong_line);
    let points = chart_points(&speeds, area);
    match points.as_slice() {
        [] => {}
        [point] => {
            painter.circle_filled(*point, METRICS.strong_line, PALETTE.chart_line);
        }
        _ => {
            painter.add(area_under(&points, area.bottom()));
            painter.line(points, Stroke::new(METRICS.strong_line, PALETTE.chart_line));
        }
    }

    let Some(pointer) = response.hover_pos() else {
        return;
    };
    let back = ((area.right() - pointer.x) / chart_step(area))
        .round()
        .max(0.0) as usize;
    if let Some(speed) = speeds.iter().rev().nth(back) {
        let when = match back {
            0 => "the last second".to_string(),
            _ => format!("{back} s earlier"),
        };
        let speed = millions(*speed);
        response.on_hover_text_at_pointer(format!("{} M instr/s, {when}", speed.trim_start()));
    }
}

/// A square of `colour`, the side of a swatch, in line with the text.
fn swatch(ui: &mut Ui, colour: Color32) {
    let (rect, _) = ui.allocate_exact_size(Vec2::splat(METRICS.swatch), Sense::hover());
    ui.painter().rect_filled(rect, 0.0, colour);
}

/// The parts of a frame's host time, in the order of `PARTS`.
fn parts(frames: &Frames) -> [Duration; 5] {
    [
        frames.running,
        frames.idle,
        frames.overlay,
        frames.drawing,
        frames.waiting(),
    ]
}

/// The parts of the frames' host time side by side, each as long as its
/// share, in a bar as wide as a chart. Before the first average the bar is
/// empty.
fn bar(ui: &mut Ui, frames: Option<&Frames>) {
    let size = vec2(METRICS.chart.x, METRICS.bar);
    let (rect, _) = ui.allocate_exact_size(size, Sense::hover());
    let painter = ui.painter();
    painter.rect_filled(rect, 0.0, PALETTE.well);
    let Some(frames) = frames else {
        return;
    };
    let mut left = rect.left();
    for ((_, colour, _), part) in PARTS.iter().zip(parts(frames)) {
        let width = frames.share(part) as f32 * rect.width();
        let segment = Rect::from_min_size(pos2(left, rect.top()), vec2(width, rect.height()));
        painter.rect_filled(segment, 0.0, *colour);
        left += width;
    }
}

/// Where the host's time goes, over the frames of the last second: a bar of
/// the parts of a frame, each part's time in a frame and its share, and
/// the frames themselves, how long and how many a second.
fn frames(ui: &mut Ui, frames: Option<&Frames>) {
    ui.strong("Where the time goes");
    bar(ui, frames);
    Grid::new("stats_frames").num_columns(3).show(ui, |ui| {
        for (index, (name, colour, holds)) in PARTS.into_iter().enumerate() {
            ui.horizontal(|ui| {
                swatch(ui, colour);
                ui.label(name).on_hover_text(holds);
            });
            let part = frames.map(|frames| (frames, parts(frames)[index]));
            ui.label(figure(
                part.map(|(frames, part)| span(part / frames.count.max(1))),
            ));
            ui.label(figure(
                part.map(|(frames, part)| percent(frames.share(part))),
            ));
            ui.end_row();
        }

        ui.label("Frame, on average");
        ui.label(figure(frames.map(|frames| span(frames.mean()))));
        ui.label(figure(
            frames.map(|frames| format!("{:>5.1} a second", frames.rate())),
        ));
        ui.end_row();

        ui.label("Frame, at most");
        ui.label(figure(frames.map(|frames| span(frames.longest))));
        ui.end_row();
    });
}

/// What the timer's interrupts came to against what channel 0 of the 8253
/// is programmed for: `  18.20 Hz taken, 18.21 Hz programmed`. The rate
/// taken keeps its width, so that the rest stays in place as it changes.
fn timer(taken: Option<f64>, programmed: Option<f64>, measured: bool) -> String {
    let taken = taken.map_or_else(|| NOTHING.to_string(), hertz);
    let programmed = match programmed {
        Some(rate) => format!("{} programmed", hertz(rate).trim_start()),
        None if measured => "channel 0 not repeating".to_string(),
        None => format!("{NOTHING} programmed"),
    };
    format!("{taken} taken, {programmed}")
}

/// How the guest's time kept up with the host's over its last second of
/// running: its clock; its timer's interrupts, over its last seconds,
/// against the rate programmed; what it was owed and what it lost; and
/// what share of it it spent halted.
fn guest_time(ui: &mut Ui, meter: &Meter) {
    ui.strong("The guest's time");
    let second = meter.second();
    Grid::new("stats_guest_time").num_columns(2).show(ui, |ui| {
        ui.label("Clock")
            .on_hover_text("The timer time the guest was handed, over the host time it ran for");
        ui.label(figure(second.map(|second| {
            format!("{} of real time", percent(second.clock_rate()))
        })));
        ui.end_row();

        ui.label("Timer").on_hover_text(format!(
            "IRQ0, from channel 0 of the 8253: the interrupts the guest took, over its last \
             {RATE_SECONDS} seconds of running, and the rate the channel is programmed for"
        ));
        let taken = meter.interrupt_rates().map(|rates| rates[0]);
        let measured = meter.counts().is_some();
        ui.label(figure(Some(timer(taken, meter.timer_rate(), measured))));
        ui.end_row();

        ui.label("Owed")
            .on_hover_text("Timer time the guest was owed when its last second ended");
        ui.label(figure(second.map(|second| span(second.owed_time()))));
        ui.end_row();

        ui.label("Dropped").on_hover_text(
            "Host time the guest will never be handed: it can be owed a quarter of a second",
        );
        let dropped =
            second.map(|second| format!("{} in the last second", span(second.dropped_time())));
        let lost = second.is_some_and(|second| second.dropped > 0);
        ui.label(warned(figure(dropped), lost));
        ui.end_row();

        ui.label("Dropped in all");
        let all = meter.dropped();
        let all_figure = figure(meter.counts().map(|_| span(all)));
        ui.label(warned(all_figure, !all.is_zero()));
        ui.end_row();

        ui.label("Halted").on_hover_text(
            "The share of its time the guest spent in HLT, waiting for an interrupt",
        );
        let halted = second.and_then(Second::halted_share);
        ui.label(figure(
            halted.map(|share| format!("{} of the guest's time", percent(share))),
        ));
        ui.end_row();
    });
}

/// `figure` in the colour of a warning when `warn` says so.
fn warned(figure: RichText, warn: bool) -> RichText {
    if warn {
        figure.color(PALETTE.warning)
    } else {
        figure
    }
}

/// The interrupts the guest takes, line by line: per second over its last
/// seconds of running, and since power-on. A line that never took one is
/// left out.
fn interrupts(ui: &mut Ui, meter: &Meter) {
    ui.strong("Interrupts taken");
    let totals = meter
        .counts()
        .map_or([0; LINES], |counts| counts.interrupts);
    if totals.iter().all(|&total| total == 0) {
        ui.label("None yet");
        return;
    }
    let rates = meter.interrupt_rates();
    Grid::new("stats_interrupts")
        .num_columns(4)
        .striped(true)
        .show(ui, |ui| {
            ui.strong("Line");
            ui.strong("Device");
            ui.strong("Per second").on_hover_text(format!(
                "Over the guest's last {RATE_SECONDS} seconds of running"
            ));
            ui.strong("Since power-on");
            ui.end_row();
            for line in (0..LINES).filter(|&line| totals[line] > 0) {
                ui.label(format!("IRQ{line}"));
                ui.label(DEVICES[line]);
                ui.label(figure(rates.map(|rates| rate(rates[line]))));
                ui.label(figure(Some(format!("{:>13}", grouped(totals[line])))));
                ui.end_row();
            }
        });
}

#[cfg(test)]
#[path = "stats_tests.rs"]
mod tests;
