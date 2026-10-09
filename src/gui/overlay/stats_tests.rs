//! The Stats panel: how it sets out a figure, where its chart puts the
//! seconds, and what it shows of a meter told a known history by hand.

use super::*;
use crate::gui::meter::Counts;
use crate::gui::overlay::testing::{Driver, Screen, Shell, waiting_for_a_key};
use crate::gui::pacer::Slice;
use crate::platform::Instant;

/// The panel's title, which the View menu and its window show.
const TITLE: &str = "Stats";

fn ms(millis: u64) -> Duration {
    Duration::from_millis(millis)
}

/// A meter told `seconds` seconds of a steady guest, fifty frames a second:
/// 8 ms of the rest of the frame, then a slice of 12 ms of which 9 were
/// waited. In each frame the guest runs 200,000 instructions and is handed
/// 23,864 timer clocks, three quarters of them while halted, and is owed
/// 1,193 (a millisecond) after its slice; in 18 frames of each 50 it takes
/// an interrupt on IRQ0. Channel 0 repeats every 65536 clocks, as the BIOS
/// leaves it, and the machine had taken 1,000 interrupts on IRQ0 before the
/// first frame. In the very last frame `dropped` timer clocks are dropped.
fn steady(seconds: u64, dropped: u64) -> Meter {
    let origin = Instant::now();
    let mut meter = Meter::default();
    let mut counts = Counts {
        timer_period: Some(0x10000),
        ..Counts::default()
    };
    counts.interrupts[0] = 1000;
    counts.time.owed = 1193;
    let mut end = Duration::ZERO;
    for frame in 0..=seconds * 50 {
        let start = end + ms(8);
        end = start + ms(12);
        if frame > 0 {
            counts.instructions += 200_000;
            counts.time.delivered += 23_864;
            counts.time.halted += 17_898;
            if frame % 50 < 18 {
                counts.interrupts[0] += 1;
            }
            if frame == seconds * 50 {
                counts.time.dropped += dropped;
            }
        }
        let slice = Slice {
            start: origin + start,
            end: origin + end,
            waited: ms(9),
            paused: false,
        };
        meter.frame(slice, counts);
    }
    meter
}

/// The Stats panel open over a machine whose BIOS waits for a key, showing
/// `meter`.
fn panel(meter: Meter) -> Driver<Shell> {
    let (runtime, _keys) = waiting_for_a_key();
    let mut shell = Shell::with_panel(runtime, TITLE);
    shell.target.meter = meter;
    // A window takes its new size in the frame after its contents change.
    shell.frame(Vec::new());
    shell.frame(Vec::new());
    shell
}

/// The row of the panel `first` heads, each text without the spaces that
/// keep a figure's width.
fn row(screen: &Screen, first: &str) -> Vec<String> {
    let row = screen.row(first);
    row.iter().map(|text| text.trim().to_string()).collect()
}

/// Each kind of figure keeps its width over the range of its values: the
/// window, which takes the size of what it shows, does not jitter as the
/// figures change from one second to the next.
#[test]
fn a_figure_keeps_its_width() {
    for speed in [0.0, 312_000.0, 9_870_000.0, 45_670_000.0, 123_450_000.0] {
        assert_eq!(millions(speed).len(), 6, "{speed}");
    }
    let spans = [
        Duration::ZERO,
        Duration::from_micros(7),
        Duration::from_micros(16_700),
        ms(999),
        ms(9_990),
        ms(12_500),
        Duration::from_secs(300),
    ];
    for time in spans {
        assert_eq!(span(time).len(), 9, "{time:?}");
    }
    for share in [0.0, 0.024, 0.576, 1.0] {
        assert_eq!(percent(share).len(), 7, "{share}");
    }
    for hz in [0.0, 18.2065, 70.0, 1193.2] {
        assert_eq!(hertz(hz).len(), 10, "{hz}");
    }
    for per_second in [0.0, 0.4, 18.2, 1193.2] {
        assert_eq!(rate(per_second).len(), 7, "{per_second}");
    }
}

/// Each figure is spelled with its unit and its precision: millions of
/// instructions to two decimals, spans to a tenth of a millisecond and from
/// ten seconds on to a tenth of a second, shares to a tenth of a percent,
/// counts with their thousands apart, the uptime as a clock.
#[test]
fn each_figure_is_spelled_with_its_unit() {
    assert_eq!(millions(12_345_678.0), " 12.35");
    assert_eq!(span(Duration::from_micros(16_700)), "  16.7 ms");
    assert_eq!(span(ms(12_500)), "  12.5  s");
    assert_eq!(percent(0.576), " 57.6 %");
    assert_eq!(hertz(18.2065), "  18.21 Hz");
    assert_eq!(rate(0.4), "    0.4");
    assert_eq!(
        [0, 999, 1000, 1_234_567].map(grouped),
        ["0", "999", "1,000", "1,234,567"]
    );
    assert_eq!(clock(Duration::from_secs(3723)), "1:02:03");
    assert_eq!(clock(Duration::from_millis(59_999)), "0:00:59");
}

/// The timer's figure sets the rate taken beside the rate programmed, and
/// says so when channel 0 does not repeat; before the first average both
/// are the mark of nothing.
#[test]
fn the_timer_is_taken_against_programmed() {
    assert_eq!(
        timer(Some(18.0), Some(18.2065), true),
        "  18.00 Hz taken, 18.21 Hz programmed"
    );
    assert_eq!(
        timer(Some(0.0), None, true),
        "   0.00 Hz taken, channel 0 not repeating"
    );
    assert_eq!(timer(None, None, false), "— taken, — programmed");
}

/// The chart puts the newest second at its right edge and each one before
/// it a step further left, a minute across; a standstill at its foot, and
/// the fastest second a fifth below its top.
#[test]
fn the_chart_has_the_newest_second_at_its_right_and_the_fastest_near_its_top() {
    // 59 steps of 2 points: a minute across 118 points.
    let area = Rect::from_min_size(pos2(10.0, 20.0), vec2(118.0, 30.0));
    assert_eq!(chart_step(area), 2.0);
    // The top, 20, stands for 12.5: 10 is 24 points above the foot.
    assert_eq!(
        chart_points(&[0.0, 5.0, 10.0], area),
        [pos2(124.0, 50.0), pos2(126.0, 38.0), pos2(128.0, 26.0)]
    );
    assert_eq!(chart_points(&[0.0], area), [pos2(128.0, 50.0)]);
    assert!(chart_points(&[], area).is_empty());

    let speeds = [1.0; HISTORY];
    let points = chart_points(&speeds, area);
    assert_eq!(points[0].x, area.left(), "a minute spans the chart");

    let mesh = area_under(&points[..3], area.bottom());
    assert_eq!((mesh.vertices.len(), mesh.indices.len()), (8, 12));
}

/// A meter in a known state is shown figure by figure: the answer in
/// words, the speed and what the guest did since power-on, where a frame's
/// time went, how the guest's time kept up, and the interrupts it took on
/// the one line that took any.
#[test]
fn a_meter_in_a_known_state_shows_its_figures() {
    let shell = panel(steady(12, 0));
    let screen = shell.screen();
    assert!(screen.shows("Keeping up with real time"));
    for (first, rest) in [
        ("Speed", vec!["10.00 M instr/s"]),
        ("Executed", vec!["120,000,000 instructions"]),
        ("Uptime", vec!["0:00:12"]),
        ("Running the guest", vec!["3.0 ms", "15.0 %"]),
        ("Idle (halted or paused)", vec!["9.0 ms", "45.0 %"]),
        ("Overlay, drawing, display", vec!["8.0 ms", "40.0 %"]),
        ("Frame, on average", vec!["20.0 ms", "50.0 a second"]),
        ("Frame, at most", vec!["20.0 ms"]),
        ("Clock", vec!["100.0 % of real time"]),
        ("Timer", vec!["18.00 Hz taken, 18.21 Hz programmed"]),
        ("Owed", vec!["1.0 ms"]),
        ("Dropped", vec!["0.0 ms in the last second"]),
        ("Dropped in all", vec!["0.0 ms"]),
        ("Halted", vec!["75.0 % of the guest's time"]),
        ("IRQ0", vec!["timer", "18.0", "1,216"]),
    ] {
        let mut expected = vec![first.to_string()];
        expected.extend(rest.into_iter().map(str::to_string));
        assert_eq!(row(screen, first), expected);
    }
    assert!(
        !screen.shows("IRQ1"),
        "a line that took nothing is left out"
    );
}

/// Before the guest's first second is over there is nothing to show but
/// the uptime: every figure is the mark of nothing, the answer says the
/// meter is measuring, and no line has taken an interrupt.
#[test]
fn before_the_first_second_there_are_no_figures() {
    let shell = panel(Meter::default());
    let screen = shell.screen();
    assert!(screen.shows("Measuring"));
    assert_eq!(row(screen, "Speed"), ["Speed", NOTHING]);
    assert_eq!(row(screen, "Uptime"), ["Uptime", "0:00:00"]);
    assert_eq!(
        row(screen, "Running the guest"),
        ["Running the guest", NOTHING, NOTHING]
    );
    assert_eq!(row(screen, "Timer"), ["Timer", "— taken, — programmed"]);
    assert!(screen.shows("None yet"));
}

/// A paused guest keeps up with nothing: the answer says that it is
/// paused and that its figures are from before the pause, which stay as
/// they were.
#[test]
fn a_paused_guest_is_told_so_and_keeps_its_figures() {
    let mut shell = panel(steady(3, 0));
    shell.target.paused = true;
    let screen = shell.frame(Vec::new());
    assert!(screen.shows("Paused: the guest's figures are from before the pause"));
    assert!(!screen.shows("Keeping up with real time"));
    assert_eq!(row(screen, "Speed"), ["Speed", "10.00 M instr/s"]);
}

/// A guest that lost time in its last second is told to be falling behind,
/// by how much, and the time it lost is counted in all.
#[test]
fn a_guest_falling_behind_is_told_so_in_words() {
    // 59,659 clocks: 50 ms of timer time.
    let shell = panel(steady(2, 59_659));
    let screen = shell.screen();
    assert!(
        screen.shows("Falling behind: 50.0 ms of time dropped in the last second"),
        "{:?}",
        screen.texts()
    );
    assert_eq!(
        row(screen, "Dropped"),
        ["Dropped", "50.0 ms in the last second"]
    );
    assert_eq!(row(screen, "Dropped in all"), ["Dropped in all", "50.0 ms"]);
}
