use super::*;
use crate::io::pic::PicDevice;
use crate::vm::runtime::Runtime;

/// A controller fed through a channel, with the machine its port
/// handlers and its poll are handed.
struct Bench {
    keyboard: KeyboardController,
    keys: mpsc::Sender<EmulatorEvent>,
    rt: Runtime,
}

impl Bench {
    fn new() -> Self {
        let (keys, rx) = mpsc::channel();
        Self {
            keyboard: KeyboardController::with_receiver(rx),
            keys,
            rt: Runtime::new_test(),
        }
    }

    fn poll(&mut self) {
        self.keyboard.poll(&mut self.rt.cpu.pic);
    }

    fn out(&mut self, port: u16, value: u8) {
        self.keyboard.port_out_byte(port, value, &mut self.rt.cpu);
    }

    fn inp(&mut self, port: u16) -> u8 {
        self.keyboard.port_in_byte(port, &mut self.rt.cpu)
    }

    /// IRQ1 is waiting in the interrupt controller's request register.
    fn irq1_requested(&mut self) -> bool {
        PicDevice.port_in_byte(0x20, &mut self.rt.cpu) & 0x02 != 0
    }
}

#[test]
fn scancode_is_held_until_port_b_bit_7_is_pulsed() {
    let mut bench = Bench::new();
    bench.keys.send(EmulatorEvent::Scancode(0x1E)).unwrap();
    bench.keys.send(EmulatorEvent::ScanRelease(0x9E)).unwrap();

    bench.poll();
    assert_eq!(bench.inp(0x60), 0x1E);
    assert!(bench.irq1_requested());
    bench.poll();
    assert_eq!(
        bench.inp(0x60),
        0x1E,
        "the next byte waits for the acknowledge"
    );

    bench.out(0x61, 0x80);
    bench.poll();
    assert_eq!(
        bench.inp(0x60),
        0x1E,
        "nothing is loaded while bit 7 is high"
    );
    bench.out(0x61, 0x00);
    assert_eq!(bench.inp(0x60), 0x00);
    assert!(!bench.irq1_requested());

    bench.poll();
    assert_eq!(bench.inp(0x60), 0x9E);
    assert!(bench.irq1_requested());
}

#[test]
fn releasing_the_clock_line_makes_the_keyboard_answer_aa() {
    let mut bench = Bench::new();
    bench.out(0x61, 0x08); // clock line low
    bench.out(0x61, 0xC8); // clock released, keyboard held clear
    bench.out(0x61, 0x48); // keyboard enabled
    bench.poll();
    assert_eq!(bench.inp(0x60), 0xAA);
    assert!(bench.irq1_requested());
}

#[test]
fn ports_b_and_c_report_timer_channel_2() {
    let mut bench = Bench::new();
    let pit = Rc::new(RefCell::new(Pit::new()));
    bench.keyboard.set_pit(Rc::clone(&pit));
    // Channel 2 in mode 3: the output is high while the gate is low.
    pit.borrow_mut()
        .port_out_byte(0x43, 0xB6, &mut bench.rt.cpu);

    assert_eq!(bench.inp(0x61) & 0x20, 0x20);
    assert_eq!(bench.inp(0x62), 0x20, "speaker data is gated off");
    bench.out(0x61, 0x02); // speaker data enabled
    assert_eq!(bench.inp(0x62), 0x30);
}

/// The view follows the interface: a scancode in port A waits for its
/// acknowledge with the next one queued behind it; the keyboard is held
/// clear while port B bit 7 is high; the clock is held low while bit 6 is,
/// and its release starts a reset. Looking flips no bit of port B and
/// drains nothing from the channel; a read of port 61h flips bit 4.
#[test]
fn the_view_follows_the_interface() {
    let mut bench = Bench::new();
    assert_eq!(
        bench.keyboard.view(),
        KeyboardView {
            port_a: 0x00,
            port_b: 0x00,
            awaiting_acknowledge: false,
            held_clear: false,
            clock_held_low: false,
            reset_pending: false,
            queued: 0,
        }
    );

    bench.keys.send(EmulatorEvent::Scancode(0x1E)).unwrap();
    bench.keys.send(EmulatorEvent::ScanRelease(0x9E)).unwrap();
    assert_eq!(bench.keyboard.view().queued, 0, "not polled yet");
    bench.poll();
    let view = bench.keyboard.view();
    assert_eq!(
        (view.port_a, view.awaiting_acknowledge, view.queued),
        (0x1E, true, 1)
    );

    bench.out(0x61, 0xC8); // bit 7 high: the keyboard is held clear
    let view = bench.keyboard.view();
    assert_eq!((view.port_b, view.held_clear), (0xC8, true));
    bench.out(0x61, 0x48); // and low again: the acknowledge
    let view = bench.keyboard.view();
    assert_eq!(
        (view.port_a, view.awaiting_acknowledge, view.held_clear),
        (0x00, false, false)
    );

    bench.out(0x61, 0x08); // the clock held low
    assert!(bench.keyboard.view().clock_held_low);
    bench.out(0x61, 0x48); // released: the keyboard resets
    let view = bench.keyboard.view();
    assert!(!view.clock_held_low && view.reset_pending);

    assert_eq!(bench.keyboard.view().port_b, 0x48, "looking flips nothing");
    bench.inp(0x61);
    assert_eq!(bench.keyboard.view().port_b, 0x58, "a read flips bit 4");
}
