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
