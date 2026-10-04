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

/// A translator for a terminal that reports key presses only.
fn terminal() -> KeyTranslator {
    KeyTranslator::new(false)
}

fn press(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
    KeyEvent::new(code, modifiers)
}

fn release(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
    KeyEvent::new_with_kind(code, modifiers, KeyEventKind::Release)
}

/// The make and break codes in `events`, as the bytes the guest reads.
fn scancodes(events: Vec<EmulatorEvent>) -> Vec<u8> {
    events
        .into_iter()
        .map(|event| match event {
            EmulatorEvent::Scancode(code) | EmulatorEvent::ScanRelease(code) => code,
            other => panic!("not a scancode: {other:?}"),
        })
        .collect()
}

#[test]
fn plain_key_sends_its_make_and_break() {
    let mut keys = terminal();
    assert_eq!(
        keys.translate(&press(KeyCode::Char('a'), KeyModifiers::NONE)),
        [
            EmulatorEvent::Scancode(0x1E),
            EmulatorEvent::ScanRelease(0x9E)
        ]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Enter, KeyModifiers::NONE))),
        [0x1C, 0x9C]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::F(1), KeyModifiers::NONE))),
        [0x3B, 0xBB]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Left, KeyModifiers::NONE))),
        [0x4B, 0xCB]
    );
}

#[test]
fn modifier_keys_follow_the_flags_on_each_key() {
    let mut keys = terminal();
    // Shift goes down with the first shifted key and stays down ...
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Up, KeyModifiers::SHIFT))),
        [0x2A, 0x48, 0xC8]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Up, KeyModifiers::SHIFT))),
        [0x48, 0xC8]
    );
    // ... until a key arrives without it.
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('s'), KeyModifiers::CONTROL))),
        [0xAA, 0x1D, 0x1F, 0x9F]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('x'), KeyModifiers::ALT))),
        [0x9D, 0x38, 0x2D, 0xAD]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Enter, KeyModifiers::NONE))),
        [0xB8, 0x1C, 0x9C]
    );
}

#[test]
fn emulator_keys_are_not_passed_to_the_guest() {
    let mut keys = terminal();
    assert_eq!(
        keys.translate(&press(KeyCode::F(12), KeyModifiers::NONE)),
        [EmulatorEvent::EnterCLI]
    );
    assert_eq!(
        keys.translate(&press(KeyCode::F(11), KeyModifiers::SHIFT)),
        [EmulatorEvent::DumpVga]
    );
    assert_eq!(
        keys.translate(&press(KeyCode::Char('c'), KeyModifiers::CONTROL)),
        [EmulatorEvent::Quit]
    );
    // None of them touched the emulated modifier keys.
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('a'), KeyModifiers::NONE))),
        [0x1E, 0x9E]
    );
}

#[test]
fn releases_and_unmapped_keys_send_no_scancode() {
    let mut keys = terminal();
    assert_eq!(
        keys.translate(&release(KeyCode::Char('a'), KeyModifiers::NONE)),
        []
    );
    assert_eq!(
        keys.translate(&press(KeyCode::Pause, KeyModifiers::NONE)),
        []
    );
    assert_eq!(
        keys.translate(&press(KeyCode::Char('\u{e9}'), KeyModifiers::NONE)),
        []
    );
}

/// Shift is held as the US layout needs it for the character typed,
/// whatever Shift state the host typed the character with.
#[test]
fn shift_follows_the_character_typed() {
    // Typed with Shift on the host (digits on an AZERTY keyboard) but
    // without it on the US layout. Shift is released for the key and
    // goes back down afterwards, because the host still holds it.
    let mut keys = terminal();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('1'), KeyModifiers::SHIFT))),
        [0x02, 0x82, 0x2A]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('.'), KeyModifiers::SHIFT))),
        [0xAA, 0x34, 0xB4, 0x2A]
    );

    // Typed without Shift on the host (keypad keys, Caps Lock, terminals
    // that only flag Shift for capital letters) but with it on the US
    // layout. Shift is pressed for the key only.
    let mut keys = terminal();
    for (c, scancode) in [
        ('*', 0x09),
        ('+', 0x0D),
        (':', 0x27),
        ('!', 0x02),
        ('A', 0x1E),
        ('~', 0x29),
    ] {
        assert_eq!(
            scancodes(keys.translate(&press(KeyCode::Char(c), KeyModifiers::NONE))),
            [0x2A, scancode, scancode | 0x80, 0xAA],
            "{c:?}"
        );
    }
}

#[test]
fn shift_matching_the_host_is_left_alone() {
    let mut keys = terminal();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('!'), KeyModifiers::SHIFT))),
        [0x2A, 0x02, 0x82]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('Q'), KeyModifiers::SHIFT))),
        [0x10, 0x90]
    );
    // A space is the same character either way: the host decides.
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char(' '), KeyModifiers::SHIFT))),
        [0x39, 0xB9]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('q'), KeyModifiers::NONE))),
        [0xAA, 0x10, 0x90]
    );
}

/// With Ctrl or Alt held the host reports the key's unshifted character,
/// so its Shift flag counts as well.
#[test]
fn shift_flag_is_honoured_in_ctrl_and_alt_combinations() {
    let mut keys = terminal();
    assert_eq!(
        scancodes(keys.translate(&press(
            KeyCode::Char('2'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT
        ))),
        [0x2A, 0x1D, 0x03, 0x83]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('x'), KeyModifiers::ALT))),
        [0xAA, 0x9D, 0x38, 0x2D, 0xAD]
    );
}

/// Shift+Tab arrives as a key code of its own, and is typed as Tab with
/// Shift held.
#[test]
fn back_tab_is_shift_tab() {
    let mut keys = terminal();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::BackTab, KeyModifiers::SHIFT))),
        [0x2A, 0x0F, 0x8F]
    );
    let mut keys = terminal();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::BackTab, KeyModifiers::NONE))),
        [0x2A, 0x0F, 0x8F, 0xAA]
    );
}

/// A translator for the Windows console, which reports key releases.
fn windows_console() -> KeyTranslator {
    KeyTranslator::new(true)
}

/// On the Windows console a key stays down on the emulated keyboard until
/// the console reports its release.
#[test]
fn windows_console_key_stays_down_until_it_is_released() {
    let mut keys = windows_console();
    assert_eq!(
        keys.translate(&press(KeyCode::Up, KeyModifiers::NONE)),
        [EmulatorEvent::Scancode(0x48)]
    );
    // The console repeats the press while the key is held, like the
    // typematic makes of a real keyboard.
    assert_eq!(
        keys.translate(&press(KeyCode::Up, KeyModifiers::NONE)),
        [EmulatorEvent::Scancode(0x48)]
    );
    assert_eq!(
        keys.translate(&release(KeyCode::Up, KeyModifiers::NONE)),
        [EmulatorEvent::ScanRelease(0xC8)]
    );

    // The release of a key that went down before the emulator was
    // listening has nothing to let go of.
    assert_eq!(
        keys.translate(&release(KeyCode::Enter, KeyModifiers::NONE)),
        []
    );
}

#[test]
fn windows_console_modifiers_follow_presses_and_releases() {
    let mut keys = windows_console();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Up, KeyModifiers::SHIFT))),
        [0x2A, 0x48]
    );
    // Shift was let go before the key, so the key's release comes without it.
    assert_eq!(
        scancodes(keys.translate(&release(KeyCode::Up, KeyModifiers::NONE))),
        [0xAA, 0xC8]
    );

    // Shift held only to type a character goes up with its key.
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('*'), KeyModifiers::NONE))),
        [0x2A, 0x09]
    );
    assert_eq!(
        scancodes(keys.translate(&release(KeyCode::Char('*'), KeyModifiers::NONE))),
        [0xAA, 0x89]
    );
}

/// A release that names a key that is not down lets go of the newest key
/// that is. On a layout where Shift moves a character to another US key
/// (AZERTY digits), letting go of Shift first makes the release name the
/// other key.
#[test]
fn release_naming_another_character_lets_go_of_the_newest_key() {
    let mut keys = windows_console();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Up, KeyModifiers::NONE))),
        [0x48]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('1'), KeyModifiers::SHIFT))),
        [0x02]
    );
    assert_eq!(
        scancodes(keys.translate(&release(KeyCode::Char('&'), KeyModifiers::NONE))),
        [0x82]
    );
    assert_eq!(
        scancodes(keys.translate(&release(KeyCode::Up, KeyModifiers::NONE))),
        [0xC8]
    );
}

#[test]
fn losing_focus_releases_everything() {
    let mut keys = windows_console();
    keys.translate(&press(KeyCode::Tab, KeyModifiers::ALT)); // Alt+Tab away
    assert_eq!(scancodes(keys.release_all()), [0x8F, 0xB8]);
    assert_eq!(keys.release_all(), []);
}

#[test]
fn windows_console_emulator_keys_act_on_the_press_only() {
    let mut keys = windows_console();
    assert_eq!(
        keys.translate(&press(KeyCode::F(12), KeyModifiers::NONE)),
        [EmulatorEvent::EnterCLI]
    );
    assert_eq!(
        keys.translate(&release(KeyCode::F(12), KeyModifiers::NONE)),
        []
    );
    assert_eq!(
        keys.translate(&press(KeyCode::Char('c'), KeyModifiers::CONTROL)),
        [EmulatorEvent::Quit]
    );
}

/// The Windows console reports AltGr as Ctrl+Alt beside the character it
/// produced: a symbol that arrives with both is typed without them.
#[test]
fn altgr_character_is_typed_without_ctrl_and_alt() {
    let altgr = KeyModifiers::CONTROL | KeyModifiers::ALT;
    let mut keys = windows_console();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('\\'), altgr))),
        [0x2B]
    );
    assert_eq!(
        scancodes(keys.translate(&release(KeyCode::Char('\\'), altgr))),
        [0xAB]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('@'), altgr))),
        [0x2A, 0x03]
    );
    assert_eq!(
        scancodes(keys.translate(&release(KeyCode::Char('@'), altgr))),
        [0xAA, 0x83]
    );
    // A character with no key on the XT keyboard presses nothing at all.
    assert_eq!(keys.translate(&press(KeyCode::Char('\u{20ac}'), altgr)), []);
}

#[test]
fn ctrl_alt_with_a_letter_digit_or_special_key_is_passed_on() {
    let ctrl_alt = KeyModifiers::CONTROL | KeyModifiers::ALT;
    let mut keys = windows_console();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('q'), ctrl_alt))),
        [0x1D, 0x38, 0x10]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('7'), ctrl_alt))),
        [0x08]
    );
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Delete, ctrl_alt))),
        [0x53]
    );

    // Only the Windows console reports AltGr this way; elsewhere a
    // symbol with Ctrl+Alt is the real combination.
    let mut keys = terminal();
    assert_eq!(
        scancodes(keys.translate(&press(KeyCode::Char('['), ctrl_alt))),
        [0x1D, 0x38, 0x1A, 0x9A]
    );
}
