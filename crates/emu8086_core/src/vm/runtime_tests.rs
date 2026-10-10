use super::*;

/// A machine without devices whose guest spins in `JMP $`.
fn spinning_guest() -> Runtime {
    let mut rt = Runtime::new_test();
    rt.cpu.memory.write_byte(0x10000, 0xEB); // JMP $
    rt.cpu.memory.write_byte(0x10001, 0xFE);
    rt.cpu.registers.cs.reg_mut().set(0x1000);
    rt.cpu.registers.pc.set(0);
    rt
}

/// The fraction of a timer clock left over after a credit of host time is
/// carried into the next one: dropping it would make the timer run several
/// percent slow while the guest is busy.
#[test]
fn timer_time_is_not_lost_to_rounding() {
    let mut rt = Runtime::new_test();
    for _ in 0..1_000_000 {
        rt.credit_pit_time(100); // less than one timer clock
    }
    assert_eq!(rt.pit_debt, 119_318); // 0.1 s of a 1,193,182 Hz clock
}

/// A running guest executes exactly the instructions `run` is asked for:
/// whoever paces it by a clock counts on the length of a call.
#[test]
fn run_executes_the_instructions_asked_for() {
    let mut rt = spinning_guest();
    rt.run(256);
    assert_eq!(rt.instruction_count, 256);
    rt.run(1);
    assert_eq!(rt.instruction_count, 257);
    rt.run(0);
    assert_eq!(rt.instruction_count, 257);
}

/// `run` returns as soon as the CPU is halted with nothing to wake it, and
/// a call on such a CPU makes one pass and executes nothing: the caller
/// learns at once that the guest is waiting.
#[test]
fn run_returns_when_the_guest_halts() {
    let mut rt = Runtime::new_test();
    rt.cpu.memory.write_byte(0x10000, 0x90); // NOP
    rt.cpu.memory.write_byte(0x10001, 0xF4); // HLT, with interrupts off
    rt.cpu.registers.cs.reg_mut().set(0x1000);
    rt.cpu.registers.pc.set(0);

    rt.run(1000);
    assert!(rt.cpu.halted);
    assert_eq!(rt.instruction_count, 2);

    rt.run(1000);
    assert!(rt.cpu.halted);
    assert_eq!(rt.instruction_count, 2);
}

/// The machine reads no clock: however long the guest runs, the timer is
/// clocked for the time the machine was credited with and for no other.
#[test]
fn guest_time_is_the_time_credited_and_no_other() {
    let mut rt = spinning_guest();
    rt.run(100_000);
    assert_eq!(rt.video_clock.get(), 0, "no time was credited");

    rt.credit_pit_time(1_000_000); // 1 ms: 1,193 timer clocks
    rt.run(100_000);
    assert_eq!(rt.video_clock.get(), 1_193);

    rt.run(100_000);
    assert_eq!(rt.video_clock.get(), 1_193, "a credit is paid out once");
}

#[test]
fn reset_restarts_the_cpu_at_the_reset_vector() {
    let mut rt = Runtime::new_test();
    rt.cpu.registers.cs.reg_mut().set(0x1234);
    rt.cpu.registers.pc.set(0x5678);
    rt.cpu.registers.ds.reg_mut().set(0x1111);
    rt.cpu.registers.es.reg_mut().set(0x2222);
    rt.cpu.registers.ss.reg_mut().set(0x3333);
    rt.cpu.flags = 0x0FD5;
    rt.cpu.halted = true;
    rt.interrupt_inhibit = true;
    rt.prefixes.rep = Some(true);
    rt.prefixes.pending = true;

    rt.reset();

    assert_eq!(rt.cpu.registers.cs.reg().word(), 0xFFFF);
    assert_eq!(rt.cpu.registers.pc.word(), 0x0000);
    assert_eq!(rt.cpu.registers.ds.reg().word(), 0x0000);
    assert_eq!(rt.cpu.registers.es.reg().word(), 0x0000);
    assert_eq!(rt.cpu.registers.ss.reg().word(), 0x0000);
    assert_eq!(rt.cpu.flags, 0xF002);
    assert!(!rt.cpu.halted);
    assert!(!rt.interrupt_inhibit);
    assert!(rt.prefixes.rep.is_none() && !rt.prefixes.pending);
}

/// The bytes of the ROM socket, F000:0000-FFFF.
fn socket(rt: &Runtime) -> Vec<u8> {
    (ROM_START..MEMORY_SIZE)
        .map(|address| rt.cpu.memory.read_byte(address))
        .collect()
}

/// A ROM shorter than 64 KB ends at the top of the first megabyte, so
/// that the reset vector at F000:FFF0 lies inside it, and the socket below
/// it is empty: it reads 0xFF.
#[test]
fn short_rom_image_is_loaded_top_aligned() {
    let mut rt = Runtime::new_test();
    let mut rom = vec![0x90u8; 0x2000]; // 8 KB
    rom[0x1FF0] = 0xEA; // far jump at the reset vector
    rt.load_rom_image(&rom, "an 8 KB ROM").unwrap();

    assert_eq!(rt.cpu.memory.read_byte(0xFFFF0), 0xEA);
    assert_eq!(rt.cpu.memory.read_byte(0xFE000), 0x90);
    let socket = socket(&rt);
    assert!(
        socket[..0xE000].iter().all(|&byte| byte == EMPTY_SOCKET),
        "nothing below the image"
    );
    assert_eq!(rt.rom(), Some("an 8 KB ROM"));
}

/// A ROM swapped for a smaller one leaves nothing of itself in the socket:
/// below the new image it is empty, and the guest can no more write there
/// than over the image.
#[test]
fn a_smaller_rom_leaves_nothing_of_the_one_before() {
    let mut rt = Runtime::new_test();
    rt.load_rom_image(&[0xAA; ROM_MAX_SIZE], "a 64 KB ROM")
        .unwrap();
    rt.load_rom_image(&[0x5A; 0x2000], "an 8 KB ROM").unwrap();

    let socket = socket(&rt);
    assert!(socket[..0xE000].iter().all(|&byte| byte == EMPTY_SOCKET));
    assert!(socket[0xE000..].iter().all(|&byte| byte == 0x5A));
    assert_eq!(rt.rom(), Some("an 8 KB ROM"));

    rt.cpu.memory.write_byte(ROM_START, 0x00);
    rt.cpu.memory.write_byte(0xFFFF0, 0x00);
    assert_eq!(rt.cpu.memory.read_byte(ROM_START), EMPTY_SOCKET);
    assert_eq!(rt.cpu.memory.read_byte(0xFFFF0), 0x5A);
}

/// An image that is not 16 bytes to 64 KB long is refused with a message
/// that names it and says how long it is, and changes nothing: the socket
/// holds the ROM it held, known by its name. The shortest image there can
/// be holds the reset vector, and the longest fills the socket.
#[test]
fn an_image_that_cannot_be_a_rom_changes_nothing() {
    let mut rt = Runtime::new_test();
    rt.load_rom_image(&[0xAA; 0x4000], "the ROM before")
        .unwrap();
    let before = socket(&rt);

    for (size, said) in [
        (0, "empty"),
        (ROM_MIN_SIZE - 1, "15 bytes"),
        (ROM_MAX_SIZE + 1, "65537 bytes"),
    ] {
        let error = rt
            .load_rom_image(&vec![0x5A; size], "disk.img")
            .unwrap_err();
        assert_eq!(
            error,
            format!("disk.img is {said}, and a ROM image is 16 bytes to 64 KB")
        );
        assert!(socket(&rt) == before, "{size} bytes");
        assert_eq!(rt.rom(), Some("the ROM before"));
    }

    for size in [ROM_MIN_SIZE, ROM_MAX_SIZE] {
        assert_eq!(rt.load_rom_image(&vec![0x5A; size], "a ROM"), Ok(()));
        assert_eq!(rt.cpu.memory.read_byte(0xFFFF0), 0x5A, "{size} bytes");
    }
}

#[test]
fn unusable_rom_file_is_an_error_that_names_the_file() {
    let mut rt = Runtime::new_test();
    assert_eq!(rt.rom(), None, "no ROM yet");

    let error = rt.load_rom(PathBuf::from("no-such-rom.bin")).unwrap_err();
    assert!(error.contains("no-such-rom.bin"), "{}", error);

    let path = std::env::temp_dir().join(format!("emu8086-rom-{}.bin", std::process::id()));
    for size in [0usize, ROM_MAX_SIZE + 1] {
        std::fs::write(&path, vec![0xFFu8; size]).unwrap();
        let error = rt.load_rom(path.clone()).unwrap_err();
        assert!(error.contains("emu8086-rom-"), "{}", error);
    }
    assert_eq!(rt.rom(), None, "nothing was loaded");

    std::fs::write(&path, vec![0xEAu8; ROM_MAX_SIZE]).unwrap();
    assert_eq!(rt.load_rom(path.clone()), Ok(()));
    assert_eq!(rt.cpu.memory.read_byte(0xF0000), 0xEA);
    assert_eq!(rt.rom(), Some(path.display().to_string().as_str()));
    let _ = std::fs::remove_file(&path);
}

#[test]
fn timer_debt_is_capped_after_a_long_stall() {
    let mut rt = Runtime::new_test();
    for _ in 0..3 {
        rt.credit_pit_time(NANOS_PER_SEC);
    }
    assert_eq!(rt.pit_debt, PIT_MAX_DEBT);
}

/// Host time the guest cannot be owed is counted as it is dropped: what a
/// credit puts over the cap on the debt, and in a credit of more than a
/// second, what is over the second. A credit under the cap drops nothing.
#[test]
fn the_time_dropped_at_the_cap_is_counted() {
    let mut rt = Runtime::new_test();
    rt.credit_pit_time(NANOS_PER_SEC / 10); // 100 ms: 119,318.2 clocks
    assert_eq!(
        rt.guest_time(),
        GuestTime {
            owed: 119_318,
            ..GuestTime::default()
        }
    );

    // 200 ms more, 238,636.4 clocks, with the 0.2 carried over: 357,954
    // owed, 59,659 of them over the cap.
    rt.credit_pit_time(NANOS_PER_SEC / 5);
    let time = rt.guest_time();
    assert_eq!(
        (time.owed, time.dropped),
        (PIT_MAX_DEBT, 357_954 - PIT_MAX_DEBT)
    );

    // Five seconds at once: the four over a second, and the one second
    // left, since the debt is at its cap already.
    rt.credit_pit_time(5 * NANOS_PER_SEC);
    let time = rt.guest_time();
    assert_eq!(time.owed, PIT_MAX_DEBT);
    assert_eq!(time.dropped, 59_659 + 4 * PIT_CLOCK_HZ + PIT_CLOCK_HZ);

    // However long the stall, the count does not overflow.
    rt.credit_pit_time(u64::MAX);
    assert!(rt.guest_time().dropped > time.dropped);
}

/// The clocks handed to the 8253 are accounted for as they go, and apart,
/// those handed over while the CPU is halted: the guest's time in HLT. A
/// busy guest's time is never halted, a halted guest's is all halted.
#[test]
fn the_time_a_halted_guest_is_handed_is_counted_apart() {
    let mut rt = spinning_guest();
    rt.credit_pit_time(1_000_000); // 1 ms: 1,193.182 clocks
    rt.run(100_000);
    assert_eq!(
        rt.guest_time(),
        GuestTime {
            delivered: 1_193,
            ..GuestTime::default()
        }
    );

    // Interrupts are off on this machine: nothing wakes the CPU.
    rt.cpu.halted = true;
    rt.credit_pit_time(2_000_000); // 2,386.364 clocks, and the 0.182 carried
    rt.run(1);
    assert_eq!(
        rt.guest_time(),
        GuestTime {
            delivered: 1_193 + 2_386,
            halted: 2_386,
            ..GuestTime::default()
        }
    );
}
