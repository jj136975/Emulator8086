use super::*;

/// The fraction of a timer clock left over after a host clock sample is
/// carried into the next one: dropping it would make the timer run several
/// percent slow while the guest is busy.
#[test]
fn timer_time_is_not_lost_to_rounding() {
    let mut rt = Runtime::new_test();
    rt.use_manual_clock();
    for _ in 0..1_000_000 {
        rt.credit_pit_time(100); // less than one timer clock
    }
    assert_eq!(rt.pit_debt, 119_318); // 0.1 s of a 1,193,182 Hz clock
}

/// A time slice runs batches back to back, as many as fit: one batch per
/// displayed frame would hold the guest to 50,000 instructions per frame.
#[test]
fn run_slice_runs_batches_back_to_back() {
    let mut rt = Runtime::new_test();
    rt.use_manual_clock();
    rt.cpu.memory.write_byte(0x10000, 0xEB); // JMP $
    rt.cpu.memory.write_byte(0x10001, 0xFE);
    rt.cpu.registers.cs.reg_mut().set(0x1000);
    rt.cpu.registers.pc.set(0);

    assert!(rt.run_slice(Duration::from_millis(100)));
    assert!(
        rt.instruction_count >= 2 * RENDER_INTERVAL,
        "only {} instructions ran",
        rt.instruction_count
    );
}

/// A halted CPU does not end the slice early: the machine keeps being
/// polled until the slice is over.
#[test]
fn run_slice_lasts_the_whole_slice_when_halted() {
    let mut rt = Runtime::new_test();
    rt.use_manual_clock();
    rt.cpu.halted = true; // interrupts disabled: nothing can wake it

    let slice = Duration::from_millis(30);
    let started = Instant::now();
    assert!(rt.run_slice(slice));
    assert!(started.elapsed() >= slice);
    assert_eq!(rt.instruction_count, 0);
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

/// A ROM shorter than 64 KB ends at the top of the first megabyte, so
/// that the reset vector at F000:FFF0 lies inside it.
#[test]
fn short_rom_image_is_loaded_top_aligned() {
    let mut rt = Runtime::new_test();
    let mut rom = vec![0x90u8; 0x2000]; // 8 KB
    rom[0x1FF0] = 0xEA; // far jump at the reset vector
    rt.load_rom_image(&rom);

    assert_eq!(rt.cpu.memory.read_byte(0xFFFF0), 0xEA);
    assert_eq!(rt.cpu.memory.read_byte(0xFE000), 0x90);
    assert_eq!(
        rt.cpu.memory.read_byte(0xFDFFF),
        0x00,
        "nothing below the image"
    );
}

#[test]
fn unusable_rom_file_is_an_error_that_names_the_file() {
    let mut rt = Runtime::new_test();

    let error = rt.load_rom(PathBuf::from("no-such-rom.bin")).unwrap_err();
    assert!(error.contains("no-such-rom.bin"), "{}", error);

    let path = std::env::temp_dir().join(format!("emu8086-rom-{}.bin", std::process::id()));
    for size in [0usize, ROM_MAX_SIZE + 1] {
        std::fs::write(&path, vec![0xFFu8; size]).unwrap();
        let error = rt.load_rom(path.clone()).unwrap_err();
        assert!(error.contains("emu8086-rom-"), "{}", error);
    }

    std::fs::write(&path, vec![0xEAu8; ROM_MAX_SIZE]).unwrap();
    assert_eq!(rt.load_rom(path.clone()), Ok(()));
    assert_eq!(rt.cpu.memory.read_byte(0xF0000), 0xEA);
    let _ = std::fs::remove_file(&path);
}

#[test]
fn timer_debt_is_capped_after_a_long_stall() {
    let mut rt = Runtime::new_test();
    rt.use_manual_clock();
    for _ in 0..3 {
        rt.credit_pit_time(NANOS_PER_SEC);
    }
    assert_eq!(rt.pit_debt, PIT_MAX_DEBT);
}
