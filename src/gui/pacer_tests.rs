//! The pacer on the host's real clock: these tests take the time they
//! measure, a few hundred milliseconds in all.

use super::*;
use crate::wall_clock::HostClock;
use emu8086_core::BIOS_ROM;
use emu8086_core::io::keyboard::KeyboardController;

/// A machine without disks, with `rom` as its ROM. Nothing ever types on
/// its keyboard.
fn machine(rom: &[u8]) -> Runtime {
    let (_keys, key_events) = std::sync::mpsc::channel();
    let keyboard = KeyboardController::with_receiver(key_events);
    let mut rt = Runtime::new(vec![], vec![], None, false, keyboard, &HostClock)
        .expect("a machine without disks has nothing to open");
    rt.load_rom_image(rom);
    rt
}

/// The 16 bytes from the reset vector to the top of memory, starting with
/// `code`: what the CPU executes first.
fn reset_code(code: &[u8]) -> [u8; 16] {
    let mut rom = [0x90; 16]; // NOP
    rom[..code.len()].copy_from_slice(code);
    rom
}

/// A time slice runs the guest for as long as it lasts, sample after
/// sample: one sample's worth of instructions per displayed frame would
/// hold the guest to 256 instructions per frame.
#[test]
fn run_slice_runs_the_guest_until_the_slice_is_over() {
    let mut rt = machine(&reset_code(&[0xEB, 0xFE])); // JMP $

    Pacer::new().run_slice(&mut rt, Duration::from_millis(100));
    assert!(
        rt.instruction_count >= 100_000,
        "only {} instructions ran",
        rt.instruction_count
    );
}

/// A halted CPU does not end the slice early: the machine keeps being
/// looked at until the slice is over.
#[test]
fn run_slice_lasts_the_whole_slice_when_halted() {
    let mut rt = machine(&reset_code(&[]));
    rt.cpu.halted = true; // interrupts are off after a reset: nothing can wake it

    let slice = Duration::from_millis(30);
    let started = Instant::now();
    Pacer::new().run_slice(&mut rt, slice);
    assert!(started.elapsed() >= slice);
    assert_eq!(rt.instruction_count, 0);
}

/// A machine on which the BIOS has programmed the timer for 18.2 Hz, found
/// no disk to boot and waits for a key in HLT, with interrupts on. It gets
/// there without any time passing.
fn machine_idling_in_the_bios() -> Runtime {
    let mut rt = machine(BIOS_ROM);
    for _ in 0..2000 {
        rt.run(50_000);
        if rt.cpu.halted {
            break;
        }
    }
    assert!(rt.cpu.halted, "the BIOS waits for a key");
    rt
}

/// The BIOS tick counter at 0040:006C, incremented by every INT 08h.
fn timer_ticks(rt: &Runtime) -> u32 {
    let memory = &rt.cpu.memory;
    memory.read_word(0x046C) as u32 | (memory.read_word(0x046E) as u32) << 16
}

/// The timer interrupts a 250 ms slice brings to the guest.
fn ticks_in_a_quarter_second(rt: &mut Runtime) -> u32 {
    let before = timer_ticks(rt);
    Pacer::new().run_slice(rt, Duration::from_millis(250));
    timer_ticks(rt) - before
}

/// Within a time slice a guest idling in HLT is looked at every millisecond,
/// not once per displayed frame, and is credited with the host time that
/// has passed: its timer keeps ticking at its own rate.
#[test]
fn timer_ticks_reach_an_idle_guest_during_a_time_slice() {
    let mut rt = machine_idling_in_the_bios();
    let ticks = ticks_in_a_quarter_second(&mut rt);
    assert!(
        (3..=6).contains(&ticks),
        "{ticks} ticks in 250 ms, expected 4 or 5"
    );
}

/// A guest that never halts is credited with the host time between two
/// samples of the clock, a few hundred instructions apart: its timer ticks
/// at the same rate as an idle guest's, however many instructions it runs.
#[test]
fn timer_ticks_reach_a_busy_guest_at_the_same_rate() {
    let mut rt = machine_idling_in_the_bios();
    // JMP $ at 1000:0000, with interrupts on as the BIOS left them.
    rt.cpu.memory.write_byte(0x10000, 0xEB);
    rt.cpu.memory.write_byte(0x10001, 0xFE);
    rt.cpu.registers.cs.reg_mut().set(0x1000);
    rt.cpu.registers.pc.set(0);
    rt.cpu.halted = false;

    let ticks = ticks_in_a_quarter_second(&mut rt);
    assert!(
        (3..=6).contains(&ticks),
        "{ticks} ticks in 250 ms, expected 4 or 5"
    );
    assert!(!rt.cpu.halted, "the guest was busy all along");
}

/// A paused guest is not run. Its slice goes by all the same, as long as
/// any other: the loop that calls slice after slice keeps its pace.
#[test]
fn a_paused_guest_does_not_run_and_its_slice_still_lasts() {
    let mut rt = machine(&reset_code(&[0xEB, 0xFE])); // JMP $
    let mut pacer = Pacer::new();
    pacer.set_paused(true);
    assert!(pacer.paused());

    let slice = Duration::from_millis(30);
    let started = Instant::now();
    pacer.run_slice(&mut rt, slice);
    assert!(started.elapsed() >= slice);
    assert_eq!(rt.instruction_count, 0);

    pacer.set_paused(false);
    pacer.run_slice(&mut rt, slice);
    assert!(rt.instruction_count > 0, "it runs again");
}

/// The time a guest stood paused is not credited to it when it runs again:
/// its timer does not tick while it is paused, and afterwards goes on at
/// its own rate, without the burst of ticks the pause would be worth.
#[test]
fn the_time_spent_paused_is_never_credited() {
    let mut rt = machine_idling_in_the_bios();
    let mut pacer = Pacer::new();
    pacer.run_slice(&mut rt, Duration::from_millis(60));

    pacer.set_paused(true);
    let ticks_at_pause = timer_ticks(&rt);
    for _ in 0..30 {
        pacer.run_slice(&mut rt, Duration::from_millis(10));
    }
    assert_eq!(timer_ticks(&rt), ticks_at_pause, "no tick while paused");

    pacer.set_paused(false);
    pacer.run_slice(&mut rt, Duration::from_millis(110));
    let ticks = timer_ticks(&rt) - ticks_at_pause;
    // 110 ms hold two ticks of 55 ms. The 300 ms of the pause would be
    // five more, or four with the cap on what the timer can be owed.
    assert!(
        (1..=3).contains(&ticks),
        "{ticks} ticks in 110 ms after a pause of 300 ms, expected 2"
    );
}
