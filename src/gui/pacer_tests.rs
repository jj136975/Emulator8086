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

/// Within a time slice a guest idling in HLT is looked at every millisecond,
/// not once per displayed frame, and is credited with the host time that
/// has passed: its timer keeps ticking at its own rate.
#[test]
fn timer_ticks_reach_an_idle_guest_during_a_time_slice() {
    // The BIOS programs 18.2 Hz, finds no disk to boot and waits for a key
    // in HLT. It gets there without any time passing.
    let mut rt = machine(BIOS_ROM);
    for _ in 0..2000 {
        rt.run(50_000);
        if rt.cpu.halted {
            break;
        }
    }
    assert!(rt.cpu.halted, "the BIOS waits for a key");

    // The BIOS tick counter at 0040:006C, incremented by every INT 08h.
    let timer_ticks = |rt: &Runtime| {
        let memory = &rt.cpu.memory;
        memory.read_word(0x046C) as u32 | (memory.read_word(0x046E) as u32) << 16
    };
    let before = timer_ticks(&rt);
    Pacer::new().run_slice(&mut rt, Duration::from_millis(250));
    let ticks = timer_ticks(&rt) - before;
    assert!(
        (3..=6).contains(&ticks),
        "{ticks} ticks in 250 ms, expected 4 or 5"
    );
}
