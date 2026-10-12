//! The machine as the tool runs it: with no disk, whose built-in BIOS finds
//! nothing to boot and waits for a key that never comes, or with a program
//! of the test's in its memory, which the CPU runs instead of the BIOS.

use super::*;

/// The line the built-in BIOS finishes on the port of a machine with no
/// disk: the marks of its POST and of INT 19h, run into the message INT 19h
/// prints, which the teletype copies to the port.
const NO_BOOT: &str = "BILFNo bootable disk - press any key to retry";
/// What INT 19h then shows on the screen.
const NO_BOOT_SCREEN: &str = "No bootable disk - press any key to retry";
/// jmp $: a program that runs forever and never halts.
const JUMP_TO_ITSELF: [u8; 2] = [0xEB, 0xFE];

/// A machine with no disk whose CPU runs `program`, put at 1000:0000, from
/// its first byte, with interrupts off as at power-on: the BIOS never runs.
fn running(program: &[u8]) -> Guest {
    const SEGMENT: u16 = 0x1000;
    let mut guest = Guest::power_on(Vec::new());
    let cpu = &mut guest.runtime.cpu;
    let base = usize::from(SEGMENT) << 4;
    for (at, byte) in program.iter().enumerate() {
        cpu.memory.write_byte(base + at, *byte);
    }
    cpu.registers.cs.reg_mut().set(SEGMENT);
    cpu.registers.pc.set(0);
    guest
}

/// Each line the guest finishes on the port is handed over once, by the
/// read after the batch that finished it: the BIOS's comes after the first
/// batch, and none of the batches it then spends waiting for a key hands
/// it over again.
#[test]
fn each_line_of_the_port_is_handed_over_once() {
    let mut guest = Guest::power_on(Vec::new());
    let mut handed = Vec::new();
    for batch in 0..50 {
        guest.step();
        for line in guest.new_lines().unwrap() {
            handed.push((batch, line));
        }
    }
    assert_eq!(handed, [(0, NO_BOOT.to_string())]);
    assert!(guest.runtime.cpu.halted, "the BIOS waits for a key");
}

/// A batch takes the guest 100 ns an instruction it ran, a full batch of a
/// million 100 ms, and one that ends halted a millisecond more: the time of
/// a guest that waits for a key passes, in the batches that run none of its
/// instructions too.
#[test]
fn a_batch_takes_the_time_of_its_instructions_and_a_halt_a_millisecond() {
    let mut guest = running(&JUMP_TO_ITSELF);
    for batch in 1..=3 {
        guest.step();
        assert_eq!(guest.instructions(), batch * 1_000_000);
        assert_eq!(guest.time(), Duration::from_millis(batch * 100));
    }

    let mut guest = Guest::power_on(Vec::new());
    let mut waits = 0;
    for _ in 0..200 {
        let (instructions, time) = (guest.instructions(), guest.time());
        guest.step();
        assert!(guest.runtime.cpu.halted, "the BIOS waits for a key");
        let ran = guest.instructions() - instructions;
        assert_eq!(
            guest.time() - time,
            Duration::from_nanos(ran * 100) + Duration::from_millis(1)
        );
        if ran == 0 {
            waits += 1;
        }
    }
    assert!(waits > 0, "no batch ran nothing");
}

/// A stage that never finds the guest where it wants it gives up after the
/// first batch that brings the guest's time to the end of its budget, or
/// past it, the budget counted from the call, and is shown what the guest
/// showed then. A guest that waits for a key spends its budget too.
#[test]
fn a_stage_gives_up_once_its_budget_of_guest_time_is_spent() {
    let mut guest = running(&JUMP_TO_ITSELF);
    for (budget, end) in [(300, 300), (250, 600), (100, 700)] {
        let stopped = guest
            .run_until(Duration::from_millis(budget), |_, _| false)
            .unwrap_err();
        assert_eq!(stopped.time, Duration::from_millis(end), "{budget} ms");
        assert_eq!(stopped.instructions, end * 10_000, "{budget} ms");
    }

    let budget = Duration::from_secs(1);
    let mut guest = Guest::power_on(Vec::new());
    let mut times = Vec::new();
    let stopped = guest
        .run_until(budget, |guest, _| {
            times.push(guest.time());
            false
        })
        .unwrap_err();
    let &[.., before, last] = times.as_slice() else {
        panic!("{times:?}");
    };
    assert!(before < budget && budget <= last, "{before:?} {last:?}");
    assert_eq!(stopped.time, last);
    assert!(
        stopped.screen.lines().any(|line| line == NO_BOOT_SCREEN),
        "{}",
        stopped.screen
    );
    assert_eq!(stopped.port, [NO_BOOT]);
}
