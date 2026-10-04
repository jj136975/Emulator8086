use super::alu::{CMP, alu};
use crate::utils::number::SpecialOps;
use crate::vm::memory::Segment;
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Runtime;

/// An element width of the string instructions: `u8` for the byte forms,
/// `u16` for the word forms.
pub(super) trait Element: SpecialOps {
    /// Bytes per element.
    const SIZE: u16;

    fn read(segment: &Segment, offset: u16) -> Self;
    fn write(segment: &mut Segment, offset: u16, value: Self);
    /// AL or AX.
    fn accumulator(vm: &Runtime) -> Self;
    fn set_accumulator(vm: &mut Runtime, value: Self);
    fn port_in(vm: &mut Runtime, port: u16) -> Self;
    fn port_out(vm: &mut Runtime, port: u16, value: Self);
}

impl Element for u8 {
    const SIZE: u16 = 1;

    #[inline(always)]
    fn read(segment: &Segment, offset: u16) -> u8 {
        segment.read_byte(offset)
    }

    #[inline(always)]
    fn write(segment: &mut Segment, offset: u16, value: u8) {
        segment.write_byte(offset, value)
    }

    #[inline(always)]
    fn accumulator(vm: &Runtime) -> u8 {
        vm.cpu.registers.ax.low()
    }

    #[inline(always)]
    fn set_accumulator(vm: &mut Runtime, value: u8) {
        vm.cpu.registers.ax.set_low(value)
    }

    #[inline(always)]
    fn port_in(vm: &mut Runtime, port: u16) -> u8 {
        vm.port_in_byte(port)
    }

    #[inline(always)]
    fn port_out(vm: &mut Runtime, port: u16, value: u8) {
        vm.port_out_byte(port, value)
    }
}

impl Element for u16 {
    const SIZE: u16 = 2;

    #[inline(always)]
    fn read(segment: &Segment, offset: u16) -> u16 {
        segment.read_word(offset)
    }

    #[inline(always)]
    fn write(segment: &mut Segment, offset: u16, value: u16) {
        segment.write_word(offset, value)
    }

    #[inline(always)]
    fn accumulator(vm: &Runtime) -> u16 {
        vm.cpu.registers.ax.word()
    }

    #[inline(always)]
    fn set_accumulator(vm: &mut Runtime, value: u16) {
        vm.cpu.registers.ax.set(value)
    }

    #[inline(always)]
    fn port_in(vm: &mut Runtime, port: u16) -> u16 {
        vm.port_in_word(port)
    }

    #[inline(always)]
    fn port_out(vm: &mut Runtime, port: u16, value: u16) {
        vm.port_out_word(port, value)
    }
}

/// Rewind PC to the start of the current REP-string instruction (the first
/// prefix byte) so the main loop can service a pending interrupt. After
/// IRET the CPU re-enters through the prefixes with CX at its decremented
/// value, resuming the string op.
#[inline]
fn break_for_interrupt(vm: &mut Runtime) {
    vm.cpu.registers.pc.set(vm.cpu.registers.op_start_pc);
}

/// Check whether we should suspend a REP-string op for a pending interrupt.
#[inline]
fn interrupt_pending(vm: &Runtime) -> bool {
    vm.cpu.check_flag(Interrupt) && vm.cpu.pic.has_interrupt()
}

/// Runs a string instruction. `element` processes one element and moves SI
/// and/or DI on. Without a REP prefix it runs once; with one it runs CX
/// times, CX counting down. For the comparing instructions (`compares`:
/// CMPS and SCAS) the repetition also ends when ZF stops matching the
/// prefix: REPE/REPZ continues while ZF is set, REPNE/REPNZ while it is clear.
#[inline(always)]
fn repeat(vm: &mut Runtime, compares: bool, mut element: impl FnMut(&mut Runtime)) {
    let rep = vm.prefixes.rep;
    loop {
        if rep.is_some() && vm.cpu.registers.cx.word() == 0 {
            break;
        }

        element(vm);

        let Some(while_zero) = rep else {
            break;
        };
        vm.cpu.registers.cx.operation(1, u16::wrapping_sub);
        if compares && vm.cpu.check_flag(Zero) != while_zero {
            break;
        }
        // Yield mid-REP so a pending, unmasked IRQ can be serviced.
        if vm.cpu.registers.cx.word() != 0 && interrupt_pending(vm) {
            break_for_interrupt(vm);
            return;
        }
    }
}

/// What is added to SI and DI after each element: the element size, negated
/// when the direction flag is set.
#[inline(always)]
fn element_step<T: Element>(vm: &Runtime) -> u16 {
    if vm.cpu.check_flag(Directional) {
        T::SIZE.wrapping_neg()
    } else {
        T::SIZE
    }
}

// The instructions are generic over the element width and kept out of line:
// inlined into the opcode dispatcher, the REP loops of the byte forms run
// about half as fast.

/// MOVS — copy DS:[SI] to ES:[DI]
#[inline(never)]
pub(super) fn movs<T: Element>(vm: &mut Runtime) {
    let step = element_step::<T>(vm);
    repeat(vm, false, |vm| {
        let si = vm.cpu.registers.si.word();
        let di = vm.cpu.registers.di.word();
        let value = T::read(vm.data_segment(), si);
        T::write(&mut vm.cpu.registers.es, di, value);
        vm.cpu.registers.si.operation(step, u16::wrapping_add);
        vm.cpu.registers.di.operation(step, u16::wrapping_add);
    });
}

/// CMPS — compare DS:[SI] with ES:[DI]
#[inline(never)]
pub(super) fn cmps<T: Element>(vm: &mut Runtime) {
    let step = element_step::<T>(vm);
    repeat(vm, true, |vm| {
        let si = vm.cpu.registers.si.word();
        let di = vm.cpu.registers.di.word();
        let source = T::read(vm.data_segment(), si);
        let destination = T::read(&vm.cpu.registers.es, di);
        alu(vm, CMP, source, destination);
        vm.cpu.registers.si.operation(step, u16::wrapping_add);
        vm.cpu.registers.di.operation(step, u16::wrapping_add);
    });
}

/// STOS — store the accumulator at ES:[DI]
#[inline(never)]
pub(super) fn stos<T: Element>(vm: &mut Runtime) {
    let step = element_step::<T>(vm);
    repeat(vm, false, |vm| {
        let di = vm.cpu.registers.di.word();
        let value = T::accumulator(vm);
        T::write(&mut vm.cpu.registers.es, di, value);
        vm.cpu.registers.di.operation(step, u16::wrapping_add);
    });
}

/// LODS — load the accumulator from DS:[SI]
#[inline(never)]
pub(super) fn lods<T: Element>(vm: &mut Runtime) {
    let step = element_step::<T>(vm);
    repeat(vm, false, |vm| {
        let si = vm.cpu.registers.si.word();
        let value = T::read(vm.data_segment(), si);
        T::set_accumulator(vm, value);
        vm.cpu.registers.si.operation(step, u16::wrapping_add);
    });
}

/// INS — read from port [DX] into ES:[DI] (80186+)
#[inline(never)]
pub(super) fn ins<T: Element>(vm: &mut Runtime) {
    let step = element_step::<T>(vm);
    repeat(vm, false, |vm| {
        let port = vm.cpu.registers.dx.word();
        let di = vm.cpu.registers.di.word();
        let value = T::port_in(vm, port);
        T::write(&mut vm.cpu.registers.es, di, value);
        vm.cpu.registers.di.operation(step, u16::wrapping_add);
    });
}

/// OUTS — write DS:[SI] to port [DX] (80186+)
#[inline(never)]
pub(super) fn outs<T: Element>(vm: &mut Runtime) {
    let step = element_step::<T>(vm);
    repeat(vm, false, |vm| {
        let port = vm.cpu.registers.dx.word();
        let si = vm.cpu.registers.si.word();
        let value = T::read(vm.data_segment(), si);
        T::port_out(vm, port, value);
        vm.cpu.registers.si.operation(step, u16::wrapping_add);
    });
}

/// SCAS — compare the accumulator with ES:[DI]
#[inline(never)]
pub(super) fn scas<T: Element>(vm: &mut Runtime) {
    let step = element_step::<T>(vm);
    repeat(vm, true, |vm| {
        let di = vm.cpu.registers.di.word();
        let value = T::read(&vm.cpu.registers.es, di);
        alu(vm, CMP, T::accumulator(vm), value);
        vm.cpu.registers.di.operation(step, u16::wrapping_add);
    });
}
