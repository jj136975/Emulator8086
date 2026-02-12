use crate::vm::runtime::{Prefix, Runtime};
use crate::vm::runtime::CpuFlag::*;
use super::{update_arithmetic_flags_word, update_arithmetic_flags_byte};
use crate::utils::number::SpecialOps;

pub(super) fn movs(vm: &mut Runtime, is_word: bool) {
    let rep = matches!(&vm.prefix, Some(Prefix::Rep(_)));
    let step: u16 = if is_word { 2 } else { 1 };
    let dir = vm.check_flag(Directional);

    loop {
        if rep && vm.registers.cx.word() == 0 { break; }

        let si = vm.registers.si.word();
        let di = vm.registers.di.word();
        if is_word {
            let word = vm.data_segment().read_word(si);
            vm.registers.es.write_word(di, word);
        } else {
            let byte = vm.data_segment().read_byte(si);
            vm.registers.es.write_byte(di, byte);
        }
        if dir {
            vm.registers.si.operation(step, u16::wrapping_sub);
            vm.registers.di.operation(step, u16::wrapping_sub);
        } else {
            vm.registers.si.operation(step, u16::wrapping_add);
            vm.registers.di.operation(step, u16::wrapping_add);
        }

        if !rep { break; }
        vm.registers.cx.operation(1, u16::wrapping_sub);
    }
    if rep { vm.prefix = None; }
}

pub(super) fn cmps(vm: &mut Runtime, is_word: bool) {
    let rep = match &vm.prefix {
        Some(Prefix::Rep(z)) => Some(*z),
        _ => None,
    };
    let step: u16 = if is_word { 2 } else { 1 };
    let dir = vm.check_flag(Directional);

    loop {
        if rep.is_some() && vm.registers.cx.word() == 0 { break; }

        let si = vm.registers.si.word();
        let di = vm.registers.di.word();
        if is_word {
            let word1 = vm.data_segment().read_word(si);
            let word2 = vm.registers.es.read_word(di);
            let (res, overflow, carry) = word1.oc_sub(word2);
            update_arithmetic_flags_word(vm, res, overflow, carry);
        } else {
            let byte1 = vm.data_segment().read_byte(si);
            let byte2 = vm.registers.es.read_byte(di);
            let (res, overflow, carry) = byte1.oc_sub(byte2);
            update_arithmetic_flags_byte(vm, res, overflow, carry);
        }
        if dir {
            vm.registers.si.operation(step, u16::wrapping_sub);
            vm.registers.di.operation(step, u16::wrapping_sub);
        } else {
            vm.registers.si.operation(step, u16::wrapping_add);
            vm.registers.di.operation(step, u16::wrapping_add);
        }

        if rep.is_none() { break; }
        vm.registers.cx.operation(1, u16::wrapping_sub);
        if vm.check_flag(Zero) != rep.unwrap() { break; }
    }
    if rep.is_some() { vm.prefix = None; }
}

pub(super) fn stos(vm: &mut Runtime, is_word: bool) {
    let rep = matches!(&vm.prefix, Some(Prefix::Rep(_)));
    let step: u16 = if is_word { 2 } else { 1 };
    let dir = vm.check_flag(Directional);

    loop {
        if rep && vm.registers.cx.word() == 0 { break; }

        let di = vm.registers.di.word();
        if is_word {
            let word = vm.registers.ax.word();
            vm.registers.es.write_word(di, word);
        } else {
            let byte = vm.registers.ax.low();
            vm.registers.es.write_byte(di, byte);
        }
        if dir {
            vm.registers.di.operation(step, u16::wrapping_sub);
        } else {
            vm.registers.di.operation(step, u16::wrapping_add);
        }

        if !rep { break; }
        vm.registers.cx.operation(1, u16::wrapping_sub);
    }
    if rep { vm.prefix = None; }
}

pub(super) fn lods(vm: &mut Runtime, is_word: bool) {
    let rep = matches!(&vm.prefix, Some(Prefix::Rep(_)));
    let step: u16 = if is_word { 2 } else { 1 };
    let dir = vm.check_flag(Directional);

    loop {
        if rep && vm.registers.cx.word() == 0 { break; }

        let si = vm.registers.si.word();
        if is_word {
            let word = vm.data_segment().read_word(si);
            vm.registers.ax.set(word);
        } else {
            let byte = vm.data_segment().read_byte(si);
            vm.registers.ax.set_low(byte);
        }
        if dir {
            vm.registers.si.operation(step, u16::wrapping_sub);
        } else {
            vm.registers.si.operation(step, u16::wrapping_add);
        }

        if !rep { break; }
        vm.registers.cx.operation(1, u16::wrapping_sub);
    }
    if rep { vm.prefix = None; }
}

pub(super) fn scas(vm: &mut Runtime, is_word: bool) {
    let rep = match &vm.prefix {
        Some(Prefix::Rep(z)) => Some(*z),
        _ => None,
    };
    let step: u16 = if is_word { 2 } else { 1 };
    let dir = vm.check_flag(Directional);

    loop {
        if rep.is_some() && vm.registers.cx.word() == 0 { break; }

        let di = vm.registers.di.word();
        if is_word {
            let word = vm.registers.es.read_word(di);
            let (res, overflow, carry) = vm.registers.ax.word().oc_sub(word);
            update_arithmetic_flags_word(vm, res, overflow, carry);
        } else {
            let byte = vm.registers.es.read_byte(di);
            let (res, overflow, carry) = vm.registers.ax.low().oc_sub(byte);
            update_arithmetic_flags_byte(vm, res, overflow, carry);
        }
        if dir {
            vm.registers.di.operation(step, u16::wrapping_sub);
        } else {
            vm.registers.di.operation(step, u16::wrapping_add);
        }

        if rep.is_none() { break; }
        vm.registers.cx.operation(1, u16::wrapping_sub);
        if vm.check_flag(Zero) != rep.unwrap() { break; }
    }
    if rep.is_some() { vm.prefix = None; }
}
