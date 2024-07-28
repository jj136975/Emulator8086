use log::error;
use crate::vm::runtime::{CpuFlag, Runtime};

const WORD_MASK: u8 = 0b_00_00_00_01;
const AUX_CARRY_MASK: u16 = 0b_0000_0000_0000_1111;
const AUX_CARRY_FLAG: u16 = 0b_0000_0000_0001_0000;
const SIGN_FLAG: u16 = 0b_1000_0000_0000_0000;

fn process(vm: &mut Runtime) {
    let opcode = vm.fetch_byte();
    let is_word: bool = opcode & WORD_MASK != 0;

    match opcode & !WORD_MASK {
        0b_00_00_01_00 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax;
                let (res, overflow) = ax.overflowing_add(word);
                vm.registers.update_flag(CpuFlag::Overflow, overflow);
                vm.registers.update_flag(CpuFlag::Zero, res == 0u16);
                vm.registers.update_flag(CpuFlag::Sign, res & SIGN_FLAG != 0u16);
                vm.registers.update_flag(CpuFlag::Parity, res.count_ones() & 1 == 0);
                vm.registers.update_flag(CpuFlag::AuxCarry, (((word & AUX_CARRY_MASK) + (ax & AUX_CARRY_MASK)) & AUX_CARRY_FLAG) != 0u16);

                vm.registers.ax = res;
            }
        }
        _ => error!("Unknown instruction: {:#02X}", opcode)
    }
}