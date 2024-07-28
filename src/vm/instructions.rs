use log::error;
use crate::vm::registers::{ByteWrapper, WordWrapper};

use crate::vm::runtime::{CpuFlag, ModRM, Runtime};

const WORD_MASK: u8 = 0b_00_00_00_01;
const DIRECTION_MASK: u8 = 0b_00_00_00_10;
const OPCODE_MASK: u8 = !(WORD_MASK | DIRECTION_MASK);

const WORD_LOW_ORDER_MASK: u16 = 0b_0000_0000_1111_1111;
const WORD_AUX_CARRY_MASK: u16 = 0b_0000_0000_0000_1111;
const WORD_AUX_CARRY_FLAG: u16 = 0b_0000_0000_0001_0000;
const WORD_SIGN_FLAG: u16 = 0b_1000_0000_0000_0000;

const BYTE_AUX_CARRY_MASK: u8 = 0b_0000_1111;
const BYTE_AUX_CARRY_FLAG: u8 = 0b_0001_0000;
const BYTE_SIGN_FLAG: u8 = 0b_1000_0000;

pub fn process(vm: &mut Runtime<'_>) {
    let opcode = vm.fetch_byte();
    let is_word: bool = opcode & WORD_MASK != 0;
    let directional: bool = opcode & DIRECTION_MASK != 0;

    println!("OP: {:#02X}", opcode & !OPCODE_MASK);

    match opcode & OPCODE_MASK {
        // ADD (AL | AX) <- Word
        0b_00_00_01_00 => {
            if is_word {
                let word = vm.fetch_word();
                let ax = vm.registers.ax.word();
                let (res, overflow) = ax.overflowing_add(word);

                vm.update_flag(CpuFlag::Overflow, overflow);
                vm.update_flag(CpuFlag::Zero, res == 0u16);
                vm.update_flag(CpuFlag::Sign, res & WORD_SIGN_FLAG != 0u16);
                vm.update_flag(CpuFlag::Parity, (res & WORD_LOW_ORDER_MASK).count_ones() & 1 == 0);
                vm.update_flag(CpuFlag::AuxCarry, (((word & WORD_AUX_CARRY_MASK) + (ax & WORD_AUX_CARRY_MASK)) & WORD_AUX_CARRY_FLAG) != 0u16);
                // TODO: check carry

                vm.registers.ax.set(res);
            } else {
                let byte = vm.fetch_byte();
                let ax = vm.registers.ax.low();
                let (res, overflow) = ax.overflowing_add(byte);

                vm.update_flag(CpuFlag::Overflow, overflow);
                vm.update_flag(CpuFlag::Zero, res == 0u8);
                vm.update_flag(CpuFlag::Sign, res & BYTE_SIGN_FLAG != 0u8);
                vm.update_flag(CpuFlag::Parity, res.count_ones() & 1 == 0);
                vm.update_flag(CpuFlag::AuxCarry, (((byte & BYTE_AUX_CARRY_MASK) + (ax & BYTE_AUX_CARRY_MASK)) & BYTE_AUX_CARRY_FLAG) != 0u8);
                // TODO: check carry

                vm.registers.ax.set_low(res);
            }
        },
        // ADD MOD_R/M <- Word
        0b_10_00_00_00 => {
            if is_word {
                let modrm: WordWrapper = vm.mod_rm_single();
                let word = vm.fetch_word();

                let prev = modrm.word();
                let (res, overflow) = prev.overflowing_add(word);

                vm.update_flag(CpuFlag::Overflow, overflow);
                vm.update_flag(CpuFlag::Zero, res == 0u16);
                vm.update_flag(CpuFlag::Sign, res & WORD_SIGN_FLAG != 0u16);
                vm.update_flag(CpuFlag::Parity, (res & WORD_LOW_ORDER_MASK).count_ones() & 1 == 0);
                vm.update_flag(CpuFlag::AuxCarry, (((word & WORD_AUX_CARRY_MASK) + (prev & WORD_AUX_CARRY_MASK)) & WORD_AUX_CARRY_FLAG) != 0u16);
                // TODO: check carry

                modrm.set(res);
            } else {
                let modrm: ByteWrapper = vm.mod_rm_single();
                let byte = vm.fetch_byte();

                let prev = modrm.byte();
                let (res, overflow) = prev.overflowing_add(byte);

                vm.update_flag(CpuFlag::Overflow, overflow);
                vm.update_flag(CpuFlag::Zero, res == 0u8);
                vm.update_flag(CpuFlag::Sign, res & BYTE_SIGN_FLAG != 0u8);
                vm.update_flag(CpuFlag::Parity, res.count_ones() & 1 == 0);
                vm.update_flag(CpuFlag::AuxCarry, (((byte & BYTE_AUX_CARRY_MASK) + (prev & BYTE_AUX_CARRY_MASK)) & BYTE_AUX_CARRY_FLAG) != 0u8);
                // TODO: check carry

                modrm.set(res);
            }
        },
        // ADD MOD_R/M <- MOD_R/M
        0b_00_00_00_00 => {
            if is_word {
                let (modrm, word): (WordWrapper, u16) = if directional { vm.mod_rm_lhs() } else { vm.mod_rm_rhs() };
                // let word = vm.fetch_word();

                let prev = modrm.word();
                let (res, overflow) = prev.overflowing_add(word);

                vm.update_flag(CpuFlag::Overflow, overflow);
                vm.update_flag(CpuFlag::Zero, res == 0u16);
                vm.update_flag(CpuFlag::Sign, res & WORD_SIGN_FLAG != 0u16);
                vm.update_flag(CpuFlag::Parity, (res & WORD_LOW_ORDER_MASK).count_ones() & 1 == 0);
                vm.update_flag(CpuFlag::AuxCarry, (((word & WORD_AUX_CARRY_MASK) + (prev & WORD_AUX_CARRY_MASK)) & WORD_AUX_CARRY_FLAG) != 0u16);
                // TODO: check carry

                modrm.set(res);
            } else {
                let (modrm, byte): (ByteWrapper, u8) = if directional { vm.mod_rm_lhs() } else { vm.mod_rm_rhs() };

                let prev = modrm.byte();
                let (res, overflow) = prev.overflowing_add(byte);

                vm.update_flag(CpuFlag::Overflow, overflow);
                vm.update_flag(CpuFlag::Zero, res == 0u8);
                vm.update_flag(CpuFlag::Sign, res & BYTE_SIGN_FLAG != 0u8);
                vm.update_flag(CpuFlag::Parity, res.count_ones() & 1 == 0);
                vm.update_flag(CpuFlag::AuxCarry, (((byte & BYTE_AUX_CARRY_MASK) + (prev & BYTE_AUX_CARRY_MASK)) & BYTE_AUX_CARRY_FLAG) != 0u8);
                // TODO: check carry

                modrm.set(res);
            }
        },
        opcode => error!("Unknown instruction: {:#02X}", opcode)
    }
}