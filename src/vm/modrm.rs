use crate::vm::registers::{ByteWrapper, WordWrapper};
use crate::vm::runtime::Runtime;

pub trait ModRM {
    type Target;

    fn mod_rm_lhs(vm: &mut Runtime) -> (Self::Target, Self);
    fn mod_rm_rhs(vm: &mut Runtime) -> (Self::Target, Self);
    fn mod_rm_single(vm: &mut Runtime) -> (Self::Target, u8);
}

#[inline(always)]
pub fn rm_address(vm: &Runtime, rm: u8) -> u16 {
    match rm {
        0b000 => vm.registers.bx.word().wrapping_add(vm.registers.si.word()),
        0b001 => vm.registers.bx.word().wrapping_add(vm.registers.di.word()),
        0b010 => vm.registers.bp.word().wrapping_add(vm.registers.si.word()),
        0b011 => vm.registers.bp.word().wrapping_add(vm.registers.di.word()),
        0b100 => vm.registers.si.word(),
        0b101 => vm.registers.di.word(),
        0b110 => vm.registers.bp.word(),
        0b111 => vm.registers.bx.word(),
        _ => unreachable!()
    }
}

#[inline(always)]
pub fn direct_address(vm: &mut Runtime, rm: u8) -> u16 {
    match rm {
        0b000 => vm.registers.bx.word().wrapping_add(vm.registers.si.word()),
        0b001 => vm.registers.bx.word().wrapping_add(vm.registers.di.word()),
        0b010 => vm.registers.bp.word().wrapping_add(vm.registers.si.word()),
        0b011 => vm.registers.bp.word().wrapping_add(vm.registers.di.word()),
        0b100 => vm.registers.si.word(),
        0b101 => vm.registers.di.word(),
        0b110 => vm.fetch_word(),
        0b111 => vm.registers.bx.word(),
        _ => unreachable!()
    }
}

impl ModRM for u8 {
    type Target = ByteWrapper;
    fn mod_rm_lhs(vm: &mut Runtime) -> (ByteWrapper, u8) {
        let mod_rm = vm.fetch_byte();
        let rm = mod_rm & 0b111;

        let r2 = match (mod_rm >> 6) & 0b11 {
            0b00 => {
                let address = direct_address(vm, rm);
                vm.data_segment().read_byte(address)
            }
            0b01 => {
                let displacement = vm.fetch_byte() as i8 as i16;
                let address = rm_address(vm, rm);
                vm.data_segment().read_byte(address.wrapping_add_signed(displacement))
            }
            0b10 => {
                let displacement = vm.fetch_word();
                let address = rm_address(vm, rm);
                vm.data_segment().read_byte(address.wrapping_add(displacement))
            }
            0b11 => vm.registers.read_reg_byte(rm),
            _ => unreachable!()
        };
        (vm.registers.ref_reg_byte((mod_rm >> 3) & 0b111), r2)
    }

    fn mod_rm_rhs(vm: &mut Runtime) -> (ByteWrapper, u8) {
        let mod_rm = vm.fetch_byte();
        let rm = mod_rm & 0b111;

        (
            match (mod_rm >> 6) & 0b11 {
                0b00 => {
                    let address = direct_address(vm, rm);
                    vm.data_segment().ref_byte(address)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    vm.data_segment().ref_byte(address.wrapping_add_signed(displacement))
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    vm.data_segment().ref_byte(address.wrapping_add(displacement))
                }
                0b11 => vm.registers.ref_reg_byte(rm),
                _ => unreachable!()
            },
            vm.registers.read_reg_byte((mod_rm >> 3) & 0b111)
        )
    }

    fn mod_rm_single(vm: &mut Runtime) -> (ByteWrapper, u8) {
        let mod_rm = vm.fetch_byte();
        let rm = mod_rm & 0b111;

        (match (mod_rm >> 6) & 0b11 {
            0b00 => {
                let address = direct_address(vm, rm);
                vm.data_segment().ref_byte(address)
            }
            0b01 => {
                let displacement = vm.fetch_byte() as i8 as i16;
                let address = rm_address(vm, rm);
                vm.data_segment().ref_byte(address.wrapping_add_signed(displacement))
            }
            0b10 => {
                let displacement = vm.fetch_word();
                let address = rm_address(vm, rm);
                vm.data_segment().ref_byte(address.wrapping_add(displacement))
            }
            0b11 => vm.registers.ref_reg_byte(rm),
            _ => unreachable!()
        }, (mod_rm >> 3) & 0b111)
    }
}

impl ModRM for u16 {
    type Target = WordWrapper;

    fn mod_rm_lhs(vm: &mut Runtime) -> (WordWrapper, u16) {
        let mod_rm = vm.fetch_byte();
        let rm = mod_rm & 0b111;

        let r2 = match (mod_rm >> 6) & 0b11 {
            0b00 => {
                let address = direct_address(vm, rm);
                vm.data_segment().read_word(address)
            }
            0b01 => {
                let displacement = vm.fetch_byte() as i8 as i16;
                let address = rm_address(vm, rm);
                vm.data_segment().read_word(address.wrapping_add_signed(displacement))
            }
            0b10 => {
                let displacement = vm.fetch_word();
                let address = rm_address(vm, rm);
                vm.data_segment().read_word(address.wrapping_add(displacement))
            }
            0b11 => vm.registers.read_reg_word(rm),
            _ => unreachable!()
        };
        (vm.registers.ref_reg_word((mod_rm >> 3) & 0b111), r2)
    }

    fn mod_rm_rhs(vm: &mut Runtime) -> (WordWrapper, u16) {
        let mod_rm = vm.fetch_byte();
        let rm = mod_rm & 0b111;

        (
            match (mod_rm >> 6) & 0b11 {
                0b00 => {
                    let address = direct_address(vm, rm);
                    vm.data_segment().ref_word(address)
                }
                0b01 => {
                    let displacement = vm.fetch_byte() as i8 as i16;
                    let address = rm_address(vm, rm);
                    vm.data_segment().ref_word(address.wrapping_add_signed(displacement))
                }
                0b10 => {
                    let displacement = vm.fetch_word();
                    let address = rm_address(vm, rm);
                    vm.data_segment().ref_word(address.wrapping_add(displacement))
                }
                0b11 => vm.registers.ref_reg_word(rm),
                _ => unreachable!()
            },
            vm.registers.read_reg_word((mod_rm >> 3) & 0b111)
        )
    }

    fn mod_rm_single(vm: &mut Runtime) -> (WordWrapper, u8) {
        let mod_rm = vm.fetch_byte();
        let rm = mod_rm & 0b111;

        (match (mod_rm >> 6) & 0b11 {
            0b00 => {
                let address = direct_address(vm, rm);
                vm.data_segment().ref_word(address)
            }
            0b01 => {
                let displacement = vm.fetch_byte() as i8 as i16;
                let address = rm_address(vm, rm);
                vm.data_segment().ref_word(address.wrapping_add_signed(displacement))
            }
            0b10 => {
                let displacement = vm.fetch_word();
                let address = rm_address(vm, rm);
                vm.data_segment().ref_word(address.wrapping_add(displacement))
            }
            0b11 => vm.registers.ref_reg_word(rm),
            _ => unreachable!()
        }, (mod_rm >> 3) & 0b111)
    }
}
