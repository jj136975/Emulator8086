use std::fmt::{Debug, Formatter};
use std::io::Cursor;

use byteorder::{LittleEndian, ReadBytesExt};

use crate::a_out::executable::Executable;
use crate::vm::instructions::process;
use crate::vm::memory::Memory;
use crate::vm::registers::{ByteWrapper, Registers, WordWrapper};
use crate::vm::runtime::CpuFlag::{AuxCarry, Carry, Directional, Interrupt, Overflow, Parity, Sign, Trap, Zero};

pub enum CpuFlag {
    Carry = 0,
    Parity = 2,
    AuxCarry = 4,
    Zero = 6,
    Sign = 7,
    Trap = 8,
    Interrupt = 9,
    Directional = 10,
    Overflow = 11,
}

pub struct Runtime<'vm> {
    pub registers: Registers,
    pub memory: Memory,
    cursor: Cursor<&'vm mut [u8]>,
    flags: u16,
    running: bool,
}

impl<'vm> Runtime<'vm> {
    pub fn new(exe: &'vm mut Executable) -> Self {
        let mut vm = Self {
            registers: Registers::default(),
            memory: Memory::new(exe.data_segment.as_slice()),
            cursor: Cursor::new(exe.text_segment.as_mut_slice()),
            flags: 0,
            running: true,
        };
        vm.set_flag(Interrupt);
        vm
    }

    #[inline(always)]
    fn ref_reg_byte(&mut self, reg: u8) -> ByteWrapper {
        match reg {
            0b000 => self.registers.ax.ref_mut_low(),
            0b001 => self.registers.cx.ref_mut_low(),
            0b010 => self.registers.dx.ref_mut_low(),
            0b011 => self.registers.bx.ref_mut_low(),
            0b100 => self.registers.ax.ref_mut_high(),
            0b101 => self.registers.cx.ref_mut_high(),
            0b110 => self.registers.dx.ref_mut_high(),
            0b111 => self.registers.bx.ref_mut_high(),
            _ => panic!()
        }
    }

    #[inline(always)]
    fn read_reg_byte(&self, reg: u8) -> u8 {
        match reg {
            0b000 => self.registers.ax.low(),
            0b001 => self.registers.cx.low(),
            0b010 => self.registers.dx.low(),
            0b011 => self.registers.bx.low(),
            0b100 => self.registers.ax.high(),
            0b101 => self.registers.cx.high(),
            0b110 => self.registers.dx.high(),
            0b111 => self.registers.bx.high(),
            _ => panic!()
        }
    }

    #[inline(always)]
    fn ref_reg_word(&mut self, reg: u8) -> WordWrapper {
        WordWrapper::from_register(match reg {
            0b000 => &mut self.registers.ax,
            0b001 => &mut self.registers.cx,
            0b010 => &mut self.registers.dx,
            0b011 => &mut self.registers.bx,
            0b100 => &mut self.registers.ax,
            0b101 => &mut self.registers.cx,
            0b110 => &mut self.registers.dx,
            0b111 => &mut self.registers.bx,
            _ => panic!()
        })
    }

    #[inline(always)]
    fn read_reg_word(&self, reg: u8) -> u16 {
        match reg {
            0b000 => self.registers.ax.word(),
            0b001 => self.registers.cx.word(),
            0b010 => self.registers.dx.word(),
            0b011 => self.registers.bx.word(),
            0b100 => self.registers.sp.word(),
            0b101 => self.registers.bp.word(),
            0b110 => self.registers.si.word(),
            0b111 => self.registers.di.word(),
            _ => panic!()
        }
    }
}

impl Runtime<'_> {
    #[inline]
    pub fn fetch_byte(&mut self) -> u8 {
        self.cursor.read_u8().unwrap()
    }

    #[inline]
    pub fn fetch_word(&mut self) -> u16 {
        self.cursor.read_u16::<LittleEndian>().unwrap()
    }

    #[inline(always)]
    pub fn set_flag(&mut self, flag: CpuFlag) {
        self.flags |= 1u16 << (flag as u8);
    }
    #[inline(always)]
    pub fn unset_flag(&mut self, flag: CpuFlag) {
        self.flags &= !(1u16 << (flag as u8));
    }

    #[inline(always)]
    pub fn update_flag(&mut self, flag: CpuFlag, active: bool) {
        if active {
            self.set_flag(flag);
        } else {
            self.unset_flag(flag);
        }
    }

    #[inline(always)]
    pub fn check_flag(&self, flag: CpuFlag) -> bool {
        (self.flags & 1u16 << (flag as u8)) != 0
    }

    pub fn run(&mut self) {
        while self.running {
            println!("{:?}", self);
            process(self);
            //self.running = false;
        }
    }
}

impl Debug for Runtime<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.registers)?;
        writeln!(f, "================ FLAGS ===================")?;
        writeln!(f, " C  | P  | AC | Z  | S  | T  | I  | D  | O")?;
        writeln!(f,
                 " {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}  | {}",
                 self.check_flag(Carry) as u8,
                 self.check_flag(Parity) as u8,
                 self.check_flag(AuxCarry) as u8,
                 self.check_flag(Zero) as u8,
                 self.check_flag(Sign) as u8,
                 self.check_flag(Trap) as u8,
                 self.check_flag(Interrupt) as u8,
                 self.check_flag(Directional) as u8,
                 self.check_flag(Overflow) as u8,
        )?;
        Ok(())
    }
}

pub trait ModRM<In, Out> {
    fn mod_rm_lhs(&mut self) -> (In, Out);
    fn mod_rm_rhs(&mut self) -> (In, Out);
    fn mod_rm_single(&mut self) -> In;
}

#[inline(always)]
fn rm_address(vm: &Runtime, rm: u8) -> u16 {
    match rm {
        0b000 => vm.registers.bx.word() + vm.registers.si.word(),
        0b001 => vm.registers.bx.word() + vm.registers.di.word(),
        0b010 => vm.registers.bp.word() + vm.registers.si.word(),
        0b011 => vm.registers.bp.word() + vm.registers.di.word(),
        0b100 => vm.registers.si.word(),
        0b101 => vm.registers.di.word(),
        0b110 => vm.registers.bp.word(),
        0b111 => vm.registers.bx.word(),
        _ => panic!()
    }
}

#[inline(always)]
fn direct_address(vm: &mut Runtime, rm: u8) -> u16 {
    match rm {
        0b000 => vm.registers.bx.word() + vm.registers.si.word(),
        0b001 => vm.registers.bx.word() + vm.registers.di.word(),
        0b010 => vm.registers.bp.word() + vm.registers.si.word(),
        0b011 => vm.registers.bp.word() + vm.registers.di.word(),
        0b100 => vm.registers.si.word(),
        0b101 => vm.registers.di.word(),
        0b110 => vm.fetch_word(),
        0b111 => vm.registers.bx.word(),
        _ => panic!()
    }
}

impl ModRM<ByteWrapper, u8> for Runtime<'_> {
    fn mod_rm_lhs(&mut self) -> (ByteWrapper, u8) {
        let mod_rm = self.fetch_byte();
        let rm = mod_rm & 0b111;

        let r2 = match (mod_rm >> 6) & 0b11 {
            0b00 => {
                let address = direct_address(self, rm);
                self.memory.read_byte_data(address)
            }
            0b01 => {
                let displacement = self.fetch_byte() as i8 as i16;
                let address = rm_address(self, rm);
                self.memory.read_byte_data(address.checked_add_signed(displacement)
                    .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
            }
            0b10 => {
                let displacement = self.fetch_word();
                let address = rm_address(self, rm);
                self.memory.read_byte_data(address.checked_add(displacement)
                    .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
            }
            0b11 => self.read_reg_byte(rm),
            _ => panic!()
        };
        (self.ref_reg_byte((mod_rm >> 3) & 0b111), r2)
    }

    fn mod_rm_rhs(&mut self) -> (ByteWrapper, u8) {
        let mod_rm = self.fetch_byte();
        let rm = mod_rm & 0b111;

        (
            match (mod_rm >> 6) & 0b11 {
                0b00 => {
                    let address = direct_address(self, rm);
                    self.memory.ref_byte_data(address)
                }
                0b01 => {
                    let displacement = self.fetch_byte() as i8 as i16;
                    let address = rm_address(self, rm);
                    self.memory.ref_byte_data(address.checked_add_signed(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
                }
                0b10 => {
                    let displacement = self.fetch_word();
                    let address = rm_address(self, rm);
                    self.memory.ref_byte_data(address.checked_add(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
                }
                0b11 => self.ref_reg_byte(rm),
                _ => panic!()
            },
            self.read_reg_byte((mod_rm >> 3) & 0b111)
        )
    }

    fn mod_rm_single(&mut self) -> ByteWrapper {
        let mod_rm = self.fetch_byte();
        todo!()
    }
}

impl ModRM<WordWrapper, u16> for Runtime<'_> {
    fn mod_rm_lhs(&mut self) -> (WordWrapper, u16) {
        let mod_rm = self.fetch_byte();
        let rm = mod_rm & 0b111;

        let r2 = match (mod_rm >> 6) & 0b11 {
            0b00 => {
                let address = direct_address(self, rm);
                self.memory.read_word_data(address)
            }
            0b01 => {
                let displacement = self.fetch_byte() as i8 as i16;
                let address = rm_address(self, rm);
                self.memory.read_word_data(address.checked_add_signed(displacement)
                    .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
            }
            0b10 => {
                let displacement = self.fetch_word();
                let address = rm_address(self, rm);
                self.memory.read_word_data(address.checked_add(displacement)
                    .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
            }
            0b11 => self.read_reg_word(rm),
            _ => panic!()
        };
        (self.ref_reg_word((mod_rm >> 3) & 0b111), r2)
    }

    fn mod_rm_rhs(&mut self) -> (WordWrapper, u16) {
        let mod_rm = self.fetch_byte();
        let rm = mod_rm & 0b111;

        (
            match (mod_rm >> 6) & 0b11 {
                0b00 => {
                    let address = direct_address(self, rm);
                    self.memory.ref_word_data(address)
                }
                0b01 => {
                    let displacement = self.fetch_byte() as i8 as i16;
                    let address = rm_address(self, rm);
                    self.memory.ref_word_data(address.checked_add_signed(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
                }
                0b10 => {
                    let displacement = self.fetch_word();
                    let address = rm_address(self, rm);
                    self.memory.ref_word_data(address.checked_add(displacement)
                        .unwrap_or_else(|| panic!("Invalid memory address: {} + {}", address, displacement)))
                }
                0b11 => self.ref_reg_word(rm),
                _ => panic!()
            },
            self.read_reg_word((mod_rm >> 3) & 0b111)
        )
    }

    fn mod_rm_single(&mut self) -> WordWrapper {
        let mod_rm = self.fetch_byte();
        todo!()
    }
}