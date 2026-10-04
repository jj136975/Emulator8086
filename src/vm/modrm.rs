use crate::vm::memory::Segment;
use crate::vm::registers::{ByteWrapper, OperandRef, WordWrapper};
use crate::vm::runtime::Runtime;

/// An operand width (`u8` or `u16`) and how operands of that width are
/// decoded from a ModR/M byte.
pub trait ModRM: Sized {
    /// Reference to an operand of this width.
    type Target: OperandRef<Self>;

    /// Value of the r/m operand.
    fn read_rm(vm: &mut Runtime, operand: RmOperand) -> Self;
    /// Reference to the r/m operand.
    fn ref_rm(vm: &mut Runtime, operand: RmOperand) -> Self::Target;
    /// Value of register `reg`.
    fn read_reg(vm: &Runtime, reg: u8) -> Self;
    /// Reference to register `reg`.
    fn ref_reg(vm: &mut Runtime, reg: u8) -> Self::Target;

    /// Fetches a ModR/M byte whose reg field names the destination: returns
    /// a reference to that register and the value of the r/m operand.
    fn mod_rm_lhs(vm: &mut Runtime) -> (Self::Target, Self) {
        let mod_rm = vm.cpu.fetch_byte();
        let operand = decode_rm(vm, mod_rm);
        let value = Self::read_rm(vm, operand);
        (Self::ref_reg(vm, reg_field(mod_rm)), value)
    }

    /// Fetches a ModR/M byte whose r/m operand is the destination: returns
    /// a reference to it and the value of the register in the reg field.
    fn mod_rm_rhs(vm: &mut Runtime) -> (Self::Target, Self) {
        let mod_rm = vm.cpu.fetch_byte();
        let operand = decode_rm(vm, mod_rm);
        let target = Self::ref_rm(vm, operand);
        (target, Self::read_reg(vm, reg_field(mod_rm)))
    }

    /// Fetches a ModR/M byte with one operand: returns a reference to the
    /// r/m operand and the reg field, which such instructions use as an
    /// opcode extension or a register number of another kind.
    fn mod_rm_single(vm: &mut Runtime) -> (Self::Target, u8) {
        let mod_rm = vm.cpu.fetch_byte();
        let operand = decode_rm(vm, mod_rm);
        (Self::ref_rm(vm, operand), reg_field(mod_rm))
    }
}

/// The reg field (bits 3-5) of a ModR/M byte.
#[inline(always)]
pub fn reg_field(mod_rm: u8) -> u8 {
    (mod_rm >> 3) & 0b111
}

/// What the mod and r/m fields of a ModR/M byte designate.
#[derive(Clone, Copy)]
pub enum RmOperand {
    /// mod = 11: the register numbered by the r/m field.
    Register(u8),
    /// mod = 00, 01 or 10: a memory operand.
    Memory(MemoryOperand),
}

/// A memory operand: its offset, plus the mod and r/m fields, which select
/// its default segment.
#[derive(Clone, Copy)]
pub struct MemoryOperand {
    mod_val: u8,
    rm: u8,
    pub offset: u16,
}

impl MemoryOperand {
    /// The segment the operand is in: the override prefix if there is one,
    /// otherwise SS for BP-based addressing and DS for everything else.
    #[inline(always)]
    pub fn segment(self, vm: &mut Runtime) -> &mut Segment {
        vm.effective_segment(self.mod_val, self.rm)
    }

    /// Reads the far pointer stored at the operand: the offset word, then
    /// the segment word two bytes further on (wrapping inside the segment).
    #[inline(always)]
    pub fn read_far_pointer(self, vm: &mut Runtime) -> (u16, u16) {
        let segment = self.segment(vm);
        (
            segment.read_word(self.offset),
            segment.read_word(self.offset.wrapping_add(2)),
        )
    }
}

impl RmOperand {
    /// The operand as a memory operand, for LEA, LES and LDS. Their register
    /// forms are undefined on the 8086; here the register's value is taken
    /// as the offset.
    #[inline(always)]
    pub fn as_memory(self, vm: &Runtime) -> MemoryOperand {
        match self {
            RmOperand::Memory(memory) => memory,
            RmOperand::Register(rm) => MemoryOperand {
                mod_val: 0b11,
                rm,
                offset: vm.cpu.registers.read_reg_word(rm),
            },
        }
    }
}

/// Decodes the mod and r/m fields of `mod_rm`, fetching the displacement
/// bytes that follow it, if any. This is the only effective-address decoder:
/// every instruction with a ModR/M byte goes through it.
#[inline(always)]
pub fn decode_rm(vm: &mut Runtime, mod_rm: u8) -> RmOperand {
    let rm = mod_rm & 0b111;
    let mod_val = mod_rm >> 6;
    let offset = match mod_val {
        // mod = 00 with r/m = 110 is a bare 16-bit address
        0b00 if rm == 0b110 => vm.cpu.fetch_word(),
        0b00 => base_address(vm, rm),
        0b01 => {
            let displacement = vm.cpu.fetch_byte() as i8 as i16;
            base_address(vm, rm).wrapping_add_signed(displacement)
        }
        0b10 => {
            let displacement = vm.cpu.fetch_word();
            base_address(vm, rm).wrapping_add(displacement)
        }
        _ => return RmOperand::Register(rm),
    };
    RmOperand::Memory(MemoryOperand { mod_val, rm, offset })
}

/// Sum of the base and index registers selected by the r/m field.
#[inline(always)]
fn base_address(vm: &Runtime, rm: u8) -> u16 {
    match rm {
        0b000 => vm.cpu.registers.bx.word().wrapping_add(vm.cpu.registers.si.word()),
        0b001 => vm.cpu.registers.bx.word().wrapping_add(vm.cpu.registers.di.word()),
        0b010 => vm.cpu.registers.bp.word().wrapping_add(vm.cpu.registers.si.word()),
        0b011 => vm.cpu.registers.bp.word().wrapping_add(vm.cpu.registers.di.word()),
        0b100 => vm.cpu.registers.si.word(),
        0b101 => vm.cpu.registers.di.word(),
        0b110 => vm.cpu.registers.bp.word(),
        0b111 => vm.cpu.registers.bx.word(),
        _ => unreachable!()
    }
}

impl ModRM for u8 {
    type Target = ByteWrapper;

    #[inline(always)]
    fn read_rm(vm: &mut Runtime, operand: RmOperand) -> u8 {
        match operand {
            RmOperand::Register(rm) => vm.cpu.registers.read_reg_byte(rm),
            RmOperand::Memory(memory) => memory.segment(vm).read_byte(memory.offset),
        }
    }

    #[inline(always)]
    fn ref_rm(vm: &mut Runtime, operand: RmOperand) -> ByteWrapper {
        match operand {
            RmOperand::Register(rm) => vm.cpu.registers.ref_reg_byte(rm),
            RmOperand::Memory(memory) => memory.segment(vm).ref_byte(memory.offset),
        }
    }

    #[inline(always)]
    fn read_reg(vm: &Runtime, reg: u8) -> u8 {
        vm.cpu.registers.read_reg_byte(reg)
    }

    #[inline(always)]
    fn ref_reg(vm: &mut Runtime, reg: u8) -> ByteWrapper {
        vm.cpu.registers.ref_reg_byte(reg)
    }
}

impl ModRM for u16 {
    type Target = WordWrapper;

    #[inline(always)]
    fn read_rm(vm: &mut Runtime, operand: RmOperand) -> u16 {
        match operand {
            RmOperand::Register(rm) => vm.cpu.registers.read_reg_word(rm),
            RmOperand::Memory(memory) => memory.segment(vm).read_word(memory.offset),
        }
    }

    #[inline(always)]
    fn ref_rm(vm: &mut Runtime, operand: RmOperand) -> WordWrapper {
        match operand {
            RmOperand::Register(rm) => vm.cpu.registers.ref_reg_word(rm),
            RmOperand::Memory(memory) => memory.segment(vm).ref_word(memory.offset),
        }
    }

    #[inline(always)]
    fn read_reg(vm: &Runtime, reg: u8) -> u16 {
        vm.cpu.registers.read_reg_word(reg)
    }

    #[inline(always)]
    fn ref_reg(vm: &mut Runtime, reg: u8) -> WordWrapper {
        vm.cpu.registers.ref_reg_word(reg)
    }
}
