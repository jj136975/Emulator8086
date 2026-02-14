use crate::vm::instructions::process;
use crate::vm::memory::Memory;
use crate::vm::runtime::CpuFlag::*;
use crate::vm::runtime::Runtime;

fn setup(code: &[u8]) -> Runtime {
    let mut vm = Runtime::new_test();
    // Set SP to 0xFFFE for stack room, PC at 0
    vm.registers.sp.set(0xFFFE);
    vm.registers.pc.set(0);
    // Load code at CS:0000 (physical 0x00000 since CS=0)
    Memory::copy_data(&mut vm.memory, 0, code);
    vm
}

fn exec(vm: &mut Runtime) {
    process(vm);
}

fn exec_n(vm: &mut Runtime, n: usize) {
    for _ in 0..n {
        process(vm);
    }
}

fn parity(v: u8) -> bool {
    v.count_ones() & 1 == 0
}

// ========================================================================
// DATA TRANSFER
// ========================================================================

#[test]
fn mov_reg16_imm16() {
    // MOV AX,0x1234  (B8 34 12)
    let mut vm = setup(&[0xB8, 0x34, 0x12]);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x1234);
}

#[test]
fn mov_reg8_imm8() {
    // MOV AL,0x42  (B0 42)
    let mut vm = setup(&[0xB0, 0x42]);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn mov_reg8_imm8_high() {
    // MOV AH,0xAB  (B4 AB)
    let mut vm = setup(&[0xB4, 0xAB]);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.high(), 0xAB);
}

#[test]
fn mov_bx_imm16() {
    // MOV BX,0xBEEF (BB EF BE)
    let mut vm = setup(&[0xBB, 0xEF, 0xBE]);
    exec(&mut vm);
    assert_eq!(vm.registers.bx.word(), 0xBEEF);
}

#[test]
fn mov_reg_reg_word() {
    // MOV AX,0x1234; MOV BX,AX  (89 C3 = MOV BX,AX: mod=11 reg=000(AX) rm=011(BX))
    let mut vm = setup(&[0xB8, 0x34, 0x12, 0x89, 0xC3]);
    exec_n(&mut vm, 2);
    assert_eq!(vm.registers.bx.word(), 0x1234);
}

#[test]
fn mov_reg_reg_byte() {
    // MOV AL,0x55; MOV CL,AL  (88 C1 = MOV CL,AL: mod=11 reg=000(AL) rm=001(CL))
    let mut vm = setup(&[0xB0, 0x55, 0x88, 0xC1]);
    exec_n(&mut vm, 2);
    assert_eq!(vm.registers.cx.low(), 0x55);
}

#[test]
fn mov_mem_reg_word() {
    // MOV AX,0xABCD; MOV [0x100],AX  (A3 00 01)
    let mut vm = setup(&[0xB8, 0xCD, 0xAB, 0xA3, 0x00, 0x01]);
    exec_n(&mut vm, 2);
    assert_eq!(vm.registers.ds.read_word(0x100), 0xABCD);
}

#[test]
fn mov_al_moffs() {
    // Store 0x77 at DS:0x200, then MOV AL,[0x200] (A0 00 02)
    let mut vm = setup(&[0xA0, 0x00, 0x02]);
    vm.registers.ds.write_byte(0x200, 0x77);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x77);
}

#[test]
fn mov_ax_moffs() {
    // Store 0xBEEF at DS:0x200, then MOV AX,[0x200] (A1 00 02)
    let mut vm = setup(&[0xA1, 0x00, 0x02]);
    vm.registers.ds.write_word(0x200, 0xBEEF);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0xBEEF);
}

#[test]
fn mov_moffs_al() {
    // MOV AL,0x33; MOV [0x300],AL (A2 00 03)
    let mut vm = setup(&[0xB0, 0x33, 0xA2, 0x00, 0x03]);
    exec_n(&mut vm, 2);
    assert_eq!(vm.registers.ds.read_byte(0x300), 0x33);
}

#[test]
fn mov_rm_imm_word() {
    // MOV WORD [0x100],0x5678  (C7 06 00 01 78 56)
    // ModRM: mod=00 reg=000 rm=110(disp16) => 0x06
    let mut vm = setup(&[0xC7, 0x06, 0x00, 0x01, 0x78, 0x56]);
    exec(&mut vm);
    assert_eq!(vm.registers.ds.read_word(0x100), 0x5678);
}

#[test]
fn mov_rm_imm_byte() {
    // MOV BYTE [0x100],0xAA  (C6 06 00 01 AA)
    let mut vm = setup(&[0xC6, 0x06, 0x00, 0x01, 0xAA]);
    exec(&mut vm);
    assert_eq!(vm.registers.ds.read_byte(0x100), 0xAA);
}

#[test]
fn mov_sreg_rm() {
    // MOV BX,0x1000; MOV ES,BX  (8E C3 = mod=11 reg=000(ES) rm=011(BX))
    let mut vm = setup(&[0xBB, 0x00, 0x10, 0x8E, 0xC3]);
    exec_n(&mut vm, 2);
    assert_eq!(vm.registers.es.reg().word(), 0x1000);
}

#[test]
fn mov_rm_sreg() {
    // MOV AX,DS  (8C D8 = mod=11 reg=011(DS) rm=000(AX))
    let mut vm = setup(&[0x8C, 0xD8]);
    vm.registers.ds.reg_mut().set(0x2000);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x2000);
}

#[test]
fn xchg_reg_ax() {
    // MOV AX,0x1111; MOV CX,0x2222; XCHG CX,AX  (91)
    let mut vm = setup(&[0xB8, 0x11, 0x11, 0xB9, 0x22, 0x22, 0x91]);
    exec_n(&mut vm, 3);
    assert_eq!(vm.registers.ax.word(), 0x2222);
    assert_eq!(vm.registers.cx.word(), 0x1111);
}

#[test]
fn xchg_rm_reg_word() {
    // MOV AX,0xAAAA; MOV BX,0xBBBB; XCHG AX,BX (87 D8 = mod=11 reg=011(BX) rm=000(AX))
    let mut vm = setup(&[0xB8, 0xAA, 0xAA, 0xBB, 0xBB, 0xBB, 0x87, 0xD8]);
    exec_n(&mut vm, 3);
    assert_eq!(vm.registers.ax.word(), 0xBBBB);
    assert_eq!(vm.registers.bx.word(), 0xAAAA);
}

#[test]
fn lea_basic() {
    // LEA AX,[BX+SI]  (8D 00 = mod=00 reg=000(AX) rm=000(BX+SI))
    let mut vm = setup(&[0x8D, 0x00]);
    vm.registers.bx.set(0x100);
    vm.registers.si.set(0x050);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x150);
}

#[test]
fn lea_disp16() {
    // LEA AX,[0x1234]  (8D 06 34 12 = mod=00 reg=000 rm=110(disp16))
    let mut vm = setup(&[0x8D, 0x06, 0x34, 0x12]);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x1234);
}

#[test]
fn lds_basic() {
    // LDS AX,[0x200]  (C5 06 00 02 = mod=00 reg=000 rm=110)
    // At DS:0x200: offset=0x1234, segment=0x5678
    let mut vm = setup(&[0xC5, 0x06, 0x00, 0x02]);
    vm.registers.ds.write_word(0x200, 0x1234);
    vm.registers.ds.write_word(0x202, 0x5678);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x1234);
    assert_eq!(vm.registers.ds.reg().word(), 0x5678);
}

#[test]
fn les_basic() {
    // LES BX,[0x200]  (C4 1E 00 02 = mod=00 reg=011(BX) rm=110)
    let mut vm = setup(&[0xC4, 0x1E, 0x00, 0x02]);
    vm.registers.ds.write_word(0x200, 0xAAAA);
    vm.registers.ds.write_word(0x202, 0xBBBB);
    exec(&mut vm);
    assert_eq!(vm.registers.bx.word(), 0xAAAA);
    assert_eq!(vm.registers.es.reg().word(), 0xBBBB);
}

#[test]
fn push_pop_reg() {
    // MOV AX,0x4321; PUSH AX; MOV AX,0; POP BX
    let mut vm = setup(&[
        0xB8, 0x21, 0x43, // MOV AX,0x4321
        0x50,             // PUSH AX
        0xB8, 0x00, 0x00, // MOV AX,0
        0x5B,             // POP BX
    ]);
    exec_n(&mut vm, 4);
    assert_eq!(vm.registers.bx.word(), 0x4321);
    assert_eq!(vm.registers.ax.word(), 0x0000);
}

#[test]
fn push_pop_sreg() {
    // PUSH DS (0x1E); POP ES (0x07)
    let mut vm = setup(&[0x1E, 0x07]);
    vm.registers.ds.reg_mut().set(0x3000);
    exec_n(&mut vm, 2);
    assert_eq!(vm.registers.es.reg().word(), 0x3000);
}

#[test]
fn push_cs() {
    // PUSH CS (0x0E); POP AX (0x58)
    // CS=0x0000 (default in test), so pushed value is 0
    let mut vm = setup(&[0x0E, 0x58]);
    exec_n(&mut vm, 2);
    assert_eq!(vm.registers.ax.word(), 0x0000);
}

#[test]
fn pushf_popf() {
    let mut vm = setup(&[0x9C, 0x9D]); // PUSHF; POPF
    vm.flags = 0x0246; // ZF+PF+reserved
    exec(&mut vm); // PUSHF
    vm.flags = 0;
    exec(&mut vm); // POPF
    assert_eq!(vm.flags, 0x0246);
}

#[test]
fn lahf_sahf() {
    let mut vm = setup(&[0x9F, 0x9E]); // LAHF; SAHF
    vm.flags = 0x00D7; // SF+ZF+AF+PF+CF
    exec(&mut vm); // LAHF
    assert_eq!(vm.registers.ax.high(), 0xD7);

    // Now clear flags and restore from AH
    vm.flags = 0xFF00;
    exec(&mut vm); // SAHF
    assert_eq!(vm.flags & 0xFF, 0xD7);
}

#[test]
fn xlat_basic() {
    // XLAT (0xD7): AL = DS:[BX+AL]
    let mut vm = setup(&[0xD7]);
    vm.registers.bx.set(0x200);
    vm.registers.ax.set_low(5);
    vm.registers.ds.write_byte(0x205, 0x99);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x99);
}

#[test]
fn cbw_positive() {
    // CBW (0x98): sign-extend AL into AH
    let mut vm = setup(&[0x98]);
    vm.registers.ax.set_low(0x7F);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x007F);
}

#[test]
fn cbw_negative() {
    let mut vm = setup(&[0x98]);
    vm.registers.ax.set_low(0x80);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0xFF80);
}

#[test]
fn cwd_positive() {
    // CWD (0x99): sign-extend AX into DX
    let mut vm = setup(&[0x99]);
    vm.registers.ax.set(0x7FFF);
    exec(&mut vm);
    assert_eq!(vm.registers.dx.word(), 0x0000);
}

#[test]
fn cwd_negative() {
    let mut vm = setup(&[0x99]);
    vm.registers.ax.set(0x8000);
    exec(&mut vm);
    assert_eq!(vm.registers.dx.word(), 0xFFFF);
}

// ========================================================================
// ARITHMETIC
// ========================================================================

#[test]
fn add_al_imm() {
    // ADD AL,0x10 (04 10)
    let mut vm = setup(&[0x04, 0x10]);
    vm.registers.ax.set_low(0x20);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x30);
    assert!(!vm.check_flag(Zero));
    assert!(!vm.check_flag(Carry));
    assert!(!vm.check_flag(Overflow));
}

#[test]
fn add_ax_imm() {
    // ADD AX,0x1000 (05 00 10)
    let mut vm = setup(&[0x05, 0x00, 0x10]);
    vm.registers.ax.set(0x2000);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x3000);
}

#[test]
fn add_overflow() {
    // ADD AL,0x01 with AL=0x7F -> overflow
    let mut vm = setup(&[0x04, 0x01]);
    vm.registers.ax.set_low(0x7F);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x80);
    assert!(vm.check_flag(Overflow));
    assert!(vm.check_flag(Sign));
    assert!(!vm.check_flag(Zero));
}

#[test]
fn add_carry() {
    // ADD AL,0x01 with AL=0xFF -> carry
    let mut vm = setup(&[0x04, 0x01]);
    vm.registers.ax.set_low(0xFF);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x00);
    assert!(vm.check_flag(Carry));
    assert!(vm.check_flag(Zero));
}

#[test]
fn add_reg_reg() {
    // MOV AX,5; MOV BX,3; ADD AX,BX (01 D8 = mod=11 reg=011(BX) rm=000(AX))
    let mut vm = setup(&[0xB8, 0x05, 0x00, 0xBB, 0x03, 0x00, 0x01, 0xD8]);
    exec_n(&mut vm, 3);
    assert_eq!(vm.registers.ax.word(), 8);
}

#[test]
fn add_parity_flag() {
    // ADD AL,0x02 with AL=0x01 -> result 0x03 = 0b11 -> 2 bits -> even parity -> PF set
    let mut vm = setup(&[0x04, 0x02]);
    vm.registers.ax.set_low(0x01);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x03);
    assert!(vm.check_flag(Parity)); // even number of 1-bits
}

#[test]
fn adc_with_carry() {
    // ADC AL,0x01 with AL=0x10 and CF=1
    let mut vm = setup(&[0x14, 0x01]);
    vm.registers.ax.set_low(0x10);
    vm.set_flag(Carry);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x12);
}

#[test]
fn adc_without_carry() {
    let mut vm = setup(&[0x14, 0x01]);
    vm.registers.ax.set_low(0x10);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x11);
}

#[test]
fn adc_ax_imm_with_carry() {
    // ADC AX,0x0001 with AX=0xFFFF and CF=1 -> 0x0001, CF=1
    let mut vm = setup(&[0x15, 0x01, 0x00]);
    vm.registers.ax.set(0xFFFF);
    vm.set_flag(Carry);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0001);
    assert!(vm.check_flag(Carry));
}

#[test]
fn sub_al_imm() {
    // SUB AL,0x05 (2C 05)
    let mut vm = setup(&[0x2C, 0x05]);
    vm.registers.ax.set_low(0x10);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x0B);
    assert!(!vm.check_flag(Carry));
    assert!(!vm.check_flag(Zero));
}

#[test]
fn sub_borrow() {
    // SUB AL,0x10 with AL=0x05 -> borrow
    let mut vm = setup(&[0x2C, 0x10]);
    vm.registers.ax.set_low(0x05);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0xF5);
    assert!(vm.check_flag(Carry));
    assert!(vm.check_flag(Sign));
}

#[test]
fn sub_zero() {
    let mut vm = setup(&[0x2C, 0x05]);
    vm.registers.ax.set_low(0x05);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x00);
    assert!(vm.check_flag(Zero));
    assert!(parity(0));
    assert!(vm.check_flag(Parity));
}

#[test]
fn sbb_al_with_carry() {
    // SBB AL,0x01 with AL=0x10 and CF=1 -> 0x0E
    let mut vm = setup(&[0x1C, 0x01]);
    vm.registers.ax.set_low(0x10);
    vm.set_flag(Carry);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x0E);
}

#[test]
fn cmp_equal() {
    // CMP AL,0x42 with AL=0x42 -> ZF set, no register change
    let mut vm = setup(&[0x3C, 0x42]);
    vm.registers.ax.set_low(0x42);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42); // unchanged
    assert!(vm.check_flag(Zero));
    assert!(!vm.check_flag(Carry));
}

#[test]
fn cmp_less() {
    // CMP AL,0x50 with AL=0x10 -> CF set (borrow)
    let mut vm = setup(&[0x3C, 0x50]);
    vm.registers.ax.set_low(0x10);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x10); // unchanged
    assert!(vm.check_flag(Carry));
    assert!(!vm.check_flag(Zero));
}

#[test]
fn cmp_ax_imm() {
    // CMP AX,0x1000 (3D 00 10) with AX=0x1000 -> ZF
    let mut vm = setup(&[0x3D, 0x00, 0x10]);
    vm.registers.ax.set(0x1000);
    exec(&mut vm);
    assert!(vm.check_flag(Zero));
}

#[test]
fn inc_reg16() {
    // INC AX (0x40)
    let mut vm = setup(&[0x40]);
    vm.registers.ax.set(0x00FF);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0100);
    assert!(!vm.check_flag(Zero));
}

#[test]
fn inc_preserves_carry() {
    // INC AX should NOT affect CF
    let mut vm = setup(&[0x40]);
    vm.registers.ax.set(0);
    vm.set_flag(Carry);
    exec(&mut vm);
    assert!(vm.check_flag(Carry)); // CF preserved
    assert_eq!(vm.registers.ax.word(), 1);
}

#[test]
fn inc_overflow() {
    // INC AX with AX=0x7FFF -> OF set
    let mut vm = setup(&[0x40]);
    vm.registers.ax.set(0x7FFF);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x8000);
    assert!(vm.check_flag(Overflow));
}

#[test]
fn dec_reg16() {
    // DEC CX (0x49)
    let mut vm = setup(&[0x49]);
    vm.registers.cx.set(0x0100);
    exec(&mut vm);
    assert_eq!(vm.registers.cx.word(), 0x00FF);
}

#[test]
fn dec_to_zero() {
    let mut vm = setup(&[0x49]);
    vm.registers.cx.set(1);
    exec(&mut vm);
    assert_eq!(vm.registers.cx.word(), 0);
    assert!(vm.check_flag(Zero));
}

#[test]
fn dec_preserves_carry() {
    let mut vm = setup(&[0x48]); // DEC AX
    vm.registers.ax.set(0);
    vm.set_flag(Carry);
    exec(&mut vm);
    assert!(vm.check_flag(Carry)); // CF preserved
    assert_eq!(vm.registers.ax.word(), 0xFFFF);
}

#[test]
fn neg_byte() {
    // NEG AL: F6 /3 => F6 D8 (mod=11 reg=011 rm=000)
    let mut vm = setup(&[0xF6, 0xD8]);
    vm.registers.ax.set_low(0x05);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0xFB); // -5 = 0xFB
    assert!(vm.check_flag(Carry)); // CF=1 when operand != 0
    assert!(vm.check_flag(Sign));
}

#[test]
fn neg_zero() {
    let mut vm = setup(&[0xF6, 0xD8]);
    vm.registers.ax.set_low(0x00);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x00);
    assert!(!vm.check_flag(Carry)); // CF=0 when operand == 0
    assert!(vm.check_flag(Zero));
}

#[test]
fn neg_word() {
    // NEG AX: F7 D8 (mod=11 reg=011 rm=000)
    let mut vm = setup(&[0xF7, 0xD8]);
    vm.registers.ax.set(0x0001);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0xFFFF);
    assert!(vm.check_flag(Carry));
}

#[test]
fn mul_byte() {
    // MUL CL: F6 E1 (mod=11 reg=100 rm=001)
    // AL * CL -> AX
    let mut vm = setup(&[0xF6, 0xE1]);
    vm.registers.ax.set_low(10);
    vm.registers.cx.set_low(20);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 200);
    assert!(!vm.check_flag(Carry)); // high byte is 0
    assert!(!vm.check_flag(Overflow));
}

#[test]
fn mul_byte_overflow() {
    // MUL CL: 0xFF * 0xFF = 0xFE01
    let mut vm = setup(&[0xF6, 0xE1]);
    vm.registers.ax.set_low(0xFF);
    vm.registers.cx.set_low(0xFF);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0xFE01);
    assert!(vm.check_flag(Carry));
    assert!(vm.check_flag(Overflow));
}

#[test]
fn mul_word() {
    // MUL BX: F7 E3 (mod=11 reg=100 rm=011)
    // AX * BX -> DX:AX
    let mut vm = setup(&[0xF7, 0xE3]);
    vm.registers.ax.set(0x100);
    vm.registers.bx.set(0x100);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0000);
    assert_eq!(vm.registers.dx.word(), 0x0001); // 0x10000
}

#[test]
fn imul_byte() {
    // IMUL CL: F6 E9 (mod=11 reg=101 rm=001)
    // AL(signed) * CL(signed) -> AX
    let mut vm = setup(&[0xF6, 0xE9]);
    vm.registers.ax.set_low(0xFE); // -2
    vm.registers.cx.set_low(3);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0xFFFA); // -6
}

#[test]
fn imul_word() {
    // IMUL BX: F7 EB (mod=11 reg=101 rm=011)
    let mut vm = setup(&[0xF7, 0xEB]);
    vm.registers.ax.set(0xFFFE); // -2
    vm.registers.bx.set(0x0003); // 3
    exec(&mut vm);
    // -2 * 3 = -6 = 0xFFFF_FFFA -> DX=0xFFFF, AX=0xFFFA
    assert_eq!(vm.registers.ax.word(), 0xFFFA);
    assert_eq!(vm.registers.dx.word(), 0xFFFF);
}

#[test]
fn div_byte() {
    // DIV CL: F6 F1 (mod=11 reg=110 rm=001)
    // AX / CL -> AL=quotient, AH=remainder
    let mut vm = setup(&[0xF6, 0xF1]);
    vm.registers.ax.set(17);
    vm.registers.cx.set_low(5);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 3);  // 17/5 = 3
    assert_eq!(vm.registers.ax.high(), 2); // 17%5 = 2
}

#[test]
fn div_word() {
    // DIV BX: F7 F3 (mod=11 reg=110 rm=011)
    // DX:AX / BX -> AX=quotient, DX=remainder
    let mut vm = setup(&[0xF7, 0xF3]);
    vm.registers.dx.set(0);
    vm.registers.ax.set(100);
    vm.registers.bx.set(7);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 14); // 100/7 = 14
    assert_eq!(vm.registers.dx.word(), 2);  // 100%7 = 2
}

#[test]
fn idiv_byte() {
    // IDIV CL: F6 F9 (mod=11 reg=111 rm=001)
    let mut vm = setup(&[0xF6, 0xF9]);
    vm.registers.ax.set((-17i16) as u16); // 0xFFEF
    vm.registers.cx.set_low(5);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low() as i8, -3); // -17/5 = -3
    assert_eq!(vm.registers.ax.high() as i8, -2); // -17%5 = -2
}

#[test]
fn idiv_word() {
    // IDIV BX: F7 FB (mod=11 reg=111 rm=011)
    let mut vm = setup(&[0xF7, 0xFB]);
    vm.registers.dx.set(0xFFFF);
    vm.registers.ax.set((-100i16) as u16);
    vm.registers.bx.set(7);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word() as i16, -14);
    assert_eq!(vm.registers.dx.word() as i16, -2);
}

#[test]
fn group_83_add_sign_extend() {
    // ADD AX,imm8 (sign-extended): 83 C0 FF -> ADD AX,-1 (0xFFFF)
    let mut vm = setup(&[0x83, 0xC0, 0xFF]);
    vm.registers.ax.set(0x0005);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0004);
}

#[test]
fn group_83_cmp() {
    // CMP AX,0x05: 83 F8 05 (mod=11 reg=111(CMP) rm=000(AX))
    let mut vm = setup(&[0x83, 0xF8, 0x05]);
    vm.registers.ax.set(0x0005);
    exec(&mut vm);
    assert!(vm.check_flag(Zero));
    assert_eq!(vm.registers.ax.word(), 0x0005); // unchanged
}

#[test]
fn group_80_and_byte() {
    // AND BYTE [reg], imm8: 80 E0 0F -> AND AL,0x0F (mod=11 reg=100(AND) rm=000(AL))
    let mut vm = setup(&[0x80, 0xE0, 0x0F]);
    vm.registers.ax.set_low(0xAB);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x0B);
    assert!(!vm.check_flag(Carry));
    assert!(!vm.check_flag(Overflow));
}

#[test]
fn aaa_adjust() {
    // AAA (0x37)
    let mut vm = setup(&[0x37]);
    vm.registers.ax.set_low(0x0F); // nibble >= 0xA
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x05); // (0x0F + 6) & 0x0F = 0x05
    assert!(vm.check_flag(Carry));
    assert!(vm.check_flag(AuxCarry));
}

#[test]
fn aas_adjust() {
    // AAS (0x3F)
    let mut vm = setup(&[0x3F]);
    vm.registers.ax.set_low(0x0F);
    exec(&mut vm);
    // AL = (0x0F - 6) & 0x0F = 0x09
    assert_eq!(vm.registers.ax.low(), 0x09);
    assert!(vm.check_flag(Carry));
}

#[test]
fn aam_basic() {
    // AAM (0xD4 0x0A) - divide AL by 10
    let mut vm = setup(&[0xD4, 0x0A]);
    vm.registers.ax.set_low(35); // 35 / 10 = AH=3, AL=5
    exec(&mut vm);
    assert_eq!(vm.registers.ax.high(), 3);
    assert_eq!(vm.registers.ax.low(), 5);
}

#[test]
fn aad_basic() {
    // AAD (0xD5 0x0A) - AH*10 + AL -> AL, AH=0
    let mut vm = setup(&[0xD5, 0x0A]);
    vm.registers.ax.set_high(3);
    vm.registers.ax.set_low(5);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 35);
    assert_eq!(vm.registers.ax.high(), 0);
}

#[test]
fn daa_basic() {
    // DAA (0x27)
    let mut vm = setup(&[0x27]);
    vm.registers.ax.set_low(0x0A);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x10);
    assert!(vm.check_flag(AuxCarry));
}

#[test]
fn das_basic() {
    // DAS (0x2F)
    let mut vm = setup(&[0x2F]);
    vm.registers.ax.set_low(0x10);
    vm.set_flag(AuxCarry);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x0A);
}

// ========================================================================
// LOGIC
// ========================================================================

#[test]
fn and_al_imm() {
    // AND AL,0x0F (24 0F)
    let mut vm = setup(&[0x24, 0x0F]);
    vm.registers.ax.set_low(0xAB);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x0B);
    assert!(!vm.check_flag(Carry));
    assert!(!vm.check_flag(Overflow));
}

#[test]
fn and_ax_imm() {
    // AND AX,0x00FF (25 FF 00)
    let mut vm = setup(&[0x25, 0xFF, 0x00]);
    vm.registers.ax.set(0x1234);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0034);
}

#[test]
fn or_al_imm() {
    // OR AL,0xF0 (0C F0)
    let mut vm = setup(&[0x0C, 0xF0]);
    vm.registers.ax.set_low(0x0A);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0xFA);
    assert!(!vm.check_flag(Carry));
    assert!(!vm.check_flag(Overflow));
    assert!(vm.check_flag(Sign));
}

#[test]
fn or_ax_imm() {
    // OR AX,0xFF00 (0D 00 FF)
    let mut vm = setup(&[0x0D, 0x00, 0xFF]);
    vm.registers.ax.set(0x00FF);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0xFFFF);
}

#[test]
fn xor_al_imm() {
    // XOR AL,0xFF (34 FF)
    let mut vm = setup(&[0x34, 0xFF]);
    vm.registers.ax.set_low(0xAA);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x55);
}

#[test]
fn xor_ax_ax_zeroes() {
    // XOR AX,AX (31 C0 = mod=11 reg=000 rm=000 with opcode 0x31)
    let mut vm = setup(&[0x31, 0xC0]);
    vm.registers.ax.set(0x1234);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0000);
    assert!(vm.check_flag(Zero));
    assert!(vm.check_flag(Parity));
    assert!(!vm.check_flag(Carry));
    assert!(!vm.check_flag(Overflow));
}

#[test]
fn not_byte() {
    // NOT AL: F6 D0 (mod=11 reg=010 rm=000)
    let mut vm = setup(&[0xF6, 0xD0]);
    vm.registers.ax.set_low(0xAA);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x55);
}

#[test]
fn not_word() {
    // NOT AX: F7 D0 (mod=11 reg=010 rm=000)
    let mut vm = setup(&[0xF7, 0xD0]);
    vm.registers.ax.set(0xFF00);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x00FF);
}

#[test]
fn test_al_imm() {
    // TEST AL,0xFF (A8 FF) with AL=0x00 -> ZF set
    let mut vm = setup(&[0xA8, 0xFF]);
    vm.registers.ax.set_low(0x00);
    exec(&mut vm);
    assert!(vm.check_flag(Zero));
    assert!(!vm.check_flag(Carry));
    assert!(!vm.check_flag(Overflow));
}

#[test]
fn test_ax_imm() {
    // TEST AX,0x8000 (A9 00 80) with AX=0x8000 -> SF set
    let mut vm = setup(&[0xA9, 0x00, 0x80]);
    vm.registers.ax.set(0x8000);
    exec(&mut vm);
    assert!(vm.check_flag(Sign));
    assert!(!vm.check_flag(Zero));
}

#[test]
fn test_rm_reg() {
    // TEST AX,BX (85 D8 = mod=11 reg=011(BX) rm=000(AX))
    let mut vm = setup(&[0x85, 0xD8]);
    vm.registers.ax.set(0x00FF);
    vm.registers.bx.set(0xFF00);
    exec(&mut vm);
    assert!(vm.check_flag(Zero)); // no overlapping bits
}

// ========================================================================
// SHIFT / ROTATE
// ========================================================================

#[test]
fn shl_by_1() {
    // SHL AX,1: D1 E0 (mod=11 reg=100 rm=000)
    let mut vm = setup(&[0xD1, 0xE0]);
    vm.registers.ax.set(0x4000);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x8000);
    assert!(!vm.check_flag(Carry));
}

#[test]
fn shl_carry() {
    // SHL AX,1 with AX=0x8000 -> CF=1
    let mut vm = setup(&[0xD1, 0xE0]);
    vm.registers.ax.set(0x8000);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0000);
    assert!(vm.check_flag(Carry));
}

#[test]
fn shl_by_cl() {
    // SHL AX,CL: D3 E0 (mod=11 reg=100 rm=000)
    let mut vm = setup(&[0xD3, 0xE0]);
    vm.registers.ax.set(0x0001);
    vm.registers.cx.set_low(4);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0010);
}

#[test]
fn shl_by_imm() {
    // SHL AX,4: C1 E0 04 (mod=11 reg=100 rm=000, imm=4)
    let mut vm = setup(&[0xC1, 0xE0, 0x04]);
    vm.registers.ax.set(0x0001);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0010);
}

#[test]
fn shr_by_1() {
    // SHR AX,1: D1 E8 (mod=11 reg=101 rm=000)
    let mut vm = setup(&[0xD1, 0xE8]);
    vm.registers.ax.set(0x0004);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0002);
    assert!(!vm.check_flag(Carry));
}

#[test]
fn shr_carry() {
    // SHR AX,1 with AX=0x0001 -> CF=1
    let mut vm = setup(&[0xD1, 0xE8]);
    vm.registers.ax.set(0x0001);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0000);
    assert!(vm.check_flag(Carry));
}

#[test]
fn shr_by_cl() {
    // SHR AX,CL: D3 E8
    let mut vm = setup(&[0xD3, 0xE8]);
    vm.registers.ax.set(0x0100);
    vm.registers.cx.set_low(4);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0010);
}

#[test]
fn sar_by_1_negative() {
    // SAR AX,1: D1 F8 (mod=11 reg=111 rm=000)
    let mut vm = setup(&[0xD1, 0xF8]);
    vm.registers.ax.set(0x8000);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0xC000); // sign preserved
}

#[test]
fn sar_by_1_positive() {
    let mut vm = setup(&[0xD1, 0xF8]);
    vm.registers.ax.set(0x0004);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0002);
}

#[test]
fn rol_by_1() {
    // ROL AX,1: D1 C0 (mod=11 reg=000 rm=000)
    let mut vm = setup(&[0xD1, 0xC0]);
    vm.registers.ax.set(0x8001);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x0003); // MSB wraps to bit 0
    assert!(vm.check_flag(Carry)); // CF = bit 0 of result
}

#[test]
fn ror_by_1() {
    // ROR AX,1: D1 C8 (mod=11 reg=001 rm=000)
    let mut vm = setup(&[0xD1, 0xC8]);
    vm.registers.ax.set(0x0001);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0x8000); // LSB wraps to MSB
    assert!(vm.check_flag(Carry)); // CF = MSB of result
}

#[test]
fn rcl_by_1() {
    // RCL AX,1: D1 D0 (mod=11 reg=010 rm=000)
    let mut vm = setup(&[0xD1, 0xD0]);
    vm.registers.ax.set(0x8000);
    vm.set_flag(Carry);
    exec(&mut vm);
    // Old CF goes to bit 0, old MSB goes to CF
    assert_eq!(vm.registers.ax.word(), 0x0001);
    assert!(vm.check_flag(Carry)); // old MSB was 1
}

#[test]
fn rcr_by_1() {
    // RCR AX,1: D1 D8 (mod=11 reg=011 rm=000)
    let mut vm = setup(&[0xD1, 0xD8]);
    vm.registers.ax.set(0x0001);
    vm.set_flag(Carry);
    exec(&mut vm);
    // Old CF goes to MSB, old LSB goes to CF
    assert_eq!(vm.registers.ax.word(), 0x8000);
    assert!(vm.check_flag(Carry)); // old LSB was 1
}

#[test]
fn shl_byte_by_1() {
    // SHL AL,1: D0 E0 (mod=11 reg=100 rm=000)
    let mut vm = setup(&[0xD0, 0xE0]);
    vm.registers.ax.set_low(0x81);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x02);
    assert!(vm.check_flag(Carry));
}

// ========================================================================
// STRING OPERATIONS
// ========================================================================

#[test]
fn movsb_single() {
    // MOVSB (0xA4): DS:[SI] -> ES:[DI], inc SI/DI
    let mut vm = setup(&[0xA4]);
    vm.registers.si.set(0x200);
    vm.registers.di.set(0x300);
    vm.registers.ds.write_byte(0x200, 0xAB);
    exec(&mut vm);
    assert_eq!(vm.registers.es.read_byte(0x300), 0xAB);
    assert_eq!(vm.registers.si.word(), 0x201);
    assert_eq!(vm.registers.di.word(), 0x301);
}

#[test]
fn movsw_single() {
    // MOVSW (0xA5)
    let mut vm = setup(&[0xA5]);
    vm.registers.si.set(0x200);
    vm.registers.di.set(0x300);
    vm.registers.ds.write_word(0x200, 0xBEEF);
    exec(&mut vm);
    assert_eq!(vm.registers.es.read_word(0x300), 0xBEEF);
    assert_eq!(vm.registers.si.word(), 0x202);
    assert_eq!(vm.registers.di.word(), 0x302);
}

#[test]
fn rep_movsb() {
    // REP MOVSB: F3 A4
    let mut vm = setup(&[0xF3, 0xA4]);
    vm.registers.si.set(0x200);
    vm.registers.di.set(0x300);
    vm.registers.cx.set(3);
    vm.registers.ds.write_byte(0x200, 0x11);
    vm.registers.ds.write_byte(0x201, 0x22);
    vm.registers.ds.write_byte(0x202, 0x33);
    exec_n(&mut vm, 2); // REP prefix + MOVSB
    assert_eq!(vm.registers.es.read_byte(0x300), 0x11);
    assert_eq!(vm.registers.es.read_byte(0x301), 0x22);
    assert_eq!(vm.registers.es.read_byte(0x302), 0x33);
    assert_eq!(vm.registers.cx.word(), 0);
}

#[test]
fn stosb_single() {
    // STOSB (0xAA): AL -> ES:[DI]
    let mut vm = setup(&[0xAA]);
    vm.registers.ax.set_low(0x42);
    vm.registers.di.set(0x300);
    exec(&mut vm);
    assert_eq!(vm.registers.es.read_byte(0x300), 0x42);
    assert_eq!(vm.registers.di.word(), 0x301);
}

#[test]
fn stosw_single() {
    // STOSW (0xAB)
    let mut vm = setup(&[0xAB]);
    vm.registers.ax.set(0xBEEF);
    vm.registers.di.set(0x300);
    exec(&mut vm);
    assert_eq!(vm.registers.es.read_word(0x300), 0xBEEF);
    assert_eq!(vm.registers.di.word(), 0x302);
}

#[test]
fn rep_stosb() {
    // REP STOSB: F3 AA
    let mut vm = setup(&[0xF3, 0xAA]);
    vm.registers.ax.set_low(0xFF);
    vm.registers.di.set(0x300);
    vm.registers.cx.set(4);
    exec_n(&mut vm, 2);
    for i in 0..4u16 {
        assert_eq!(vm.registers.es.read_byte(0x300 + i), 0xFF);
    }
    assert_eq!(vm.registers.cx.word(), 0);
}

#[test]
fn lodsb_single() {
    // LODSB (0xAC): DS:[SI] -> AL
    let mut vm = setup(&[0xAC]);
    vm.registers.si.set(0x200);
    vm.registers.ds.write_byte(0x200, 0x77);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x77);
    assert_eq!(vm.registers.si.word(), 0x201);
}

#[test]
fn lodsw_single() {
    // LODSW (0xAD)
    let mut vm = setup(&[0xAD]);
    vm.registers.si.set(0x200);
    vm.registers.ds.write_word(0x200, 0xCAFE);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.word(), 0xCAFE);
    assert_eq!(vm.registers.si.word(), 0x202);
}

#[test]
fn scasb_equal() {
    // SCASB (0xAE): AL - ES:[DI]
    let mut vm = setup(&[0xAE]);
    vm.registers.ax.set_low(0x42);
    vm.registers.di.set(0x300);
    vm.registers.es.write_byte(0x300, 0x42);
    exec(&mut vm);
    assert!(vm.check_flag(Zero));
    assert_eq!(vm.registers.di.word(), 0x301);
}

#[test]
fn scasb_not_equal() {
    let mut vm = setup(&[0xAE]);
    vm.registers.ax.set_low(0x42);
    vm.registers.di.set(0x300);
    vm.registers.es.write_byte(0x300, 0x43);
    exec(&mut vm);
    assert!(!vm.check_flag(Zero));
}

#[test]
fn cmpsb_equal() {
    // CMPSB (0xA6): DS:[SI] - ES:[DI]
    let mut vm = setup(&[0xA6]);
    vm.registers.si.set(0x200);
    vm.registers.di.set(0x300);
    vm.registers.ds.write_byte(0x200, 0x55);
    vm.registers.es.write_byte(0x300, 0x55);
    exec(&mut vm);
    assert!(vm.check_flag(Zero));
}

#[test]
fn cmpsb_not_equal() {
    let mut vm = setup(&[0xA6]);
    vm.registers.si.set(0x200);
    vm.registers.di.set(0x300);
    vm.registers.ds.write_byte(0x200, 0x55);
    vm.registers.es.write_byte(0x300, 0x56);
    exec(&mut vm);
    assert!(!vm.check_flag(Zero));
}

#[test]
fn movsb_direction_flag() {
    // STD; MOVSB — should decrement SI/DI
    let mut vm = setup(&[0xFD, 0xA4]); // STD, MOVSB
    vm.registers.si.set(0x205);
    vm.registers.di.set(0x305);
    vm.registers.ds.write_byte(0x205, 0xEE);
    exec_n(&mut vm, 2);
    assert_eq!(vm.registers.es.read_byte(0x305), 0xEE);
    assert_eq!(vm.registers.si.word(), 0x204);
    assert_eq!(vm.registers.di.word(), 0x304);
}

#[test]
fn repe_cmpsb() {
    // REPE CMPSB (F3 A6): compare while equal
    let mut vm = setup(&[0xF3, 0xA6]);
    vm.registers.si.set(0x200);
    vm.registers.di.set(0x300);
    vm.registers.cx.set(4);
    // First 3 bytes match, 4th differs
    for i in 0..3u16 {
        vm.registers.ds.write_byte(0x200 + i, 0xAA);
        vm.registers.es.write_byte(0x300 + i, 0xAA);
    }
    vm.registers.ds.write_byte(0x203, 0xAA);
    vm.registers.es.write_byte(0x303, 0xBB);
    exec_n(&mut vm, 2);
    assert!(!vm.check_flag(Zero)); // mismatch found
    assert_eq!(vm.registers.cx.word(), 0); // all compared
}

#[test]
fn repne_scasb() {
    // REPNE SCASB (F2 AE): scan for AL in ES:[DI]
    let mut vm = setup(&[0xF2, 0xAE]);
    vm.registers.ax.set_low(0x42);
    vm.registers.di.set(0x300);
    vm.registers.cx.set(5);
    vm.registers.es.write_byte(0x300, 0x00);
    vm.registers.es.write_byte(0x301, 0x00);
    vm.registers.es.write_byte(0x302, 0x42); // match at [302]
    vm.registers.es.write_byte(0x303, 0x00);
    exec_n(&mut vm, 2);
    assert!(vm.check_flag(Zero)); // found
    assert_eq!(vm.registers.cx.word(), 2); // 5-3=2
}

// ========================================================================
// CONTROL FLOW
// ========================================================================

#[test]
fn jmp_short() {
    // JMP +2 (EB 02): skip 2 bytes, execute NOP NOP, then MOV AL,0x42
    let mut vm = setup(&[0xEB, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    exec(&mut vm); // JMP
    exec(&mut vm); // MOV AL,0x42
    assert_eq!(vm.registers.ax.low(), 0x42);
    assert_eq!(vm.registers.pc.word(), 6);
}

#[test]
fn jmp_near() {
    // JMP near +3 (E9 03 00): skip 3 bytes
    let mut vm = setup(&[0xE9, 0x03, 0x00, 0x00, 0x00, 0x00, 0xB0, 0x99]);
    exec(&mut vm); // JMP
    exec(&mut vm); // MOV AL,0x99
    assert_eq!(vm.registers.ax.low(), 0x99);
}

#[test]
fn jmp_far() {
    // JMP 0x1000:0x0000 (EA 00 00 00 10)
    let mut vm = setup(&[0xEA, 0x00, 0x00, 0x00, 0x10]);
    exec(&mut vm);
    assert_eq!(vm.registers.pc.word(), 0x0000);
    assert_eq!(vm.registers.cs.reg().word(), 0x1000);
}

#[test]
fn jz_taken() {
    // JZ +2 (74 02) with ZF=1
    let mut vm = setup(&[0x74, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.set_flag(Zero);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jz_not_taken() {
    // JZ +2 (74 02) with ZF=0 -> falls through to NOP NOP
    let mut vm = setup(&[0x74, 0x02, 0xB0, 0x99, 0xB0, 0x42]);
    exec(&mut vm); // JZ not taken
    exec(&mut vm); // MOV AL,0x99
    assert_eq!(vm.registers.ax.low(), 0x99);
}

#[test]
fn jnz_taken() {
    // JNZ +2 (75 02) with ZF=0
    let mut vm = setup(&[0x75, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jc_taken() {
    // JC +2 (72 02) with CF=1
    let mut vm = setup(&[0x72, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.set_flag(Carry);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jnc_taken() {
    // JNC +2 (73 02) with CF=0
    let mut vm = setup(&[0x73, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn js_taken() {
    // JS +2 (78 02) with SF=1
    let mut vm = setup(&[0x78, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.set_flag(Sign);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jo_taken() {
    // JO +2 (70 02) with OF=1
    let mut vm = setup(&[0x70, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.set_flag(Overflow);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jp_taken() {
    // JP +2 (7A 02) with PF=1
    let mut vm = setup(&[0x7A, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.set_flag(Parity);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jl_taken() {
    // JL +2 (7C 02): SF != OF
    let mut vm = setup(&[0x7C, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.set_flag(Sign);
    // OF=0, SF=1 -> SF!=OF -> taken
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jge_taken() {
    // JGE +2 (7D 02): SF == OF
    let mut vm = setup(&[0x7D, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    // SF=0, OF=0 -> equal -> taken
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jle_taken() {
    // JLE +2 (7E 02): ZF=1 or SF!=OF
    let mut vm = setup(&[0x7E, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.set_flag(Zero);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jg_taken() {
    // JG +2 (7F 02): ZF=0 and SF==OF
    let mut vm = setup(&[0x7F, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    // ZF=0, SF=0, OF=0 -> taken
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jbe_taken() {
    // JBE +2 (76 02): CF=1 or ZF=1
    let mut vm = setup(&[0x76, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.set_flag(Carry);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn ja_taken() {
    // JA +2 (77 02): CF=0 and ZF=0
    let mut vm = setup(&[0x77, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn loop_basic() {
    // Loop 3 times, incrementing AX each time
    // MOV CX,3; INC AX; LOOP -2
    let mut vm = setup(&[
        0xB9, 0x03, 0x00, // MOV CX,3
        0x40,             // INC AX (offset 3)
        0xE2, 0xFD,       // LOOP -3 (offset 5, disp=-3 -> target=offset 3+(-1)=3... wait)
    ]);
    // LOOP at offset 4: fetch disp at offset 5 -> PC=6, disp=0xFD=-3 -> 6+(-3)=3
    exec_n(&mut vm, 1 + 3 * 2); // MOV CX + 3 iterations of (INC + LOOP)
    assert_eq!(vm.registers.ax.word(), 3);
    assert_eq!(vm.registers.cx.word(), 0);
}

#[test]
fn loopz_basic() {
    // LOOPZ: loop while CX!=0 AND ZF=1
    // CMP AX,0; LOOPZ back
    let mut vm = setup(&[
        0xB9, 0x03, 0x00, // MOV CX,3
        0x3D, 0x00, 0x00, // CMP AX,0 -> ZF=1 (AX starts at 0) (offset 3)
        0xE1, 0xFB,       // LOOPZ -5 -> offset 8+(-5)=3 (offset 6)
    ]);
    exec_n(&mut vm, 1 + 3 * 2);
    assert_eq!(vm.registers.cx.word(), 0);
}

#[test]
fn loopnz_basic() {
    // LOOPNZ: loop while CX!=0 AND ZF=0
    let mut vm = setup(&[
        0xB9, 0x03, 0x00, // MOV CX,3
        0x3D, 0x01, 0x00, // CMP AX,1 -> ZF=0 (AX=0) (offset 3)
        0xE0, 0xFB,       // LOOPNZ -5 (offset 6)
    ]);
    exec_n(&mut vm, 1 + 3 * 2);
    assert_eq!(vm.registers.cx.word(), 0);
}

#[test]
fn jcxz_taken() {
    // JCXZ +2 (E3 02) with CX=0
    let mut vm = setup(&[0xE3, 0x02, 0x00, 0x00, 0xB0, 0x42]);
    vm.registers.cx.set(0);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x42);
}

#[test]
fn jcxz_not_taken() {
    let mut vm = setup(&[0xE3, 0x02, 0xB0, 0x99, 0xB0, 0x42]);
    vm.registers.cx.set(1);
    exec(&mut vm);
    exec(&mut vm);
    assert_eq!(vm.registers.ax.low(), 0x99);
}

#[test]
fn call_near_ret() {
    // CALL +3 (E8 03 00) -> skips 3 bytes -> MOV AL,0x42; RET
    // Then after RET, we should be at offset 3
    let mut vm = setup(&[
        0xE8, 0x03, 0x00, // CALL +3 (offset 0, pushes return addr=3)
        0xB0, 0x99,       // MOV AL,0x99 (offset 3 - return point)
        0x90,             // NOP (offset 5)
        0xB0, 0x42,       // MOV AL,0x42 (offset 6 - call target)
        0xC3,             // RET (offset 8)
    ]);
    exec(&mut vm); // CALL -> jumps to offset 6
    exec(&mut vm); // MOV AL,0x42
    assert_eq!(vm.registers.ax.low(), 0x42);
    exec(&mut vm); // RET -> returns to offset 3
    exec(&mut vm); // MOV AL,0x99
    assert_eq!(vm.registers.ax.low(), 0x99);
}

#[test]
fn call_far_retf() {
    // CALL 0x0000:0x0010 (9A 10 00 00 00)
    // At 0x0010: MOV AL,0x42; RETF (CB)
    let mut vm = setup(&[
        0x9A, 0x10, 0x00, 0x00, 0x00, // CALL 0x0000:0x0010
    ]);
    // Place code at offset 0x0010
    Memory::copy_data(&mut vm.memory, 0x0010, &[0xB0, 0x42, 0xCB]);
    exec(&mut vm); // CALL far
    assert_eq!(vm.registers.pc.word(), 0x0010);
    exec(&mut vm); // MOV AL,0x42
    assert_eq!(vm.registers.ax.low(), 0x42);
    exec(&mut vm); // RETF
    assert_eq!(vm.registers.pc.word(), 0x0005); // after the CALL instruction
}

#[test]
fn ret_imm() {
    // RET 4 (C2 04 00): pop PC and add 4 to SP
    // Place code at 0x100 so we have stack room below
    let mut vm = setup(&[]);
    Memory::copy_data(&mut vm.memory, 0x100, &[0xC2, 0x04, 0x00]);
    vm.registers.pc.set(0x100);
    vm.registers.sp.set(0x8000);
    vm.push_word(0x1234); // push return address, SP=0x7FFE
    exec(&mut vm);
    assert_eq!(vm.registers.pc.word(), 0x1234);
    assert_eq!(vm.registers.sp.word(), 0x8000 + 4); // original SP + 4 extra
}

#[test]
fn retf_imm() {
    // RETF 2 (CA 02 00): pop PC, pop CS, add 2 to SP
    let mut vm = setup(&[]);
    Memory::copy_data(&mut vm.memory, 0x100, &[0xCA, 0x02, 0x00]);
    vm.registers.pc.set(0x100);
    vm.registers.sp.set(0x8000);
    vm.push_word(0x0000); // push CS (must be 0 so fetch works after)
    vm.push_word(0x0050); // push IP
    exec(&mut vm);
    assert_eq!(vm.registers.pc.word(), 0x0050);
    assert_eq!(vm.registers.cs.reg().word(), 0x0000);
    assert_eq!(vm.registers.sp.word(), 0x8000 + 2); // original SP + 2 extra
}

#[test]
fn jmp_indirect_reg() {
    // JMP AX: FF E0 (mod=11 reg=100 rm=000)
    let mut vm = setup(&[0xFF, 0xE0]);
    vm.registers.ax.set(0x1234);
    exec(&mut vm);
    assert_eq!(vm.registers.pc.word(), 0x1234);
}

#[test]
fn call_indirect_reg() {
    // CALL AX: FF D0 (mod=11 reg=010 rm=000)
    let mut vm = setup(&[0xFF, 0xD0]);
    vm.registers.ax.set(0x1234);
    exec(&mut vm);
    assert_eq!(vm.registers.pc.word(), 0x1234);
    // Return address (0x0002) should be on stack
    let ret = vm.pop_word();
    assert_eq!(ret, 0x0002);
}

// ========================================================================
// FLAG CONTROL
// ========================================================================

#[test]
fn clc_stc() {
    let mut vm = setup(&[0xF9, 0xF8]); // STC, CLC
    exec(&mut vm); // STC
    assert!(vm.check_flag(Carry));
    exec(&mut vm); // CLC
    assert!(!vm.check_flag(Carry));
}

#[test]
fn cmc() {
    let mut vm = setup(&[0xF5, 0xF5]); // CMC, CMC
    assert!(!vm.check_flag(Carry));
    exec(&mut vm);
    assert!(vm.check_flag(Carry));
    exec(&mut vm);
    assert!(!vm.check_flag(Carry));
}

#[test]
fn cld_std() {
    let mut vm = setup(&[0xFD, 0xFC]); // STD, CLD
    exec(&mut vm);
    assert!(vm.check_flag(Directional));
    exec(&mut vm);
    assert!(!vm.check_flag(Directional));
}

#[test]
fn cli_sti() {
    let mut vm = setup(&[0xFA, 0xFB]); // CLI, STI
    vm.set_flag(Interrupt);
    exec(&mut vm); // CLI
    assert!(!vm.check_flag(Interrupt));
    exec(&mut vm); // STI
    assert!(vm.check_flag(Interrupt));
}

// ========================================================================
// MISCELLANEOUS
// ========================================================================

#[test]
fn nop() {
    let mut vm = setup(&[0x90]);
    let pc_before = vm.registers.pc.word();
    exec(&mut vm);
    assert_eq!(vm.registers.pc.word(), pc_before + 1);
}

#[test]
fn push_rm_word() {
    // PUSH AX via FF /6: FF F0 (mod=11 reg=110 rm=000)
    let mut vm = setup(&[0xFF, 0xF0]);
    vm.registers.ax.set(0xBEEF);
    let old_sp = vm.registers.sp.word();
    exec(&mut vm);
    assert_eq!(vm.registers.sp.word(), old_sp - 2);
    assert_eq!(vm.registers.ss.read_word(vm.registers.sp.word()), 0xBEEF);
}

#[test]
fn inc_rm_byte() {
    // INC BYTE [0x200]: FE 06 00 02 (mod=00 reg=000 rm=110)
    let mut vm = setup(&[0xFE, 0x06, 0x00, 0x02]);
    vm.registers.ds.write_byte(0x200, 0x41);
    exec(&mut vm);
    assert_eq!(vm.registers.ds.read_byte(0x200), 0x42);
}

#[test]
fn dec_rm_byte() {
    // DEC BYTE [0x200]: FE 0E 00 02 (mod=00 reg=001 rm=110)
    let mut vm = setup(&[0xFE, 0x0E, 0x00, 0x02]);
    vm.registers.ds.write_byte(0x200, 0x42);
    exec(&mut vm);
    assert_eq!(vm.registers.ds.read_byte(0x200), 0x41);
}

#[test]
fn inc_rm_word() {
    // INC WORD [0x200]: FF 06 00 02
    let mut vm = setup(&[0xFF, 0x06, 0x00, 0x02]);
    vm.registers.ds.write_word(0x200, 0x00FF);
    exec(&mut vm);
    assert_eq!(vm.registers.ds.read_word(0x200), 0x0100);
}

#[test]
fn dec_rm_word() {
    // DEC WORD [0x200]: FF 0E 00 02
    let mut vm = setup(&[0xFF, 0x0E, 0x00, 0x02]);
    vm.registers.ds.write_word(0x200, 0x0100);
    exec(&mut vm);
    assert_eq!(vm.registers.ds.read_word(0x200), 0x00FF);
}
