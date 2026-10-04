//! Regression tests for CPU behaviour that the single-instruction hardware
//! vectors cannot reach: host-side arithmetic overflow, wrap-around at segment
//! and memory boundaries, ROM protection, prefix combinations, and interrupt
//! and single-step sequencing.

use crate::io::bus::IoDevice;
use crate::io::pic::PicDevice;
use crate::vm::instructions::process;
use crate::vm::runtime::{CpuFlag, Runtime};

const CODE: usize = 0x10100; // 1000:0100

fn new_rt(code: &[u8]) -> Runtime {
    let mut rt = Runtime::new_test();
    rt.cpu.registers.cs.reg_mut().set(0x1000);
    rt.cpu.registers.pc.set(0x0100);
    rt.cpu.registers.ss.reg_mut().set(0x2000);
    rt.cpu.registers.sp.set(0x1000);
    rt.cpu.registers.ds.reg_mut().set(0x3000);
    rt.cpu.registers.es.reg_mut().set(0x4000);
    rt.cpu.flags = 0xF002;
    poke(&mut rt, CODE, code);
    rt
}

fn poke(rt: &mut Runtime, addr: usize, bytes: &[u8]) {
    for (i, b) in bytes.iter().enumerate() {
        rt.cpu.memory.write_byte(addr + i, *b);
    }
}

fn peek(rt: &Runtime, addr: usize) -> u8 {
    rt.cpu.memory.read_byte(addr)
}

fn set_ivt(rt: &mut Runtime, vector: u8, seg: u16, off: u16) {
    rt.cpu.memory.write_word(vector as usize * 4, off);
    rt.cpu.memory.write_word(vector as usize * 4 + 2, seg);
}

fn init_pic(rt: &mut Runtime, imr: u8) {
    let mut dev = PicDevice;
    dev.port_out_byte(0x20, 0x13, &mut rt.cpu);
    dev.port_out_byte(0x21, 0x08, &mut rt.cpu);
    dev.port_out_byte(0x21, 0x01, &mut rt.cpu);
    dev.port_out_byte(0x21, imr, &mut rt.cpu);
}

fn cs_ip(rt: &Runtime) -> (u16, u16) {
    (rt.cpu.registers.cs.reg().word(), rt.cpu.registers.pc.word())
}

// ───── IDIV of the most negative dividend by -1 raises INT 0 ─────

#[test]
fn idiv_word_min_by_minus_one_raises_int0() {
    let mut rt = new_rt(&[0xF7, 0xF9]); // IDIV CX
    set_ivt(&mut rt, 0, 0x5000, 0x0010);
    rt.cpu.registers.dx.set(0x8000);
    rt.cpu.registers.ax.set(0x0000);
    rt.cpu.registers.cx.set(0xFFFF);
    process(&mut rt);
    assert_eq!(cs_ip(&rt), (0x5000, 0x0010), "divide error must vector through INT 0");
    assert_eq!(rt.cpu.registers.sp.word(), 0x0FFA, "FLAGS, CS, IP pushed");
    assert_eq!(rt.cpu.memory.read_word(0x20FFA), 0x0102, "8086 pushes the address of the next instruction");
}

#[test]
fn idiv_byte_min_by_minus_one_raises_int0() {
    let mut rt = new_rt(&[0xF6, 0xF9]); // IDIV CL
    set_ivt(&mut rt, 0, 0x5000, 0x0010);
    rt.cpu.registers.ax.set(0x8000);
    rt.cpu.registers.cx.set(0x00FF);
    process(&mut rt);
    assert_eq!(cs_ip(&rt), (0x5000, 0x0010));
}

#[test]
fn rep_idiv_boundary_operands_never_panic() {
    let specials: [u16; 10] = [0, 1, 2, 0x7F, 0x80, 0xFF, 0x7FFF, 0x8000, 0xFFFE, 0xFFFF];
    for &opcode in &[0xF6u8, 0xF7] {
        for rep in [None, Some(0xF2u8), Some(0xF3)] {
            for &ax in &specials {
                for &dx in &specials {
                    for &div in &specials {
                        let mut code = Vec::new();
                        if let Some(p) = rep {
                            code.push(p);
                        }
                        code.extend_from_slice(&[opcode, 0xFB]); // IDIV BX / IDIV BL
                        let mut rt = new_rt(&code);
                        rt.cpu.registers.ax.set(ax);
                        rt.cpu.registers.dx.set(dx);
                        rt.cpu.registers.bx.set(div);
                        for _ in 0..code.len() - 1 {
                            process(&mut rt); // must not panic
                        }
                    }
                }
            }
        }
    }
}

// ───── far CALL/JMP through a pointer that wraps ─────

#[test]
fn jmp_far_pointer_at_fffd_wraps_inside_segment() {
    let mut rt = new_rt(&[0xFF, 0x2E, 0xFD, 0xFF]); // JMP FAR [FFFD]
    poke(&mut rt, 0x3FFFD, &[0x34, 0x12, 0x78]);
    poke(&mut rt, 0x30000, &[0x56]);
    poke(&mut rt, 0x40000, &[0xAB]);
    process(&mut rt);
    assert_eq!(cs_ip(&rt), (0x5678, 0x1234));
}

#[test]
fn jmp_far_pointer_at_ffff_wraps_inside_segment() {
    let mut rt = new_rt(&[0xFF, 0x2E, 0xFF, 0xFF]); // JMP FAR [FFFF]
    poke(&mut rt, 0x3FFFF, &[0x34]);
    poke(&mut rt, 0x30000, &[0x12, 0x78, 0x56]);
    process(&mut rt);
    assert_eq!(cs_ip(&rt), (0x5678, 0x1234));
}

#[test]
fn jmp_far_pointer_at_physical_ffffe_wraps_to_zero() {
    let mut rt = new_rt(&[0xFF, 0x2E, 0x0E, 0x00]); // JMP FAR [000E], DS=FFFF
    rt.cpu.registers.ds.reg_mut().set(0xFFFF);
    poke(&mut rt, 0xFFFFE, &[0x34, 0x12]);
    poke(&mut rt, 0x00000, &[0x78, 0x56]);
    process(&mut rt);
    assert_eq!(cs_ip(&rt), (0x5678, 0x1234));
}

#[test]
fn call_far_pointer_at_fffd() {
    let mut rt = new_rt(&[0xFF, 0x1E, 0xFD, 0xFF]); // CALL FAR [FFFD]
    poke(&mut rt, 0x3FFFD, &[0x34, 0x12, 0x78]);
    poke(&mut rt, 0x30000, &[0x56]);
    process(&mut rt);
    assert_eq!(cs_ip(&rt), (0x5678, 0x1234));
    assert_eq!(rt.cpu.memory.read_word(0x20FFE), 0x1000, "old CS pushed first");
    assert_eq!(rt.cpu.memory.read_word(0x20FFC), 0x0104, "then the return IP");
}

// ───── word operands at offset FFFFh are written back ─────

#[test]
fn word_writes_through_modrm_at_offset_ffff() {
    let cases: Vec<(&str, Vec<u8>, u16)> = vec![
        ("MOV [FFFF],AX", vec![0x89, 0x06, 0xFF, 0xFF], 0x1234),
        ("MOV W[FFFF],5678", vec![0xC7, 0x06, 0xFF, 0xFF, 0x78, 0x56], 0x5678),
        ("INC W[FFFF]", vec![0xFF, 0x06, 0xFF, 0xFF], 0xAAAB),
        ("ADD [FFFF],AX", vec![0x01, 0x06, 0xFF, 0xFF], 0xBCDE),
        ("XCHG [FFFF],AX", vec![0x87, 0x06, 0xFF, 0xFF], 0x1234),
        ("POP W[FFFF]", vec![0x8F, 0x06, 0xFF, 0xFF], 0xBEEF),
        ("MOV [FFFF],DS", vec![0x8C, 0x1E, 0xFF, 0xFF], 0x3000),
        ("SHL W[FFFF],1", vec![0xD1, 0x26, 0xFF, 0xFF], 0x5554),
        ("NOT W[FFFF]", vec![0xF7, 0x16, 0xFF, 0xFF], 0x5555),
    ];
    let mut failures = Vec::new();
    for (label, code, expect) in cases {
        let mut rt = new_rt(&code);
        rt.cpu.registers.ax.set(0x1234);
        poke(&mut rt, 0x3FFFF, &[0xAA]);
        poke(&mut rt, 0x30000, &[0xAA]);
        poke(&mut rt, 0x21000, &[0xEF, 0xBE]);
        process(&mut rt);
        let got = u16::from_le_bytes([peek(&rt, 0x3FFFF), peek(&rt, 0x30000)]);
        if got != expect {
            failures.push(format!("{}: expected {:04X}, memory holds {:04X}", label, expect, got));
        }
    }
    assert!(failures.is_empty(), "word writes lost at DS:FFFF: {:#?}", failures);
}

#[test]
fn xchg_at_offset_ffff_also_loads_the_register() {
    let mut rt = new_rt(&[0x87, 0x06, 0xFF, 0xFF]);
    rt.cpu.registers.ax.set(0x1234);
    poke(&mut rt, 0x3FFFF, &[0x78]);
    poke(&mut rt, 0x30000, &[0x56]);
    process(&mut rt);
    assert_eq!(rt.cpu.registers.ax.word(), 0x5678);
    assert_eq!((peek(&rt, 0x3FFFF), peek(&rt, 0x30000)), (0x34, 0x12));
}

#[test]
fn word_write_through_modrm_at_physical_fffff() {
    let mut rt = new_rt(&[0x89, 0x06, 0x0F, 0x00]); // MOV [000F],AX with DS=FFFF
    rt.cpu.registers.ds.reg_mut().set(0xFFFF);
    rt.cpu.registers.ax.set(0x1234);
    process(&mut rt);
    assert_eq!((peek(&rt, 0xFFFFF), peek(&rt, 0x00000)), (0x34, 0x12));
}

// ───── trap flag / single-step ─────

#[test]
fn trap_flag_single_steps() {
    // MOV AX,0102h ; PUSH AX ; POPF ; INC CX ; INC CX ; INC CX ; HLT
    let mut rt = new_rt(&[0xB8, 0x02, 0x01, 0x50, 0x9D, 0x41, 0x41, 0x41, 0xF4]);
    set_ivt(&mut rt, 1, 0x0000, 0x0600);
    poke(&mut rt, 0x00600, &[0xBB, 0xEF, 0xBE, 0xF4]); // MOV BX,BEEF ; HLT
    rt.step_frame();
    assert_eq!(rt.cpu.registers.bx.word(), 0xBEEF, "INT 1 handler must run");
    assert_eq!(rt.cpu.registers.cx.word(), 1, "exactly one instruction runs after POPF sets TF");
    assert!(!rt.cpu.check_flag(CpuFlag::Trap), "TF is cleared on entry to the handler");
    let sp = rt.cpu.registers.sp.word() as usize;
    assert_eq!(rt.cpu.memory.read_word(0x20000 + sp), 0x0106, "return IP = instruction after the first INC");
    assert_eq!(rt.cpu.memory.read_word(0x20000 + sp + 4) & 0x0100, 0x0100, "stacked FLAGS keep TF set");
}

#[test]
fn trap_handler_iret_keeps_stepping() {
    // MOV AX,0102h ; PUSH AX ; POPF          (TF on)
    // INC CX ; INC CX ; INC CX               (3 traps)
    // MOV AX,0002h ; PUSH AX ; POPF          (3 traps: the POPF that clears TF still traps)
    // HLT                                    (TF off again: no trap)
    // INT 1 handler: INC BX ; IRET
    let mut rt = new_rt(&[
        0xB8, 0x02, 0x01, 0x50, 0x9D, 0x41, 0x41, 0x41, 0xB8, 0x02, 0x00, 0x50, 0x9D, 0xF4,
    ]);
    set_ivt(&mut rt, 1, 0x0000, 0x0600);
    poke(&mut rt, 0x00600, &[0x43, 0xCF]); // INC BX ; IRET
    rt.cpu.registers.bx.set(0);
    rt.step_frame();
    assert!(rt.cpu.halted, "program should reach its HLT");
    assert_eq!(cs_ip(&rt), (0x1000, 0x010E));
    assert_eq!(rt.cpu.registers.cx.word(), 3);
    assert_eq!(rt.cpu.registers.bx.word(), 6, "one INT 1 per instruction that started with TF=1");
}

#[test]
fn trap_and_irq_at_the_same_boundary() {
    // TF=1, IF=0, IRQ3 pending.  STI ; NOP ; NOP ; HLT
    // After the first NOP both the IRQ and the single-step trap are due: the IRQ is
    // acknowledged first, then the trap fires on entry to the IRQ handler.
    let mut rt = new_rt(&[0xFB, 0x90, 0x90, 0xF4]);
    rt.cpu.flags = 0xF002 | 0x0100;
    init_pic(&mut rt, 0x01);
    set_ivt(&mut rt, 0x0B, 0x0000, 0x0500); // IRQ3
    set_ivt(&mut rt, 0x01, 0x0000, 0x0600); // single-step
    poke(&mut rt, 0x00500, &[0xF4]);
    poke(&mut rt, 0x00600, &[0xF4]); // HLT so the stack can be inspected
    rt.cpu.pic.request_interrupt(3);
    rt.step_frame();
    assert_eq!(cs_ip(&rt), (0x0000, 0x0601), "stopped in the INT 1 handler");
    assert_eq!(rt.cpu.registers.sp.word(), 0x0FF4, "two interrupt frames");
    // inner frame (pushed by the trap): return to the IRQ handler's first instruction, TF/IF clear
    assert_eq!(rt.cpu.memory.read_word(0x20FF4), 0x0500);
    assert_eq!(rt.cpu.memory.read_word(0x20FF6), 0x0000);
    assert_eq!(rt.cpu.memory.read_word(0x20FF8) & 0x0300, 0x0000);
    // outer frame (pushed by the IRQ): return after the NOP, TF and IF still set
    assert_eq!(rt.cpu.memory.read_word(0x20FFA), 0x0102);
    assert_eq!(rt.cpu.memory.read_word(0x20FFC), 0x1000);
    assert_eq!(rt.cpu.memory.read_word(0x20FFE) & 0x0300, 0x0300);
}

// ───── interrupts after a prefixed instruction ─────

/// IF=0, IRQ3 pending, handler = HLT; returns the stacked return IP if the IRQ was taken.
fn irq_return_ip(code: &[u8]) -> Option<u16> {
    let mut rt = new_rt(code);
    init_pic(&mut rt, 0x01);
    set_ivt(&mut rt, 0x0B, 0x0000, 0x0500);
    poke(&mut rt, 0x00500, &[0xF4]);
    rt.cpu.pic.request_interrupt(3);
    rt.step_frame();
    if cs_ip(&rt) == (0x0000, 0x0501) {
        let sp = rt.cpu.registers.sp.word();
        Some(rt.cpu.registers.ss.read_word(sp))
    } else {
        None
    }
}

#[test]
fn irq_is_recognised_right_after_a_prefixed_instruction() {
    // STI ; MOV ES:[BX],AL ; NOP ; NOP ; HLT
    assert_eq!(irq_return_ip(&[0xFB, 0x26, 0x88, 0x07, 0x90, 0x90, 0xF4]), Some(0x0104));
    // STI ; LOCK MOV [BX],AL ; NOP ; NOP ; HLT
    assert_eq!(irq_return_ip(&[0xFB, 0xF0, 0x88, 0x07, 0x90, 0x90, 0xF4]), Some(0x0104));
    // STI ; ES: MOVSB ; NOP ; NOP ; HLT
    assert_eq!(irq_return_ip(&[0xFB, 0x26, 0xA4, 0x90, 0x90, 0xF4]), Some(0x0103));
}

#[test]
fn sti_window_around_a_prefixed_instruction_lets_the_irq_in() {
    // STI ; MOV ES:[BX],AL ; CLI ; HLT
    assert_eq!(irq_return_ip(&[0xFB, 0x26, 0x88, 0x07, 0xFA, 0xF4]), Some(0x0104));
    // STI ; MOV AL,CS:[0200] ; CLI ; HLT
    assert_eq!(irq_return_ip(&[0xFB, 0x2E, 0xA0, 0x00, 0x02, 0xFA, 0xF4]), Some(0x0105));
}

#[test]
fn no_irq_between_prefix_and_opcode_and_inhibits_still_work() {
    // STI ; NOP ; ...            -> shadow covers the NOP
    assert_eq!(irq_return_ip(&[0xFB, 0x90, 0x90, 0x90, 0xF4]), Some(0x0102));
    // STI ; ES: REP NOP ; ...    -> never between prefixes and opcode
    assert_eq!(irq_return_ip(&[0xFB, 0x26, 0xF3, 0x90, 0x90, 0xF4]), Some(0x0104));
}

#[test]
fn prefixed_hlt_wakes_on_irq() {
    for code in [vec![0xFBu8, 0xF3, 0xF4, 0x90, 0xF4], vec![0xFB, 0x2E, 0xF4, 0x90, 0xF4], vec![0xFB, 0xF0, 0xF4, 0x90, 0xF4]] {
        let mut rt = new_rt(&code);
        init_pic(&mut rt, 0x01);
        set_ivt(&mut rt, 0x0B, 0x0000, 0x0500);
        poke(&mut rt, 0x00500, &[0xBB, 0xEF, 0xBE, 0xCF]); // MOV BX,BEEF ; IRET
        rt.step_frame();
        assert!(rt.cpu.halted);
        rt.cpu.pic.request_interrupt(3);
        rt.step_frame();
        assert_eq!(rt.cpu.registers.bx.word(), 0xBEEF, "IRQ must wake a prefixed HLT ({:02X?})", code);
    }
}

// ───── REP survives later prefixes ─────

#[test]
fn rep_survives_any_prefix_order() {
    // (code, expected source segment marker)
    let cases: [(&[u8], u8); 9] = [
        (&[0xF3, 0xA4], 0xD0),
        (&[0x26, 0xF3, 0xA4], 0xE0),
        (&[0xF3, 0x26, 0xA4], 0xE0),
        (&[0xF0, 0xF3, 0xA4], 0xD0),
        (&[0xF3, 0xF0, 0xA4], 0xD0),
        (&[0xF3, 0xF1, 0xA4], 0xD0),
        (&[0x26, 0xF3, 0xF0, 0xA4], 0xE0),
        (&[0xF3, 0x26, 0x2E, 0xA4], 0xC0),
        (&[0x2E, 0xF3, 0x26, 0xA4], 0xE0),
    ];
    let mut failures = Vec::new();
    for (code, marker) in cases {
        let mut rt = new_rt(code);
        for i in 0..5usize {
            poke(&mut rt, 0x30200 + i, &[0xD0 + i as u8]);
            poke(&mut rt, 0x40200 + i, &[0xE0 + i as u8]);
            poke(&mut rt, 0x10200 + i, &[0xC0 + i as u8]);
        }
        rt.cpu.registers.cx.set(5);
        rt.cpu.registers.si.set(0x0200);
        rt.cpu.registers.di.set(0x0400);
        for _ in 0..code.len() {
            process(&mut rt);
        }
        let dest: Vec<u8> = (0..5usize).map(|i| peek(&rt, 0x40400 + i)).collect();
        let want: Vec<u8> = (0..5u8).map(|i| marker + i).collect();
        if dest != want || rt.cpu.registers.cx.word() != 0 {
            failures.push(format!("{:02X?}: dest={:02X?} CX={} (want {:02X?}, CX=0)", code, dest, rt.cpu.registers.cx.word(), want));
        }
    }
    assert!(failures.is_empty(), "REP dropped: {:#?}", failures);
}

#[test]
fn override_does_not_leak_into_the_next_instruction() {
    // ES: NOP ; MOV AL,[BX]   -> the MOV reads DS
    let mut rt = new_rt(&[0x26, 0x90, 0x8A, 0x07]);
    rt.cpu.registers.bx.set(0x0010);
    poke(&mut rt, 0x30010, &[0xDD]);
    poke(&mut rt, 0x40010, &[0xEE]);
    for _ in 0..3 {
        process(&mut rt);
    }
    assert_eq!(rt.cpu.registers.ax.low(), 0xDD);
    // REP NOP ; MOVSB   -> a single element, CX untouched
    let mut rt = new_rt(&[0xF3, 0x90, 0xA4]);
    rt.cpu.registers.cx.set(5);
    for _ in 0..3 {
        process(&mut rt);
    }
    assert_eq!(rt.cpu.registers.cx.word(), 5);
    assert_eq!(rt.cpu.registers.si.word(), 1);
}

#[test]
fn interrupted_rep_resumes_with_all_prefixes() {
    // Handler: INC BX ; MOV AL,20h ; OUT 20h,AL ; IRET
    let handler = [0x43u8, 0xB0, 0x20, 0xE6, 0x20, 0xCF];
    for code in [vec![0xF3u8, 0xA4, 0xF4], vec![0x26, 0xF3, 0xA4, 0xF4], vec![0xF3, 0x26, 0xA4, 0xF4], vec![0xF3, 0xF0, 0x26, 0xA4, 0xF4]] {
        let from_es = code.contains(&0x26);
        let mut rt = new_rt(&code);
        init_pic(&mut rt, 0x01);
        set_ivt(&mut rt, 0x0B, 0x0000, 0x0500);
        poke(&mut rt, 0x00500, &handler);
        for i in 0..8usize {
            poke(&mut rt, 0x30200 + i, &[0xD0 + i as u8]);
            poke(&mut rt, 0x40200 + i, &[0x60 + i as u8]);
        }
        rt.cpu.set_flag(CpuFlag::Interrupt);
        rt.cpu.registers.cx.set(8);
        rt.cpu.registers.si.set(0x0200);
        rt.cpu.registers.di.set(0x0400);
        rt.cpu.registers.bx.set(0);
        rt.cpu.pic.request_interrupt(3);
        rt.step_frame();
        let dest: Vec<u8> = (0..8usize).map(|i| peek(&rt, 0x40400 + i)).collect();
        let base = if from_es { 0x60 } else { 0xD0 };
        let want: Vec<u8> = (0..8u8).map(|i| base + i).collect();
        assert_eq!(rt.cpu.registers.bx.word(), 1, "handler ran once ({:02X?})", code);
        assert_eq!(rt.cpu.registers.cx.word(), 0, "REP completed after the IRQ ({:02X?})", code);
        assert_eq!(dest, want, "{:02X?}", code);
    }
}

// ───── REP in front of unsigned DIV has no effect ─────

#[test]
fn rep_div_is_plain_div() {
    for (code, steps) in [(vec![0xF3u8, 0xF7, 0xF3], 2), (vec![0xF2, 0xF7, 0xF3], 2), (vec![0xF3, 0xF6, 0xF3], 2)] {
        let mut rt = new_rt(&code);
        set_ivt(&mut rt, 0, 0x5000, 0x0010);
        rt.cpu.registers.dx.set(0);
        rt.cpu.registers.ax.set(100);
        rt.cpu.registers.bx.set(7);
        for _ in 0..steps {
            process(&mut rt);
        }
        assert_eq!(cs_ip(&rt), (0x1000, 0x0103), "no divide error ({:02X?})", code);
        if code[1] == 0xF7 {
            assert_eq!((rt.cpu.registers.ax.word(), rt.cpu.registers.dx.word()), (14, 2));
        } else {
            assert_eq!((rt.cpu.registers.ax.low(), rt.cpu.registers.ax.high()), (14, 2));
        }
    }
}

#[test]
fn rep_still_negates_mul_imul_idiv() {
    // REP MUL BX / REP IMUL BX : 100 * 7 -> -700 ; REP IDIV BX : 100 / 7 -> -14 rem 2
    for (code, ax, dx) in [(vec![0xF3u8, 0xF7, 0xE3], 0xFD44u16, 0xFFFFu16), (vec![0xF3, 0xF7, 0xEB], 0xFD44, 0xFFFF), (vec![0xF3, 0xF7, 0xFB], 0xFFF2, 0x0002)] {
        let mut rt = new_rt(&code);
        rt.cpu.registers.dx.set(0);
        rt.cpu.registers.ax.set(100);
        rt.cpu.registers.bx.set(7);
        process(&mut rt);
        process(&mut rt);
        assert_eq!((rt.cpu.registers.ax.word(), rt.cpu.registers.dx.word()), (ax, dx), "{:02X?}", code);
    }
}

// ───── ROM cannot be modified through any kind of operand ─────

/// A machine whose top 64 KiB are write-protected ROM filled with 0x5A.
fn new_rt_with_rom(code: &[u8]) -> Runtime {
    let mut rt = new_rt(code);
    for addr in 0xF0000..0x100000 {
        rt.cpu.memory.write_byte_unchecked(addr, 0x5A);
    }
    rt.cpu.memory.enable_rom_protection();
    rt.cpu.registers.ds.reg_mut().set(0xF000);
    rt
}

#[test]
fn stores_through_modrm_operands_do_not_modify_rom() {
    let cases: Vec<(&str, Vec<u8>)> = vec![
        ("MOV B[0100],55", vec![0xC6, 0x06, 0x00, 0x01, 0x55]),
        ("MOV W[0100],1234", vec![0xC7, 0x06, 0x00, 0x01, 0x34, 0x12]),
        ("MOV [0100],AX", vec![0x89, 0x06, 0x00, 0x01]),
        ("INC B[0100]", vec![0xFE, 0x06, 0x00, 0x01]),
        ("ADD [0100],AX", vec![0x01, 0x06, 0x00, 0x01]),
        ("NOT W[0100]", vec![0xF7, 0x16, 0x00, 0x01]),
        ("POP W[0100]", vec![0x8F, 0x06, 0x00, 0x01]),
        ("MOV [0100],AL (moffs)", vec![0xA2, 0x00, 0x01]),
    ];
    for (label, code) in cases {
        let mut rt = new_rt_with_rom(&code);
        rt.cpu.registers.ax.set(0x1234);
        process(&mut rt);
        assert_eq!(rt.cpu.memory.read_word(0xF0100), 0x5A5A, "{} changed ROM", label);
    }
}

#[test]
fn xchg_with_rom_loads_the_register_and_leaves_rom_alone() {
    let mut rt = new_rt_with_rom(&[0x87, 0x06, 0x00, 0x01]); // XCHG [0100],AX
    rt.cpu.registers.ax.set(0x1234);
    process(&mut rt);
    assert_eq!(rt.cpu.registers.ax.word(), 0x5A5A);
    assert_eq!(rt.cpu.memory.read_word(0xF0100), 0x5A5A);
}

#[test]
fn read_modify_instructions_see_rom_contents() {
    let mut rt = new_rt_with_rom(&[0x03, 0x06, 0x00, 0x01]); // ADD AX,[0100]
    rt.cpu.registers.ax.set(0x0001);
    process(&mut rt);
    assert_eq!(rt.cpu.registers.ax.word(), 0x5A5B);
}

#[test]
fn far_jump_through_a_pointer_stored_in_rom() {
    let mut rt = new_rt(&[0xFF, 0x2E, 0x00, 0x01]); // JMP FAR [0100], DS=F000
    rt.cpu.memory.write_byte_unchecked(0xF0100, 0x34);
    rt.cpu.memory.write_byte_unchecked(0xF0101, 0x12);
    rt.cpu.memory.write_byte_unchecked(0xF0102, 0x78);
    rt.cpu.memory.write_byte_unchecked(0xF0103, 0x56);
    rt.cpu.memory.enable_rom_protection();
    rt.cpu.registers.ds.reg_mut().set(0xF000);
    process(&mut rt);
    assert_eq!(cs_ip(&rt), (0x5678, 0x1234));
}
