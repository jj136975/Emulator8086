pub fn disas(opcode: u8) -> bool {
    match opcode {
        // ADD ac,data
        0b_0000_0100 | 0b_0000_0101 => {
            println!("{:02X} ADD ac,data", opcode);
            true
        }
        // ADD MOD_R/M
        0b_0000_0000..=0b_0000_0011 => {
            println!("{:02X} ADD MOD_R/M", opcode);
            true
        }
        // AAA
        0x37 => {
            println!("{:02X} AAA", opcode);
            true
        }
        // AAD
        0xD5 => {
            println!("{:02X} AAD", opcode);
            true
        }
        // AAM
        0xD4 => {
            println!("{:02X} AAM", opcode);
            true
        }
        // AAS
        0x3F => {
            println!("{:02X} AAS", opcode);
            true
        }
        // ADC ac,data
        0b_0001_0100 | 0b_0001_0101 => {
            println!("{:02X} ADC ac,data", opcode);
            true
        }
        // ADC Mod R/M
        0b_0001_0000..=0b_0001_0011 => {
            println!("{:02X} ADC Mod R/M", opcode);
            true
        }
        // AND ac,data
        0b_0010_0100 | 0b_0010_0101 => {
            println!("{:02X} AND ac,data", opcode);
            true
        }
        // AND Mod R/M
        0b_0010_0000..=0b_0010_0011 => {
            println!("{:02X} AND Mod R/M", opcode);
            true
        }
        // CALL addr
        0x9A => {
            println!("{:02X} CALL addr", opcode);
            true
        }
        // CALL | JMP disp16
        0xE8 | 0xE9 => {
            println!("{:02X} CALL | JMP disp16", opcode);
            true
        }
        // CBW
        0x98 => {
            println!("{:02X} CBW", opcode);
            true
        }
        // CLC / STC
        0xF8 | 0xF9 => {
            println!("{:02X} CLC / STC", opcode);
            true
        }
        // CLD / STD
        0xFC => {
            println!("{:02X} CLD / STD", opcode);
            true
        }
        // CLI / STI
        0xFA => {
            println!("{:02X} CLI / STI", opcode);
            true
        }
        // CMC
        0xF5 => {
            println!("{:02X} CMC", opcode);
            true
        }
        // CMP
        0b_0011_1100 | 0b_0011_1101 => {
            println!("{:02X} CMP", opcode);
            true
        }
        // CMP
        0b_0011_1000..=0b_0011_1011 => {
            println!("{:02X} CMP", opcode);
            true
        }
        // CMPS
        0b_1010_0110 | 0b_1010_0111 => {
            println!("{:02X} CMPS", opcode);
            true
        }
        // CWD
        0x99 => {
            println!("{:02X} CWD", opcode);
            true
        }
        // DAA
        0x27 => {
            println!("{:02X} DAA", opcode);
            true
        }
        // DAS
        0x2F => {
            println!("{:02X} DAS", opcode);
            true
        }
        // (GRP) DEC, INC
        0b_1111_1110 | 0b_1111_1111 => {
            println!("{:02X} (GRP) DEC, INC", opcode);
            true
        }
        // DEC
        0b_0100_1000..=0b_0100_1111 => {
            println!("{:02X} DEC", opcode);
            true
        }
        // (GRP) DIV, IDIV, AND, TEST
        0b_1111_0110 | 0b_1111_0111 => {
            println!("{:02X} (GRP) DIV, IDIV, AND, TEST", opcode);
            true
        }
        // ESC
        0b_1101_1000..=0b_1101_1111 => {
            println!("{:02X} ESC", opcode);
            true
        }
        // HLT
        0xF4 => {
            println!("{:02X} HLT", opcode);
            true
        }
        // IN ac,DX
        0b_11101100 | 0b_11101101 => {
            println!("{:02X} IN ac,DX", opcode);
            true
        }
        // IN ac,port
        0b_11100100 | 0b_11100101 => {
            println!("{:02X} IN ac,port", opcode);
            true
        }
        // INC
        0b_0100_0000..=0b_0100_0111 => {
            println!("{:02X} INC", opcode);
            true
        }
        // INT
        0b_1100_1100 | 0b_1100_1101 => {
            println!("{:02X} INT", opcode);
            true
        }
        // INT0
        0xCE => {
            println!("{:02X} INT0", opcode);
            true
        }
        // IRET
        0xCF => {
            println!("{:02X} IRET", opcode);
            true
        }
        // JMP CONDITIONAL disp
        0x70..=0x7F => {
            println!("{:02X} JMP CONDITIONAL disp", opcode);
            true
        }
        // JCXZ disp
        0xE3 => {
            println!("{:02X} JCXZ disp", opcode);
            true
        }
        // JMP addr
        0xEA => {
            println!("{:02X} JMP addr", opcode);
            true
        }
        // JMP disp
        0xEB => {
            println!("{:02X} JMP disp", opcode);
            true
        }
        // LAHF
        0x9F => {
            println!("{:02X} LAHF", opcode);
            true
        }
        // LDS
        0xC5 => {
            println!("{:02X} LDS", opcode);
            true
        }
        // LEA
        0x8D => {
            println!("{:02X} LEA", opcode);
            true
        }
        // LES
        0xC4 => {
            println!("{:02X} LES", opcode);
            true
        }
        // LOCK
        0xF0 => {
            println!("{:02X} LOCK", opcode);
            true
        }
        // LODS
        0b_1010_1100 | 0b_1010_1101 => {
            println!("{:02X} LODS", opcode);
            true
        }
        // LOOP
        0xE2 => {
            println!("{:02X} LOOP", opcode);
            true
        }
        // LOOPZ disp, LOOPE disp
        0xE1 => {
            println!("{:02X} LOOPZ disp, LOOPE disp", opcode);
            true
        }
        // LOOPNZ disp, LOOPNE disp
        0xE0 => {
            println!("{:02X} LOOPNZ disp, LOOPNE disp", opcode);
            true
        }
        // MOV Mod R/M
        0b_1000_1000..=0b_1000_1011 => {
            println!("{:02X} MOV Mod R/M", opcode);
            true
        }
        // MOV data,reg
        0b_1011_0000..=0b_1011_1111 => {
            println!("{:02X} MOV data,reg", opcode);
            true
        }
        // MOV ac,mem
        0b_1010_0000 | 0b_1010_0001 => {
            println!("{:02X} MOV ac,mem", opcode);
            true
        }
        // MOV mem,ac
        0b_1010_0010 | 0b_1010_0011 => {
            println!("{:02X} MOV mem,ac", opcode);
            true
        }
        // MOV segreg,mem/reg
        0x8E => {
            println!("{:02X} MOV segreg,mem/reg", opcode);
            true
        }
        // MOV mem/reg,segreg
        0x8C => {
            println!("{:02X} MOV mem/reg,segreg", opcode);
            true
        }
        // MOV mem/reg,data
        0b_1100_0110 | 0b_1100_0111 => {
            println!("{:02X} MOV mem/reg,datas", opcode);
            true
        }
        // MOVS
        0b_1010_0100 | 0b_1010_0101 => {
            println!("{:02X} MOVS", opcode);
            true
        }
        // NOP
        0x90 => {
            println!("{:02X} NOP", opcode);
            true
        }
        // OR ac,data
        0b_0000_1100 | 0b_0000_1101 => {
            println!("{:02X} OR ac,data", opcode);
            true
        }
        // OR Mod/RM
        0b_0000_1000..=0b_0000_1011 => {
            println!("{:02X} OR Mod/RM", opcode);
            true
        }
        // OUT
        0b_1110_1110 | 0b_1110_1111 => {
            println!("{:02X} OUT", opcode);
            true
        }
        // OUT
        0b_1110_0110 | 0b_1110_0111 => {
            println!("{:02X} OUT", opcode);
            true
        }
        // POP mem/reg
        0x8F => {
            println!("{:02X} POP mem/reg", opcode);
            true
        }
        // POP reg
        0b_0101_1000..=0b_0101_1111 => {
            println!("{:02X} POP reg", opcode);
            true
        }
        // POP sreg
        0b_0000_0111 | 0b_0001_0111 | 0b_0001_1111 => {
            println!("{:02X} POP sreg", opcode);
            true
        }
        // POPF
        0x9D => {
            println!("{:02X} POPF", opcode);
            true
        }
        // PUSH reg
        0b_0101_0000..=0b_0101_0111 => {
            println!("{:02X} PUSH reg", opcode);
            true
        }
        // PUSH sreg
        0b_0000_0110 | 0b_0000_1110 | 0b_0001_0110 | 0b_0001_1110 => {
            println!("{:02X} PUSH sreg", opcode);
            true
        }
        // PUSHF
        0x9C => {
            println!("{:02X} PUSHF", opcode);
            true
        }
        // ROL / ROR / RCL / RCR / SHL / SHR / SAL / SAR
        0b_1101_0000..=0b_1101_0011 => {
            println!("{:02X} ROL / ROR / RCL / RCR / SHL / SHR / SAL / SAR", opcode);
            true
        }
        // REP | REPE | REPNE | REPNZ | REPZ
        0b_1111_0010 | 0b_1111_0011 => {
            println!("{:02X} REP | REPE | REPNE | REPNZ | REPZ", opcode);
            true
        }
        // RET (long)
        0xCB => {
            println!("{:02X} RET (long)", opcode);
            true
        }
        // RET
        0xC3 => {
            println!("{:02X} RET", opcode);
            true
        }
        // RET disp16
        0xCA => {
            println!("{:02X} RET disp16", opcode);
            true
        }
        // RET disp16
        0xC2 => {
            println!("{:02X} RET disp16", opcode);
            true
        }
        // SAHF
        0x9E => {
            println!("{:02X} SAHF", opcode);
            true
        }
        // SBB
        0b_0001_1100 | 0b_0001_1101 => {
            println!("{:02X} SBB", opcode);
            true
        }
        // SBB, SUB, XOR, CMP, AND, ADC
        0b_1000_0000..=0b_1000_0011 => {
            println!("{:02X} SBB, SUB, XOR, CMP, AND, ADC", opcode);
            true
        }
        // SBB
        0b_0001_1000..=0b_0001_1011 => {
            println!("{:02X} SBB", opcode);
            true
        }
        // SCAS
        0b_1010_1110 | 0b_1010_1111 => {
            println!("{:02X} SCAS", opcode);
            true
        }
        // SEG
        0b_0010_0110 | 0b_0010_1110 | 0b_0011_0110 | 0b_0011_1110 => {
            println!("{:02X} SEG", opcode);
            true
        }
        // STOS
        0b_1010_1010 | 0b_1010_1011 => {
            println!("{:02X} STOS", opcode);
            true
        }
        // SBB
        0b_0010_1100 | 0b_0010_1101 => {
            println!("{:02X} SBB", opcode);
            true
        }
        // SBB
        0b_0010_1000..=0b_0010_1011 => {
            println!("{:02X} SBB", opcode);
            true
        }
        // TEST ac,data
        0b_1010_1000 | 0b_1010_1001 => {
            println!("{:02X} TEST ac,data", opcode);
            true
        }
        // TEST Mod R/M
        0b_1000_0100 | 0b_1000_0101 => {
            println!("{:02X} TEST Mod R/M", opcode);
            true
        }
        // WAIT
        0x9B => {
            println!("{:02X} WAIT", opcode);
            true
        }
        // XCHNG reg
        0b_1001_0001..=0b_1001_0111 => {
            println!("{:02X} XCHNG reg", opcode);
            true
        }
        // XCHNG Mod R/M
        0b_1000_0110 | 0b_1000_0111 => {
            println!("{:02X} XCHNG Mod R/M", opcode);
            true
        }
        // XLAT
        0xD7 => {
            println!("{:02X} XLAT", opcode);
            true
        }
        // XOR
        0b_0011_0100 | 0b_0011_0101 => {
            println!("{:02X} XOR", opcode);
            true
        }
        // XOR
        0b_0011_0000..=0b_0011_0011 => {
            println!("{:02X} XOR", opcode);
            true
        }
        opcode => {
            println!("Unknown instruction: {:#02X}", opcode);
            false
        }
    }
}

mod test {
    use crate::vm::disassembler::disas;

    #[test]
    fn test_all_op() {
        for i in 0..=255u8 {
            disas(i);
        }
    }

    /// Finds missing opcodes based on undocumented opcodes
    /// https://www.righto.com/2023/07/undocumented-8086-instructions.html
    #[test]
    fn test_invalid_op() {
        for i in 0..=255u8 {
            let x = disas(i);
            match i {
                0x0F // POP CS
                | 0x60..=0x6F // JMP CONDITIONAL
                | 0xC0 // RETF imm
                | 0xC1 // RET
                | 0xC8 // RETF imm
                | 0xC9 // RET
                | 0xD6 // SALC
                | 0xF1 // LOCK prefix
                => {
                    if x {
                        println!("====== SHOULD NOT BE IMPLEMENTED: {:02X}", i);
                    }
                }
                _ => {
                    if !x {
                        println!("====== NOT IMPLEMENTED: {:02X}", i);
                    }
                }
            }
        }
    }
}