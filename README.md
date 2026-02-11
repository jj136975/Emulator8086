# Emulator8086

An Intel 8086 CPU emulator written in Rust, capable of loading and executing MINIX 2 a.out binaries. Implements real-mode segmented memory, ModR/M addressing, and a subset of MINIX 2 system calls.

## Features

- **70+ instructions** including arithmetic, logic, shifts/rotates, string operations, control flow, and stack manipulation
- **Full 8086 segmentation** with CS, DS, ES, SS segments and physical address calculation (`segment << 4 + offset`)
- **ModR/M decoding** with all addressing modes (register direct, memory indirect, displacement)
- **Instruction prefixes**: REP/REPE/REPNE, LOCK, segment overrides
- **MINIX 2 a.out loader** with text/data segment initialization and stack setup (argc/argv/envp)
- **MINIX 2 syscalls**: `EXIT` and `WRITE` (stdout/stderr) via `INT` instruction

## Architecture

```
src/
  main.rs                   Entry point, CLI argument parsing
  vm/
    runtime.rs              CPU state, fetch-decode-execute loop, flags, stack ops
    registers.rs            16-bit general purpose, segment, and flag registers
    memory.rs               320KB segmented memory (5 x 64KB segments)
    modrm.rs                ModR/M byte decoding with trait-based byte/word dispatch
    instructions.rs         Instruction implementations (~1,500 lines)
    disassembler.rs         Disassembly support (incomplete)
  a_out/
    executable.rs           A.OUT binary loading
    header.rs               A.OUT header parsing (magic 0x0301)
    cpuid.rs                CPU type enumeration
    flags.rs                Executable flags
    error.rs                Error types
  minix2/
    syscall.rs              MINIX 2 syscall dispatch (WRITE, EXIT)
    interruption.rs         Interrupt message structures
  utils/
    number.rs               Arithmetic helpers
```

### Memory layout

| Offset | Size  | Segment |
|--------|-------|---------|
| 0x00000 | 64 KB | Interrupt vector table / scratch |
| 0x10000 | 64 KB | CS (Code) |
| 0x20000 | 64 KB | DS (Data) |
| 0x30000 | 64 KB | ES (Extra) |
| 0x40000 | 64 KB | SS (Stack) |

### Instruction coverage

| Category | Instructions |
|----------|-------------|
| Arithmetic | ADD, ADC, SUB, SBB, INC, DEC, CMP, MUL, IMUL, DIV, IDIV, AAA, AAS, AAD, AAM, DAA, DAS |
| Logic | AND, OR, XOR, NOT, NEG, TEST |
| Shifts | ROL, ROR, RCL, RCR, SHL/SAL, SHR, SAR |
| Data movement | MOV, XCHG, LEA, LES, LDS, LAHF, SAHF, CBW, CWD, XLAT |
| String | MOVS, LODS, STOS, CMPS, SCAS (with REP/REPE/REPNE) |
| Control flow | JMP, CALL, RET, RETF, IRET, Jcc (all conditions), JCXZ, LOOP, LOOPE, LOOPNE |
| Stack | PUSH, POP, PUSHF, POPF |
| Flags | CLC, STC, CLD, STD, CLI, STI, CMC |
| Interrupts | INT, INTO, IRET |
| Misc | NOP, HLT, WAIT, ESC, IN/OUT (stubs) |

## Building and running

```bash
cargo build --release
cargo run -- <path-to-minix-aout-binary> [args...]
```

The emulator prints a register trace for every executed instruction:

```
 AX   BX   CX   DX   SP   BP   SI   DI  FLAGS IP
0000 0000 0000 0000 ffdc 0000 0000 0000 ---- 0000:31ed
```

Flags displayed: **O**verflow, **S**ign, **Z**ero, **C**arry.

Set `RUST_LOG=debug` for verbose logging:

```bash
RUST_LOG=debug cargo run -- ./my_program
```

## Known issues

### Bugs

1. **`get_segment()` always returns ES** (`runtime.rs:213-219`) -- All four arms of the segment match return `&mut self.registers.es`. This breaks segment override prefixes entirely.

2. **`push_word()` increments SP instead of decrementing** (`runtime.rs:191-194`) -- Uses `wrapping_add` where it should use `wrapping_sub`. The 8086 stack grows downward: PUSH should decrement SP before writing. `push_byte()` correctly uses `wrapping_sub`.

3. **STOS uses SI instead of DI** for the destination address -- The 8086 STOS instruction stores AL/AX at ES:DI, not ES:SI.

4. **SCAS uses SI instead of DI** for the scan address -- Same issue; SCAS compares AL/AX with the byte/word at ES:DI.

5. **Auxiliary carry flag** is not updated by several instructions (ADC, SBB, CMP and others).

### Limitations

- Only two MINIX syscalls implemented (WRITE and EXIT) -- no file I/O, process management, or signals
- IN/OUT port instructions are stubs (no peripheral emulation)
- No coprocessor (8087) support beyond ESC passthrough
- Disassembler module is incomplete
- No test suite
- Unknown opcodes terminate with `exit(1)` rather than raising an invalid opcode interrupt

## Recommendations

Here are some suggestions ordered by impact:

1. **Fix the `get_segment()` bug** -- This is a critical copy-paste error. The CS, SS, and DS arms should return their respective segments. Without this fix, any program using segment override prefixes will malfunction.

2. **Fix `push_word()`** -- Change `wrapping_add` to `wrapping_sub`. This bug corrupts the stack for every word-sized PUSH (including CALL, INT, and PUSHF), which means most non-trivial programs will crash.

3. **Fix STOS/SCAS register usage** -- These should use DI, not SI.

4. **Add tests** -- Even simple unit tests for flag computation, ModR/M decoding, and push/pop would catch regressions and make bug-fixing safer. Integration tests that run small known-good binaries and check output would also be very valuable.

5. **Implement auxiliary carry flag** -- Required for BCD instructions (DAA/DAS/AAA/AAS) to work correctly. Mark the TODO sites and address them systematically.

6. **Improve error handling** -- Replace `exit(1)` on unknown opcodes with a proper invalid-opcode interrupt or at least a descriptive panic with the offending PC and opcode byte.

7. **Complete the disassembler** -- Useful for debugging; could be exposed as a `--disasm` CLI flag.

8. **Expand syscall support** -- READ and OPEN would allow running more interesting MINIX programs.

## Dependencies

| Crate | Purpose |
|-------|---------|
| `clap` / `clap_derive` | CLI argument parsing |
| `byteorder` | Endian-aware byte reads |
| `log` / `env_logger` | Logging |
| `enum_primitive` / `num-derive` / `num-traits` | Enum-to-integer conversions |

## License

Not yet specified.
