# Emulator8086

An Intel 8086 CPU emulator written in Rust with two execution modes: **MINIX a.out** binary execution and **BIOS boot** from floppy disk images. Implements real-mode segmented memory (1MB), IVT-based interrupt dispatch, I/O port emulation, and hardware device emulation.

## Features

- **70+ instructions** including arithmetic, logic, shifts/rotates, string operations, control flow, and stack manipulation
- **Full 8086 segmentation** with CS, DS, ES, SS segments and 20-bit physical address calculation (`segment << 4 + offset`)
- **ModR/M decoding** with all addressing modes (register direct, memory indirect, displacement)
- **Instruction prefixes**: REP/REPE/REPNE, LOCK, segment overrides
- **MINIX 2 a.out loader** with text/data segment initialization and stack setup (argc/argv/envp)
- **MINIX 2 syscalls**: `EXIT` and `WRITE` (stdout/stderr) via `INT 0x20`
- **BIOS boot mode**: boots from raw floppy disk images (e.g. MS-DOS)
- **I/O port subsystem** with IoDevice trait and port-mapped bus
- **Hardware emulation**: 8259 PIC, 8253 PIT, keyboard controller (8042), VGA text mode, floppy disk
- **BIOS services**: INT 10h (video), INT 11h (equipment), INT 12h (memory), INT 13h (disk), INT 16h (keyboard), INT 19h (bootstrap), INT 1Ah (timer)

## Building and running

```bash
cargo build --release
```

### MINIX a.out mode

```bash
cargo run --release -- <path-to-minix-aout-binary> [args...]
```

Use `--trace` to enable per-instruction register trace output:

```bash
cargo run --release -- --trace <path-to-minix-aout-binary> [args...]
```

Trace format:
```
 AX   BX   CX   DX   SP   BP   SI   DI  FLAGS IP
0000 0000 0000 0000 ffdc 0000 0000 0000 ---- 0000:31ed
```

Flags displayed: **O**verflow, **S**ign, **Z**ero, **C**arry.

### BIOS boot mode

```bash
cargo run --release -- --bios --disk <path-to-floppy-image.img>
```

The emulator loads the boot sector from the disk image at 0x7C00, verifies the 0x55AA signature, and begins execution. BIOS service interrupts are handled in Rust; the guest OS interacts with hardware through I/O ports and BIOS INT calls.

Set `RUST_LOG=debug` for verbose logging (disk I/O, BIOS calls, etc.):

```bash
RUST_LOG=debug cargo run --release -- --bios --disk dos.img
```

## Architecture

```
src/
  main.rs                   Entry point, CLI argument parsing
  vm/
    runtime.rs              CPU state, fetch-decode-execute loop, flags, stack ops
    registers.rs            16-bit general purpose, segment, and flag registers
    memory.rs               1MB segmented memory with 20-bit address masking
    modrm.rs                ModR/M byte decoding with trait-based byte/word dispatch
    instructions.rs         Instruction implementations (~1,600 lines)
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
  io/
    bus.rs                  IoDevice trait, IoBus port dispatcher
    pic.rs                  8259 PIC (master, ICW init state machine)
    pit.rs                  8253 PIT (channel 0 timer)
    keyboard.rs             8042 keyboard controller with threaded stdin input
    vga.rs                  VGA CRT registers, ANSI terminal rendering
    disk.rs                 Floppy disk image reader (CHS geometry)
  bios/
    handlers.rs             BIOS INT service handlers (10h-1Ah)
    init.rs                 IVT setup, BDA init, DPT, bootstrap
  utils/
    number.rs               Arithmetic helpers
```

### Memory layout

**BIOS mode** (1MB real-mode address space):

| Address     | Size    | Description                 |
|-------------|---------|---------------------------- |
| `0x00000`   | 1 KB    | Interrupt Vector Table      |
| `0x00400`   | 256 B   | BIOS Data Area              |
| `0x07C00`   | 512 B   | Boot sector load address    |
| `0x0A000`   |         | End of conventional memory  |
| `0xB8000`   | 4 KB    | VGA text buffer (80x25x2)   |
| `0xF0000`   | 64 KB   | BIOS ROM area (IRET stubs)  |

**MINIX mode** (segmented, 5 x 64KB):

| Segment | Base      | Description |
|---------|-----------|-------------|
| IVT     | `0x00000` | Scratch     |
| CS      | `0x10000` | Code        |
| DS      | `0x20000` | Data        |
| ES      | `0x30000` | Extra       |
| SS      | `0x40000` | Stack       |

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
| Interrupts | INT, INT 3, INTO, IRET |
| I/O | IN, OUT (byte and word, immediate and DX-addressed) |
| Misc | NOP, HLT, WAIT, ESC, LOCK |

## Dependencies

| Crate | Purpose |
|-------|---------|
| `clap` / `clap_derive` | CLI argument parsing |
| `byteorder` | Endian-aware byte reads |
| `log` / `env_logger` | Logging |
| `enum_primitive` / `num-derive` / `num-traits` | Enum-to-integer conversions |
