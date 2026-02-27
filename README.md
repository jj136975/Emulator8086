# Emulator8086

A bare-metal Intel 8086 emulator written in Rust that boots real operating systems. It runs MS-DOS, plays DOOM, and renders VGA graphics — all on an emulated 4.77 MHz 8086 with a custom BIOS written in x86 assembly.

## Screenshots

### MS-DOS 6.22

Terminal mode (ANSI rendering) and GUI mode (wgpu):

![MS-DOS running in terminal and GUI mode](assets/msdos.gif)

### MS-DOS Installation

Installing from floppy images with the F12 disk-swap menu:

![MS-DOS installation with floppy swap](assets/msdos_install.gif)

### DOOM (8088 port)

Copying DOOM to the hard drive and running it — in text mode and Mode 13h:

![DOOM running on the emulator](assets/doom.gif)

## Features

- Boots MS-DOS 6.22, runs real DOS programs and games
- Custom BIOS written in x86 assembly with INT 10h, 13h, 16h, 1Ah services
- Full 8086 instruction set (70+ opcodes) validated against a real Intel P80C86A-2 CPU
- VGA text mode (80x25) with ANSI terminal rendering
- VGA Mode 13h (320x200, 256 colors) with wgpu hardware-accelerated rendering
- Floppy and hard disk support with file-backed or in-memory images
- F12 menu for swapping floppy disks at runtime
- Hardware emulation: 8259 PIC, 8253 PIT, 8042 keyboard, VGA, DMA, FDC

## Installation

### Requirements

- [Rust](https://www.rust-lang.org/tools/install) (edition 2021)
- A BIOS ROM (included in `assets/rom/bios.bin`, or build your own with [NASM](https://www.nasm.us/))

### Build

```bash
# Terminal mode (default)
cargo build --release

# GUI mode (wgpu + winit window)
cargo build --release --features gui
```

## Usage

```bash
# Boot from floppy
cargo run --release -- assets/rom/bios.bin --disk a:floppy.img

# Boot from hard disk
cargo run --release -- assets/rom/bios.bin --hd msdos.vhd --boot hd0

# Multiple disks with boot order
cargo run --release -- assets/rom/bios.bin --disk a:dos1.img --hd msdos.vhd --boot a,hd0

# GUI mode (Mode 13h graphics)
cargo run --release --features gui -- assets/rom/bios.bin --hd msdos.vhd --boot hd0
```

### CLI Options

| Option | Description |
|--------|-------------|
| `<ROM>` | Path to BIOS ROM image (required) |
| `--disk <SPEC>` | Floppy disk: `a:image.img`, `b:memory`, append `:ro` for read-only |
| `--hd <PATH>` | Hard disk image (creates 32MB if file doesn't exist) |
| `--boot <ORDER>` | Boot order: `a,hd0` (default), `hd0,a`, etc. |
| `--trace` | Enable per-instruction register trace |

### In-Emulator Controls

| Key | Action |
|-----|--------|
| F12 | Open disk-swap menu (terminal) / Dump VGA state (GUI) |

## Technical Details

### CPU

Full 8086 real-mode with 1MB segmented memory (`segment << 4 + offset`).

| Category | Instructions |
|----------|-------------|
| Arithmetic | ADD, ADC, SUB, SBB, INC, DEC, CMP, MUL, IMUL, DIV, IDIV, AAA, AAS, AAD, AAM, DAA, DAS |
| Logic | AND, OR, XOR, NOT, NEG, TEST |
| Shifts/Rotates | ROL, ROR, RCL, RCR, SHL/SAL, SHR, SAR |
| Data Movement | MOV, XCHG, LEA, LES, LDS, LAHF, SAHF, CBW, CWD, XLAT |
| String Ops | MOVS, LODS, STOS, CMPS, SCAS (with REP/REPE/REPNE) |
| Control Flow | JMP, CALL, RET, RETF, IRET, Jcc (all conditions), JCXZ, LOOP, LOOPE, LOOPNE |
| Stack | PUSH, POP, PUSHF, POPF |
| I/O | IN, OUT (byte/word, immediate/DX) |
| Misc | NOP, HLT, WAIT, ESC, LOCK, INT, INT 3, INTO, CLI, STI, CLC, STC, CLD, STD, CMC |

### Emulated Hardware

| Device | Description |
|--------|-------------|
| 8259A PIC | Programmable Interrupt Controller — IRQ masking, ICW init, EOI, ISR/IRR |
| 8253 PIT | Programmable Interval Timer — 3 channels (system timer, DRAM refresh, speaker) |
| 8042 Keyboard | Scancode processing, shift/ctrl/alt tracking, circular key buffer |
| VGA | Text mode 80x25 (B8000h) + Mode 13h 320x200x256 (A0000h), CRTC, DAC, Sequencer, Attribute Controller, Graphics Controller |
| 8237A DMA | 4-channel DMA controller (register-level) |
| FDC (uPD765) | Floppy disk controller — motor control, status registers |
| HDC (Xebec) | Hard disk controller — status/config registers |
| Disk I/O | Floppy (1.44MB, 720KB) and hard disk images, CHS geometry, file-backed or in-memory |

### BIOS Interrupts

| Interrupt | Service |
|-----------|---------|
| INT 08h | Timer tick (IRQ0) — BDA counters, motor timeout, user hook (INT 1Ch) |
| INT 09h | Keyboard (IRQ1) — scancode decoding, modifier tracking, buffer management |
| INT 10h | Video — set mode (text + 13h), cursor control, scroll, read/write char, teletype, get mode, write string |
| INT 11h | Equipment list |
| INT 12h | Conventional memory size (640 KB) |
| INT 13h | Disk I/O — read/write sectors, get drive parameters, reset |
| INT 15h | System services — extended memory size query |
| INT 16h | Keyboard — read key, check buffer, shift flags, enhanced keyboard (AH=10h-12h) |
| INT 19h | Bootstrap loader |
| INT 1Ah | Time — read/set system timer ticks |

### What's Not Supported

- **Protected mode / 286+ instructions** — this is a pure 8086 emulator
- **VGA Mode X/Y** — unchained planar modes (only chain-4 Mode 13h)
- **EGA/CGA graphics modes** — only text mode and Mode 13h
- **Sound** — no Sound Blaster, PC speaker, or AdLib emulation
- **Serial/parallel ports** — COM and LPT not implemented
- **Mouse** — no mouse driver support
- **Networking** — no packet driver or NE2000

### Testing

The CPU is validated against 2,000+ test vectors per opcode, captured from a real Intel P80C86A-2 processor:

```bash
# Run all CPU tests (requires test data in 8086/v1/)
cargo test cpu_tests -- --ignored

# Test a single opcode
cargo test cpu_tests::test_opcode_00 -- --ignored
```

## Credits

- [DOOM 8088](https://github.com/FrenkelS/Doom8088) by Frenkel Smeijers — DOOM port for 8088/8086
- CPU test vectors from [SingleStepTests/8086](https://github.com/SingleStepTests/8086) captured on real P80C86A-2 hardware

## License

This project is licensed under the [MIT License](LICENSE).

You are free to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of this software.
