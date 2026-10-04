# Emulator8086

A bare-metal Intel 8086 emulator written in Rust that boots real operating systems. It runs MS-DOS, plays DOOM, and renders VGA graphics — all on an emulated 8086 PC with a custom BIOS written in x86 assembly.

## Features

- Boots MS-DOS 6.22, runs real DOS programs and games
- Custom BIOS written in x86 assembly with INT 10h, 13h, 16h, 1Ah services
- Full 8086 instruction set (70+ opcodes) validated against a real Intel P80C86A-2 CPU
- VGA text mode (80x25) and Mode 13h (320x200, 256 colors), shown in a window (winit + wgpu) at a whole-number scale
- A tools overlay in the window (F12): pause and resume, reset, VGA dump, the machine's drives at a glance
- Floppy and hard disk support with file-backed or in-memory images
- Hardware emulation: 8259 PIC, 8253 PIT, XT keyboard (8255 PPI), VGA, DMA, FDC

## Screenshots

### MS-DOS 6.22

![MS-DOS running in the window](assets/gif/msdos_edit_gui.gif)

### DOOM (8088 port)

Running DOOM in Mode 13h:
![DOOM running on the emulator](assets/gif/msdos_doom.gif)

## Installation

### Requirements

- [Rust](https://www.rust-lang.org/tools/install) 1.97.1 (edition 2024; the version is pinned in `rust-toolchain`, which rustup follows)
- A graphics adapter that wgpu supports (Vulkan, Direct3D 12, Metal or OpenGL)
- Nothing else to run it: the BIOS is built into the binary. [NASM](https://www.nasm.us/) is only needed to change the BIOS (`assets/rom`)

### Build

```bash
cargo build --release
```

## Usage

```bash
# Boot from floppy
cargo run --release -- --disk a:floppy.img

# Boot from hard disk
cargo run --release -- --hd msdos.vhd --boot hd0

# Multiple disks with boot order
cargo run --release -- --disk a:dos1.img --hd msdos.vhd --boot a,hd0

# Another BIOS image in place of the built-in one
cargo run --release -- my-bios.bin --hd msdos.vhd --boot hd0
```

### CLI Options

| Option | Description |
|--------|-------------|
| `[ROM]` | Path to a BIOS ROM image to run in place of the built-in BIOS (optional) |
| `--disk <SPEC>` | Floppy disk: `[DRIVE:]SOURCE`, with drive `a`–`d` (default `a`) and source an image file, `memory` or `memory:KB` (e.g. `a:image.img`, `b:memory`). Append `:ro` for read-only. The image for `a:` must exist; for the other drives a missing file is created as a blank 1.44 MB image |
| `--hd <PATH>` | Hard disk image, or `memory` (creates a blank 32 MB image if the file doesn't exist) |
| `--boot <ORDER>` | Boot order: `a,hd0` (default), `hd0,a`, etc. |
| `--trace` | Per-instruction register trace, written to the debug log: run with `RUST_LOG=debug` |

### In-Emulator Controls

| Key | Action |
|-----|--------|
| F12 | Show or hide the tools overlay |
| Any other key | Goes to the guest, with the exceptions below while the overlay is shown |

Closing the window quits the emulator.

### The Tools Overlay

F12 puts a menu bar over the picture; for the first seconds after start a hint in the corner names the key. The guest keeps running under it.

| Menu | Entry | What it does |
|------|-------|--------------|
| Machine | Pause / Resume | Holds the guest still while the window goes on drawing. The menu bar and the window title say so. The guest's timer is not owed the time it stood paused |
| Machine | Reset | Reboots the guest |
| Machine | Dump VGA state | Writes the state of the display to `vga_dump.txt` in the working directory |
| View | About | A panel with the key bindings, the version, and the machine: the image in each drive, whether it can be written, and the boot order |

A panel is a window over the picture that can be moved and closed. What a tool did, or why it failed, shows for a few seconds below the menu bar.

While the overlay is shown the keys still go to the guest, except that Tab moves between the overlay's controls, and that the keys are the overlay's own for as long as one of its menus is open or one of its controls has the keyboard focus (Escape closes the menu and gives the focus up). Whatever the guest was holding at that moment is released.

Disk tools (swapping and saving disks, copying host files onto them) are to come as panels of the overlay; until then, disks are chosen on the command line.

### Keyboard Layouts

The window sends the physical key positions, like a real keyboard: with a non-US keyboard, load the matching layout in DOS (`KEYB`).

## Technical Details

### Layout

The repository is a Cargo workspace of two crates. `crates/emu8086_core` is the machine as a library: the CPU, the devices, the disks and the BIOS it embeds. It opens no window, uses no GPU and reads no clock, and builds for `wasm32-unknown-unknown` as it is. The root package is the application around it: the command line and the window, which shows the machine's picture, sends it the keys and tells it the time, and draws the tools overlay over the picture (egui). `cargo build` and `cargo run` at the root build and run the application.

### CPU

Full 8086 real-mode with 1MB segmented memory (`segment << 4 + offset`), plus the 80186 additions PUSHA/POPA, PUSH imm, IMUL imm, INS/OUTS, shift/rotate by an immediate count and ENTER/LEAVE (BOUND is accepted and does nothing).

The CPU is not cycle-timed: it runs as fast as the host allows, while the timer chip follows real time.

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
| XT Keyboard (8255 PPI) | Scancode latch with acknowledge and reset through port 61h, speaker gate, status port |
| VGA | Text mode 80x25 (B8000h) + Mode 13h 320x200x256 (A0000h), CRTC, DAC, Sequencer, Attribute Controller, Graphics Controller |
| 8237A DMA | 4-channel DMA controller (register-level) |
| FDC (uPD765) | Stub — digital output register and an idle status only; disk I/O goes through the BIOS |
| HDC (Xebec) | Stub — status/config registers |
| Disk I/O | Floppy (160 KB to 2.88 MB) and hard disk images, CHS geometry, change line, file-backed or in-memory |

### BIOS Interrupts

| Interrupt | Service |
|-----------|---------|
| INT 08h | Timer tick (IRQ0) — BDA counters, motor timeout, user hook (INT 1Ch) |
| INT 09h | Keyboard (IRQ1) — scancode decoding, modifier tracking, buffer management, Ctrl-Break, Ctrl-Alt-Del |
| INT 10h | Video — set mode (80x25 text + 13h, with palette load), cursor and page control, scroll, read/write char, teletype, get mode, write string, DAC and blink functions, Mode 13h pixels |
| INT 11h | Equipment list |
| INT 12h | Conventional memory size (640 KB) |
| INT 13h | Disk I/O — read/write/verify sectors, format track, drive parameters, disk type and change line, reset, LBA extensions |
| INT 15h | System services — extended memory size query |
| INT 16h | Keyboard — read key, check buffer, shift flags, enhanced keyboard (AH=10h-12h) |
| INT 19h | Bootstrap loader |
| INT 1Ah | Time — read/set system timer ticks |

### What's Not Supported

- **Protected mode / 286+ instructions** — real mode only, 8086 plus the 80186 additions listed above
- **VGA Mode X/Y** — unchained planar modes (only chain-4 Mode 13h)
- **EGA/CGA graphics modes, 40-column text** — only 80x25 text mode and Mode 13h
- **Real-time clock** — DOS starts at 1980-01-01
- **Sound** — no Sound Blaster, PC speaker, or AdLib emulation
- **Serial/parallel ports** — COM and LPT not implemented
- **Mouse** — no mouse driver support
- **Networking** — no packet driver or NE2000

### Testing

The CPU is validated against 608,000 single-instruction test vectors (2,000 per file) captured from a real Intel P80C86A-2 processor. The opcodes that are given their 80186 meaning (60–62, 64–6F, C0, C1, C8, C9) are outside that set. The BIOS and the devices are covered by tests that boot the real ROM on a fully wired machine. The picture is checked on the graphics adapter itself: one test draws a text frame and a Mode 13h frame into an offscreen texture and compares the pixels it reads back, another draws the overlay over the picture; on a machine without an adapter they say so and pass. The overlay's logic is tested without a window, by clicking through it with hand-made input.

```bash
# Fetch the CPU test vectors (git submodule)
git submodule update --init

# Unit and integration tests of both crates
cargo test --workspace

# Everything, including the hardware-captured CPU vectors
cargo test --workspace --release -- --include-ignored

# Test a single opcode
cargo test -p emu8086_core cpu_tests::test_opcode_00 -- --ignored
```

## Credits

- [DOOM 8088](https://github.com/FrenkelS/Doom8088) by Frenkel Smeijers — DOOM port for 8088/8086
- CPU test vectors from [SingleStepTests/8086](https://github.com/SingleStepTests/8086) captured on real P80C86A-2 hardware

## License

This project is licensed under the [MIT License](LICENSE).

You are free to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of this software.
