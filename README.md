# Emulator8086

A bare-metal Intel 8086 emulator written in Rust that boots real operating systems. It runs MS-DOS, plays DOOM, and renders VGA graphics — all on an emulated 8086 PC with a custom BIOS written in x86 assembly.

## Features

- Boots MS-DOS 6.22, runs real DOS programs and games
- Custom BIOS written in x86 assembly with INT 10h, 13h, 16h, 1Ah services
- Full 8086 instruction set (70+ opcodes) validated against a real Intel P80C86A-2 CPU
- VGA text mode (80x25) and Mode 13h (320x200, 256 colors), shown in a window (winit + wgpu) at a whole-number scale
- A tools overlay in the window (F12): pause and resume, reset, a restart with another ROM, the guest's speed and where the time goes, viewers of memory and of each device, VGA dump, the drives (put disks in and take them out, save them, copy host files onto them), and an on-screen PC keyboard
- Runs in a browser as well, [online](https://jj136975.github.io/Emulator8086/): the same application built for WebAssembly, drawn with WebGPU or WebGL2, on phones and tablets too, where MS-DOS 4.00 or DOOM is one press away
- Builds MS-DOS 4.00 from Microsoft's published source inside itself, with the tools of 1988, 61 of its 62 files byte for byte as released; the site publishes an MS-DOS 4.00 boot floppy and a DOOM hard disk made of it ([The site's disks](#the-sites-disks))
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
| `[ROM]` | Path to a BIOS ROM image to run in place of the built-in BIOS (optional): 16 bytes to 64 KB, which end at F000:FFFF. The rest of the ROM area, F000:0000 up, reads 0xFF, as an empty socket does |
| `--disk <SPEC>` | Floppy disk: `[DRIVE:]SOURCE`, with drive `a`–`d` (default `a`) and source an image file, `memory` or `memory:KB` (e.g. `a:image.img`, `b:memory`). Append `:ro` for read-only. The image for `a:` must exist; for the other drives a missing file is created as a blank 1.44 MB image |
| `--hd <PATH>` | Hard disk image, or `memory` (creates a blank 32 MB image if the file doesn't exist) |
| `--boot <ORDER>` | Boot order: `a,hd0` (default), `hd0,a`, etc. |
| `--trace` | Per-instruction register trace, written to the debug log: run with `RUST_LOG=debug` |

### In-Emulator Controls

| Control | Action |
|---------|--------|
| F12 | Show or hide the tools overlay (in a browser: a right-click on the picture, see [Web build](#web-build)) |
| Touch and hold | On a touch screen: a finger held still on the picture for half a second shows or hides the tools overlay, as soon as the time is up |
| Any other key | Goes to the guest, with the exceptions below while the overlay is shown |

Closing the window quits the emulator.

### The Tools Overlay

F12, or a finger held on the picture, puts a menu bar over the picture; for the first seconds after start a hint in the corner names the switch (in a browser, which starts with the tools shown, once they hide). The guest keeps running under it.

| Menu | Entry | What it does |
|------|-------|--------------|
| Machine | Pause / Resume | Holds the guest still while the window goes on drawing. The menu bar and the window title say so. The guest's timer is not owed the time it stood paused |
| Machine | Reset | Reboots the guest |
| Machine | Load ROM | Restarts the machine with another ROM image, in place of the BIOS: the built-in BIOS, the BIOS the site publishes (in a browser, see [the site's files](#the-sites-files)), or an image file of 16 bytes to 64 KB chosen in a file dialog. The image ends at F000:FFFF and the rest of the ROM area reads 0xFF, as an empty socket does; the machine restarts at the reset vector, as Reset does, with its memory, its devices and its drives as they are. A message says which ROM runs then, or why the machine runs on with the one it had |
| Machine | Dump VGA state | Writes the state of the display to `vga_dump.txt` in the working directory (in a browser: offers it as a download) |
| View | Start | In a browser only: a button for each disk of the site, Boot MS-DOS 4.00 and Play DOOM, which restarts the machine with that disk ([The Start window](#the-start-window)). A browser opens it at start |
| View | About | A panel with the key bindings, the version, and the machine: the ROM it runs, the image in each drive, whether it can be written, and the boot order |
| View | Disks | Every drive with the disk it holds: where its image lives (a file, or memory), its size, its cylinders, heads and sectors, and whether it can be written. For the drive chosen in the list: put in an image file, a blank disk held in memory (a formatted floppy of a standard size, or a blank 32 MB hard disk) or a new image file (a formatted 1.44 MB floppy, or a blank 32 MB hard disk), write-protected if the box is ticked; take the disk out, move it to another drive, save it to a file, or copy host files or a whole folder onto it |
| View | On-screen keyboard | The 83 keys of the PC/XT keyboard, printed for the US and laid out like a keyboard without a numeric pad, as large as the screen allows: for a phone, or wherever no keyboard is at hand. A key is pressed with a finger or the mouse, several at once with several fingers, and sends what the physical key sends, so the guest's layout decides what it types. A key held repeats as an XT's does. Shift, Ctrl and Alt tapped alone (down and up again before they repeat, with no other key down) stay down until the next key comes up, and a second tap lets them go. The lock keys light up with the guest's Caps Lock, Num Lock and Scroll Lock |
| View | Stats | Whether the guest keeps up with real time, and where the host's time goes: the guest's speed with a chart of its last minute, how a frame divides between running the guest, idling, the overlay, drawing and waiting for the display, the guest's clock and its timer against real time, the time owed and the time dropped, the share spent halted, and the interrupts taken on each line. The menu bar shows the speed at its right end |
| View | Memory | A hex view of guest memory with an ASCII column: an address field (`B8000` or `B800:0000`), jumps to where the registers point and to the vector table, the BIOS data area, video memory and the ROM |
| View | Interrupts (8259A) | Each interrupt line's request, in-service and mask bits, the vector base and the modes in force |
| View | Timer (8253) | The three channels: mode, reload value, current count, output and gate, and the rate of the timer interrupt |
| View | Video (VGA) | The video mode, the cursor, the registers a reader looks for first, and the 256 colours of the DAC |
| View | Keyboard (8255) | The byte in port A, whether the guest still owes its acknowledge, the queued scancodes and port B's bits by name |
| View | Port E9h log | The last lines the guest wrote to the debug port: the BIOS's progress markers and a copy of its teletype output |

A panel is a window over the picture that can be moved and closed. What a tool did, or why it failed, shows for a few seconds below the menu bar.

While the overlay is shown the keys still go to the guest, except that Tab moves between the overlay's controls, and that the keys are the overlay's own for as long as one of its menus is open or one of its controls has the keyboard focus (Escape closes the menu and gives the focus up). Whatever the guest was holding at that moment is released.

The Disks panel asks for files in the system's own file dialogs, and the guest runs on while one is open. An image file put into a drive is the disk itself: what the guest writes goes straight to the file. A blank disk lives in memory until it is saved. One image file is in one drive at most, putting an image in never creates a file, a new image file never replaces one, and an empty file is no disk: putting one in is refused. DOS notices a floppy change by itself. It does not notice a change to a hard disk, and writes what it holds in memory over it: after a hard disk was put in, taken out, moved or written to, the guest has to be rebooted before it uses the drive, which the panel says, with a button that resets the machine. To copy files onto a hard disk without a reboot, put them on a floppy and `COPY` them in DOS.

### Keyboard Layouts

The window sends the physical key positions, like a real keyboard: with a non-US keyboard, load the matching layout in DOS (`KEYB`). The on-screen keyboard is printed for the US whatever layout the guest has loaded.

## Web build

The same application builds for `wasm32-unknown-unknown` and runs in a browser, in a page that holds nothing but the emulator's picture, as large as the browser's window.

The build of `main` is online at <https://jj136975.github.io/Emulator8086/>: GitHub Actions builds it and publishes it on GitHub Pages at every push to `main` (`.github/workflows/pages.yml`).

### Requirements

- The `wasm32-unknown-unknown` target. `rust-toolchain` lists it, so rustup installs it with the toolchain
- [trunk](https://trunkrs.dev/) 0.21 (`cargo install trunk`). On its first build it fetches the `wasm-bindgen` tool of the version the project is locked to, and `wasm-opt` for a release build

### Build and serve

```bash
# Build, serve on http://127.0.0.1:8080 and rebuild on every change
trunk serve

# The same with the optimised build, the one to run a guest in
trunk serve --release

# Only build: the page and its files go to dist/, for any web server
trunk build --release
```

`index.html` at the root of the repository is the page trunk builds the application into. The page `trunk build` makes is for the root of a web server. A page served below a path, as GitHub Pages serves a repository's site, is built with that path as its public URL:

```bash
trunk build --release --public-url /Emulator8086/
```

### The site's files

Beside the page, the site publishes files the application fetches from it, each named in a table of the application's (`src/site.rs`):

| Path | What it is | Where the application offers it |
|------|------------|---------------------------------|
| `rom/bios.bin` | The BIOS. The workflow assembles it from its sources in `assets/rom` with NASM, and fails unless it is the image committed there, which is the one built into the application | Machine > Load ROM > BIOS from the site |
| `disks/msdos400.img` | "MS-DOS 4.00": a boot floppy of MS-DOS 4.00, which the workflow builds ([The site's disks](#the-sites-disks)) | The Start window (View > Start) > Boot MS-DOS 4.00 |
| `disks/doom8088.img` | "DOOM": a hard disk on which MS-DOS 4.00 starts DOOM, which the workflow makes as well | The Start window (View > Start) > Play DOOM |

`trunk build` and `trunk serve` put the BIOS beside the page, from the repository. The disks are not in the repository: the workflow makes them, and puts them beside the page in what trunk built, so a site served by `trunk serve` has none. A file is fetched by its path relative to the page, so it is found wherever the page is served from: at the root of `trunk serve`, or below `/Emulator8086/` on GitHub Pages. Where the server has no such file (a 404, or the page itself in its place, which is how `trunk serve` answers a path it has no file for), a message says that the site has no such file, and the machine runs on as it was.

### Browsers

The picture is drawn with WebGPU where the browser has a working one, and with WebGL2 where it has not. Which of the two is in use is logged to the browser's console at start ("Drawing with ..."). A browser offers WebGPU to secure pages only: `http://127.0.0.1:8080` and `https` are, the same server reached over plain `http` from another machine is not, and is drawn with WebGL2.

### What is different in a browser

| | On a desktop | In a browser |
|---|---|---|
| The machine at start | What the command line asks for | The built-in BIOS and no disk, with the tools shown and the Start window open ([below](#the-start-window)). Behind it the BIOS reports that there is nothing to boot and waits for a key: a disk put in with the Disks panel boots at the next key |
| The tools overlay | F12 | A right-click on the picture. F12 stays the browser's key, for its developer tools; the hint and the About panel say "Right-click or touch and hold" |
| View > Start | Not offered: a desktop's program has no site | The disks of the site, one press each |
| Machine > Dump VGA state | Writes `vga_dump.txt` | Offers `vga_dump.txt` as a download |
| Machine > Load ROM | The built-in BIOS, or an image file chosen in a dialog | The built-in BIOS, the BIOS the site publishes (`rom/bios.bin`), or an image file chosen in the file picker |
| An image file put into a drive | Is the disk: the guest writes to the file | Is copied into the page and held in memory: the file stays as it was |
| Saving a disk | To a file named in a dialog | As a download |
| A new image file, a folder of host files | Offered | Not offered: a page can create no file and read no folder |
| The log, and errors | stderr, at the levels `RUST_LOG` names | The browser's console, from the `info` level up. A panic is told there too |
| Time stamps on FAT volumes | The local time of the host | The local time of the browser |

The keys go to the guest while the picture has the keyboard: click it once. The page takes them from the browser, so that F5 or Tab reach the guest instead of reloading the page or leaving the picture. It leaves the browser the two keys no XT keyboard has, F11 (full screen) and F12 (developer tools), and the keys a browser gives to no page (Ctrl+W, Ctrl+T and the like) do not reach the guest either.

The machine runs with the frames of the page. An idle or paused guest gives the page its thread back until the next frame. A tab in the background gets no frames: the machine stands still until the tab is shown again, and the guest's timer is then owed a quarter of a second at most, as after any stall.

Disks get into the browser through the Disks panel: an image file chosen in the browser's file picker is copied into the page, where it lives in memory and takes the guest's writes, and Download gives the disk back as a file. A blank disk can be put in as on a desktop, and files chosen in the picker copied onto a disk. A browser opens its file picker only right after a click or a key: when it will not, a small form in the page asks for the file instead.

### The Start window

At start, with no disk in any drive, the page shows the tools with a small window, Start, which has a button for each disk of the site: **Boot MS-DOS 4.00** and **Play DOOM** ([The site's disks](#the-sites-disks)). A press fetches the disk from the site, and the window says so meanwhile ("Fetching MS-DOS 4.00...") and offers nothing else. Once the disk has come it goes into its drive, the floppy into A: and the hard disk in as the first hard disk, C:, in place of the disk that was there; the BIOS is told to boot that drive first, and the machine restarts, as Machine > Reset restarts it. A message says what it restarted with ("Restarted with MS-DOS 4.00, the floppy in A:"), the Start window closes and the tools hide, so that the keys go to the guest; a user who has not worked the switch yet is then shown the hint that names it. Where the site has no such disk (a site of `trunk serve` has none), or the disk did not come, a message says why, and the machine runs on as it was.

The window stays at hand in the View menu. A disk started from it lives in memory, as one put in with the Disks panel does: what the guest writes to it is lost with the page, unless Download gives the disk back as a file. A desktop, which comes from no site, has no Start window, and starts as its command line says.

### Phones and tablets

On a touch screen a finger works the tools overlay as the mouse does: a tap is a click. There is no right-click to show the overlay with: a finger held still on the picture for half a second shows it or hides it, as soon as the time is up (on a desktop's touch screen as well). A finger held on the overlay itself, on its menu bar, a window or a message, is the overlay's however long it stays there. The page keeps the fingers on the picture for itself: the browser neither scrolls nor zooms it.

A finger that presses a button of the Start window leaves the tools shown once the machine has restarted, with the On-screen keyboard open in place of the Start window: one tap, and the phone can type at `A:\>` or play.

The guest's keys come from View > On-screen keyboard, which fits the width of a phone held upright and is easier to hit with the phone on its side. Each finger holds a key of its own, so that keys are held together, an arrow while Ctrl fires in DOOM; a modifier tapped alone stays down for the next key, for Ctrl+C or Ctrl+Alt+Del. The keyboard is a panel of the overlay: hidden with it, it lets go of whatever a finger held. The phone's own keyboard is not used: it types characters rather than pressing keys, holds none down, and has no Esc, function keys, Ctrl, Alt or arrows.

A keyboard paired with the phone or the tablet reaches the guest as a desktop's keyboard does.

## The site's disks

Beside the page, the site publishes two disks, which the emulator makes itself:

| Disk | What it is |
|------|------------|
| `disks/msdos400.img` | A 1.44 MB floppy that boots MS-DOS 4.00 to its `A:\>` prompt, with all of its programs but the installer, SELECT |
| `disks/doom8088.img` | A 2.5 MB hard disk on which MS-DOS 4.00 starts [Doom8088](https://github.com/FrenkelS/Doom8088), Frenkel Smeijers's port of DOOM to the 8088, with the shareware episode |

The MS-DOS 4.00 on them is built from its source, inside the emulator. Microsoft and IBM published the source of MS-DOS 4.00 under the MIT License in 2024, and the community restored the files that came out damaged ([lotharsm/MS-DOS](https://github.com/lotharsm/MS-DOS), branch `dos-4.00-reconstructed`). The workflow that publishes the site boots FreeDOS 1.4 in the emulator, and FreeDOS builds that source with the tools that come with it: NMAKE, MASM, C and LINK of 1988, which run 11.4 billion instructions on the emulated 8086. Of the 62 files the build makes, 61 are byte for byte those of the MS-DOS 4.00 release of 1988-10-06; the 62nd, XMAEM.SYS, is not on that release's disks. The emulator then boots each disk: the floppy to its prompt, and the hard disk until DOOM plays its demo; MS-DOS 4.00's own CHKDSK finds both clean. The workflow fails unless the 62 files and the two disks are those whose SHA-256 the repository lists (`crates/dos_images/msdos400-binaries.sha256`, `disks.sha256`): the same inputs make the same disks, byte for byte. Its inputs are pinned (`crates/dos_images/pins.env`) and checked when they are fetched.

The licenses:

- MS-DOS 4.00 is under the MIT License. Each disk carries it as `LICENSE.TXT`, with a `README.TXT` that says where the source is and how the disk was made.
- Doom8088 is under the GNU General Public License, version 2. The hard disk carries the license as `\DOOM\COPYING`, and where its source is as `\DOOM\SOURCE.TXT`.
- `DOOM1.WAD` is id Software's shareware DOOM data, as Doom8088's release ships it, prepared for Doom8088. It is not under the GPL.
- FreeDOS only builds MS-DOS 4.00, and is on neither disk.

To make the disks on your own machine, in bash with curl, sha256sum, unzip and git (on Windows: Git Bash), from the root of the repository, into directories beside it:

```bash
inputs=../dos-inputs out=../dos-out lists=$PWD/crates/dos_images

# Fetch the inputs into a new directory, each checked against its pin
bash crates/dos_images/fetch-inputs.sh "$inputs"

# Build MS-DOS 4.00 in the emulator and make the disks of it: about two minutes
cargo run --release -p dos_images -- --freedos "$inputs/freedos" \
    --msdos "$inputs/msdos/v4.0" --doom8088 "$inputs/doom8088" --out "$out"

# Check what was made as the workflow does
(cd "$out/bin" && sha256sum --check --quiet "$lists/msdos400-binaries.sha256")
(cd "$out/disks" && sha256sum --check "$lists/disks.sha256")

# Boot them in the window
cargo run --release -- --disk "a:$out/disks/msdos400.img"
cargo run --release -- --hd "$out/disks/doom8088.img" --boot hd0
```

The tool writes the 62 files the build made into `$out/bin`, the disks into `$out/disks`, and what it did into `$out/report.txt`. A stage that fails says why, with what the emulated screen showed; a build that fails leaves its disk as `$out/failed-build.img`.

## Technical Details

### Layout

The repository is a Cargo workspace of three crates. `crates/emu8086_core` is the machine as a library: the CPU, the devices, the disks and the BIOS it embeds. It opens no window, uses no GPU and reads no clock, and builds for `wasm32-unknown-unknown` as it is. The root package is the application around it: the command line and the window, which shows the machine's picture, sends it the keys and tells it the time, and draws the tools overlay over the picture (egui). `cargo build` and `cargo run` at the root build and run the application. It builds for a desktop and for a browser; what differs between the two is in one module, `src/platform`, and nothing else in the application asks which of them it runs on. `crates/dos_images` is a tool without a window that runs the machine to build MS-DOS 4.00 and make the site's disks ([The site's disks](#the-sites-disks)); the guest's time there is counted in the instructions it runs, so that every run makes the same disks.

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

The CPU is validated against 608,000 single-instruction test vectors (2,000 per file) captured from a real Intel P80C86A-2 processor. The opcodes that are given their 80186 meaning (60–62, 64–6F, C0, C1, C8, C9) are outside that set. The BIOS and the devices are covered by tests that boot the real ROM on a fully wired machine. The picture is checked on the graphics adapter itself: one test draws a text frame and a Mode 13h frame into an offscreen texture and compares the pixels it reads back, another draws the overlay over the picture; on a machine without an adapter they say so and pass. The overlay's logic is tested without a window, by clicking and touching through it with hand-made input, and the disk tools and the ROM loader with file dialogs the tests answer by hand. Of the browser build, the suite covers what needs no browser, on the desktop: the pacing of a guest on a host that cannot wait, the window's events as the overlay's input, touches among them, the time stamps, the disk tools on a host that hands files over as their contents, the ROMs of the site offered and loaded with answers the tests give by hand, the Start window opened at start and pressed with a finger or the mouse, its disks fetched, put in and booted by the built-in BIOS with answers the tests give by hand, what a site's answer to a fetch says, and the frame drawn within the limits WebGL2 guarantees. The files the site publishes are checked against what the page and the workflow publish. The page itself is outside the suite, and so are the system's file dialogs, the browser's file picker and its fetch.

The build of MS-DOS 4.00 is the widest test of the CPU there is: NMAKE, MASM, C and LINK run 11.4 billion instructions, and a fault of the CPU that one of them meets is likely to change one of the 62 files they make, which the workflow compares with the release of 1988. It needs inputs the repository does not hold, so it is not part of `cargo test`: the workflow runs it at every push that changes the machine, and a developer runs it by hand ([The site's disks](#the-sites-disks)). `cargo test` covers the tool's parts that need no inputs: what it reads of the makefiles, the layout of the disks, the markers, the build's log and CHKDSK's report, and the notices.

```bash
# Fetch the CPU test vectors (git submodule)
git submodule update --init

# Unit and integration tests of the three crates
cargo test --workspace

# Everything, including the hardware-captured CPU vectors
cargo test --workspace --release -- --include-ignored

# Test a single opcode
cargo test -p emu8086_core cpu_tests::test_opcode_00 -- --ignored
```

## Credits

- [DOOM 8088](https://github.com/FrenkelS/Doom8088) by Frenkel Smeijers — DOOM port for 8088/8086
- CPU test vectors from [SingleStepTests/8086](https://github.com/SingleStepTests/8086) captured on real P80C86A-2 hardware
- [MS-DOS](https://github.com/microsoft/MS-DOS) 4.00 by Microsoft and IBM, under the MIT License, as restored in [lotharsm/MS-DOS](https://github.com/lotharsm/MS-DOS) (branch `dos-4.00-reconstructed`)
- [FreeDOS](https://www.freedos.org/) 1.4, which builds it

## License

This project is licensed under the [MIT License](LICENSE).

You are free to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of this software.
