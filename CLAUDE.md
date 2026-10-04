# Emulator8086 — agent guide

An Intel 8086 PC emulator in Rust: a real-mode CPU interpreter, the PC/XT
devices around it and a BIOS written in NASM. It boots MS-DOS 6.22 and runs
DOOM8088. One binary crate in one configuration (no cargo features): the
machine, and a winit + wgpu window as its only frontend. `README.md` is the
user-facing description (options, controls, what is and is not emulated).

The coding style and conventions are the ones of the sibling project
`bettervoxel_engine` (its `CLAUDE.md`, "Writing conventions" and "Hard
rules"), restated below as far as they apply to a single crate.

## Module map

| path | role |
|---|---|
| `src/main.rs` | the command line and start-up; embeds the BIOS image a command line without a ROM argument runs; declares the crate-level test modules |
| `src/vm/runtime.rs` | the machine: the devices on the port bus (`Runtime::with_devices` IS the port map), the run loop (`step_frame`, `run_slice`), timer pacing, interrupt delivery, reset, the ROM loader |
| `src/vm/instructions/` | the CPU: `process()` in `mod.rs` is the opcode table, one line per opcode; the bodies live in the domain modules beside it (`alu`, `bcd`, `control`, `shift`, `stack`, `string`, `transfer`) |
| `src/vm/modrm.rs` | `decode_rm`, the only effective-address decoder, and the `ModRM` operand widths |
| `src/vm/registers.rs`, `memory.rs`, `cpu.rs` | the register file and the raw-pointer operand wrappers, the 1 MB memory with its ROM protection, the `Cpu` (which also holds the 8259A) |
| `src/utils/number.rs` | `SpecialOps`: the arithmetic the instruction code is written once with for `u8` and `u16` |
| `src/vm/cli.rs` | the monitor's disk commands (swap, new, move, save, eject, put, reset); compiled for tests only until the overlay's disk panel calls them |
| `src/gui/mod.rs` | the winit application: owns the machine and the renderer, runs the guest once per redraw |
| `src/gui/renderer.rs` | wgpu: `FrameBlit` (frame texture and blit pipeline, no surface needed) and `Renderer` (the window's surface) |
| `src/gui/keys.rs` | physical key to XT scancode |
| `src/io/bus.rs` | `IoDevice` and the port bus |
| `src/io/pic.rs`, `pit.rs`, `keyboard.rs`, `dma.rs`, `fdc.rs`, `hdc.rs`, `console.rs` | 8259A, 8253, the XT keyboard on the 8255, the 8237 as registers, two controller stand-ins, debug port E9h |
| `src/io/vga.rs`, `raster.rs`, `font_cp437.rs` | the VGA registers, the CPU-side rasterisers the window uploads, the 8x16 font |
| `src/io/disk/`, `src/io/disk_spec.rs` | disk images, the host side of INT 13h, host-side FAT access; the spellings of drives and images on the command line and in the monitor |
| `assets/rom/` | the BIOS: `bios.asm` plus one `.inc` per interrupt, and the tracked `bios.bin` |
| `build.rs` | reassembles `bios.bin` when a ROM source is newer and patches its checksum |
| `src/bios_tests.rs` | tests that boot the real ROM on a fully wired machine (the `Machine` harness) |
| `src/cpu_tests.rs` | the CPU against 608,000 vectors captured from real hardware (`8086/` submodule) |
| `src/bios_inspection_tests.rs` | static checks of the ROM image |

Every source file carries a `//!` header stating what it owns — skim headers
before reading bodies.

## Finding things

- **An opcode**: one arm of `process()`. A call handles ONE opcode byte; a
  prefix is a call of its own that latches into `vm.prefixes`. The 80186
  opcodes (60-62, 64-6F, C0, C1, C8, C9) are implemented on purpose; BOUND
  is a documented no-op. The dispatcher is the hot path and its speed moves
  by several percent with code placement alone: judge a change to it over
  more than one build, not by a single run.
- **A device**: implements `IoDevice` (byte accesses only — the bus splits a
  word access in two) and is registered with its port range in
  `Runtime::with_devices`.
- **A BIOS service**: `assets/rom/intNN_*.inc`. INT 13h is a trap: the ROM
  writes to port B0h and the host serves the call in `src/io/disk/int13.rs`.
  Port B1h hands the boot order to INT 19h.
- **Guest time**: only `Runtime` turns host time into guest time
  (`sample_host_clock`). Devices get it as 8253 clocks, and the VGA's
  retrace status derives from the same count; the window's cursor blink is
  the one thing drawn on host time. Tests drive time by hand
  (`use_manual_clock`, `credit_pit_time`).
- **Keys**: the window sends `EmulatorEvent`s down a channel to
  `KeyboardController`: the scancode of the physical key (`src/gui/keys.rs`),
  so the layout is the guest's business. F12 is kept by the window.
- **A machine-level test**: `Machine` in `src/bios_tests.rs` (`boot`,
  `run_stub`, `press`, `feed_time`, `screen_text`).

## Build & test

```
cargo test                                    # the fast suite
cargo test --release -- --include-ignored     # everything, the hardware CPU vectors included (git submodule update --init)
cargo clippy --all-targets                    # 0 warnings
cargo doc --no-deps                           # 0 warnings
cargo fmt --all                               # run before every commit
```

There is one configuration: no cargo feature selects anything.

The CPU is validated against real hardware: a change under `src/vm/` runs
the suite with `--include-ignored`. Never judge a suite through a pipe
(`cargo test | tail` hides the exit code): redirect to a log, check the exit
code, then read the log.

`bios.bin` is a tracked build product. With `nasm` on PATH the build script
reassembles it whenever a file in `assets/rom` is newer; commit it together
with the source change that produced it.

The emulator binary opens a window: do not run it from an agent session.
The machine is exercised through the test harness. The wgpu pipeline (frame
upload, whole-number scaling, centring, sRGB) is exercised by
`gui::renderer::tests::blit_draws_the_rasterised_frame_scaled_and_centred`,
which draws offscreen on a real adapter and reads the pixels back; without
an adapter it prints why and passes (`--nocapture` shows the adapter, and
`WGPU_BACKEND=dx12|vulkan|gl` picks the backend). The window itself (the
surface and its presentation, the winit event loop, key delivery) is
compile-checked only and needs the user's eyes.

## Writing conventions

- **Tests**: unit tests live in a sibling file — `src/feature.rs` declares
  `#[cfg(test)] #[path = "feature_tests.rs"] mod tests;` and the module body
  lives in `src/feature_tests.rs` (for a directory module: `#[cfg(test)] mod
  tests;` → `dir/tests.rs`). Keep only trivial (<~60 line) test modules
  inline. A test's doc comment states the rule the test pins, in the present
  tense.
- **File size**: keep source files under ~800 lines; split by domain into a
  directory module (`feature/mod.rs` + children, `pub(super)` for cross-file
  items) once they outgrow that. Test files and single data tables are exempt.
- Every file starts with a `//!` header: what the module owns, and the one
  invariant a reader must not violate.
- Comments state constraints the code can't show — never history, review or
  phase tags, or "what the next line does". Hardware facts are constraints:
  the mnemonic on an opcode arm, what a register bit means, what the
  datasheet prescribes.
- Every `unsafe` block carries a `// SAFETY:` comment that is true at that
  site.
- A deliberate deviation from the hardware is stated where it is implemented.

## Shared agent knowledge

The repo is the only memory shared between agents and sessions: a convention
or ruling both need lives in this file. Machine-specific facts (absolute
paths, local build dirs) go in `CLAUDE.local.md` (gitignored), never here.

## Hard rules

- **Ask before adding ANY new library** (dev-dependencies included). What
  was asked and answered is in the dependency ledger below: read it first.
- **Never push.** The user pushes. Never `git add -A`; commit with explicit
  pathspecs (`git commit -m … -- <paths>`) — the working tree holds
  user-local files (disk images, guest software) that must not be swept into
  a commit.
- Conventional commits (`feat`/`fix`/`refactor`/`docs`/`test`/…, `!` for
  breaking), no co-author lines, one reviewable slice per commit, tests land
  WITH the change.
- The crate is clippy-clean and stays that way.

## Dependency ledger

The record of what the user was asked about libraries and what they
answered, so that a later session neither asks again nor takes more than was
granted. An approval covers the named crates for the named scope. Anything
not listed has not been asked: ask first, and add the answer here.

| date | crate or tool | answer | scope |
|---|---|---|---|
| 2026-10-04 | `wgpu` 24 → 30, `winit` 0.30.13 | approved | the window; the versions `egui-wgpu` 0.36 requires |
| 2026-10-04 | `egui` 0.36, `egui-wgpu`, `egui-winit` | approved | the overlay in the window |
| 2026-10-04 | `rfd` 0.17 | approved | file dialogs, native and in the browser |
| 2026-10-04 | `wasm-bindgen`, `wasm-bindgen-futures`, `web-sys`, `js-sys`, `web-time` | approved | the browser (wasm32) build |
| 2026-10-04 | `trunk` (`cargo install trunk`) | approved | build tool for the browser build, not a dependency of the crate |
| 2026-10-04 | `console_error_panic_hook`, `console_log` | refused | the browser build's panic hook and console logger are written by hand |

Approved does not mean added: `Cargo.toml` says what is in use. `crossterm`
left with the terminal frontend on the same date.

When a version of the GPU stack changes, check that `Cargo.lock` holds one
`windows` crate. `gpu-allocator` accepts a range of them and cargo leaves it
on the version already locked, while `wgpu-hal` needs its own: with two,
`wgpu-hal` does not compile on Windows. `cargo update -p windows@<old>
--precise <the one wgpu-hal uses>` brings them together.
