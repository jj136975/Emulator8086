# Emulator8086 — agent guide

An Intel 8086 PC emulator in Rust: a real-mode CPU interpreter, the PC/XT
devices around it and a BIOS written in NASM. It boots MS-DOS 6.22 and runs
DOOM8088. One Cargo workspace of two crates in one configuration (no cargo
features): the machine, and the app around it with a winit + wgpu window as
its only frontend and an egui overlay of tools over the picture. The app
builds for two hosts, a desktop (the native targets) and a browser
(wasm32-unknown-unknown, built with trunk); what differs between them is in
`src/platform` and nowhere else. `README.md` is the user-facing description
(options, controls, the web build, what is and is not emulated).

The coding style and conventions are the ones of the sibling project
`bettervoxel_engine` (its `CLAUDE.md`, "Writing conventions" and "Hard
rules"), restated below as far as they apply here.

## Workspace map

| crate | role |
|---|---|
| `crates/emu8086_core` | the machine: the CPU, the devices, the disks, the BIOS ROM it embeds (`BIOS_ROM`) with the build script that assembles it, and every test of the machine. A library with no window, no GPU and no clock: it builds for wasm32-unknown-unknown on its own |
| root package `emulator_8086` | the app: the start-up, the winit window, the wgpu renderer, the key map, the overlay of tools (egui), and what depends on the host (`src/platform`). It owns the machine, measures host time for it and is its wall clock. `cargo run` at the root runs it on the desktop; `trunk serve` at the root builds it for wasm32 and serves it in `index.html` |

The root `Cargo.toml` is the version authority for every dependency
(`[workspace.dependencies]`; a member only adds features, as the app adds
`chrono` to fatfs), and holds `[workspace.lints]`, the edition and the
profiles. `assets/rom/` (the BIOS sources and image) and `8086/` (the CPU
vectors, a submodule) stay at the root of the repository; the core reaches
them from its own directory. `index.html` at the root is the page trunk
builds the browser build into; `dist/`, where trunk puts the result, is
ignored.

The app, in `src/`:

| path | role |
|---|---|
| `main.rs` | start-up: `Options`, what a start asks of the machine (the host says what: `platform::options`); builds the machine they describe and loads its ROM, the image they name or the core's `BIOS_ROM`; tells the user what opening the drives did (`platform::notice`) |
| `platform/mod.rs` | THE names for what depends on the host, each defined by both sides with one meaning (the list is in its header), and the types they share: `OverlaySwitch`, and for files `FileAccess`, `FileQuestion`, `Wanted` |
| `platform/native.rs` | the desktop side: the command line (`Cli`; its doc comments are the `--help` text), env_logger, the blocking event loop, the sleep, the files, the system's file dialogs (rfd's, each waited for on a thread of its own), egui-winit |
| `platform/web.rs` | the browser side: the console logger and the panic hook, the spawned event loop, the canvas in the page, the file picker (rfd's), the download of a text or a disk, the page's `Date` |
| `platform/egui_events.rs`, `fat_stamp.rs` | the parts of the browser side that know nothing of a browser, built on the desktop for their tests alone: winit's events as egui's input (egui-winit does not build for wasm32), and a `Date` as a FAT time stamp |
| `platform/tests.rs` | the rule of `platform` as a test: a sweep of the app's sources |
| `wall_clock.rs` | `HostClock`, the wall clock the machine's FAT code is handed: the host's local time, as `platform::fat_options` carries it |
| `gui/mod.rs` | the winit application, and all that needs the window: hands its events to egui and to the session, works the overlay's switch, takes the renderer when its start-up is over, runs and draws a frame per redraw |
| `gui/session.rs` | `Session`: the machine, the pacer, the meter, the guest's keyboard, the disk tools and the overlay, without a window. Decides whose a key is, and carries out the overlay's `Command`s on the machine; times the overlay's pass and passes the window's `Drawn` on to the meter |
| `gui/disk_tools.rs` | `DiskTools`, the session's side of the Disks panel: a `DiskCommand` becomes an operation of the core (`io::disk_ops`), carried out at once or once the host's file dialog has answered (`poll`, at the start of every frame); `FileHost`, the host as the tools reach it: `platform`, or a test's hand-made answers |
| `gui/pacer.rs` | real-time pacing: `Pacer` reads the host clock, credits the machine with the time that passed and runs it 256 instructions at a time; the 1 ms wait of an idle guest; pause; what each slice did with its time (`Slice`) |
| `gui/meter.rs` | measuring the emulation: `Meter`, told of each frame by the session (the pacer's `Slice`, the machine's `Counts`) and of what the frame spent outside its slice (the overlay's pass, the window's `Drawn`), averages over about a second the guest's speed (with the last minute of it), the frames and how their host time divides into five (running, idle, the overlay, the drawing, the wait for the display), the guest's time (its clock, the time owed and dropped, the share spent halted) and the interrupts per line. It reads no clock |
| `gui/renderer.rs` | wgpu: `FrameBlit` (frame texture and blit pipeline), `OverlayPass` (egui's meshes), `record_frame` (both in one render pass, no surface needed) and `Renderer` (the window's surface), whose `render` returns a `Drawn`: how long the surface kept it waiting for a frame to draw on, and how long the rest took |
| `gui/keys.rs` | physical key to XT scancode; `GuestKeys` sends the codes and remembers which keys the guest holds |
| `gui/overlay/mod.rs` | the overlay's shell: shown or hidden, the hint, the menu bar, the panels' windows (each the size of its contents, first opened a cascade step from the one before it); `Command`, what a tool asks the app to do; `Cx`, what a panel works on |
| `gui/overlay/panels.rs` | THE panel table: `Panel`, `ALL`, `title`, the arm of each body |
| `gui/overlay/about.rs` | the About panel; every panel has a file like it |
| `gui/overlay/disks.rs` | the Disks panel: every drive with its disk (`image_place`, `disk_size`, which the About panel shares), the buttons that ask the app for a `DiskCommand` on the drive chosen, what the host cannot do left out (`platform::FILE_ACCESS`), and a reboot that is due shown with the reset |
| `gui/overlay/stats.rs` | the Stats panel: whether the guest keeps up with real time and where the host's time goes (five parts of each frame, as a bar and as rows), from the `Meter` the shell is lent (`Cx::meter`); the chart of the last minute, drawn with egui's painter; and the guest's speed at the right end of the menu bar (`readout`) |
| `gui/overlay/memory.rs` | the Memory panel: a hex view of guest memory, the address field (`parse_address`), the jumps to where the register pairs point and to the landmarks of the first megabyte |
| `gui/overlay/pic.rs`, `pit.rs`, `vga.rs`, `keyboard.rs`, `debug_port.rs` | the device panels: the 8259A, the 8253, the VGA and its DAC, the 8255's keyboard interface, the port E9h log |
| `gui/overlay/messages.rs` | where a tool says what it did or why it failed |
| `gui/overlay/theme.rs` | the palette and the sizes as named slots, and their one application to egui's style |
| `gui/overlay/testing.rs` | test support: drives the overlay frame by frame with hand-made input and reads the screen as egui describes it to a screen reader; `Shell`, the shell alone over a machine; `waiting_for_a_key`, a machine the BIOS booted |

The core, in `crates/emu8086_core/`:

| path | role |
|---|---|
| `src/lib.rs` | the two rules of the crate, its public modules, `BIOS_ROM`, the crate-level test modules |
| `src/vm/runtime.rs` | the machine, built from what the command line names (`Runtime::new`) or around drives opened already (`with_drives`): the devices on the port bus (`Runtime::with_devices` IS the port map), the run loop (`run`), guest time (`credit_pit_time` and the debt owed to the 8253), interrupt delivery, reset, the ROM loader, the loans of the devices the app reads (`pit`, `keyboard`, `debug_console`), and the account of the guest's time it keeps for whoever measures it (`guest_time`: timer clocks handed over, of them while halted, owed, dropped) |
| `src/vm/instructions/` | the CPU: `process()` in `mod.rs` is the opcode table, one line per opcode; the bodies live in the domain modules beside it (`alu`, `bcd`, `control`, `shift`, `stack`, `string`, `transfer`) |
| `src/vm/modrm.rs` | `decode_rm`, the only effective-address decoder, and the `ModRM` operand widths |
| `src/vm/registers.rs`, `memory.rs`, `cpu.rs` | the register file and the raw-pointer operand wrappers, the 1 MB memory with its ROM protection, the `Cpu` (which also holds the 8259A) |
| `src/utils/number.rs` | `SpecialOps`: the arithmetic the instruction code is written once with for `u8` and `u16` |
| `src/io/bus.rs` | `IoDevice` and the port bus |
| `src/io/pic.rs`, `pit.rs`, `keyboard.rs`, `dma.rs`, `fdc.rs`, `hdc.rs`, `console.rs` | 8259A, 8253, the XT keyboard on the 8255, the 8237 as registers, two controller stand-ins, debug port E9h (which keeps its last lines). The first three and the console describe themselves to a reader without a port access: `Pic::view`, `Pit::channels`, `KeyboardController::view`, `DebugConsole::lines`; `Pic::acknowledged` tallies the interrupts acknowledged on each line |
| `src/io/vga.rs`, `raster.rs`, `font_cp437.rs` | the VGA registers (`VgaDevice::registers` and `dac_colour` for a reader), the CPU-side rasterisers the window uploads, the 8x16 font |
| `src/io/disk/`, `src/io/disk_spec.rs` | disk images (in a host file, or in memory, made of a host's bytes and given back as them), the controller with the change lines and the reboot due after a hard disk changed (`reboot_due`), the host side of INT 13h, host-side FAT access and the `WallClock` its callers pass; the spellings of drives and images on the command line, and `open_drives`, which opens what the command line names and returns what the user is to be told of it (`OpenedDrives`) |
| `src/io/disk_ops.rs` | the disk tools' operations (`Operation`: insert, create, move, save, eject, put; `apply`; `contents`, a disk as its bytes), each answering with an `Outcome`: words for the user, and whether the guest must be rebooted. A file comes as a `HostFile`, by its path or by its name and contents |
| `build.rs` | reassembles `assets/rom/bios.bin` when a ROM source is newer and patches its checksum |
| `src/bios_tests.rs` | tests that boot the real ROM on a fully wired machine (the `Machine` harness) |
| `src/cpu_tests.rs` | the CPU against 608,000 vectors captured from real hardware (`8086/` submodule) |
| `src/bios_inspection_tests.rs` | static checks of the ROM image |
| `src/crate_rules_tests.rs` | the two rules of the crate as tests: no clock in its sources, no dependency but the listed ones |

`assets/rom/` holds the BIOS: `bios.asm` plus one `.inc` per interrupt, and
the tracked `bios.bin`.

Every source file carries a `//!` header stating what it owns — skim headers
before reading bodies.

The core's public surface is what the app reaches. A module of `io` or `vm`
is `pub` when the app reaches into it, by name or through a public field of
the `Runtime`, and `pub(crate)` otherwise; an item is widened from
`pub(crate)` when the app needs it, not before. `Runtime::cpu` is public for
reading: whoever writes to it keeps the rule of `vm/mod.rs` (the `Memory`
box is never replaced).

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
  writes to port B0h and the host serves the call in the core's
  `src/io/disk/int13.rs`. Port B1h hands the boot order to INT 19h.
- **Guest time**: the machine is told the time, it never reads a clock.
  `Runtime::credit_pit_time` turns host time into the 8253 clocks the guest
  is owed (the debt, its 250 ms cap) and `Runtime::run` pays them out, four
  per instruction; the VGA's retrace status derives from the same count.
  The one reader of the host clock for the machine is the app's `Pacer`
  (`src/gui/pacer.rs`): it samples every 256 instructions while the guest
  runs and at once when it halts. A paused guest (`Pacer::set_paused`) is
  not run, and the time it stands still is credited to nobody. The pacer's
  clock and its wait are the host's (`platform::Instant`, `platform::wait`):
  a desktop sleeps a millisecond at a time; a browser cannot wait, so there
  the slice of a halted or paused guest ends at once and the page's next
  frame comes back to it, the time between the two credited by the next
  slice like any other. The window's cursor blink is the one thing drawn on
  host time, and the overlay's only time is that of egui's input. A test
  states the time: `credit_pit_time`, then `step_frame`.
- **A measurement of the emulation**: `Meter` (`src/gui/meter.rs`). The
  session tells it of every frame, shown or hidden, after the slice: the
  pacer's `Slice` (when it began and ended, what it waited) and `Counts`,
  what the machine counted since power-on (`instruction_count`,
  `Pic::acknowledged`, `Runtime::guest_time`, channel 0's period). What a
  frame spent outside its slice is told too, and falls into the frame the
  next slice ends: the overlay's pass, which the session times in
  `run_overlay` (`Meter::overlay_pass`), and what the window drew, which
  `Renderer::render` reports as a `Drawn` (how long the surface kept it
  waiting for a frame to draw on, and how long the rest took) and the
  window hands to `Session::drawn`. The meter reads no clock: a test tells
  it a history by hand. A figure is a ratio of two counts over the same
  span, the frames' over a second of host time, the guest's over a second
  of its running (a paused guest has none). A frame's host time divides
  into five parts: its running, its idling, the overlay, the drawing and
  the waiting for the display. The waiting is the measured wait with
  whatever nothing measured (in a browser, the wait for the next animation
  frame), so the five add up to the frame's time. What a measurement needs
  of the machine is a read-only count there, never a read on the hot path
  beyond an increment; the Stats panel shows only what the meter averaged.
- **The wall clock**: `io::disk::WallClock`, an argument of whatever formats
  a disk or copies a file onto one. The app passes `HostClock`
  (`src/wall_clock.rs`), a test `FixedClock`. How a host tells its local
  time is `platform::fat_options`: fatfs's own chrono clock on a desktop,
  the page's `Date` in a browser (`platform/fat_stamp.rs`).
- **What depends on the host**: a name in the list of `src/platform/mod.rs`,
  defined in `native.rs` and in `web.rs` with the same signature and the
  same meaning, and called from the rest of the app without a `cfg`. Logic
  of the browser side that needs no browser goes in a file of its own,
  built under `cfg(any(target_arch = "wasm32", test))` and tested on the
  desktop (`egui_events.rs`, `fat_stamp.rs`); where the two hosts differ in
  what a caller does next, the name says so in what it returns
  (`platform::wait`, `platform::FILE_ACCESS`) and a test puts the other
  host's answer in its place (`Pacer::waiting_with`, `DiskTools::on`).
- **Keys**: the window sends `EmulatorEvent`s down a channel to
  `KeyboardController`: the scancode of the physical key (`src/gui/keys.rs`),
  so the layout is the guest's business. The overlay's switch is the
  host's (`platform::OVERLAY_SWITCH`). On a desktop it is F12, which the
  window keeps: neither egui nor the guest sees it. In a browser F12 stays
  the browser's key (the developer tools; `platform/web.rs` keeps F11 and
  F12, which no XT keyboard has, from the canvas), and the switch is the
  secondary button of the pointer: the press is offered to egui first and
  works the switch only on the picture (`Session::overlay_button`). Every
  other event of the window goes to egui first. A key then goes on to the
  guest unless egui kept it (Tab) or the overlay has the keyboard: one of
  its widgets has the focus or one of its menus is open
  (`Overlay::wants_keyboard`, `Session::key`). The guest gets the release
  of a key if and only if it got its press: when the overlay takes the
  keyboard, or the window loses it, the keys the guest holds are let go
  (`GuestKeys::release_all`).
- **An overlay panel**: ONE row in THE panel table
  (`src/gui/overlay/panels.rs`: a variant of `Panel`, its place in `ALL`,
  its arm in `title` and in `body`) plus one file for the body,
  `pub(super) fn body(ui: &mut egui::Ui, cx: &mut Cx<'_>)`. The checkbox in
  the View menu, the window and its close button follow from `ALL`. The
  body has the machine as `cx.runtime` for the length of the pass, asks the
  app for what only the app can do through `cx.commands`, and says what it
  did or why it failed through `cx.messages`. The window takes the size of
  the body: a horizontal separator would stretch it across the screen, so
  panels part their sections with a gap. A panel's own state lives in
  egui's memory (`ui.data_mut`), as the Memory panel's view does.
- **A device panel** (8259A, 8253, VGA, keyboard, port E9h): reads the
  device's description, never its ports, since a port read is seen by the
  guest (a poll answered, a flip-flop reset, a DAC index moved):
  `cpu.pic.view()`, `pit().channels()`, `vga` with `registers()` and
  `dac_colour()`, `keyboard().view()`, `debug_console().lines()`. What
  the core does not describe yet is added as a read-only view of the
  device, never by making its internals public. A panel that shows many
  rows (the memory dump, the log) lays out only those on the screen
  (`ScrollArea::show_rows`).
- **A machine action of the overlay**: a variant of `Command`
  (`src/gui/overlay/mod.rs`), its entry in the menu bar, and its arm in
  `Session::execute` (`src/gui/session.rs`), which reports through
  `Overlay::messages`. What touches the host goes from the session to
  `platform` (the VGA dump: `platform::save_text`, a file on a desktop and a
  download in a browser; the disk tools' dialogs), never from the overlay.
- **A disk tool**: an `Operation` of the core's `io::disk_ops` (what it
  does to the drives, the rules it keeps, its words for the user and
  whether the guest must be rebooted, in its `Outcome`), a `DiskCommand`
  the Disks panel asks for (`src/gui/overlay/disks.rs`), and its arm in
  `DiskTools::execute` (`src/gui/disk_tools.rs`), which carries it out at
  once or asks the host for a file first. A file dialog never blocks:
  `platform::FileDialogs::ask` returns a channel at once, a desktop waits
  for the dialog on a thread of its own and a browser in a task of the
  page, and the session takes up the answers at the start of every frame,
  shown or hidden (`DiskTools::poll`). On a desktop a file comes by its
  path and an image stays in its file; in a browser a file comes as its
  name and bytes (`HostFile::Contents`), an image lives in memory and a
  disk goes back as a download (`disk_ops::contents`,
  `platform::save_bytes`). `platform::FILE_ACCESS` says which, and the
  panel offers nothing the host cannot do. A floppy changed from the host
  has its change line raised; a hard disk leaves
  `DiskController::reboot_due` set until the guest's bootstrap runs again
  (INT 19h writes port B1h), and the panel shows it with the reset. A
  test answers the dialogs by hand (`FileHost`, `disk_tools_tests.rs`).
- **A colour of the overlay**: a slot of `PALETTE` in
  `src/gui/overlay/theme.rs`, a size a slot of `METRICS`. No other file
  under `src/gui/overlay/` spells a colour.
- **A machine-level test**: `Machine` in the core's `src/bios_tests.rs`
  (`boot`, `run_stub`, `press`, `feed_time`, `screen_text`). A test of the
  app builds its machine with `Runtime::new`: the core's `#[cfg(test)]`
  helpers do not exist outside the core.
- **A test of the overlay**: `Driver` in `src/gui/overlay/testing.rs` runs
  the shell (`Shell`, for `overlay::tests` and each panel's tests) or the
  whole session (`session::tests`) frame by frame with hand-made input, and
  `Screen` is what a pass showed: a control is found by its role and its
  label and clicked with the pointer, as a user would (`click`, `fill` for
  a text field, `frame`, `wait`, `screen().shows(..)`, `screen().row(..)`
  for a row of a table). `Shell::with_panel` opens a panel over a machine;
  `waiting_for_a_key` is one the BIOS booted, the same on every run. No
  window, no GPU; the time is the one the test states.

## Build & test

```
cargo test --workspace                                        # the fast suite
cargo test --workspace --release -- --include-ignored         # everything, the hardware CPU vectors included (git submodule update --init)
cargo clippy --workspace --all-targets                        # 0 warnings
cargo doc --workspace --no-deps                               # 0 warnings
cargo check -p emu8086_core --target wasm32-unknown-unknown   # the core builds for the browser on its own, 0 warnings
cargo clippy --workspace --target wasm32-unknown-unknown      # the browser build, 0 warnings (no --all-targets: the tests are the desktop's)
trunk build                                                   # the browser build links and binds, into dist/
cargo fmt --all                                               # run before every commit
```

Without `--workspace` cargo acts on the root package alone: that is what
makes `cargo run --release -- ...` run the app, and it makes a bare
`cargo test` skip every test of the machine. `-p emu8086_core` runs the
core's alone. There is one configuration: no cargo feature selects anything.
What selects is the target: the dependencies only one host has stand in the
target tables of the root manifest.

The browser build is trunk's (0.21, `cargo install trunk`): `trunk serve`
builds it, serves it on http://127.0.0.1:8080 and rebuilds on change, and
`--release` gives the optimised build. Trunk fetches the wasm-bindgen CLI of
the version locked in `Cargo.lock`, and wasm-opt for a release build, on
first use.

The CPU is validated against real hardware: a change under the core's
`src/vm/` runs the suite with `--include-ignored`. Never judge a suite
through a pipe (`cargo test | tail` hides the exit code): redirect to a log,
check the exit code, then read the log.

`bios.bin` is a tracked build product. With `nasm` on PATH the core's build
script reassembles it whenever a file in `assets/rom` is newer; commit it
together with the source change that produced it.

The emulator binary opens a window: do not run it from an agent session.
The machine is exercised through the test harness. The wgpu pipeline (frame
upload, whole-number scaling, centring, sRGB) is exercised by the app's
`gui::renderer::tests::blit_draws_the_rasterised_frame_scaled_and_centred`,
and the overlay drawn over the picture in the same pass by
`the_overlay_is_drawn_over_the_picture_and_only_where_it_is` beside it:
both record frames with `record_frame`, the function the window calls, draw
offscreen on a real adapter and read the pixels back; without an adapter
they print why and pass (`--nocapture` shows the adapter, and
`WGPU_BACKEND=dx12|vulkan|gl` picks the backend). The overlay's logic and
the session's (the menus, the commands, pause, whose a key is) run without
a window on hand-made input (`gui::overlay::tests`, `gui::session::tests`),
and the disk tools on a host whose dialogs the test answers
(`gui::disk_tools::tests`). The pacer runs on the host's real clock in
`gui::pacer::tests`. The window itself is compile-checked only and needs
the user's eyes: the surface and its presentation, the winit event loop,
`gui/mod.rs`, which is where the window's events become egui's input and
where the overlay's switch is picked out, and the system's file dialogs
(`platform::FileDialogs::ask` on a desktop; the thread their answers come
from is tested, `answer_later`).

Of the browser build the suite covers, on the desktop, what needs no
browser: the pacer on a host that cannot wait (`gui::pacer::tests`), the
window's events as egui's input (`platform::egui_events::tests`), the FAT
stamp of a date (`platform::fat_stamp::tests`), the overlay's button
(`gui::session::tests`), the disk tools and the Disks panel on a host that
hands files over as their contents (`gui::disk_tools::tests`,
`gui::overlay::disks::tests`), and the frame drawn on a device that has no
more than WebGL2 guarantees
(`gui::renderer::tests::the_frame_is_drawn_within_the_limits_of_webgl2`;
with `WGPU_BACKEND=gl` it runs on the backend WebGL2 shares its shader
translation with). `platform/web.rs` and the page are compile-checked
only, the file picker and the download among them.
The page is looked at through the Playwright MCP server alone (tools named
`mcp__playwright__*`), never through another browser: serve it with
`trunk serve --address 0.0.0.0`, and the server's browser, which runs in a
container, reaches it at `http://host.docker.internal:8080`. That browser
has no GPU, so what it shows is the WebGL2 path; WebGPU needs the user's
own browser. Without those tools, say that the page was not looked at.

Known trap of the browser build: `DiskImage::save_to_file` in the core
names its temporary file with `std::process::id()`, which panics on wasm32.
A browser never reaches it: `disk_ops::Operation::Save` takes a path, which
a browser's file picker never gives, and where files come as their
contents the disk tools take a disk back as its bytes
(`disk_ops::contents`, `DiskImage::contents`) and hand those to
`platform::save_bytes`, a download
(`gui::disk_tools::tests::a_disk_is_handed_over_as_its_contents_where_files_have_no_paths`).
Whatever else saves a disk in a browser goes the same way.

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

- **The core never reads a clock.** Nothing in `crates/emu8086_core` names
  `Instant` or `SystemTime`, sleeps, or lets a dependency read the time for
  it (fatfs comes without its `chrono` feature there): the machine has to
  run where there is no clock to read (a browser), and its tests have to
  give the same result on every run. The app measures host time and tells
  the machine (`Runtime::credit_pit_time`), and it passes its wall clock to
  what formats or writes a disk (`io::disk::WallClock`). The core has no
  `cfg(target_arch)` either: what differs between hosts is the app's.
- **Nothing that opens a window or a GPU is a dependency of the core**: no
  winit, wgpu, egui, rfd, pollster, bytemuck, clap or env_logger. Its
  dependencies are the ones `crate_rules_tests` lists.
- Both rules are tests (`crate_rules_tests` in the core). The wasm32 check
  alone does not hold the first: a clock read compiles for that target and
  panics when it runs.
- **Only `src/platform` knows the host.** Outside that directory no source
  file of the app names `cfg(target_arch` (or `target_os`,
  `target_family`), `wasm_bindgen`, `web_sys` or `js_sys`, and none sleeps
  (`thread::sleep`), blocks on a future (`block_on`) or names a clock of
  `std::time` (`Instant`, `SystemTime`, `UNIX_EPOCH`): a browser has one
  thread, which must return to the page, and no clock std can read. What
  depends on the host is a name of `platform` that both sides define; the
  clocks are `platform::Instant` and `platform::SystemTime`. The rule is a
  test: `platform::tests::only_platform_knows_the_host` sweeps `src/`. A
  test source may sleep and block, since the tests run on the desktop
  alone; it may not ask for the host. Nothing blocks in `platform/web.rs`
  either: what takes time there is a task or a callback of the page.
- **The overlay has no thread, and nothing in it blocks or reads a
  clock.** It has the machine as `&mut Runtime` for the length of its pass,
  on the thread that draws: there is no channel to a machine elsewhere.
  Its time is the time of egui's input, never `std::time::Instant`, and it
  writes no file itself: what needs the host is a `Command` the session
  carries out. The overlay has to run in a browser, which has neither a
  second thread nor a file system to offer.
- **Only `src/gui/overlay/theme.rs` spells a colour of the overlay.** The
  shell and the panels name a slot of `PALETTE`, so that a change of the
  theme reaches all they draw. The rule is a test:
  `theme::tests::only_the_theme_spells_a_colour` sweeps the sources under
  `src/gui/overlay/` for a colour built from literals or a named constant
  of `Color32`, `Rgba` or `Hsva`.
- **Ask before adding ANY new library** (dev-dependencies included). What
  was asked and answered is in the dependency ledger below: read it first.
- **Never push.** The user pushes. Never `git add -A`; commit with explicit
  pathspecs (`git commit -m … -- <paths>`) — the working tree holds
  user-local files (disk images, guest software) that must not be swept into
  a commit.
- Conventional commits (`feat`/`fix`/`refactor`/`docs`/`test`/…, `!` for
  breaking), no co-author lines, one reviewable slice per commit, tests land
  WITH the change.
- The workspace is clippy-clean and stays that way.

## Dependency ledger

The record of what the user was asked about libraries and what they
answered, so that a later session neither asks again nor takes more than was
granted. An approval covers the named crates for the named scope. Anything
not listed has not been asked: ask first, and add the answer here.

| date | crate or tool | answer | scope |
|---|---|---|---|
| 2026-10-04 | `wgpu` 24 → 30, `winit` 0.30.13 | approved | the window; the versions `egui-wgpu` 0.36 requires |
| 2026-10-04 | `egui` 0.36, `egui-wgpu`, `egui-winit` | approved | the overlay in the window. In use, in the app only, with egui's `default_fonts` and no other feature: no system clipboard (`arboard`), no screen-reader adapter (`accesskit_winit`), no opening of links (`webbrowser`), none of which a browser has. `egui-winit` is a dependency of the native targets only: 0.36.2 does not build for wasm32 (see below) |
| 2026-10-04 | `rfd` 0.17 | approved | file dialogs, native and in the browser. In use, in the app on both hosts (`src/platform`): the system's dialogs on a desktop, the file picker in a browser. With its default features, which only Linux and the BSDs build (the desktop portal's dialogs, parented to a Wayland window too). In a browser it brings `web-sys` features of its own (the file input and its reader, the user's activation), which cargo adds to those the root manifest lists |
| 2026-10-04 | `wasm-bindgen`, `wasm-bindgen-futures`, `web-sys`, `js-sys`, `web-time` | approved | the browser (wasm32) build. In use: `web-time` in the app on both hosts (the clocks of `src/platform`), the other four as dependencies of the wasm32 target only, `web-sys` with the features the root manifest lists and no other (beside those `rfd` asks for itself) |
| 2026-10-04 | `trunk` (`cargo install trunk`) | approved | build tool for the browser build, not a dependency of the crate. In use: 0.21, with `index.html` at the root |
| 2026-10-04 | `console_error_panic_hook`, `console_log` | refused | the browser build's panic hook and console logger are written by hand: they are in `src/platform/web.rs` |

Approved does not mean added: the manifests say what is in use, the root
one at which version and each crate's own which of them it takes.
`crossterm` left with the terminal frontend on the same date. clap,
env_logger and pollster are dependencies of the native targets only, and
wgpu takes its `webgl` feature for wasm32 only (WebGPU is among its default
features). chrono is a direct dependency of neither crate: on a desktop the
app gets the local time through fatfs's `chrono` feature, which it turns on
for the native targets only; in a browser the time is the page's `Date`.

egui-winit 0.36.2 does not compile for wasm32: its `NativeFile` implements
`egui::DroppedFile::bytes`, which egui declares only off wasm32 (the trait
has `bytes_async` there), and upstream's main branch was the same on
2026-10-04. In a browser the app therefore turns winit's events into egui's
input itself (`src/platform/egui_events.rs`: pointer, wheel, keys, text,
modifiers, focus, cursor; no touch, IME, dropped files or clipboard). Once
an egui-winit builds for wasm32, that file can go and `OverlayInput` can be
egui-winit's state on both hosts. A patched copy of egui-winit in the tree
was not taken: it has not been asked.

egui depends on the `accesskit` crate (the data types a screen reader is
described the screen with) whatever its features: it is in the lock file
because egui brings it, and the overlay's tests read the screen through it
(`egui::accesskit`, no dependency of its own).

When a version of the GPU stack changes, check that `Cargo.lock` holds one
`windows` crate. `gpu-allocator` accepts a range of them and cargo leaves it
on the version already locked, while `wgpu-hal` needs its own: with two,
`wgpu-hal` does not compile on Windows. `cargo update -p windows@<old>
--precise <the one wgpu-hal uses>` brings them together.
