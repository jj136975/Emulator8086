# Emulator8086 — agent guide

An Intel 8086 PC emulator in Rust: a real-mode CPU interpreter, the PC/XT
devices around it and a BIOS written in NASM. It boots MS-DOS 6.22 and runs
DOOM8088. One Cargo workspace of three crates in one configuration (no cargo
features): the machine; the app around it, with a winit + wgpu window as its
only frontend and an egui overlay of tools over the picture; and a tool that
builds MS-DOS 4.00 from its source inside the machine, for the disks the
site publishes. The app builds for two hosts, a desktop (the native
targets) and a browser (wasm32-unknown-unknown, built with trunk); what
differs between them is in `src/platform` and nowhere else. `README.md` is
the user-facing description (options, controls, the web build, the site's
disks, what is and is not emulated).

The coding style and conventions are the ones of the sibling project
`bettervoxel_engine` (its `CLAUDE.md`, "Writing conventions" and "Hard
rules"), restated below as far as they apply here.

## Workspace map

| crate | role |
|---|---|
| `crates/emu8086_core` | the machine: the CPU, the devices, the disks, the BIOS ROM it embeds (`BIOS_ROM`) with the build script that assembles it, and every test of the machine. A library with no window, no GPU and no clock: it builds for wasm32-unknown-unknown on its own |
| root package `emulator_8086` | the app: the start-up, the winit window, the wgpu renderer, the key map, the overlay of tools (egui), and what depends on the host (`src/platform`). It owns the machine, measures host time for it and is its wall clock. `cargo run` at the root runs it on the desktop; `trunk serve` at the root builds it for wasm32 and serves it in `index.html` |
| `crates/dos_images` | the tool that makes the site's two disks: FreeDOS 1.4 builds MS-DOS 4.00 from its source inside the machine, and the tool makes an MS-DOS 4.00 floppy and a hard disk that starts Doom8088 of what it built, boots each and reports. A headless binary of the core and fatfs alone, which the Pages workflow runs, and a developer by hand; neither the app nor a browser has it. Its inputs are fetched by `fetch-inputs.sh`, checked against `pins.env` |

The root `Cargo.toml` is the version authority for every dependency
(`[workspace.dependencies]`; a member only adds features, as the app adds
`chrono` to fatfs), and holds `[workspace.lints]`, the edition and the
profiles. `assets/rom/` (the BIOS sources and image) and `8086/` (the CPU
vectors, a submodule) stay at the root of the repository; the core reaches
them from its own directory. `index.html` at the root is the page trunk
builds the browser build into, and has trunk copy the BIOS beside it as
`rom/bios.bin`; `dist/`, where trunk puts the result, is ignored.
`.github/workflows/pages.yml` assembles the BIOS, makes the site's disks
with `crates/dos_images`, and publishes the browser build with them on
GitHub Pages ("Build & test"). `.gitattributes` keeps what bash and
sha256sum read in `crates/dos_images` (the script, the pins, the lists of
hashes) in LF line ends in every checkout: a Windows checkout gives other
text files CR LF.

The app, in `src/`:

| path | role |
|---|---|
| `main.rs` | start-up: `Options`, what a start asks of the machine (the host says what: `platform::options`); builds the machine they describe and loads its ROM, the image they name or the core's `BIOS_ROM`; tells the user what opening the drives did (`platform::notice`) |
| `platform/mod.rs` | THE names for what depends on the host, each defined by both sides with one meaning (the list is in its header), and the types they share: `OverlaySwitch`, for files `FileAccess`, `FileQuestion`, `Wanted`, and for the site's `Fetched` |
| `platform/native.rs` | the desktop side: the command line (`Cli`; its doc comments are the `--help` text), env_logger, the blocking event loop, the sleep, the files, the system's file dialogs (rfd's, each waited for on a thread of its own), egui-winit, and no site (`HAS_SITE` is false, and `fetch` says so at once) |
| `platform/web.rs` | the browser side: the console logger and the panic hook, the spawned event loop, the canvas in the page, the file picker (rfd's), the download of a text or a disk, the fetch of a file of the site (relative to the page, in a task of the page), the page's `Date` |
| `platform/egui_events.rs`, `fat_stamp.rs`, `site_answer.rs` | the parts of the browser side that know nothing of a browser, built on the desktop for their tests alone: winit's events as egui's input (egui-winit does not build for wasm32), a `Date` as a FAT time stamp, and what a site's answer to a fetch says of the file (`verdict`: a 404, or a page in the place of a file, is "The site has no ...") |
| `platform/tests.rs` | the rule of `platform` as a test: a sweep of the app's sources |
| `wall_clock.rs` | `HostClock`, the wall clock the machine's FAT code is handed: the host's local time, as `platform::fat_options` carries it |
| `site.rs` | THE table of the files the site publishes beside the page (`FILES`: each one's path relative to the page, its `Kind`, its title, and for a disk the verb of its button in the Start panel); `site_tests.rs` sweeps `index.html` and the Pages workflow for each of them |
| `gui/mod.rs` | the winit application, and all that needs the window: hands its events to egui and to the session, works the overlay's switch, takes the renderer when its start-up is over, runs and draws a frame per redraw, and forgets what the overlay asked of the window once it is hidden, by the switch or by the session |
| `gui/session.rs` | `Session`: the machine, the pacer, the meter, the guest's keyboard, the fingers on the screen, the disk tools, the ROM loader, the starter and the overlay, without a window. Starts with the overlay shown and the Start panel open where the host came from a site and the machine has no disk (`Session::on`). Decides whose a key is and when a finger held on the picture works the overlay's switch, and carries out the overlay's `Command`s on the machine; puts the overlay away after a start (`started`); times the overlay's pass and passes the window's `Drawn` on to the meter |
| `gui/hold.rs` | the overlay's switch on a touch screen: `Hold`, told of the fingers and of the time, says when a finger held still on the picture is due |
| `gui/disk_tools.rs` | `DiskTools`, the session's side of the Disks panel: a `DiskCommand` becomes an operation of the core (`io::disk_ops`), carried out at once or once the host's file dialog has answered (`poll`, at the start of every frame) |
| `gui/rom_loader.rs` | `RomLoader`, the session's side of Machine > Load ROM: a `RomCommand` becomes the image the machine restarts with, at once (the built-in BIOS) or once the host's file dialog or the site has answered (`poll`, at the start of every frame) |
| `gui/starter.rs` | `Starter`, the session's side of the Start panel: a `StartCommand` becomes a disk of the site fetched from it, put into the drive the panel chose for its kind with the core's Insert, the boot order set and the machine restarted, once the site has answered (`poll`, at the start of every frame); one start at a time (`starting`), and one that fails changes nothing |
| `gui/host/` | the host as the session's tools reach it. `mod.rs`: `FileHost` (its dialogs, what it lets out, the files of its site), `Platform` (the host the app runs on), `Awaited`, the questions whose answers are on their way down channels, each with what it was asked for (`purposes`), and `fetched`, what an answer of the site brought; `testing.rs`: `ByHand`, a host whose dialogs and site a test answers by hand |
| `gui/pacer.rs` | real-time pacing: `Pacer` reads the host clock, credits the machine with the time that passed and runs it 256 instructions at a time; the 1 ms wait of an idle guest; pause; what each slice did with its time (`Slice`) |
| `gui/meter.rs` | measuring the emulation: `Meter`, told of each frame by the session (the pacer's `Slice`, the machine's `Counts`) and of what the frame spent outside its slice (the overlay's pass, the window's `Drawn`), averages over about a second the guest's speed (with the last minute of it), the frames and how their host time divides into five (running, idle, the overlay, the drawing, the wait for the display), the guest's time (its clock, the time owed and dropped, the share spent halted) and the interrupts per line. It reads no clock |
| `gui/renderer/` | wgpu. `mod.rs`: `record_frame` (the picture and the overlay in one render pass, no surface needed) and what asks for a device; `blit.rs`: `FrameBlit` (frame texture and blit pipeline, the whole-number viewport, the WGSL); `overlay_pass.rs`: `OverlayPass` (egui's meshes); `surface.rs`: `Renderer` (the window's surface), whose `render` returns a `Drawn`: how long the surface kept it waiting for a frame to draw on, and how long the rest took |
| `gui/keys.rs` | physical key to XT scancode; `GuestKeys` sends the codes and remembers which keys the guest holds, and whether the host's keyboard or the on-screen one holds each |
| `gui/overlay/mod.rs` | the overlay's shell: shown or hidden, by the switch (`toggle`) or by the app (`show`, `hide`, which leave the hint to the first bare picture), the hint, the menu bar, the windows of the panels the host offers (`Overlay::on`: whether it came from a site; each window the size of its contents, first opened a cascade step from the one before it, or, for a keyboard, at the foot of the screen: `Panel::opens_at_the_bottom`), which the app may open and close too (`open`, `close`); `Command`, what a tool asks the app to do; `Cx`, what a panel works on; what the last pass covered of the screen (`covers`) and which panels it drew (`shows`); which of the pointer's events are a finger's echo (`with_echoes`) |
| `gui/overlay/load_rom.rs` | Machine > Load ROM, a menu within the Machine menu: the built-in BIOS, the ROM images of the site where there is one (`platform::HAS_SITE`, `site::FILES`) and a file, each asked for with a `RomCommand` |
| `gui/overlay/start.rs` | the Start panel, offered where the host came from a site: a button per disk of the site (`site::FILES`, the floppies then the hard disks, labelled with each disk's verb and title: "Boot MS-DOS 4.00", "Play DOOM"), under each where its disk goes (`Place`, by its kind), each asking for a `StartCommand`, which says whether a finger pressed it; "Fetching ..." and its buttons greyed out while a start is on its way (`Cx::starting`) |
| `gui/overlay/panels.rs` | THE panel table: `Panel`, `ALL`, `title`, the arm of each body, and the panels only a host that came from a site offers (`needs_site`) |
| `gui/overlay/about.rs` | the About panel; every panel has a file like it |
| `gui/overlay/disks.rs` | the Disks panel: every drive with its disk (`image_place`, `disk_size`, which the About panel shares), the buttons that ask the app for a `DiskCommand` on the drive chosen, what the host cannot do left out (`platform::FILE_ACCESS`), and a reboot that is due shown with the reset |
| `gui/overlay/on_screen_keyboard/` | the On-screen keyboard panel. `mod.rs`: the body, which takes up the frame's touches and clicks (the first finger's pointer events are its echo) and draws the keys at a size that follows the screen's; `layout.rs`: THE key table, the 83 keys of the XT by make code, where each lies, its name and its legend; `presses.rs`: who holds which key (each finger, the pointer), the latched modifiers and the repeat, answered with `Command::Key` |
| `gui/overlay/stats.rs` | the Stats panel: whether the guest keeps up with real time and where the host's time goes (five parts of each frame, as a bar and as rows), from the `Meter` the shell is lent (`Cx::meter`); the chart of the last minute, drawn with egui's painter; and the guest's speed at the right end of the menu bar (`readout`) |
| `gui/overlay/memory.rs` | the Memory panel: a hex view of guest memory, the address field (`parse_address`), the jumps to where the register pairs point and to the landmarks of the first megabyte |
| `gui/overlay/pic.rs`, `pit.rs`, `vga.rs`, `keyboard.rs`, `debug_port.rs` | the device panels: the 8259A, the 8253, the VGA and its DAC, the 8255's keyboard interface, the port E9h log |
| `gui/overlay/messages.rs` | where a tool says what it did or why it failed |
| `gui/overlay/theme.rs` | the palette and the sizes as named slots, and their one application to egui's style |
| `gui/overlay/testing.rs` | test support: drives the overlay frame by frame with hand-made input (keys, the pointer, and fingers as a host hands them to egui) and reads the screen as egui describes it to a screen reader; `Shell`, the shell alone over a machine, on a host that came from a site or not (`Shell::on`); `waiting_for_a_key`, a machine the BIOS booted |

The core, in `crates/emu8086_core/`:

| path | role |
|---|---|
| `src/lib.rs` | the two rules of the crate, its public modules, `BIOS_ROM` and what the user knows it by (`BIOS_ROM_NAME`), the crate-level test modules |
| `src/vm/runtime.rs` | the machine, built from what the command line names (`Runtime::new`) or around drives opened already (`with_drives`): the devices on the port bus (`Runtime::with_devices` IS the port map), the run loop (`run`), guest time (`credit_pit_time` and the debt owed to the 8253), interrupt delivery, reset, the ROM socket (`load_rom_image`: an image of 16 bytes to 64 KB at the top of the first megabyte, the socket below it empty, and the name the image is known by, `rom`), the loans of the devices the app reads (`pit`, `keyboard`, `debug_console`), and the account of the guest's time it keeps for whoever measures it (`guest_time`: timer clocks handed over, of them while halted, owed, dropped) |
| `src/vm/instructions/` | the CPU: `process()` in `mod.rs` is the opcode table, one line per opcode; the bodies live in the domain modules beside it (`alu`, `bcd`, `control`, `shift`, `stack`, `string`, `transfer`) |
| `src/vm/modrm.rs` | `decode_rm`, the only effective-address decoder, and the `ModRM` operand widths |
| `src/vm/registers.rs`, `memory.rs`, `cpu.rs` | the register file and the raw-pointer operand wrappers, the 1 MB memory with its ROM protection, the `Cpu` (which also holds the 8259A) |
| `src/utils/number.rs` | `SpecialOps`: the arithmetic the instruction code is written once with for `u8` and `u16` |
| `src/io/bus.rs` | `IoDevice` and the port bus |
| `src/io/pic.rs`, `pit.rs`, `keyboard.rs`, `dma.rs`, `fdc.rs`, `hdc.rs`, `console.rs` | 8259A, 8253, the XT keyboard on the 8255, the 8237 as registers, two controller stand-ins, debug port E9h (which keeps its last lines). The first three and the console describe themselves to a reader without a port access: `Pic::view`, `Pit::channels`, `KeyboardController::view`, `DebugConsole::lines`; `Pic::acknowledged` tallies the interrupts acknowledged on each line |
| `src/io/vga.rs`, `raster.rs`, `font_cp437.rs` | the VGA registers (`VgaDevice::registers` and `dac_colour` for a reader), the CPU-side rasterisers the window uploads, the 8x16 font |
| `src/io/disk/`, `src/io/disk_spec.rs` | disk images (in a host file, or in memory, made of a host's bytes and given back as them), the controller with the change lines and the reboot due after a hard disk changed (`reboot_due`), the host side of INT 13h, host-side FAT access and the `WallClock` its callers pass; the spellings of drives and images on the command line, and `open_drives`, which opens what the command line names and returns what the user is to be told of it (`OpenedDrives`) |
| `src/io/disk_ops.rs` | the disk tools' operations (`Operation`: insert, create, move, save, eject, put; `apply`; `contents`, a disk as its bytes), each answering with an `Outcome`: words for the user, and whether the guest must be rebooted. A file comes as a `HostFile`, by its path or by its name and contents; an image of no bytes is no disk, and the Insert refuses it |
| `build.rs` | reassembles `assets/rom/bios.bin` when a ROM source is newer and patches its checksum |
| `src/bios_tests.rs` | tests that boot the real ROM on a fully wired machine (the `Machine` harness) |
| `src/cpu_tests.rs` | the CPU against 608,000 vectors captured from real hardware (`8086/` submodule) |
| `src/bios_inspection_tests.rs` | static checks of the ROM image |
| `src/crate_rules_tests.rs` | the two rules of the crate as tests: no clock in its sources, no dependency but the listed ones |

`assets/rom/` holds the BIOS: `bios.asm` plus one `.inc` per interrupt, and
the tracked `bios.bin`.

The tool, in `crates/dos_images/`:

| path | role |
|---|---|
| `pins.env` | THE pins of the inputs: the URLs and SHA-256 of FreeDOS 1.4's Floppy Edition and its boot floppy, the repository, branch, commit and git tree of MS-DOS 4.00's restored source, Doom8088's tag, commit, release zip, files and GPL text. `fetch-inputs.sh` sources it, the tool compiles it in (`src/pins.rs`), the workflow's cache key hashes it |
| `fetch-inputs.sh` | fetches the inputs into a new directory, each checked against its pin: curl and `sha256sum --check`, unzip of the files needed, and `v4.0` of the commit by a sparse, depth-1 git fetch whose commit and tree are checked, its files checked out as committed |
| `msdos400-binaries.sha256`, `disks.sha256` | the lists the workflow checks the output against: the 62 files the build makes (61 of the MS-DOS 4.00 release of 1988, and XMAEM.SYS) and the two disks |
| `src/main.rs` | the command line (`--freedos`, `--msdos`, `--doom8088`, `--out`), the stages run, a failure printed, the exit code |
| `src/stages/` | the stages in their order (`mod.rs`, `run`), each with its error type: `host` (FreeDOS's floppy with its own FDCONFIG.SYS and FDAUTO.BAT), `build_disk` (the source on a 100 MB FAT16 disk, with BUILD.BAT), `build` (the run: its markers in turn, its budgets, a line a module), `collect` (what CPY.BAT copied, the boot sector, the master boot record, a clean BUILD.LOG), `floppy`, `hard_disk`; `msdos` puts MS-DOS on a volume for both, `check` has CHKDSK check each from a copy of the floppy |
| `src/guest.rs` | the machine as the tool runs it: batches of instructions credited with guest time, budgets of guest time, the port E9h's new lines, the text screen, the hash of the Mode 13h picture, a disk's contents |
| `src/makefiles.rs` | the modules, from the makefiles that only dispatch; BUILD.BAT; the files of CPY.BAT |
| `src/markers.rs` | E9.COM, and the markers the batch files send through it |
| `src/log_scan.rs` | BUILD.LOG: the lines of an error, the warnings of the tools, a module's part |
| `src/layout.rs` | the disks below their file systems: CHS and the partition entry, `HardDisk`, the build's boot sector on a volume with the volume's own BPB, IO.SYS and MSDOS.SYS sealed where the boot sector looks |
| `src/fat.rs`, `src/stamp.rs` | fatfs mounted at a fixed stamp; DOS names, DOS text; the stamps and the volumes' serials |
| `src/notices.rs`, `src/pins.rs` | README.TXT and SOURCE.TXT of the disks; `pins.env` read |
| `src/failure.rs`, `src/report.rs` | a failure as an `::error` annotation of GitHub Actions and what the guest showed; `report.txt` |

The tool's rules. Guest time comes from the instructions run, never from a
clock: each batch of a million instructions is credited 100 ns an
instruction, a guest of 10 MIPS, and a budget is of guest time, which a
guest that waits for a key spends too. fatfs stamps what the tool writes
with fixed times and the volumes have fixed serials, so the same inputs
make the same disks, byte for byte, on every run and every host; the host's
clock only measures, for the report. The tool reaches the core through its
public API alone. Its inputs are plain directories: it downloads nothing
and checks no hash itself, which `fetch-inputs.sh` and the workflow do with
curl and sha256sum. A pin is written in `pins.env` and nowhere else. A
change that changes what a disk holds changes its bytes: `disks.sha256` is
then taken anew from a run of the tool and committed with the change.

Every source file carries a `//!` header stating what it owns — skim headers
before reading bodies.

The core's public surface is what the app and the tool reach. A module of
`io` or `vm` is `pub` when one of them reaches into it, by name or through a
public field of the `Runtime`, and `pub(crate)` otherwise; an item is
widened from `pub(crate)` when one of them needs it, not before.
`Runtime::cpu` is public for reading: whoever writes to it keeps the rule of
`vm/mod.rs` (the `Memory` box is never replaced).

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
- **A ROM**: the socket is the core's. `Runtime::load_rom_image` takes an
  image of 16 bytes to 64 KB, which ends at F000:FFFF, makes the rest of
  the socket read 0xFF as an empty socket does (nothing of the ROM before
  stays below a smaller one) and keeps the name the user knows it by
  (`Runtime::rom`, which the About panel shows); `load_rom` reads a file,
  and refuses one longer than an image unread. Nothing else is refused,
  and an image refused is a message for the user that changes nothing;
  one longer than an image is told as "more than 64 KB", whatever its
  length. The command line names a ROM for the start. At run time Machine
  > Load ROM (`src/gui/overlay/load_rom.rs`) asks for one with a
  `RomCommand`, and `RomLoader` (`src/gui/rom_loader.rs`) loads it, at
  once or when the host's dialog or the site has answered, and restarts
  the machine at the reset vector as Reset does, its memory, devices and
  drives as they are. Its dialog asks for a ROM (`Wanted::Rom`), of which
  a browser reads no more than `ROM_MAX_SIZE` and a byte: a disk image of
  gigabytes chosen by mistake is refused there unread as well.
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
  frame comes back to it. The time between the two is credited by the
  next slice like any other, to a halted guest; to a paused one, nothing
  is credited, there as on a desktop. The window's cursor blink is the
  one thing drawn on host time, and the overlay's only time is that of
  egui's input. A test states the time: `credit_pit_time`, then
  `step_frame`.
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
  desktop (`egui_events.rs`, `fat_stamp.rs`, `site_answer.rs`); where the
  two hosts differ in what a caller does next, the name says so in what it
  returns (`platform::wait`, `platform::FILE_ACCESS`, `platform::HAS_SITE`)
  and a test puts the other host's answer in its place
  (`Pacer::waiting_with`, `DiskTools::on`, `RomLoader::on`, `Session::on`,
  `Overlay::on`).
- **Keys**: the window sends `EmulatorEvent`s down a channel to
  `KeyboardController`: the scancode of the physical key (`src/gui/keys.rs`),
  so the layout is the guest's business. The overlay's switch is the
  host's (`platform::OVERLAY_SWITCH`). On a desktop it is F12, which the
  window keeps: neither egui nor the guest sees it. In a browser F12 stays
  the browser's key (the developer tools; `platform/web.rs` keeps F11 and
  F12, which no XT keyboard has, from the canvas), and the switch is the
  secondary button of the pointer: the press is offered to egui first and
  works the switch only on the picture (`Session::overlay_button`). On
  every host a finger held still on the picture for half a second works
  the switch too (`gui/hold.rs`), as soon as its time is up: the window
  tells the session of every finger (`Session::touch`) and asks at every
  frame, hidden or shown, whether one's time is up (`Session::hold`),
  since a finger that rests sends no event. What lies under the finger is
  decided then, at the finger's own position, from what the overlay's last
  pass laid out (`Overlay::covers`: egui's layers there, the menu bar,
  which egui draws on its background layer, and the messages, which take
  no input). An overlay the app showed by itself (`Overlay::show`) covers
  everything until a pass has laid it out: at the start, while a browser
  readies its drawing, a finger held anywhere on the page is the overlay's,
  and the Start panel stays shown for the visitor to choose from.
  What egui kept of a finger that just came down says nothing:
  egui answers for where its pointer last was, which after a finger lifted
  is nowhere. The browser's switch is named for both ("Right-click or
  touch and hold"). Every other event of the window goes to egui first. A
  key then goes on to the guest unless egui kept it (Tab) or the overlay
  has the keyboard: one of its widgets has the focus or one of its menus
  is open (`Overlay::wants_keyboard`, `Session::key`). The overlay's
  On-screen keyboard is a keyboard of the guest's as well: its panel asks
  for each press and release of a key by its make code (`Command::Key`:
  the overlay knows no winit), and the session hands them to
  `GuestKeys::screen_key`, under the rules of a host's key. The guest gets
  the release of a key if and only if it got its press, whichever keyboard
  pressed it: when the overlay takes the keyboard, or the window loses it,
  every key the guest holds is let go (`GuestKeys::release_all`); when the
  on-screen keyboard leaves the screen (the overlay hidden, or a pass that
  did not draw the panel: `Overlay::shows`), the keys it held are
  (`GuestKeys::release_screen`), and the panel drawn again remembers
  nothing (`Cx::drawn_before`). No key of the panel takes the focus: one
  that did would give the overlay the keyboard, and undo the chord it is
  part of.
- **An overlay panel**: ONE row in THE panel table
  (`src/gui/overlay/panels.rs`: a variant of `Panel`, its place in `ALL`,
  its arm in `title` and in `body`) plus one file for the body,
  `pub(super) fn body(ui: &mut egui::Ui, cx: &mut Cx<'_>)`. The checkbox in
  the View menu, the window and its close button follow from `ALL`; a panel
  that works on what only a host that came from a site has is named in
  `Panel::needs_site`, and the View menu of another host leaves it out
  (the shell is told the host's answer: `Overlay::on`). The app opens and
  closes a panel as its checkbox does (`Overlay::open`, `close`). The
  body has the machine as `cx.runtime` for the length of the pass, asks the
  app for what only the app can do through `cx.commands`, and says what it
  did or why it failed through `cx.messages`. The window takes the size of
  the body: a horizontal separator would stretch it across the screen, so
  panels part their sections with a gap. A panel's own state lives in
  egui's memory (`ui.data_mut`), as the Memory panel's view does; one that
  holds something only while it is on the screen, as the on-screen
  keyboard holds keys, drops it when the pass before did not draw it
  (`cx.drawn_before`: not after its window was closed or collapsed, or the
  overlay hidden). egui may run the UI twice for one frame, and gives the
  frame's events (`ui.input(|i| i.events)`) to the first run alone. Which
  of them a finger made is the shell's to tell (`with_echoes`): the first
  finger is egui's pointer as well, and a press or a release of the
  primary button right after a touch is that touch's echo.
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
  download in a browser; the dialogs of the disk tools and of the ROM
  loader; the fetch of a file of the site), never from the overlay. Load
  ROM is a menu of its own within the Machine menu
  (`src/gui/overlay/load_rom.rs`).
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
  (INT 19h writes port B1h), and the panel shows it with the reset. The
  host is a `FileHost` (`src/gui/host/`), and a test answers its dialogs
  by hand (`host::testing::ByHand`, `disk_tools_tests.rs`).
- **A file of the site**: a row of THE table in `src/site.rs` (its path
  relative to the page, its kind, its title), a `copy-file` link in
  `index.html` that has trunk put it beside the page (so `trunk serve`
  serves it too), and the Pages workflow's check of it in `dist/`.
  `site_tests.rs` sweeps the page and the workflow for every row, and for
  a file the page publishes that the table does not have. The app offers
  the site's files only where `platform::HAS_SITE`, and fetches one
  through `FileHost::fetch` (`platform::fetch`), which answers later down
  a channel like a dialog: in a browser relative to the page, so the same
  path works at the root of `trunk serve` and below `/Emulator8086/` on
  Pages. An answer that is a 404, or a page in the place of the file (as
  `trunk serve` answers a path it has no file for), is "The site has no
  ..." (`platform/site_answer.rs`). A test answers the fetches by hand
  (`host::testing::ByHand`).
- **A disk of the site's**: a row of THE table in `src/site.rs` of the kind
  `Floppy` or `HardDisk`, with the verb of its button (`disks/msdos400.img`,
  "MS-DOS 4.00", "Boot"; `disks/doom8088.img`, "DOOM", "Play"). The app
  offers the disks in the Start panel (`src/gui/overlay/start.rs`), where
  `platform::HAS_SITE` (`Panel::needs_site`): a button each, the floppies
  then the hard disks, labelled with the verb and the title
  ("Boot MS-DOS 4.00", "Play DOOM"), so that the table stays the one
  source of both. Where the host came from a site and the machine starts
  with no disk, the session starts with the overlay shown and the panel
  open (`Session::on`); the View menu has it besides. A press asks for
  `Command::Start` with where the disk's kind puts it (`Place`: a floppy
  into A:, booted first, then the first hard disk; a hard disk in as the
  first hard disk, booted first, then A:) and whether a finger pressed it
  (the release of the button is a finger's echo). The session's `Starter`
  (`src/gui/starter.rs`) fetches the disk (`FileHost::fetch`), one start at
  a time: meanwhile the panel says "Fetching ..." with its buttons greyed
  out (`Cx::starting`). Once the disk has come, the core's Insert puts it
  into its drive from its contents (a hard disk by
  `DiskImage::hard_disk_from_bytes`, as the tool built and booted it), the
  boot order is set (`DiskController::set_boot_order`) and the machine
  restarts as Reset does: "Restarted with MS-DOS 4.00, the floppy in A:".
  A fetch that failed, a site without the disk, or an image the Insert
  refuses (one of no bytes) changes nothing and says why: "... The machine
  runs on as it was". A start carried out closes the Start panel and runs
  the machine, a paused one too (`Pacer::set_paused`); pressed with a
  finger it shows the overlay with the on-screen keyboard open, even one
  hidden while the disk was on its way (`Overlay::show`), otherwise the
  overlay hides (`Overlay::hide`), so that the keys
  are the guest's, and the window forgets what the overlay asked of the
  cursor. The app's own showing and hiding of the overlay ends no hint:
  the hint's seconds are those of the first bare picture. A test answers
  the fetches by hand (`host::testing::ByHand`; `starter_tests.rs`,
  `session_tests.rs`). The repository holds no disk: the Pages
  workflow makes them with `crates/dos_images`, checks them against
  `crates/dos_images/disks.sha256` and copies them into `dist/disks/`
  after trunk built, by those names. The page names none, so `trunk serve`
  serves no disk: a fetch of one there is answered "The site has no ...".
  `site_tests.rs` holds the table to both: a ROM is copied by trunk and
  checked by the workflow, a disk is copied by the workflow and not named
  by the page. What a disk holds is the tool's `stages/floppy.rs` and
  `stages/hard_disk.rs`, with `stages/msdos.rs` (MS-DOS put on a volume),
  `layout.rs` (below the file system) and `notices.rs` (README.TXT,
  SOURCE.TXT); a change there changes the disk's bytes and its line of
  `disks.sha256`, taken from a run of the tool. A change to the core or to
  the BIOS may change what the build makes: the workflow builds anew
  (its cache key holds both) and fails where a file differs from
  `msdos400-binaries.sha256`, which is the release's for 61 of them. A
  stage that fails prints an `::error` annotation, what the emulated
  screen showed, the last lines of port E9h and, for the build, the
  failing module's part of BUILD.LOG.
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
core's alone, `-p dos_images` the tool's. There is one configuration: no
cargo feature selects anything. What selects is the target: the
dependencies only one host has stand in the target tables of the root
manifest.

The site's disks are made by hand as the workflow makes them (bash with
curl, sha256sum, unzip and git; Git Bash on Windows), with directories
outside the repository:

```
bash crates/dos_images/fetch-inputs.sh <inputs>               # the inputs, each checked against pins.env; <inputs> new or empty
cargo run --release -p dos_images -- --freedos <inputs>/freedos --msdos <inputs>/msdos/v4.0 --doom8088 <inputs>/doom8088 --out <out>
(cd <out>/bin && sha256sum --check --quiet <repo>/crates/dos_images/msdos400-binaries.sha256)
(cd <out>/disks && sha256sum --check <repo>/crates/dos_images/disks.sha256)
```

The tool's run is the full build, about two minutes in release on a
desktop: there is no ignored test of it, since its inputs are not in the
repository and the check of its output is sha256sum's. `cargo test
--workspace` runs the tool's unit tests, which need no input: the
makefiles, the layouts, the markers and E9.COM, the log, CHKDSK's report,
the notices, the pins, and the account of a build told its markers by
hand. Run the tool with `--release`: the emulator of a debug build is many
times slower.

The browser build is trunk's (0.21, `cargo install trunk`): `trunk serve`
builds it, serves it on http://127.0.0.1:8080 and rebuilds on change, and
`--release` gives the optimised build. Trunk fetches the wasm-bindgen CLI of
the version locked in `Cargo.lock`, and wasm-opt for a release build, on
first use.

The browser build of `main` is published on GitHub Pages, at
https://jj136975.github.io/Emulator8086/, by `.github/workflows/pages.yml`:
at every push to main, and by hand from the Actions tab, GitHub's Ubuntu
runner builds it with `trunk build --release` and publishes `dist/`. The
site is served below the repository's name, so the workflow gives trunk the
base path Pages reports as its `--public-url`; a page built for `/` loads
nothing there. The runner installs the toolchain `rust-toolchain` pins, and
takes trunk from its releases at the version the workflow names, checked
against the checksum beside it: a new version of trunk changes both. The
workflow runs no test and no lint (the bar above is run by hand before a
commit), and uses GitHub's own actions alone (the ledger below), each
pinned to the commit of the release named beside it: a new release is
taken by changing both. Each job's token can do what that job needs and
no more: the build, which runs the build scripts of every crate and the
tools it downloads, reads the repository and the Pages settings, and its
checkout keeps no token; only the deployment has `pages: write` and
`id-token: write`. Its deployment needs the repository's Settings > Pages
to name "GitHub Actions" as the source of the site; the `github-pages`
environment there takes deployments of `main` alone.

The site publishes the BIOS beside the page, as `rom/bios.bin`, assembled
on the runner. The workflow installs nasm from Ubuntu's packages and
touches the sources in `assets/rom`, so that the core's build script, the
one path that assembles the BIOS (nasm, then the checksum patch), assembles
`bios.bin` again during the build that embeds it. trunk copies the image
into `dist/rom/` (the `copy-file` link of `index.html`, which `trunk serve`
serves too), in a pipeline that runs beside the build of the app, before or
after the build script replaced the image. The run then fails unless
`bios.bin` was assembled in it (it is newer than its sources), is the image
committed (`git diff --exit-code`) and is what `dist/` publishes (`cmp`). A
change to the BIOS's sources is committed with the `bios.bin` it assembles
to; a nasm that assembled other bytes would fail the run as well.

The site publishes the two disks beside the page as well
(`disks/msdos400.img`, `disks/doom8088.img`), which the workflow makes
after trunk built the page and the BIOS was checked. It runs
`crates/dos_images/fetch-inputs.sh`, then the tool in release, then checks
the 62 files against `msdos400-binaries.sha256`, copies the disks into
`dist/disks/` and checks them there against `disks.sha256`, and puts the
tool's report into the run's summary. The tool's whole output is kept with
`actions/cache`, keyed on what can change it: `pins.env`, all of
`crates/dos_images` and `crates/emu8086_core`, `assets/rom/bios.bin` (the
machine's BIOS), the root manifest, `Cargo.lock` and `rust-toolchain`. A run
that finds it in the cache neither fetches the inputs nor runs the tool,
and still checks both lists and copies the disks. One that does not takes
about two and a half minutes more, as the workflow's steps replayed in
`ubuntu:24.04` on a desktop took them (the fetch 6 s, the tool's compile
6 s after trunk's build, its run 125 s, of which the build in the emulator
122 s); GitHub's runner is slower than that desktop per thread.

The CPU is validated against real hardware: a change under the core's
`src/vm/` runs the suite with `--include-ignored`. Never judge a suite
through a pipe (`cargo test | tail` hides the exit code): redirect to a log,
check the exit code, then read the log.

`bios.bin` is a tracked build product. With `nasm` on PATH the core's build
script reassembles it whenever a file in `assets/rom` is newer; commit it
together with the source change that produced it. The Pages workflow checks
that it was (above).

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
`gui/mod.rs`, which is where the window's events become egui's input,
where the overlay's switch is picked out and where the window forgets
what a hidden overlay asked of the cursor, and the system's file dialogs
(`platform::FileDialogs::ask` on a desktop; the thread their answers come
from is tested, `answer_later`).

Of the browser build the suite covers, on the desktop, what needs no
browser: the pacer on a host that cannot wait (`gui::pacer::tests`), the
window's events as egui's input, the fingers on a touch screen among them
(`platform::egui_events::tests`), the FAT stamp of a date
(`platform::fat_stamp::tests`), the overlay's button and the hold of a
finger (`gui::session::tests`, `gui::hold::tests`), the On-screen keyboard
driven with fingers as a host hands them to egui
(`gui::overlay::on_screen_keyboard::tests`, and through the session to the
guest's keyboard in `gui::session::tests`), the disk tools and the Disks
panel on a host that hands files over as their contents
(`gui::disk_tools::tests`, `gui::overlay::disks::tests`), Machine > Load
ROM on a host with a site and the ROM of the site loaded from answers
given by hand (`gui::overlay::load_rom::tests`, `gui::rom_loader::tests`),
the Start panel on a host with a site and not on one without, its buttons
pressed with the pointer and with a finger, its disks fetched, put in and
booted by the built-in BIOS with answers given by hand, the panel opened
at start with no disk, and the overlay put away after a start, for the
pointer and for a finger (`gui::overlay::start::tests`, `gui::starter::tests`,
`gui::session::tests`; the hint that waits for the bare picture in
`gui::overlay::tests`), what a site's answer to a fetch says
(`platform::site_answer::tests`), the site's files against what the page
and the workflow publish (`site::tests`), and the frame drawn on a device
that has no more than WebGL2 guarantees
(`gui::renderer::tests::the_frame_is_drawn_within_the_limits_of_webgl2`;
with `WGPU_BACKEND=gl` it runs on the backend WebGL2 shares its shader
translation with). `platform/web.rs` and the page are outside the suite,
the file picker, the download and the fetch among them.
The page is looked at through the Playwright MCP server alone (tools named
`mcp__playwright__*`), never through another browser: serve it with
`trunk serve` on its default address (with `--address 0.0.0.0`, trunk 0.21
was seen to log a listening server and hold no socket), and the server's
browser, which runs in a container, reaches it through Docker Desktop at
`http://host.docker.internal:8080`. That browser has no GPU, so what it
shows is the WebGL2 path; WebGPU needs the user's own browser. The overlay
is drawn in the canvas, which a snapshot of the page shows empty: it is
driven by coordinates, `page.mouse` and `page.keyboard` in
`browser_run_code_unsafe`. Without those tools, say that the page was not
looked at.

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
| 2026-10-04 | `rfd` 0.17 | approved | file dialogs, native and in the browser. In use, in the app on both hosts (`src/platform`): the system's dialogs on a desktop, the file picker in a browser. With its default features, which only Linux and the BSDs build (the desktop portal's dialogs, parented to a Wayland window too). In a browser it brings `web-sys` features of its own (the file input and its reader, the user's activation), which cargo adds to those the root manifest lists. Its `file-handle-inner` feature, which adds no crate, is on in a browser since 2026-10-12: the page's `File` behind a picked file, of which a ROM's is read no further than an image and a byte (`Wanted::Rom`) |
| 2026-10-04 | `wasm-bindgen`, `wasm-bindgen-futures`, `web-sys`, `js-sys`, `web-time` | approved | the browser (wasm32) build. In use: `web-time` in the app on both hosts (the clocks of `src/platform`), the other four as dependencies of the wasm32 target only, `web-sys` with the features the root manifest lists and no other (beside those `rfd` asks for itself). `Response` and `Headers` were added on 2026-10-10 with the fetch of the site's files (`platform::fetch`), and `File` on 2026-10-12 with the part of a ROM's file the picker reads |
| 2026-10-04 | `trunk` (`cargo install trunk`) | approved | build tool for the browser build, not a dependency of the crate. In use: 0.21, with `index.html` at the root |
| 2026-10-04 | `console_error_panic_hook`, `console_log` | refused | the browser build's panic hook and console logger are written by hand: they are in `src/platform/web.rs` |
| 2026-10-10 | GitHub's own actions: `actions/checkout`, `actions/cache`, `actions/configure-pages`, `actions/upload-pages-artifact`, `actions/deploy-pages` | requested with the deployment (the user asked for the browser build to be built by CI and published on GitHub Pages), not asked one by one | `.github/workflows/pages.yml` alone. No action of a third party: the runner's rustup installs the toolchain, and trunk comes from its releases, checked against the checksum the workflow holds. Since 2026-10-12 each is pinned to the commit of a release, the one the major tag named then (checkout v7.0.1, cache v6.1.0, configure-pages v6.0.0, upload-pages-artifact v5.0.0, deploy-pages v5.0.1) |
| 2026-10-10 | `nasm`, from Ubuntu's packages on the runner | requested with the user's "compile + upload" of the BIOS on 2026-10-10 (CI assembles the BIOS and publishes it with the site), not asked apart | `.github/workflows/pages.yml` alone: the runner installs it with apt for the core's build script to assemble `assets/rom`. A build tool, not a dependency of either crate; a desktop build uses whatever nasm is on PATH, or none |
| 2026-10-10 | MS-DOS 4.00's source, restored: lotharsm/MS-DOS, branch `dos-4.00-reconstructed`, commit `500867d` (Microsoft's 2d04cac with its damaged files mended), MIT License | the user's choice on 2026-10-10: MS-DOS 4.00, built from source in CI, over FreeDOS as the DOS of the site's disks. MS-DOS 6.22, which Microsoft never licensed for redistribution, is never published | an input of `crates/dos_images` alone, fetched pinned (`pins.env`: the commit, and the git tree of `v4.0`): built inside the emulator, and published on the two disks with its MIT notice (`LICENSE.TXT`) and a `README.TXT` that names the source |
| 2026-10-11 | FreeDOS 1.4, the boot floppy of its Floppy Edition (kernel 2043, FreeCOM 0.86; GPL) | requested with the CI build of MS-DOS 4.00 (the user's choice of 2026-10-10), as its build host; not asked apart | `crates/dos_images` alone: fetched pinned (`pins.env`: the zip's and the floppy's SHA-256), booted in the emulator to run the build, and never published. Nothing of it is on the disks |
| 2026-10-11 | Doom8088 v20260304 (FrenkelS/Doom8088, tag commit `8f97ce0`; GPL-2.0), its Mode 13h build `D8M13H.EXE`, and the shareware `DOOM1.WAD` of its release | the user's decision on 2026-10-11 to publish the DOOM disk with the WAD, told that id Software's shareware license forbids modified copies and that Doom8088's release ships the WAD preprocessed for it | `crates/dos_images` alone: fetched pinned (`pins.env`), with the repository's GPL text at the tag's commit as `COPYING`; published on the DOOM disk in `\DOOM` with Doom8088's README, `COPYING` and a `SOURCE.TXT` naming the repository, the tag and the commit, and the WAD as id Software's shareware data, not under the GPL |

Approved does not mean added: the manifests say what is in use, the root
one at which version and each crate's own which of them it takes.
`crossterm` left with the terminal frontend on the same date. clap,
env_logger and pollster are dependencies of the native targets only, and
wgpu takes its `webgl` feature for wasm32 only (WebGPU is among its default
features). chrono is a direct dependency of neither crate: on a desktop the
app gets the local time through fatfs's `chrono` feature, which it turns on
for the native targets only; in a browser the time is the page's `Date`.
The fetch of a file of the site is `Window::fetch_with_str`, which needs no
feature beyond `Window`; its answer needs `Response`, and the content type
that tells a page from a file needs `Headers`. No `Request` or
`RequestInit`: the fetch is a plain GET of a path relative to the page.

egui-winit 0.36.2 does not compile for wasm32: its `NativeFile` implements
`egui::DroppedFile::bytes`, which egui declares only off wasm32 (the trait
has `bytes_async` there), and upstream's main branch was the same on
2026-10-04. In a browser the app therefore turns winit's events into egui's
input itself (`src/platform/egui_events.rs`: pointer, wheel, touch, keys,
text, modifiers, focus, cursor; no IME, dropped files or clipboard). Once
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
