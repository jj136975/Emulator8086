//! Machine > Load ROM carried out on a host whose dialogs and site the tests
//! answer by hand, when they like: what each command asks of the host, when
//! the machine restarts and with which image, and what the messages say.
//! The rules of a ROM image are the core's and are tested there
//! (`emu8086_core::vm::runtime`); the menu that asks for the commands is
//! tested beside it (`overlay::load_rom`).

use std::path::PathBuf;
use std::sync::mpsc::channel;

use egui::accesskit::Role;
use winit::event::ElementState;

use super::*;
use crate::gui::host::testing::ByHand;
use crate::gui::overlay::testing::{Driver, submenu};
use crate::gui::session::Session;
use crate::site::FILES;
use crate::wall_clock::HostClock;
use emu8086_core::io::disk_spec::parse_disk_specs;
use emu8086_core::io::keyboard::KeyboardController;

/// What the machine runs before a test loads another ROM: a jump to itself.
const BUSY_ROM: [u8; 16] = {
    let mut rom = [0x90; 16]; // NOP
    rom[0] = 0xEB; // JMP $
    rom[1] = 0xFE;
    rom
};
const BUSY_NAME: &str = "a busy loop";

/// Where a marking ROM leaves its mark: 0000:0500, just above the BIOS
/// data area, which no ROM of these tests sets up.
const MARK_AT: usize = 0x0500;

/// A ROM image of `size` bytes that, from its reset vector, writes `mark`
/// to `MARK_AT` and then jumps to itself; NOPs below.
fn marking_rom(size: usize, mark: u16) -> Vec<u8> {
    let [low, high] = mark.to_le_bytes();
    let mut rom = vec![0x90; size];
    let reset_vector = size - 16;
    rom[reset_vector..reset_vector + 8].copy_from_slice(&[
        0xB8, low, high, // MOV AX, mark
        0xA3, 0x00, 0x05, // MOV [0500h], AX (DS is 0 after a reset)
        0xEB, 0xFE, // JMP $
    ]);
    rom
}

/// The bytes of the ROM socket, F000:0000-FFFF.
fn socket(runtime: &Runtime) -> Vec<u8> {
    (0xF0000..0x100000)
        .map(|address| runtime.cpu.memory.read_byte(address))
        .collect()
}

/// A machine with a floppy in A:, running `BUSY_ROM`, its CPU away from
/// the reset vector, as one that has run.
fn machine() -> Runtime {
    let (_keys, key_events) = channel();
    let mut runtime = Runtime::new(
        parse_disk_specs(&["a:memory".to_string()]).unwrap(),
        Vec::new(),
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .unwrap();
    runtime.load_rom_image(&BUSY_ROM, BUSY_NAME).unwrap();
    runtime.cpu.registers.cs.reg_mut().set(0x1234);
    runtime.cpu.registers.pc.set(0x5678);
    runtime
}

/// Whether the CPU of `runtime` is about to run its first instruction
/// after a reset: at F000:FFF0, as FFFF:0000.
fn at_the_reset_vector(runtime: &Runtime) -> bool {
    let registers = &runtime.cpu.registers;
    registers.cs.reg().word() == 0xFFFF && registers.pc.word() == 0 && !runtime.cpu.halted
}

/// A message that tells of something done.
fn done(text: &str) -> (String, bool) {
    (text.to_string(), false)
}

/// A message that tells of something that failed.
fn failed(text: &str) -> (String, bool) {
    (text.to_string(), true)
}

/// The loader on a host whose dialogs and site the test answers, the
/// machine it restarts, and the messages it reports in.
struct Bench {
    loader: RomLoader,
    host: ByHand,
    runtime: Runtime,
    messages: Messages,
}

impl Bench {
    fn new() -> Self {
        let host = ByHand::default();
        Self {
            loader: RomLoader::on(Box::new(host.clone())),
            host,
            runtime: machine(),
            messages: Messages::default(),
        }
    }

    fn execute(&mut self, command: RomCommand) {
        self.loader
            .execute(command, &mut self.runtime, &mut self.messages);
    }

    /// What a frame does first: takes up the answers that came.
    fn poll(&mut self) {
        self.loader.poll(&mut self.runtime, &mut self.messages);
    }

    /// What the messages said since the last look.
    fn said(&mut self) -> Vec<(String, bool)> {
        self.messages.take_said()
    }

    /// Whether the machine is as `machine` made it: its ROM, its CPU
    /// where it was, its floppy in A:.
    fn unchanged(&self) -> bool {
        let mut busy = vec![0xFF; 0x10000 - BUSY_ROM.len()];
        busy.extend(BUSY_ROM);
        socket(&self.runtime) == busy
            && self.runtime.rom() == Some(BUSY_NAME)
            && self.runtime.cpu.registers.cs.reg().word() == 0x1234
            && self.holds_its_floppy()
    }

    fn holds_its_floppy(&self) -> bool {
        let disks = self.runtime.disks.borrow();
        disks.drives.iter().any(|(drive, _)| *drive == 0x00)
    }
}

/// The built-in BIOS is at hand: the machine restarts with it at once, at
/// the reset vector, without asking the host anything, and the message says
/// so. The drives keep their disks.
#[test]
fn the_built_in_bios_restarts_the_machine_at_once() {
    let mut bench = Bench::new();
    bench.execute(RomCommand::BuiltIn);

    assert!(socket(&bench.runtime) == BIOS_ROM);
    assert_eq!(bench.runtime.rom(), Some(BIOS_ROM_NAME));
    assert!(at_the_reset_vector(&bench.runtime));
    assert!(bench.holds_its_floppy());
    assert_eq!(bench.said(), [done("Restarted with the built-in BIOS")]);
    assert!(bench.host.questions().is_empty() && bench.host.fetches().is_empty());
}

/// A ROM image of the user's is asked for in the host's dialog, and the
/// machine runs on as it was, however many frames go by, until the answer
/// comes. Then it restarts at the reset vector with the image, which ends
/// at the top of the socket with the socket empty below it, and runs it.
/// The message names the image as the host handed it over.
#[test]
fn a_rom_of_the_users_restarts_the_machine_when_the_dialog_answers() {
    let mut bench = Bench::new();
    bench.execute(RomCommand::FromFile);
    let (asked, answer) = bench.host.question();
    assert_eq!(
        asked,
        FileQuestion {
            title: "Restart the machine with a ROM image".to_string(),
            wanted: Wanted::Image,
        }
    );
    for _ in 0..3 {
        bench.poll();
    }
    assert!(bench.unchanged(), "the dialog is open");
    assert_eq!(bench.said(), []);

    let image = marking_rom(0x2000, 0xA55A);
    let handed = HostFile::Contents {
        name: "small.bin".to_string(),
        bytes: image.clone(),
    };
    answer.send(vec![handed]).unwrap();
    bench.poll();

    assert_eq!(bench.said(), [done("Restarted with small.bin")]);
    assert_eq!(bench.runtime.rom(), Some("small.bin"));
    let socket = socket(&bench.runtime);
    assert!(
        socket[..0xE000].iter().all(|&byte| byte == 0xFF),
        "empty below"
    );
    assert!(socket[0xE000..] == image[..]);
    assert_eq!(socket[0xFFF0..0xFFF3], [0xB8, 0x5A, 0xA5], "at F000:FFF0");
    assert!(at_the_reset_vector(&bench.runtime));
    assert!(bench.holds_its_floppy());

    bench.runtime.run(10);
    assert_eq!(bench.runtime.cpu.memory.read_word(MARK_AT), 0xA55A);
}

/// A scratch file in the host's temp directory, removed again on drop.
struct Scratch(PathBuf);

impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

/// Where files have paths, the image chosen is read from its file, and
/// known by its path.
#[test]
fn a_rom_chosen_by_its_path_is_read_from_its_file() {
    let file = Scratch(
        std::env::temp_dir().join(format!("emu8086-rom-loader-{}.bin", std::process::id())),
    );
    std::fs::write(&file.0, marking_rom(0x4000, 0x1234)).unwrap();
    let mut bench = Bench::new();

    bench.execute(RomCommand::FromFile);
    let (_, answer) = bench.host.question();
    answer.send(vec![HostFile::Path(file.0.clone())]).unwrap();
    bench.poll();

    let path = file.0.display().to_string();
    assert_eq!(bench.said(), [done(&format!("Restarted with {path}"))]);
    assert_eq!(bench.runtime.rom(), Some(path.as_str()));
    bench.runtime.run(10);
    assert_eq!(bench.runtime.cpu.memory.read_word(MARK_AT), 0x1234);
}

/// A ROM image of the site's is fetched from it, by its path relative to
/// the page, and the machine restarts with it when it has come, known as
/// the site's file.
#[test]
fn a_rom_of_the_sites_restarts_the_machine_when_it_has_come() {
    let mut bench = Bench::new();
    let file = &FILES[0];
    bench.execute(RomCommand::FromSite(file));
    let (path, answer) = bench.host.fetch_asked();
    assert_eq!(path, "rom/bios.bin");
    bench.poll();
    assert!(bench.unchanged(), "the file is on its way");

    answer.send(Ok(BIOS_ROM.to_vec())).unwrap();
    bench.poll();
    assert_eq!(
        bench.said(),
        [done("Restarted with rom/bios.bin from the site")]
    );
    assert!(socket(&bench.runtime) == BIOS_ROM);
    assert!(at_the_reset_vector(&bench.runtime));
    assert!(bench.holds_its_floppy());
}

/// A file the site does not bring changes nothing: the message says why,
/// in the host's words (`platform::site_answer` has them for a browser),
/// and which ROM the machine runs on with. A fetch that could not be asked
/// at all, its channel closed, is told the same way.
#[test]
fn a_rom_the_site_does_not_bring_changes_nothing_and_says_why() {
    let mut bench = Bench::new();
    let file = &FILES[0];
    for _ in 0..3 {
        bench.execute(RomCommand::FromSite(file));
    }
    let mut fetches = bench.host.fetches();
    let (_, never_asked) = fetches.pop().unwrap();
    let (_, failing) = fetches.pop().unwrap();
    let (_, missing) = fetches.pop().unwrap();

    missing
        .send(Err("The site has no rom/bios.bin".to_string()))
        .unwrap();
    failing
        .send(Err("Cannot fetch rom/bios.bin: Failed to fetch".to_string()))
        .unwrap();
    drop(never_asked);
    bench.poll();

    assert_eq!(
        bench.said(),
        [
            failed("The site has no rom/bios.bin. The machine runs on with a busy loop"),
            failed(
                "Cannot fetch rom/bios.bin: Failed to fetch. The machine runs on with a busy loop"
            ),
            failed(
                "The site could not be asked for rom/bios.bin. The machine runs on with a busy loop"
            ),
        ]
    );
    assert!(bench.unchanged());
}

/// An image that cannot be a ROM, such as a disk image chosen by mistake,
/// changes nothing: the message says why, and which ROM the machine runs on
/// with.
#[test]
fn an_image_that_cannot_be_a_rom_changes_nothing_and_says_why() {
    let mut bench = Bench::new();
    bench.execute(RomCommand::FromFile);
    let (_, answer) = bench.host.question();
    let floppy = HostFile::Contents {
        name: "disk.img".to_string(),
        bytes: vec![0; 1_474_560],
    };
    answer.send(vec![floppy]).unwrap();
    bench.poll();

    assert_eq!(
        bench.said(),
        [failed(
            "disk.img is 1474560 bytes, and a ROM image is 16 bytes to 64 KB. The machine runs on with a busy loop"
        )]
    );
    assert!(bench.unchanged());
}

/// A dialog the user closes without choosing asks for nothing, and one
/// whose channel closes without an answer could not be shown: either way
/// the machine runs on as it was, and the loader waits for neither.
#[test]
fn a_dialog_that_brings_no_file_changes_nothing() {
    let mut bench = Bench::new();
    bench.execute(RomCommand::FromFile);
    bench.execute(RomCommand::FromFile);
    let mut questions = bench.host.questions();
    let (_, not_shown) = questions.pop().unwrap();
    let (_, closed) = questions.pop().unwrap();

    closed.send(Vec::new()).unwrap();
    drop(not_shown);
    bench.poll();
    bench.poll();
    assert_eq!(bench.said(), [failed("The file dialog could not be shown")]);
    assert!(bench.unchanged());
    assert_eq!(bench.loader.chosen.waiting(), 0);
}

/// The session restarts the machine with an image chosen through Machine >
/// Load ROM on the first frame after the answer came, whether the overlay
/// is shown or not, and the overlay says which ROM runs then.
#[test]
fn the_session_restarts_the_machine_on_the_frame_after_the_answer_came() {
    let host = ByHand::default();
    let (kbd_tx, _sent) = channel();
    let mut session = Session::new(machine(), kbd_tx);
    session.roms = RomLoader::on(Box::new(host.clone()));
    session.overlay_key(ElementState::Pressed, false);
    let mut shown = Driver::new(session);
    shown.frame(Vec::new());

    shown.click(Role::Button, "Machine");
    shown.click(Role::Button, &submenu("Load ROM"));
    shown.click(Role::Button, "From a file...");
    let (_, answer) = host.question();

    // Hidden, the overlay runs no pass, and the answer is taken up all the
    // same.
    shown.target.overlay_key(ElementState::Pressed, false);
    let image = marking_rom(0x1000, 0x4D52);
    let handed = HostFile::Contents {
        name: "tiny.rom".to_string(),
        bytes: image,
    };
    answer.send(vec![handed]).unwrap();
    shown
        .target
        .frame(|| panic!("no input is gathered for an overlay that shows nothing"));
    assert_eq!(shown.target.runtime.rom(), Some("tiny.rom"));
    assert_eq!(
        shown.target.runtime.cpu.memory.read_word(MARK_AT),
        0x4D52,
        "the frame ran the new ROM"
    );

    shown.target.overlay_key(ElementState::Pressed, false);
    let screen = shown.frame(Vec::new());
    assert!(
        screen.shows("Restarted with tiny.rom"),
        "{:?}",
        screen.texts()
    );
}
