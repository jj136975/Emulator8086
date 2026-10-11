//! The Start panel's commands carried out on a host whose site the tests
//! answer by hand, when they like: what a start asks of the site, what the
//! machine is once the disk has come, and what the messages say. The disks
//! are small, made by hand: the Insert takes an image of any size but none.
//! The panel that asks for the starts is tested beside it
//! (`overlay::start`), and what the session does with the overlay after a
//! start in `session::tests`.

use std::sync::mpsc::channel;

use super::*;
use crate::gui::host::testing::ByHand;
use crate::site::{Kind, of_kind};
use emu8086_core::io::disk::DiskImage;
use emu8086_core::io::disk_spec::parse_disk_specs;
use emu8086_core::io::keyboard::KeyboardController;

/// What the machine runs before a start: a jump to itself.
const BUSY_ROM: [u8; 16] = {
    let mut rom = [0x90; 16]; // NOP
    rom[0] = 0xEB; // JMP $
    rom[1] = 0xFE;
    rom
};

/// The floppy of the site: MS-DOS 4.00.
fn floppy() -> &'static SiteFile {
    of_kind(Kind::Floppy).next().expect("the site has a floppy")
}

/// The hard disk of the site: DOOM.
fn hard_disk() -> &'static SiteFile {
    of_kind(Kind::HardDisk)
        .next()
        .expect("the site has a hard disk")
}

/// A floppy image of 160 KB, the smallest floppy there is, every byte of
/// it `fill`.
fn small_floppy(fill: u8) -> Vec<u8> {
    vec![fill; 163_840]
}

/// A hard disk image of two cylinders of 16 heads and 63 sectors, every
/// byte of it `fill`.
fn small_hard_disk(fill: u8) -> Vec<u8> {
    vec![fill; 2 * 16 * 63 * 512]
}

/// A machine with a 720 KB floppy in A: and no hard disk, the bootstrap
/// trying A: and then the first hard disk, running `BUSY_ROM`, its CPU away
/// from the reset vector, as one that has run.
fn machine() -> Runtime {
    let (_keys, key_events) = channel();
    let mut runtime = Runtime::new(
        parse_disk_specs(&["a:memory:720".to_string()]).unwrap(),
        Vec::new(),
        None,
        false,
        KeyboardController::with_receiver(key_events),
        &HostClock,
    )
    .unwrap();
    runtime.load_rom_image(&BUSY_ROM, "a busy loop").unwrap();
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

/// The starter on a host whose site the test answers, the machine it
/// starts, and the messages it reports in.
struct Bench {
    starter: Starter,
    host: ByHand,
    runtime: Runtime,
    messages: Messages,
}

impl Bench {
    fn new() -> Self {
        let host = ByHand::default();
        Self {
            starter: Starter::on(Box::new(host.clone())),
            host,
            runtime: machine(),
            messages: Messages::default(),
        }
    }

    /// What a frame does first: takes up the answer that came, if one did.
    fn poll(&mut self) -> Option<StartCommand> {
        self.starter.poll(&mut self.runtime, &mut self.messages)
    }

    /// What the messages said since the last look.
    fn said(&mut self) -> Vec<(String, bool)> {
        self.messages.take_said()
    }

    /// The disk in `drive`, if it holds one.
    fn disk(&self, drive: u8) -> Option<Held> {
        let mut disks = self.runtime.disks.borrow_mut();
        let (_, image) = disks.drives.iter().find(|(held, _)| *held == drive)?;
        let (name, shape, read_only) = (
            image.name().map(str::to_string),
            (image.cylinders, image.heads, image.sectors_per_track),
            image.readonly,
        );
        let bytes = disk_ops::contents(&mut disks, drive).unwrap();
        Some(Held {
            name,
            shape,
            read_only,
            bytes,
        })
    }

    /// Whether the machine is as `machine` made it: its 720 KB floppy in
    /// A:, no hard disk, the boot order the default, its CPU where it was.
    fn unchanged(&self) -> bool {
        let floppy = self.disk(0x00);
        let disks = self.runtime.disks.borrow();
        floppy.is_some_and(|floppy| floppy.name.is_none() && floppy.shape == (80, 2, 9))
            && disks.drives.len() == 1
            && disks.boot_order() == [0x00, 0x80]
            && self.runtime.cpu.registers.cs.reg().word() == 0x1234
    }
}

/// A disk in a drive, as a test reads it.
#[derive(Debug, PartialEq, Eq)]
struct Held {
    /// The name of the file it was made of.
    name: Option<String>,
    /// Its cylinders, heads and sectors per track.
    shape: (u16, u8, u8),
    read_only: bool,
    bytes: Vec<u8>,
}

/// A floppy of the site is fetched from it, by its path relative to the
/// page, and the machine runs on as it was, however many frames go by,
/// until it has come. Then it goes into A:, in place of the floppy there,
/// held in memory and known by the name of its file; the bootstrap tries
/// A: first, then the first hard disk; and the machine restarts at the
/// reset vector. The message says what it restarted with.
#[test]
fn a_floppy_of_the_site_goes_into_a_and_the_machine_restarts_from_it() {
    let mut bench = Bench::new();
    let start = StartCommand::of(floppy(), false);
    bench.starter.execute(start);
    let (path, answer) = bench.host.fetch_asked();
    assert_eq!(path, "disks/msdos400.img");
    for _ in 0..3 {
        assert_eq!(bench.poll(), None);
    }
    assert!(bench.unchanged(), "the disk is on its way");
    assert_eq!(bench.said(), []);

    answer.send(Ok(small_floppy(0xF6))).unwrap();
    assert_eq!(bench.poll(), Some(start));

    assert_eq!(
        bench.said(),
        [done("Restarted with MS-DOS 4.00, the floppy in A:")]
    );
    assert_eq!(
        bench.disk(0x00),
        Some(Held {
            name: Some("msdos400.img".to_string()),
            shape: (40, 1, 8),
            read_only: false,
            bytes: small_floppy(0xF6),
        })
    );
    assert_eq!(bench.runtime.disks.borrow().drives.len(), 1);
    assert_eq!(bench.runtime.disks.borrow().boot_order(), [0x00, 0x80]);
    assert!(at_the_reset_vector(&bench.runtime));
    assert_eq!(bench.starter.starting(), None, "the start is over");
}

/// A hard disk of the site goes in as the first hard disk, made as the tool
/// that made the site's disk and booted it made it
/// (`DiskImage::hard_disk_from_bytes`): whole cylinders of 16 heads and 63
/// sectors, known by the name of its file. The bootstrap tries it first,
/// then A:, whose floppy stays where it is; the machine restarts at the
/// reset vector, and the message says what it restarted with.
#[test]
fn a_hard_disk_of_the_site_goes_in_as_the_first_hard_disk_and_a_stays() {
    let mut bench = Bench::new();
    let start = StartCommand::of(hard_disk(), false);
    bench.starter.execute(start);
    let (path, answer) = bench.host.fetch_asked();
    assert_eq!(path, "disks/doom8088.img");
    answer.send(Ok(small_hard_disk(0x5A))).unwrap();
    assert_eq!(bench.poll(), Some(start));

    assert_eq!(
        bench.said(),
        [done("Restarted with DOOM, the hard disk as C:")]
    );
    let made = DiskImage::hard_disk_from_bytes(small_hard_disk(0x5A), "doom8088.img", false);
    assert_eq!(
        bench.disk(0x80),
        Some(Held {
            name: made.name().map(str::to_string),
            shape: (made.cylinders, made.heads, made.sectors_per_track),
            read_only: made.readonly,
            bytes: small_hard_disk(0x5A),
        })
    );
    assert_eq!(made.cylinders, 2);
    let floppy = bench.disk(0x00).expect("A: keeps its floppy");
    assert_eq!((floppy.name, floppy.shape), (None, (80, 2, 9)));
    assert_eq!(bench.runtime.disks.borrow().boot_order(), [0x80, 0x00]);
    assert!(at_the_reset_vector(&bench.runtime));
}

/// A disk the site does not bring changes nothing: no disk goes in, the
/// boot order stays, the machine is not restarted, and the message says
/// why, in the host's words (`platform::site_answer` has them for a
/// browser), and that the machine runs on as it was. A fetch that could not
/// be asked at all, its channel closed, is told the same way. Each failed
/// start is over: the next one is asked of the site.
#[test]
fn a_disk_the_site_does_not_bring_changes_nothing_and_says_why() {
    let mut bench = Bench::new();
    let start = StartCommand::of(floppy(), false);

    bench.starter.execute(start);
    let (_, missing) = bench.host.fetch_asked();
    missing
        .send(Err("The site has no disks/msdos400.img".to_string()))
        .unwrap();
    assert_eq!(bench.poll(), None);

    bench.starter.execute(start);
    let (_, failing) = bench.host.fetch_asked();
    failing
        .send(Err(
            "Cannot fetch disks/msdos400.img: Failed to fetch".to_string()
        ))
        .unwrap();
    assert_eq!(bench.poll(), None);

    bench.starter.execute(start);
    let (_, never_asked) = bench.host.fetch_asked();
    drop(never_asked);
    assert_eq!(bench.poll(), None);

    assert_eq!(
        bench.said(),
        [
            failed("The site has no disks/msdos400.img. The machine runs on as it was"),
            failed(
                "Cannot fetch disks/msdos400.img: Failed to fetch. The machine runs on as it was"
            ),
            failed(
                "The site could not be asked for disks/msdos400.img. The machine runs on as it was"
            ),
        ]
    );
    assert!(bench.unchanged());
    assert_eq!(bench.starter.starting(), None);
}

/// A disk the Insert refuses, which the site brought with no bytes,
/// changes nothing either: the message says why, in the Insert's words, and
/// that the machine runs on as it was.
#[test]
fn a_disk_the_insert_refuses_changes_nothing_and_says_why() {
    for disk in [floppy(), hard_disk()] {
        let mut bench = Bench::new();
        bench.starter.execute(StartCommand::of(disk, true));
        let (_, answer) = bench.host.fetch_asked();
        answer.send(Ok(Vec::new())).unwrap();
        assert_eq!(bench.poll(), None);

        let name = file_name(disk.path);
        assert_eq!(
            bench.said(),
            [failed(&format!(
                "{name} is empty: an image of no bytes is no disk. The machine runs on as it was"
            ))]
        );
        assert!(bench.unchanged(), "{name}");
    }
}

/// One start at a time: while the disk of one is on its way, the starter
/// says which, and a start asked for meanwhile is ignored, its disk not
/// asked of the site. Once the first is over, another can be asked for.
#[test]
fn a_start_asked_while_one_is_on_its_way_is_ignored() {
    let mut bench = Bench::new();
    assert_eq!(bench.starter.starting(), None);
    bench.starter.execute(StartCommand::of(floppy(), false));
    bench.starter.execute(StartCommand::of(hard_disk(), true));
    assert_eq!(bench.starter.starting(), Some(floppy()));
    let (path, answer) = bench.host.fetch_asked();
    assert_eq!(path, floppy().path, "the hard disk is not asked for");

    answer.send(Ok(small_floppy(0))).unwrap();
    assert_eq!(bench.poll(), Some(StartCommand::of(floppy(), false)));
    assert_eq!(bench.disk(0x80), None, "no hard disk went in");
    assert_eq!(bench.starter.starting(), None);

    bench.starter.execute(StartCommand::of(hard_disk(), true));
    assert_eq!(bench.starter.starting(), Some(hard_disk()));
    assert_eq!(bench.host.fetch_asked().0, hard_disk().path);
}

/// A disk is known by the name of its file: the last part of its path.
#[test]
fn a_disk_is_known_by_the_name_of_its_file() {
    assert_eq!(file_name("disks/msdos400.img"), "msdos400.img");
    assert_eq!(file_name("a/b/c.img"), "c.img");
    assert_eq!(file_name("top.img"), "top.img");
}
