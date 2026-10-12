//! The tests reach the controller the way the BIOS does: they load the
//! registers of a test machine and write to the trap port (`int13`). A disk
//! is held in memory unless the test is about the host file; files and
//! directories on the host live in the system temp directory and are
//! removed on drop.

use super::*;
use super::{fat::PartitionSlice, image::DiskStorage};
use crate::io::bus::IoDevice;
use crate::vm::runtime::{CpuFlag, Runtime};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

/// INT 13h AH=02 with AL=0 transfers 256 sectors, as the IBM PC BIOS
/// does, rather than none.
#[test]
fn int13h_al_zero_should_mean_256_sectors() {
    let mut rt = Runtime::new_test();

    // Pick a disk big enough for 256 sectors at CHS(0,0,1).
    // 10 MB hard disk: geometry is (cyl, 16, 63) — far more than 256 sectors.
    let mut img = DiskImage::new_in_memory_hard_disk(10);

    // Seed the first 256 sectors with a per-sector marker byte so we
    // can tell a real read from a silent no-op.
    for sector in 0..256u16 {
        let heads = img.heads as u16;
        let spt = img.sectors_per_track as u16;
        let c = sector / (heads * spt);
        let h = ((sector / spt) % heads) as u8;
        let s = ((sector % spt) + 1) as u8;
        let marker = sector as u8;
        let data: Vec<u8> = vec![marker; 512];
        img.write_sectors(c, h, s, 1, &data).expect("seed write");
    }

    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x80, img);

    // INT 13h AH=02, AL=0 (=>256), CHS=(0,0,1), ES:BX=0x1000:0000, DL=0x80
    rt.cpu.registers.ax.set_high(0x02);
    rt.cpu.registers.ax.set_low(0x00);
    rt.cpu.registers.cx.set_high(0x00);
    rt.cpu.registers.cx.set_low(0x01);
    rt.cpu.registers.dx.set_high(0x00);
    rt.cpu.registers.dx.set_low(0x80);
    rt.cpu.registers.es.reg_mut().set(0x1000);
    rt.cpu.registers.bx.set(0x0000);

    disk_ctrl.port_out_byte(0xB0, 0x00, &mut rt.cpu);

    // A real BIOS places 256 * 512 = 131_072 bytes at 0x10000, with the
    // markers 0, 1, 2, ..., 255: the first bytes of sector 0 and of sector
    // 255 show both ends of the range.
    let sector_0_byte = rt.cpu.memory.read_byte(0x10000);
    let sector_255_byte = rt.cpu.memory.read_byte(0x10000 + 255 * 512);

    assert_eq!(
        sector_0_byte, 0,
        "Sector 0 marker missing — AL=0 was treated as 0 sectors rather \
         than 256"
    );
    assert_eq!(
        sector_255_byte, 255,
        "Sector 255 marker missing — AL=0 only transferred a partial \
         range (or nothing)"
    );
}

/// INT 13h AH=08 packs the geometry as CH = max cylinder bits 0-7,
/// CL bits 6-7 = max cylinder bits 8-9, CL bits 0-5 = SPT, DH = max head;
/// for a 1.44MB floppy that is max_cyl=79, SPT=18, max_head=1.
#[test]
fn int13h_get_drive_params_floppy_returns_correct_geometry() {
    let mut rt = Runtime::new_test();
    let img = DiskImage::new_in_memory_sized(1_474_560, false);
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, img);

    rt.cpu.registers.ax.set_high(0x08);
    rt.cpu.registers.dx.set_low(0x00); // drive A
    disk_ctrl.port_out_byte(0xB0, 0x00, &mut rt.cpu);

    let ch = rt.cpu.registers.cx.high();
    let cl = rt.cpu.registers.cx.low();
    let dh = rt.cpu.registers.dx.high();

    let max_cyl = (ch as u16) | (((cl as u16) & 0xC0) << 2);
    let spt = cl & 0x3F;
    assert_eq!(max_cyl, 79, "max_cyl for 80-cylinder floppy should be 79");
    assert_eq!(spt, 18, "SPT=18 for 1.44M floppy");
    assert_eq!(dh, 1, "max_head for 2-head floppy should be 1");
    assert_eq!(rt.cpu.registers.ax.high(), 0x00, "AH=0 on success");
}

/// A write to a read-only disk fails with CF set and AH=0x03 (write
/// protected).
#[test]
fn int13h_write_to_readonly_disk_returns_0x03() {
    let mut rt = Runtime::new_test();
    // 1.44 MB floppy, readonly
    let img = DiskImage::new_in_memory_sized(1_474_560, true);
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, img);

    // INT 13h AH=03 (write), AL=1, CHS=(0,0,1), drive=0
    rt.cpu.registers.ax.set_high(0x03);
    rt.cpu.registers.ax.set_low(0x01);
    rt.cpu.registers.cx.set_high(0x00);
    rt.cpu.registers.cx.set_low(0x01);
    rt.cpu.registers.dx.set_high(0x00);
    rt.cpu.registers.dx.set_low(0x00);
    rt.cpu.registers.es.reg_mut().set(0x1000);
    rt.cpu.registers.bx.set(0x0000);

    disk_ctrl.port_out_byte(0xB0, 0x00, &mut rt.cpu);

    assert!(
        rt.cpu.check_flag(CpuFlag::Carry),
        "CF should be set for write-protect error"
    );
    assert_eq!(
        rt.cpu.registers.ax.high(),
        0x03,
        "AH should be 0x03 (write protected), got 0x{:02X}",
        rt.cpu.registers.ax.high()
    );
}

/// A CHS sector number above the track's sectors per track (S=20 on a
/// 1.44 MB floppy with SPT=18) fails with CF set and AH=0x04 (sector
/// not found) instead of underflowing the per-track sector count.
#[test]
fn int13h_sector_above_spt_should_return_error_not_underflow() {
    let mut rt = Runtime::new_test();

    // 1.44 MB floppy: (80, 2, 18). SPT = 18.
    let img = DiskImage::new_in_memory_sized(1_474_560, false);
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, img);

    // INT 13h AH=02, AL=1, CHS=(0, 0, 20). S=20 > SPT=18 → should error.
    rt.cpu.registers.ax.set_high(0x02);
    rt.cpu.registers.ax.set_low(0x01);
    rt.cpu.registers.cx.set_high(0x00);
    rt.cpu.registers.cx.set_low(20); // S=20, cyl_hi=0
    rt.cpu.registers.dx.set_high(0x00);
    rt.cpu.registers.dx.set_low(0x00);
    rt.cpu.registers.es.reg_mut().set(0x1000);
    rt.cpu.registers.bx.set(0x0000);

    disk_ctrl.port_out_byte(0xB0, 0x00, &mut rt.cpu);

    assert!(
        rt.cpu.check_flag(CpuFlag::Carry),
        "Carry should be set on an invalid sector number"
    );
    assert_eq!(
        rt.cpu.registers.ax.high(),
        0x04,
        "AH should be 0x04 (sector not found); got 0x{:02X}",
        rt.cpu.registers.ax.high()
    );
}

/// A file in the host's temp directory that is removed again on drop.
struct ScratchFile(PathBuf);

impl ScratchFile {
    fn new(name: &str) -> Self {
        let path =
            std::env::temp_dir().join(format!("emu8086-test-{}-{}", std::process::id(), name));
        let _ = std::fs::remove_file(&path);
        Self(path)
    }

    fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for ScratchFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

/// Floppy-sized content in which every sector is filled with its own
/// number, so a read shows which sector it really came from.
fn numbered_sectors() -> Vec<u8> {
    (0..FLOPPY_144_SIZE as usize)
        .map(|i| (i / SECTOR_SIZE) as u8)
        .collect()
}

/// A new image whose file cannot be given its size leaves no file behind:
/// one left there, empty, would refuse the next try, since a new image
/// never replaces a file. No file system takes 2^64 - 1 bytes.
#[test]
fn an_image_that_cannot_be_made_leaves_no_file_behind() {
    let scratch = ScratchFile::new("unmade.img");
    assert!(DiskImage::create(scratch.path(), u64::MAX).is_err());
    assert!(!scratch.path().exists());

    let image = DiskImage::create(scratch.path(), FLOPPY_144_SIZE).unwrap();
    assert_eq!(image.total_bytes(), FLOPPY_144_SIZE);
}

// -----------------------------------------------------------------------
// Saving an image to a host file
// -----------------------------------------------------------------------

/// Saving a file-backed image onto the file it lives in must not open
/// that file for writing: truncating it would empty the image itself.
#[test]
fn save_onto_own_backing_file_keeps_the_image() {
    let scratch = ScratchFile::new("save-self.img");
    let content = numbered_sectors();
    std::fs::write(scratch.path(), &content).unwrap();

    let mut img = DiskImage::open(scratch.path(), false).unwrap();
    img.save_to_file(scratch.path())
        .expect("saving an image onto its own file");

    assert_eq!(std::fs::read(scratch.path()).unwrap(), content);
    assert_eq!(
        img.read_sectors(0, 1, 1, 1).unwrap(),
        content[18 * SECTOR_SIZE..19 * SECTOR_SIZE]
    );
}

#[test]
fn save_tells_a_copy_from_the_image_s_own_file() {
    let own = ScratchFile::new("save-own.img");
    let other = ScratchFile::new("save-other.img");
    std::fs::write(own.path(), numbered_sectors()).unwrap();

    let mut img = DiskImage::open(own.path(), false).unwrap();
    assert_eq!(
        img.save_to_file(own.path()).unwrap(),
        SaveOutcome::AlreadyBacking
    );
    assert_eq!(
        img.save_to_file(other.path()).unwrap(),
        SaveOutcome::Written
    );
    assert_eq!(std::fs::read(other.path()).unwrap(), numbered_sectors());
}

/// The copy replaces an existing target as a whole, and nothing else is
/// left behind in the target's directory.
#[test]
fn save_replaces_an_existing_file_with_a_complete_copy() {
    let target = ScratchFile::new("save-target.img");
    std::fs::write(target.path(), vec![0xEEu8; 3 * FLOPPY_144_SIZE as usize]).unwrap();

    let mut img = DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false);
    img.write_sectors(0, 0, 1, 1, &[0x5Au8; SECTOR_SIZE])
        .unwrap();
    assert_eq!(
        img.save_to_file(target.path()).unwrap(),
        SaveOutcome::Written
    );

    let saved = std::fs::read(target.path()).unwrap();
    assert_eq!(saved.len() as u64, FLOPPY_144_SIZE);
    assert_eq!(saved[..SECTOR_SIZE], [0x5Au8; SECTOR_SIZE]);
    assert!(saved[SECTOR_SIZE..].iter().all(|b| *b == 0));

    let target_name = target
        .path()
        .file_name()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    let leftovers: Vec<String> = std::fs::read_dir(target.path().parent().unwrap())
        .unwrap()
        .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
        .filter(|name| name.starts_with(&target_name) && *name != target_name)
        .collect();
    assert!(
        leftovers.is_empty(),
        "temporary files left behind: {:?}",
        leftovers
    );
}

#[test]
fn controller_knows_which_drive_lives_in_a_host_file() {
    let scratch = ScratchFile::new("backing.img");
    std::fs::write(scratch.path(), numbered_sectors()).unwrap();

    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false));
    disk_ctrl.attach(0x01, DiskImage::open(scratch.path(), false).unwrap());

    assert_eq!(disk_ctrl.drive_backed_by(scratch.path()), Some(0x01));
    assert_eq!(
        disk_ctrl.drive_backed_by(&scratch.path().with_extension("other")),
        None
    );
}

/// A drive holds one disk: attaching to a drive that already has one
/// replaces it, so a later detach leaves the drive empty.
#[test]
fn attach_replaces_the_disk_in_the_drive() {
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false));
    disk_ctrl.attach(0x00, DiskImage::new_in_memory_sized(737_280, false));
    assert_eq!(disk_ctrl.find_drive(0x00).unwrap().total_bytes(), 737_280);
    assert_eq!(disk_ctrl.drives.len(), 1);

    assert!(disk_ctrl.detach(0x00).is_some());
    assert!(disk_ctrl.find_drive(0x00).is_none());
}

// -----------------------------------------------------------------------
// Floppy drives: presence, drive type and change line
// -----------------------------------------------------------------------

/// Calls the disk trap the way the BIOS INT 13h handler does.
fn int13(disk_ctrl: &mut DiskController, rt: &mut Runtime, ax: u16, cx: u16, dx: u16) {
    rt.cpu.registers.ax.set(ax);
    rt.cpu.registers.cx.set(cx);
    rt.cpu.registers.dx.set(dx);
    disk_ctrl.port_out_byte(DISK_TRAP_PORT, 0x00, &mut rt.cpu);
}

fn carry(rt: &Runtime) -> bool {
    rt.cpu.check_flag(CpuFlag::Carry)
}

fn floppy(size: u64) -> DiskImage {
    DiskImage::new_in_memory_sized(size, false)
}

/// Drives A: and B: exist whether or not a disk is in them. If an empty
/// one were reported as "no such drive", DOS would fall back to a 360K
/// drive without change line.
#[test]
fn empty_floppy_drive_reports_the_parameters_of_a_144m_drive() {
    let mut rt = Runtime::new_test();
    rt.cpu.memory.write_word(0x78, 0xEFC7); // INT 1Eh -> diskette parameter table
    rt.cpu.memory.write_word(0x7A, 0xF000);
    let mut disk_ctrl = DiskController::new();

    for drive in [0x00u16, 0x01] {
        rt.cpu.registers.bx.set(0xBBBB);
        int13(&mut disk_ctrl, &mut rt, 0x08EE, 0xCCCC, 0xDD00 | drive);
        assert!(!carry(&rt), "AH=08h must succeed for drive {:02X}", drive);
        assert_eq!(rt.cpu.registers.ax.word(), 0x0000);
        assert_eq!(rt.cpu.registers.bx.word(), 0x0004, "drive type 1.44M");
        assert_eq!(
            rt.cpu.registers.cx.word(),
            0x4F12,
            "79 cylinders max, 18 sectors"
        );
        assert_eq!(rt.cpu.registers.dx.word(), 0x0102, "head 1 max, two drives");
        assert_eq!(rt.cpu.registers.es.reg().word(), 0xF000);
        assert_eq!(rt.cpu.registers.di.word(), 0xEFC7);
    }
}

#[test]
fn floppy_units_beyond_b_exist_only_once_configured() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();

    int13(&mut disk_ctrl, &mut rt, 0x0800, 0, 0x0002);
    assert!(carry(&rt), "unit 2 is not installed");
    assert_eq!(rt.cpu.registers.ax.high(), 0x07);
    int13(&mut disk_ctrl, &mut rt, 0x1500, 0, 0x0002);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x00, "no such drive");

    disk_ctrl.attach(0x02, floppy(737_280));
    int13(&mut disk_ctrl, &mut rt, 0x1500, 0, 0x0002);
    assert_eq!(rt.cpu.registers.ax.high(), 0x02);
    int13(&mut disk_ctrl, &mut rt, 0x0800, 0, 0x0000);
    assert_eq!(
        rt.cpu.registers.dx.low(),
        3,
        "A:, B: and the configured unit 2"
    );

    // The unit stays installed after its disk is ejected.
    disk_ctrl.detach(0x02);
    int13(&mut disk_ctrl, &mut rt, 0x1500, 0, 0x0002);
    assert_eq!(rt.cpu.registers.ax.high(), 0x02);
}

/// Every floppy drive has a change line, whatever is in it, small images
/// and empty drives included. Without one DOS is left with its "nobody
/// can change a disk in under two seconds" rule.
#[test]
fn floppy_drives_report_change_line_support() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();

    int13(&mut disk_ctrl, &mut rt, 0x15FF, 0, 0x0000);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x02, "empty drive A:");

    for size in [163_840u64, 368_640, 737_280, 1_228_800, FLOPPY_144_SIZE] {
        disk_ctrl.detach(0x00);
        disk_ctrl.attach(0x00, floppy(size));
        int13(&mut disk_ctrl, &mut rt, 0x15FF, 0, 0x0000);
        assert!(!carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x02, "{}-byte image", size);
    }
}

/// AH=16h is how the guest learns that a disk was swapped: the change
/// is reported once, by the drive it happened in.
#[test]
fn change_line_reports_a_swap_exactly_once() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));

    // The disk that was put in before power-on counts as a change too.
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x06);
    assert_eq!(
        rt.cpu.memory.read_byte(0x0441),
        0x06,
        "status byte at 40:41"
    );

    for _ in 0..2 {
        int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
        assert!(!carry(&rt), "no change since the last query");
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);
    }

    // What putting another disk into the drive from the host does.
    disk_ctrl.detach(0x00);
    disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x06);
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x00);

    // The latch belongs to the drive: B: is unaffected.
    disk_ctrl.attach(0x01, floppy(FLOPPY_144_SIZE));
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert_eq!(rt.cpu.registers.ax.high(), 0x00);
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0001);
    assert_eq!(rt.cpu.registers.ax.high(), 0x06);
}

#[test]
fn change_line_of_an_empty_drive_is_not_ready_and_hard_disks_have_none() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(1));

    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x80, "no disk in A:");

    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0080);
    assert!(carry(&rt));
    assert_eq!(
        rt.cpu.registers.ax.high(),
        0x01,
        "invalid function for a hard disk"
    );

    // Ejecting counts as a change of the (now empty) drive, not of the
    // next disk: inserting one raises the line again.
    disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    disk_ctrl.detach(0x00);
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert_eq!(rt.cpu.registers.ax.high(), 0x80);
    disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert_eq!(rt.cpu.registers.ax.high(), 0x06);
}

#[test]
fn transfers_on_an_empty_drive_time_out() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    rt.cpu.registers.es.reg_mut().set(0x1000);
    rt.cpu.registers.bx.set(0);

    for function in [0x02u16, 0x03, 0x04, 0x05] {
        int13(
            &mut disk_ctrl,
            &mut rt,
            function << 8 | 0x01,
            0x0001,
            0x0000,
        );
        assert!(carry(&rt));
        assert_eq!(rt.cpu.registers.ax.high(), 0x80, "AH={:02X}h", function);
    }
}

fn volume_serial(image: &mut DiskImage) -> u32 {
    let boot = image.read_sectors(0, 0, 1, 1).unwrap();
    u32::from_le_bytes([boot[0x27], boot[0x28], boot[0x29], boot[0x2A]])
}

/// DOS compares volume serial numbers to decide whether a disk was
/// changed, so two floppies formatted here must not carry the same one.
#[test]
fn formatted_floppies_get_distinct_volume_serials() {
    let mut first = floppy(FLOPPY_144_SIZE);
    let mut second = floppy(FLOPPY_144_SIZE);
    first.format_fat(&FixedClock).unwrap();
    second.format_fat(&FixedClock).unwrap();
    assert_ne!(volume_serial(&mut first), volume_serial(&mut second));
}

/// A serial comes from the time on the clock that was passed in and from
/// the number of volumes formatted before: disks formatted at different
/// times differ, and so do disks formatted within one tick of the clock.
#[test]
fn volume_serial_follows_the_clock_and_the_order_of_formatting() {
    let now = FixedClock.unix_nanos();
    assert_ne!(fat::volume_serial(now, 0), fat::volume_serial(now + 1, 0));
    assert_ne!(
        fat::volume_serial(now, 0),
        fat::volume_serial(now + 1_000_000_000, 0)
    );
    assert_ne!(fat::volume_serial(now, 0), fat::volume_serial(now, 1));
}

/// A file written into the disk from the host changes what the guest
/// has cached of it just like a swap does.
#[test]
fn host_side_write_raises_the_change_line() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    let mut image = floppy(FLOPPY_144_SIZE);
    image.format_fat(&FixedClock).unwrap();
    disk_ctrl.attach(0x00, image);
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000); // consume the insertion

    disk_ctrl
        .put_file(0x00, b"hello", "HELLO.TXT", &FixedClock)
        .unwrap();
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x06);
    int13(&mut disk_ctrl, &mut rt, 0x1600, 0, 0x0000);
    assert_eq!(rt.cpu.registers.ax.high(), 0x00);

    assert!(
        disk_ctrl
            .put_file(0x01, b"hello", "HELLO.TXT", &FixedClock)
            .is_err(),
        "B: is empty"
    );
}

/// A new controller holds no disk: it has the floppy drives A: and B:,
/// empty, no hard disk, and a bootstrap that tries A:, then the first hard
/// disk. No reboot is due on it.
#[test]
fn a_new_controller_has_two_empty_floppy_drives_and_boots_a_then_c() {
    let controller = DiskController::default();
    assert!(controller.drives.is_empty());
    assert_eq!(controller.floppy_drive_count(), 2);
    assert_eq!(controller.hard_disk_count(), 0);
    assert_eq!(controller.boot_order(), [0x00, 0x80]);
    assert!(!controller.reboot_due());
}

/// A hard disk has no change line to tell the guest with. What the host
/// does to one leaves a reboot due instead, until the bootstrap starts
/// again: a write to the boot order port, which rewinds the list. What the
/// host does to a floppy leaves none due, nor does a copy that a
/// write-protected or missing hard disk refused untouched.
#[test]
fn host_side_change_of_a_hard_disk_leaves_a_reboot_due_until_the_bootstrap() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    let mut hard_disk = DiskImage::new_in_memory_hard_disk(1);
    hard_disk.format_fat(&FixedClock).unwrap();
    // What INT 19h does first.
    let bootstrap = |disk_ctrl: &mut DiskController, rt: &mut Runtime| {
        disk_ctrl.port_out_byte(BOOT_ORDER_PORT, 0x00, &mut rt.cpu);
    };
    assert!(!disk_ctrl.reboot_due());

    disk_ctrl.attach(0x00, formatted_floppy());
    disk_ctrl
        .put_file(0x00, b"hello", "HELLO.TXT", &FixedClock)
        .unwrap();
    disk_ctrl.detach(0x00);
    assert!(!disk_ctrl.reboot_due(), "the change line tells of a floppy");

    disk_ctrl.attach(0x80, hard_disk);
    assert!(disk_ctrl.reboot_due(), "inserted");
    bootstrap(&mut disk_ctrl, &mut rt);
    assert!(!disk_ctrl.reboot_due());

    disk_ctrl
        .put_file(0x80, b"hello", "HELLO.TXT", &FixedClock)
        .unwrap();
    assert!(disk_ctrl.reboot_due(), "written to");
    bootstrap(&mut disk_ctrl, &mut rt);

    disk_ctrl.find_drive(0x80).unwrap().readonly = true;
    assert!(
        disk_ctrl
            .put_file(0x80, b"hello", "OTHER.TXT", &FixedClock)
            .is_err()
    );
    assert!(
        disk_ctrl
            .put_dir(0x80, &std::env::temp_dir(), &FixedClock)
            .is_err()
    );
    assert!(
        disk_ctrl
            .put_file(0x81, b"hello", "HELLO.TXT", &FixedClock)
            .is_err()
    );
    assert!(!disk_ctrl.reboot_due(), "no disk was touched");

    // Reading the boot order takes nothing away.
    disk_ctrl.detach(0x80);
    assert!(disk_ctrl.reboot_due(), "removed");
    disk_ctrl.port_in_byte(BOOT_ORDER_PORT, &mut rt.cpu);
    assert!(disk_ctrl.reboot_due());
    bootstrap(&mut disk_ctrl, &mut rt);
    assert!(disk_ctrl.detach(0x80).is_none());
    assert!(!disk_ctrl.reboot_due(), "nothing was there to remove");
}

// -----------------------------------------------------------------------
// Geometry of image files
// -----------------------------------------------------------------------

/// The contents of a 1.44M floppy with an empty FAT volume.
fn formatted_floppy_bytes() -> Vec<u8> {
    let mut image = floppy(FLOPPY_144_SIZE);
    image.format_fat(&FixedClock).unwrap();
    (0..160u16)
        .flat_map(|track| {
            image
                .read_sectors(track / 2, (track % 2) as u8, 1, 18)
                .unwrap()
        })
        .collect()
}

fn geometry_of(path: &Path) -> (u16, u8, u8) {
    let image = DiskImage::open(path, false).unwrap();
    (image.cylinders, image.heads, image.sectors_per_track)
}

/// DOS addresses a disk with the heads and sectors per track its boot
/// sector states. An image whose size is not one of the floppy formats
/// has to get that geometry: with any other, every sector DOS asks for
/// comes from the wrong place.
#[test]
fn image_of_unlisted_size_takes_its_geometry_from_the_boot_sector() {
    let scratch = ScratchFile::new("geometry-bpb.img");

    // A 1.44M disk with one sector of padding behind it.
    let mut bytes = formatted_floppy_bytes();
    bytes.extend_from_slice(&[0u8; SECTOR_SIZE]);
    std::fs::write(scratch.path(), &bytes).unwrap();
    assert_eq!(geometry_of(scratch.path()), (80, 2, 18));

    // A 1.68M disk: 80 cylinders of 21 sectors per track.
    let mut bytes = formatted_floppy_bytes();
    bytes.resize(80 * 2 * 21 * SECTOR_SIZE, 0);
    bytes[0x13..0x15].copy_from_slice(&3360u16.to_le_bytes());
    bytes[0x18..0x1A].copy_from_slice(&21u16.to_le_bytes());
    std::fs::write(scratch.path(), &bytes).unwrap();
    assert_eq!(geometry_of(scratch.path()), (80, 2, 21));

    // A volume that ends inside a cylinder still gets that cylinder.
    bytes[0x13..0x15].copy_from_slice(&3340u16.to_le_bytes());
    std::fs::write(scratch.path(), &bytes).unwrap();
    assert_eq!(geometry_of(scratch.path()), (80, 2, 21));
}

/// The size table decides for the floppy formats, and an image without
/// a credible boot sector keeps the geometry its size gives it.
#[test]
fn boot_sector_is_consulted_only_when_it_is_needed_and_credible() {
    let scratch = ScratchFile::new("geometry-fallback.img");

    // A floppy format, whatever the boot sector claims.
    let mut bytes = formatted_floppy_bytes();
    bytes[0x18..0x1A].copy_from_slice(&21u16.to_le_bytes());
    std::fs::write(scratch.path(), &bytes).unwrap();
    assert_eq!(geometry_of(scratch.path()), (80, 2, 18));

    // No boot sector at all.
    std::fs::write(
        scratch.path(),
        vec![0u8; FLOPPY_144_SIZE as usize + SECTOR_SIZE],
    )
    .unwrap();
    assert_eq!(geometry_of(scratch.path()), (2, 16, 63));

    // Boot sectors whose parameter block cannot be right.
    for (offset, value) in [
        (0x0Busize, 1024u16), // bytes per sector
        (0x13, 0),            // no sectors
        (0x18, 0),            // sectors per track
        (0x18, 64),
        (0x1A, 0), // heads
        (0x1A, 256),
    ] {
        let mut bytes = formatted_floppy_bytes();
        bytes.extend_from_slice(&[0u8; SECTOR_SIZE]);
        bytes[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
        std::fs::write(scratch.path(), &bytes).unwrap();
        assert_eq!(
            geometry_of(scratch.path()),
            (2, 16, 63),
            "word at {:#x} = {}",
            offset,
            value
        );
    }
    let mut bytes = formatted_floppy_bytes();
    bytes.extend_from_slice(&[0u8; SECTOR_SIZE]);
    bytes[0] = 0x00; // no jump instruction: not a boot sector
    std::fs::write(scratch.path(), &bytes).unwrap();
    assert_eq!(geometry_of(scratch.path()), (2, 16, 63));
}

// -----------------------------------------------------------------------
// Images a host hands over as their contents, and takes back as such
// -----------------------------------------------------------------------

fn geometry(image: &DiskImage) -> (u16, u8, u8) {
    (image.cylinders, image.heads, image.sectors_per_track)
}

/// An image handed over as its contents is the disk its file would be: the
/// floppy format its size is, whatever its boot sector says; else what a
/// credible boot sector states; else a hard disk of its size.
#[test]
fn an_image_from_bytes_has_the_geometry_its_file_would_have() {
    let scratch = ScratchFile::new("geometry-bytes.img");
    let formatted = formatted_floppy_bytes();

    let mut lying = formatted.clone();
    lying[0x18..0x1A].copy_from_slice(&21u16.to_le_bytes());
    let mut padded = formatted.clone();
    padded.extend_from_slice(&[0u8; SECTOR_SIZE]);
    let mut wide = formatted.clone();
    wide.resize(80 * 2 * 21 * SECTOR_SIZE, 0);
    wide[0x13..0x15].copy_from_slice(&3360u16.to_le_bytes());
    wide[0x18..0x1A].copy_from_slice(&21u16.to_le_bytes());
    let blank = vec![0u8; FLOPPY_144_SIZE as usize + SECTOR_SIZE];

    for (contents, expected) in [
        (formatted, (80, 2, 18)),
        (lying, (80, 2, 18)),
        (padded, (80, 2, 18)),
        (wide, (80, 2, 21)),
        (blank, (2, 16, 63)),
        (vec![0u8; 737_280], (80, 2, 9)),
        (Vec::new(), (1, 16, 63)),
    ] {
        std::fs::write(scratch.path(), &contents).unwrap();
        let size = contents.len();
        let image = DiskImage::from_bytes(contents, "floppy.img", false);
        assert_eq!(geometry(&image), expected, "{size} bytes");
        assert_eq!(geometry(&image), geometry_of(scratch.path()), "as its file");
    }

    let hard_disk = DiskImage::hard_disk_from_bytes(vec![0u8; 4 * 16 * 63 * 512], "c.vhd", true);
    assert_eq!(geometry(&hard_disk), (4, 16, 63));
    assert!(hard_disk.readonly);
    // A hard disk is whole cylinders, and has one at least.
    let odd = DiskImage::hard_disk_from_bytes(vec![0u8; 4 * 16 * 63 * 512 + 1000], "c.vhd", false);
    assert_eq!(geometry(&odd), (4, 16, 63));
    let tiny = DiskImage::hard_disk_from_bytes(vec![0u8; 1000], "c.vhd", false);
    assert_eq!(geometry(&tiny), (1, 16, 63));
}

/// An image held in memory reaches as far as its disk. Contents that end
/// before the geometry does are filled up with zeros, which the guest reads
/// and may write; contents that go on behind it stay as they are, out of
/// the guest's reach, and are given back with the rest.
#[test]
fn an_image_from_bytes_is_as_long_as_its_disk() {
    // A volume of 3340 sectors on tracks of 21: it ends inside its last
    // cylinder, which the disk has in full.
    let mut short = formatted_floppy_bytes();
    short.resize(3340 * SECTOR_SIZE, 0);
    short[0x13..0x15].copy_from_slice(&3340u16.to_le_bytes());
    short[0x18..0x1A].copy_from_slice(&21u16.to_le_bytes());
    let mut image = DiskImage::from_bytes(short, "short.img", false);
    assert_eq!(geometry(&image), (80, 2, 21));
    assert_eq!(
        image.read_sectors(79, 1, 21, 1).unwrap(),
        [0u8; SECTOR_SIZE]
    );
    image
        .write_sectors(79, 1, 21, 1, &[0x5Au8; SECTOR_SIZE])
        .unwrap();
    let contents = image.contents().unwrap();
    assert_eq!(contents.len(), 3360 * SECTOR_SIZE);
    assert_eq!(contents[3359 * SECTOR_SIZE..], [0x5Au8; SECTOR_SIZE]);

    // Four cylinders of a hard disk, and a tail that is no whole cylinder.
    let mut long = vec![0u8; 4 * 16 * 63 * SECTOR_SIZE];
    long.extend_from_slice(&[0xEEu8; 1000]);
    let mut image = DiskImage::hard_disk_from_bytes(long.clone(), "long.vhd", false);
    assert!(image.read_sectors(4, 0, 1, 1).is_err(), "behind the disk");
    assert!(
        image
            .write_sectors(4, 0, 1, 1, &[0u8; SECTOR_SIZE])
            .is_err()
    );
    assert_eq!(image.contents().unwrap(), long);
}

/// The contents of an image are what a save writes to its target, whether
/// the image lives in memory or in a file.
#[test]
fn the_contents_of_an_image_are_what_a_save_writes() {
    let own = ScratchFile::new("contents-own.img");
    let saved = ScratchFile::new("contents-saved.img");
    std::fs::write(own.path(), numbered_sectors()).unwrap();

    let mut on_file = DiskImage::open(own.path(), false).unwrap();
    on_file
        .write_sectors(0, 0, 1, 1, &[0x5Au8; SECTOR_SIZE])
        .unwrap();
    on_file.save_to_file(saved.path()).unwrap();
    assert_eq!(
        on_file.contents().unwrap(),
        std::fs::read(saved.path()).unwrap()
    );
    // Reading the whole file does not disturb the transfers that follow.
    assert_eq!(
        on_file.read_sectors(0, 0, 2, 1).unwrap(),
        [1u8; SECTOR_SIZE]
    );

    let mut in_memory = DiskImage::from_bytes(numbered_sectors(), "numbered.img", false);
    in_memory
        .write_sectors(79, 1, 18, 1, &[0xA5u8; SECTOR_SIZE])
        .unwrap();
    in_memory.save_to_file(saved.path()).unwrap();
    assert_eq!(
        in_memory.contents().unwrap(),
        std::fs::read(saved.path()).unwrap()
    );
}

/// An image made of a file's contents is known by the name of that file. A
/// blank image has no name, and one that lives in a file has its path.
#[test]
fn an_image_from_bytes_carries_the_name_of_its_file() {
    let scratch = ScratchFile::new("named.img");
    std::fs::write(scratch.path(), numbered_sectors()).unwrap();

    let handed_over = DiskImage::from_bytes(numbered_sectors(), "DOS 6.22 disk 1.img", false);
    assert_eq!(handed_over.name(), Some("DOS 6.22 disk 1.img"));
    assert!(handed_over.backing_path().is_none());
    let hard_disk = DiskImage::hard_disk_from_bytes(vec![0u8; 516_096], "c.vhd", false);
    assert_eq!(hard_disk.name(), Some("c.vhd"));

    assert_eq!(floppy(FLOPPY_144_SIZE).name(), None);
    let on_file = DiskImage::open(scratch.path(), false).unwrap();
    assert_eq!(on_file.name(), None);
    assert!(on_file.backing_path().is_some());
}

/// CHS addresses end at 1024 cylinders. An image held in memory that goes
/// on for a cylinder or more behind them says so, as an image file does;
/// one that fits says nothing.
#[test]
fn an_image_from_bytes_too_large_to_reach_in_full_says_so() {
    let cylinder = 16 * 63 * SECTOR_SIZE;
    let small = DiskImage::hard_disk_from_bytes(vec![0u8; 4 * cylinder], "small.vhd", false);
    assert_eq!(small.capacity_note(), None);

    // 1030 cylinders of zeros, which the allocator hands out untouched.
    let large = DiskImage::hard_disk_from_bytes(vec![0u8; 1030 * cylinder], "large.vhd", false);
    assert_eq!(large.cylinders, 1024);
    let note = large
        .capacity_note()
        .expect("the guest cannot reach it all");
    assert!(
        note.contains("506 MB") && note.contains("504 MB") && note.contains("1024 cylinders"),
        "{note}"
    );
}

// -----------------------------------------------------------------------
// Registers and status of the individual functions
// -----------------------------------------------------------------------

/// AH=08h returns AX = 0 and, for a floppy drive, the drive type in BX.
/// BX describes floppy drives only: for a hard disk it keeps the
/// caller's value.
#[test]
fn drive_parameters_set_the_documented_registers() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
    disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(32));

    rt.cpu.registers.bx.set(0xBBBB);
    int13(&mut disk_ctrl, &mut rt, 0x08EE, 0xCCCC, 0xDD00);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.word(), 0x0000, "AX");
    assert_eq!(
        rt.cpu.registers.bx.word(),
        0x0004,
        "BH = 0, BL = drive type"
    );
    assert_eq!(rt.cpu.registers.cx.word(), 0x4F12);
    assert_eq!(rt.cpu.registers.dx.word(), 0x0102);

    rt.cpu.registers.bx.set(0xBBBB);
    int13(&mut disk_ctrl, &mut rt, 0x08EE, 0xCCCC, 0xDD80);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.word(), 0x0000, "AX");
    assert_eq!(
        rt.cpu.registers.bx.word(),
        0xBBBB,
        "BX is not used for a hard disk"
    );
    assert_eq!(
        rt.cpu.registers.cx.word(),
        0x403F,
        "cylinder 64 max, 63 sectors"
    );
    assert_eq!(
        rt.cpu.registers.dx.word(),
        0x0F01,
        "head 15 max, one hard disk"
    );
}

/// The 40-track formats (160K, 180K, 320K and 360K) belong to a 360K
/// drive, not to the 1.44M drive of an unknown size.
#[test]
fn forty_track_floppies_report_a_360k_drive() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    for (size, drive_type) in [
        (163_840u64, 0x01u8),
        (184_320, 0x01),
        (327_680, 0x01),
        (368_640, 0x01),
        (1_228_800, 0x02),
        (737_280, 0x03),
        (FLOPPY_144_SIZE, 0x04),
        (2_949_120, 0x06),
    ] {
        disk_ctrl.attach(0x00, floppy(size));
        int13(&mut disk_ctrl, &mut rt, 0x0800, 0, 0x0000);
        assert_eq!(rt.cpu.registers.bx.low(), drive_type, "{}-byte image", size);
    }
}

/// AH=01h returns the status of the last operation in AH and in AL.
#[test]
fn status_of_the_last_operation_is_returned_in_ah_and_al() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();

    int13(&mut disk_ctrl, &mut rt, 0x0201, 0x0001, 0x0000); // read from the empty A:
    int13(&mut disk_ctrl, &mut rt, 0x01EE, 0, 0x0000);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.word(), 0x8080);

    int13(&mut disk_ctrl, &mut rt, 0x0000, 0, 0x0000); // a reset clears it
    int13(&mut disk_ctrl, &mut rt, 0x01EE, 0, 0x0000);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.word(), 0x0000);
}

/// Every function records its result in the BIOS data area, unknown
/// ones included: AH=01h must not find the status of an earlier
/// operation there.
#[test]
fn every_function_records_its_status() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(1));

    // An unknown function, on the hard disk and on a floppy drive.
    int13(&mut disk_ctrl, &mut rt, 0x1800, 0, 0x0080);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x01);
    assert_eq!(
        rt.cpu.memory.read_byte(0x0474),
        0x01,
        "hard disk status at 40:74"
    );
    assert_eq!(
        rt.cpu.memory.read_byte(0x0441),
        0x00,
        "diskette status at 40:41"
    );
    int13(&mut disk_ctrl, &mut rt, 0x1800, 0, 0x0000);
    assert_eq!(rt.cpu.memory.read_byte(0x0441), 0x01);

    // A call that succeeds clears it again.
    for function in [0x0Cu16, 0x15] {
        rt.cpu.memory.write_byte(0x0474, 0x04);
        int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0080);
        assert!(!carry(&rt), "AH={:02X}h", function);
        assert_eq!(
            rt.cpu.memory.read_byte(0x0474),
            0x00,
            "AH={:02X}h",
            function
        );
    }
}

/// Seek, recalibrate, reset and the drive tests have nothing to do on an
/// image, but they must not succeed for a drive that is not there.
#[test]
fn housekeeping_functions_succeed_only_for_drives_that_exist() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(1));

    for function in [0x09u16, 0x0C, 0x0D, 0x10, 0x11, 0x14] {
        int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0080);
        assert!(!carry(&rt), "AH={:02X}h on the hard disk", function);
        assert_eq!(rt.cpu.registers.ax.high(), 0x00);

        int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0081);
        assert!(
            carry(&rt),
            "AH={:02X}h on a hard disk that does not exist",
            function
        );
        assert_eq!(rt.cpu.registers.ax.high(), 0x01);

        int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0000);
        assert!(!carry(&rt), "AH={:02X}h on drive A:", function);
        int13(&mut disk_ctrl, &mut rt, function << 8, 0x0001, 0x0003);
        assert!(
            carry(&rt),
            "AH={:02X}h on a floppy drive that does not exist",
            function
        );
    }
}

/// Read long (0Ah) and write long (0Bh) transfer the ECC bytes too,
/// which an image does not have. They are invalid functions rather than
/// a success without data.
#[test]
fn read_long_and_write_long_are_not_supported() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(1));

    for function in [0x0Au16, 0x0B] {
        int13(
            &mut disk_ctrl,
            &mut rt,
            function << 8 | 0x01,
            0x0001,
            0x0080,
        );
        assert!(carry(&rt), "AH={:02X}h", function);
        assert_eq!(rt.cpu.registers.ax.high(), 0x01);
    }
}

// -----------------------------------------------------------------------
// CHS addresses outside the geometry
// -----------------------------------------------------------------------

/// CX for a CHS address: CH = cylinder bits 0-7, CL = sector plus
/// cylinder bits 8-9 in its top two bits.
fn chs_cx(cylinder: u16, sector: u8) -> u16 {
    (cylinder & 0xFF) << 8 | (cylinder >> 8 & 0x03) << 6 | (sector & 0x3F) as u16
}

/// An in-memory 1.44M floppy in which every sector holds its own number.
fn numbered_floppy() -> DiskImage {
    let mut image = floppy(FLOPPY_144_SIZE);
    let content = numbered_sectors();
    for (track, data) in content.chunks(18 * SECTOR_SIZE).enumerate() {
        image
            .write_sectors((track / 2) as u16, (track % 2) as u8, 1, 18, data)
            .unwrap();
    }
    image
}

/// A write past the last cylinder must fail on a file-backed image too.
/// If it extended the host file, the file would lose the size of a
/// floppy and be opened with a different geometry the next time.
#[test]
fn write_beyond_the_last_cylinder_fails_and_leaves_the_file_alone() {
    let scratch = ScratchFile::new("chs-write.img");
    std::fs::write(scratch.path(), numbered_sectors()).unwrap();
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, DiskImage::open(scratch.path(), false).unwrap());
    rt.cpu.registers.es.reg_mut().set(0x1000);
    rt.cpu.registers.bx.set(0);

    for (cylinder, head) in [(80u16, 0u16), (100, 0), (1023, 1)] {
        int13(
            &mut disk_ctrl,
            &mut rt,
            0x0301,
            chs_cx(cylinder, 1),
            head << 8,
        );
        assert!(carry(&rt), "write at C={} H={}", cylinder, head);
        assert_eq!(rt.cpu.registers.ax.high(), 0x04);
    }
    assert_eq!(
        std::fs::metadata(scratch.path()).unwrap().len(),
        FLOPPY_144_SIZE
    );
}

#[test]
fn format_beyond_the_last_cylinder_fails_and_leaves_the_file_alone() {
    let scratch = ScratchFile::new("chs-format.img");
    std::fs::write(scratch.path(), vec![0u8; 368_640]).unwrap();
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, DiskImage::open(scratch.path(), false).unwrap());

    int13(&mut disk_ctrl, &mut rt, 0x0509, chs_cx(39, 1), 0x0100);
    assert!(
        !carry(&rt),
        "the last track of a 360K disk can be formatted"
    );
    int13(&mut disk_ctrl, &mut rt, 0x0509, chs_cx(40, 1), 0x0000);
    assert!(carry(&rt), "cylinder 40 does not exist");
    assert_eq!(rt.cpu.registers.ax.high(), 0x04);
    int13(&mut disk_ctrl, &mut rt, 0x0509, chs_cx(0, 1), 0x0200);
    assert!(carry(&rt), "head 2 does not exist");

    assert_eq!(std::fs::metadata(scratch.path()).unwrap().len(), 368_640);
}

/// A head number beyond the last head is an error. Folded into the
/// address, head 2 of cylinder 0 would read (and write) cylinder 1.
#[test]
fn head_beyond_the_geometry_is_rejected() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, numbered_floppy());
    rt.cpu.registers.es.reg_mut().set(0x1000);
    rt.cpu.registers.bx.set(0);
    rt.cpu.memory.write_byte(0x10000, 0xEE);

    int13(&mut disk_ctrl, &mut rt, 0x0201, chs_cx(0, 1), 0x0200);
    assert!(carry(&rt), "read from head 2 of a two-sided disk");
    assert_eq!(rt.cpu.registers.ax.high(), 0x04);
    assert_eq!(
        rt.cpu.memory.read_byte(0x10000),
        0xEE,
        "nothing was transferred"
    );

    int13(&mut disk_ctrl, &mut rt, 0x0301, chs_cx(0, 1), 0x0500);
    assert!(carry(&rt), "write to head 5");
    assert_eq!(rt.cpu.registers.ax.high(), 0x04);
    let image = disk_ctrl.find_drive(0x00).unwrap();
    assert_eq!(
        image.read_sectors(2, 1, 1, 1).unwrap()[0],
        90,
        "sector 90 is untouched"
    );
}

/// DH=FFh is a head outside the geometry like any other: a transfer from
/// it that continues on the next track is rejected, and the step to the
/// next head does not overflow the 8-bit head number.
#[test]
fn head_ff_with_a_transfer_across_tracks_is_rejected() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x80, DiskImage::new_in_memory_hard_disk(32));
    rt.cpu.registers.es.reg_mut().set(0x1000);
    rt.cpu.registers.bx.set(0);

    int13(&mut disk_ctrl, &mut rt, 0x0202, chs_cx(0, 63), 0xFF80);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x04);
}

/// The image itself refuses addresses outside its geometry, whether it
/// lives in memory or in a file.
#[test]
fn images_are_bounded_by_their_geometry() {
    let scratch = ScratchFile::new("chs-bounds.img");
    std::fs::write(scratch.path(), numbered_sectors()).unwrap();
    let sector = [0x5Au8; SECTOR_SIZE];

    for mut image in [
        DiskImage::open(scratch.path(), false).unwrap(),
        numbered_floppy(),
    ] {
        assert!(image.read_sectors(79, 1, 18, 1).is_ok(), "the last sector");
        assert!(
            image.read_sectors(79, 1, 18, 2).is_err(),
            "one past the last sector"
        );
        assert!(image.read_sectors(80, 0, 1, 1).is_err(), "cylinder 80");
        assert!(image.read_sectors(0, 2, 1, 1).is_err(), "head 2");
        assert!(image.read_sectors(0, 0, 19, 1).is_err(), "sector 19");
        assert!(image.read_sectors(0, 0, 0, 1).is_err(), "sector 0");
        assert!(image.write_sectors(80, 0, 1, 1, &sector).is_err());
        assert!(image.write_sectors(0, 2, 1, 1, &sector).is_err());
        assert!(image.format_track(80, 0).is_err());
        assert!(image.format_track(0, 2).is_err());
    }
    assert_eq!(
        std::fs::metadata(scratch.path()).unwrap().len(),
        FLOPPY_144_SIZE
    );
}

/// A transfer that starts on the disk and runs off its end stops there
/// and reports how many sectors it moved.
#[test]
fn transfer_running_off_the_end_reports_the_sectors_done() {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x00, numbered_floppy());
    rt.cpu.registers.es.reg_mut().set(0x1000);
    rt.cpu.registers.bx.set(0);

    // 20 sectors from C=79 H=1 S=10: only 9 are left on the disk.
    int13(&mut disk_ctrl, &mut rt, 0x0214, chs_cx(79, 10), 0x0100);
    assert!(carry(&rt));
    assert_eq!(
        rt.cpu.registers.ax.word(),
        0x0409,
        "sector not found after 9 sectors"
    );
    assert_eq!(rt.cpu.memory.read_byte(0x10000), (2871 % 256) as u8);
    assert_eq!(
        rt.cpu.memory.read_byte(0x10000 + 8 * SECTOR_SIZE),
        (2879 % 256) as u8
    );
}

// -----------------------------------------------------------------------
// INT 13h extensions (AH=41h-48h)
// -----------------------------------------------------------------------

/// Linear address of the disk address packet and of the transfer buffer
/// (1000:0000) the tests use.
const PACKET: usize = 0x0600;
const BUFFER: usize = 0x10000;

/// Number of sectors on the disk `numbered_hard_disk` returns.
const HARD_DISK_SECTORS: u64 = 2 * 16 * 63;

/// A hard disk on which every sector starts with its own number.
fn numbered_hard_disk() -> DiskImage {
    let mut image = DiskImage::new_in_memory_hard_disk(1);
    assert_eq!(
        (image.cylinders, image.heads, image.sectors_per_track),
        (2, 16, 63)
    );
    for lba in 0..HARD_DISK_SECTORS {
        let mut sector = [0u8; SECTOR_SIZE];
        sector[..8].copy_from_slice(&lba.to_le_bytes());
        let (c, h, s) = (
            (lba / 1008) as u16,
            (lba / 63 % 16) as u8,
            (lba % 63) as u8 + 1,
        );
        image.write_sectors(c, h, s, 1, &sector).unwrap();
    }
    image
}

fn machine_with_numbered_hard_disk() -> (Runtime, DiskController) {
    let mut rt = Runtime::new_test();
    let mut disk_ctrl = DiskController::new();
    disk_ctrl.attach(0x80, numbered_hard_disk());
    // Something recognisable where the transfers go.
    for i in 0..16 * SECTOR_SIZE {
        rt.cpu.memory.write_byte(BUFFER + i, 0xEE);
    }
    (rt, disk_ctrl)
}

/// Calls an extended function with a disk address packet for `count`
/// sectors at `lba`, transfer buffer 1000:0000.
fn int13_packet(
    disk_ctrl: &mut DiskController,
    rt: &mut Runtime,
    function: u8,
    drive: u8,
    lba: u64,
    count: u16,
) {
    let mut packet = vec![0x10, 0x00];
    packet.extend_from_slice(&count.to_le_bytes());
    packet.extend_from_slice(&[0x00, 0x00, 0x00, 0x10]);
    packet.extend_from_slice(&lba.to_le_bytes());
    for (i, byte) in packet.iter().enumerate() {
        rt.cpu.memory.write_byte(PACKET + i, *byte);
    }
    rt.cpu.registers.ds.reg_mut().set(0x0000);
    rt.cpu.registers.si.set(PACKET as u16);
    int13(disk_ctrl, rt, (function as u16) << 8, 0, drive as u16);
}

/// The sector number at the start of the `index`th sector in the buffer.
fn sector_in_buffer(rt: &Runtime, index: usize) -> u64 {
    let at = BUFFER + index * SECTOR_SIZE;
    (0..8).fold(0, |number, i| {
        number | (rt.cpu.memory.read_byte(at + i) as u64) << (8 * i)
    })
}

#[test]
fn lba_transfers_reach_the_addressed_sectors() {
    let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

    int13_packet(&mut disk_ctrl, &mut rt, 0x42, 0x80, 1000, 3);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x00);
    assert_eq!(
        [
            sector_in_buffer(&rt, 0),
            sector_in_buffer(&rt, 1),
            sector_in_buffer(&rt, 2)
        ],
        [1000, 1001, 1002]
    );
    assert_eq!(rt.cpu.memory.read_byte(BUFFER + 3 * SECTOR_SIZE), 0xEE);

    // Written back 500 sectors further on.
    int13_packet(&mut disk_ctrl, &mut rt, 0x43, 0x80, 1500, 2);
    assert!(!carry(&rt));
    let image = disk_ctrl.find_drive(0x80).unwrap();
    assert_eq!(
        image.read_sectors(1, 7, 52, 1).unwrap()[..2],
        [0xE8, 0x03],
        "sector 1500"
    );
    assert_eq!(
        image.read_sectors(1, 7, 53, 1).unwrap()[..2],
        [0xE9, 0x03],
        "sector 1501"
    );
    assert_eq!(
        image.read_sectors(1, 7, 54, 1).unwrap()[..2],
        [0xDE, 0x05],
        "sector 1502"
    );

    // The last sector of the disk.
    int13_packet(
        &mut disk_ctrl,
        &mut rt,
        0x42,
        0x80,
        HARD_DISK_SECTORS - 1,
        1,
    );
    assert!(!carry(&rt));
    assert_eq!(sector_in_buffer(&rt, 0), HARD_DISK_SECTORS - 1);
}

/// The installation check answers "supported" only to a caller that
/// passes 55AAh in BX, and only for a hard disk that exists.
#[test]
fn extensions_check_requires_the_signature_in_bx() {
    let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

    rt.cpu.registers.bx.set(0x1234);
    int13(&mut disk_ctrl, &mut rt, 0x4100, 0, 0x0080);
    assert!(carry(&rt), "BX is not 55AAh");
    assert_eq!(rt.cpu.registers.ax.high(), 0x01);
    assert_eq!(rt.cpu.registers.bx.word(), 0x1234);

    rt.cpu.registers.bx.set(0x55AA);
    int13(&mut disk_ctrl, &mut rt, 0x4100, 0, 0x0080);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.bx.word(), 0xAA55);
    assert_eq!(rt.cpu.registers.ax.high(), 0x01, "version 1.x");
    assert_eq!(
        rt.cpu.registers.cx.word(),
        0x0001,
        "extended disk access functions"
    );

    for drive in [0x0081u16, 0x0000] {
        rt.cpu.registers.bx.set(0x55AA);
        int13(&mut disk_ctrl, &mut rt, 0x4100, 0, drive);
        assert!(carry(&rt), "drive {:02X}h has no extensions", drive);
        assert_eq!(rt.cpu.registers.bx.word(), 0x55AA);
    }
}

/// Block numbers have 64 bits. A block far beyond the disk is an error,
/// not the sector near the start of the disk that its number turns into
/// when it is cut to 32 bits or its cylinder to 16.
#[test]
fn lba_read_beyond_the_disk_is_rejected() {
    let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

    for lba in [
        HARD_DISK_SECTORS,
        65_536 * 1008 + 5, // cylinder 65536, which is cylinder 0 in 16 bits
        0x1_0000_0007,     // sector 7 in 32 bits
        u64::MAX,
    ] {
        int13_packet(&mut disk_ctrl, &mut rt, 0x42, 0x80, lba, 2);
        assert!(carry(&rt), "block {:#x}", lba);
        assert_eq!(rt.cpu.registers.ax.high(), 0x04, "block {:#x}", lba);
        assert_eq!(
            rt.cpu.memory.read_byte(BUFFER),
            0xEE,
            "block {:#x}: nothing is read",
            lba
        );
        assert_eq!(rt.cpu.memory.read_word(PACKET + 2), 0, "blocks transferred");
    }
    assert_eq!(
        rt.cpu.memory.read_byte(0x0474),
        0x04,
        "hard disk status at 40:74"
    );
}

/// A transfer that starts on the disk and runs off its end stops there
/// and reports in the packet how many blocks it moved.
#[test]
fn lba_read_running_off_the_end_reports_the_blocks_done() {
    let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

    int13_packet(
        &mut disk_ctrl,
        &mut rt,
        0x42,
        0x80,
        HARD_DISK_SECTORS - 6,
        10,
    );
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x04);
    assert_eq!(rt.cpu.memory.read_word(PACKET + 2), 6);
    assert_eq!(sector_in_buffer(&rt, 5), HARD_DISK_SECTORS - 1);
    assert_eq!(rt.cpu.memory.read_byte(BUFFER + 6 * SECTOR_SIZE), 0xEE);
}

/// A write to a block far beyond the disk must not land on the sector
/// its number turns into when it is cut to 32 or 16 bits: sector 0.
#[test]
fn lba_write_beyond_the_disk_is_rejected() {
    let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

    for lba in [65_536 * 1008, 0x1_0000_0000, HARD_DISK_SECTORS] {
        int13_packet(&mut disk_ctrl, &mut rt, 0x43, 0x80, lba, 1);
        assert!(carry(&rt), "block {:#x}", lba);
        assert_eq!(rt.cpu.registers.ax.high(), 0x04, "block {:#x}", lba);
    }
    let image = disk_ctrl.find_drive(0x80).unwrap();
    assert_eq!(
        image.read_sectors(0, 0, 1, 1).unwrap()[..2],
        [0x00, 0x00],
        "sector 0"
    );

    disk_ctrl.find_drive(0x80).unwrap().readonly = true;
    int13_packet(&mut disk_ctrl, &mut rt, 0x43, 0x80, 0, 1);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x03, "write-protected");
}

/// Extended verify has no data to check on an image, but it answers for
/// the drive and for the range of blocks: a drive that does not exist is
/// not ready, and a range that leaves the disk is "sector not found".
#[test]
fn extended_verify_checks_the_drive_and_the_range() {
    let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

    int13_packet(&mut disk_ctrl, &mut rt, 0x44, 0x83, 0, 1);
    assert!(carry(&rt), "drive 83h does not exist");
    assert_eq!(rt.cpu.registers.ax.high(), 0x80);

    int13_packet(
        &mut disk_ctrl,
        &mut rt,
        0x44,
        0x80,
        0,
        HARD_DISK_SECTORS as u16,
    );
    assert!(!carry(&rt), "the whole disk");
    assert_eq!(rt.cpu.registers.ax.high(), 0x00);

    int13_packet(
        &mut disk_ctrl,
        &mut rt,
        0x44,
        0x80,
        1,
        HARD_DISK_SECTORS as u16,
    );
    assert!(carry(&rt), "one block past the end");
    assert_eq!(rt.cpu.registers.ax.high(), 0x04);
    assert_eq!(
        rt.cpu.memory.read_word(PACKET + 2),
        HARD_DISK_SECTORS as u16 - 1
    );

    int13_packet(&mut disk_ctrl, &mut rt, 0x44, 0x80, 0x7FFF_FFFF, 1);
    assert!(carry(&rt));

    // A packet that is too short to hold a block number.
    rt.cpu.memory.write_byte(PACKET, 0x0C);
    int13(&mut disk_ctrl, &mut rt, 0x4400, 0, 0x0080);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x01);
}

/// The installation check promises the extended seek and the extended
/// drive parameters along with the transfers.
#[test]
fn extended_seek_checks_the_block_number() {
    let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();

    int13_packet(
        &mut disk_ctrl,
        &mut rt,
        0x47,
        0x80,
        HARD_DISK_SECTORS - 1,
        0,
    );
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x00);

    int13_packet(&mut disk_ctrl, &mut rt, 0x47, 0x80, HARD_DISK_SECTORS, 0);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x04);

    int13_packet(&mut disk_ctrl, &mut rt, 0x47, 0x81, 0, 0);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x80);
}

#[test]
fn extended_drive_parameters_describe_the_disk() {
    let (mut rt, mut disk_ctrl) = machine_with_numbered_hard_disk();
    let word = |rt: &Runtime, offset: usize| rt.cpu.memory.read_word(PACKET + offset);
    rt.cpu.registers.ds.reg_mut().set(0x0000);
    rt.cpu.registers.si.set(PACKET as u16);

    // The caller says how large its buffer is; 30 bytes is the size
    // version 2 of the interface uses.
    for i in 0..0x20 {
        rt.cpu.memory.write_byte(PACKET + i, 0xEE);
    }
    rt.cpu.memory.write_word(PACKET, 0x001E);
    int13(&mut disk_ctrl, &mut rt, 0x4800, 0, 0x0080);
    assert!(!carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x00);
    assert_eq!(word(&rt, 0x00), 0x001A, "size of the data returned");
    assert_eq!(word(&rt, 0x02), 0x0002, "flags: the CHS values are valid");
    assert_eq!([word(&rt, 0x04), word(&rt, 0x06)], [2, 0], "cylinders");
    assert_eq!([word(&rt, 0x08), word(&rt, 0x0A)], [16, 0], "heads");
    assert_eq!(
        [word(&rt, 0x0C), word(&rt, 0x0E)],
        [63, 0],
        "sectors per track"
    );
    assert_eq!(
        [
            word(&rt, 0x10),
            word(&rt, 0x12),
            word(&rt, 0x14),
            word(&rt, 0x16)
        ],
        [HARD_DISK_SECTORS as u16, 0, 0, 0],
        "total sectors"
    );
    assert_eq!(word(&rt, 0x18), 512, "bytes per sector");
    assert_eq!(
        rt.cpu.memory.read_byte(PACKET + 0x1A),
        0xEE,
        "nothing beyond 26 bytes"
    );

    // A buffer that cannot hold the 26 bytes.
    rt.cpu.memory.write_word(PACKET, 0x0010);
    int13(&mut disk_ctrl, &mut rt, 0x4800, 0, 0x0080);
    assert!(carry(&rt));
    assert_eq!(rt.cpu.registers.ax.high(), 0x01);

    rt.cpu.memory.write_word(PACKET, 0x001A);
    int13(&mut disk_ctrl, &mut rt, 0x4800, 0, 0x0081);
    assert!(carry(&rt), "drive 81h does not exist");
    assert_eq!(rt.cpu.registers.ax.high(), 0x80);

    // A floppy disk is a removable medium in a drive with a change line.
    disk_ctrl.attach(0x00, floppy(FLOPPY_144_SIZE));
    rt.cpu.memory.write_word(PACKET, 0x001A);
    int13(&mut disk_ctrl, &mut rt, 0x4800, 0, 0x0000);
    assert!(!carry(&rt));
    assert_eq!(word(&rt, 0x02), 0x0016, "flags");
    assert_eq!(
        [word(&rt, 0x04), word(&rt, 0x08), word(&rt, 0x0C)],
        [80, 2, 18]
    );
    assert_eq!(word(&rt, 0x10), 2880, "total sectors");
}

// -----------------------------------------------------------------------
// Copying host files into the FAT volume (`put_file`, `put_dir`)
// -----------------------------------------------------------------------

fn formatted_floppy() -> DiskImage {
    let mut image = floppy(FLOPPY_144_SIZE);
    image.format_fat(&FixedClock).unwrap();
    image
}

/// Reads a file back from the FAT volume of an unpartitioned in-memory
/// image; `path` uses '/' between directories.
fn fat_file(image: &mut DiskImage, path: &str) -> Option<Vec<u8>> {
    let DiskStorage::InMemory(data) = &mut image.storage else {
        panic!("in-memory image expected");
    };
    let volume = std::io::Cursor::new(data.as_mut_slice());
    let fs = fatfs::FileSystem::new(volume, FixedClock.fat_options()).unwrap();
    let mut file = fs.root_dir().open_file(path).ok()?;
    let mut content = Vec::new();
    file.read_to_end(&mut content).unwrap();
    Some(content)
}

/// The root directory of an unpartitioned FAT12 floppy as it is on the
/// disk: the number of long-name entries and the 8.3 names.
fn root_directory(image: &mut DiskImage) -> (usize, Vec<String>) {
    let boot = image.read_sectors(0, 0, 1, 1).unwrap();
    let reserved = u16::from_le_bytes([boot[14], boot[15]]) as usize;
    let fats = boot[16] as usize;
    let root_entries = u16::from_le_bytes([boot[17], boot[18]]) as usize;
    let sectors_per_fat = u16::from_le_bytes([boot[22], boot[23]]) as usize;
    let first = reserved + fats * sectors_per_fat;

    let mut long_names = 0;
    let mut names = Vec::new();
    for sector in first..first + root_entries * 32 / SECTOR_SIZE {
        let (track, index) = (sector / 18, sector % 18);
        let data = image
            .read_sectors((track / 2) as u16, (track % 2) as u8, index as u8 + 1, 1)
            .unwrap();
        for entry in data.chunks(32) {
            match (entry[0], entry[11]) {
                (0x00, _) => return (long_names, names),
                (0xE5, _) => {}
                (_, 0x0F) => long_names += 1,
                _ => {
                    let base = String::from_utf8_lossy(&entry[..8]).trim_end().to_string();
                    let ext = String::from_utf8_lossy(&entry[8..11])
                        .trim_end()
                        .to_string();
                    names.push(if ext.is_empty() {
                        base
                    } else {
                        format!("{}.{}", base, ext)
                    });
                }
            }
        }
    }
    (long_names, names)
}

/// When the root directory entry `name` of an unpartitioned in-memory
/// image was created and last modified.
fn fat_stamps(image: &mut DiskImage, name: &str) -> (fatfs::DateTime, fatfs::DateTime) {
    let DiskStorage::InMemory(data) = &mut image.storage else {
        panic!("in-memory image expected");
    };
    let volume = std::io::Cursor::new(data.as_mut_slice());
    let fs = fatfs::FileSystem::new(volume, FixedClock.fat_options()).unwrap();
    let entry = fs
        .root_dir()
        .iter()
        .map(|entry| entry.unwrap())
        .find(|entry| entry.short_file_name_as_bytes() == name.as_bytes())
        .expect("the file is in the root directory");
    (entry.created(), entry.modified())
}

/// A clock that shows another time than `FixedClock`: the first second of
/// the year 2000.
#[derive(Debug)]
struct ClockAt2000;

impl ClockAt2000 {
    const NOW: fatfs::DateTime = fatfs::DateTime {
        date: fatfs::Date {
            year: 2000,
            month: 1,
            day: 1,
        },
        time: fatfs::Time {
            hour: 0,
            min: 0,
            sec: 0,
            millis: 0,
        },
    };
}

impl fatfs::TimeProvider for ClockAt2000 {
    fn get_current_date(&self) -> fatfs::Date {
        Self::NOW.date
    }

    fn get_current_date_time(&self) -> fatfs::DateTime {
        Self::NOW
    }
}

impl WallClock for ClockAt2000 {
    fn fat_options(&self) -> fatfs::FsOptions {
        fatfs::FsOptions::new().time_provider(&ClockAt2000)
    }

    fn unix_nanos(&self) -> u64 {
        946_684_800_000_000_000
    }
}

/// A file is stamped with the time on the clock its copy was given, and
/// with no other: the same disk takes the time of whichever clock comes
/// with each file.
#[test]
fn put_stamps_a_file_with_the_time_of_the_clock_passed_in() {
    let mut image = formatted_floppy();
    image
        .copy_file_to_fat(b"old", "OLD.TXT", &FixedClock)
        .unwrap();
    image
        .copy_file_to_fat(b"new", "NEW.TXT", &ClockAt2000)
        .unwrap();

    assert_eq!(
        fat_stamps(&mut image, "OLD.TXT"),
        (FixedClock::NOW, FixedClock::NOW)
    );
    assert_eq!(
        fat_stamps(&mut image, "NEW.TXT"),
        (ClockAt2000::NOW, ClockAt2000::NOW)
    );

    // Writing a file again leaves its creation time and moves the other.
    image
        .copy_file_to_fat(b"newer", "OLD.TXT", &ClockAt2000)
        .unwrap();
    assert_eq!(
        fat_stamps(&mut image, "OLD.TXT"),
        (FixedClock::NOW, ClockAt2000::NOW)
    );
}

/// Putting a file over an existing longer one replaces it as a whole:
/// nothing of the old tail stays, and the file has the new length.
#[test]
fn put_over_an_existing_file_replaces_it() {
    let mut image = formatted_floppy();
    let old: Vec<u8> = b"ECHO OLD LINE\r\n"
        .iter()
        .cycle()
        .take(600)
        .copied()
        .collect();
    let new = b"@ECHO NEW\r\n".to_vec();

    image
        .copy_file_to_fat(&old, "AUTOEXEC.BAT", &FixedClock)
        .unwrap();
    image
        .copy_file_to_fat(&new, "AUTOEXEC.BAT", &FixedClock)
        .unwrap();
    assert_eq!(fat_file(&mut image, "AUTOEXEC.BAT"), Some(new));

    // Growing again works too, across several clusters.
    let big: Vec<u8> = (0..5000u32).map(|i| i as u8).collect();
    image
        .copy_file_to_fat(&big, "autoexec.bat", &FixedClock)
        .unwrap();
    assert_eq!(fat_file(&mut image, "AUTOEXEC.BAT"), Some(big));
    assert_eq!(root_directory(&mut image).1, ["AUTOEXEC.BAT"]);
}

/// MS-DOS 6 shows a VFAT long-name entry as the volume label when the
/// disk has none, and every file costs a second directory slot.
#[test]
fn put_writes_no_long_name_entries() {
    let mut image = formatted_floppy();
    image
        .copy_file_to_fat(b"hello", "GAME.EXE", &FixedClock)
        .unwrap();
    image
        .copy_file_to_fat(b"hello", "readme.txt", &FixedClock)
        .unwrap();
    image
        .copy_file_to_fat(b"hello", "My Document.final.txt", &FixedClock)
        .unwrap();

    let (long_names, names) = root_directory(&mut image);
    assert_eq!(long_names, 0);
    assert_eq!(names, ["GAME.EXE", "README.TXT", "MYDOCU~1.TXT"]);
}

#[test]
fn put_respects_the_write_protection() {
    let mut image = DiskImage::new_in_memory_sized(FLOPPY_144_SIZE, false);
    image.format_fat(&FixedClock).unwrap();
    image.readonly = true;

    let error = image
        .copy_file_to_fat(b"hello", "HELLO.TXT", &FixedClock)
        .unwrap_err();
    assert_eq!(error.kind(), std::io::ErrorKind::PermissionDenied);
    assert_eq!(root_directory(&mut image).1, Vec::<String>::new());
}

/// DOS resolves these names to devices in every directory, so a file
/// called CON or NUL.TXT could never be opened or deleted.
#[test]
fn put_refuses_dos_device_names() {
    let mut image = formatted_floppy();
    for name in [
        "CON", "nul", "NUL.TXT", "prn.log", "AUX", "COM1", "lpt3.dat", "CLOCK$",
    ] {
        assert!(
            image.copy_file_to_fat(b"x", name, &FixedClock).is_err(),
            "{} is a device",
            name
        );
    }
    for name in ["CONSOLE.TXT", "COM5", "NULL", "LPT.TXT"] {
        assert!(
            image.copy_file_to_fat(b"x", name, &FixedClock).is_ok(),
            "{} is a plain file name",
            name
        );
    }
    assert_eq!(
        root_directory(&mut image).1,
        ["CONSOLE.TXT", "COM5", "NULL", "LPT.TXT"]
    );
}

#[test]
fn put_reports_the_name_the_file_has_in_dos() {
    let mut image = formatted_floppy();

    assert_eq!(
        image
            .copy_file_to_fat(b"1", "game.exe", &FixedClock)
            .unwrap(),
        "GAME.EXE"
    );
    assert_eq!(
        image
            .copy_file_to_fat(b"2", "\\GAME.EXE", &FixedClock)
            .unwrap(),
        "GAME.EXE"
    );
    assert_eq!(
        image
            .copy_file_to_fat(b"3", "My Document.final.txt", &FixedClock)
            .unwrap(),
        "MYDOCU~1.TXT"
    );
    // A long name cannot be matched against the entry it was stored
    // under, so putting it again makes a second file rather than
    // overwriting one that merely shares the generated name.
    assert_eq!(
        image
            .copy_file_to_fat(b"4", "My Document.final.txt", &FixedClock)
            .unwrap(),
        "MYDOCU~2.TXT"
    );
    assert_eq!(fat_file(&mut image, "GAME.EXE"), Some(b"2".to_vec()));
    assert_eq!(fat_file(&mut image, "MYDOCU~1.TXT"), Some(b"3".to_vec()));

    let error = image
        .copy_file_to_fat(b"5", "NODIR\\FILE.TXT", &FixedClock)
        .unwrap_err();
    assert_eq!(error.kind(), std::io::ErrorKind::NotFound);
    assert!(
        image
            .copy_file_to_fat(b"6", "what?.txt", &FixedClock)
            .is_err()
    );
    assert!(image.copy_file_to_fat(b"7", "\\", &FixedClock).is_err());
}

/// A directory tree on the host, removed again on drop.
struct ScratchDir(PathBuf);

impl ScratchDir {
    fn new(name: &str) -> Self {
        let path =
            std::env::temp_dir().join(format!("emu8086-test-{}-{}", std::process::id(), name));
        let _ = std::fs::remove_dir_all(&path);
        std::fs::create_dir_all(&path).unwrap();
        Self(path)
    }

    fn write(&self, relative: &str, content: &[u8]) {
        let path = self.0.join(relative);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, content).unwrap();
    }
}

impl Drop for ScratchDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[test]
fn put_of_a_directory_copies_the_tree_and_replaces_existing_files() {
    let host = ScratchDir::new("put-dir");
    host.write("readme.txt", &[b'r'; 3000]);
    host.write("data/level1.dat", b"level one");
    host.write("data/deep/level2.dat", b"level two");

    let mut image = formatted_floppy();
    assert_eq!(image.copy_dir_to_fat(&host.0, &FixedClock).unwrap(), 3);
    assert_eq!(fat_file(&mut image, "README.TXT"), Some(vec![b'r'; 3000]));
    assert_eq!(
        fat_file(&mut image, "DATA/LEVEL1.DAT"),
        Some(b"level one".to_vec())
    );
    assert_eq!(
        fat_file(&mut image, "DATA/DEEP/LEVEL2.DAT"),
        Some(b"level two".to_vec())
    );

    // A second copy goes into the same directories and files.
    host.write("readme.txt", b"short");
    assert_eq!(image.copy_dir_to_fat(&host.0, &FixedClock).unwrap(), 3);
    assert_eq!(fat_file(&mut image, "README.TXT"), Some(b"short".to_vec()));
    let (long_names, mut names) = root_directory(&mut image);
    names.sort();
    assert_eq!(
        (long_names, names),
        (0, vec!["DATA".to_string(), "README.TXT".to_string()])
    );

    // A file can also be put into one of the copied directories.
    assert_eq!(
        image
            .copy_file_to_fat(b"new", "data\\deep\\level3.dat", &FixedClock)
            .unwrap(),
        "DATA\\DEEP\\LEVEL3.DAT"
    );
    assert_eq!(
        fat_file(&mut image, "DATA/DEEP/LEVEL3.DAT"),
        Some(b"new".to_vec())
    );
}

// -----------------------------------------------------------------------
// The FAT volume of a partitioned disk
// -----------------------------------------------------------------------

/// The bytes 0, 1, ..., 99 with a partition of 30 bytes that starts at
/// byte 10.
fn partition_slice() -> PartitionSlice<std::io::Cursor<Vec<u8>>> {
    PartitionSlice::new(std::io::Cursor::new((0..100).collect()), 10, 30).unwrap()
}

#[test]
fn partition_slice_positions_are_relative_to_the_partition() {
    let mut slice = partition_slice();
    let mut two = [0u8; 2];

    assert_eq!(slice.stream_position().unwrap(), 0);
    assert_eq!(slice.seek(SeekFrom::Start(5)).unwrap(), 5);
    slice.read_exact(&mut two).unwrap();
    assert_eq!(two, [15, 16]);
    assert_eq!(slice.stream_position().unwrap(), 7);
    assert_eq!(slice.seek(SeekFrom::Current(-4)).unwrap(), 3);
    assert_eq!(
        slice.seek(SeekFrom::End(0)).unwrap(),
        30,
        "the end of the partition"
    );
    assert_eq!(slice.seek(SeekFrom::End(-2)).unwrap(), 28);
    slice.read_exact(&mut two).unwrap();
    assert_eq!(two, [38, 39]);
}

/// A seek cannot end up in front of the partition or behind it, and
/// reads and writes stop at its end.
#[test]
fn partition_slice_keeps_inside_the_partition() {
    let mut slice = partition_slice();
    let mut buffer = [0u8; 8];

    assert!(
        slice.seek(SeekFrom::Current(-4)).is_err(),
        "in front of the partition"
    );
    assert!(
        slice.seek(SeekFrom::Start(31)).is_err(),
        "behind the partition"
    );
    assert!(slice.seek(SeekFrom::End(1)).is_err());
    assert_eq!(
        slice.stream_position().unwrap(),
        0,
        "a failed seek does not move"
    );
    slice.read_exact(&mut buffer[..1]).unwrap();
    assert_eq!(buffer[0], 10);

    slice.seek(SeekFrom::Start(28)).unwrap();
    assert_eq!(
        slice.read(&mut buffer).unwrap(),
        2,
        "a read stops at the end"
    );
    assert_eq!(buffer[..2], [38, 39]);
    assert_eq!(slice.read(&mut buffer).unwrap(), 0);

    slice.seek(SeekFrom::Start(28)).unwrap();
    assert_eq!(slice.write(&[0xFF; 8]).unwrap(), 2, "and so does a write");
    assert!(slice.write_all(&[0xFF; 8]).is_err());
    assert_eq!(slice.inner.get_ref()[36..42], [36, 37, 0xFF, 0xFF, 40, 41]);
}

/// A master boot record with the given partitions: type, first sector
/// and number of sectors of each.
fn master_boot_record(partitions: &[(u8, u32, u32)]) -> [u8; SECTOR_SIZE] {
    let mut sector = [0u8; SECTOR_SIZE];
    sector[0] = 0xFA; // boot code, not the jump of a FAT boot sector
    for (index, (kind, first, count)) in partitions.iter().enumerate() {
        let entry = 0x1BE + 16 * index;
        sector[entry + 4] = *kind;
        sector[entry + 8..entry + 12].copy_from_slice(&first.to_le_bytes());
        sector[entry + 12..entry + 16].copy_from_slice(&count.to_le_bytes());
    }
    sector[510] = 0x55;
    sector[511] = 0xAA;
    sector
}

/// First sector of the FAT volume on the disks below.
const VOLUME_START: u64 = 63;

/// A hard disk of 8064 sectors with `mbr` in its first sector and an
/// empty FAT volume of 2880 sectors from sector 63 on.
fn hard_disk_with_a_fat_volume(mbr: &[u8; SECTOR_SIZE]) -> DiskImage {
    let mut image = DiskImage::new_in_memory_hard_disk(4);
    for (index, sector) in formatted_floppy_bytes().chunks(SECTOR_SIZE).enumerate() {
        write_block(&mut image, VOLUME_START + index as u64, sector);
    }
    image.write_sectors(0, 0, 1, 1, mbr).unwrap();
    image
}

fn write_block(image: &mut DiskImage, lba: u64, data: &[u8]) {
    let (c, h, s) = (
        (lba / 1008) as u16,
        (lba / 63 % 16) as u8,
        (lba % 63) as u8 + 1,
    );
    image.write_sectors(c, h, s, 1, data).unwrap();
}

fn read_block(image: &mut DiskImage, lba: u64) -> Vec<u8> {
    let (c, h, s) = (
        (lba / 1008) as u16,
        (lba / 63 % 16) as u8,
        (lba % 63) as u8 + 1,
    );
    image.read_sectors(c, h, s, 1).unwrap()
}

/// The first sector of the root directory of the volume at sector 63.
fn root_directory_of_the_volume(image: &mut DiskImage) -> Vec<u8> {
    let boot = read_block(image, VOLUME_START);
    let reserved = u16::from_le_bytes([boot[14], boot[15]]) as u64;
    let sectors_per_fat = u16::from_le_bytes([boot[22], boot[23]]) as u64;
    read_block(
        image,
        VOLUME_START + reserved + boot[16] as u64 * sectors_per_fat,
    )
}

/// The FAT partition may be described by any of the four entries of
/// the partition table, behind empty entries and partitions of other
/// types.
#[test]
fn put_finds_the_fat_partition_in_any_entry() {
    for partitions in [
        vec![(0x06u8, 63u32, 2880u32)],
        vec![(0x00, 0, 0), (0x06, 63, 2880)],
        vec![(0x05, 4000, 2000), (0x01, 63, 2880)],
        vec![
            (0x83, 4000, 2000),
            (0x00, 0, 0),
            (0x00, 0, 0),
            (0x04, 63, 2880),
        ],
    ] {
        let mut image = hard_disk_with_a_fat_volume(&master_boot_record(&partitions));
        let stored = image.copy_file_to_fat(b"hello", "HELLO.TXT", &FixedClock);
        assert_eq!(
            stored.ok().as_deref(),
            Some("HELLO.TXT"),
            "{:?}",
            partitions
        );
        let root = root_directory_of_the_volume(&mut image);
        assert_eq!(&root[..11], b"HELLO   TXT", "{:?}", partitions);
    }
}

#[test]
fn put_reports_a_disk_without_a_fat_partition() {
    for partitions in [
        vec![],
        vec![(0x83u8, 63u32, 2880u32)],
        vec![(0x06, 63, 0)],
        vec![(0x06, 9000, 100)], // behind the end of the disk
    ] {
        let mut image = hard_disk_with_a_fat_volume(&master_boot_record(&partitions));
        let error = image
            .copy_file_to_fat(b"hello", "HELLO.TXT", &FixedClock)
            .unwrap_err();
        assert!(
            error.to_string().contains("no FAT partition"),
            "{:?}: {}",
            partitions,
            error
        );
    }
}
