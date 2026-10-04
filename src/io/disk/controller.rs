//! The disk controller: which image is in which drive, which floppy drives
//! exist, the change line of each, and the order in which the BIOS bootstrap
//! tries the drives. It answers on two I/O ports: a write to
//! `DISK_TRAP_PORT` is an INT 13h call (served in `int13`), and
//! `BOOT_ORDER_PORT` hands the boot order to INT 19h.
//!
//! The invariant: the guest is told whenever a floppy changes behind its
//! back. DOS caches what it has read of a disk and relies on the change line
//! (AH=16h) to learn that the disk is another one, so every way in from the
//! host (`attach`, `detach`, `put_file`, `put_dir`) raises the change line
//! of that drive, and the guest is told exactly once. A hard disk has no
//! change line: what the host changes on one takes a reboot of the guest.

use super::image::DiskImage;
use crate::io::bus::IoDevice;
use crate::vm::cpu::Cpu;
use std::path::Path;

/// Port of the BIOS disk service trap: the INT 13h handler does
/// `OUT 0xB0, AL`, which makes the controller read the CPU registers, perform
/// the disk I/O, and set the return values and flags.
pub const DISK_TRAP_PORT: u16 = 0xB0;

/// Port through which the BIOS bootstrap (INT 19h) learns the boot order:
/// any write rewinds the list, each read returns the next BIOS drive number,
/// and `BOOT_ORDER_END` is returned once the list is exhausted.
pub const BOOT_ORDER_PORT: u16 = 0xB1;
pub const BOOT_ORDER_END: u8 = 0xFF;

/// BIOS drive numbers 0..FLOPPY_UNITS are floppy drives.
const FLOPPY_UNITS: usize = 4;

pub struct DiskController {
    pub drives: Vec<(u8, DiskImage)>, // (drive number, image)
    boot_order: Vec<u8>,
    boot_cursor: usize,
    /// Floppy units that exist as drives, with or without a disk in them.
    /// A: and B: always do; further units once an image was attached to them.
    floppy_installed: [bool; FLOPPY_UNITS],
    /// Change line of each floppy drive: the disk was inserted, removed or
    /// rewritten from the host since the guest last asked (AH=16h).
    pub(super) floppy_changed: [bool; FLOPPY_UNITS],
}

impl DiskController {
    pub fn new() -> Self {
        Self {
            drives: Vec::new(),
            boot_order: vec![0x00, 0x80], // floppy A:, then hard disk 0
            boot_cursor: 0,
            floppy_installed: [true, true, false, false],
            floppy_changed: [false; FLOPPY_UNITS],
        }
    }

    /// Sets the BIOS drive numbers the bootstrap tries, in order.
    pub fn set_boot_order(&mut self, order: Vec<u8>) {
        self.boot_order = order;
        self.boot_cursor = 0;
    }

    pub fn boot_order(&self) -> &[u8] {
        &self.boot_order
    }

    /// Puts `image` into `drive`, in place of whatever was there.
    pub fn attach(&mut self, drive: u8, image: DiskImage) {
        if let Some(unit) = Self::floppy_unit(drive) {
            self.floppy_installed[unit] = true;
            self.floppy_changed[unit] = true;
        }
        self.drives.retain(|(attached, _)| *attached != drive);
        self.drives.push((drive, image));
    }

    /// Raises the change line of a floppy drive after its disk was modified
    /// from the host, so the guest drops what it has cached of that disk.
    pub(super) fn mark_changed(&mut self, drive: u8) {
        if let Some(unit) = Self::floppy_unit(drive) {
            self.floppy_changed[unit] = true;
        }
    }

    /// Copies a host file into the FAT volume on the disk in `drive` and
    /// returns the path under which DOS sees it.
    pub fn put_file(&mut self, drive: u8, data: &[u8], dos_path: &str) -> std::io::Result<String> {
        let result = self.disk_in(drive)?.copy_file_to_fat(data, dos_path);
        self.mark_changed(drive);
        result
    }

    /// Copies a host directory into the FAT volume on the disk in `drive`
    /// and returns the number of files copied.
    pub fn put_dir(&mut self, drive: u8, host_dir: &Path) -> std::io::Result<usize> {
        let result = self.disk_in(drive)?.copy_dir_to_fat(host_dir);
        self.mark_changed(drive);
        result
    }

    fn disk_in(&mut self, drive: u8) -> std::io::Result<&mut DiskImage> {
        self.find_drive(drive).ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::NotFound, "no disk in the drive")
        })
    }

    pub(super) fn floppy_unit(drive: u8) -> Option<usize> {
        ((drive as usize) < FLOPPY_UNITS).then_some(drive as usize)
    }

    /// Whether `drive` is a floppy drive that exists, with or without a disk.
    pub(super) fn is_installed_floppy(&self, drive: u8) -> bool {
        Self::floppy_unit(drive).is_some_and(|unit| self.floppy_installed[unit])
    }

    pub(super) fn floppy_drive_count(&self) -> u8 {
        self.floppy_installed
            .iter()
            .filter(|installed| **installed)
            .count() as u8
    }

    pub(crate) fn find_drive(&mut self, drive: u8) -> Option<&mut DiskImage> {
        self.drives
            .iter_mut()
            .find(|(d, _)| *d == drive)
            .map(|(_, img)| img)
    }

    /// The drive whose image lives in the host file `path`, if any.
    pub fn drive_backed_by(&self, path: &Path) -> Option<u8> {
        self.drives
            .iter()
            .find(|(_, img)| img.is_backed_by(path))
            .map(|(drive, _)| *drive)
    }

    pub(super) fn hard_disk_count(&self) -> u8 {
        self.drives
            .iter()
            .filter(|(drive, _)| *drive >= 0x80)
            .count() as u8
    }

    /// Whether `drive` is a floppy drive that exists or a hard disk with an
    /// image attached.
    pub(super) fn drive_exists(&self, drive: u8) -> bool {
        self.is_installed_floppy(drive)
            || (drive >= 0x80 && self.drives.iter().any(|(attached, _)| *attached == drive))
    }

    /// Remove a drive and return its image.
    pub fn detach(&mut self, drive: u8) -> Option<DiskImage> {
        if let Some(pos) = self.drives.iter().position(|(d, _)| *d == drive) {
            self.mark_changed(drive);
            Some(self.drives.remove(pos).1)
        } else {
            None
        }
    }
}

impl IoDevice for DiskController {
    fn port_in_byte(&mut self, port: u16, _cpu: &mut Cpu) -> u8 {
        if port != BOOT_ORDER_PORT {
            return 0xFF;
        }
        match self.boot_order.get(self.boot_cursor) {
            Some(&drive) => {
                self.boot_cursor += 1;
                drive
            }
            None => BOOT_ORDER_END,
        }
    }

    fn port_out_byte(&mut self, port: u16, _value: u8, cpu: &mut Cpu) {
        match port {
            DISK_TRAP_PORT => self.handle_trap(cpu),
            BOOT_ORDER_PORT => self.boot_cursor = 0,
            _ => {}
        }
    }
}
