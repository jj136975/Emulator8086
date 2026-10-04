//! The INT 13h services behind the disk trap. The BIOS handler is one
//! `OUT 0xB0, AL`: the controller takes the function from AH and the drive
//! from DL, serves the call on the image in that drive and returns as a BIOS
//! does: the status in AH, CF set unless it is 0, and the status recorded in
//! the BIOS data area for AH=01h. This file holds the functions that address
//! by CHS; the extended ones are in `int13_ext`.
//!
//! The invariant: a CHS address from the guest is checked against the
//! geometry before a sector moves. Sector 0 and a sector beyond the track
//! are refused at once, a transfer is cut into runs on one track each
//! (`track_runs`), and the image checks every run. An address outside the
//! disk is "sector not found", with the sectors done so far in AL, never a
//! transfer from the sector that address would fold into.

use super::SECTOR_SIZE;
use super::controller::DiskController;
use crate::vm::cpu::Cpu;
use crate::vm::runtime::CpuFlag::Carry;
use log::debug;

/// Parameters reported for a floppy drive that has no disk in it: those of
/// a 3.5" 1.44M drive.
const EMPTY_FLOPPY_DRIVE_TYPE: u8 = 0x04;
const EMPTY_FLOPPY_MAX_CYLINDER: u16 = 79;
const EMPTY_FLOPPY_SECTORS: u8 = 18;
const EMPTY_FLOPPY_MAX_HEAD: u8 = 1;

/// INT 13h status codes: returned in AH and kept in the BIOS data area.
pub(super) const STATUS_OK: u8 = 0x00;
pub(super) const STATUS_BAD_COMMAND: u8 = 0x01;
const STATUS_WRITE_PROTECTED: u8 = 0x03;
pub(super) const STATUS_SECTOR_NOT_FOUND: u8 = 0x04;
const STATUS_DISK_CHANGED: u8 = 0x06;
const STATUS_NO_PARAMETERS: u8 = 0x07;
pub(super) const STATUS_NOT_READY: u8 = 0x80;

/// BIOS data area: the status of the last diskette operation, of the last
/// hard disk operation, and the number of hard disks.
const BDA_DISKETTE_STATUS: usize = 0x0441;
const BDA_HARD_DISK_STATUS: usize = 0x0474;
const BDA_HARD_DISK_COUNT: usize = 0x0475;

/// The drive type AH=08h reports for a floppy disk of `size` bytes: the
/// drive that disk is made for.
fn floppy_drive_type(size: u64) -> u8 {
    match size {
        163_840 | 184_320 | 327_680 | 368_640 => 0x01, // 5.25" 360K
        1_228_800 => 0x02,                             // 5.25" 1.2M
        737_280 => 0x03,                               // 3.5" 720K
        2_949_120 => 0x06,                             // 3.5" 2.88M
        _ => 0x04,                                     // 3.5" 1.44M
    }
}

/// A run of sectors that lie on one track.
struct TrackRun {
    cylinder: u16,
    head: u8,
    sector: u8,
    count: u8,
}

/// Splits a transfer of `count` sectors that starts at a CHS address into
/// runs of sectors on one track each: a transfer that reaches the end of a
/// track continues on the next head and, after the last head, on the next
/// cylinder.
fn track_runs(
    heads: u8,
    sectors_per_track: u8,
    start: (u16, u8, u8),
    count: u16,
) -> impl Iterator<Item = TrackRun> {
    let (mut cylinder, mut head, mut sector) = start;
    let mut remaining = count;
    std::iter::from_fn(move || {
        if remaining == 0 {
            return None;
        }
        let left_on_track = sectors_per_track.saturating_sub(sector) as u16 + 1;
        let run = TrackRun {
            cylinder,
            head,
            sector,
            count: remaining.min(left_on_track).min(255) as u8,
        };
        remaining -= run.count as u16;
        sector = 1;
        if head as u16 + 1 < heads as u16 {
            head += 1;
        } else {
            head = 0;
            cylinder = cylinder.saturating_add(1);
        }
        Some(run)
    })
}

impl DiskController {
    /// Points ES:DI at the diskette parameter table (the INT 1Eh vector).
    fn point_to_diskette_parameters(cpu: &mut Cpu) {
        let offset = cpu.memory.read_word(0x0078);
        let segment = cpu.memory.read_word(0x007A);
        cpu.registers.es.reg_mut().set(segment);
        cpu.registers.di.set(offset);
    }

    /// Where the BIOS data area keeps the status of the last operation on
    /// `drive`: the diskettes and the hard disks have a byte each.
    fn status_address(drive: u8) -> usize {
        if drive < 0x80 {
            BDA_DISKETTE_STATUS
        } else {
            BDA_HARD_DISK_STATUS
        }
    }

    /// Ends an INT 13h call: AH holds the status, CF is set unless the
    /// status is 0, and the status is recorded for AH=01h.
    fn finish(cpu: &mut Cpu, drive: u8, status: u8) {
        cpu.registers.ax.set_high(status);
        if status == STATUS_OK {
            cpu.unset_flag(Carry);
        } else {
            cpu.set_flag(Carry);
        }
        cpu.memory.write_byte(Self::status_address(drive), status);
    }

    /// The CHS address in CX and DH: cylinder (10 bits), head and sector
    /// (1-based).
    fn decode_chs(cpu: &Cpu) -> (u16, u8, u8) {
        let cl = cpu.registers.cx.low();
        let cylinder = cpu.registers.cx.high() as u16 | ((cl as u16 & 0xC0) << 2);
        (cylinder, cpu.registers.dx.high(), cl & 0x3F)
    }

    /// The sector count in AL. AL=0 is the IBM PC BIOS convention for
    /// "transfer 256 sectors".
    fn sector_count(cpu: &Cpu) -> u16 {
        match cpu.registers.ax.low() {
            0 => 256,
            count => count as u16,
        }
    }

    /// Linear address of the transfer buffer at ES:BX.
    fn buffer_address(cpu: &Cpu) -> usize {
        let es = cpu.registers.es.reg().word() as usize;
        let bx = cpu.registers.bx.word() as usize;
        ((es << 4) + bx) & 0xFFFFF
    }

    /// Copies `data` into guest memory at the linear address `address`,
    /// wrapping at 1 MB, and returns the address behind it.
    pub(super) fn write_guest(cpu: &mut Cpu, address: usize, data: &[u8]) -> usize {
        for (i, byte) in data.iter().enumerate() {
            cpu.memory.write_byte((address + i) & 0xFFFFF, *byte);
        }
        (address + data.len()) & 0xFFFFF
    }

    /// Reads `length` bytes of guest memory at the linear address `address`,
    /// wrapping at 1 MB.
    pub(super) fn read_guest(cpu: &Cpu, address: usize, length: usize) -> Vec<u8> {
        (0..length)
            .map(|i| cpu.memory.read_byte((address + i) & 0xFFFFF))
            .collect()
    }

    /// The status for a write that failed.
    pub(super) fn write_error_status(error: &std::io::Error) -> u8 {
        if error.kind() == std::io::ErrorKind::PermissionDenied {
            STATUS_WRITE_PROTECTED
        } else {
            STATUS_SECTOR_NOT_FOUND
        }
    }

    pub(super) fn handle_trap(&mut self, cpu: &mut Cpu) {
        let function = cpu.registers.ax.high();
        let drive = cpu.registers.dx.low();

        // The BIOS start-up code only puts a default at 40:75. The number
        // of hard disks is known here, and it changes when the monitor
        // attaches or ejects one, so every call writes it.
        cpu.memory
            .write_byte(BDA_HARD_DISK_COUNT, self.hard_disk_count());

        let status = match function {
            // AH=00: Reset
            0x00 => STATUS_OK,

            // AH=01: Status of the last operation, in AH and AL
            0x01 => {
                let status = cpu.memory.read_byte(Self::status_address(drive));
                cpu.registers.ax.set_low(status);
                status
            }

            0x02 => self.read(cpu, drive),
            0x03 => self.write(cpu, drive),
            0x04 => self.verify(cpu, drive),
            0x05 => self.format(cpu, drive),
            0x08 => self.drive_parameters(cpu, drive),

            // AH=09/0C/0D/10/11/14: initialise, seek, reset, test ready,
            // recalibrate and diagnose have nothing to do on an image. They
            // succeed for a drive that exists.
            0x09 | 0x0C | 0x0D | 0x10 | 0x11 | 0x14 => {
                if self.drive_exists(drive) {
                    STATUS_OK
                } else {
                    STATUS_BAD_COMMAND
                }
            }

            // AH=15: Disk type. AH carries the type instead of a status.
            0x15 => {
                let disk_type = self.disk_type(cpu, drive);
                Self::finish(cpu, drive, STATUS_OK);
                cpu.registers.ax.set_high(disk_type);
                return;
            }

            0x16 => self.change_line(drive),

            // AH=41: Extensions installation check. The caller passes 55AAh
            // in BX; hard disks have the extended disk access functions
            // (42h-44h, 47h, 48h). AH carries the version instead of a
            // status.
            0x41 => {
                let asked_properly = cpu.registers.bx.word() == 0x55AA;
                if asked_properly && drive >= 0x80 && self.find_drive(drive).is_some() {
                    cpu.registers.bx.set(0xAA55);
                    cpu.registers.cx.set(0x0001); // extended disk access functions
                    Self::finish(cpu, drive, STATUS_OK);
                    cpu.registers.ax.set_high(0x01); // version 1.x
                    return;
                }
                STATUS_BAD_COMMAND
            }

            0x42 => self.read_lba(cpu, drive),
            0x43 => self.write_lba(cpu, drive),
            0x44 => self.verify_lba(cpu, drive),
            0x47 => self.seek_lba(cpu, drive),
            0x48 => self.extended_parameters(cpu, drive),

            _ => STATUS_BAD_COMMAND,
        };
        Self::finish(cpu, drive, status);
    }

    /// AH=02: Read sectors. A transfer that runs past the end of a track
    /// continues on the next one. Returns the status; AL is set to the
    /// number of sectors transferred.
    fn read(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let count = Self::sector_count(cpu);
        let (cylinder, head, sector) = Self::decode_chs(cpu);

        // CHS sector is 1-based; 0 is invalid.
        if sector == 0 {
            return STATUS_SECTOR_NOT_FOUND;
        }
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };
        // Sector number must fit within the disk's track geometry.
        if sector > disk.sectors_per_track {
            return STATUS_SECTOR_NOT_FOUND;
        }

        let mut address = Self::buffer_address(cpu);
        let mut done: u16 = 0;
        let runs = track_runs(
            disk.heads,
            disk.sectors_per_track,
            (cylinder, head, sector),
            count,
        );
        for run in runs {
            match disk.read_sectors(run.cylinder, run.head, run.sector, run.count) {
                Ok(data) => address = Self::write_guest(cpu, address, &data),
                Err(e) => {
                    debug!("Disk read error: {}", e);
                    cpu.registers.ax.set_low(done as u8);
                    return STATUS_SECTOR_NOT_FOUND;
                }
            }
            done += run.count as u16;
        }
        // AL reports count transferred; 256 wraps to 0 per BIOS convention.
        cpu.registers.ax.set_low(count as u8);
        STATUS_OK
    }

    /// AH=03: Write sectors, continuing across tracks like a read.
    fn write(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let count = Self::sector_count(cpu);
        let (cylinder, head, sector) = Self::decode_chs(cpu);

        if sector == 0 {
            return STATUS_SECTOR_NOT_FOUND;
        }
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };
        if sector > disk.sectors_per_track {
            return STATUS_SECTOR_NOT_FOUND;
        }

        let mut address = Self::buffer_address(cpu);
        let mut done: u16 = 0;
        let runs = track_runs(
            disk.heads,
            disk.sectors_per_track,
            (cylinder, head, sector),
            count,
        );
        for run in runs {
            let length = run.count as usize * SECTOR_SIZE;
            let data = Self::read_guest(cpu, address, length);
            address = (address + length) & 0xFFFFF;

            if let Err(e) = disk.write_sectors(run.cylinder, run.head, run.sector, run.count, &data)
            {
                debug!("Disk write error: {}", e);
                cpu.registers.ax.set_low(done as u8);
                return Self::write_error_status(&e);
            }
            done += run.count as u16;
        }
        // AL reports count transferred; 256 wraps to 0 per BIOS convention.
        cpu.registers.ax.set_low(count as u8);
        STATUS_OK
    }

    /// AH=04: Verify sectors. An image has nothing to verify; only the
    /// address is checked.
    fn verify(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let count = cpu.registers.ax.low();
        let (cylinder, head, sector) = Self::decode_chs(cpu);

        match self.find_drive(drive) {
            Some(disk) if disk.verify_sectors(cylinder, head, sector, count) => STATUS_OK,
            Some(_) => STATUS_SECTOR_NOT_FOUND,
            None => STATUS_NOT_READY,
        }
    }

    /// AH=05: Format track
    fn format(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let (cylinder, head, _) = Self::decode_chs(cpu);

        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };
        match disk.format_track(cylinder, head) {
            Ok(()) => STATUS_OK,
            Err(e) => {
                debug!("Disk format error: {}", e);
                Self::write_error_status(&e)
            }
        }
    }

    /// AH=08: Get drive parameters. CX and DH hold the highest cylinder,
    /// sector and head; DL the number of drives of that kind. For a floppy
    /// drive BX holds the drive type and ES:DI points to the diskette
    /// parameter table.
    fn drive_parameters(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let installed_floppy = self.is_installed_floppy(drive);
        let (max_cylinder, max_head, sectors, drive_type) = match self.find_drive(drive) {
            Some(disk) => (
                disk.cylinders - 1,
                disk.heads - 1,
                disk.sectors_per_track,
                floppy_drive_type(disk.total_bytes()),
            ),
            // The drive exists but holds no disk: describe the drive.
            None if installed_floppy => (
                EMPTY_FLOPPY_MAX_CYLINDER,
                EMPTY_FLOPPY_MAX_HEAD,
                EMPTY_FLOPPY_SECTORS,
                EMPTY_FLOPPY_DRIVE_TYPE,
            ),
            None => return STATUS_NO_PARAMETERS,
        };

        cpu.registers.ax.set_low(0x00);
        cpu.registers.cx.set_high((max_cylinder & 0xFF) as u8);
        cpu.registers
            .cx
            .set_low((sectors & 0x3F) | (((max_cylinder >> 8) as u8) << 6));
        cpu.registers.dx.set_high(max_head);
        if drive < 0x80 {
            cpu.registers.bx.set(drive_type as u16);
            cpu.registers.dx.set_low(self.floppy_drive_count());
            Self::point_to_diskette_parameters(cpu);
        } else {
            // BX describes floppy drives only and is left alone.
            cpu.registers.dx.set_low(self.hard_disk_count());
        }
        STATUS_OK
    }

    /// AH=15: Get disk type: 0 = no such drive, 2 = floppy drive with change
    /// line, 3 = hard disk, whose size in sectors is returned in CX:DX.
    ///
    /// Every floppy drive that exists has a change line, whatever disk is in
    /// it (or none): the guest decides once, at start-up, whether it may
    /// rely on AH=16h.
    fn disk_type(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        if drive < 0x80 {
            return if self.is_installed_floppy(drive) {
                0x02
            } else {
                0x00
            };
        }
        match self.find_drive(drive) {
            Some(disk) => {
                let total = disk.total_sectors() as u32;
                cpu.registers.cx.set((total >> 16) as u16);
                cpu.registers.dx.set((total & 0xFFFF) as u16);
                0x03
            }
            None => 0x00,
        }
    }

    /// AH=16: Change line status. A change is reported once.
    fn change_line(&mut self, drive: u8) -> u8 {
        match Self::floppy_unit(drive) {
            None => STATUS_BAD_COMMAND, // hard disks have no change line
            Some(_) if self.find_drive(drive).is_none() => STATUS_NOT_READY, // no disk
            Some(unit) if self.floppy_changed[unit] => {
                self.floppy_changed[unit] = false;
                STATUS_DISK_CHANGED
            }
            Some(_) => STATUS_OK,
        }
    }
}
