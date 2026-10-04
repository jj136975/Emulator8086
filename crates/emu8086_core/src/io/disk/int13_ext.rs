//! The INT 13h extensions (AH=42h-44h, 47h and 48h): read, write, verify and
//! seek by block number, with a disk address packet at DS:SI, and the
//! extended drive parameters. The installation check (AH=41h, in `int13`)
//! announces these functions for hard disks.
//!
//! The invariant: a block number has 64 bits and is checked at that width.
//! `block_address` asks the disk for the CHS address before anything is
//! narrowed, so a block far beyond the disk is "sector not found", never the
//! sector its number turns into when it is cut to 32 bits or its cylinder to
//! 16. After a failed transfer the packet holds the number of blocks done.

use super::SECTOR_SIZE;
use super::controller::DiskController;
use super::image::DiskImage;
use super::int13::{STATUS_BAD_COMMAND, STATUS_NOT_READY, STATUS_OK, STATUS_SECTOR_NOT_FOUND};
use crate::vm::cpu::Cpu;
use log::debug;

/// The disk address packet a caller of the extended transfer functions
/// passes in DS:SI: its own size (byte 0), the number of blocks (word at 2),
/// the transfer buffer as offset and segment (words at 4 and 6) and the
/// 64-bit number of the first block (at 8).
struct AddressPacket {
    /// Linear address of the packet itself.
    address: usize,
    /// Number of blocks to transfer.
    count: u16,
    /// Linear address of the transfer buffer.
    buffer: usize,
    /// Number of the first block.
    lba: u64,
}

impl AddressPacket {
    const MIN_SIZE: u8 = 16;

    /// Reads the packet at the linear address `address`; `None` if its size
    /// field says that it is too short to hold a block number.
    fn read(cpu: &Cpu, address: usize) -> Option<Self> {
        if cpu.memory.read_byte(address) < Self::MIN_SIZE {
            return None;
        }
        let word = |offset: usize| cpu.memory.read_word(address + offset);
        let buffer = ((word(6) as usize) << 4) + word(4) as usize;
        let lba = (8..16)
            .step_by(2)
            .rev()
            .fold(0u64, |lba, offset| lba << 16 | word(offset) as u64);
        Some(Self {
            address,
            count: word(2),
            buffer: buffer & 0xFFFFF,
            lba,
        })
    }

    /// Stores the number of blocks that were transferred in the packet,
    /// where the caller looks for it after a failed transfer.
    fn set_transferred(&self, cpu: &mut Cpu, count: u16) {
        DiskController::write_guest(cpu, self.address + 2, &count.to_le_bytes());
    }
}

impl DiskController {
    /// The linear address DS:SI points to, where the extended functions
    /// find their argument.
    fn ds_si(cpu: &Cpu) -> usize {
        let ds = cpu.registers.ds.reg().word() as usize;
        let si = cpu.registers.si.word() as usize;
        ((ds << 4) + si) & 0xFFFFF
    }

    /// The CHS address of block `index` of the range a packet names. Blocks
    /// are numbered with 64 bits; the disk decides which of them exist.
    fn block_address(
        disk: &DiskImage,
        packet: &AddressPacket,
        index: u16,
    ) -> std::io::Result<(u16, u8, u8)> {
        packet
            .lba
            .checked_add(index as u64)
            .and_then(|lba| disk.lba_to_chs(lba))
            .ok_or_else(DiskImage::outside_the_disk)
    }

    /// AH=42: Extended read. On an error the packet says how many blocks
    /// were transferred.
    pub(super) fn read_lba(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let Some(packet) = AddressPacket::read(cpu, Self::ds_si(cpu)) else {
            return STATUS_BAD_COMMAND;
        };
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };

        let mut address = packet.buffer;
        for done in 0..packet.count {
            let block = Self::block_address(disk, &packet, done)
                .and_then(|(c, h, s)| disk.read_sectors(c, h, s, 1));
            match block {
                Ok(data) => address = Self::write_guest(cpu, address, &data),
                Err(e) => {
                    debug!("LBA read error: {}", e);
                    packet.set_transferred(cpu, done);
                    return STATUS_SECTOR_NOT_FOUND;
                }
            }
        }
        STATUS_OK
    }

    /// AH=43: Extended write. On an error the packet says how many blocks
    /// were transferred.
    pub(super) fn write_lba(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let Some(packet) = AddressPacket::read(cpu, Self::ds_si(cpu)) else {
            return STATUS_BAD_COMMAND;
        };
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };

        let mut address = packet.buffer;
        for done in 0..packet.count {
            let data = Self::read_guest(cpu, address, SECTOR_SIZE);
            address = (address + SECTOR_SIZE) & 0xFFFFF;

            let written = Self::block_address(disk, &packet, done)
                .and_then(|(c, h, s)| disk.write_sectors(c, h, s, 1, &data));
            if let Err(e) = written {
                debug!("LBA write error: {}", e);
                packet.set_transferred(cpu, done);
                return Self::write_error_status(&e);
            }
        }
        STATUS_OK
    }

    /// AH=44: Extended verify. An image has nothing to verify; the range of
    /// blocks is checked, and on an error the packet says how many of them
    /// are on the disk.
    pub(super) fn verify_lba(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        let Some(packet) = AddressPacket::read(cpu, Self::ds_si(cpu)) else {
            return STATUS_BAD_COMMAND;
        };
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };

        let on_disk = disk.total_sectors().saturating_sub(packet.lba);
        if packet.count as u64 > on_disk {
            packet.set_transferred(cpu, on_disk as u16);
            return STATUS_SECTOR_NOT_FOUND;
        }
        STATUS_OK
    }

    /// AH=47: Extended seek. Only the block number is checked.
    pub(super) fn seek_lba(&mut self, cpu: &Cpu, drive: u8) -> u8 {
        let Some(packet) = AddressPacket::read(cpu, Self::ds_si(cpu)) else {
            return STATUS_BAD_COMMAND;
        };
        match self.find_drive(drive) {
            Some(disk) if disk.lba_to_chs(packet.lba).is_some() => STATUS_OK,
            Some(_) => STATUS_SECTOR_NOT_FOUND,
            None => STATUS_NOT_READY,
        }
    }

    /// AH=48: Extended drive parameters. The caller's buffer at DS:SI
    /// starts with its size and receives the geometry, the number of
    /// sectors and the sector size.
    pub(super) fn extended_parameters(&mut self, cpu: &mut Cpu, drive: u8) -> u8 {
        /// Size of the result in version 1 of the interface.
        const RESULT_SIZE: u16 = 0x1A;
        /// Flags: the CHS values are valid; the medium is removable; the
        /// drive has a change line.
        const CHS_VALID: u16 = 0x0002;
        const REMOVABLE: u16 = 0x0004;
        const CHANGE_LINE: u16 = 0x0010;

        let buffer = Self::ds_si(cpu);
        if cpu.memory.read_word(buffer) < RESULT_SIZE {
            return STATUS_BAD_COMMAND;
        }
        let Some(disk) = self.find_drive(drive) else {
            return STATUS_NOT_READY;
        };
        let flags = if drive < 0x80 {
            CHS_VALID | REMOVABLE | CHANGE_LINE
        } else {
            CHS_VALID
        };

        let mut result = Vec::with_capacity(RESULT_SIZE as usize);
        result.extend_from_slice(&RESULT_SIZE.to_le_bytes());
        result.extend_from_slice(&flags.to_le_bytes());
        result.extend_from_slice(&(disk.cylinders as u32).to_le_bytes());
        result.extend_from_slice(&(disk.heads as u32).to_le_bytes());
        result.extend_from_slice(&(disk.sectors_per_track as u32).to_le_bytes());
        result.extend_from_slice(&disk.total_sectors().to_le_bytes());
        result.extend_from_slice(&(SECTOR_SIZE as u16).to_le_bytes());
        Self::write_guest(cpu, buffer, &result);
        STATUS_OK
    }
}
