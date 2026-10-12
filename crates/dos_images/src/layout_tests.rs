//! The layouts of the disks, on volumes formatted in memory.

use super::*;
use crate::fat;
use crate::stamp;
use std::io::{Seek, SeekFrom, Write};

/// A sector whose bytes are `fill`, with the boot signature.
fn signed_sector(fill: u8) -> [u8; SECTOR] {
    let mut sector = [fill; SECTOR];
    sector[SIGNATURE..].copy_from_slice(&[0x55, 0xAA]);
    sector
}

/// A volume of `sectors`, formatted as `fat_type` with clusters of a
/// sector.
fn formatted(fat_type: fatfs::FatType, sectors: u32) -> Vec<u8> {
    let mut bytes = vec![0; sectors as usize * SECTOR];
    let options = fatfs::FormatVolumeOptions::new()
        .fat_type(fat_type)
        .bytes_per_cluster(SECTOR as u32)
        .total_sectors(sectors);
    fatfs::format_volume(Cursor::new(&mut bytes[..]), options).unwrap();
    bytes
}

/// Writes the files `names` onto `volume` in their order, each of
/// `clusters` sectors.
fn write_files(volume: &mut [u8], names: &[(&str, usize)]) {
    let mounted = fat::mount(volume, &stamp::MSDOS).unwrap();
    for (name, sectors) in names {
        fat::put(&mounted.root_dir(), name, &vec![0x90; sectors * SECTOR]).unwrap();
    }
    mounted.unmount().unwrap();
}

/// A partition entry gives a sector as its head, its sector with the high
/// bits of its cylinder above it, and the low bits of its cylinder; a
/// cylinder past 1023 is given as 1023.
#[test]
fn chs_addresses_are_those_of_a_partition_entry() {
    assert_eq!(chs(0), [0, 1, 0]);
    assert_eq!(chs(63), [1, 1, 0]);
    assert_eq!(chs(5 * 16 * 63 - 1), [15, 63, 4]);
    assert_eq!(chs(300 * 16 * 63), [0, 0x41, 0x2C]);
    assert_eq!(chs(1024 * 16 * 63 + 5), [0, 0xC6, 0xFF]);
}

/// The DOOM disk's partition: active, FAT12, from head 1 of cylinder 0 to
/// the end of its five cylinders, 4,977 sectors from sector 63.
#[test]
fn the_partition_entry_of_the_doom_disk() {
    assert_eq!(
        partition_entry(0x01, 63, 4977),
        [
            0x80, 0x01, 0x01, 0x00, 0x01, 0x0F, 0x3F, 0x04, 0x3F, 0x00, 0x00, 0x00, 0x71, 0x13,
            0x00, 0x00
        ]
    );
}

/// A new hard disk holds the code of its master boot record, its one
/// partition entry and the boot signature. Its volume is the partition, as
/// the entry says, also of a disk the guest left; of a disk that holds less
/// than its entry says, it is as much as the disk holds, and a disk too
/// short to hold the entry has none, which fails a mount at the end of its
/// bytes. Formatting the volume counts the sectors before it as hidden.
#[test]
fn a_hard_disk_holds_its_record_and_one_partition() {
    let record = signed_sector(0xEE);
    let mut bytes = HardDisk::new(5, 0x01, Some(&record)).into_bytes();
    assert_eq!(bytes.len(), 5 * 16 * 63 * SECTOR);
    assert_eq!(bytes[..PARTITION_TABLE], record[..PARTITION_TABLE]);
    assert_eq!(
        bytes[PARTITION_TABLE..PARTITION_TABLE + 16],
        partition_entry(0x01, 63, 4977)
    );
    assert_eq!(bytes[SIGNATURE..SECTOR], [0x55, 0xAA]);
    assert!(
        bytes[PARTITION_TABLE + 16..SIGNATURE]
            .iter()
            .all(|&b| b == 0)
    );

    bytes[63 * SECTOR] = 0xAB;
    assert_eq!(volume_of(&bytes).len(), 4977 * SECTOR);
    assert_eq!(volume_of(&bytes)[0], 0xAB, "the volume begins at sector 63");
    assert_eq!(volume_of(&bytes[..100 * SECTOR]).len(), 37 * SECTOR);
    assert!(volume_of(&bytes[..PARTITION_TABLE + 15]).is_empty());
    assert!(volume_of(&bytes[..4]).is_empty());
    let mounted = fat::mount_to_read(volume_of(&bytes[..4]), &stamp::MSDOS);
    assert_eq!(
        mounted.err().map(|error| error.kind()),
        Some(io::ErrorKind::UnexpectedEof)
    );

    let mut disk = HardDisk::new(5, 0x01, None);
    let options = fatfs::FormatVolumeOptions::new()
        .fat_type(fatfs::FatType::Fat12)
        .bytes_per_cluster(2048)
        .total_sectors(4977);
    disk.format(options).unwrap();
    assert_eq!(
        disk.volume()[HIDDEN_SECTORS..HIDDEN_SECTORS + 4],
        63u32.to_le_bytes()
    );
    let mounted = fat::mount(disk.volume(), &stamp::MSDOS).unwrap();
    assert_eq!(mounted.fat_type(), fatfs::FatType::Fat12);
    assert_eq!(mounted.cluster_size(), 2048);
}

/// The boot sector is the last sector of MSBOOT.BIN, the record the last
/// of FDBOOT.BIN; an image of another length, or a sector without the boot
/// signature, is refused.
#[test]
fn the_boot_sector_and_the_record_are_the_last_sectors_of_their_images() {
    let mut msboot = vec![0; 0x7E00 - SECTOR];
    msboot.extend_from_slice(&signed_sector(0xB0));
    assert_eq!(boot_sector(&msboot), Ok(signed_sector(0xB0)));
    let mut fdboot = vec![0; 0x600];
    fdboot.extend_from_slice(&signed_sector(0xF0));
    assert_eq!(master_boot_record(&fdboot), Ok(signed_sector(0xF0)));

    assert_eq!(
        boot_sector(&msboot[1..]),
        Err(LayoutError::Image {
            name: "BOOT\\MSBOOT.BIN",
            length: 0x7E00 - 1,
            expected: 0x7E00
        })
    );
    *fdboot.last_mut().unwrap() = 0;
    assert_eq!(
        master_boot_record(&fdboot),
        Err(LayoutError::NoSignature("CMD\\FDISK\\FDBOOT.BIN"))
    );
}

/// The boot sector put on a volume keeps the volume's BIOS parameter block
/// and extended one, reads from the drive given, and is the build's code
/// everywhere else.
#[test]
fn the_boot_sector_keeps_the_volumes_parameters() {
    let mut volume = formatted(fatfs::FatType::Fat12, 2880);
    let parameters = volume[BPB].to_vec();
    let boot = signed_sector(0xB0);
    install_boot_sector(&mut volume, &boot, 0x80);
    let mut expected = parameters;
    expected[DRIVE - BPB.start] = 0x80;
    expected[DRIVE + 1 - BPB.start] = 0;
    assert_eq!(volume[BPB], expected[..]);
    assert_eq!(volume[..BPB.start], boot[..BPB.start]);
    assert_eq!(volume[BPB.end..SECTOR], boot[BPB.end..]);
    assert!(
        fat::mount(&mut volume, &stamp::MSDOS).is_ok(),
        "fatfs reads it still"
    );
}

/// Sealing gives IO.SYS and MSDOS.SYS SYS's attributes, on FAT12 as on
/// FAT16, and touches nothing else.
#[test]
fn the_system_files_are_sealed_where_the_boot_sector_finds_them() {
    for (fat_type, sectors) in [(fatfs::FatType::Fat12, 2880), (fatfs::FatType::Fat16, 9000)] {
        let mut volume = formatted(fat_type, sectors);
        write_files(
            &mut volume,
            &[("IO.SYS", 3), ("MSDOS.SYS", 2), ("COMMAND.COM", 1)],
        );
        let before = volume.clone();
        assert_eq!(seal_system_files(&mut volume), Ok(()), "{fat_type:?}");
        let root = Bpb::of(&volume).root;
        let changed: Vec<usize> = (0..volume.len())
            .filter(|&at| volume[at] != before[at])
            .collect();
        assert_eq!(changed, [root + 11, root + 32 + 11]);
        assert_eq!(
            (volume[root + 11], volume[root + 43]),
            (SYSTEM_FILE, SYSTEM_FILE)
        );
    }
}

/// A volume whose root does not begin with IO.SYS and MSDOS.SYS, whose
/// IO.SYS does not begin at the first data cluster, or whose IO.SYS is not
/// one run of clusters, is refused, and left as it was.
#[test]
fn a_volume_the_boot_sector_cannot_load_is_refused() {
    let mut volume = formatted(fatfs::FatType::Fat12, 2880);
    write_files(
        &mut volume,
        &[("COMMAND.COM", 1), ("IO.SYS", 1), ("MSDOS.SYS", 1)],
    );
    assert_eq!(
        seal_system_files(&mut volume),
        Err(LayoutError::RootOrder {
            first: "COMMAND.COM".to_string(),
            second: "IO.SYS".to_string()
        })
    );

    let mut volume = formatted(fatfs::FatType::Fat12, 2880);
    write_files(&mut volume, &[("IO.SYS", 1), ("MSDOS.SYS", 1)]);
    {
        let mounted = fat::mount(&mut volume, &stamp::MSDOS).unwrap();
        let mut io_sys = mounted.root_dir().open_file("IO.SYS").unwrap();
        io_sys.seek(SeekFrom::End(0)).unwrap();
        io_sys.write_all(&[0x90; SECTOR]).unwrap();
    }
    let before = volume.clone();
    assert_eq!(
        seal_system_files(&mut volume),
        Err(LayoutError::Fragmented(2))
    );
    assert!(volume == before);

    let mut volume = formatted(fatfs::FatType::Fat12, 2880);
    write_files(&mut volume, &[("IO.SYS", 1), ("MSDOS.SYS", 1)]);
    let root = Bpb::of(&volume).root;
    volume[root + 26] = 5;
    assert_eq!(
        seal_system_files(&mut volume),
        Err(LayoutError::NotFirst(5))
    );
}
