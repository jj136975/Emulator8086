use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;

pub struct DiskImage {
    file: File,
    pub cylinders: u16,
    pub heads: u8,
    pub sectors_per_track: u8,
}

impl DiskImage {
    pub fn open(path: &Path) -> std::io::Result<Self> {
        let file = File::open(path)?;
        let size = file.metadata()?.len();

        // Derive CHS geometry from file size
        let (cylinders, heads, spt) = match size {
            163_840 => (40, 1, 8),    // 160KB
            184_320 => (40, 1, 9),    // 180KB
            327_680 => (40, 2, 8),    // 320KB
            368_640 => (40, 2, 9),    // 360KB
            737_280 => (80, 2, 9),    // 720KB
            1_228_800 => (80, 2, 15), // 1.2MB
            1_474_560 => (80, 2, 18), // 1.44MB
            2_949_120 => (80, 2, 36), // 2.88MB
            _ => {
                // Generic: assume 2 heads, 18 spt
                let total_sectors = (size / 512) as u16;
                let spt = 18u8;
                let heads = 2u8;
                let cylinders = total_sectors / (heads as u16 * spt as u16);
                (cylinders.max(1), heads, spt)
            }
        };

        Ok(Self {
            file,
            cylinders: cylinders as u16,
            heads: heads as u8,
            sectors_per_track: spt as u8,
        })
    }

    pub fn read_sectors(&mut self, c: u16, h: u8, s: u8, count: u8) -> std::io::Result<Vec<u8>> {
        let lba = (c as u64 * self.heads as u64 + h as u64)
            * self.sectors_per_track as u64
            + (s as u64 - 1);
        let byte_offset = lba * 512;
        let total_bytes = count as usize * 512;

        self.file.seek(SeekFrom::Start(byte_offset))?;
        let mut buf = vec![0u8; total_bytes];
        self.file.read_exact(&mut buf)?;
        Ok(buf)
    }
}
