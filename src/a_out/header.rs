use std::io;
use std::io::Read;

use byteorder::{LittleEndian, ReadBytesExt};

use crate::a_out::cpuid::CPUid;

const MAGIC: u16 = 0x0301;

#[derive(Debug)]
pub struct Header {
    magic: u16,   // magic number
    flags: u8,        // flags, see below
    cpu: CPUid,          // cpu id
    hdrlen: u8,       // length of header
    unused: u8,       // reserved for future use
    version: u16,     // version stamp (not used at present)
    pub(crate) text: usize,        // size of text segment in bytes
    pub(crate) data: usize,        // size of data segment in bytes
    bss: usize,         // size of bss segment in bytes
    entry: i32,       // entry point
    total: i32,       // total memory allocated
    syms: usize,        // size of symbol table
    ext: Option<HeaderExt>
}

#[derive(Debug)]
pub struct HeaderExt {
    trsize: i32,      // text relocation size
    drsize: i32,      // data relocation size
    tbase: i32,       // text relocation base
    dbase: i32,       // data relocation base
}

impl Header {
    pub fn from_reader(data: &mut impl Read) -> io::Result<Self> {
        let mut header = Self {
            magic: data.read_u16::<LittleEndian>()?,
            flags: data.read_u8()?,
            cpu: CPUid::from_id(data.read_u8()?),
            hdrlen: data.read_u8()?,
            unused: data.read_u8()?,
            version: data.read_u16::<LittleEndian>()?,
            text: data.read_i32::<LittleEndian>()? as usize,
            data: data.read_i32::<LittleEndian>()? as usize,
            bss: data.read_i32::<LittleEndian>()? as usize,
            entry: data.read_i32::<LittleEndian>()?,
            total: data.read_i32::<LittleEndian>()?,
            syms: data.read_i32::<LittleEndian>()? as usize,
            ext: None
        };
        if header.magic != MAGIC {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "Invalid magic number"));
        }
        if header.hdrlen > 32 {
            header.ext = Some(
                HeaderExt {
                    trsize: data.read_i32::<LittleEndian>()?,
                    drsize: data.read_i32::<LittleEndian>()?,
                    tbase: data.read_i32::<LittleEndian>()?,
                    dbase: data.read_i32::<LittleEndian>()?
                }
            );
        }
        Ok(header)
    }
}