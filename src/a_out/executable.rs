use std::io;
use std::io::{Cursor, Read, Result};

use byteorder::ReadBytesExt;

use crate::a_out::header::Header;

#[derive(Debug)]
pub struct Executable {
    header: Header,
    pub text_segment: Vec<u8>,
    pub data_segment: Vec<u8>,
}

impl Executable {
    pub fn from_reader(data: &mut impl Read) -> Result<Self> {
        let header = Header::from_reader(data)?;

        let mut text_segment: Vec<u8> = vec![0; header.text];
        data.read_exact(text_segment.as_mut_slice())?;

        let mut data_segment: Vec<u8> = vec![0; header.data];
        data.read_exact(data_segment.as_mut_slice())?;

        Ok(Self {
            header,
            text_segment,
            data_segment
        })
    }

    fn step<R: Read>(&self, reader: &mut R) -> Result<String> {
        let code = reader.read_u8()?;

        Ok(format!("HEX: {:x}", code))
    }

    pub fn disassemble(&self) -> Vec<String> {
        let mut lines: Vec<String> = Vec::new();

        let mut reader = Cursor::new(self.text_segment.as_slice());
        
        while let Ok(line) = self.step(&mut reader) {
            lines.push(line);
        }
        lines
    }
}