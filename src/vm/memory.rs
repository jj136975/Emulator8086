use crate::vm::registers::{ByteWrapper, WordWrapper};

const SEGMENT_SIZE: usize = (1 << 16) - 1;

pub struct Memory {
    pub stack: [u8; SEGMENT_SIZE],
    pub data: [u8; SEGMENT_SIZE],
    pub heap: [u8; SEGMENT_SIZE]
}

impl Memory {
    pub fn new(data: &[u8]) -> Self {
        let mut mem = Self {
            heap: [0; SEGMENT_SIZE],
            data: [0; SEGMENT_SIZE],
            stack: [0; SEGMENT_SIZE],
        };
        mem.data[..data.len()].copy_from_slice(data);
        mem
    }

    #[inline]
    pub fn ref_byte_data(&mut self, address: u16) -> ByteWrapper {
        ByteWrapper::new(&mut self.data[address as usize])
    }

    #[inline]
    pub fn read_byte_data(&self, address: u16) -> u8 {
        self.data[address as usize]
    }

    #[inline]
    pub fn read_word_data(&self, address: u16) -> u16 {
        let bytes: [u8; 2] = [self.data[address as usize], self.data[(address + 1) as usize]];
        u16::from_le_bytes(bytes)
    }

    #[inline]
    pub fn ref_word_data(&mut self, address: u16) -> WordWrapper {
        let slice: &mut [u8; 2] = (&mut self.data[address as usize ..= (address+1) as usize ])
            .try_into()
            .unwrap_or_else(|_| panic!("Invalid Memory address: {}", address));
        WordWrapper::from_slice(slice)
    }
}
