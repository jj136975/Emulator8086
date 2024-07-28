
#[derive(Debug)]
pub enum CPUid {
    Unknown,
    I8086 = 0x04,
    M68K = 0x0B,
    NS16K = 0x0C,
    I80386 = 0x10,
    SPARC = 0x17
}

impl CPUid {
    pub fn from_id(id: u8) -> CPUid {
        match id {
            0x04 => CPUid::I8086,
            0x0B => CPUid::M68K,
            0x0C => CPUid::NS16K,
            0x10 => CPUid::I80386,
            0x17 => CPUid::SPARC,
            _ => CPUid::Unknown,
        }
    }
}