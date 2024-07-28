
const SEGMENT_SIZE: usize = (1 << 16) - 1;

pub struct Memory {
    stack: [u8; SEGMENT_SIZE],
    heap: [u8; SEGMENT_SIZE]
}