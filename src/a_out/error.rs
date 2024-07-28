use std::io;

pub enum Error {
    UnsupportedCpu(u8),
    HeaderError(io::Error)
}