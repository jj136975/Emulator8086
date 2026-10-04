pub mod cli;
pub mod cpu;
pub mod instructions;
pub mod memory;
mod modrm;
pub mod registers;
pub mod runtime;
#[cfg(feature = "gui")]
pub mod runtime_gui;
