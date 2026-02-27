pub mod runtime;
#[cfg(feature = "gui")]
pub mod runtime_gui;
pub mod memory;
pub mod registers;
pub mod instructions;
mod modrm;
pub mod cli;
pub mod cpu;
