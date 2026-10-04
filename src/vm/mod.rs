//! The machine: the CPU state (`cpu`, `registers`, `memory`), the
//! instruction interpreter (`instructions`, with `modrm` decoding its
//! operands), the `runtime` that wires the devices to it and runs it, and the
//! monitor (`cli`).
//!
//! The invariant: a machine is single-threaded and its `Memory` box is
//! permanent. The segment registers hold raw pointers to that allocation,
//! and the operand references of an instruction point into it or into the
//! register file (`registers` has the rules), so the box is never replaced
//! and a `Runtime` never crosses threads.

pub mod cli;
pub mod cpu;
pub mod instructions;
pub mod memory;
mod modrm;
pub mod registers;
pub mod runtime;
#[cfg(feature = "gui")]
pub mod runtime_gui;
