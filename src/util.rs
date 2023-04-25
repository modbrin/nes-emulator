//! This module contains supplementary utilites used by main logic

pub enum NesError {
    /// Attempt to access memory out of bounds
    RamOutOfBounds,
    /// Stack size limit exceeded
    StackOverflow,
    /// Attempt to pop empty stack
    StackUnderflow,
    /// Program counter overflow
    PcOverflow,
    /// Program counter underflow
    PcUnderflow,
    /// ROM is larger than designated region in memory
    OversizedRom,
    /// Addressing Mode is not supported currently
    UnsupportedAddressingMode,
}

pub struct InstructionMetadata {
    /// Number of cycles consumed by instruction
    pub cycles: u8,
    /// Apply non-default program counter after execution
    pub pc_override: Option<u16>,
    /// Should the execution be aborted
    pub is_break: bool,
}

impl InstructionMetadata {
    pub fn with_cycles(cycles: u8) -> Self {
        Self {
            cycles,
            pc_override: None,
            is_break: false,
        }
    }

    pub fn normal() -> Self {
        Self {
            cycles: 1,
            pc_override: None,
            is_break: false,
        }
    }

    pub fn stop() -> Self {
        Self {
            cycles: 1,
            pc_override: None,
            is_break: true,
        }
    }
}

pub type InstResult = Result<InstructionMetadata, NesError>;

impl From<InstructionMetadata> for InstResult {
    fn from(meta: InstructionMetadata) -> Self {
        InstResult::Ok(meta)
    }
}

pub trait ByteExt {
    fn set_by_mask(&mut self, mask: u8);
    fn unset_by_mask(&mut self, mask: u8);
}

impl ByteExt for u8 {
    fn set_by_mask(&mut self, mask: u8) {
        *self |= mask;
    }
    fn unset_by_mask(&mut self, mask: u8) {
        *self &= !mask;
    }
}
