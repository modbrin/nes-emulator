//! This module contains supplementary utilites used by main logic

#[derive(Clone, Copy, Debug)]
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
    /// Invoking logic that is not implemented yet
    Unimplemented,
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

pub fn extract_overflow_bit(a: u8, b: u8, result: u8) -> bool {
    (a ^ result) & (b ^ result) & 0x80 != 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn overflow_bit_checks() {
        let expect_overflow = |a: u8, b: u8, bit: bool| {
            let result = a.wrapping_add(b);
            assert_eq!(extract_overflow_bit(a, b, result), bit)
        };

        expect_overflow(80, 16, false);
        expect_overflow(80, 80, true);
        expect_overflow(80, 144, false);
        expect_overflow(80, 208, false);
        expect_overflow(208, 16, false);
        expect_overflow(208, 80, false);
        expect_overflow(208, 144, true);
        expect_overflow(208, 208, false);
    }
}
