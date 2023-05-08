//! This module contains supplementary utilites used by main logic

use crate::prelude::*;

#[derive(Clone, Copy, Debug)]
pub enum NesError {
    /// Attempt to access memory out of bounds
    MemoryOutOfBounds,
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
    /// Addressing Mode is not supported by this instruction
    UnsupportedAddressingMode,
    /// Invoking logic that is not implemented yet
    Unimplemented,
    /// Attempting to write in rom section
    RomWriteAttempt,
}

pub struct InstructionMetadata {
    /// Number of cycles consumed by instruction
    pub cycles: u8,
    /// program counter state after execution
    pub update_pc: u16,
    /// Should the execution be aborted
    pub is_break: bool,
}

pub type InstResult = Result<InstructionMetadata, NesError>;

impl From<InstructionMetadata> for InstResult {
    fn from(meta: InstructionMetadata) -> Self {
        InstResult::Ok(meta)
    }
}

/// Represents address extracted from operands
pub struct AddrRes {
    /// Parameter address pointed via addressing mode
    pub addr: u16,
    /// Program counter state after param extraction
    pub pc_upd: u16,
}

impl AddrRes {
    pub fn new(addr: u16, pc_upd: u16) -> Self {
        Self { addr, pc_upd }
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

pub fn expect_mode(mode_a: AddressingMode, mode_b: AddressingMode) -> Result<(), NesError> {
    if mode_a != mode_b {
        return Err(NesError::UnsupportedAddressingMode);
    }
    Ok(())
}

/// Check if addition of two values `a` and `b` with same sign, results
/// in a different sign. E.g. adding positive numbers results in negative number,
/// or adding negative numbers results in positive number.
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
            assert_eq!(extract_overflow_bit(a, b, result), bit);
            assert_eq!(extract_overflow_bit(a, b, result.wrapping_add(1)), bit);
        };
        let as_u8 = |val: i8| -> u8 { unsafe { std::mem::transmute(val) } };

        expect_overflow(80, 16, false);
        expect_overflow(80, 80, true);
        expect_overflow(80, 144, false);
        expect_overflow(80, 208, false);
        expect_overflow(208, 16, false);
        expect_overflow(208, 80, false);
        expect_overflow(208, 144, true);
        expect_overflow(208, 208, false);

        expect_overflow(as_u8(-80), as_u8(-16), false);
        expect_overflow(as_u8(-80), as_u8(-80), true);
        expect_overflow(as_u8(-80), as_u8(112), false);
        expect_overflow(as_u8(-80), as_u8(48), false);
        expect_overflow(as_u8(48), as_u8(-16), false);
        expect_overflow(as_u8(48), as_u8(-80), false);
        expect_overflow(as_u8(48), as_u8(112), true);
        expect_overflow(as_u8(48), as_u8(48), false);
    }
}
