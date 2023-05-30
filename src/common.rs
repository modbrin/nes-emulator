//! This module contains supplementary utilites used by main logic

use std::ops::Not;

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
    /// Attempting to write in ppu read-only memory
    PpuWriteForbidden,
    /// Attempting tot read from ppu write-only memory
    PpuReadForbidden,
    /// PPU Address out of valid range
    PpuAddressViolation,
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

pub fn mempage_differs(location_a: u16, location_b: u16) -> bool {
    location_a & 0xFF00 != location_b & 0xFF00
}

/// Check if addition of two values `a` and `b` with same sign, results
/// in a different sign. E.g. adding positive numbers results in negative number,
/// or adding negative numbers results in positive number.
pub fn extract_overflow_bit(a: u8, b: u8, result: u8) -> bool {
    (a ^ result) & (b ^ result) & 0x80 != 0
}

pub struct PixelColor {
    r: u8,
    g: u8,
    b: u8,
}

impl PixelColor {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b }
    }

    pub fn from_tuple(tuple: (u8, u8, u8)) -> Self {
        Self::new(tuple.0, tuple.1, tuple.2)
    }

    pub fn as_array(&self) -> [u8; 3] {
        [self.r, self.g, self.b]
    }
}

pub struct Frame {
    pub data: [u8; DISPLAY_SIZE_PAL],
}

impl Frame {
    pub fn new() -> Self {
        Frame {
            data: [0; DISPLAY_SIZE_PAL],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, color: PixelColor) {
        let idx = (y * DISPLAY_RES_PAL.0 + x) * 3;
        if idx + 2 < self.data.len() {
            self.data
                .get_mut(idx..idx + 3)
                .map(|pixel| pixel.copy_from_slice(&color.as_array()));
        }
    }
}

/// Reading from this address modifies the PPU state, therefore
/// it should not be read from a tracing utility.
fn is_forbidden_address(addr: u16) -> bool {
    match addr {
        PPU_MMAP_RNG_START..=PPU_MMAP_RNG_END => true,
        _ => false,
    }
}

fn get_operands_repr(device: &Device, mode: AddressingMode, op: Opcode) -> String {
    use AddressingMode::*;
    let lowercase = match mode {
        Immediate => {
            let val = device.read_one(device.cpu.pc + 1).unwrap();
            format!("#${:02x}", val)
        }
        Absolute => {
            let addr = device.read_le_u16(device.cpu.pc + 1).unwrap();
            let val = if is_forbidden_address(addr) {
                0x44
            } else {
                device.read_one(addr).unwrap()
            };
            match op {
                Opcode::JMP | Opcode::JSR => format!("${:04x}", addr),
                _ => format!("${:04x} = {:02x}", addr, val),
            }
        }
        AbsoluteX => {
            let addr = device.read_le_u16(device.cpu.pc + 1).unwrap();
            let res = addr.wrapping_add(device.cpu.reg_x as u16);
            let val = if is_forbidden_address(addr) {
                0x44
            } else {
                device.read_one(res).unwrap()
            };
            format!("${:04x},X @ {:04x} = {:02x}", addr, res, val)
        }
        AbsoluteY => {
            let addr = device.read_le_u16(device.cpu.pc + 1).unwrap();
            let res = addr.wrapping_add(device.cpu.reg_y as u16);
            let val = if is_forbidden_address(res) {
                0x44
            } else {
                device.read_one(res).unwrap()
            };
            format!("${:04x},Y @ {:04x} = {:02x}", addr, res, val)
        }
        Zeropage => {
            let addr = device.read_one(device.cpu.pc + 1).unwrap() as u16;
            let val = if is_forbidden_address(addr) {
                0x44
            } else {
                device.read_one(addr).unwrap()
            };
            format!("${:02x} = {:02x}", addr, val)
        }
        ZeropageX => {
            let addr = device.read_one(device.cpu.pc + 1).unwrap();
            let res = addr.wrapping_add(device.cpu.reg_x) as u16;
            let val = if is_forbidden_address(res) {
                0x44
            } else {
                device.read_one(res).unwrap()
            };
            format!("${:02x},X @ {:02x} = {:02x}", addr, res, val)
        }
        ZeropageY => {
            let addr = device.read_one(device.cpu.pc + 1).unwrap();
            let res = addr.wrapping_add(device.cpu.reg_y) as u16;
            let val = if is_forbidden_address(res) {
                0x44
            } else {
                device.read_one(res).unwrap()
            };
            format!("${:02x},Y @ {:02x} = {:02x}", addr, res, val)
        }
        IndexedIndirect => {
            let param = device.read_one(device.cpu.pc + 1).unwrap();
            let addr = param.wrapping_add(device.cpu.reg_x);
            let lo = device.read_one(addr as u16).unwrap() as u16;
            let hi = device.read_one(addr.wrapping_add(1) as u16).unwrap() as u16;
            let res = (hi << 8) | lo;
            let val = if is_forbidden_address(res) {
                0x44
            } else {
                device.read_one(res).unwrap()
            };
            format!(
                "(${:02x},X) @ {:02x} = {:04x} = {:02x}",
                param, addr, res, val
            )
        }
        IndirectIndexed => {
            let addr = device.read_one(device.cpu.pc + 1).unwrap();
            let lo = device.read_one(addr as u16).unwrap() as u16;
            let hi = device.read_one(addr.wrapping_add(1) as u16).unwrap() as u16;
            let loc = (hi << 8) | lo;
            let res = loc.wrapping_add(device.cpu.reg_y as u16);
            let val = if is_forbidden_address(res) {
                0x44
            } else {
                device.read_one(res).unwrap()
            };
            format!(
                "(${:02x}),Y = {:04x} @ {:04x} = {:02x}",
                addr, loc, res, val
            )
        }
        Accumulator => {
            format!("A")
        }
        Implied => {
            format!("")
        }
        Relative => {
            let mut new_pc = device.cpu.pc + 1;
            let offset = device.read_one(new_pc).unwrap();
            new_pc += 1;
            let target = if offset & 0x80 != 0 {
                // subtract two's complement
                new_pc.wrapping_sub(offset.not().wrapping_add(1) as u16)
            } else {
                new_pc.wrapping_add(offset as u16)
            };
            format!("${:04x}", target)
        }
        Indirect => {
            let addr = device.read_le_u16(device.cpu.pc + 1).unwrap();
            // 6502 bug imitation
            let loc = if addr & 0x00FF == 0x00FF {
                let lo = device.read_one(addr).unwrap() as u16;
                let hi = device.read_one(addr & 0xFF00).unwrap() as u16;
                hi << 8 | lo
            } else {
                if is_forbidden_address(addr) {
                    0x4444
                } else {
                    device.read_le_u16(addr).unwrap()
                }
            };
            format!("(${:04x}) = {:04x}", addr, loc)
        }
    };
    lowercase.to_uppercase()
}

pub fn trace(device: &Device) {
    let inst = device.bus.read_one(device.cpu.pc).unwrap();
    let (_, op, func, addr_mode, cycles) = crate::cpu::INST_TABLE[inst as usize];

    let pc_str = format!("{:04x}", device.cpu.pc).to_uppercase();

    let text_op = if op as u8 >= Opcode::ANC as u8 {
        format!("{}{:?}", "*", op)[0..4].to_string()
    } else {
        format!("{}{:?}", " ", op)
    };

    let b0 = device.bus.read_one(device.cpu.pc).unwrap();
    let b1 = device.bus.read_one(device.cpu.pc + 1);
    let b2 = device.bus.read_one(device.cpu.pc + 2);
    use AddressingMode::*;
    let bytes = match addr_mode {
        Implied | Accumulator => vec![b0],
        Indirect | Absolute | AbsoluteX | AbsoluteY => vec![b0, b1.unwrap(), b2.unwrap()],
        Relative | Immediate | Zeropage | ZeropageX | ZeropageY | IndirectIndexed
        | IndexedIndirect => vec![b0, b1.unwrap()],
    };
    let bytes_str = bytes
        .iter()
        .map(|b| format!("{:02x}", b).to_uppercase())
        .collect::<Vec<_>>()
        .join(" ");
    let operands = get_operands_repr(device, addr_mode, op);
    let registers = format!(
        "A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x}",
        device.cpu.reg_a, device.cpu.reg_x, device.cpu.reg_y, device.cpu.reg_p, device.cpu.sp
    )
    .to_uppercase();

    println!(
        "{}  {:8} {} {:27} {}",
        pc_str, bytes_str, text_op, operands, registers
    );
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
