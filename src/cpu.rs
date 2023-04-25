//! This module contains layout and logic of CPU and instruction handlers

use crate::prelude::*;

type InstPtr = for<'a> fn(&'a mut Device, AddressingMode) -> Result<InstructionMetadata, NesError>;

#[rustfmt::skip]
static INST_TABLE: &[(u8, Opcode, InstPtr, AddressingMode, u8)] = {
    use Opcode::*;
    use AddressingMode::*;
    type D = Device;
    &[
         /*0x00*/                                       /*0x01*/                                       /*0x02*/                                       /*0x03*/
/*0x00*/ (0x00, BRK, D::inst_brk, Implied,         7),  (0x01, LDA, D::inst_non, IndexedIndirect, 1),  (0x02, LDA, D::inst_non, IndexedIndirect, 1),  (0x03, LDA, D::inst_non, IndexedIndirect, 1),
/*0x04*/ (0x04, LDA, D::inst_non, IndexedIndirect, 1),  (0x05, LDA, D::inst_non, IndexedIndirect, 1),  (0x06, LDA, D::inst_non, IndexedIndirect, 1),  (0x07, LDA, D::inst_non, IndexedIndirect, 1),
/*0x08*/ (0x08, LDA, D::inst_non, IndexedIndirect, 1),  (0x09, LDA, D::inst_non, IndexedIndirect, 1),  (0x0A, LDA, D::inst_non, IndexedIndirect, 1),  (0x0B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x0C*/ (0x0C, LDA, D::inst_non, IndexedIndirect, 1),  (0x0D, LDA, D::inst_non, IndexedIndirect, 1),  (0x0E, LDA, D::inst_non, IndexedIndirect, 1),  (0x0F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x10*/ (0x10, LDA, D::inst_non, IndexedIndirect, 1),  (0x11, LDA, D::inst_non, IndexedIndirect, 1),  (0x12, LDA, D::inst_non, IndexedIndirect, 1),  (0x13, LDA, D::inst_non, IndexedIndirect, 1),
/*0x14*/ (0x14, LDA, D::inst_non, IndexedIndirect, 1),  (0x15, LDA, D::inst_non, IndexedIndirect, 1),  (0x16, LDA, D::inst_non, IndexedIndirect, 1),  (0x17, LDA, D::inst_non, IndexedIndirect, 1),
/*0x18*/ (0x18, LDA, D::inst_non, IndexedIndirect, 1),  (0x19, LDA, D::inst_non, IndexedIndirect, 1),  (0x1A, LDA, D::inst_non, IndexedIndirect, 1),  (0x1B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x1C*/ (0x1C, LDA, D::inst_non, IndexedIndirect, 1),  (0x1D, LDA, D::inst_non, IndexedIndirect, 1),  (0x1E, LDA, D::inst_non, IndexedIndirect, 1),  (0x1F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x20*/ (0x20, LDA, D::inst_non, IndexedIndirect, 1),  (0x21, LDA, D::inst_non, IndexedIndirect, 1),  (0x22, LDA, D::inst_non, IndexedIndirect, 1),  (0x23, LDA, D::inst_non, IndexedIndirect, 1),
/*0x24*/ (0x24, LDA, D::inst_non, IndexedIndirect, 1),  (0x25, LDA, D::inst_non, IndexedIndirect, 1),  (0x26, LDA, D::inst_non, IndexedIndirect, 1),  (0x27, LDA, D::inst_non, IndexedIndirect, 1),
/*0x28*/ (0x28, LDA, D::inst_non, IndexedIndirect, 1),  (0x29, LDA, D::inst_non, IndexedIndirect, 1),  (0x2A, LDA, D::inst_non, IndexedIndirect, 1),  (0x2B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x2C*/ (0x2C, LDA, D::inst_non, IndexedIndirect, 1),  (0x2D, LDA, D::inst_non, IndexedIndirect, 1),  (0x2E, LDA, D::inst_non, IndexedIndirect, 1),  (0x2F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x30*/ (0x30, LDA, D::inst_non, IndexedIndirect, 1),  (0x31, LDA, D::inst_non, IndexedIndirect, 1),  (0x32, LDA, D::inst_non, IndexedIndirect, 1),  (0x33, LDA, D::inst_non, IndexedIndirect, 1),
/*0x34*/ (0x34, LDA, D::inst_non, IndexedIndirect, 1),  (0x35, LDA, D::inst_non, IndexedIndirect, 1),  (0x36, LDA, D::inst_non, IndexedIndirect, 1),  (0x37, LDA, D::inst_non, IndexedIndirect, 1),
/*0x38*/ (0x38, LDA, D::inst_non, IndexedIndirect, 1),  (0x39, LDA, D::inst_non, IndexedIndirect, 1),  (0x3A, LDA, D::inst_non, IndexedIndirect, 1),  (0x3B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x3C*/ (0x3C, LDA, D::inst_non, IndexedIndirect, 1),  (0x3D, LDA, D::inst_non, IndexedIndirect, 1),  (0x3E, LDA, D::inst_non, IndexedIndirect, 1),  (0x3F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x40*/ (0x40, LDA, D::inst_non, IndexedIndirect, 1),  (0x41, LDA, D::inst_non, IndexedIndirect, 1),  (0x42, LDA, D::inst_non, IndexedIndirect, 1),  (0x43, LDA, D::inst_non, IndexedIndirect, 1),
/*0x44*/ (0x44, LDA, D::inst_non, IndexedIndirect, 1),  (0x45, LDA, D::inst_non, IndexedIndirect, 1),  (0x46, LDA, D::inst_non, IndexedIndirect, 1),  (0x47, LDA, D::inst_non, IndexedIndirect, 1),
/*0x48*/ (0x48, LDA, D::inst_non, IndexedIndirect, 1),  (0x49, LDA, D::inst_non, IndexedIndirect, 1),  (0x4A, LDA, D::inst_non, IndexedIndirect, 1),  (0x4B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x4C*/ (0x4C, LDA, D::inst_non, IndexedIndirect, 1),  (0x4D, LDA, D::inst_non, IndexedIndirect, 1),  (0x4E, LDA, D::inst_non, IndexedIndirect, 1),  (0x4F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x50*/ (0x50, LDA, D::inst_non, IndexedIndirect, 1),  (0x51, LDA, D::inst_non, IndexedIndirect, 1),  (0x52, LDA, D::inst_non, IndexedIndirect, 1),  (0x53, LDA, D::inst_non, IndexedIndirect, 1),
/*0x54*/ (0x54, LDA, D::inst_non, IndexedIndirect, 1),  (0x55, LDA, D::inst_non, IndexedIndirect, 1),  (0x56, LDA, D::inst_non, IndexedIndirect, 1),  (0x57, LDA, D::inst_non, IndexedIndirect, 1),
/*0x58*/ (0x58, LDA, D::inst_non, IndexedIndirect, 1),  (0x59, LDA, D::inst_non, IndexedIndirect, 1),  (0x5A, LDA, D::inst_non, IndexedIndirect, 1),  (0x5B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x5C*/ (0x5C, LDA, D::inst_non, IndexedIndirect, 1),  (0x5D, LDA, D::inst_non, IndexedIndirect, 1),  (0x5E, LDA, D::inst_non, IndexedIndirect, 1),  (0x5F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x60*/ (0x60, LDA, D::inst_non, IndexedIndirect, 1),  (0x61, LDA, D::inst_non, IndexedIndirect, 1),  (0x62, LDA, D::inst_non, IndexedIndirect, 1),  (0x63, LDA, D::inst_non, IndexedIndirect, 1),
/*0x64*/ (0x64, LDA, D::inst_non, IndexedIndirect, 1),  (0x65, LDA, D::inst_non, IndexedIndirect, 1),  (0x66, LDA, D::inst_non, IndexedIndirect, 1),  (0x67, LDA, D::inst_non, IndexedIndirect, 1),
/*0x68*/ (0x68, LDA, D::inst_non, IndexedIndirect, 1),  (0x69, LDA, D::inst_non, IndexedIndirect, 1),  (0x6A, LDA, D::inst_non, IndexedIndirect, 1),  (0x6B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x6C*/ (0x6C, LDA, D::inst_non, IndexedIndirect, 1),  (0x6D, LDA, D::inst_non, IndexedIndirect, 1),  (0x6E, LDA, D::inst_non, IndexedIndirect, 1),  (0x6F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x70*/ (0x70, LDA, D::inst_non, IndexedIndirect, 1),  (0x71, LDA, D::inst_non, IndexedIndirect, 1),  (0x72, LDA, D::inst_non, IndexedIndirect, 1),  (0x73, LDA, D::inst_non, IndexedIndirect, 1),
/*0x74*/ (0x74, LDA, D::inst_non, IndexedIndirect, 1),  (0x75, LDA, D::inst_non, IndexedIndirect, 1),  (0x76, LDA, D::inst_non, IndexedIndirect, 1),  (0x77, LDA, D::inst_non, IndexedIndirect, 1),
/*0x78*/ (0x78, LDA, D::inst_non, IndexedIndirect, 1),  (0x79, LDA, D::inst_non, IndexedIndirect, 1),  (0x7A, LDA, D::inst_non, IndexedIndirect, 1),  (0x7B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x7C*/ (0x7C, LDA, D::inst_non, IndexedIndirect, 1),  (0x7D, LDA, D::inst_non, IndexedIndirect, 1),  (0x7E, LDA, D::inst_non, IndexedIndirect, 1),  (0x7F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x80*/ (0x80, LDA, D::inst_non, IndexedIndirect, 1),  (0x81, LDA, D::inst_non, IndexedIndirect, 1),  (0x82, LDA, D::inst_non, IndexedIndirect, 1),  (0x83, LDA, D::inst_non, IndexedIndirect, 1),
/*0x84*/ (0x84, LDA, D::inst_non, IndexedIndirect, 1),  (0x85, LDA, D::inst_non, IndexedIndirect, 1),  (0x86, LDA, D::inst_non, IndexedIndirect, 1),  (0x87, LDA, D::inst_non, IndexedIndirect, 1),
/*0x88*/ (0x88, LDA, D::inst_non, IndexedIndirect, 1),  (0x89, LDA, D::inst_non, IndexedIndirect, 1),  (0x8A, LDA, D::inst_non, IndexedIndirect, 1),  (0x8B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x8C*/ (0x8C, LDA, D::inst_non, IndexedIndirect, 1),  (0x8D, LDA, D::inst_non, IndexedIndirect, 1),  (0x8E, LDA, D::inst_non, IndexedIndirect, 1),  (0x8F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x90*/ (0x90, LDA, D::inst_non, IndexedIndirect, 1),  (0x91, LDA, D::inst_non, IndexedIndirect, 1),  (0x92, LDA, D::inst_non, IndexedIndirect, 1),  (0x93, LDA, D::inst_non, IndexedIndirect, 1),
/*0x94*/ (0x94, LDA, D::inst_non, IndexedIndirect, 1),  (0x95, LDA, D::inst_non, IndexedIndirect, 1),  (0x96, LDA, D::inst_non, IndexedIndirect, 1),  (0x97, LDA, D::inst_non, IndexedIndirect, 1),
/*0x98*/ (0x98, LDA, D::inst_non, IndexedIndirect, 1),  (0x99, LDA, D::inst_non, IndexedIndirect, 1),  (0x9A, LDA, D::inst_non, IndexedIndirect, 1),  (0x9B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x9C*/ (0x9C, LDA, D::inst_non, IndexedIndirect, 1),  (0x9D, LDA, D::inst_non, IndexedIndirect, 1),  (0x9E, LDA, D::inst_non, IndexedIndirect, 1),  (0x9F, LDA, D::inst_non, IndexedIndirect, 1),
/*0xA0*/ (0xA0, LDA, D::inst_non, IndexedIndirect, 1),  (0xA1, LDA, D::inst_lda, IndexedIndirect, 6),  (0xA2, LDA, D::inst_non, IndexedIndirect, 1),  (0xA3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xA4*/ (0xA4, LDA, D::inst_non, IndexedIndirect, 1),  (0xA5, LDA, D::inst_lda, Zeropage,        3),  (0xA6, LDA, D::inst_non, IndexedIndirect, 1),  (0xA7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xA8*/ (0xA8, LDA, D::inst_non, IndexedIndirect, 1),  (0xA9, LDA, D::inst_lda, Immediate,       2),  (0xAA, TAX, D::inst_tax, Implied,         2),  (0xAB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xAC*/ (0xAC, LDA, D::inst_non, IndexedIndirect, 1),  (0xAD, LDA, D::inst_lda, Absolute,        4),  (0xAE, LDA, D::inst_non, IndexedIndirect, 1),  (0xAF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xB0*/ (0xB0, LDA, D::inst_non, IndexedIndirect, 1),  (0xB1, LDA, D::inst_lda, IndirectIndexed, 5),  (0xB2, LDA, D::inst_non, IndexedIndirect, 1),  (0xB3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xB4*/ (0xB4, LDA, D::inst_non, IndexedIndirect, 1),  (0xB5, LDA, D::inst_lda, ZeropageX,       4),  (0xB6, LDA, D::inst_non, IndexedIndirect, 1),  (0xB7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xB8*/ (0xB8, LDA, D::inst_non, IndexedIndirect, 1),  (0xB9, LDA, D::inst_lda, AbsoluteY,       4),  (0xBA, LDA, D::inst_non, IndexedIndirect, 1),  (0xBB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xBC*/ (0xBC, LDA, D::inst_non, IndexedIndirect, 1),  (0xBD, LDA, D::inst_lda, AbsoluteX,       4),  (0xBE, LDA, D::inst_non, IndexedIndirect, 1),  (0xBF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xC0*/ (0xC0, LDA, D::inst_non, IndexedIndirect, 1),  (0xC1, LDA, D::inst_non, IndexedIndirect, 1),  (0xC2, LDA, D::inst_non, IndexedIndirect, 1),  (0xC3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xC4*/ (0xC4, LDA, D::inst_non, IndexedIndirect, 1),  (0xC5, LDA, D::inst_non, IndexedIndirect, 1),  (0xC6, LDA, D::inst_non, IndexedIndirect, 1),  (0xC7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xC8*/ (0xC8, LDA, D::inst_non, IndexedIndirect, 1),  (0xC9, LDA, D::inst_non, IndexedIndirect, 1),  (0xCA, LDA, D::inst_non, IndexedIndirect, 1),  (0xCB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xCC*/ (0xCC, LDA, D::inst_non, IndexedIndirect, 1),  (0xCD, LDA, D::inst_non, IndexedIndirect, 1),  (0xCE, LDA, D::inst_non, IndexedIndirect, 1),  (0xCF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xD0*/ (0xD0, LDA, D::inst_non, IndexedIndirect, 1),  (0xD1, LDA, D::inst_non, IndexedIndirect, 1),  (0xD2, LDA, D::inst_non, IndexedIndirect, 1),  (0xD3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xD4*/ (0xD4, LDA, D::inst_non, IndexedIndirect, 1),  (0xD5, LDA, D::inst_non, IndexedIndirect, 1),  (0xD6, LDA, D::inst_non, IndexedIndirect, 1),  (0xD7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xD8*/ (0xD8, LDA, D::inst_non, IndexedIndirect, 1),  (0xD9, LDA, D::inst_non, IndexedIndirect, 1),  (0xDA, LDA, D::inst_non, IndexedIndirect, 1),  (0xDB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xDC*/ (0xDC, LDA, D::inst_non, IndexedIndirect, 1),  (0xDD, LDA, D::inst_non, IndexedIndirect, 1),  (0xDE, LDA, D::inst_non, IndexedIndirect, 1),  (0xDF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xE0*/ (0xE0, LDA, D::inst_non, IndexedIndirect, 1),  (0xE1, LDA, D::inst_non, IndexedIndirect, 1),  (0xE2, LDA, D::inst_non, IndexedIndirect, 1),  (0xE3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xE4*/ (0xE4, LDA, D::inst_non, IndexedIndirect, 1),  (0xE5, LDA, D::inst_non, IndexedIndirect, 1),  (0xE6, LDA, D::inst_non, IndexedIndirect, 1),  (0xE7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xE8*/ (0xE8, LDA, D::inst_non, IndexedIndirect, 1),  (0xE9, LDA, D::inst_non, IndexedIndirect, 1),  (0xEA, LDA, D::inst_non, IndexedIndirect, 1),  (0xEB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xEC*/ (0xEC, LDA, D::inst_non, IndexedIndirect, 1),  (0xED, LDA, D::inst_non, IndexedIndirect, 1),  (0xEE, LDA, D::inst_non, IndexedIndirect, 1),  (0xEF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xF0*/ (0xF0, LDA, D::inst_non, IndexedIndirect, 1),  (0xF1, LDA, D::inst_non, IndexedIndirect, 1),  (0xF2, LDA, D::inst_non, IndexedIndirect, 1),  (0xF3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xF4*/ (0xF4, LDA, D::inst_non, IndexedIndirect, 1),  (0xF5, LDA, D::inst_non, IndexedIndirect, 1),  (0xF6, LDA, D::inst_non, IndexedIndirect, 1),  (0xF7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xF8*/ (0xF8, LDA, D::inst_non, IndexedIndirect, 1),  (0xF9, LDA, D::inst_non, IndexedIndirect, 1),  (0xFA, LDA, D::inst_non, IndexedIndirect, 1),  (0xFB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xFC*/ (0xFC, LDA, D::inst_non, IndexedIndirect, 1),  (0xFD, LDA, D::inst_non, IndexedIndirect, 1),  (0xFE, LDA, D::inst_non, IndexedIndirect, 1),  (0xFF, LDA, D::inst_non, IndexedIndirect, 1),
    ]
};

/// 8-bit 6502 CPU
#[derive(Default)]
pub struct Cpu {
    /// accumulator register
    pub reg_a: u8,
    /// index register X
    pub reg_x: u8,
    /// index register Y
    pub reg_y: u8,
    /// status register
    pub reg_p: u8,
    /// stack pointer
    pub sp: u8,
    /// program counter
    pub pc: u16,
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }
}

impl Device {
    /// decode and execute the instruction
    pub(crate) fn decode_and_execute(&mut self, opcode: u8) -> InstResult {
        match opcode {
            0x00 => self.inst_brk(AddressingMode::Implied),
            0xA9 => self.inst_lda(AddressingMode::Absolute),
            0xAA => self.inst_tax(AddressingMode::Implied),
            0x85 => self.inst_sta(AddressingMode::Zeropage),
            0x95 => self.inst_sta(AddressingMode::ZeropageX),

            _ => self.unknown(opcode),
        }
    }

    pub(crate) fn unknown(&mut self, opcode: u8) -> InstResult {
        println!("Unknown opcode: {:#04x}", opcode);
        Meta::normal().into()
    }

    pub(crate) fn todo(&mut self) -> InstResult {
        println!("Encountered TODO");
        Meta::normal().into()
    }
}

/// Flag Operations
impl Device {
    fn get_flag(&self, flag: Flag) -> bool {
        let mask = flag as u8;
        self.cpu.reg_p & mask != 0
    }

    fn set_flag(&mut self, flag: Flag, is_set: bool) {
        let mask = flag as u8;
        if is_set {
            self.cpu.reg_p.set_by_mask(mask);
        } else {
            self.cpu.reg_p.unset_by_mask(mask);
        }
    }

    fn update_zero_negative_flags(&mut self, target: u8) {
        self.set_flag(Flag::Zero, target == 0);
        self.set_flag(Flag::Negative, target & BIT7 != 0);
    }
}

impl Device {
    fn inst_non(&mut self, _mode: AddressingMode) -> InstResult {
        Err(NesError::Unimplemented)
    }

    /// BRK
    fn inst_brk(&mut self, _mode: AddressingMode) -> InstResult {
        Meta::stop().into()
    }
    /// ADC - Add with Carry
    fn inst_adc(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param_val = self.ram_read(param_addr)?;
        let (mut res, mut overflow) = param_val.overflowing_add(self.cpu.reg_a);
        if self.get_flag(Flag::Carry) {
            let (res_carry, overflow_carry) = res.overflowing_add(1);
            res = res_carry;
            overflow |= overflow_carry;
        }
        self.cpu.reg_a = res;
        self.set_flag(Flag::Carry, overflow);
        self.update_zero_negative_flags(self.cpu.reg_a);

        Meta::normal().into()
    }

    /// LDA
    fn inst_lda(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.cpu.reg_a = self.ram_read(param_addr)?;
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// TAX
    fn inst_tax(&mut self, _mode: AddressingMode) -> InstResult {
        self.cpu.reg_x = self.cpu.reg_a;
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// INX
    fn inst_inx(&mut self, _mode: AddressingMode) -> InstResult {
        self.cpu.reg_x = self.cpu.reg_x.wrapping_add(1);
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// INY
    fn inst_iny(&mut self, _mode: AddressingMode) -> InstResult {
        self.cpu.reg_y = self.cpu.reg_y.wrapping_add(1);
        self.update_zero_negative_flags(self.cpu.reg_y);
        Meta::normal().into()
    }

    /// STA
    fn inst_sta(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.ram_write(param_addr, self.cpu.reg_a)?;
        Meta::normal().into()
    }
}

#[cfg(test)]
mod tests {
    use super::INST_TABLE;

    #[test]
    fn validate_table_bytecodes() {
        for (i, (op, ..)) in INST_TABLE.iter().enumerate() {
            assert_eq!(i, *op as usize);
        }
    }
}
