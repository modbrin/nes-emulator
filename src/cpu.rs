//! This module contains layout and logic of CPU and instruction handlers

use std::ops::Not;

use crate::prelude::*;

type InstPtr = for<'a> fn(&'a mut Device, AddressingMode) -> Result<InstructionMetadata, NesError>;

#[rustfmt::skip]
static INST_TABLE: &[(u8, Opcode, InstPtr, AddressingMode, u8)] = {
    use Opcode::*;
    use AddressingMode::*;
    type D = Device;
    &[
         /*0x00*/                                       /*0x01*/                                       /*0x02*/                                       /*0x03*/
/*0x00*/ (0x00, BRK, D::inst_brk, Implied,         7),  (0x01, ORA, D::inst_ora, IndexedIndirect, 6),  (0x02, LDA, D::inst_non, IndexedIndirect, 1),  (0x03, LDA, D::inst_non, IndexedIndirect, 1),
/*0x04*/ (0x04, LDA, D::inst_non, IndexedIndirect, 1),  (0x05, ORA, D::inst_ora, Zeropage,        3),  (0x06, ASL, D::inst_asl, Zeropage,        5),  (0x07, LDA, D::inst_non, IndexedIndirect, 1),
/*0x08*/ (0x08, PHP, D::inst_php, Implied,         3),  (0x09, ORA, D::inst_ora, Immediate,       2),  (0x0A, ASL, D::inst_asl, Accumulator,     2),  (0x0B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x0C*/ (0x0C, LDA, D::inst_non, IndexedIndirect, 1),  (0x0D, ORA, D::inst_ora, Absolute,        4),  (0x0E, ASL, D::inst_asl, Absolute,        6),  (0x0F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x10*/ (0x10, BPL, D::inst_bpl, Relative,        2),  (0x11, ORA, D::inst_ora, IndirectIndexed, 5),  (0x12, LDA, D::inst_non, IndexedIndirect, 1),  (0x13, LDA, D::inst_non, IndexedIndirect, 1),
/*0x14*/ (0x14, LDA, D::inst_non, IndexedIndirect, 1),  (0x15, ORA, D::inst_ora, ZeropageX,       4),  (0x16, ASL, D::inst_asl, ZeropageX,       6),  (0x17, LDA, D::inst_non, IndexedIndirect, 1),
/*0x18*/ (0x18, CLC, D::inst_clc, Implied,         2),  (0x19, ORA, D::inst_ora, AbsoluteY,       4),  (0x1A, LDA, D::inst_non, IndexedIndirect, 1),  (0x1B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x1C*/ (0x1C, LDA, D::inst_non, IndexedIndirect, 1),  (0x1D, ORA, D::inst_ora, AbsoluteX,       4),  (0x1E, ASL, D::inst_asl, AbsoluteX,       7),  (0x1F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x20*/ (0x20, JSR, D::inst_jsr, Absolute,        6),  (0x21, AND, D::inst_and, IndexedIndirect, 6),  (0x22, LDA, D::inst_non, IndexedIndirect, 1),  (0x23, LDA, D::inst_non, IndexedIndirect, 1),
/*0x24*/ (0x24, BIT, D::inst_bit, Zeropage,        3),  (0x25, AND, D::inst_and, Zeropage,        3),  (0x26, ROL, D::inst_rol, Zeropage,        5),  (0x27, LDA, D::inst_non, IndexedIndirect, 1),
/*0x28*/ (0x28, PLP, D::inst_plp, Implied,         4),  (0x29, AND, D::inst_and, Immediate,       2),  (0x2A, ROL, D::inst_rol, Accumulator,     2),  (0x2B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x2C*/ (0x2C, BIT, D::inst_bit, Absolute,        4),  (0x2D, AND, D::inst_and, Absolute,        4),  (0x2E, ROL, D::inst_rol, Absolute,        6),  (0x2F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x30*/ (0x30, BMI, D::inst_bmi, Relative,        2),  (0x31, AND, D::inst_and, IndirectIndexed, 5),  (0x32, LDA, D::inst_non, IndexedIndirect, 1),  (0x33, LDA, D::inst_non, IndexedIndirect, 1),
/*0x34*/ (0x34, LDA, D::inst_non, IndexedIndirect, 1),  (0x35, AND, D::inst_and, ZeropageX,       4),  (0x36, ROL, D::inst_rol, ZeropageX,       6),  (0x37, LDA, D::inst_non, IndexedIndirect, 1),
/*0x38*/ (0x38, SEC, D::inst_sec, Implied,         2),  (0x39, AND, D::inst_and, AbsoluteY,       4),  (0x3A, LDA, D::inst_non, IndexedIndirect, 1),  (0x3B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x3C*/ (0x3C, LDA, D::inst_non, IndexedIndirect, 1),  (0x3D, AND, D::inst_and, AbsoluteX,       4),  (0x3E, ROL, D::inst_rol, AbsoluteX,       7),  (0x3F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x40*/ (0x40, RTI, D::inst_rti, Implied,         6),  (0x41, EOR, D::inst_eor, IndexedIndirect, 6),  (0x42, LDA, D::inst_non, IndexedIndirect, 1),  (0x43, LDA, D::inst_non, IndexedIndirect, 1),
/*0x44*/ (0x44, LDA, D::inst_non, IndexedIndirect, 1),  (0x45, EOR, D::inst_eor, Zeropage,        3),  (0x46, LSR, D::inst_lsr, Zeropage,        5),  (0x47, LDA, D::inst_non, IndexedIndirect, 1),
/*0x48*/ (0x48, PHA, D::inst_pha, Implied,         3),  (0x49, EOR, D::inst_eor, Immediate,       2),  (0x4A, LSR, D::inst_lsr, Accumulator,     2),  (0x4B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x4C*/ (0x4C, JMP, D::inst_jmp, Absolute,        3),  (0x4D, EOR, D::inst_eor, Absolute,        4),  (0x4E, LSR, D::inst_lsr, Absolute,        6),  (0x4F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x50*/ (0x50, BVC, D::inst_bvc, Relative,        2),  (0x51, EOR, D::inst_eor, IndirectIndexed, 5),  (0x52, LDA, D::inst_non, IndexedIndirect, 1),  (0x53, LDA, D::inst_non, IndexedIndirect, 1),
/*0x54*/ (0x54, LDA, D::inst_non, IndexedIndirect, 1),  (0x55, EOR, D::inst_eor, ZeropageX,       4),  (0x56, LSR, D::inst_lsr, ZeropageX,       6),  (0x57, LDA, D::inst_non, IndexedIndirect, 1),
/*0x58*/ (0x58, CLI, D::inst_cli, Implied,         2),  (0x59, EOR, D::inst_eor, AbsoluteY,       4),  (0x5A, LDA, D::inst_non, IndexedIndirect, 1),  (0x5B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x5C*/ (0x5C, LDA, D::inst_non, IndexedIndirect, 1),  (0x5D, EOR, D::inst_eor, AbsoluteX,       4),  (0x5E, LSR, D::inst_lsr, AbsoluteX,       7),  (0x5F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x60*/ (0x60, RTS, D::inst_rts, Implied,         6),  (0x61, ADC, D::inst_adc, IndexedIndirect, 6),  (0x62, LDA, D::inst_non, IndexedIndirect, 1),  (0x63, LDA, D::inst_non, IndexedIndirect, 1),
/*0x64*/ (0x64, LDA, D::inst_non, IndexedIndirect, 1),  (0x65, ADC, D::inst_adc, Zeropage,        3),  (0x66, ROR, D::inst_ror, Zeropage,        5),  (0x67, LDA, D::inst_non, IndexedIndirect, 1),
/*0x68*/ (0x68, PLA, D::inst_pla, Implied,         4),  (0x69, ADC, D::inst_adc, Immediate,       2),  (0x6A, ROR, D::inst_ror, Accumulator,     2),  (0x6B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x6C*/ (0x6C, JMP, D::inst_jmp, Indirect,        5),  (0x6D, ADC, D::inst_adc, Absolute,        4),  (0x6E, ROR, D::inst_ror, Absolute,        6),  (0x6F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x70*/ (0x70, BVS, D::inst_bvs, Relative,        2),  (0x71, ADC, D::inst_adc, IndirectIndexed, 5),  (0x72, LDA, D::inst_non, IndexedIndirect, 1),  (0x73, LDA, D::inst_non, IndexedIndirect, 1),
/*0x74*/ (0x74, LDA, D::inst_non, IndexedIndirect, 1),  (0x75, ADC, D::inst_adc, ZeropageX,       4),  (0x76, ROR, D::inst_ror, ZeropageX,       6),  (0x77, LDA, D::inst_non, IndexedIndirect, 1),
/*0x78*/ (0x78, SEI, D::inst_sei, Implied,         2),  (0x79, ADC, D::inst_adc, AbsoluteY,       4),  (0x7A, LDA, D::inst_non, IndexedIndirect, 1),  (0x7B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x7C*/ (0x7C, LDA, D::inst_non, IndexedIndirect, 1),  (0x7D, ADC, D::inst_adc, AbsoluteX,       4),  (0x7E, ROR, D::inst_ror, AbsoluteX,       7),  (0x7F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x80*/ (0x80, LDA, D::inst_non, IndexedIndirect, 1),  (0x81, STA, D::inst_sta, IndexedIndirect, 6),  (0x82, LDA, D::inst_non, IndexedIndirect, 1),  (0x83, LDA, D::inst_non, IndexedIndirect, 1),
/*0x84*/ (0x84, STY, D::inst_sty, Zeropage,        3),  (0x85, STA, D::inst_sta, Zeropage,        3),  (0x86, STX, D::inst_stx, Zeropage,        3),  (0x87, LDA, D::inst_non, IndexedIndirect, 1),
/*0x88*/ (0x88, DEY, D::inst_dey, Implied,         2),  (0x89, LDA, D::inst_non, IndexedIndirect, 1),  (0x8A, TXA, D::inst_txa, Implied,         2),  (0x8B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x8C*/ (0x8C, STY, D::inst_sty, Absolute,        4),  (0x8D, STA, D::inst_sta, Absolute,        4),  (0x8E, STX, D::inst_stx, Absolute,        4),  (0x8F, LDA, D::inst_non, IndexedIndirect, 1),
/*0x90*/ (0x90, BCC, D::inst_bcc, Relative,        2),  (0x91, STA, D::inst_sta, IndirectIndexed, 6),  (0x92, LDA, D::inst_non, IndexedIndirect, 1),  (0x93, LDA, D::inst_non, IndexedIndirect, 1),
/*0x94*/ (0x94, STY, D::inst_sty, ZeropageX,       4),  (0x95, STA, D::inst_sta, ZeropageX,       4),  (0x96, STX, D::inst_stx, ZeropageY,       4),  (0x97, LDA, D::inst_non, IndexedIndirect, 1),
/*0x98*/ (0x98, TYA, D::inst_tya, Implied,         2),  (0x99, STA, D::inst_sta, AbsoluteY,       5),  (0x9A, TXS, D::inst_txs, Implied,         2),  (0x9B, LDA, D::inst_non, IndexedIndirect, 1),
/*0x9C*/ (0x9C, LDA, D::inst_non, IndexedIndirect, 1),  (0x9D, STA, D::inst_sta, AbsoluteX,       5),  (0x9E, LDA, D::inst_non, IndexedIndirect, 1),  (0x9F, LDA, D::inst_non, IndexedIndirect, 1),
/*0xA0*/ (0xA0, LDY, D::inst_ldy, Immediate,       2),  (0xA1, LDA, D::inst_lda, IndexedIndirect, 6),  (0xA2, LDX, D::inst_ldx, Immediate,       2),  (0xA3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xA4*/ (0xA4, LDY, D::inst_ldy, Zeropage,        3),  (0xA5, LDA, D::inst_lda, Zeropage,        3),  (0xA6, LDX, D::inst_ldx, Zeropage,        3),  (0xA7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xA8*/ (0xA8, TAY, D::inst_tay, Implied,         2),  (0xA9, LDA, D::inst_lda, Immediate,       2),  (0xAA, TAX, D::inst_tax, Implied,         2),  (0xAB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xAC*/ (0xAC, LDY, D::inst_ldy, Absolute,        4),  (0xAD, LDA, D::inst_lda, Absolute,        4),  (0xAE, LDX, D::inst_ldx, Absolute,        4),  (0xAF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xB0*/ (0xB0, BCS, D::inst_bcs, Relative,        2),  (0xB1, LDA, D::inst_lda, IndirectIndexed, 5),  (0xB2, LDA, D::inst_non, IndexedIndirect, 1),  (0xB3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xB4*/ (0xB4, LDY, D::inst_ldy, ZeropageX,       4),  (0xB5, LDA, D::inst_lda, ZeropageX,       4),  (0xB6, LDX, D::inst_ldx, ZeropageY,       4),  (0xB7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xB8*/ (0xB8, CLV, D::inst_clv, Implied,         2),  (0xB9, LDA, D::inst_lda, AbsoluteY,       4),  (0xBA, TSX, D::inst_tsx, Implied,         2),  (0xBB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xBC*/ (0xBC, LDY, D::inst_ldy, AbsoluteY,       4),  (0xBD, LDA, D::inst_lda, AbsoluteX,       4),  (0xBE, LDX, D::inst_ldx, AbsoluteY,       4),  (0xBF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xC0*/ (0xC0, CPY, D::inst_cpy, Immediate,       2),  (0xC1, CMP, D::inst_cmp, IndexedIndirect, 6),  (0xC2, LDA, D::inst_non, IndexedIndirect, 1),  (0xC3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xC4*/ (0xC4, CPY, D::inst_cpy, Zeropage,        3),  (0xC5, CMP, D::inst_cmp, Zeropage,        3),  (0xC6, DEC, D::inst_dec, Zeropage,        5),  (0xC7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xC8*/ (0xC8, INY, D::inst_iny, Implied,         2),  (0xC9, CMP, D::inst_cmp, Immediate,       2),  (0xCA, DEX, D::inst_dex, Implied,         2),  (0xCB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xCC*/ (0xCC, CPY, D::inst_cpy, Absolute,        4),  (0xCD, CMP, D::inst_cmp, Absolute,        4),  (0xCE, DEC, D::inst_dec, Absolute,        6),  (0xCF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xD0*/ (0xD0, BNE, D::inst_bne, Relative,        2),  (0xD1, CMP, D::inst_cmp, IndirectIndexed, 5),  (0xD2, LDA, D::inst_non, IndexedIndirect, 1),  (0xD3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xD4*/ (0xD4, LDA, D::inst_non, IndexedIndirect, 1),  (0xD5, CMP, D::inst_cmp, ZeropageX,       4),  (0xD6, DEC, D::inst_dec, ZeropageX,       6),  (0xD7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xD8*/ (0xD8, CLD, D::inst_cld, Implied,         2),  (0xD9, CMP, D::inst_cmp, AbsoluteY,       4),  (0xDA, LDA, D::inst_non, IndexedIndirect, 1),  (0xDB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xDC*/ (0xDC, LDA, D::inst_non, IndexedIndirect, 1),  (0xDD, CMP, D::inst_cmp, AbsoluteX,       4),  (0xDE, DEC, D::inst_dec, AbsoluteX,       7),  (0xDF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xE0*/ (0xE0, CPX, D::inst_cpx, Immediate,       2),  (0xE1, SBC, D::inst_sbc, IndexedIndirect, 6),  (0xE2, LDA, D::inst_non, IndexedIndirect, 1),  (0xE3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xE4*/ (0xE4, CPX, D::inst_cpx, Zeropage,        3),  (0xE5, SBC, D::inst_sbc, Zeropage,        3),  (0xE6, INC, D::inst_inc, Zeropage,        5),  (0xE7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xE8*/ (0xE8, INX, D::inst_inx, Implied,         2),  (0xE9, SBC, D::inst_sbc, Immediate,       2),  (0xEA, LDA, D::inst_non, IndexedIndirect, 1),  (0xEB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xEC*/ (0xEC, CPX, D::inst_cpx, Absolute,        4),  (0xED, SBC, D::inst_sbc, Absolute,        4),  (0xEE, INC, D::inst_inc, Absolute,        6),  (0xEF, LDA, D::inst_non, IndexedIndirect, 1),
/*0xF0*/ (0xF0, BEQ, D::inst_beq, Relative,        2),  (0xF1, SBC, D::inst_sbc, IndirectIndexed, 5),  (0xF2, LDA, D::inst_non, IndexedIndirect, 1),  (0xF3, LDA, D::inst_non, IndexedIndirect, 1),
/*0xF4*/ (0xF4, LDA, D::inst_non, IndexedIndirect, 1),  (0xF5, SBC, D::inst_sbc, ZeropageX,       4),  (0xF6, INC, D::inst_inc, ZeropageX,       6),  (0xF7, LDA, D::inst_non, IndexedIndirect, 1),
/*0xF8*/ (0xF8, SED, D::inst_sed, IndexedIndirect, 2),  (0xF9, SBC, D::inst_sbc, AbsoluteY,       4),  (0xFA, LDA, D::inst_non, IndexedIndirect, 1),  (0xFB, LDA, D::inst_non, IndexedIndirect, 1),
/*0xFC*/ (0xFC, LDA, D::inst_non, IndexedIndirect, 1),  (0xFD, SBC, D::inst_sbc, AbsoluteX,       4),  (0xFE, INC, D::inst_inc, AbsoluteX,       7),  (0xFF, LDA, D::inst_non, IndexedIndirect, 1),
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

    fn branch_if(&mut self, cond: bool) -> Result<(), NesError> {
        if cond {
            let offset = self.fetch_next()?;
            if offset & 0x80 != 0 {
                // subtract two's complement
                self.cpu.pc = self
                    .cpu
                    .pc
                    .wrapping_sub(offset.not().wrapping_add(1) as u16);
            } else {
                self.cpu.pc = self.cpu.pc.wrapping_add(offset as u16);
            }
        } else {
            // consume unused argument
            self.skip_one()?;
        }
        Ok(())
    }

    fn cmp_with_mem(&mut self, mode: AddressingMode, target: u8) -> Result<(), NesError> {
        let param_addr = self.fetch_param_addr(mode)?;
        let param = self.ram_read(param_addr)?;
        self.set_flag(Flag::Carry, target >= param);
        self.set_flag(Flag::Zero, target == param);
        self.set_flag(Flag::Negative, target.wrapping_sub(param) & BIT7 != 0);
        Ok(())
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
        // TODO: not properly implemented
        Meta::stop().into()
    }
    /// ADC - Add with Carry
    fn inst_adc(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param_val = self.ram_read(param_addr)?;
        let (mut res, mut overflow) = self.cpu.reg_a.overflowing_add(param_val);
        if self.get_flag(Flag::Carry) {
            let (res_carry, overflow_carry) = res.overflowing_add(1);
            res = res_carry;
            overflow |= overflow_carry;
        }
        self.set_flag(
            Flag::Overflow,
            extract_overflow_bit(param_val, self.cpu.reg_a, res),
        );
        self.cpu.reg_a = res;
        self.set_flag(Flag::Carry, overflow);
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// ASL
    fn inst_and(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param = self.ram_read(param_addr)?;
        self.cpu.reg_a &= param;
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// ASL
    fn inst_asl(&mut self, mode: AddressingMode) -> InstResult {
        if mode == AddressingMode::Accumulator {
            self.set_flag(Flag::Carry, self.cpu.reg_a & BIT7 != 0);
            self.cpu.reg_a = self.cpu.reg_a << 1;
            self.update_zero_negative_flags(self.cpu.reg_a);
        } else {
            let val_addr = self.fetch_param_addr(mode)?;
            let val = self.ram_read(val_addr)?;
            self.set_flag(Flag::Carry, val & BIT7 != 0);
            self.ram_write(val_addr, val << 1)?;
            self.update_zero_negative_flags(val);
        }
        Meta::normal().into()
    }

    /// BCC
    fn inst_bcc(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(!self.get_flag(Flag::Carry))?;
        Meta::normal().into()
    }

    /// BCS
    fn inst_bcs(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(self.get_flag(Flag::Carry))?;
        Meta::normal().into()
    }

    /// BEQ
    fn inst_beq(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(self.get_flag(Flag::Zero))?;
        Meta::normal().into()
    }

    /// BIT
    fn inst_bit(&mut self, mode: AddressingMode) -> InstResult {
        let loc_addr = self.fetch_param_addr(mode)?;
        let loc = self.ram_read(loc_addr)?;
        self.set_flag(Flag::Overflow, loc & BIT6 != 0);
        self.set_flag(Flag::Negative, loc & BIT7 != 0);
        self.set_flag(Flag::Zero, self.cpu.reg_a & loc == 0);
        Meta::normal().into()
    }

    /// BMI
    fn inst_bmi(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(self.get_flag(Flag::Negative))?;
        Meta::normal().into()
    }

    /// BNE
    fn inst_bne(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(!self.get_flag(Flag::Zero))?;
        Meta::normal().into()
    }

    /// BPL
    fn inst_bpl(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(!self.get_flag(Flag::Negative))?;
        Meta::normal().into()
    }

    /// BVC
    fn inst_bvc(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(!self.get_flag(Flag::Overflow))?;
        Meta::normal().into()
    }

    /// BVS
    fn inst_bvs(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(self.get_flag(Flag::Overflow))?;
        Meta::normal().into()
    }

    /// CLC
    fn inst_clc(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Carry, false);
        Meta::normal().into()
    }

    /// CLD
    fn inst_cld(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Decimal, false);
        Meta::normal().into()
    }

    /// CLI
    fn inst_cli(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::IntDis, false);
        Meta::normal().into()
    }

    /// CLV
    fn inst_clv(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Overflow, false);
        Meta::normal().into()
    }

    /// CMP
    fn inst_cmp(&mut self, mode: AddressingMode) -> InstResult {
        self.cmp_with_mem(mode, self.cpu.reg_a)?;
        Meta::normal().into()
    }

    /// CPX
    fn inst_cpx(&mut self, mode: AddressingMode) -> InstResult {
        self.cmp_with_mem(mode, self.cpu.reg_x)?;
        Meta::normal().into()
    }

    /// CPY
    fn inst_cpy(&mut self, mode: AddressingMode) -> InstResult {
        self.cmp_with_mem(mode, self.cpu.reg_y)?;
        Meta::normal().into()
    }

    /// DEC
    fn inst_dec(&mut self, mode: AddressingMode) -> InstResult {
        let loc_addr = self.fetch_param_addr(mode)?;
        let mut loc = self.ram_read(loc_addr)?;
        loc = loc.wrapping_sub(1);
        self.update_zero_negative_flags(loc);
        self.ram_write(loc_addr, loc)?;
        Meta::normal().into()
    }

    /// DEX
    fn inst_dex(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_x = self.cpu.reg_x.wrapping_sub(1);
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// DEY
    fn inst_dey(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_y = self.cpu.reg_y.wrapping_sub(1);
        self.update_zero_negative_flags(self.cpu.reg_y);
        Meta::normal().into()
    }

    /// EOR
    fn inst_eor(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param = self.ram_read(param_addr)?;
        self.cpu.reg_a = self.cpu.reg_a ^ param;
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// INC
    fn inst_inc(&mut self, mode: AddressingMode) -> InstResult {
        let loc_addr = self.fetch_param_addr(mode)?;
        let mut loc = self.ram_read(loc_addr)?;
        loc = loc.wrapping_add(1);
        self.update_zero_negative_flags(loc);
        self.ram_write(loc_addr, loc)?;
        Meta::normal().into()
    }

    /// INX
    fn inst_inx(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_x = self.cpu.reg_x.wrapping_add(1);
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// INY
    fn inst_iny(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_y = self.cpu.reg_y.wrapping_add(1);
        self.update_zero_negative_flags(self.cpu.reg_y);
        Meta::normal().into()
    }

    /// JMP
    fn inst_jmp(&mut self, mode: AddressingMode) -> InstResult {
        let jump_to = if mode == AddressingMode::Indirect {
            let addr = self.ram_read_le_u16(self.cpu.pc)?;
            self.skip_n(2)?;
            // 6502 bug imitation
            let loc = if addr & 0x00FF == 0xFF00 {
                let lo = self.ram_read(addr)? as u16;
                let hi = self.ram_read(addr & 0xFF00)? as u16;
                hi << 8 | lo
            } else {
                self.ram_read_le_u16(addr)?
            };
            loc
        } else {
            self.fetch_param_addr(mode)?
        };
        self.cpu.pc = jump_to;
        Meta::normal().into()
    }

    /// JSR
    fn inst_jsr(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Absolute)?;
        self.stack_push_le_u16(self.cpu.pc + 2 - 1)?;
        self.cpu.pc = self.ram_read_le_u16(self.cpu.pc)?;
        Meta::normal().into()
    }

    /// LDA
    fn inst_lda(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.cpu.reg_a = self.ram_read(param_addr)?;
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// LDX
    fn inst_ldx(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.cpu.reg_x = self.ram_read(param_addr)?;
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// LDY
    fn inst_ldy(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.cpu.reg_y = self.ram_read(param_addr)?;
        self.update_zero_negative_flags(self.cpu.reg_y);
        Meta::normal().into()
    }

    /// LSR
    fn inst_lsr(&mut self, mode: AddressingMode) -> InstResult {
        if mode == AddressingMode::Accumulator {
            self.set_flag(Flag::Carry, self.cpu.reg_a & BIT0 != 0);
            self.cpu.reg_a = self.cpu.reg_a >> 1;
            self.update_zero_negative_flags(self.cpu.reg_a);
        } else {
            let val_addr = self.fetch_param_addr(mode)?;
            let val = self.ram_read(val_addr)?;
            self.set_flag(Flag::Carry, val & BIT0 != 0);
            self.ram_write(val_addr, val >> 1)?;
            self.update_zero_negative_flags(val);
        }
        Meta::normal().into()
    }

    /// NOP
    fn inst_nop(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        Meta::normal().into()
    }

    /// ORA
    fn inst_ora(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param = self.ram_read(param_addr)?;
        self.cpu.reg_a = self.cpu.reg_a | param;
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// PHA
    fn inst_pha(&mut self, _mode: AddressingMode) -> InstResult {
        self.stack_push(self.cpu.reg_a)?;
        Meta::normal().into()
    }

    /// PHP
    fn inst_php(&mut self, _mode: AddressingMode) -> InstResult {
        self.stack_push(self.cpu.reg_p)?;
        self.set_flag(Flag::B, true);
        self.set_flag(Flag::B2, true);
        Meta::normal().into()
    }

    /// PLA
    fn inst_pla(&mut self, _mode: AddressingMode) -> InstResult {
        self.cpu.reg_a = self.stack_pop()?;
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// PLP
    fn inst_plp(&mut self, _mode: AddressingMode) -> InstResult {
        self.cpu.reg_p = self.stack_pop()?;
        self.set_flag(Flag::B, false);
        self.set_flag(Flag::B2, true);
        Meta::normal().into()
    }

    /// ROL
    fn inst_rol(&mut self, mode: AddressingMode) -> InstResult {
        let carry_mask: u8 = self.get_flag(Flag::Carry).then(|| 1).unwrap_or(0);
        if mode == AddressingMode::Accumulator {
            self.set_flag(Flag::Carry, self.cpu.reg_a & BIT7 != 0);
            self.cpu.reg_a = (self.cpu.reg_a << 1) | carry_mask;
            self.update_zero_negative_flags(self.cpu.reg_a);
        } else {
            let val_addr = self.fetch_param_addr(mode)?;
            let val = self.ram_read(val_addr)?;
            self.set_flag(Flag::Carry, val & BIT7 != 0);
            self.ram_write(val_addr, (val << 1) | carry_mask)?;
            self.update_zero_negative_flags(val);
        }
        Meta::normal().into()
    }

    /// ROR
    fn inst_ror(&mut self, mode: AddressingMode) -> InstResult {
        let carry_mask: u8 = self.get_flag(Flag::Carry).then(|| 1 << 7).unwrap_or(0);
        if mode == AddressingMode::Accumulator {
            self.set_flag(Flag::Carry, self.cpu.reg_a & BIT0 != 0);
            self.cpu.reg_a = (self.cpu.reg_a >> 1) | carry_mask;
            self.update_zero_negative_flags(self.cpu.reg_a);
        } else {
            let val_addr = self.fetch_param_addr(mode)?;
            let val = self.ram_read(val_addr)?;
            self.set_flag(Flag::Carry, val & BIT0 != 0);
            self.ram_write(val_addr, (val >> 1) | carry_mask)?;
            self.update_zero_negative_flags(val);
        }
        Meta::normal().into()
    }

    /// RTI
    fn inst_rti(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_p = self.stack_pop()?;
        self.set_flag(Flag::B, false);
        self.set_flag(Flag::B2, true);

        self.cpu.pc = self.stack_pop_le_u16()?;
        Meta::normal().into()
    }

    /// RTS
    fn inst_rts(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.pc = self.stack_pop_le_u16()? + 1;
        Meta::normal().into()
    }

    /// SBC
    fn inst_sbc(&mut self, mode: AddressingMode) -> InstResult {
        // TODO: deduplicate code
        let param_addr = self.fetch_param_addr(mode)?;
        let param_val = self.ram_read(param_addr)?;
        let (mut res, mut overflow) = self.cpu.reg_a.overflowing_sub(param_val);
        if !self.get_flag(Flag::Carry) {
            let (res_carry, overflow_carry) = res.overflowing_sub(1);
            res = res_carry;
            overflow |= overflow_carry;
        }
        self.set_flag(
            Flag::Overflow,
            extract_overflow_bit(param_val, self.cpu.reg_a, res),
        );
        self.cpu.reg_a = res;
        self.set_flag(Flag::Carry, !overflow);
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// SEC
    fn inst_sec(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Carry, true);
        Meta::normal().into()
    }

    /// SED
    fn inst_sed(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Decimal, true);
        Meta::normal().into()
    }

    /// SEI
    fn inst_sei(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::IntDis, true);
        Meta::normal().into()
    }

    /// STA
    fn inst_sta(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.ram_write(param_addr, self.cpu.reg_a)?;
        Meta::normal().into()
    }

    /// STX
    fn inst_stx(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.ram_write(param_addr, self.cpu.reg_x)?;
        Meta::normal().into()
    }

    /// STY
    fn inst_sty(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.ram_write(param_addr, self.cpu.reg_y)?;
        Meta::normal().into()
    }

    /// TAX
    fn inst_tax(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_x = self.cpu.reg_a;
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// TAY
    fn inst_tay(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_y = self.cpu.reg_a;
        self.update_zero_negative_flags(self.cpu.reg_y);
        Meta::normal().into()
    }

    /// TSX
    fn inst_tsx(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_x = self.cpu.sp;
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// TXA
    fn inst_txa(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_a = self.cpu.reg_x;
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// TXS
    fn inst_txs(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.sp = self.cpu.reg_x;
        Meta::normal().into()
    }

    /// TYA
    fn inst_tya(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_a = self.cpu.reg_y;
        self.update_zero_negative_flags(self.cpu.reg_a);
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
