//! This module contains layout and logic of CPU and instruction handlers

use crate::prelude::*;
use std::ops::{Not, Shl, Shr};

type InstPtr = fn(&mut Device, AddressingMode) -> Result<InstructionMetadata, NesError>;

#[rustfmt::skip]
pub static INST_TABLE: &[(u8, Opcode, InstPtr, AddressingMode, u8)] = {
    use Opcode::*;
    use AddressingMode::*;
    type D = Device;
    &[
         /*0x00*/                                 /*0x01*/                                       /*0x02*/                                   /*0x03*/
/*0x00*/ (0x00, BRK, D::inst_brk, Implied,   7),  (0x01, ORA, D::inst_ora, IndexedIndirect, 6),  (0x02, JAM, D::inst_jam, Implied,     0),  (0x03, SLO, D::inst_slo, IndexedIndirect, 8),
/*0x04*/ (0x04, NOP2,D::inst_dop, Zeropage,  3),  (0x05, ORA, D::inst_ora, Zeropage,        3),  (0x06, ASL, D::inst_asl, Zeropage,    5),  (0x07, SLO, D::inst_slo, Zeropage,        5),
/*0x08*/ (0x08, PHP, D::inst_php, Implied,   3),  (0x09, ORA, D::inst_ora, Immediate,       2),  (0x0A, ASL, D::inst_asl, Accumulator, 2),  (0x0B, ANC, D::inst_anc, Immediate,       2),
/*0x0C*/ (0x0C, NOP4,D::inst_top, Absolute,  4),  (0x0D, ORA, D::inst_ora, Absolute,        4),  (0x0E, ASL, D::inst_asl, Absolute,    6),  (0x0F, SLO, D::inst_slo, Absolute,        6),
/*0x10*/ (0x10, BPL, D::inst_bpl, Relative,  2),  (0x11, ORA, D::inst_ora, IndirectIndexed, 5),  (0x12, JAM, D::inst_jam, Implied,     0),  (0x13, SLO, D::inst_slo, IndirectIndexed, 8),
/*0x14*/ (0x14, NOP2,D::inst_dop, ZeropageX, 4),  (0x15, ORA, D::inst_ora, ZeropageX,       4),  (0x16, ASL, D::inst_asl, ZeropageX,   6),  (0x17, SLO, D::inst_slo, ZeropageX,       6),
/*0x18*/ (0x18, CLC, D::inst_clc, Implied,   2),  (0x19, ORA, D::inst_ora, AbsoluteY,       4),  (0x1A, NOP3,D::inst_nop3,Implied,     2),  (0x1B, SLO, D::inst_slo, AbsoluteY,       7),
/*0x1C*/ (0x1C, NOP4,D::inst_top, AbsoluteX, 4),  (0x1D, ORA, D::inst_ora, AbsoluteX,       4),  (0x1E, ASL, D::inst_asl, AbsoluteX,   7),  (0x1F, SLO, D::inst_slo, AbsoluteX,       7),
/*0x20*/ (0x20, JSR, D::inst_jsr, Absolute,  6),  (0x21, AND, D::inst_and, IndexedIndirect, 6),  (0x22, JAM, D::inst_jam, Implied,     0),  (0x23, RLA, D::inst_rla, IndexedIndirect, 8),
/*0x24*/ (0x24, BIT, D::inst_bit, Zeropage,  3),  (0x25, AND, D::inst_and, Zeropage,        3),  (0x26, ROL, D::inst_rol, Zeropage,    5),  (0x27, RLA, D::inst_rla, Zeropage,        5),
/*0x28*/ (0x28, PLP, D::inst_plp, Implied,   4),  (0x29, AND, D::inst_and, Immediate,       2),  (0x2A, ROL, D::inst_rol, Accumulator, 2),  (0x2B, ANC, D::inst_anc, Immediate,       2),
/*0x2C*/ (0x2C, BIT, D::inst_bit, Absolute,  4),  (0x2D, AND, D::inst_and, Absolute,        4),  (0x2E, ROL, D::inst_rol, Absolute,    6),  (0x2F, RLA, D::inst_rla, Absolute,        6),
/*0x30*/ (0x30, BMI, D::inst_bmi, Relative,  2),  (0x31, AND, D::inst_and, IndirectIndexed, 5),  (0x32, JAM, D::inst_jam, Implied,     0),  (0x33, RLA, D::inst_rla, IndirectIndexed, 8),
/*0x34*/ (0x34, NOP2,D::inst_dop, ZeropageX, 4),  (0x35, AND, D::inst_and, ZeropageX,       4),  (0x36, ROL, D::inst_rol, ZeropageX,   6),  (0x37, RLA, D::inst_rla, ZeropageX,       6),
/*0x38*/ (0x38, SEC, D::inst_sec, Implied,   2),  (0x39, AND, D::inst_and, AbsoluteY,       4),  (0x3A, NOP3,D::inst_nop3,Implied,     2),  (0x3B, RLA, D::inst_rla, AbsoluteY,       7),
/*0x3C*/ (0x3C, NOP4,D::inst_top, AbsoluteX, 4),  (0x3D, AND, D::inst_and, AbsoluteX,       4),  (0x3E, ROL, D::inst_rol, AbsoluteX,   7),  (0x3F, RLA, D::inst_rla, AbsoluteX,       7),
/*0x40*/ (0x40, RTI, D::inst_rti, Implied,   6),  (0x41, EOR, D::inst_eor, IndexedIndirect, 6),  (0x42, JAM, D::inst_jam, Implied,     0),  (0x43, SRE, D::inst_sre, IndexedIndirect, 8),
/*0x44*/ (0x44, NOP2,D::inst_dop, Zeropage,  3),  (0x45, EOR, D::inst_eor, Zeropage,        3),  (0x46, LSR, D::inst_lsr, Zeropage,    5),  (0x47, SRE, D::inst_sre, Zeropage,        5),
/*0x48*/ (0x48, PHA, D::inst_pha, Implied,   3),  (0x49, EOR, D::inst_eor, Immediate,       2),  (0x4A, LSR, D::inst_lsr, Accumulator, 2),  (0x4B, ASR, D::inst_asr, Immediate,       2),
/*0x4C*/ (0x4C, JMP, D::inst_jmp, Absolute,  3),  (0x4D, EOR, D::inst_eor, Absolute,        4),  (0x4E, LSR, D::inst_lsr, Absolute,    6),  (0x4F, SRE, D::inst_sre, Absolute,        6),
/*0x50*/ (0x50, BVC, D::inst_bvc, Relative,  2),  (0x51, EOR, D::inst_eor, IndirectIndexed, 5),  (0x52, JAM, D::inst_jam, Implied,     0),  (0x53, SRE, D::inst_sre, IndirectIndexed, 8),
/*0x54*/ (0x54, NOP2,D::inst_dop, ZeropageX, 4),  (0x55, EOR, D::inst_eor, ZeropageX,       4),  (0x56, LSR, D::inst_lsr, ZeropageX,   6),  (0x57, SRE, D::inst_sre, ZeropageX,       6),
/*0x58*/ (0x58, CLI, D::inst_cli, Implied,   2),  (0x59, EOR, D::inst_eor, AbsoluteY,       4),  (0x5A, NOP3,D::inst_nop3,Implied,     2),  (0x5B, SRE, D::inst_sre, AbsoluteY,       7),
/*0x5C*/ (0x5C, NOP4,D::inst_top, AbsoluteX, 4),  (0x5D, EOR, D::inst_eor, AbsoluteX,       4),  (0x5E, LSR, D::inst_lsr, AbsoluteX,   7),  (0x5F, SRE, D::inst_sre, AbsoluteX,       7),
/*0x60*/ (0x60, RTS, D::inst_rts, Implied,   6),  (0x61, ADC, D::inst_adc, IndexedIndirect, 6),  (0x62, JAM, D::inst_jam, Implied,     0),  (0x63, RRA, D::inst_rra, IndexedIndirect, 8),
/*0x64*/ (0x64, NOP2,D::inst_dop, Zeropage,  3),  (0x65, ADC, D::inst_adc, Zeropage,        3),  (0x66, ROR, D::inst_ror, Zeropage,    5),  (0x67, RRA, D::inst_rra, Zeropage,        5),
/*0x68*/ (0x68, PLA, D::inst_pla, Implied,   4),  (0x69, ADC, D::inst_adc, Immediate,       2),  (0x6A, ROR, D::inst_ror, Accumulator, 2),  (0x6B, ARR, D::inst_arr, Immediate,       2),
/*0x6C*/ (0x6C, JMP, D::inst_jmp, Indirect,  5),  (0x6D, ADC, D::inst_adc, Absolute,        4),  (0x6E, ROR, D::inst_ror, Absolute,    6),  (0x6F, RRA, D::inst_rra, Absolute,        6),
/*0x70*/ (0x70, BVS, D::inst_bvs, Relative,  2),  (0x71, ADC, D::inst_adc, IndirectIndexed, 5),  (0x72, JAM, D::inst_jam, Implied,     0),  (0x73, RRA, D::inst_rra, IndirectIndexed, 8),
/*0x74*/ (0x74, NOP2,D::inst_dop, ZeropageX, 4),  (0x75, ADC, D::inst_adc, ZeropageX,       4),  (0x76, ROR, D::inst_ror, ZeropageX,   6),  (0x77, RRA, D::inst_rra, ZeropageX,       6),
/*0x78*/ (0x78, SEI, D::inst_sei, Implied,   2),  (0x79, ADC, D::inst_adc, AbsoluteY,       4),  (0x7A, NOP3,D::inst_nop3,Implied,     2),  (0x7B, RRA, D::inst_rra, AbsoluteY,       7),
/*0x7C*/ (0x7C, NOP4,D::inst_top, AbsoluteX, 4),  (0x7D, ADC, D::inst_adc, AbsoluteX,       4),  (0x7E, ROR, D::inst_ror, AbsoluteX,   7),  (0x7F, RRA, D::inst_rra, AbsoluteX,       7),
/*0x80*/ (0x80, NOP2,D::inst_dop, Immediate, 2),  (0x81, STA, D::inst_sta, IndexedIndirect, 6),  (0x82, NOP2,D::inst_dop, Immediate,   2),  (0x83, SAX, D::inst_sax, IndexedIndirect, 6),
/*0x84*/ (0x84, STY, D::inst_sty, Zeropage,  3),  (0x85, STA, D::inst_sta, Zeropage,        3),  (0x86, STX, D::inst_stx, Zeropage,    3),  (0x87, SAX, D::inst_sax, Zeropage,        3),
/*0x88*/ (0x88, DEY, D::inst_dey, Implied,   2),  (0x89, NOP2,D::inst_dop, Immediate,       2),  (0x8A, TXA, D::inst_txa, Implied,     2),  (0x8B, ANE, D::inst_ane, Immediate,       2),
/*0x8C*/ (0x8C, STY, D::inst_sty, Absolute,  4),  (0x8D, STA, D::inst_sta, Absolute,        4),  (0x8E, STX, D::inst_stx, Absolute,    4),  (0x8F, SAX, D::inst_sax, Absolute,        4),
/*0x90*/ (0x90, BCC, D::inst_bcc, Relative,  2),  (0x91, STA, D::inst_sta, IndirectIndexed, 6),  (0x92, JAM, D::inst_jam, Implied,     0),  (0x93, SHA, D::inst_sha, IndirectIndexed, 6),
/*0x94*/ (0x94, STY, D::inst_sty, ZeropageX, 4),  (0x95, STA, D::inst_sta, ZeropageX,       4),  (0x96, STX, D::inst_stx, ZeropageY,   4),  (0x97, SAX, D::inst_sax, ZeropageY,       4),
/*0x98*/ (0x98, TYA, D::inst_tya, Implied,   2),  (0x99, STA, D::inst_sta, AbsoluteY,       5),  (0x9A, TXS, D::inst_txs, Implied,     2),  (0x9B, SHS, D::inst_shs, AbsoluteY,       5),
/*0x9C*/ (0x9C, SHY, D::inst_shy, AbsoluteX, 5),  (0x9D, STA, D::inst_sta, AbsoluteX,       5),  (0x9E, SHX, D::inst_shx, AbsoluteY,   5),  (0x9F, SHA, D::inst_sha, AbsoluteY,       5),
/*0xA0*/ (0xA0, LDY, D::inst_ldy, Immediate, 2),  (0xA1, LDA, D::inst_lda, IndexedIndirect, 6),  (0xA2, LDX, D::inst_ldx, Immediate,   2),  (0xA3, LAX, D::inst_lax, IndexedIndirect, 6),
/*0xA4*/ (0xA4, LDY, D::inst_ldy, Zeropage,  3),  (0xA5, LDA, D::inst_lda, Zeropage,        3),  (0xA6, LDX, D::inst_ldx, Zeropage,    3),  (0xA7, LAX, D::inst_lax, Zeropage,        3),
/*0xA8*/ (0xA8, TAY, D::inst_tay, Implied,   2),  (0xA9, LDA, D::inst_lda, Immediate,       2),  (0xAA, TAX, D::inst_tax, Implied,     2),  (0xAB, LXA, D::inst_lxa, Immediate,       2),
/*0xAC*/ (0xAC, LDY, D::inst_ldy, Absolute,  4),  (0xAD, LDA, D::inst_lda, Absolute,        4),  (0xAE, LDX, D::inst_ldx, Absolute,    4),  (0xAF, LAX, D::inst_lax, Absolute,        4),
/*0xB0*/ (0xB0, BCS, D::inst_bcs, Relative,  2),  (0xB1, LDA, D::inst_lda, IndirectIndexed, 5),  (0xB2, JAM, D::inst_jam, Implied,     0),  (0xB3, LAX, D::inst_lax, IndirectIndexed, 5),
/*0xB4*/ (0xB4, LDY, D::inst_ldy, ZeropageX, 4),  (0xB5, LDA, D::inst_lda, ZeropageX,       4),  (0xB6, LDX, D::inst_ldx, ZeropageY,   4),  (0xB7, LAX, D::inst_lax, ZeropageY,       4),
/*0xB8*/ (0xB8, CLV, D::inst_clv, Implied,   2),  (0xB9, LDA, D::inst_lda, AbsoluteY,       4),  (0xBA, TSX, D::inst_tsx, Implied,     2),  (0xBB, LAE, D::inst_lae, AbsoluteY,       4),
/*0xBC*/ (0xBC, LDY, D::inst_ldy, AbsoluteX, 4),  (0xBD, LDA, D::inst_lda, AbsoluteX,       4),  (0xBE, LDX, D::inst_ldx, AbsoluteY,   4),  (0xBF, LAX, D::inst_lax, AbsoluteY,       4),
/*0xC0*/ (0xC0, CPY, D::inst_cpy, Immediate, 2),  (0xC1, CMP, D::inst_cmp, IndexedIndirect, 6),  (0xC2, NOP2,D::inst_dop, Immediate,   2),  (0xC3, DCP, D::inst_dcp, IndexedIndirect, 8),
/*0xC4*/ (0xC4, CPY, D::inst_cpy, Zeropage,  3),  (0xC5, CMP, D::inst_cmp, Zeropage,        3),  (0xC6, DEC, D::inst_dec, Zeropage,    5),  (0xC7, DCP, D::inst_dcp, Zeropage,        5),
/*0xC8*/ (0xC8, INY, D::inst_iny, Implied,   2),  (0xC9, CMP, D::inst_cmp, Immediate,       2),  (0xCA, DEX, D::inst_dex, Implied,     2),  (0xCB, SBX, D::inst_sbx, Immediate,       2),
/*0xCC*/ (0xCC, CPY, D::inst_cpy, Absolute,  4),  (0xCD, CMP, D::inst_cmp, Absolute,        4),  (0xCE, DEC, D::inst_dec, Absolute,    6),  (0xCF, DCP, D::inst_dcp, Absolute,        6),
/*0xD0*/ (0xD0, BNE, D::inst_bne, Relative,  2),  (0xD1, CMP, D::inst_cmp, IndirectIndexed, 5),  (0xD2, JAM, D::inst_jam, Implied,     0),  (0xD3, DCP, D::inst_dcp, IndirectIndexed, 8),
/*0xD4*/ (0xD4, NOP2,D::inst_dop, ZeropageX, 4),  (0xD5, CMP, D::inst_cmp, ZeropageX,       4),  (0xD6, DEC, D::inst_dec, ZeropageX,   6),  (0xD7, DCP, D::inst_dcp, ZeropageX,       6),
/*0xD8*/ (0xD8, CLD, D::inst_cld, Implied,   2),  (0xD9, CMP, D::inst_cmp, AbsoluteY,       4),  (0xDA, NOP3,D::inst_nop3,Implied,     2),  (0xDB, DCP, D::inst_dcp, AbsoluteY,       7),
/*0xDC*/ (0xDC, NOP4,D::inst_top, AbsoluteX, 4),  (0xDD, CMP, D::inst_cmp, AbsoluteX,       4),  (0xDE, DEC, D::inst_dec, AbsoluteX,   7),  (0xDF, DCP, D::inst_dcp, AbsoluteX,       7),
/*0xE0*/ (0xE0, CPX, D::inst_cpx, Immediate, 2),  (0xE1, SBC, D::inst_sbc, IndexedIndirect, 6),  (0xE2, NOP2,D::inst_dop, Immediate,   2),  (0xE3, ISB, D::inst_isb, IndexedIndirect, 8),
/*0xE4*/ (0xE4, CPX, D::inst_cpx, Zeropage,  3),  (0xE5, SBC, D::inst_sbc, Zeropage,        3),  (0xE6, INC, D::inst_inc, Zeropage,    5),  (0xE7, ISB, D::inst_isb, Zeropage,        5),
/*0xE8*/ (0xE8, INX, D::inst_inx, Implied,   2),  (0xE9, SBC, D::inst_sbc, Immediate,       2),  (0xEA, NOP, D::inst_nop, Implied,     2),  (0xEB, SBC2,D::inst_sbc2,Immediate,       2),
/*0xEC*/ (0xEC, CPX, D::inst_cpx, Absolute,  4),  (0xED, SBC, D::inst_sbc, Absolute,        4),  (0xEE, INC, D::inst_inc, Absolute,    6),  (0xEF, ISB, D::inst_isb, Absolute,        6),
/*0xF0*/ (0xF0, BEQ, D::inst_beq, Relative,  2),  (0xF1, SBC, D::inst_sbc, IndirectIndexed, 5),  (0xF2, JAM, D::inst_jam, Implied,     0),  (0xF3, ISB, D::inst_isb, IndirectIndexed, 8),
/*0xF4*/ (0xF4, NOP2,D::inst_dop, ZeropageX, 4),  (0xF5, SBC, D::inst_sbc, ZeropageX,       4),  (0xF6, INC, D::inst_inc, ZeropageX,   6),  (0xF7, ISB, D::inst_isb, ZeropageX,       6),
/*0xF8*/ (0xF8, SED, D::inst_sed, Implied,   2),  (0xF9, SBC, D::inst_sbc, AbsoluteY,       4),  (0xFA, NOP3,D::inst_nop3,Implied,     2),  (0xFB, ISB, D::inst_isb, AbsoluteY,       7),
/*0xFC*/ (0xFC, NOP4,D::inst_top, AbsoluteX, 4),  (0xFD, SBC, D::inst_sbc, AbsoluteX,       4),  (0xFE, INC, D::inst_inc, AbsoluteX,   7),  (0xFF, ISB, D::inst_isb, AbsoluteX,       7),
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
        let (repr, op, func, mode, cycles) = INST_TABLE[opcode as usize];
        let res = func(self, mode);
        match res {
            Ok(mut meta) => {
                meta.cycles += cycles;
                Ok(meta)
            }
            Err(err) => {
                println!("Got error: {:?}, on instruction: {:#02x}", err, repr);
                Err(err)
            }
        }
    }

    fn result_extra_cycles(&self, cycles: u8) -> InstResult {
        Ok(InstructionMetadata {
            cycles,
            update_pc: self.cpu.pc,
            is_break: false,
        })
    }

    fn result_simple(&self) -> InstResult {
        Ok(InstructionMetadata {
            cycles: 0,
            update_pc: self.cpu.pc,
            is_break: false,
        })
    }

    fn result_pc(&self, update_pc: u16) -> InstResult {
        Ok(InstructionMetadata {
            cycles: 0,
            update_pc,
            is_break: false,
        })
    }

    fn result_skip(&self, skip_n: u16) -> InstResult {
        let update_pc = self
            .cpu
            .pc
            .checked_add(skip_n)
            .ok_or(NesError::PcOverflow)?;
        Ok(InstructionMetadata {
            cycles: 0,
            update_pc,
            is_break: false,
        })
    }

    fn result_stop(&self) -> InstResult {
        Ok(InstructionMetadata {
            cycles: 0,
            update_pc: self.cpu.pc,
            is_break: true,
        })
    }

    fn branch_if(&mut self, cond: bool) -> InstResult {
        if cond {
            let offset = self.read_one(self.cpu.pc)?;
            let curr_pc = self.cpu.pc.wrapping_add(1);
            let jump_to = if offset & 0x80 != 0 {
                // subtract two's complement
                curr_pc.wrapping_sub(offset.not().wrapping_add(1) as u16)
            } else {
                curr_pc.wrapping_add(offset as u16)
            };
            self.result_pc(jump_to)
        } else {
            // consume unused argument
            self.result_skip(1)
        }
    }

    fn cmp_with_mem(&mut self, mode: AddressingMode, target: u8) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param = self.read_one(param_addr.addr)?;
        self.set_flag(Flag::Carry, target >= param);
        self.set_flag(Flag::Zero, target == param);
        self.set_flag(Flag::Negative, target.wrapping_sub(param) & BIT7 != 0);
        self.result_pc(param_addr.pc_upd)
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

/// instruction placeholder
fn undef(device: &mut Device, _mode: AddressingMode) -> InstResult {
    Err(NesError::Unimplemented)
}

impl Device {
    /// BRK
    fn inst_brk(&mut self, _mode: AddressingMode) -> InstResult {
        // TODO: not properly implemented?
        self.result_stop()
    }

    fn add_with_carry(&mut self, val: u8) -> Result<(), NesError> {
        let (mut res, mut overflow) = self.cpu.reg_a.overflowing_add(val);
        if self.get_flag(Flag::Carry) {
            let (res_carry, overflow_carry) = res.overflowing_add(1);
            res = res_carry;
            overflow |= overflow_carry;
        }
        self.set_flag(
            Flag::Overflow,
            extract_overflow_bit(val, self.cpu.reg_a, res),
        );
        self.cpu.reg_a = res;
        self.set_flag(Flag::Carry, overflow);
        self.update_zero_negative_flags(self.cpu.reg_a);
        Ok(())
    }

    /// ADC - Add with Carry
    fn inst_adc(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param_val = self.read_one(param_addr.addr)?;
        self.add_with_carry(param_val)?;
        self.result_pc(param_addr.pc_upd)
    }

    /// ASL
    fn inst_and(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param = self.read_one(param_addr.addr)?;
        self.cpu.reg_a &= param;
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_pc(param_addr.pc_upd)
    }

    fn rotate_at_location(
        &mut self,
        mode: AddressingMode,
        op: fn(u8, i32) -> u8,
        set_mask: u8,
        get_mask: u8,
    ) -> InstResult {
        let carry_mask: u8 = self.get_flag(Flag::Carry).then(|| set_mask).unwrap_or(0);
        if mode == AddressingMode::Accumulator {
            self.set_flag(Flag::Carry, self.cpu.reg_a & get_mask != 0);
            self.cpu.reg_a = op(self.cpu.reg_a, 1) | carry_mask;
            self.update_zero_negative_flags(self.cpu.reg_a);
            self.result_simple()
        } else {
            let val_addr = self.fetch_param_addr(mode)?;
            let val = self.read_one(val_addr.addr)?;
            self.set_flag(Flag::Carry, val & get_mask != 0);
            let shifted_val = op(val, 1) | carry_mask;
            self.write_one(val_addr.addr, shifted_val)?;
            self.update_zero_negative_flags(shifted_val);
            self.result_pc(val_addr.pc_upd)
        }
    }

    /// ASL
    fn inst_asl(&mut self, mode: AddressingMode) -> InstResult {
        self.rotate_at_location(mode, u8::shl, 0, BIT7)
    }

    /// BCC
    fn inst_bcc(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(!self.get_flag(Flag::Carry))
    }

    /// BCS
    fn inst_bcs(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(self.get_flag(Flag::Carry))
    }

    /// BEQ
    fn inst_beq(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(self.get_flag(Flag::Zero))
    }

    /// BIT
    fn inst_bit(&mut self, mode: AddressingMode) -> InstResult {
        let loc_addr = self.fetch_param_addr(mode)?;
        let loc = self.read_one(loc_addr.addr)?;
        self.set_flag(Flag::Overflow, loc & BIT6 != 0);
        self.set_flag(Flag::Negative, loc & BIT7 != 0);
        self.set_flag(Flag::Zero, self.cpu.reg_a & loc == 0);
        self.result_pc(loc_addr.pc_upd)
    }

    /// BMI
    fn inst_bmi(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(self.get_flag(Flag::Negative))
    }

    /// BNE
    fn inst_bne(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(!self.get_flag(Flag::Zero))
    }

    /// BPL
    fn inst_bpl(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(!self.get_flag(Flag::Negative))
    }

    /// BVC
    fn inst_bvc(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(!self.get_flag(Flag::Overflow))
    }

    /// BVS
    fn inst_bvs(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Relative)?;
        self.branch_if(self.get_flag(Flag::Overflow))
    }

    /// CLC
    fn inst_clc(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Carry, false);
        self.result_simple()
    }

    /// CLD
    fn inst_cld(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Decimal, false);
        self.result_simple()
    }

    /// CLI
    fn inst_cli(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::IntDis, false);
        self.result_simple()
    }

    /// CLV
    fn inst_clv(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Overflow, false);
        self.result_simple()
    }

    /// CMP
    fn inst_cmp(&mut self, mode: AddressingMode) -> InstResult {
        self.cmp_with_mem(mode, self.cpu.reg_a)
    }

    /// CPX
    fn inst_cpx(&mut self, mode: AddressingMode) -> InstResult {
        self.cmp_with_mem(mode, self.cpu.reg_x)
    }

    /// CPY
    fn inst_cpy(&mut self, mode: AddressingMode) -> InstResult {
        self.cmp_with_mem(mode, self.cpu.reg_y)
    }

    /// DEC
    fn inst_dec(&mut self, mode: AddressingMode) -> InstResult {
        let loc_addr = self.fetch_param_addr(mode)?;
        let mut loc = self.read_one(loc_addr.addr)?;
        loc = loc.wrapping_sub(1);
        self.update_zero_negative_flags(loc);
        self.write_one(loc_addr.addr, loc)?;
        self.result_pc(loc_addr.pc_upd)
    }

    /// DEX
    fn inst_dex(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_x = self.cpu.reg_x.wrapping_sub(1);
        self.update_zero_negative_flags(self.cpu.reg_x);
        self.result_simple()
    }

    /// DEY
    fn inst_dey(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_y = self.cpu.reg_y.wrapping_sub(1);
        self.update_zero_negative_flags(self.cpu.reg_y);
        self.result_simple()
    }

    /// EOR
    fn inst_eor(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param = self.read_one(param_addr.addr)?;
        self.cpu.reg_a = self.cpu.reg_a ^ param;
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_pc(param_addr.pc_upd)
    }

    /// INC
    fn inst_inc(&mut self, mode: AddressingMode) -> InstResult {
        let loc_addr = self.fetch_param_addr(mode)?;
        let mut loc = self.read_one(loc_addr.addr)?;
        loc = loc.wrapping_add(1);
        self.update_zero_negative_flags(loc);
        self.write_one(loc_addr.addr, loc)?;
        self.result_pc(loc_addr.pc_upd)
    }

    /// INX
    fn inst_inx(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_x = self.cpu.reg_x.wrapping_add(1);
        self.update_zero_negative_flags(self.cpu.reg_x);
        self.result_simple()
    }

    /// INY
    fn inst_iny(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_y = self.cpu.reg_y.wrapping_add(1);
        self.update_zero_negative_flags(self.cpu.reg_y);
        self.result_simple()
    }

    /// JMP
    fn inst_jmp(&mut self, mode: AddressingMode) -> InstResult {
        let jump_to = if mode == AddressingMode::Indirect {
            let addr = self.read_le_u16(self.cpu.pc)?;
            // 6502 bug imitation
            if addr & 0x00FF == 0x00FF {
                let lo = self.read_one(addr)? as u16;
                let hi = self.read_one(addr & 0xFF00)? as u16;
                (hi << 8) | lo
            } else {
                self.read_le_u16(addr)?
            }
        } else {
            self.fetch_param_addr(mode)?.addr
        };
        self.result_pc(jump_to)
    }

    /// JSR
    fn inst_jsr(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Absolute)?;
        let save_pc = self.cpu.pc.wrapping_add(2 - 1);
        self.stack_push_le_u16(save_pc)?;
        self.result_pc(self.read_le_u16(self.cpu.pc)?)
    }

    /// LDA
    fn inst_lda(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.cpu.reg_a = self.read_one(param_addr.addr)?;
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_pc(param_addr.pc_upd)
    }

    /// LDX
    fn inst_ldx(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.cpu.reg_x = self.read_one(param_addr.addr)?;
        self.update_zero_negative_flags(self.cpu.reg_x);
        self.result_pc(param_addr.pc_upd)
    }

    /// LDY
    fn inst_ldy(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.cpu.reg_y = self.read_one(param_addr.addr)?;
        self.update_zero_negative_flags(self.cpu.reg_y);
        self.result_pc(param_addr.pc_upd)
    }

    /// LSR
    fn inst_lsr(&mut self, mode: AddressingMode) -> InstResult {
        self.rotate_at_location(mode, u8::shr, 0, BIT0)
    }

    /// NOP
    fn inst_nop(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.result_simple()
    }

    /// ORA
    fn inst_ora(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param = self.read_one(param_addr.addr)?;
        self.cpu.reg_a = self.cpu.reg_a | param;
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_pc(param_addr.pc_upd)
    }

    /// PHA
    fn inst_pha(&mut self, _mode: AddressingMode) -> InstResult {
        self.stack_push(self.cpu.reg_a)?;
        self.result_simple()
    }

    /// PHP
    fn inst_php(&mut self, _mode: AddressingMode) -> InstResult {
        let backup = self.cpu.reg_p;
        self.set_flag(Flag::B, true);
        self.set_flag(Flag::B2, true);
        self.stack_push(self.cpu.reg_p)?;
        self.cpu.reg_p = backup;
        self.result_simple()
    }

    /// PLA
    fn inst_pla(&mut self, _mode: AddressingMode) -> InstResult {
        self.cpu.reg_a = self.stack_pop()?;
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_simple()
    }

    /// PLP
    fn inst_plp(&mut self, _mode: AddressingMode) -> InstResult {
        self.cpu.reg_p = self.stack_pop()?;
        self.set_flag(Flag::B, false);
        self.set_flag(Flag::B2, true);
        self.result_simple()
    }

    /// ROL
    fn inst_rol(&mut self, mode: AddressingMode) -> InstResult {
        self.rotate_at_location(mode, u8::shl, BIT0, BIT7)
    }

    /// ROR
    fn inst_ror(&mut self, mode: AddressingMode) -> InstResult {
        self.rotate_at_location(mode, u8::shr, BIT7, BIT0)
    }

    /// RTI
    fn inst_rti(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_p = self.stack_pop()?;
        self.set_flag(Flag::B, false);
        self.set_flag(Flag::B2, true);
        let popped_pc = self.stack_pop_le_u16()?;
        self.result_pc(popped_pc)
    }

    /// RTS
    fn inst_rts(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        let popped_pc = self.stack_pop_le_u16()?;
        self.result_pc(popped_pc.wrapping_add(1)) // TODO: why add 1?
    }

    /// SBC
    fn inst_sbc(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let param_val = self.read_one(param_addr.addr)?;
        self.add_with_carry(param_val.not())?;
        self.result_pc(param_addr.pc_upd)
    }

    /// SEC
    fn inst_sec(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Carry, true);
        self.result_simple()
    }

    /// SED
    fn inst_sed(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::Decimal, true);
        self.result_simple()
    }

    /// SEI
    fn inst_sei(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.set_flag(Flag::IntDis, true);
        self.result_simple()
    }

    /// STA
    fn inst_sta(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.write_one(param_addr.addr, self.cpu.reg_a)?;
        self.result_pc(param_addr.pc_upd)
    }

    /// STX
    fn inst_stx(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.write_one(param_addr.addr, self.cpu.reg_x)?;
        self.result_pc(param_addr.pc_upd)
    }

    /// STY
    fn inst_sty(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.write_one(param_addr.addr, self.cpu.reg_y)?;
        self.result_pc(param_addr.pc_upd)
    }

    /// TAX
    fn inst_tax(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_x = self.cpu.reg_a;
        self.update_zero_negative_flags(self.cpu.reg_x);
        self.result_simple()
    }

    /// TAY
    fn inst_tay(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_y = self.cpu.reg_a;
        self.update_zero_negative_flags(self.cpu.reg_y);
        self.result_simple()
    }

    /// TSX
    fn inst_tsx(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_x = self.cpu.sp;
        self.update_zero_negative_flags(self.cpu.reg_x);
        self.result_simple()
    }

    /// TXA
    fn inst_txa(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_a = self.cpu.reg_x;
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_simple()
    }

    /// TXS
    fn inst_txs(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.sp = self.cpu.reg_x;
        self.result_simple()
    }

    /// TYA
    fn inst_tya(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.cpu.reg_a = self.cpu.reg_y;
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_simple()
    }

    // unofficial opcodes:

    /// ANC
    fn inst_anc(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let val = self.read_one(param_addr.addr)?;
        self.cpu.reg_a &= val;
        self.set_flag(Flag::Carry, self.cpu.reg_a & BIT7 != 0);
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_pc(param_addr.pc_upd)
    }

    /// SAX
    fn inst_sax(&mut self, mode: AddressingMode) -> InstResult {
        let val = self.cpu.reg_x & self.cpu.reg_a;
        self.update_zero_negative_flags(val);
        let loc_addr = self.fetch_param_addr(mode)?;
        self.write_one(loc_addr.addr, val)?;
        self.result_pc(loc_addr.pc_upd)
    }

    /// ARR
    fn inst_arr(&mut self, mode: AddressingMode) -> InstResult {
        // let addr = self.fetch_param_addr(mode)?;
        // let val = self.read_one(addr)?;
        // self.cpu.reg_a &= val;
        // self.cpu.reg_a = self.cpu.reg_a.rotate_right(1); // TODO: likely wrong
        // let bit5 = self.cpu.reg_a & BIT5 != 0;
        // let bit6 = self.cpu.reg_a & BIT6 != 0;
        // let (flag_c, flag_v) = match (bit5, bit6) {
        //     (true, true) => (true, false),
        //     (false, false) => (false, false),
        //     (true, false) => (false, true),
        //     (false, true) => (true, true),
        // };
        // self.set_flag(Flag::Overflow, flag_v);
        // self.set_flag(Flag::Carry, flag_c);
        // self.update_zero_negative_flags(self.cpu.reg_a);
        let _ = self.inst_and(mode)?;
        self.inst_ror(AddressingMode::Accumulator)
    }

    /// ASR
    fn inst_asr(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let val = self.read_one(param_addr.addr)?;
        self.cpu.reg_a &= val;
        self.set_flag(Flag::Carry, self.cpu.reg_a & BIT0 != 0);
        self.cpu.reg_a >>= 1;
        self.update_zero_negative_flags(self.cpu.reg_a);
        self.result_pc(param_addr.pc_upd)
    }

    /// LXA
    fn inst_lxa(&mut self, mode: AddressingMode) -> InstResult {
        let loc_addr = self.fetch_param_addr(mode)?;
        let val = self.read_one(loc_addr.addr)?;
        self.cpu.reg_a &= val;
        self.cpu.reg_x = self.cpu.reg_a;
        self.update_zero_negative_flags(self.cpu.reg_x);
        self.result_pc(loc_addr.pc_upd)
    }

    /// SHA
    fn inst_sha(&mut self, mode: AddressingMode) -> InstResult {
        let loc_addr = self.fetch_param_addr(mode)?;
        let val = (self.cpu.reg_x & self.cpu.reg_a) & 0x7;
        self.write_one(loc_addr.addr, val)?;
        self.result_pc(loc_addr.pc_upd)
    }

    /// SBX
    fn inst_sbx(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let val = self.read_one(param_addr.addr)?;
        let res_and = self.cpu.reg_a & self.cpu.reg_x;
        let (res, ovf) = res_and.overflowing_sub(val);
        self.cpu.reg_x = res;
        self.set_flag(Flag::Carry, ovf);
        self.update_zero_negative_flags(self.cpu.reg_x);
        self.result_pc(param_addr.pc_upd)
    }

    /// DCP
    fn inst_dcp(&mut self, mode: AddressingMode) -> InstResult {
        let _ = self.inst_dec(mode)?;
        self.inst_cmp(mode)
    }

    /// DOP (NOP2)
    fn inst_dop(&mut self, mode: AddressingMode) -> InstResult {
        let res = self.fetch_param_addr(mode)?;
        self.result_pc(res.pc_upd)
    }

    /// ISB (ISC)
    fn inst_isb(&mut self, mode: AddressingMode) -> InstResult {
        let _ = self.inst_inc(mode)?;
        self.inst_sbc(mode)
    }

    /// JAM
    fn inst_jam(&mut self, _mode: AddressingMode) -> InstResult {
        self.result_stop()
    }

    /// LAE
    fn inst_lae(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let val = self.read_one(param_addr.addr)?;
        let res = self.cpu.sp & val;
        self.cpu.reg_a = res;
        self.cpu.reg_x = res;
        self.cpu.sp = res;
        self.update_zero_negative_flags(res);
        self.result_pc(param_addr.pc_upd)
    }

    /// LAX
    fn inst_lax(&mut self, mode: AddressingMode) -> InstResult {
        let _ = self.inst_lda(mode)?;
        self.inst_ldx(mode)
    }

    /// NOP3
    fn inst_nop3(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Implied)?;
        self.result_simple()
    }

    /// RLA
    fn inst_rla(&mut self, mode: AddressingMode) -> InstResult {
        let _ = self.inst_rol(mode)?;
        self.inst_and(mode)
    }

    /// RRA
    fn inst_rra(&mut self, mode: AddressingMode) -> InstResult {
        let _ = self.inst_ror(mode)?;
        self.inst_adc(mode)
    }

    /// SBC2
    fn inst_sbc2(&mut self, mode: AddressingMode) -> InstResult {
        expect_mode(mode, AddressingMode::Immediate)?;
        self.inst_sbc(mode)
    }

    /// SLO
    fn inst_slo(&mut self, mode: AddressingMode) -> InstResult {
        let _ = self.inst_asl(mode)?;
        self.inst_ora(mode)
    }

    /// SRE
    fn inst_sre(&mut self, mode: AddressingMode) -> InstResult {
        let _ = self.inst_lsr(mode)?;
        self.inst_eor(mode)
    }

    /// SHX
    fn inst_shx(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let val = self.cpu.reg_x & ((param_addr.addr >> 8) as u8).wrapping_add(1);
        self.write_one(param_addr.addr, val.wrapping_add(1))?;
        self.result_pc(param_addr.pc_upd)
    }

    /// SHY
    fn inst_shy(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        let val = self.cpu.reg_y & ((param_addr.addr >> 8) as u8).wrapping_add(1);
        self.write_one(param_addr.addr, val)?;
        self.result_pc(param_addr.pc_upd)
    }

    /// TOP (NOP4)
    fn inst_top(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.result_pc(param_addr.pc_upd)
    }

    /// ANE
    fn inst_ane(&mut self, mode: AddressingMode) -> InstResult {
        self.inst_tax(AddressingMode::Implied)?;
        self.inst_and(mode)
    }

    /// SHS
    fn inst_shs(&mut self, mode: AddressingMode) -> InstResult {
        let tmp = self.cpu.reg_x & self.cpu.reg_x;
        self.cpu.sp = tmp;
        let param_addr = self.fetch_param_addr(mode)?;
        let val = tmp & ((param_addr.addr >> 8) as u8).wrapping_add(1);
        self.write_one(param_addr.addr, val)?;
        self.result_pc(param_addr.pc_upd)
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
