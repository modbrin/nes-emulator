//! This module contains constants, including system details and common numeric/hardware values

/// available ram of cpu, 2kb
pub const RAM_SIZE: usize = 2 * 1024;
/// available ram of ppu, 2kb
pub const VRAM_SIZE: usize = 2 * 1024;
/// cpu frequency, 1.79 MHz
pub const CPU_FREQ: usize = 1_790_000;
/// ppu frequency, 5.37 MHz
pub const PPU_FREQ: usize = 5_370_000;
/// memory indices marking start and end of stack section in memory
pub const STACK_SECTION_START: u16 = 0x0100;
pub const STACK_SECTION_END: u16 = 0x01FF;
/// memory indices marking start and end of rom section in memory
pub const ROM_SECTION_START: u16 = 0x8000;
pub const ROM_SECTION_END: u16 = 0xFFFF;
/// memory location where startup value for program counter is stored
pub const PC_RESET_ADDR: u16 = 0xFFFC;

pub const BIT0: u8 = 1 << 0;
pub const BIT1: u8 = 1 << 1;
pub const BIT2: u8 = 1 << 2;
pub const BIT3: u8 = 1 << 3;
pub const BIT4: u8 = 1 << 4;
pub const BIT5: u8 = 1 << 5;
pub const BIT6: u8 = 1 << 6;
pub const BIT7: u8 = 1 << 7;

pub const CPU_MMAP_RNG_START: u16 = 0x0000;
pub const CPU_MMAP_RNG_END: u16 = 0x1FFF;
pub const PPU_MMAP_RNG_START: u16 = 0x2000;
pub const PPU_MMAP_RNG_END: u16 = 0x3FFF;
pub const CPU_MIRROR_MASK: u16 = 0x07FF;
pub const PPU_MIRROR_MASK: u16 = 0x0207;

pub const NES_TAG: &[u8] = &[0x4E, 0x45, 0x53, 0x1A];
pub const PRG_BANK_SIZE: usize = 16 * 1024; // 16 kB
pub const CHR_BANK_SIZE: usize = 8 * 1024; // 8 kB
pub const ROM_TRAINER_SIZE: usize = 512; // 512 bytes

pub const SIZE_16KB: usize = 0x4000;

#[rustfmt::skip]
#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Flag {
    /// Carry Status Flag
    Carry    = BIT0,
    /// Zero Status Flag
    Zero     = BIT1,
    /// Interrupt Disable Status Flag
    IntDis   = BIT2,
    /// Decimal Mode Status Flag
    Decimal  = BIT3,
    /// Break Command Status Flag,
    /// used in interrupt handling
    B        = BIT4,
    /// Always set to 1, not modified
    B2       = BIT5,
    /// Overflow Status Flag
    Overflow = BIT6,
    /// Negative Status Flag
    Negative = BIT7,
}

#[rustfmt::skip]
#[derive(Clone, Copy, Debug)]
pub enum Opcode {
    // official opcodes
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC,
    CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP,
    JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, RTI, 
    RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,
    // unofficial opcodes
    ANC, SAX, ARR,  ASR, LXA, SHA, SBX, DCP,  NOP2, ISB, JAM, LAE, LAX, NOP3,
    RLA, RRA, SBC2, SLO, SRE, SHX, SHY, NOP4, ANE,  SHS,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AddressingMode {
    /// label
    Relative,
    /// (a)
    Indirect,
    /// A
    Accumulator,
    /// -- no params --
    Implied,
    /// #i
    Immediate,
    /// a
    Absolute,
    /// a,x
    AbsoluteX,
    /// a,y
    AbsoluteY,
    /// d
    Zeropage,
    /// d,x
    ZeropageX,
    /// d,y
    ZeropageY,
    /// (d,x)
    IndexedIndirect,
    /// (d),y
    IndirectIndexed,
}
