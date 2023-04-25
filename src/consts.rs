//! This page contains constants, including system details and common numeric/hardware values

use std::ops::Range;

/// available ram of cpu, 2kb
pub const RAM_SIZE: usize = 2 * 1024;
/// available ram of ppu, 2kb
pub const VRAM_SIZE: usize = 2 * 1024;
/// cpu frequency, 1.79 MHz
pub const CPU_FREQ: usize = 1_790_000;
/// ppu frequency, 5.37 MHz
pub const PPU_FREQ: usize = 5_370_000;
/// size of zeropage region in memory
pub const ZEROPAGE_SIZE: usize = 0x100;
/// memory indices marking start and end of rom section in memory
pub const ROM_SECTION: Range<usize> = 0x8000..0xFFFF;
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

#[derive(Clone, Copy)]
pub enum Opcode {
    Lda,
    Tax,
}

#[derive(Clone, Copy)]
pub enum AddressingMode {
    // Implied,
    // Accumulator, A
    // Relative, label
    // Indirect, (a)
    /// #v
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
    /// (d,y)
    IndirectIndexed,
    Other,
}
