//! This module contains constants, including system details and common numeric/hardware values

use std::collections::HashMap;

use once_cell::sync::Lazy;
use sdl2::keyboard::Keycode;

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
/// 16 kb in bytes
pub const SIZE_16KB: usize = 0x4000;

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
pub const PPU_MIRROR_MASK: u16 = 0x2007;

pub const NES_TAG: &[u8] = &[0x4E, 0x45, 0x53, 0x1A];
pub const PRG_BANK_SIZE: usize = 16 * 1024; // 16 kB
pub const CHR_BANK_SIZE: usize = 8 * 1024; // 8 kB
pub const ROM_TRAINER_SIZE: usize = 512; // 512 bytes
pub const PALETTE_COLORS_NUM: usize = 32;
pub const NMI_HANDLER_ADDR: u16 = 0xFFFA;

pub const PPU_CHR_START: u16 = 0x0000;
pub const PPU_CHR_END: u16 = 0x1FFF;
pub const PPU_RAM_START: u16 = 0x2000;
pub const PPU_RAM_END: u16 = 0x2FFF;
pub const PPU_PALETTE_START: u16 = 0x3f00;
pub const PPU_PALETTE_END: u16 = 0x3fff;

pub const DISPLAY_RES_PAL: (usize, usize) = (256, 240);
pub const DISPLAY_RES_NTSC: (usize, usize) = (256, 224);
pub const DISPLAY_SIZE_PAL: usize = DISPLAY_RES_PAL.0 * DISPLAY_RES_PAL.1 * 3;
pub const DISPLAY_SIZE_NTSC: usize = DISPLAY_RES_NTSC.0 * DISPLAY_RES_NTSC.1 * 3;

pub const UNIVERSAL_BKG_COLOR_ADDR: usize = 0x3F00;

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
#[repr(u8)]
pub enum Opcode {
    // official opcodes
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC,
    CLD, CLI, CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP,
    JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL, ROR, RTI, 
    RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,
    // unofficial opcodes
    ANC,  SAX, ARR, ASR,  LXA, SHA, SBX, DCP, NOP2, ISB, JAM, LAE, LAX, 
    NOP3, RLA, RRA, SBC2, SLO, SRE, SHX, SHY, NOP4, ANE, SHS,
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

/// Screen mirroring
#[derive(Debug)]
pub enum ScrMirror {
    Vertical,
    Horizontal,
    Single,
    FourScreen,
}

#[rustfmt::skip]
pub static PALETTE: [(u8,u8,u8); 64] = [
    (0x80, 0x80, 0x80), (0x00, 0x3D, 0xA6), (0x00, 0x12, 0xB0), (0x44, 0x00, 0x96),
    (0xA1, 0x00, 0x5E), (0xC7, 0x00, 0x28), (0xBA, 0x06, 0x00), (0x8C, 0x17, 0x00),
    (0x5C, 0x2F, 0x00), (0x10, 0x45, 0x00), (0x05, 0x4A, 0x00), (0x00, 0x47, 0x2E),
    (0x00, 0x41, 0x66), (0x00, 0x00, 0x00), (0x05, 0x05, 0x05), (0x05, 0x05, 0x05),
    (0xC7, 0xC7, 0xC7), (0x00, 0x77, 0xFF), (0x21, 0x55, 0xFF), (0x82, 0x37, 0xFA), 
    (0xEB, 0x2F, 0xB5), (0xFF, 0x29, 0x50), (0xFF, 0x22, 0x00), (0xD6, 0x32, 0x00),
    (0xC4, 0x62, 0x00), (0x35, 0x80, 0x00), (0x05, 0x8F, 0x00), (0x00, 0x8A, 0x55),
    (0x00, 0x99, 0xCC), (0x21, 0x21, 0x21), (0x09, 0x09, 0x09), (0x09, 0x09, 0x09),
    (0xFF, 0xFF, 0xFF), (0x0F, 0xD7, 0xFF), (0x69, 0xA2, 0xFF), (0xD4, 0x80, 0xFF),
    (0xFF, 0x45, 0xF3), (0xFF, 0x61, 0x8B), (0xFF, 0x88, 0x33), (0xFF, 0x9C, 0x12), 
    (0xFA, 0xBC, 0x20), (0x9F, 0xE3, 0x0E), (0x2B, 0xF0, 0x35), (0x0C, 0xF0, 0xA4),
    (0x05, 0xFB, 0xFF), (0x5E, 0x5E, 0x5E), (0x0D, 0x0D, 0x0D), (0x0D, 0x0D, 0x0D),
    (0xFF, 0xFF, 0xFF), (0xA6, 0xFC, 0xFF), (0xB3, 0xEC, 0xFF), (0xDA, 0xAB, 0xEB),
    (0xFF, 0xA8, 0xF9), (0xFF, 0xAB, 0xB3), (0xFF, 0xD2, 0xB0), (0xFF, 0xEF, 0xA6),
    (0xFF, 0xF7, 0x9C), (0xD7, 0xE8, 0x95), (0xA6, 0xED, 0xAF), (0xA2, 0xF2, 0xDA), 
    (0x99, 0xFF, 0xFC), (0xDD, 0xDD, 0xDD), (0x11, 0x11, 0x11), (0x11, 0x11, 0x11),
];

#[derive(Clone, Copy)]
#[rustfmt::skip]
#[repr(u8)]
pub enum ControllerButton {
    A      = BIT0,
    B      = BIT1,
    Select = BIT2,
    Start  = BIT3,
    Up     = BIT4,
    Down   = BIT5,
    Left   = BIT6,
    Right  = BIT7,
}

pub static SDL_BUTTON_MAPPING: Lazy<HashMap<Keycode, ControllerButton>> = Lazy::new(|| {
    let mut mapping = HashMap::new();
    mapping.insert(Keycode::J, ControllerButton::A);
    mapping.insert(Keycode::K, ControllerButton::B);
    mapping.insert(Keycode::B, ControllerButton::Select);
    mapping.insert(Keycode::V, ControllerButton::Start);
    mapping.insert(Keycode::W, ControllerButton::Up);
    mapping.insert(Keycode::S, ControllerButton::Down);
    mapping.insert(Keycode::A, ControllerButton::Left);
    mapping.insert(Keycode::D, ControllerButton::Right);
    mapping.insert(Keycode::Up, ControllerButton::Up);
    mapping.insert(Keycode::Down, ControllerButton::Down);
    mapping.insert(Keycode::Left, ControllerButton::Left);
    mapping.insert(Keycode::Right, ControllerButton::Right);
    mapping
});
