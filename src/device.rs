//! This module contains core logic for device components layout and instructions emulation

use crate::consts::*;
use crate::util::InstructionMetadata as Meta;
use crate::util::*;
use std::thread;
use std::time::{Duration, Instant};

type InstrPtr = for<'a> fn(&'a mut Device, AddressingMode) -> Result<InstructionMetadata, NesError>;

// TODO: use casey proc macro and rewrite as decl macro

#[rustfmt::skip]
static table: &[(Opcode, InstrPtr, AddressingMode, u8)] = {
    use Opcode::*;
    use AddressingMode::*;
    type D = Device;
    &[
         /*0x00*/                                 /*0x01*/
/*0x00*/ (Lda, D::inst_lda, IndexedIndirect, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x10*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x20*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x30*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x40*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x50*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x60*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x70*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x80*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0x90*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0xA0*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0xB0*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0xC0*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0xD0*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0xE0*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
/*0xF0*/ (Lda, D::inst_lda, Absolute,        1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),  (Lda, D::inst_lda, Absolute, 1),
    ]
};

pub struct Device {
    pub cpu: Cpu,
    pub ram: [u8; RAM_SIZE],
}

/// 8-bit 6502 CPU
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

/// Picture Processing Unit
pub struct Ppu {}

/// Audio Processing Unit
pub struct Apu {}

/// Control flow
impl Device {
    pub fn start(&mut self) -> Result<(), NesError> {
        self.load_rom(&[0x00, 0x00, 0x00])?;
        self.reset()?;
        self.run()?;
        Ok(())
    }

    pub fn run(&mut self) -> Result<(), NesError> {
        let time_per_cycle = Duration::from_secs(1) / CPU_FREQ as u32;
        loop {
            let clock = Instant::now();
            // process instruction
            let opcode = self.fetch_next()?;
            let meta = self.decode_and_execute(opcode)?;
            if let Some(pc) = meta.pc_override {
                // TODO: keep this logic in instruction handlers
                self.cpu.pc = pc;
            }
            // match cpu timing
            let elapsed = clock.elapsed();
            if let Some(sleep_time) = (time_per_cycle * meta.cycles as u32).checked_sub(elapsed) {
                thread::sleep(sleep_time);
            } else {
                println!(
                    "Instruction took longer than expected, op: {:#04x}, pc: {}",
                    opcode, self.cpu.pc
                );
            }
            // handle BRK
            if meta.is_break {
                println!("Encountered BRK, stopping");
                return Ok(());
            }
        }
    }

    #[inline]
    /// peek single byte pointed by `pc`
    fn peek_next(&self) -> Result<u8, NesError> {
        self.ram
            .get(self.cpu.pc as usize)
            .copied()
            .ok_or(NesError::RamOutOfBounds)
    }

    /// fetch single byte pointed by `pc`, increment `pc` by 1
    fn fetch_next(&mut self) -> Result<u8, NesError> {
        let inst = self.peek_next()?;
        self.skip_one()?;
        Ok(inst)
    }

    /// increment `pc` by 1
    fn skip_one(&mut self) -> Result<(), NesError> {
        self.cpu.pc.checked_add(1).ok_or(NesError::PcOverflow)?;
        Ok(())
    }
    fn skip_n(&mut self, n: u16) -> Result<(), NesError> {
        self.cpu.pc.checked_add(n).ok_or(NesError::PcOverflow)?;
        Ok(())
    }

    /// decrement `pc` by 1
    fn revert_one(&mut self) -> Result<(), NesError> {
        self.cpu.pc.checked_sub(1).ok_or(NesError::PcUnderflow)?;
        Ok(())
    }

    /// decode and execute the instruction
    fn decode_and_execute(&mut self, opcode: u8) -> InstResult {
        match opcode {
            // BRK
            0x00 => Meta::stop().into(),
            // LDA
            0xA9 => {
                table[0].1(self, table[0].2);
                self.inst_lda(AddressingMode::Absolute)
            }
            // TAX
            0xAA => self.inst_tax(),
            _ => self.unknown(opcode),
        }
    }

    fn load_rom(&mut self, rom: impl AsRef<[u8]>) -> Result<(), NesError> {
        let rom_ref = rom.as_ref();
        if rom_ref.len() > ROM_SECTION.len() {
            return Err(NesError::OversizedRom);
        }
        self.ram
            .get_mut(ROM_SECTION.start..ROM_SECTION.start + rom_ref.len())
            .ok_or(NesError::RamOutOfBounds)?
            .copy_from_slice(rom_ref);
        self.ram_write_le_u16(PC_RESET_ADDR, ROM_SECTION.start as u16);
        Ok(())
    }

    fn unknown(&mut self, opcode: u8) -> InstResult {
        println!("Unknown opcode: {:#04x}", opcode);
        Meta::normal().into()
    }

    fn todo(&mut self) -> InstResult {
        println!("Encountered TODO");
        Meta::normal().into()
    }
}

/// Memory Operations
impl Device {
    /// Read byte from RAM pointed by address
    fn ram_read(&mut self, addr: u16) -> Result<u8, NesError> {
        let val = *self
            .ram
            .get(addr as usize)
            .ok_or(NesError::RamOutOfBounds)?;
        Ok(val)
    }

    /// Write `val` byte to RAM pointed by address
    fn ram_write(&mut self, addr: u16, val: u8) -> Result<(), NesError> {
        *self
            .ram
            .get_mut(addr as usize)
            .ok_or(NesError::RamOutOfBounds)? = val;
        Ok(())
    }

    /// Read little-endian stored u16 from RAM
    fn ram_read_le_u16(&mut self, addr: u16) -> Result<u16, NesError> {
        let lo = self.ram_read(addr)? as u16;
        let hi = self.ram_read(addr.checked_add(1).ok_or(NesError::RamOutOfBounds)?)? as u16;
        Ok(hi << 8 | lo)
    }

    /// Write little-endian u16 to RAM
    fn ram_write_le_u16(&mut self, addr: u16, val: u16) -> Result<(), NesError> {
        let lo = (val & 0x00FF) as u8;
        let hi = ((val & 0xFF00) >> 8) as u8;
        self.ram_write(addr, lo)?;
        self.ram_write(addr.checked_add(1).ok_or(NesError::RamOutOfBounds)?, hi)?;
        Ok(())
    }

    pub fn fetch_param_addr(&mut self, mode: AddressingMode) -> Result<u16, NesError> {
        use AddressingMode::*;
        match mode {
            Immediate => {
                let addr = self.cpu.pc;
                self.skip_one()?;
                Ok(addr)
            }
            Absolute => {
                let addr = self.ram_read_le_u16(self.cpu.pc)?;
                self.skip_n(2)?;
                Ok(addr)
            }
            AbsoluteX => {
                let addr = self.ram_read_le_u16(self.cpu.pc)?;
                self.skip_n(2)?;
                Ok(addr.wrapping_add(self.cpu.reg_x as u16))
            }
            AbsoluteY => {
                let addr = self.ram_read_le_u16(self.cpu.pc)?;
                self.skip_n(2)?;
                Ok(addr.wrapping_add(self.cpu.reg_y as u16))
            }
            Zeropage => {
                let addr = self.ram_read(self.cpu.pc)?;
                self.skip_one()?;
                Ok(addr as u16)
            }
            ZeropageX => {
                let addr = self.ram_read(self.cpu.pc)?;
                self.skip_one()?;
                Ok(addr.wrapping_add(self.cpu.reg_x) as u16)
            }
            ZeropageY => {
                let addr = self.ram_read(self.cpu.pc)?;
                self.skip_one()?;
                Ok(addr.wrapping_add(self.cpu.reg_y) as u16)
            }
            IndexedIndirect => {
                let param = self.ram_read(self.cpu.pc)?;
                self.skip_one()?;
                let addr = param.wrapping_add(self.cpu.reg_x);
                let lo = self.ram_read(addr as u16)? as u16;
                let hi = self.ram_read(addr.wrapping_add(1) as u16)? as u16;
                Ok(hi << 8 | lo)
            }
            IndirectIndexed => {
                let addr = self.ram_read(self.cpu.pc)?;
                self.skip_one()?;
                let lo = self.ram_read(addr as u16)? as u16;
                let hi = self.ram_read(addr.wrapping_add(1) as u16)? as u16;
                Ok((hi << 8 | lo).wrapping_add(self.cpu.reg_y as u16))
            }
            Other => Err(NesError::UnsupportedAddressingMode),
        }
    }
}

/// Flag Operations
impl Device {
    fn reset(&mut self) -> Result<(), NesError> {
        self.cpu.reg_a = 0;
        self.cpu.reg_x = 0;
        self.cpu.reg_y = 0;
        self.cpu.reg_p = Flag::B2 as u8;
        self.cpu.pc = self.ram_read_le_u16(PC_RESET_ADDR)?;
        self.cpu.sp = 0; // FIXME
        Ok(())
    }

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

type AM = AddressingMode;

impl Device {
    /// LDA
    fn inst_lda(&mut self, mode: AddressingMode) -> InstResult {
        let param_addr = self.fetch_param_addr(mode)?;
        self.cpu.reg_a = self.ram_read(param_addr)?;
        self.update_zero_negative_flags(self.cpu.reg_a);
        Meta::normal().into()
    }

    /// TAX
    fn inst_tax(&mut self) -> InstResult {
        self.cpu.reg_x = self.cpu.reg_a;
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// INX
    fn inst_inx(&mut self) -> InstResult {
        self.cpu.reg_x.wrapping_add(1);
        self.update_zero_negative_flags(self.cpu.reg_x);
        Meta::normal().into()
    }

    /// INY
    fn inst_iny(&mut self) -> InstResult {
        self.cpu.reg_y.wrapping_add(1);
        self.update_zero_negative_flags(self.cpu.reg_y);
        Meta::normal().into()
    }
}
