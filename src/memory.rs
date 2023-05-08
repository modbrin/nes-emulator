//! This module contians logic for memory handling, namely ram and vram

use crate::prelude::*;

/// Memory Operations
impl Device {
    pub(crate) fn stack_push(&mut self, val: u8) -> Result<(), NesError> {
        if self.cpu.sp == 0 {
            return Err(NesError::StackOverflow);
        }
        self.write_one(STACK_SECTION_START + self.cpu.sp as u16, val)?;
        self.cpu.sp -= 1;
        Ok(())
    }

    pub(crate) fn stack_pop(&mut self) -> Result<u8, NesError> {
        if self.cpu.sp == u8::MAX {
            return Err(NesError::StackUnderflow);
        }
        self.cpu.sp += 1;
        let val = self.read_one(STACK_SECTION_START + self.cpu.sp as u16)?;
        Ok(val)
    }

    pub(crate) fn stack_push_le_u16(&mut self, val: u16) -> Result<(), NesError> {
        let lo = (val & 0x00FF) as u8;
        let hi = (val >> 8) as u8;
        self.stack_push(hi)?;
        self.stack_push(lo)?;
        Ok(())
    }

    pub(crate) fn stack_pop_le_u16(&mut self) -> Result<u16, NesError> {
        let lo = self.stack_pop()? as u16;
        let hi = self.stack_pop()? as u16;
        Ok((hi << 8) | lo)
    }

    pub(crate) fn fetch_param_addr(&mut self, mode: AddressingMode) -> Result<AddrRes, NesError> {
        use AddressingMode::*;
        match mode {
            Immediate => {
                let addr = self.cpu.pc;
                Ok(AddrRes::new(addr, self.pc_plus_n(1)?))
            }
            Absolute => {
                let addr = self.read_le_u16(self.cpu.pc)?;
                Ok(AddrRes::new(addr, self.pc_plus_n(2)?))
            }
            AbsoluteX => {
                let addr = self.read_le_u16(self.cpu.pc)?;
                Ok(AddrRes::new(
                    addr.wrapping_add(self.cpu.reg_x as u16),
                    self.pc_plus_n(2)?,
                ))
            }
            AbsoluteY => {
                let addr = self.read_le_u16(self.cpu.pc)?;
                Ok(AddrRes::new(
                    addr.wrapping_add(self.cpu.reg_y as u16),
                    self.pc_plus_n(2)?,
                ))
            }
            Zeropage => {
                let addr = self.read_one(self.cpu.pc)?;
                Ok(AddrRes::new(addr as u16, self.pc_plus_n(1)?))
            }
            ZeropageX => {
                let addr = self.read_one(self.cpu.pc)?;
                Ok(AddrRes::new(
                    addr.wrapping_add(self.cpu.reg_x) as u16,
                    self.pc_plus_n(1)?,
                ))
            }
            ZeropageY => {
                let addr = self.read_one(self.cpu.pc)?;
                Ok(AddrRes::new(
                    addr.wrapping_add(self.cpu.reg_y) as u16,
                    self.pc_plus_n(1)?,
                ))
            }
            IndexedIndirect => {
                let param = self.read_one(self.cpu.pc)?;
                let addr = param.wrapping_add(self.cpu.reg_x);
                let lo = self.read_one(addr as u16)? as u16;
                let hi = self.read_one(addr.wrapping_add(1) as u16)? as u16;
                Ok(AddrRes::new((hi << 8) | lo, self.pc_plus_n(1)?))
            }
            IndirectIndexed => {
                let addr = self.read_one(self.cpu.pc)?;
                let lo = self.read_one(addr as u16)? as u16;
                let hi = self.read_one(addr.wrapping_add(1) as u16)? as u16;
                Ok(AddrRes::new(
                    ((hi << 8) | lo).wrapping_add(self.cpu.reg_y as u16),
                    self.pc_plus_n(1)?,
                ))
            }
            _ => Err(NesError::UnsupportedAddressingMode),
        }
    }
}

pub trait RwMemory {
    /// Read byte from mapped memory pointed by address
    fn read_one(&self, addr: u16) -> Result<u8, NesError>;

    /// Write `val` byte to mapped memory pointed by address
    fn write_one(&mut self, addr: u16, val: u8) -> Result<(), NesError>;

    /// Read little-endian stored u16 from mapped memory
    fn read_le_u16(&self, addr: u16) -> Result<u16, NesError> {
        let lo = self.read_one(addr)? as u16;
        let hi = self.read_one(addr.checked_add(1).ok_or(NesError::MemoryOutOfBounds)?)? as u16;
        Ok((hi << 8) | lo)
    }

    /// Write little-endian u16 to mapped memory
    fn write_le_u16(&mut self, addr: u16, val: u16) -> Result<(), NesError> {
        let lo = (val & 0x00FF) as u8;
        let hi = (val >> 8) as u8;
        self.write_one(addr, lo)?;
        self.write_one(addr.checked_add(1).ok_or(NesError::MemoryOutOfBounds)?, hi)?;
        Ok(())
    }
}

impl RwMemory for Device {
    /// Read byte from mapped memory pointed by address
    fn read_one(&self, addr: u16) -> Result<u8, NesError> {
        self.bus.read_one(addr)
    }

    /// Write `val` byte to mapped memory pointed by address
    fn write_one(&mut self, addr: u16, val: u8) -> Result<(), NesError> {
        self.bus.write_one(addr, val)
    }
}

#[derive(Debug)]
pub struct Bus {
    pub ram: [u8; RAM_SIZE],
    pub rom: Rom,
}

impl Bus {
    pub fn with_rom(rom: Rom) -> Self {
        Self {
            ram: [0; RAM_SIZE],
            rom,
        }
    }
}

impl RwMemory for Bus {
    fn read_one(&self, addr: u16) -> Result<u8, NesError> {
        match addr {
            CPU_MMAP_RNG_START..=CPU_MMAP_RNG_END => {
                let mapped_addr = addr & CPU_MIRROR_MASK;
                Ok(self.ram[mapped_addr as usize])
            }
            PPU_MMAP_RNG_START..=PPU_MMAP_RNG_END => {
                let mapped_addr = addr & PPU_MIRROR_MASK;
                // todo!()
                Ok(0)
            }
            ROM_SECTION_START..=ROM_SECTION_END => {
                let mut prg_offset = (addr - ROM_SECTION_START) as usize;
                if self.rom.prg_data.len() == SIZE_16KB && prg_offset >= SIZE_16KB {
                    prg_offset %= SIZE_16KB;
                }
                Ok(self.rom.prg_data[prg_offset])
            }
            _ => {
                println!("WARN: Reading memory outside mapped bus range: {addr}");
                Ok(0x00)
            }
        }
    }

    fn write_one(&mut self, addr: u16, val: u8) -> Result<(), NesError> {
        match addr {
            CPU_MMAP_RNG_START..=CPU_MMAP_RNG_END => {
                let mapped_addr = addr & CPU_MIRROR_MASK;
                self.ram[mapped_addr as usize] = val;
            }
            PPU_MMAP_RNG_START..=PPU_MMAP_RNG_END => {
                let mapped_addr = addr & PPU_MIRROR_MASK;
                todo!()
            }
            ROM_SECTION_START..=ROM_SECTION_END => {
                return Err(NesError::RomWriteAttempt);
            }
            _ => {
                println!("WARN: Writing memory outside mapped bus range: {addr}");
            }
        }
        Ok(())
    }
}
