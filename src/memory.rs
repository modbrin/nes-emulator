//! This module contians logic for memory handling, namely ram and vram

use crate::prelude::*;

/// Memory Operations
impl Device {
    /// Read byte from RAM pointed by address
    pub(crate) fn ram_read(&mut self, addr: u16) -> Result<u8, NesError> {
        let val = *self
            .ram
            .get(addr as usize)
            .ok_or(NesError::RamOutOfBounds)?;
        Ok(val)
    }

    /// Write `val` byte to RAM pointed by address
    pub(crate) fn ram_write(&mut self, addr: u16, val: u8) -> Result<(), NesError> {
        *self
            .ram
            .get_mut(addr as usize)
            .ok_or(NesError::RamOutOfBounds)? = val;
        Ok(())
    }

    /// Read little-endian stored u16 from RAM
    pub(crate) fn ram_read_le_u16(&mut self, addr: u16) -> Result<u16, NesError> {
        let lo = self.ram_read(addr)? as u16;
        let hi = self.ram_read(addr.checked_add(1).ok_or(NesError::RamOutOfBounds)?)? as u16;
        Ok(hi << 8 | lo)
    }

    /// Write little-endian u16 to RAM
    pub(crate) fn ram_write_le_u16(&mut self, addr: u16, val: u16) -> Result<(), NesError> {
        let lo = (val & 0x00FF) as u8;
        let hi = ((val & 0xFF00) >> 8) as u8;
        self.ram_write(addr, lo)?;
        self.ram_write(addr.checked_add(1).ok_or(NesError::RamOutOfBounds)?, hi)?;
        Ok(())
    }

    pub(crate) fn fetch_param_addr(&mut self, mode: AddressingMode) -> Result<u16, NesError> {
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
            Implied => Err(NesError::UnsupportedAddressingMode),
        }
    }
}
