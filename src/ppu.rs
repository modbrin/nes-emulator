//! This module contains logic of graphics handing, PPU

use crate::prelude::*;

use std::cell::Cell;

/// Picture Processing Unit
#[derive(Debug)]
pub struct Ppu {
    pub chr_data: Vec<u8>,
    pub palette: [u8; PALETTE_COLORS],
    pub vram: [u8; VRAM_SIZE],
    pub oam_data: [u8; 256],
    pub scr_mirroring: ScrMirror,

    pub reg_addr: Cell<AddressReg>,
    pub reg_ctrl: ControlReg,
    pub internal_buf: Cell<u8>,
}

impl Ppu {
    pub fn new(chr_data: Vec<u8>, scr_mirroring: ScrMirror) -> Self {
        Self {
            chr_data,
            palette: [0; PALETTE_COLORS],
            vram: [0; VRAM_SIZE],
            oam_data: [0; 256],
            scr_mirroring,
            reg_addr: Cell::new(AddressReg::new()),
            reg_ctrl: ControlReg::new(),
            internal_buf: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AddressReg {
    reg: u16,
    is_hi: bool,
}

impl AddressReg {
    pub fn new() -> Self {
        Self {
            reg: 0,
            is_hi: true,
        }
    }

    pub fn push(&mut self, val: u8) {
        if self.is_hi {
            self.reg = (self.reg & 0x00FF) | ((val as u16) << 8);
        } else {
            self.reg = (self.reg & 0xFF00) | (val as u16);
        }
        self.reg &= 0x3FFF;
        self.is_hi = !self.is_hi;
    }

    pub fn add(&mut self, val: u8) {
        self.reg = self.reg.wrapping_add(val as u16) & 0x3FFF;
    }

    pub fn get(&self) -> u16 {
        self.reg
    }
}

#[derive(Debug)]
pub struct ControlReg {
    reg: u8,
}

impl ControlReg {
    pub fn new() -> Self {
        Self { reg: 0 }
    }

    pub fn push(&mut self, val: u8) {
        self.reg = val;
    }

    pub fn vram_addr_inc_value(&self) -> u8 {
        let bit = ControlRegType::VramAddrInc as u8;
        (self.reg & bit != 0).then(|| 32).unwrap_or_else(|| 1)
    }
}

impl Ppu {
    pub fn read_data_reg(&self) -> Result<u8, NesError> {
        let mut reg_addr_copy = self.reg_addr.get();
        let addr = reg_addr_copy.get();
        reg_addr_copy.add(self.reg_ctrl.vram_addr_inc_value());
        self.reg_addr.set(reg_addr_copy);

        match addr {
            PPU_CHR_START..=PPU_CHR_END => {
                let ret = self.internal_buf.get();
                self.internal_buf.set(self.chr_data[addr as usize]);
                Ok(ret)
            }
            PPU_RAM_START..=PPU_RAM_END => {
                let ret = self.internal_buf.get();
                let mirrored_addr = self.mirror_vram_addr(addr);
                self.internal_buf.set(self.vram[mirrored_addr as usize]);
                Ok(ret)
            }
            PPU_PALETTE_START..=PPU_PALETTE_END => {
                let offset = (addr - PPU_PALETTE_START) as usize;
                Ok(self.palette[offset])
            }
            _ => Err(NesError::PpuAddressViolation),
        }
    }

    pub fn write_data_reg(&mut self, val: u8) -> Result<(), NesError> {
        let mut reg_addr_copy = self.reg_addr.get();
        let addr = reg_addr_copy.get();

        match addr {
            PPU_CHR_START..=PPU_CHR_END => {
                return Err(NesError::PpuWriteForbidden);
            }
            PPU_RAM_START..=PPU_RAM_END => {
                let mirrored_addr = self.mirror_vram_addr(addr);
                self.vram[mirrored_addr as usize] = val;
            }
            PPU_PALETTE_START..=PPU_PALETTE_END => {
                let offset = (addr - PPU_PALETTE_START) as usize;
                self.palette[offset] = val;
            }
            _ => return Err(NesError::PpuAddressViolation),
        }
        reg_addr_copy.add(self.reg_ctrl.vram_addr_inc_value());
        self.reg_addr.set(reg_addr_copy);
        Ok(())
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let offset = addr - PPU_RAM_START;
        match self.scr_mirroring {
            ScrMirror::Vertical => offset % 0x800,
            ScrMirror::Horizontal => {
                if offset > 0x800 {
                    offset % 0x400 + 0x800
                } else {
                    offset % 0x400
                }
            }
            ScrMirror::Single => offset % 0x400,
            _ => offset,
        }
    }
}

impl RwMemory for Ppu {
    fn read_one(&self, addr: u16) -> Result<u8, NesError> {
        match addr {
            (0x2000..=0x2006) | 0x4014 => Err(NesError::PpuReadForbidden),
            0x2007 => self.read_data_reg(),
            0x2008..=PPU_MMAP_RNG_END => {
                let mirrored_addr = addr & PPU_MIRROR_MASK;
                self.read_one(mirrored_addr)
            }
            _ => {
                println!("WARN: reading memory outside mapped ppu range {addr}");
                Ok(0)
            }
        }
    }

    fn write_one(&mut self, addr: u16, val: u8) -> Result<(), NesError> {
        match addr {
            0x2000 => self.reg_ctrl.push(val),
            0x2006 => {
                let mut ra = self.reg_addr.get();
                ra.push(val);
                self.reg_addr.set(ra);
            }
            0x2007 => {
                self.write_data_reg(val)?;
            }
            0x2008..=PPU_MMAP_RNG_END => {
                let mirrored_addr = addr & PPU_MIRROR_MASK;
                self.write_one(mirrored_addr, val)?;
            }
            _ => {
                println!("WARN: reading memory outside mapped ppu range {addr}");
            }
        }
        Ok(())
    }
}
