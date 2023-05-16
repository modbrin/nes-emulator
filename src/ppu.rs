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
    pub reg_status: StatusReg,
    pub reg_mask: MaskReg,
    pub reg_scroll: ScrollReg,
    pub reg_oam_addr: u8,

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
            reg_status: StatusReg::new(),
            reg_mask: MaskReg::new(),
            reg_scroll: ScrollReg::new(),
            reg_oam_addr: 0,
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

#[derive(Default, Debug)]
pub struct ControlReg {
    /// Base nametable address
    /// (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    pub base_nametable_addr: u16,
    /// VRAM address increment per CPU read/write of PPUDATA
    /// (0: add 1, going across; 1: add 32, going down)
    pub vram_addr_inc: u8,
    /// Sprite pattern table address for 8x8 sprites
    /// (0: $0000; 1: $1000; ignored in 8x16 mode)
    pub spr_pattern_table_addr: u16,
    /// Background pattern table address (0: $0000; 1: $1000)
    pub bkg_pattern_table_addr: u16,
    /// Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
    pub sprite_size: (u8, u8),
    /// PPU master/slave select
    /// (0: read backdrop from EXT pins; 1: output color on EXT pins)
    pub ppu_master_slave: bool,
    /// Generate an NMI at the start of the vertical blanking interval (0: off; 1: on)
    pub gen_nmi_at_vbi: bool,
}

impl ControlReg {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn push(&mut self, val: u8) {
        let is_set = |bit: u8| val & bit != 0;
        self.base_nametable_addr = [0x2000, 0x2400, 0x2800, 0x2C00][(val & 0b11) as usize];
        self.vram_addr_inc = is_set(BIT2).then(|| 32).unwrap_or_else(|| 1);
        self.spr_pattern_table_addr = is_set(BIT3).then(|| 0x1000).unwrap_or_else(|| 0);
        self.bkg_pattern_table_addr = is_set(BIT4).then(|| 0x1000).unwrap_or_else(|| 0);
        self.sprite_size = is_set(BIT5).then(|| (8, 16)).unwrap_or_else(|| (8, 8));
        self.ppu_master_slave = is_set(BIT6);
        self.gen_nmi_at_vbi = is_set(BIT7);
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct MaskReg {
    /// Greyscale (0: normal color, 1: produce a greyscale display)
    pub is_grayscale: bool,
    /// 1: Show background in leftmost 8 pixels of screen, 0: Hide
    pub show_bgd_lm_8: bool,
    /// 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    pub show_spr_lm_8: bool,
    /// 1: Show background
    pub show_bgd: bool,
    /// 1: Show sprites
    pub show_spr: bool,
    /// Emphasize red (green on PAL/Dendy)
    pub emph_red: bool,
    /// Emphasize green (red on PAL/Dendy)
    pub emph_green: bool,
    /// Emphasize blue
    pub emph_blue: bool,
}

impl MaskReg {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn push(&mut self, val: u8) {
        let is_set = |bit: u8| val & bit != 0;
        self.is_grayscale = is_set(BIT0);
        self.show_bgd_lm_8 = is_set(BIT1);
        self.show_spr_lm_8 = is_set(BIT2);
        self.show_bgd = is_set(BIT3);
        self.show_spr = is_set(BIT4);
        self.emph_red = is_set(BIT5);
        self.emph_green = is_set(BIT6);
        self.emph_blue = is_set(BIT7);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StatusReg {}

impl StatusReg {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ScrollReg {
    pub x_scroll: u8,
    pub y_scroll: u8,
    is_x: bool,
}

impl ScrollReg {
    pub fn new() -> Self {
        Self {
            x_scroll: 0,
            y_scroll: 0,
            is_x: true,
        }
    }

    pub fn push(&mut self, val: u8) {
        if self.is_x {
            self.x_scroll = val;
        } else {
            self.y_scroll = val;
        }
        self.is_x = !self.is_x;
    }
}

impl Ppu {
    pub fn read_data_reg(&self) -> Result<u8, NesError> {
        let mut reg_addr_copy = self.reg_addr.get();
        let addr = reg_addr_copy.get();
        reg_addr_copy.add(self.reg_ctrl.vram_addr_inc);
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
        reg_addr_copy.add(self.reg_ctrl.vram_addr_inc);
        self.reg_addr.set(reg_addr_copy);
        Ok(())
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let offset = addr - PPU_RAM_START;
        match self.scr_mirroring {
            ScrMirror::Vertical => offset % 0x800,
            ScrMirror::Horizontal => {
                if offset >= 0x800 {
                    offset % 0x400 + 0x800
                } else {
                    offset % 0x400
                }
            }
            ScrMirror::Single => offset % 0x400,
            _ => offset,
        }
    }

    fn write_oam_addr_reg(&mut self, addr: u8) {
        self.reg_oam_addr = addr;
    }

    fn write_oam_data_reg(&mut self, val: u8) {
        self.oam_data[self.reg_oam_addr as usize] = val;
        self.reg_oam_addr = self.reg_oam_addr.wrapping_add(1);
    }

    fn read_oam_data_reg(&mut self) -> u8 {
        self.oam_data[self.reg_oam_addr as usize]
    }

    fn write_oam_dma(&mut self, data: impl AsRef<[u8]>) {
        data.as_ref()
            .iter()
            .copied()
            .for_each(|v| self.write_oam_data_reg(v))
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
