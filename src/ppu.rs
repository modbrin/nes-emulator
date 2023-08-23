//! This module contains logic of graphics handing, PPU

use itertools::Itertools;

use crate::prelude::*;

use std::cell::Cell;

const PPU_SCANLINES_NUM: u16 = 262;
const PPU_CYCLES_PER_SCANLINE: usize = 341;
const PPU_TRIGGER_VBLANK_AT: u16 = 241;

/// Picture Processing Unit
#[derive(Debug)]
pub struct Ppu {
    pub chr_data: Vec<u8>,
    pub palette: [u8; PALETTE_COLORS_NUM],
    pub vram: [u8; VRAM_SIZE],
    pub oam_data: [u8; 256],
    pub scr_mirroring: ScrMirror,

    pub reg_addr: Cell<AddressReg>,
    pub reg_ctrl: ControlReg,
    pub reg_status: Cell<StatusReg>,
    pub reg_mask: MaskReg,
    pub reg_scroll: Cell<ScrollReg>,
    pub reg_oam_addr: u8,

    pub internal_buf: Cell<u8>,
    pub scanline: u16,
    pub cycles: usize,
    pub is_pending_nmi: bool,
}

impl Ppu {
    pub fn new(chr_data: Vec<u8>, scr_mirroring: ScrMirror) -> Self {
        Self {
            chr_data,
            palette: [0; PALETTE_COLORS_NUM],
            vram: [0; VRAM_SIZE],
            oam_data: [0; 256],
            scr_mirroring,
            reg_addr: Cell::new(AddressReg::new()),
            reg_ctrl: ControlReg::new(),
            reg_status: Cell::new(StatusReg::new()),
            reg_mask: MaskReg::new(),
            reg_scroll: Cell::new(ScrollReg::new()),
            reg_oam_addr: 0,
            internal_buf: Default::default(),
            scanline: 0,
            cycles: 0,
            is_pending_nmi: false,
        }
    }

    pub fn tick(&mut self, cycles: u8) -> bool {
        self.cycles += cycles as usize;
        if self.cycles >= PPU_CYCLES_PER_SCANLINE {
            self.cycles -= PPU_CYCLES_PER_SCANLINE;
            self.scanline += 1;
            if self.scanline == PPU_TRIGGER_VBLANK_AT {
                self.reg_status.get_mut().set_vblank_started(true);
                self.reg_status.get_mut().set_sprite_zero_hit(false);
                if self.reg_ctrl.gen_nmi_at_vbi {
                    self.is_pending_nmi = true;
                }
            }
            if self.scanline >= PPU_SCANLINES_NUM {
                self.scanline = 0;
                self.is_pending_nmi = false;
                self.reg_status.get_mut().set_vblank_started(false);
                self.reg_status.get_mut().set_sprite_zero_hit(false);
                return true;
            }
        }
        return false;
    }

    pub fn check_sprite_zero_hit(&self, cycle: usize) -> bool {
        // TODO: improve detection using opaque pixels
        let zero_sprite_y = self.oam_data[0] as usize;
        let zero_sprite_x = self.oam_data[3] as usize;
        let overlap_y = zero_sprite_y as u16 == self.scanline;
        let overlap_x = zero_sprite_x == self.cycles;
        overlap_y && overlap_x && self.reg_mask.show_spr
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

    pub fn reg(&self) -> u16 {
        self.reg
    }

    pub fn reset_order(&mut self) {
        self.is_hi = true;
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
        Default::default()
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
        Default::default()
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

#[derive(Default, Debug, Clone, Copy)]
pub struct StatusReg {
    /// BIT5 - Sprite overflow. The intent was for this flag to be set
    /// whenever more than eight sprites appear on a scanline, but a
    /// hardware bug causes the actual behavior to be more complicated
    /// and generate false positives as well as false negatives; see
    /// PPU sprite evaluation. This flag is set during sprite
    /// evaluation and cleared at dot 1 (the second dot) of the
    /// pre-render line.
    ///
    /// BIT6 - Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    /// a nonzero background pixel; cleared at dot 1 of the pre-render
    /// line.  Used for raster timing.
    ///
    /// BIT7 - Vertical blank has started (0: not in vblank; 1: in vblank).
    /// Set at dot 1 of line 241 (the line *after* the post-render
    /// line); cleared after reading $2002 and at dot 1 of the
    /// pre-render line.
    reg: u8,
}

impl StatusReg {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&mut self, val: u8) {
        self.reg = val;
    }

    pub fn reg(&self) -> u8 {
        self.reg
    }

    pub fn get_sprite_overflow(&self) -> bool {
        self.reg & BIT5 != 0
    }

    pub fn get_sprite_zero_hit(&self) -> bool {
        self.reg & BIT6 != 0
    }

    pub fn get_vblank_started(&self) -> bool {
        self.reg & BIT7 != 0
    }

    pub fn set_sprite_overflow(&mut self, state: bool) {
        self.set_bit(BIT5, state)
    }

    pub fn set_sprite_zero_hit(&mut self, state: bool) {
        self.set_bit(BIT6, state)
    }

    pub fn set_vblank_started(&mut self, state: bool) {
        self.set_bit(BIT7, state)
    }

    fn set_bit(&mut self, bit: u8, state: bool) {
        if state {
            self.reg |= bit;
        } else {
            self.reg &= !bit;
        }
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

    pub fn reset_order(&mut self) {
        self.is_x = true;
    }
}

impl Ppu {
    pub fn read_data_reg(&self) -> Result<u8, NesError> {
        let mut reg_addr_copy = self.reg_addr.get();
        let mut addr = reg_addr_copy.reg();
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
                match addr {
                    0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                        addr -= 0x10;
                    }
                    _ => (),
                }
                let offset = (addr - PPU_PALETTE_START) as usize;
                Ok(self.palette[offset])
            }
            _ => Err(NesError::PpuAddressViolation),
        }
    }

    pub fn write_data_reg(&mut self, val: u8) -> Result<(), NesError> {
        let mut reg_addr_copy = self.reg_addr.get();
        let mut addr = reg_addr_copy.reg();

        match addr {
            PPU_CHR_START..=PPU_CHR_END => {
                return Err(NesError::PpuWriteForbidden);
            }
            PPU_RAM_START..=PPU_RAM_END => {
                let mirrored_addr = self.mirror_vram_addr(addr);
                self.vram[mirrored_addr as usize] = val;
            }
            PPU_PALETTE_START..=PPU_PALETTE_END => {
                match addr {
                    0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                        addr -= 0x10;
                    }
                    _ => (),
                }
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
        let offset = (addr & PPU_RAM_END) - PPU_RAM_START;
        match self.scr_mirroring {
            ScrMirror::Vertical => offset % 0x800,
            ScrMirror::Horizontal => {
                if offset >= 0x800 {
                    (offset % 0x400) + 0x400
                } else {
                    offset % 0x400
                }
            }
            ScrMirror::Single => offset % 0x400,
            _ => offset,
        }
    }

    fn write_addr_reg(&mut self, val: u8) {
        self.reg_addr.get_mut().push(val)
    }

    fn write_control_reg(&mut self, val: u8) {
        let old_nmi_flag = self.reg_ctrl.gen_nmi_at_vbi;
        self.reg_ctrl.push(val);
        let is_nmi_set = !old_nmi_flag && self.reg_ctrl.gen_nmi_at_vbi;
        if is_nmi_set && self.reg_status.get().get_vblank_started() {
            self.is_pending_nmi = true;
        }
    }

    fn write_mask_reg(&mut self, val: u8) {
        self.reg_mask.push(val)
    }

    fn read_status_reg(&self) -> u8 {
        {
            let mut tmp = self.reg_scroll.get();
            tmp.reset_order();
            self.reg_scroll.set(tmp);
        }
        {
            let mut tmp = self.reg_addr.get();
            tmp.reset_order();
            self.reg_addr.set(tmp);
        }
        let mut tmp = self.reg_status.get();
        let out = tmp.reg();
        tmp.set_vblank_started(false);
        self.reg_status.set(tmp);
        out
    }

    fn write_oam_addr_reg(&mut self, val: u8) {
        self.reg_oam_addr = val;
    }

    fn write_oam_data_reg(&mut self, val: u8) {
        self.oam_data[self.reg_oam_addr as usize] = val;
        self.reg_oam_addr = self.reg_oam_addr.wrapping_add(1);
    }

    fn read_oam_data_reg(&self) -> u8 {
        self.oam_data[self.reg_oam_addr as usize]
    }

    pub fn write_oam_dma(&mut self, data: impl AsRef<[u8]>) {
        data.as_ref()
            .iter()
            .copied()
            .for_each(|v| self.write_oam_data_reg(v))
    }

    fn write_scroll_reg(&mut self, val: u8) {
        self.reg_scroll.get_mut().push(val);
    }
}

impl RwMemory for Ppu {
    fn read_one(&self, addr: u16) -> Result<u8, NesError> {
        match addr {
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                // Err(NesError::PpuReadForbidden)
                Ok(0)
            }
            0x2002 => Ok(self.read_status_reg()),
            0x2004 => Ok(self.read_oam_data_reg()),
            0x2007 => self.read_data_reg(),
            0x2008..=PPU_MMAP_RNG_END => self.read_one(addr & PPU_MIRROR_MASK),
            _ => {
                println!(
                    "WARN: reading memory outside mapped ppu range {:04x?}",
                    addr
                );
                Ok(0)
            }
        }
    }

    fn write_one(&mut self, addr: u16, val: u8) -> Result<(), NesError> {
        match addr {
            0x2000 => self.write_control_reg(val),
            0x2001 => self.write_mask_reg(val),
            0x2002 => return Err(NesError::PpuWriteForbidden),
            0x2003 => self.write_oam_addr_reg(val),
            0x2004 => self.write_oam_data_reg(val),
            0x2005 => self.write_scroll_reg(val),
            0x2006 => self.write_addr_reg(val),
            0x2007 => self.write_data_reg(val)?,
            0x2008..=PPU_MMAP_RNG_END => self.write_one(addr & PPU_MIRROR_MASK, val)?,
            _ => {
                println!("WARN: writing memory outside mapped ppu range {addr}");
            }
        }
        Ok(())
    }
}

impl Ppu {
    pub fn extract_screen_state(&self, target: &mut Frame) {
        self.draw_background(target);
        self.draw_sprites(target);
    }

    fn draw_background(&self, target: &mut Frame) {
        let bank = self.reg_ctrl.bkg_pattern_table_addr;

        for i in 0..0x03C0 {
            let tile_idx = self.vram[i] as u16;
            let (tile_x, tile_y) = (i % 32, i / 32);
            let chr_idx = (bank + tile_idx * 16) as usize;
            let tile = &self.chr_data[chr_idx..chr_idx + 16];

            let background_pallete = self.get_background_palette(tile_x, tile_y);

            for y in 0..8 {
                // two pixel bits are in different bytes
                let (lo, hi) = (tile[y], tile[y + 8]);
                for x in 0..8 {
                    let value = (((hi >> x) & BIT0) << 1) | ((lo >> x) & BIT0);
                    let rgb = match value {
                        0 => PALETTE[self.palette[0] as usize],
                        1 | 2 | 3 => PALETTE[background_pallete[value as usize] as usize],
                        _ => unreachable!(),
                    };
                    target.set_pixel(
                        tile_x * 8 + 7 - x,
                        tile_y * 8 + y,
                        PixelColor::from_tuple(rgb),
                    )
                }
            }
        }
    }

    fn draw_sprites(&self, target: &mut Frame) {
        assert_eq!(self.oam_data.len() % 4, 0);
        let offset_flipped = |val: usize, offset: usize, flip: bool| {
            if flip {
                val + 7 - offset
            } else {
                val + offset
            }
        };
        let bank: u16 = self.reg_ctrl.spr_pattern_table_addr;

        for (tile_y, tile_idx, attrs, tile_x) in
            self.oam_data.iter().copied().tuples::<(_, _, _, _)>()
        {
            let flip_x = attrs & BIT6 != 0;
            let flip_y = attrs & BIT7 != 0;
            let palette_idx = attrs & 0b11;
            // let priority = attrs & BIT5 == 0;

            let chr_idx = (bank + tile_idx as u16 * 16) as usize;
            let tile = &self.chr_data[chr_idx..chr_idx + 16];

            let sprite_palette = self.get_sprite_palette(palette_idx as usize);

            for y in 0..8 {
                let (lo, hi) = (tile[y], tile[y + 8]);
                for x in 0..8 {
                    let value = (((hi >> x) & BIT0) << 1) | ((lo >> x) & BIT0);
                    if value != 0 {
                        let rgb = PALETTE[sprite_palette[value as usize] as usize];
                        let pixel_x = offset_flipped(tile_x as usize, x, !flip_x);
                        let pixel_y = offset_flipped(tile_y as usize, y, flip_y);
                        target.set_pixel(pixel_x, pixel_y, PixelColor::from_tuple(rgb));
                    }
                }
            }
        }
    }

    fn get_sprite_palette(&self, pallete_idx: usize) -> [u8; 4] {
        let start = 0x11 + pallete_idx * 4;
        [
            0,
            self.palette[start],
            self.palette[start + 1],
            self.palette[start + 2],
        ]
    }

    pub fn get_background_palette(&self, tile_x: usize, tile_y: usize) -> [u8; 4] {
        let attr_table_idx = (tile_y / 4) * 8 + tile_x / 4;
        let attr_byte = self.vram[0x03C0 + attr_table_idx];

        let hi = (tile_y % 4) / 2;
        let lo = (tile_x % 4) / 2;
        let attr_idx = (hi << 1) | lo;
        let palette_idx = (attr_byte >> (attr_idx * 2)) & 0b11;

        let start: usize = 1 + (palette_idx as usize) * 4;
        let c0 = self.palette[0];
        let c1 = self.palette[start];
        let c2 = self.palette[start + 1];
        let c3 = self.palette[start + 2];
        [c0, c1, c2, c3]
    }
}
