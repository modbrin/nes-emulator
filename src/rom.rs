//! This modules contains parsing logic and representation for ROM dump format (iNES)

use crate::prelude::*;

/// Image dump of the cartrige
#[derive(Debug)]
pub struct Rom {
    pub prg_data: Vec<u8>,
    pub chr_data: Vec<u8>,
    pub mapper_type: u8,
    pub scr_mirroring: ScrMirror,
}

impl Rom {
    pub fn parse(data: impl AsRef<[u8]>) -> Result<Rom, &'static str> {
        let data = data.as_ref();
        let header = data.get(0..16).ok_or("Invalid input")?;

        if &header[0..4] != NES_TAG {
            return Err("Unexpected file format - incorrect iNES tag");
        }
        let prg_size = header[4] as usize * PRG_BANK_SIZE;
        let chr_size = header[5] as usize * CHR_BANK_SIZE;
        let mapper_type = (header[6] >> 4) | (header[7] & 0xF0);
        let has_trainer = header[6] & 0b100 != 0;
        if header[7] & 0b1100 != 0 {
            return Err("Only iNES 1.0 is supported");
        }
        let four_screen_bit = header[6] & 0b1000 != 0;
        let vert_horz_bit = header[6] & 1 != 0;
        let scr_mirroring = match (four_screen_bit, vert_horz_bit) {
            (false, false) => ScrMirror::Horizontal,
            (false, true) => ScrMirror::Vertical,
            (true, _) => ScrMirror::FourScreen,
        };
        let prg_start = header.len() + has_trainer.then_some(ROM_TRAINER_SIZE).unwrap_or(0);
        let prg_end = prg_start + prg_size;
        let chr_start = prg_end;
        let chr_end = chr_start + chr_size;
        let prg_data = data
            .get(prg_start..prg_end)
            .ok_or("Corrupt PRG section")?
            .to_vec();
        let chr_data = data
            .get(chr_start..chr_end)
            .ok_or("Corrupt CHR section")?
            .to_vec();
        let rom = Rom {
            prg_data,
            chr_data,
            mapper_type,
            scr_mirroring,
        };
        Ok(rom)
    }
}
