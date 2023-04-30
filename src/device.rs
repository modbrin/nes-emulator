//! This module contains core logic for device components layout

use crate::cpu::Cpu;
use crate::prelude::*;
use std::thread;
use std::time::{Duration, Instant};

pub struct Device {
    pub cpu: Cpu,
    pub ram: [u8; RAM_SIZE],
}

/// Control flow
impl Device {
    pub fn new() -> Self {
        Device {
            cpu: Cpu::new(),
            ram: [0; RAM_SIZE],
        }
    }

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
    pub(crate) fn peek_next(&self) -> Result<u8, NesError> {
        self.ram
            .get(self.cpu.pc as usize)
            .copied()
            .ok_or(NesError::RamOutOfBounds)
    }

    /// fetch single byte pointed by `pc`, increment `pc` by 1
    pub(crate) fn fetch_next(&mut self) -> Result<u8, NesError> {
        let inst = self.peek_next()?;
        self.skip_one()?;
        Ok(inst)
    }

    /// increment `pc` by 1
    pub(crate) fn skip_one(&mut self) -> Result<(), NesError> {
        self.cpu.pc.checked_add(1).ok_or(NesError::PcOverflow)?;
        Ok(())
    }
    pub(crate) fn skip_n(&mut self, n: u16) -> Result<(), NesError> {
        self.cpu.pc.checked_add(n).ok_or(NesError::PcOverflow)?;
        Ok(())
    }

    /// decrement `pc` by 1
    pub(crate) fn revert_one(&mut self) -> Result<(), NesError> {
        self.cpu.pc.checked_sub(1).ok_or(NesError::PcUnderflow)?;
        Ok(())
    }

    pub fn load_rom(&mut self, rom: impl AsRef<[u8]>) -> Result<(), NesError> {
        let rom_ref = rom.as_ref();
        if rom_ref.len() > ROM_SECTION.len() {
            return Err(NesError::OversizedRom);
        }
        self.ram
            .get_mut(ROM_SECTION.start..ROM_SECTION.start + rom_ref.len())
            .ok_or(NesError::RamOutOfBounds)?
            .copy_from_slice(rom_ref);
        self.ram_write_le_u16(PC_RESET_ADDR, ROM_SECTION.start as u16)?;
        Ok(())
    }

    fn reset(&mut self) -> Result<(), NesError> {
        self.cpu.reg_a = 0;
        self.cpu.reg_x = 0;
        self.cpu.reg_y = 0;
        self.cpu.reg_p = Flag::B2 as u8 | Flag::IntDis as u8;
        self.cpu.pc = self.ram_read_le_u16(PC_RESET_ADDR)?;
        self.cpu.sp = 0xfd;
        Ok(())
    }
}
