//! This module contains core logic for device components layout

use std::time::{Duration, Instant};

use crate::cpu::Cpu;
use crate::memory::Bus;
use crate::prelude::*;

const IO_POLL_INTERVAL_CYCLES: usize = 10;

pub struct Device {
    pub cpu: Cpu,
    pub bus: Bus,
}

/// Control flow
impl Device {
    pub fn with_bus(bus: Bus) -> Self {
        Device {
            cpu: Cpu::new(),
            bus,
        }
    }

    pub fn run<F1, F2>(
        &mut self,
        mut render_callback: F1,
        mut io_callback: F2,
    ) -> Result<(), NesError>
    where
        F1: FnMut(&mut Device),
        F2: FnMut(&mut Device),
    {
        let time_per_cycle = Duration::from_secs(1) / CPU_FREQ as u32;
        println!("Predicted TPC {time_per_cycle:?}");
        let mut cycles_since_reset: usize = 0;
        let mut io_poll_counter: usize = 0;
        let mut cycles_clock = Instant::now();

        loop {
            let clock = Instant::now();

            if io_poll_counter == 0 {
                io_callback(self);
            }

            self.check_and_enter_nmi()?;

            // trace(&self);

            // process instruction
            let opcode = self.fetch_next()?;
            let meta = self.decode_and_execute(opcode)?;

            self.cpu.pc = meta.update_pc;

            let render_requested = self.bus.tick(meta.cycles);
            if render_requested {
                render_callback(self);
            }

            // match cpu timing
            let elapsed = clock.elapsed();
            // let expected = time_per_cycle * meta.cycles as u32;
            // if let Some(sleep_time) = expected.checked_sub(elapsed) {
            //     // thread::sleep(sleep_time);
            // } else {
            //     // println!(
            //     //     "Instruction took longer than expected: {}ns/{}ns, op: {:#04x}, pc: {}",
            //     //     elapsed.as_nanos(), expected.as_nanos(), opcode, self.cpu.pc
            //     // );
            // }

            // handle BRK
            // if meta.is_break {
            //     println!("Encountered BRK, stopping");
            //     return Ok(());
            // }

            cycles_since_reset += meta.cycles as usize;
            io_poll_counter += meta.cycles as usize;
            if io_poll_counter > IO_POLL_INTERVAL_CYCLES {
                io_poll_counter = 0;
            }
            if cycles_clock.elapsed() > Duration::from_secs(1) {
                println!("Cycles per second: {cycles_since_reset}, frame time: {elapsed:?}");
                cycles_since_reset = 0;
                cycles_clock = Instant::now();
            }
        }
    }

    /// fetch single byte pointed by `pc`, increment `pc` by 1
    pub(crate) fn fetch_next(&mut self) -> Result<u8, NesError> {
        let inst = self.bus.read_one(self.cpu.pc)?;
        self.skip_one()?;
        Ok(inst)
    }

    /// increment `pc` by 1
    pub(crate) fn skip_one(&mut self) -> Result<(), NesError> {
        self.cpu.pc = self.pc_plus_n(1)?;
        Ok(())
    }

    /// increment `pc` by n
    pub(crate) fn skip_n(&mut self, n: u16) -> Result<(), NesError> {
        self.cpu.pc = self.pc_plus_n(n)?;
        Ok(())
    }

    /// get value of `pc` incremented by n
    pub(crate) fn pc_plus_n(&mut self, n: u16) -> Result<u16, NesError> {
        Ok(self.cpu.pc.checked_add(n).ok_or(NesError::PcOverflow)?)
    }

    pub fn reset(&mut self) -> Result<(), NesError> {
        self.cpu.reg_a = 0;
        self.cpu.reg_x = 0;
        self.cpu.reg_y = 0;
        self.cpu.reg_p = Flag::B2 as u8 | Flag::IntDis as u8;
        self.cpu.pc = self.read_le_u16(PC_RESET_ADDR)?;
        self.cpu.sp = 0xfd;
        Ok(())
    }
}
