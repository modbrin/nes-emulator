use std::thread;
use std::time::{Duration, Instant};
use crate::util::NesError;

/// available ram memory, 2kb
pub const RAM_SIZE: usize = 2 * 1024;
/// cpu frequency, 1.79 MHz
pub const CPU_FREQ: usize = 1_790_000;
/// ppu frequency, 5.37 MHz
pub const PPU_FREQ: usize = 5_370_000;
/// size of zeropage region in memory
pub const ZEROPAGE_SIZE: usize = 0x100;

pub struct Device {
    pub cpu: Cpu,
    pub ram: [u8; RAM_SIZE],
}

/// 8-bit 6502 CPU
pub struct Cpu {
    /// register `A`
    pub reg_a: u8,
    /// register `X`
    pub reg_x: u8,
    /// register `Y`
    pub reg_y: u8,
    /// status register
    pub reg_p: u8,
    /// stack pointer
    pub reg_sp: u8,
    /// program counter
    pub reg_pc: u16,
}

pub struct InstructionMetadata {
    /// Number of cycles consumed by instruction
    pub cycles: u8,
    /// Apply non-default program counter after execution
    pub pc_override: Option<u16>,
}

impl Default for InstructionMetadata {
    fn default() -> Self {
        Self {
            cycles: 1,
            pc_override: None,
        }
    }
}

pub type InstResult = Result<InstructionMetadata, NesError>;

impl From<InstructionMetadata> for InstResult {
    fn from(meta: InstructionMetadata) -> Self {
        InstResult::Ok(meta)
    }
}

/// Picture Processing Unit
pub struct Ppu {}

/// Audio Processing Unit
pub struct Apu {}

impl Device {
    pub fn run(&mut self) -> Result<u8, NesError> {
        let time_per_cycle = Duration::from_secs(1) / CPU_FREQ as u32;
        loop {
            let clock = Instant::now();
            // process instruction
            let opcode = self.fetch_current()?;
            let meta = self.decode_and_execute(opcode)?;
            if let Some(pc) = meta.pc_override {
                self.cpu.reg_pc = pc;
            } else {
                self.cpu.reg_pc += 1;
            }
            // match cpu timing
            let elapsed = clock.elapsed();
            if let Some(sleep_time) = (time_per_cycle * meta.cycles as u32).checked_sub(elapsed) {
                thread::sleep(sleep_time);
            } else {
                println!("Instruction took longer than expected, op: {:#04x}, pc: {}", opcode, self.cpu.reg_pc);
            }
        }
    }

    fn fetch_current(&mut self) -> Result<u8, NesError> {
        self.ram.get(self.cpu.reg_pc as usize).copied().ok_or(NesError::RamOutOfBounds)
    }

    /// fetch instruction pointed by `pc`
    fn fetch_next(&mut self) -> Result<u8, NesError> {
        let inst = self.fetch_current()?;
        self.cpu.reg_pc += 1;
        Ok(inst)
    }

    /// decode and execute the instruction
    fn decode_and_execute(&mut self, opcode: u8) -> InstResult {
        match opcode {
            // ADC
            0x69 => self.inst_adc_imm(),
            0x65 => self.inst_adc_zp(),
            0x74 => self.inst_adc_zp_x(),


            _ => self.unknown(opcode),
        }
    }

    fn unknown(&mut self, opcode: u8) -> InstResult {
        println!("Unknown opcode: {:#04x}", opcode);
        InstructionMetadata::default().into()
    }

    fn todo(&mut self) -> InstResult {
        println!("Encountered TODO");
        InstructionMetadata::default().into()
    }
}

/// Instruction handlers
///
/// Addressing modes:
/// - Relative     - _rel
/// - Implied      - _imp
/// - Immediate    - _imm
/// - Absolute     - _abs
/// - Absolute,X   - _abs_x
/// - Absolute,Y   - _abs_y
/// - Zeropage     - _zp
/// - Zeropage,X   - _zp_x
/// - Zeropage,Y   - _zp_y
/// - (Zeropage,X) - _zp_x_i  (indirect)
/// - (Zeropage),Y - _zp_y_fi (first indirect)
impl Device {
    /// ADC - Add with Carry
    fn inst_adc(&mut self) -> InstResult {
        todo!()
    }

    /// BRK -
    fn inst_brk(&mut self) -> InstResult {
        todo!()
    }
}
