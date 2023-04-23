use crate::util::InstructionMetadata as Meta;
use crate::util::*;
use std::thread;
use std::time::{Duration, Instant};

/// available ram of cpu, 2kb
pub const RAM_SIZE: usize = 2 * 1024;
/// available ram of ppu, 2kb
pub const VRAM_SIZE: usize = 2 * 1024;
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
                let param = self.fetch_next()?;

                Meta::normal().into()
            }

            _ => self.unknown(opcode),
        }
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

pub enum AddressingMode {
    Implied,
    Accumulator,
    Immediate,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Zeropage,
    ZeropageX,
    ZeropageY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
}

type AM = AddressingMode;

/// Instruction handlers
///
/// Addressing modes:
/// - Implied      - _imp
/// - Accumulator  - _acc
/// - Immediate    - _imm
/// - Relative     - _rel
/// - Absolute     - _abs
/// - Absolute,X   - _abs_x
/// - Absolute,Y   - _abs_y
/// - Zeropage     - _zp
/// - Zeropage,X   - _zp_x
/// - Zeropage,Y   - _zp_y
/// - Indirect     - _i
/// - (Zeropage,X) - _zp_x_i  (indirect)
/// - (Zeropage),Y - _zp_y_fi (first indirect)
///
impl Device {
    // ADC - Add with Carry

    fn inst_adc(&mut self, mode: AddressingMode) -> InstResult {
        match mode {
            AM::Immediate => todo!(),
            AM::Zeropage => todo!(),
            AM::ZeropageX => todo!(),
            AM::ZeropageY => todo!(),
            AM::Absolute => todo!(),
            AM::AbsoluteX => todo!(),
            AM::AbsoluteY => todo!(),
            AM::IndexedIndirect => todo!(),
            AM::IndirectIndexed => todo!(),
            _ => unreachable!("unexpected addressing mode for instruction"),
        }
    }

    fn inst_adc_imm(&mut self) -> InstResult {
        todo!()
    }
    fn inst_adc_zp(&mut self) -> InstResult {
        todo!()
    }
    fn inst_adc_zp_x(&mut self) -> InstResult {
        todo!()
    }
    fn inst_adc_abs(&mut self) -> InstResult {
        todo!()
    }
    fn inst_adc_abs_x(&mut self) -> InstResult {
        todo!()
    }
    fn inst_adc_abs_y(&mut self) -> InstResult {
        todo!()
    }
    fn inst_adc_zp_x_i(&mut self) -> InstResult {
        todo!()
    }
    fn inst_adc_zp_y_fi(&mut self) -> InstResult {
        todo!()
    }

    // AND - Logical AND

    fn inst_and_imm(&mut self) -> InstResult {
        todo!()
    }
    fn inst_and_zp(&mut self) -> InstResult {
        todo!()
    }
    fn inst_and_zp_x(&mut self) -> InstResult {
        todo!()
    }
    fn inst_and_abs(&mut self) -> InstResult {
        todo!()
    }
    fn inst_and_abs_x(&mut self) -> InstResult {
        todo!()
    }
    fn inst_and_abs_y(&mut self) -> InstResult {
        todo!()
    }
    fn inst_and_zp_x_i(&mut self) -> InstResult {
        todo!()
    }
    fn inst_and_zp_y_fi(&mut self) -> InstResult {
        todo!()
    }

    // ASL - Arithmetic Shift Left

    fn inst_asl_acc(&mut self) -> InstResult {
        todo!()
    }
    fn inst_asl_zp(&mut self) -> InstResult {
        todo!()
    }
    fn inst_asl_zp_x(&mut self) -> InstResult {
        todo!()
    }
    fn inst_asl_abs(&mut self) -> InstResult {
        todo!()
    }
    fn inst_asl_abs_x(&mut self) -> InstResult {
        todo!()
    }

    // BCC - Branch if Carry Clear

    fn inst_bcc_rel(&mut self) -> InstResult {
        todo!()
    }

    // BCS - Branch if Carry Set

    fn inst_bcs_rel(&mut self) -> InstResult {
        todo!()
    }

    // BEQ - Branch if Equal

    fn inst_beq_rel(&mut self) -> InstResult {
        todo!()
    }
}
