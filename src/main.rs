//! This module contains logic for application startup

use std::{
    fs::File,
    io::{BufReader, Read},
    ops::Not,
};

use device::Device;
use rand::Rng;
use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    EventPump,
};

mod consts;
mod cpu;
mod device;
mod memory;
mod ppu;
mod rom;
mod util;

pub(crate) mod prelude {
    pub use crate::device::Device;

    pub use crate::consts::*;
    pub use crate::memory::*;
    pub use crate::rom::*;
    pub use crate::util::*;
}
use prelude::*;

use crate::cpu::INST_TABLE;

fn handle_user_input(device: &mut Device, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => {
                std::process::exit(0);
            }
            Event::KeyDown {
                keycode: Some(Keycode::W),
                ..
            } => {
                device.write_one(0xFF, 0x77).unwrap();
            }
            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => {
                device.write_one(0xFF, 0x73).unwrap();
            }
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => {
                device.write_one(0xFF, 0x61).unwrap();
            }
            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => {
                device.write_one(0xFF, 0x64).unwrap();
            }
            _ => { /* do nothing */ }
        }
    }
}

fn color(byte: u8) -> Color {
    match byte {
        0 => Color::BLACK,
        1 => Color::WHITE,
        2 | 9 => Color::GREY,
        3 | 10 => Color::RED,
        4 | 11 => Color::GREEN,
        5 | 12 => Color::BLUE,
        6 | 13 => Color::MAGENTA,
        7 | 14 => Color::YELLOW,
        _ => Color::CYAN,
    }
}

fn read_screen_state(device: &Device, frame: &mut [u8; 32 * 32 * 3]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    for i in 0x0200..0x0600 {
        let color_idx = device.read_one(i as u16).unwrap();
        let (b1, b2, b3) = color(color_idx).rgb();
        if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx + 1] = b2;
            frame[frame_idx + 2] = b3;
            update = true;
        }
        frame_idx += 3;
    }
    update
}

fn main() {
    // let sdl_context = sdl2::init().unwrap();
    // let video_subsystem = sdl_context.video().unwrap();
    // let window = video_subsystem
    //     .window("Snake game", (32.0 * 10.0) as u32, (32.0 * 10.0) as u32)
    //     .position_centered()
    //     .build().unwrap();

    // let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    // let mut event_pump = sdl_context.event_pump().unwrap();
    // canvas.set_scale(10.0, 10.0).unwrap();

    // let creator = canvas.texture_creator();
    // let mut texture = creator.create_texture_target(PixelFormatEnum::RGB24, 32, 32).unwrap();

    let file = File::open("roms/nestest.nes").unwrap();
    let mut buf = BufReader::new(file);
    let mut rom_data = Vec::new();
    buf.read_to_end(&mut rom_data).unwrap();

    let rom = Rom::parse(rom_data).unwrap();
    let mut device = Device::with_rom(rom);
    // device.load_rom(game_code).unwrap();

    let mut screen_state = [0 as u8; 32 * 32 * 3];
    let mut rng = rand::thread_rng();

    device.reset().unwrap();

    device.cpu.pc = 0xC000; // no ppu mode

    device
        .run(move |device| {
            // handle_user_input(device, &mut event_pump);
            // device.write_one(0xfe, rng.gen_range(1..16)).unwrap();

            // if read_screen_state(device, &mut screen_state) {
            //     texture.update(None, &screen_state, 32 * 3).unwrap();
            //     canvas.copy(&texture, None, None).unwrap();
            //     canvas.present();
            // }
            trace(&device)
        })
        .unwrap();
}

fn get_operands_repr(device: &Device, mode: AddressingMode, op: Opcode) -> String {
    use AddressingMode::*;
    let lowercase = match mode {
        Immediate => {
            let val = device.read_one(device.cpu.pc + 1).unwrap();
            format!("#${:02x}", val)
        }
        Absolute => {
            let addr = device.read_le_u16(device.cpu.pc + 1).unwrap();
            let val = device.read_one(addr).unwrap();
            match op {
                Opcode::JMP | Opcode::JSR => format!("${:04x}", addr),
                _ => format!("${:04x} = {:02x}", addr, val),
            }
        }
        AbsoluteX => {
            let addr = device.read_le_u16(device.cpu.pc + 1).unwrap();
            let res = addr.wrapping_add(device.cpu.reg_x as u16);
            let val = device.read_one(res).unwrap();
            format!("${:04x},X @ {:04x} = {:02x}", addr, res, val)
        }
        AbsoluteY => {
            let addr = device.read_le_u16(device.cpu.pc + 1).unwrap();
            let res = addr.wrapping_add(device.cpu.reg_y as u16);
            let val = device.read_one(res).unwrap();
            format!("${:04x},Y @ {:04x} = {:02x}", addr, res, val)
        }
        Zeropage => {
            let addr = device.read_one(device.cpu.pc + 1).unwrap() as u16;
            let val = device.read_one(addr).unwrap();
            format!("${:02x} = {:02x}", addr, val)
        }
        ZeropageX => {
            let addr = device.read_one(device.cpu.pc + 1).unwrap();
            let res = addr.wrapping_add(device.cpu.reg_x) as u16;
            let val = device.read_one(res).unwrap();
            format!("${:02x},X @ {:02x} = {:02x}", addr, res, val)
        }
        ZeropageY => {
            let addr = device.read_one(device.cpu.pc + 1).unwrap();
            let res = addr.wrapping_add(device.cpu.reg_y) as u16;
            let val = device.read_one(res).unwrap();
            format!("${:02x},Y @ {:02x} = {:02x}", addr, res, val)
        }
        IndexedIndirect => {
            let param = device.read_one(device.cpu.pc + 1).unwrap();
            let addr = param.wrapping_add(device.cpu.reg_x);
            let lo = device.read_one(addr as u16).unwrap() as u16;
            let hi = device.read_one(addr.wrapping_add(1) as u16).unwrap() as u16;
            let res = (hi << 8) | lo;
            let val = device.read_one(res).unwrap();
            format!(
                "(${:02x},X) @ {:02x} = {:04x} = {:02x}",
                param, addr, res, val
            )
        }
        IndirectIndexed => {
            let addr = device.read_one(device.cpu.pc + 1).unwrap();
            let lo = device.read_one(addr as u16).unwrap() as u16;
            let hi = device.read_one(addr.wrapping_add(1) as u16).unwrap() as u16;
            let loc = (hi << 8) | lo;
            let res = loc.wrapping_add(device.cpu.reg_y as u16);
            let val = device.read_one(res).unwrap();
            format!(
                "(${:02x}),Y = {:04x} @ {:04x} = {:02x}",
                addr, loc, res, val
            )
        }
        Accumulator => {
            format!("A")
        }
        Implied => {
            format!("")
        }
        Relative => {
            let mut new_pc = device.cpu.pc + 1;
            let offset = device.read_one(new_pc).unwrap();
            new_pc += 1;
            let target = if offset & 0x80 != 0 {
                // subtract two's complement
                new_pc.wrapping_sub(offset.not().wrapping_add(1) as u16)
            } else {
                new_pc.wrapping_add(offset as u16)
            };
            format!("${:04x}", target)
        }
        Indirect => {
            let addr = device.read_le_u16(device.cpu.pc + 1).unwrap();
            // 6502 bug imitation
            let loc = if addr & 0x00FF == 0x00FF {
                let lo = device.read_one(addr).unwrap() as u16;
                let hi = device.read_one(addr & 0xFF00).unwrap() as u16;
                hi << 8 | lo
            } else {
                device.read_le_u16(addr).unwrap()
            };
            format!("(${:04x}) = {:04x}", addr, loc)
        }
    };
    lowercase.to_uppercase()
}

fn trace(device: &Device) {
    let inst = device.bus.read_one(device.cpu.pc).unwrap();
    let (_, op, func, addr_mode, cycles) = INST_TABLE[inst as usize];

    let pc_str = format!("{:04x}", device.cpu.pc).to_uppercase();

    let text_op = format!(" {:?}", op);

    let b0 = device.bus.read_one(device.cpu.pc).unwrap();
    let b1 = device.bus.read_one(device.cpu.pc + 1);
    let b2 = device.bus.read_one(device.cpu.pc + 2);
    use AddressingMode::*;
    let bytes = match addr_mode {
        Implied | Accumulator => vec![b0],
        Indirect | Absolute | AbsoluteX | AbsoluteY => vec![b0, b1.unwrap(), b2.unwrap()],
        Relative | Immediate | Zeropage | ZeropageX | ZeropageY | IndirectIndexed
        | IndexedIndirect => vec![b0, b1.unwrap()],
    };
    let bytes_str = bytes
        .iter()
        .map(|b| format!("{:02x}", b).to_uppercase())
        .collect::<Vec<_>>()
        .join(" ");
    let operands = get_operands_repr(device, addr_mode, op);
    let registers = format!(
        "A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x}",
        device.cpu.reg_a, device.cpu.reg_x, device.cpu.reg_y, device.cpu.reg_p, device.cpu.sp
    )
    .to_uppercase();

    println!(
        "{}  {:8} {} {:27} {}",
        pc_str, bytes_str, text_op, operands, registers
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_trace() {}
}
