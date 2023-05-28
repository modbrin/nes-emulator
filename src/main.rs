//! This module contains logic for application startup

use std::{
    fs::File,
    io::{BufReader, Read},
    ops::Not,
};

use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    render::{Canvas, Texture, TextureCreator},
    video::{Window, WindowContext},
    EventPump, Sdl, VideoSubsystem,
};

use device::Device;
use rand::Rng;

mod common;
mod consts;
mod cpu;
mod device;
mod memory;
mod ppu;
mod rom;
mod screen;

pub(crate) mod prelude {
    pub use crate::device::Device;

    pub use crate::common::*;
    pub use crate::consts::*;
    pub use crate::memory::*;
    pub use crate::rom::*;
    pub use crate::screen::*;
}
use prelude::*;

use crate::cpu::INST_TABLE;

// fn handle_user_input(device: &mut Device, event_pump: &mut EventPump) {
//     for event in event_pump.poll_iter() {
//         match event {
//             Event::Quit { .. }
//             | Event::KeyDown {
//                 keycode: Some(Keycode::Escape),
//                 ..
//             } => {
//                 std::process::exit(0);
//             }
//             Event::KeyDown {
//                 keycode: Some(Keycode::W),
//                 ..
//             } => {
//                 device.write_one(0xFF, 0x77).unwrap();
//             }
//             Event::KeyDown {
//                 keycode: Some(Keycode::S),
//                 ..
//             } => {
//                 device.write_one(0xFF, 0x73).unwrap();
//             }
//             Event::KeyDown {
//                 keycode: Some(Keycode::A),
//                 ..
//             } => {
//                 device.write_one(0xFF, 0x61).unwrap();
//             }
//             Event::KeyDown {
//                 keycode: Some(Keycode::D),
//                 ..
//             } => {
//                 device.write_one(0xFF, 0x64).unwrap();
//             }
//             _ => { /* do nothing */ }
//         }
//     }
// }

// fn color(byte: u8) -> Color {
//     match byte {
//         0 => Color::BLACK,
//         1 => Color::WHITE,
//         2 | 9 => Color::GREY,
//         3 | 10 => Color::RED,
//         4 | 11 => Color::GREEN,
//         5 | 12 => Color::BLUE,
//         6 | 13 => Color::MAGENTA,
//         7 | 14 => Color::YELLOW,
//         _ => Color::CYAN,
//     }
// }

// fn read_screen_state(device: &Device, frame: &mut [u8; 32 * 32 * 3]) -> bool {
//     let mut frame_idx = 0;
//     let mut update = false;
//     for i in 0x0200..0x0600 {
//         let color_idx = device.read_one(i as u16).unwrap();
//         let (b1, b2, b3) = color(color_idx).rgb();
//         if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
//             frame[frame_idx] = b1;
//             frame[frame_idx + 1] = b2;
//             frame[frame_idx + 2] = b3;
//             update = true;
//         }
//         frame_idx += 3;
//     }
//     update
// }

// fn show_tile(chr: &Vec<u8>, bank: usize) -> Frame {
//     assert!(bank <= 1);

//     let mut frame = Frame::new();
//     let bank = (bank * 0x1000) as usize;

//     for x_tile in 0..=20 {
//         for y_tile in 0..=11 {
//             let tile_n = y_tile * 20 + x_tile;
//             let x_offset = x_tile * 10;
//             let y_offset = y_tile * 10;

//             let tile = &chr[(bank + tile_n * 16)..=(bank + tile_n * 16 + 15)];
//             for y in 0..=7 {
//                 let mut upper = tile[y];
//                 let mut lower = tile[y + 8];

//                 for x in (0..=7).rev() {
//                     let value = (1 & upper) << 1 | (1 & lower);
//                     upper = upper >> 1;
//                     lower = lower >> 1;
//                     let rgb = match value {
//                         0 => ppu::PALLETE[0x01],
//                         1 => ppu::PALLETE[0x23],
//                         2 => ppu::PALLETE[0x27],
//                         3 => ppu::PALLETE[0x30],
//                         _ => unreachable!()
//                     };
//                     frame.set_pixel(x + x_offset, y + y_offset, PixelColor::from_tuple(rgb))
//                 }
//             }
//         }
//     }

//     frame
// }

fn main() {
    // let sdl_context = sdl2::init().unwrap();
    // let video_subsystem = sdl_context.video().unwrap();
    // let scale: f32 = 3.0;
    // let window = video_subsystem
    //     .window("NES Emulator", (256.0 * scale) as u32, (240.0 * scale) as u32)
    //     .position_centered()
    //     .build().unwrap();

    // let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    // let mut event_pump = sdl_context.event_pump().unwrap();
    // canvas.set_scale(scale, scale).unwrap();
    // let creator = canvas.texture_creator();
    // let mut texture = creator.create_texture_target(PixelFormatEnum::RGB24, 256, 240).unwrap();
    let mut screen = Screen::init().unwrap();
    let texture_creator = screen.texture_creator();
    let mut texture = Screen::create_texture(&texture_creator).unwrap();

    let file = File::open("roms/pacman.nes").unwrap();
    let mut buf = BufReader::new(file);
    let mut rom_data = Vec::new();
    buf.read_to_end(&mut rom_data).unwrap();

    let rom = Rom::parse(rom_data).unwrap();

    // let tile_frame = show_tile(&rom.chr_data, 0);
    // texture.update(None, &tile_frame.data, 256 * 3).unwrap();
    // canvas.copy(&texture, None, None).unwrap();
    // canvas.present();

    let mut frame = Frame::new();

    let bus = Bus::with_rom(rom);
    let mut device = Device::with_bus(bus);
    // let mut screen_state = [0 as u8; 32 * 32 * 3];
    // let mut rng = rand::thread_rng();
    device.reset().unwrap();
    // device.cpu.pc = 0xC000; // no ppu mode

    device
        .run(move |device| {
            // handle_user_input(device, &mut event_pump);
            // device.write_one(0xfe, rng.gen_range(1..16)).unwrap();
            // if read_screen_state(device, &mut screen_state) {
            //     texture.update(None, &screen_state, 32 * 3).unwrap();
            //     canvas.copy(&texture, None, None).unwrap();
            //     canvas.present();
            // }

            let ppu = &device.bus.ppu;
            ppu.extract_screen_state(&ppu.vram, &mut frame);
            texture.update(None, &frame.data, 256 * 3).unwrap();
            screen.canvas.copy(&texture, None, None).unwrap();
            screen.canvas.present();
            for event in screen.event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => std::process::exit(0),
                    _ => {}
                }
            }
            // trace(&device)
        })
        .unwrap();
}
