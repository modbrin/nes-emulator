//! This module contains logic for application startup

use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

use sdl2::{event::Event, keyboard::Keycode};

use device::Device;

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

/// panics on failure
fn read_rom_from_file(path: impl AsRef<Path>) -> Rom {
    let file = File::open(path).unwrap();
    let mut buf = BufReader::new(file);
    let mut rom_data = Vec::new();
    buf.read_to_end(&mut rom_data).unwrap();
    Rom::parse(rom_data).unwrap()
}

fn main() {
    let mut screen = Screen::init().unwrap();
    let texture_creator = screen.texture_creator();
    let mut texture = Screen::create_texture(&texture_creator).unwrap();

    let rom = read_rom_from_file("roms/pacman.nes");
    let bus = Bus::with_rom(rom);
    let mut device = Device::with_bus(bus);
    device.reset().unwrap();
    // device.cpu.pc = 0xC000; // no ppu mode

    let mut frame = Frame::new();

    device
        .run(
            move |device| {
                let ppu = &device.bus.ppu;
                ppu.extract_screen_state(&mut frame);
                texture.update(None, &frame.data, 256 * 3).unwrap();
                screen.canvas.copy(&texture, None, None).unwrap();
                screen.canvas.present();

                // trace(&device)
            },
            move |device| {
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
            },
        )
        .unwrap();
}
