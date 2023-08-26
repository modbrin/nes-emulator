//! This module contains logic for application startup

use std::{
    env,
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

/// panics on failure
fn read_rom_from_file(path: impl AsRef<Path>) -> Rom {
    let file = File::open(path).unwrap();
    let mut buf = BufReader::new(file);
    let mut rom_data = Vec::new();
    buf.read_to_end(&mut rom_data).unwrap();
    Rom::parse(rom_data).unwrap()
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let Some(rom_path) = args.get(1) else {
        println!("usage: nes-emulator path/to/rom.nes");
        return;
    };

    let mut screen = Screen::init().unwrap();
    let texture_creator = screen.texture_creator();
    let mut texture = Screen::create_texture(&texture_creator).unwrap();

    let rom = read_rom_from_file(rom_path);
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
                        Event::KeyDown { keycode, .. } => {
                            if let Some(btn) = map_controller_button(keycode.as_ref()) {
                                device.bus.controller.set_button(btn, true);
                            }
                        }
                        Event::KeyUp { keycode, .. } => {
                            if let Some(btn) = map_controller_button(keycode.as_ref()) {
                                device.bus.controller.set_button(btn, false);
                            }
                        }
                        _ => {}
                    }
                }
            },
        )
        .unwrap();
}
