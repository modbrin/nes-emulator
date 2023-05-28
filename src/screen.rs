use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    render::{Canvas, Texture, TextureCreator},
    video::{Window, WindowContext},
    EventPump, Sdl, VideoSubsystem,
};

use crate::prelude::*;

pub struct Screen {
    sdl_context: Sdl,
    video_subsystem: VideoSubsystem,
    pub canvas: Canvas<Window>,
    pub event_pump: EventPump,
}

impl Screen {
    pub fn init() -> Result<Self, String> {
        let sdl_context = sdl2::init()?;
        let video_subsystem = sdl_context.video()?;
        let scale: f32 = 3.0;
        let window = video_subsystem
            .window(
                "NES Emulator",
                (256.0 * scale) as u32,
                (240.0 * scale) as u32,
            )
            .position_centered()
            .build()
            .map_err(|e| format!("{e:?}"))?;

        let mut canvas = window
            .into_canvas()
            .present_vsync()
            .build()
            .map_err(|e| format!("{e:?}"))?;
        let mut event_pump = sdl_context.event_pump().map_err(|e| format!("{e:?}"))?;
        canvas.set_scale(scale, scale)?;
        // let texture_creator = canvas.texture_creator();
        // let mut texture = texture_creator.create_texture_target(PixelFormatEnum::RGB24, 256, 240).map_err(|e|format!("{e:?}"))?;
        let screen = Self {
            sdl_context,
            video_subsystem,
            canvas,
            event_pump,
        };
        Ok(screen)
    }

    pub fn texture_creator(&self) -> TextureCreator<WindowContext> {
        self.canvas.texture_creator()
    }

    pub fn create_texture(
        texture_creator: &TextureCreator<WindowContext>,
    ) -> Result<Texture<'_>, String> {
        texture_creator
            .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
            .map_err(|e| format!("{e:?}"))
    }
}
