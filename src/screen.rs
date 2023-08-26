use sdl2::{
    EventPump,
    pixels::PixelFormatEnum,
    render::{Canvas, Texture, TextureCreator},
    Sdl, video::{Window, WindowContext}, VideoSubsystem,
};

use crate::prelude::*;

pub struct Screen {
    #[allow(unused)]
    sdl_context: Sdl,
    #[allow(unused)]
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
        let event_pump = sdl_context.event_pump().map_err(|e| format!("{e:?}"))?;
        canvas.set_scale(scale, scale)?;
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
        let texture = texture_creator
            .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
            .map_err(|e| format!("{e:?}"))?;
        Ok(texture)
    }
}
