//! This module contains logic for application startup

use device::Device;

mod consts;
mod cpu;
mod device;
mod memory;
mod ppu;
mod util;

pub(crate) mod prelude {
    pub use crate::consts::*;
    pub use crate::device::Device;
    pub use crate::util::InstructionMetadata as Meta;
    pub use crate::util::*;
}

fn main() {
    Device::new().start().unwrap();
}
