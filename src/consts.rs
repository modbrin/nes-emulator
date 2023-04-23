


/// Carry Status Flag
pub const FLAG_CARRY: u8 = 1 << 0;
/// Zero Status Flag
pub const FLAG_ZERO: u8 = 1 << 1;
/// Interrupt Disable Status Flag
pub const FLAG_INTRDIS: u8 = 1 << 2;
/// Decimal Mode Status Flag
pub const FLAG_DECIMAL: u8 = 1 << 3;
/// Break Command Status Flag,
/// used in interrupt handling
pub const FLAG_B: u8 = 1 << 4;
/// Always set to 1, not modified
pub const FLAG_STARTER: u8 = 1 << 5;
/// Overflow Status Flag
pub const FLAG_ZERO: u8 = 1 << 6;
/// Negative Status Flag
pub const FLAG_ZERO: u8 = 1 << 7;
