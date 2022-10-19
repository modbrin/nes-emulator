pub enum NesError {
    /// Attempt to access memory out of bounds
    RamOutOfBounds,
    /// Stack size limit exceeded
    StackOverflow,
}