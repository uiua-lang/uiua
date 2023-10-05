use crate::viuer::utils;

/// Configuration struct to customize printing behaviour.
pub struct Config {
    /// Enable true transparency instead of checkerboard background.
    /// Available only for the block printer. Defaults to false.
    pub transparent: bool,
    /// Make the x and y offset be relative to the top left terminal corner.
    /// If false, the y offset is relative to the cursor's position.
    /// Defaults to true.
    pub absolute_offset: bool,
    /// X offset. Defaults to 0.
    pub x: u16,
    /// Y offset. Can be negative only when `absolute_offset` is `false`. Defaults to 0.
    pub y: i16,
    /// Take a note of cursor position before printing and restore it when finished.
    /// Defaults to false.
    pub restore_cursor: bool,
    /// Optional image width. Defaults to None.
    pub width: Option<u32>,
    /// Optional image height. Defaults to None.
    pub height: Option<u32>,
    /// Use truecolor if the terminal supports it. Defaults to true.
    pub truecolor: bool,
    /// Use Kitty protocol if the terminal supports it. Defaults to true.
    pub use_kitty: bool,
    /// Use iTerm protocol if the terminal supports it. Defaults to true.
    pub use_iterm: bool,
    /// Use Sixel protocol if the terminal supports it. Defaults to true.
    #[cfg(feature = "sixel")]
    pub use_sixel: bool,
}

impl std::default::Default for Config {
    fn default() -> Self {
        Self {
            transparent: false,
            absolute_offset: true,
            x: 0,
            y: 0,
            restore_cursor: false,
            width: None,
            height: None,
            truecolor: utils::truecolor_available(),
            use_kitty: true,
            use_iterm: true,
            #[cfg(feature = "sixel")]
            use_sixel: true,
        }
    }
}
