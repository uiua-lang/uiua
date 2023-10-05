use std::env;

const DEFAULT_TERM_SIZE: (u16, u16) = (80, 24);

pub fn truecolor_available() -> bool {
    if let Ok(value) = env::var("COLORTERM") {
        value.contains("truecolor") || value.contains("24bit")
    } else {
        false
    }
}

/// Try to get the terminal size. If unsuccessful, fallback to a default (80x24).
///
/// Uses [crossterm::terminal::size].
/// ## Example
/// The example below prints "img.jpg" with dimensions 80x40 in the center of the terminal.
/// ```no_run
/// use viuer::{Config, print_from_file, terminal_size};
///
/// let (term_width, term_height) = terminal_size();
/// // Set desired image dimensions
/// let width = 80;
/// let height = 40;
///
/// let config = Config {
///     x: (term_width - width) / 2,
///     y: (term_height - height) as i16 / 2,
///     width: Some(width as u32),
///     height: Some(height as u32),
///     ..Default::default()
/// };
/// print_from_file("img.jpg", &config).expect("Image printing failed.");
/// ```
#[cfg(not(test))]
pub fn terminal_size() -> (u16, u16) {
    match crossterm::terminal::size() {
        Ok(s) => s,
        Err(_) => DEFAULT_TERM_SIZE,
    }
}

// Return a constant when running the tests
#[cfg(test)]
pub fn terminal_size() -> (u16, u16) {
    DEFAULT_TERM_SIZE
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_truecolor() {
        env::set_var("COLORTERM", "truecolor");
        assert!(truecolor_available());
        env::set_var("COLORTERM", "");
        assert!(!truecolor_available());
    }
}
