/// Custom result type for error-prone operations
pub type ViuResult<T = ()> = std::result::Result<T, ViuError>;

/// Custom error enum for `viu`ing operations
#[derive(Debug)]
pub enum ViuError {
    /// Error while doing transformations with the [`image`] crate
    Image(image::ImageError),
    /// Error while doing IO operations
    Io(std::io::Error),
    /// Invalid configuration provided
    InvalidConfiguration(String),
    /// Error while creating temp files
    Tempfile(tempfile::PersistError),
    /// Errenous response received from Kitty
    KittyResponse(Vec<console::Key>),
    /// Kitty protocol not supported
    KittyNotSupported,
    /// Error while printing with sixel
    #[cfg(feature = "sixel")]
    SixelError(sixel_rs::status::Error),
}

impl std::error::Error for ViuError {}

impl From<std::io::Error> for ViuError {
    fn from(err: std::io::Error) -> Self {
        ViuError::Io(err)
    }
}
impl From<image::ImageError> for ViuError {
    fn from(err: image::ImageError) -> Self {
        ViuError::Image(err)
    }
}

impl From<tempfile::PersistError> for ViuError {
    fn from(err: tempfile::PersistError) -> Self {
        ViuError::Tempfile(err)
    }
}

#[cfg(feature = "sixel")]
impl From<sixel_rs::status::Error> for ViuError {
    fn from(e: sixel_rs::status::Error) -> Self {
        ViuError::SixelError(e)
    }
}

impl std::fmt::Display for ViuError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ViuError::Image(e) => write!(f, "Image error: {}", e),
            ViuError::Io(e) => write!(f, "IO error: {}", e),
            ViuError::InvalidConfiguration(s) => write!(f, "Invalid Configuration: {}", s),
            ViuError::Tempfile(e) => write!(f, "Tempfile error: {}", e),
            ViuError::KittyResponse(keys) => write!(f, "Kitty response: {:?}", keys),
            ViuError::KittyNotSupported => write!(f, "Kitty graphics protocol not supported"),
            #[cfg(feature = "sixel")]
            ViuError::SixelError(e) => write!(f, "Sixel error: {:?}", e),
        }
    }
}
