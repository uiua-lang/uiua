use crate::viuer::error::ViuResult;
use crate::viuer::printer::{adjust_offset, find_best_fit, Printer};
use crate::viuer::Config;
use image::{DynamicImage, GenericImageView, ImageEncoder};
use lazy_static::lazy_static;
use std::{
    io::{BufReader, Read, Write},
    path::Path,
};

#[allow(non_camel_case_types)]
pub struct iTermPrinter;

lazy_static! {
    static ref ITERM_SUPPORT: bool = check_iterm_support();
}

/// Returns the terminal's support for the iTerm graphics protocol.
pub fn is_iterm_supported() -> bool {
    *ITERM_SUPPORT
}

impl Printer for iTermPrinter {
    fn print(
        &self,
        stdout: &mut impl Write,
        img: &DynamicImage,
        config: &Config,
    ) -> ViuResult<(u32, u32)> {
        let (width, height) = img.dimensions();

        // Transform the dynamic image to a PNG which can be given directly to iTerm
        let mut png_bytes: Vec<u8> = Vec::new();
        image::codecs::png::PngEncoder::new(&mut png_bytes).write_image(
            img.as_bytes(),
            width,
            height,
            img.color(),
        )?;

        print_buffer(stdout, img, &png_bytes[..], config)
    }

    fn print_from_file<P: AsRef<Path>>(
        &self,
        stdout: &mut impl Write,
        filename: P,
        config: &Config,
    ) -> ViuResult<(u32, u32)> {
        let file = std::fs::File::open(filename)?;

        // load the file content
        let mut buf_reader = BufReader::new(file);
        let mut file_content = Vec::new();
        buf_reader.read_to_end(&mut file_content)?;

        let img = image::load_from_memory(&file_content[..])?;
        print_buffer(stdout, &img, &file_content[..], config)
    }
}

// This function requires both a DynamicImage, which is used to calculate dimensions,
// and it's raw representation as a file, because that's the data iTerm needs to display it.
fn print_buffer(
    stdout: &mut impl Write,
    img: &DynamicImage,
    img_content: &[u8],
    config: &Config,
) -> ViuResult<(u32, u32)> {
    adjust_offset(stdout, config)?;

    let (w, h) = find_best_fit(img, config.width, config.height);

    writeln!(
        stdout,
        "\x1b]1337;File=inline=1;preserveAspectRatio=1;size={};width={};height={}:{}\x07",
        img_content.len(),
        w,
        h,
        base64::encode(img_content)
    )?;
    stdout.flush()?;

    Ok((w, h))
}

// Check if the iTerm protocol can be used
fn check_iterm_support() -> bool {
    if let Ok(term) = std::env::var("TERM_PROGRAM") {
        if term.contains("iTerm") || term.contains("WezTerm") || term.contains("mintty") {
            return true;
        }
    }
    if let Ok(lc_term) = std::env::var("LC_TERMINAL") {
        if lc_term.contains("iTerm") || lc_term.contains("WezTerm") || lc_term.contains("mintty") {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use image::GenericImage;

    #[test]
    fn test_print_e2e() {
        let mut img = DynamicImage::ImageRgba8(image::RgbaImage::new(2, 3));
        img.put_pixel(1, 2, image::Rgba([2, 4, 6, 8]));

        let config = Config {
            x: 4,
            y: 3,
            ..Default::default()
        };
        let mut vec = Vec::new();

        assert_eq!(iTermPrinter.print(&mut vec, &img, &config).unwrap(), (2, 2));
        assert_eq!(std::str::from_utf8(&vec).unwrap(), "\x1b[4;5H\x1b]1337;File=inline=1;preserveAspectRatio=1;size=71;width=2;height=2:iVBORw0KGgoAAAANSUhEUgAAAAIAAAADCAYAAAC56t6BAAAADklEQVR4AWPADphY2DgAAEMAFfOuwskAAAAASUVORK5CYII=\x07\n");
    }
}
