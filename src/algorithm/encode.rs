//! En/decode Uiua arrays to/from media formats

#[cfg(feature = "audio_encode")]
use hound::{SampleFormat, WavReader, WavSpec, WavWriter};
#[cfg(feature = "image")]
use image::{DynamicImage, ImageOutputFormat};
use serde::*;

use crate::SysBackend;
#[allow(unused_imports)]
use crate::{Array, Uiua, UiuaResult, Value};

/// Conversion of a value to some media format based on the value's shape
#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum SmartOutput {
    Normal(Value),
    Png(Vec<u8>, Option<String>),
    Gif(Vec<u8>, Option<String>),
    Wav(Vec<u8>, Option<String>),
}

impl SmartOutput {
    /// Convert a value to a SmartOutput
    pub fn from_value(value: Value, backend: &dyn SysBackend) -> Self {
        // Try to convert the value to audio
        #[cfg(feature = "audio_encode")]
        if value.shape().last().is_some_and(|&n| n >= 44100 / 4)
            && matches!(&value, Value::Num(arr) if arr.elements().all(|x| x.abs() <= 5.0))
        {
            if let Ok(bytes) = value_to_wav_bytes(&value, backend.audio_sample_rate()) {
                let label = value.meta().label.as_ref().map(Into::into);
                return Self::Wav(bytes, label);
            }
        }
        // Try to convert the value to an image
        const MIN_AUTO_IMAGE_DIM: usize = 30;
        #[cfg(feature = "image")]
        if let Ok(image) = value_to_image(&value) {
            if image.width() >= MIN_AUTO_IMAGE_DIM as u32
                && image.height() >= MIN_AUTO_IMAGE_DIM as u32
            {
                if let Ok(bytes) = image_to_bytes(&image, ImageOutputFormat::Png) {
                    let label = value.meta().label.as_ref().map(Into::into);
                    return Self::Png(bytes, label);
                }
            }
        }
        // Try to convert the value to a gif
        #[cfg(feature = "gif")]
        if let Ok(bytes) = value_to_gif_bytes(&value, 16.0) {
            match value.shape().dims() {
                &[f, h, w] | &[f, h, w, _]
                    if h >= MIN_AUTO_IMAGE_DIM && w >= MIN_AUTO_IMAGE_DIM && f >= 5 =>
                {
                    let label = value.meta().label.as_ref().map(Into::into);
                    return Self::Gif(bytes, label);
                }
                _ => {}
            }
        }
        // Otherwise, just show the value
        Self::Normal(value)
    }
}

pub(crate) fn image_encode(env: &mut Uiua) -> UiuaResult {
    #[cfg(feature = "image")]
    {
        let format = env
            .pop(1)?
            .as_string(env, "Image format must be a string")?;
        let value = env.pop(2)?;
        let output_format = match format.as_str() {
            "jpg" | "jpeg" => ImageOutputFormat::Jpeg(100),
            "png" => ImageOutputFormat::Png,
            "bmp" => ImageOutputFormat::Bmp,
            "gif" => ImageOutputFormat::Gif,
            "ico" => ImageOutputFormat::Ico,
            "qoi" => ImageOutputFormat::Qoi,
            "webp" => ImageOutputFormat::WebP,
            format => return Err(env.error(format!("Invalid image format: {}", format))),
        };
        let bytes =
            crate::encode::value_to_image_bytes(&value, output_format).map_err(|e| env.error(e))?;
        env.push(Array::<u8>::from(bytes.as_slice()));
        Ok(())
    }
    #[cfg(not(feature = "image"))]
    Err(env.error("Image encoding is not supported in this environment"))
}

pub(crate) fn image_decode(env: &mut Uiua) -> UiuaResult {
    #[cfg(feature = "image")]
    {
        let bytes: crate::cowslice::CowSlice<u8> = match env.pop(1)? {
            Value::Byte(arr) => {
                if arr.rank() != 1 {
                    return Err(env.error(format!(
                        "Image bytes array must be rank 1, but is rank {}",
                        arr.rank()
                    )));
                }
                arr.data
            }
            Value::Num(arr) => {
                if arr.rank() != 1 {
                    return Err(env.error(format!(
                        "Image bytes array must be rank 1, but is rank {}",
                        arr.rank()
                    )));
                }
                arr.data.iter().map(|&x| x as u8).collect()
            }
            _ => return Err(env.error("Image bytes must be a numeric array")),
        };
        let format = image::guess_format(&bytes)
            .map_err(|e| env.error(format!("Failed to read image: {e}")))?;
        let array = crate::encode::image_bytes_to_array(&bytes, true).map_err(|e| env.error(e))?;
        env.push(array);
        env.push(match format {
            image::ImageFormat::Jpeg => "jpeg".into(),
            fmt => format!("{fmt:?}").to_lowercase(),
        });
        Ok(())
    }
    #[cfg(not(feature = "image"))]
    Err(env.error("Image decoding is not supported in this environment"))
}

pub(crate) fn gif_encode(env: &mut Uiua) -> UiuaResult {
    #[cfg(feature = "gif")]
    {
        let delay = env.pop(1)?.as_num(env, "Delay must be a number")?;
        let value = env.pop(2)?;
        let bytes = crate::encode::value_to_gif_bytes(&value, delay).map_err(|e| env.error(e))?;
        env.push(Array::<u8>::from(bytes.as_slice()));
        Ok(())
    }
    #[cfg(not(feature = "gif"))]
    Err(env.error("GIF encoding is not supported in this environment"))
}

pub(crate) fn gif_decode(env: &mut Uiua) -> UiuaResult {
    #[cfg(feature = "gif")]
    {
        let bytes = env
            .pop(1)?
            .as_bytes(env, "Gif bytes must be a byte array")?;
        let (frame_rate, value) =
            crate::encode::gif_bytes_to_value(&bytes).map_err(|e| env.error(e))?;
        env.push(value);
        env.push(frame_rate);
        Ok(())
    }
    #[cfg(not(feature = "gif"))]
    Err(env.error("GIF encoding is not supported in this environment"))
}

pub(crate) fn audio_encode(env: &mut Uiua) -> UiuaResult {
    #[cfg(feature = "audio_encode")]
    {
        let format = env
            .pop(1)?
            .as_string(env, "Audio format must be a string")?;

        const SAMPLE_RATE_REQUIREMENT: &str = "Audio sample rate must be a positive integer";
        let sample_rate = u32::try_from(env.pop(2)?.as_nat(env, SAMPLE_RATE_REQUIREMENT)?)
            .map_err(|_| env.error("Audio sample rate is too high"))?;
        if sample_rate == 0 {
            return Err(env.error(format!("{SAMPLE_RATE_REQUIREMENT}, but it is zero")));
        }

        let value = env.pop(3)?;
        let bytes = match format.as_str() {
            "wav" => {
                crate::encode::value_to_wav_bytes(&value, sample_rate).map_err(|e| env.error(e))?
            }
            format => {
                return Err(env.error(format!("Invalid or unsupported audio format: {format}")))
            }
        };
        env.push(Array::<u8>::from(bytes.as_slice()));
        Ok(())
    }
    #[cfg(not(feature = "audio_encode"))]
    Err(env.error("Audio encoding is not supported in this environment"))
}

pub(crate) fn audio_decode(env: &mut Uiua) -> UiuaResult {
    #[cfg(feature = "audio_encode")]
    {
        let bytes: crate::cowslice::CowSlice<u8> = match env.pop(1)? {
            Value::Byte(arr) => {
                if arr.rank() != 1 {
                    return Err(env.error(format!(
                        "Audio bytes array must be rank 1, but is rank {}",
                        arr.rank()
                    )));
                }
                arr.data
            }
            Value::Num(arr) => {
                if arr.rank() != 1 {
                    return Err(env.error(format!(
                        "Audio bytes array must be rank 1, but is rank {}",
                        arr.rank()
                    )));
                }
                arr.data.iter().map(|&x| x as u8).collect()
            }
            _ => return Err(env.error("Audio bytes must be a numeric array")),
        };
        let (array, sample_rate) =
            crate::encode::array_from_wav_bytes(&bytes).map_err(|e| env.error(e))?;
        env.push(array);
        env.push(sample_rate as usize);
        env.push("wav");
        Ok(())
    }
    #[cfg(not(feature = "audio_encode"))]
    Err(env.error("Audio decoding is not supported in this environment"))
}

#[doc(hidden)]
#[cfg(feature = "image")]
pub fn value_to_image_bytes(value: &Value, format: ImageOutputFormat) -> Result<Vec<u8>, String> {
    image_to_bytes(&value_to_image(value)?, format)
}

#[doc(hidden)]
#[cfg(feature = "image")]
pub fn rgb_image_to_array(image: image::RgbImage) -> Array<f64> {
    let shape = crate::Shape::from([image.height() as usize, image.width() as usize, 3]);
    Array::new(
        shape,
        (image.into_raw().into_iter())
            .map(|b| b as f64 / 255.0)
            .collect::<crate::cowslice::CowSlice<_>>(),
    )
}

#[doc(hidden)]
#[cfg(feature = "image")]
pub fn rgba_image_to_array(image: image::RgbaImage) -> Array<f64> {
    let shape = crate::Shape::from([image.height() as usize, image.width() as usize, 4]);
    Array::new(
        shape,
        (image.into_raw().into_iter())
            .map(|b| b as f64 / 255.0)
            .collect::<crate::cowslice::CowSlice<_>>(),
    )
}

#[doc(hidden)]
#[cfg(feature = "image")]
pub fn image_bytes_to_array(bytes: &[u8], alpha: bool) -> Result<Array<f64>, String> {
    Ok(if alpha {
        let image = image::load_from_memory(bytes)
            .map_err(|e| format!("Failed to read image: {}", e))?
            .into_rgba8();
        let shape = crate::Shape::from([image.height() as usize, image.width() as usize, 4]);
        Array::new(
            shape,
            (image.into_raw().into_iter())
                .map(|b| b as f64 / 255.0)
                .collect::<crate::cowslice::CowSlice<_>>(),
        )
    } else {
        let image = image::load_from_memory(bytes)
            .map_err(|e| format!("Failed to read image: {}", e))?
            .into_rgb8();
        rgb_image_to_array(image)
    })
}

#[doc(hidden)]
#[cfg(feature = "image")]
pub fn image_to_bytes(image: &DynamicImage, format: ImageOutputFormat) -> Result<Vec<u8>, String> {
    let mut bytes = std::io::Cursor::new(Vec::new());
    image
        .write_to(&mut bytes, format)
        .map_err(|e| format!("Failed to write image: {e}"))?;
    Ok(bytes.into_inner())
}

#[doc(hidden)]
#[cfg(feature = "image")]
pub fn value_to_image(value: &Value) -> Result<DynamicImage, String> {
    if ![2, 3].contains(&value.rank()) {
        return Err(format!(
            "Image must be a rank 2 or 3 numeric array, but it is a rank-{} {} array",
            value.rank(),
            value.type_name()
        ));
    }
    let bytes = match value {
        Value::Num(nums) => nums.data.iter().map(|f| (*f * 255.0) as u8).collect(),
        Value::Byte(bytes) => bytes.data.iter().map(|&b| (b > 0) as u8 * 255).collect(),
        _ => return Err("Image must be a numeric array".into()),
    };
    #[allow(clippy::match_ref_pats)]
    let [height, width, px_size] = match value.shape().dims() {
        &[a, b] => [a, b, 1],
        &[a, b, c] => [a, b, c],
        _ => unreachable!("Shape checked above"),
    };
    Ok(match px_size {
        1 => image::GrayImage::from_raw(width as u32, height as u32, bytes)
            .ok_or("Failed to create image")?
            .into(),
        2 => image::GrayAlphaImage::from_raw(width as u32, height as u32, bytes)
            .ok_or("Failed to create image")?
            .into(),
        3 => image::RgbImage::from_raw(width as u32, height as u32, bytes)
            .ok_or("Failed to create image")?
            .into(),
        4 => image::RgbaImage::from_raw(width as u32, height as u32, bytes)
            .ok_or("Failed to create image")?
            .into(),
        n => {
            return Err(format!(
                "For a color image, the last dimension of the image array must be between 1 and 4 but it is {n}"
            ))
        }
    })
}

#[doc(hidden)]
pub fn value_to_sample(audio: &Value) -> Result<Vec<[f32; 2]>, String> {
    let unrolled: Vec<f32> = match audio {
        Value::Num(nums) => nums.data.iter().map(|&f| f as f32).collect(),
        Value::Byte(byte) => byte.data.iter().map(|&b| b as f32).collect(),
        _ => return Err("Audio must be a numeric array".into()),
    };
    let (length, mut channels) = match audio.rank() {
        1 => (unrolled.len(), vec![unrolled]),
        2 => (
            audio.row_len(),
            unrolled
                .chunks_exact(audio.row_len().max(1))
                .map(|c| c.to_vec())
                .collect(),
        ),
        n => {
            return Err(format!(
                "Audio must be a rank 1 or 2 numeric array, but it is rank {n}"
            ))
        }
    };
    if channels.is_empty() {
        channels.push(vec![0.0; length]);
    }
    let mut sterio = Vec::new();
    if channels.len() == 1 {
        for sample in channels.into_iter().next().unwrap() {
            sterio.push([sample, sample]);
        }
    } else {
        for i in 0..length {
            let left = channels[0][i];
            let right = channels[1][i];
            sterio.push([left, right]);
        }
    }
    Ok(sterio)
}

#[doc(hidden)]
pub fn value_to_audio_channels(audio: &Value) -> Result<Vec<Vec<f64>>, String> {
    let interleaved: Vec<f64> = match audio {
        Value::Num(nums) => nums.data.iter().copied().collect(),
        Value::Byte(byte) => byte.data.iter().map(|&b| b as f64).collect(),
        _ => return Err("Audio must be a numeric array".into()),
    };
    let (length, mut channels) = match audio.rank() {
        1 => (interleaved.len(), vec![interleaved]),
        2 => (
            audio.row_len(),
            interleaved
                .chunks_exact(audio.row_len().max(1))
                .map(|c| c.to_vec())
                .collect(),
        ),
        n => {
            return Err(format!(
                "Audio must be a rank 1 or 2 numeric array, but it is rank {n}"
            ))
        }
    };
    if channels.len() > 5 {
        return Err(format!(
            "Audio can have at most 5 channels, but its shape is {}",
            audio.shape()
        ));
    }

    if channels.is_empty() {
        channels.push(vec![0.0; length]);
    }
    Ok(channels)
}

#[doc(hidden)]
#[cfg(feature = "audio_encode")]
pub fn value_to_wav_bytes(audio: &Value, sample_rate: u32) -> Result<Vec<u8>, String> {
    if sample_rate == 0 {
        return Err("Sample rate must not be 0".to_string());
    }
    let channels = value_to_audio_channels(audio)?;
    #[cfg(not(feature = "audio"))]
    {
        channels_to_wav_bytes_impl(
            channels,
            |f| (f * i16::MAX as f64) as i16,
            16,
            SampleFormat::Int,
            sample_rate,
        )
    }
    #[cfg(feature = "audio")]
    {
        channels_to_wav_bytes_impl(channels, |f| f as f32, 32, SampleFormat::Float, sample_rate)
    }
}

#[cfg(feature = "audio_encode")]
fn channels_to_wav_bytes_impl<T: hound::Sample + Copy>(
    channels: Vec<Vec<f64>>,
    convert_samples: impl Fn(f64) -> T + Copy,
    bits_per_sample: u16,
    sample_format: SampleFormat,
    sample_rate: u32,
) -> Result<Vec<u8>, String> {
    // We use i16 samples for compatibility with Firefox (if I remember correctly)
    let channels: Vec<Vec<T>> = channels
        .into_iter()
        .map(|c| c.into_iter().map(convert_samples).collect())
        .collect();
    let spec = WavSpec {
        channels: channels.len() as u16,
        sample_rate,
        bits_per_sample,
        sample_format,
    };
    let mut bytes = std::io::Cursor::new(Vec::new());
    let mut writer = WavWriter::new(&mut bytes, spec).map_err(|e| e.to_string())?;
    for i in 0..channels[0].len() {
        for channel in &channels {
            writer
                .write_sample(channel[i])
                .map_err(|e| format!("Failed to write audio: {e}"))?;
        }
    }
    writer
        .finalize()
        .map_err(|e| format!("Failed to finalize audio: {e}"))?;
    Ok(bytes.into_inner())
}

#[doc(hidden)]
#[cfg(feature = "audio_encode")]
pub fn stereo_to_wave_bytes<T: hound::Sample + Copy>(
    samples: &[[f64; 2]],
    convert_samples: impl Fn(f64) -> T + Copy,
    bits_per_sample: u16,
    sample_format: SampleFormat,
    sample_rate: u32,
) -> Result<Vec<u8>, String> {
    let spec = WavSpec {
        channels: 2,
        sample_rate,
        bits_per_sample,
        sample_format,
    };
    let mut bytes = std::io::Cursor::new(Vec::new());
    let mut writer = WavWriter::new(&mut bytes, spec).map_err(|e| e.to_string())?;
    for frame in samples {
        for sample in frame {
            writer
                .write_sample(convert_samples(*sample))
                .map_err(|e| format!("Failed to write audio: {e}"))?;
        }
    }
    writer
        .finalize()
        .map_err(|e| format!("Failed to finalize audio: {e}"))?;
    Ok(bytes.into_inner())
}

#[cfg(feature = "audio_encode")]
#[doc(hidden)]
pub fn array_from_wav_bytes(bytes: &[u8]) -> Result<(Array<f64>, u32), String> {
    let mut reader: WavReader<std::io::Cursor<&[u8]>> =
        WavReader::new(std::io::Cursor::new(bytes)).map_err(|e| e.to_string())?;
    let spec = reader.spec();
    match (spec.sample_format, spec.bits_per_sample) {
        (SampleFormat::Int, 16) => {
            array_from_wav_bytes_impl::<i16>(&mut reader, |i| i as f64 / i16::MAX as f64)
        }
        (SampleFormat::Int, 32) => {
            array_from_wav_bytes_impl::<i32>(&mut reader, |i| i as f64 / i32::MAX as f64)
        }
        (SampleFormat::Float, 32) => array_from_wav_bytes_impl::<f32>(&mut reader, |f| f as f64),
        (sample_format, bits_per_sample) => Err(format!(
            "Unsupported sample format: {:?} {} bits per sample",
            sample_format, bits_per_sample
        )),
    }
}

#[cfg(feature = "audio_encode")]
fn array_from_wav_bytes_impl<T: hound::Sample>(
    reader: &mut WavReader<std::io::Cursor<&[u8]>>,
    sample_to_f64: impl Fn(T) -> f64,
) -> Result<(Array<f64>, u32), String> {
    let channel_count = reader.spec().channels as usize;
    let mut channels = vec![ecow::EcoVec::new(); channel_count];
    let mut curr_channel = 0;
    for sample in reader.samples::<T>() {
        let sample = sample.map_err(|e| e.to_string())?;
        channels[curr_channel].push(sample_to_f64(sample));
        curr_channel = (curr_channel + 1) % channel_count;
    }

    let sample_rate = reader.spec().sample_rate;
    Ok(if channel_count == 1 {
        let channel = channels.pop().unwrap();
        (channel.into(), sample_rate)
    } else {
        let arr = Array::from_row_arrays_infallible(channels.into_iter().map(|ch| ch.into()));
        (arr, sample_rate)
    })
}

#[doc(hidden)]
#[cfg(feature = "gif")]
pub fn value_to_gif_bytes(value: &Value, frame_rate: f64) -> Result<Vec<u8>, String> {
    use std::collections::{HashMap, HashSet};

    use color_quant::NeuQuant;
    use gif::{DisposalMethod, Frame};
    use image::Rgba;

    if value.row_count() == 0 {
        return Err("Cannot convert empty array into GIF".into());
    }
    let mut frames = Vec::with_capacity(value.row_count());
    let mut width = 0;
    let mut height = 0;
    for row in value.rows() {
        let image = value_to_image(&row)?.into_rgba8();
        width = image.width();
        height = image.height();
        frames.push(image);
    }
    if width > u16::MAX as u32 || height > u16::MAX as u32 {
        return Err(format!(
            "GIF dimensions must be at most {}x{}, but the frames are {}x{}",
            u16::MAX,
            u16::MAX,
            width,
            height
        ));
    }
    let mut bytes = std::io::Cursor::new(Vec::new());
    let mut opaque_colors = HashSet::new();
    for frame in &frames {
        for &Rgba([r, g, b, a]) in frame.pixels() {
            if a > 0 {
                opaque_colors.insert([r, g, b]);
            }
        }
    }
    let mut opaque_colors = opaque_colors.into_iter().collect::<Vec<_>>();
    opaque_colors.sort_unstable();
    let mut reduced_colors = HashSet::new();
    let mut color_reduction = HashMap::new();
    if opaque_colors.len() <= 255 {
        for color in opaque_colors {
            reduced_colors.insert(color);
            color_reduction.insert(color, color);
        }
    } else {
        let opaque_data: Vec<u8> = opaque_colors
            .iter()
            .flat_map(|c| c.iter().copied().chain([128]))
            .collect();
        let nq = NeuQuant::new(10, 255, &opaque_data);
        let map = nq.color_map_rgb();
        for color in opaque_colors {
            let index = nq.index_of(&[color[0], color[1], color[2], 128]);
            let start = index * 3;
            let reduced = [map[start], map[start + 1], map[start + 2]];
            reduced_colors.insert(reduced);
            color_reduction.insert(color, reduced);
        }
    }
    let mut reduced_colors = reduced_colors.into_iter().collect::<Vec<_>>();
    reduced_colors.sort_unstable();
    let mut palette = Vec::with_capacity(reduced_colors.len() * 3);
    let mut color_map: HashMap<[u8; 3], u8> = HashMap::new();
    for color in reduced_colors {
        color_map.insert(color, (palette.len() / 3) as u8);
        palette.extend(color);
    }
    let transparent_index = color_map.len() as u8;
    palette.extend([0; 3]);
    let mut encoder = gif::Encoder::new(&mut bytes, width as u16, height as u16, &palette)
        .map_err(|e| e.to_string())?;
    const MIN_FRAME_RATE: f64 = 1.0 / 60.0;
    let delay = ((1.0 / frame_rate.max(MIN_FRAME_RATE)).abs() * 100.0) as u16;
    encoder
        .set_repeat(gif::Repeat::Infinite)
        .map_err(|e| e.to_string())?;
    for image in frames {
        let mut has_transparent = false;
        let indices: Vec<u8> = image
            .as_raw()
            .chunks_exact(4)
            .map(|chunk| {
                if chunk[3] == 0 {
                    has_transparent = true;
                    return transparent_index;
                }
                let color = [chunk[0], chunk[1], chunk[2]];
                let reduced = color_reduction[&color];
                color_map[&reduced]
            })
            .collect();
        let dispose = if has_transparent {
            DisposalMethod::Previous
        } else {
            DisposalMethod::Any
        };
        let mut frame = Frame {
            dispose,
            ..Frame::from_indexed_pixels(
                width as u16,
                height as u16,
                indices,
                Some(transparent_index),
            )
        };
        frame.delay = delay;
        encoder.write_frame(&frame).map_err(|e| e.to_string())?;
    }
    drop(encoder);
    Ok(bytes.into_inner())
}

#[doc(hidden)]
#[cfg(feature = "gif")]
pub fn gif_bytes_to_value(bytes: &[u8]) -> Result<(f64, Value), gif::DecodingError> {
    let mut decoder = gif::DecodeOptions::new();
    decoder.set_color_output(gif::ColorOutput::RGBA);
    let mut decoder = decoder.read_info(bytes)?;
    let first_frame = decoder.read_next_frame()?.unwrap();
    let gif_width = first_frame.width as usize;
    let gif_height = first_frame.height as usize;
    let mut data: crate::cowslice::CowSlice<f64> = Default::default();
    let mut frame_count = 1;
    let mut delay_sum = first_frame.delay as f64 / 100.0;
    // Init frame data with the first frame
    let mut frame_data = first_frame.buffer.to_vec();
    data.extend(frame_data.iter().map(|b| *b as f64 / 255.0));
    // Loop through the rest of the frames
    while let Some(frame) = decoder.read_next_frame()? {
        let frame_width = frame.width as usize;
        let frame_height = frame.height as usize;
        // Some frames may have different dimensions than the GIF
        if frame_width == gif_width && frame_height == gif_height {
            frame_data.copy_from_slice(&frame.buffer);
        } else {
            // Copy the frame into the correct position in the GIF
            let frame_left = frame.left as usize;
            let frame_top = frame.top as usize;
            for dy in 0..frame_height {
                let y = frame_top + dy;
                for dx in 0..frame_width {
                    let x = frame_left + dx;
                    let outer_i = (y * gif_width + x) * 4;
                    let inner_i = (dy * frame_width + dx) * 4;
                    frame_data[outer_i..][..4].copy_from_slice(&frame.buffer[inner_i..][..4]);
                }
            }
        }
        data.extend(frame_data.iter().map(|b| *b as f64 / 255.0));
        frame_count += 1;
        delay_sum += frame.delay as f64 / 100.0;
    }
    let avg_delay = delay_sum / frame_count as f64;
    let frame_rate = 1.0 / avg_delay;
    let shape = crate::Shape::from_iter([frame_count, gif_height, gif_width, 4]);
    let mut num = Value::Num(Array::new(shape, data));
    num.compress();
    Ok((frame_rate, num))
}

pub(crate) fn layout_text(options: Value, text: Value, env: &Uiua) -> UiuaResult<Value> {
    #[cfg(feature = "font_shaping")]
    {
        layout_text_impl(options, text, env)
    }
    #[cfg(not(feature = "font_shaping"))]
    Err(env.error("Text layout is not supported in this environment"))
}

#[cfg(feature = "font_shaping")]
fn layout_text_impl(options: Value, text: Value, env: &Uiua) -> UiuaResult<Value> {
    use std::{cell::RefCell, iter::repeat};

    use cosmic_text::*;
    use ecow::eco_vec;

    use crate::{
        algorithm::{validate_size, FillContext},
        grid_fmt::GridFmt,
        Boxed, Shape,
    };
    struct FontStuff {
        system: FontSystem,
        swash_cache: SwashCache,
    }
    thread_local! {
        static FONT_STUFF: RefCell<Option<FontStuff>> = const { RefCell::new(None) };
    }

    let mut string = String::new();
    match text {
        Value::Char(arr) if arr.rank() <= 1 => string = arr.data.iter().copied().collect(),
        Value::Char(arr) if arr.rank() == 2 => {
            for (i, row) in arr.row_slices().enumerate() {
                if i > 0 {
                    string.push('\n');
                }
                string.extend(row.iter().copied());
            }
        }
        Value::Box(arr) if arr.rank() == 1 => {
            for (i, Boxed(val)) in arr.data.iter().enumerate() {
                if i > 0 {
                    string.push('\n');
                }
                match val {
                    Value::Char(arr) if arr.rank() <= 1 => string.extend(arr.data.iter().copied()),
                    Value::Char(arr) if arr.rank() == 2 => {
                        for (j, row) in arr.row_slices().enumerate() {
                            if j > 0 {
                                string.push(' ');
                            }
                            string.extend(row.iter().copied());
                        }
                    }
                    Value::Box(arr) if arr.rank() == 1 => {
                        for (j, Boxed(val)) in arr.data.iter().enumerate() {
                            if j > 0 {
                                string.push(' ');
                            }
                            string.push_str(&val.as_string(env, "Text word must be a string")?);
                        }
                    }
                    _ => string.push_str(&val.as_string(env, "Text line must be a string")?),
                }
            }
        }
        Value::Box(arr) if arr.rank() == 2 => {
            for (i, row) in arr.row_slices().enumerate() {
                if i > 0 {
                    string.push('\n');
                }
                for (j, Boxed(val)) in row.iter().enumerate() {
                    if j > 0 {
                        string.push(' ');
                    }
                    string.push_str(&val.as_string(env, "Text word must be a string")?);
                }
            }
        }
        val => {
            string = val.as_string(env, "Text must be a rank 0, 1, or 2 character or box array")?
        }
    }

    // Default options
    let mut size = 30.0;
    let mut line_height = 1.0;
    let mut width = None;
    let mut height = None;
    let mut color: Option<Color> = None;

    // Parse options
    let mut scalar_index = 0;
    let mut set_size = false;
    for (i, row) in options.into_rows().map(Value::unboxed).enumerate() {
        let nums = row.as_nums(env, "Options must be numbers")?;
        match &**row.shape() {
            [] => {
                match scalar_index {
                    0 => {
                        size = nums[0] as f32;
                        if size <= 0.0 {
                            return Err(env.error("Text size must be positive"));
                        }
                    }
                    1 => {
                        line_height = nums[0] as f32;
                        if line_height <= 0.0 {
                            return Err(env.error("Line height must be positive"));
                        }
                    }
                    n => {
                        return Err(env.error(format!(
                            "{} is too many scalar options to layout text",
                            n + 1
                        )))
                    }
                }
                scalar_index += 1;
            }
            [2] => {
                if set_size {
                    return Err(env.error("Cannot set text layout size twice"));
                }
                let h = nums[0];
                let w = nums[1];
                if w < 0.0 || w.is_nan() {
                    return Err(env.error(format!(
                        "Canvas width must be a non-negative number, but it is {}",
                        w.grid_string(false)
                    )));
                }
                if h < 0.0 || h.is_nan() {
                    return Err(env.error(format!(
                        "Canvas height must be a non-negative number, but it is {}",
                        h.grid_string(false)
                    )));
                }
                if !w.is_infinite() {
                    width = Some(w as f32);
                }
                if !h.is_infinite() {
                    height = Some(h as f32);
                }
                set_size = true;
            }
            [3] => {
                if color.is_some() {
                    return Err(env.error("Cannot set text layout color twice"));
                }
                color = Some(Color::rgb(
                    (nums[0] * 255.0) as u8,
                    (nums[1] * 255.0) as u8,
                    (nums[2] * 255.0) as u8,
                ));
            }
            [4] => {
                if color.is_some() {
                    return Err(env.error("Cannot set text layout color twice"));
                }
                color = Some(Color::rgba(
                    (nums[0] * 255.0) as u8,
                    (nums[1] * 255.0) as u8,
                    (nums[2] * 255.0) as u8,
                    (nums[3] * 255.0) as u8,
                ))
            }
            _ => {
                return Err(env.error(format!(
                    "Layout options must have [], [2], [3], or [4]\
                    but option {i} has shape {}",
                    row.shape()
                )))
            }
        }
    }

    line_height *= size;
    let metrics = Metrics::new(size, line_height);

    FONT_STUFF.with(|stuff| -> UiuaResult<Value> {
        let mut stuff = stuff.borrow_mut();
        if stuff.is_none() {
            let mut db = fontdb::Database::new();
            db.load_system_fonts();
            db.set_monospace_family("Uiua386");
            db.set_sans_serif_family("Uiua386");
            db.load_font_data(include_bytes!("Uiua386.ttf").to_vec());
            let locale = sys_locale::get_locale().unwrap_or_else(|| "en-US".into());
            let system = FontSystem::new_with_locale_and_db(locale, db);
            *stuff = Some(FontStuff {
                system,
                swash_cache: SwashCache::new(),
            });
        }
        let FontStuff {
            system,
            swash_cache,
        } = stuff.as_mut().unwrap();
        // Init buffer
        let mut buffer = Buffer::new(system, metrics);
        let mut buffer = buffer.borrow_with(system);
        buffer.set_size(width, height);
        let attrs = Attrs::new();
        buffer.set_text(&string, attrs, Shaping::Advanced);
        buffer.shape_until_scroll(true);

        // Get canvas size
        let canvas_width = width.unwrap_or_else(|| {
            buffer
                .layout_runs()
                .map(|run| run.line_w)
                .max_by(|a, b| a.partial_cmp(b).unwrap())
                .unwrap_or(0.0)
        });
        let canvas_height =
            height.unwrap_or_else(|| buffer.layout_runs().map(|run| run.line_height).sum::<f32>());

        // Init array shape/data
        let fill = env.array_fill::<f64>();
        let colored = color.is_some() || fill.is_ok();
        let pixel_shape: &[usize] = if colored { &[4] } else { &[] };
        let mut canvas_shape = Shape::from_iter([canvas_height as usize, canvas_width as usize]);
        canvas_shape.extend(pixel_shape.iter().copied());
        let elem_count = validate_size::<f64>(canvas_shape.iter().copied(), env)?;
        let mut canvas_data = if let Ok(fill) = fill {
            let color = match &*fill.shape {
                [] | [1] => [fill.data[0], fill.data[0], fill.data[0], 1.0],
                [3] | [4] => [
                    fill.data[0],
                    fill.data[1],
                    fill.data[2],
                    fill.data.get(3).copied().unwrap_or(1.0),
                ],
                _ => return Err(env.error("Fill color must be a list of 3 or 4 numbers")),
            };
            repeat(color).take(elem_count / 4).flatten().collect()
        } else {
            eco_vec![0.0; elem_count]
        };
        let slice = canvas_data.make_mut();
        // Render
        let color = color.unwrap_or(Color::rgb(0xFF, 0xFF, 0xFF));
        if color.a() == 0 {
            return Ok(Array::new(canvas_shape, canvas_data).into());
        }
        let a = color.a() as f64 / 255.0;
        buffer.draw(swash_cache, color, |x, y, _, _, color| {
            let alpha = color.a();
            if alpha == 0
                || (x < 0 || x >= canvas_width as i32)
                || (y < 0 || y >= canvas_height as i32)
            {
                return;
            }
            let i = (y * canvas_width as i32 + x) as usize;
            if colored {
                let a = a * alpha as f64 / 255.0;
                if a == 1.0 {
                    slice[i * 4] = color.r() as f64 / 255.0;
                    slice[i * 4 + 1] = color.g() as f64 / 255.0;
                    slice[i * 4 + 2] = color.b() as f64 / 255.0;
                    slice[i * 4 + 3] = 1.0;
                } else {
                    let [tr, tg, tb, ta, ..] = &mut slice[i * 4..] else {
                        unreachable!()
                    };
                    *tr = *tr * *ta * (1.0 - a) + color.r() as f64 / 255.0 * a;
                    *tg = *tg * *ta * (1.0 - a) + color.g() as f64 / 255.0 * a;
                    *tb = *tb * *ta * (1.0 - a) + color.b() as f64 / 255.0 * a;
                    *ta = 1.0 - ((1.0 - *ta) * (1.0 - a));
                }
            } else {
                slice[i] = color.a() as f64 / 255.0;
            }
        });
        Ok(Array::new(canvas_shape, canvas_data).into())
    })
}
