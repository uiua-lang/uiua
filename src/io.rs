use std::{
    cell::RefCell,
    collections::HashMap,
    env,
    fs::{self, File},
    io::{stderr, stdin, stdout, Cursor, Read, Write},
    sync::{Mutex, OnceLock},
};

use enum_iterator::Sequence;
use hound::{SampleFormat, WavSpec, WavWriter};
use image::{DynamicImage, ImageOutputFormat};
use rand::prelude::*;

use crate::{array::Array, grid_fmt::GridFmt, rc_take, value::Value, Byte, Uiua, UiuaResult};

macro_rules! io_op {
    ($((
        $args:literal$(($outputs:expr))?,
        $variant:ident, $name:literal
    )),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
        pub enum IoOp {
            $($variant),*
        }

        impl IoOp {
            pub const ALL: [Self; 0 $(+ {stringify!($variant); 1})*] = [
                $(Self::$variant,)*
            ];
            pub fn name(&self) -> &'static str {
                match self {
                    $(Self::$variant => $name),*
                }
            }
            pub fn args(&self) -> u8 {
                match self {
                    $(IoOp::$variant => $args,)*
                }
            }
            pub fn outputs(&self) -> Option<u8> {
                match self {
                    $($(IoOp::$variant => $outputs.into(),)?)*
                    _ => Some(1)
                }
            }
        }
    };
}

io_op! {
    (1(0), Show, "Show"),
    (1(0), Prin, "Prin"),
    (1(0), Print, "Print"),
    (0, ScanLine, "ScanLine"),
    (0, Args, "Args"),
    (1, Var, "Var"),
    (0, Rand, "Rand"),
    (1, FOpen, "FOpen"),
    (1, FCreate, "FCreate"),
    (1, FClose, "FClose"),
    (1, FExists, "FExists"),
    (1, FListDir, "FListDir"),
    (1, FIsFile, "FIsFile"),
    (1, ReadAllStr, "FReadAllStr"),
    (1, ReadAllBytes, "FReadAllBytes"),
    (1, ReadStr, "ReadStr"),
    (1, WriteStr, "WriteStr"),
    (1, ReadBytes, "ReadBytes"),
    (1, WriteBytes, "WriteBytes"),
    (1, Import, "Import"),
    (0, Now, "Now"),
    (1, ImRead, "ImRead"),
    (1, ImWrite, "ImWrite"),
    (1(0), ImShow, "ImShow"),
    (1(0), AudioPlay, "AudioPlay"),
}

/// A handle to an IO stream
///
/// 0 is stdin, 1 is stdout, 2 is stderr.
///
/// Other handles can be used by files or sockets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Handle(pub u64);

impl Handle {
    pub const STDIN: Self = Self(0);
    pub const STDOUT: Self = Self(1);
    pub const STDERR: Self = Self(2);
    pub const FIRST_UNRESERVED: Self = Self(3);
}

impl From<usize> for Handle {
    fn from(n: usize) -> Self {
        Self(n as u64)
    }
}

impl From<Handle> for Value {
    fn from(handle: Handle) -> Self {
        (handle.0 as f64).into()
    }
}

#[allow(unused_variables)]
pub trait IoBackend {
    fn print_str(&self, s: &str) -> Result<(), String> {
        self.write(Handle::STDOUT, s.as_bytes())
    }
    fn rand(&self) -> f64;
    fn show_image(&self, image: DynamicImage) -> Result<(), String> {
        Err("Showing images not supported in this environment".into())
    }
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        Err("Playing audio not supported in this environment".into())
    }
    fn scan_line(&self) -> String {
        let mut bytes = Vec::new();
        while let Ok(b) = self.read(Handle::STDIN, 1) {
            if b.is_empty() || b[0] == b'\n' {
                break;
            }
            bytes.extend_from_slice(&b);
        }
        String::from_utf8_lossy(&bytes).into_owned()
    }
    fn var(&self, name: &str) -> Option<String> {
        None
    }
    fn args(&self) -> Vec<String> {
        Vec::new()
    }
    fn file_exists(&self, path: &str) -> bool {
        false
    }
    fn list_dir(&self, path: &str) -> Result<Vec<String>, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn read(&self, handle: Handle, count: usize) -> Result<Vec<u8>, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn create_file(&self, path: &str) -> Result<Handle, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn open_file(&self, path: &str) -> Result<Handle, String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn close_file(&self, handle: Handle) -> Result<(), String> {
        Err("This IO operation is not supported in this environment".into())
    }
    fn file_read_all(&self, path: &str) -> Result<Vec<u8>, String> {
        let handle = self.open_file(path)?;
        let bytes = self.read(handle, usize::MAX)?;
        self.close_file(handle)?;
        Ok(bytes)
    }
    fn file_write_all(&self, path: &str, contents: &[u8]) -> Result<(), String> {
        let handle = self.create_file(path)?;
        self.write(handle, contents)?;
        self.close_file(handle)?;
        Ok(())
    }
}

#[derive(Default)]
pub struct StdIo;

struct GlobalStdIo {
    next_handle: Handle,
    files: HashMap<Handle, File>,
}

impl Default for GlobalStdIo {
    fn default() -> Self {
        Self {
            next_handle: Handle::FIRST_UNRESERVED,
            files: HashMap::new(),
        }
    }
}

static STDIO: OnceLock<Mutex<GlobalStdIo>> = OnceLock::new();

fn stdio<T>(mut f: impl FnMut(&mut GlobalStdIo) -> T) -> T {
    f(&mut STDIO.get_or_init(Default::default).lock().unwrap())
}

thread_local! {
    static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::seed_from_u64(instant::now().to_bits()));
    #[cfg(feature = "rodio")]
    static AUDIO_STREAM: RefCell<Option<rodio::OutputStream>> = RefCell::new(None);
}

impl IoBackend for StdIo {
    fn rand(&self) -> f64 {
        RNG.with(|rng| rng.borrow_mut().gen())
    }
    fn var(&self, name: &str) -> Option<String> {
        env::var(name).ok()
    }
    fn args(&self) -> Vec<String> {
        env::args().collect()
    }
    fn file_exists(&self, path: &str) -> bool {
        fs::metadata(path).is_ok()
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        fs::metadata(path)
            .map(|m| m.is_file())
            .map_err(|e| e.to_string())
    }
    fn list_dir(&self, path: &str) -> Result<Vec<String>, String> {
        let mut paths = Vec::new();
        for entry in fs::read_dir(path).map_err(|e| e.to_string())? {
            let entry = entry.map_err(|e| e.to_string())?;
            paths.push(entry.path().to_string_lossy().into());
        }
        Ok(paths)
    }
    fn open_file(&self, path: &str) -> Result<Handle, String> {
        stdio(|io| {
            let handle = io.next_handle;
            io.next_handle.0 += 1;
            let file = File::open(path).map_err(|e| e.to_string())?;
            io.files.insert(handle, file);
            Ok(handle)
        })
    }
    fn create_file(&self, path: &str) -> Result<Handle, String> {
        stdio(|io| {
            let handle = io.next_handle;
            io.next_handle.0 += 1;
            let file = File::create(path).map_err(|e| e.to_string())?;
            io.files.insert(handle, file);
            Ok(handle)
        })
    }
    fn close_file(&self, handle: Handle) -> Result<(), String> {
        stdio(|io| {
            io.files
                .remove(&handle)
                .ok_or_else(|| "Invalid file handle".to_string())?;
            Ok(())
        })
    }
    fn read(&self, handle: Handle, len: usize) -> Result<Vec<u8>, String> {
        match handle {
            Handle::STDIN => {
                let mut buf = Vec::new();
                stdin()
                    .lock()
                    .take(len as u64)
                    .read_to_end(&mut buf)
                    .map_err(|e| e.to_string())?;
                Ok(buf)
            }
            Handle::STDOUT => Err("Cannot read from stdout".into()),
            Handle::STDERR => Err("Cannot read from stderr".into()),
            _ => stdio(|io| {
                if let Some(file) = io.files.get_mut(&handle) {
                    let mut buf = Vec::new();
                    file.take(len as u64)
                        .read_to_end(&mut buf)
                        .map_err(|e| e.to_string())?;
                    Ok(buf)
                } else {
                    Err("Invalid file handle".to_string())
                }
            }),
        }
    }
    fn write(&self, handle: Handle, conts: &[u8]) -> Result<(), String> {
        match handle {
            Handle::STDIN => Err("Cannot write to stdin".into()),
            Handle::STDOUT => stdout().lock().write_all(conts).map_err(|e| e.to_string()),
            Handle::STDERR => stderr().lock().write_all(conts).map_err(|e| e.to_string()),
            _ => stdio(|io| {
                if let Some(file) = io.files.get_mut(&handle) {
                    file.write_all(conts).map_err(|e| e.to_string())?;
                    Ok(())
                } else {
                    Err("Invalid file handle".to_string())
                }
            }),
        }
    }
    #[cfg(feature = "viuer")]
    fn show_image(&self, image: DynamicImage) -> Result<(), String> {
        let (width, height) = if image.width() > image.height() {
            (term_size::dimensions().map(|(w, _)| w as u32), None)
        } else {
            (
                None,
                term_size::dimensions().map(|(_, h)| h.saturating_sub(1) as u32),
            )
        };
        viuer::print(
            &image,
            &viuer::Config {
                width,
                height,
                absolute_offset: false,
                transparent: true,
                ..Default::default()
            },
        )
        .map(drop)
        .map_err(|e| format!("Failed to show image: {e}"))
    }
    #[cfg(feature = "rodio")]
    fn play_audio(&self, wav_bytes: Vec<u8>) -> Result<(), String> {
        use rodio::Source;
        let decoder = rodio::Decoder::new_wav(Cursor::new(wav_bytes))
            .map_err(|e| format!("Failed to decode audio: {e}"))?;
        let (stream, handle) = rodio::OutputStream::try_default()
            .map_err(|e| format!("Failed to create audio output stream: {e}"))?;
        AUDIO_STREAM.with(|s| *s.borrow_mut() = Some(stream));
        handle
            .play_raw(decoder.convert_samples())
            .map_err(|e| format!("Failed to play audio: {e}"))?;
        Ok(())
    }
}

impl IoOp {
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            IoOp::Show => {
                let s = env.pop(1)?.grid_string();
                env.io.print_str(&s).map_err(|e| env.error(e))?;
                env.io.print_str("\n").map_err(|e| env.error(e))?;
            }
            IoOp::Prin => {
                let val = env.pop(1)?;
                env.io
                    .print_str(&val.to_string())
                    .map_err(|e| env.error(e))?;
            }
            IoOp::Print => {
                let val = env.pop(1)?;
                env.io
                    .print_str(&val.to_string())
                    .map_err(|e| env.error(e))?;
                env.io.print_str("\n").map_err(|e| env.error(e))?;
            }
            IoOp::ScanLine => {
                let line = env.io.scan_line();
                env.push(line);
            }
            IoOp::Args => {
                let args = env.io.args();
                env.push(Array::<char>::from_iter(args));
            }
            IoOp::Var => {
                let key = env
                    .pop(1)?
                    .as_string(env, "Augument to var must be a string")?;
                let var = env.io.var(&key).unwrap_or_default();
                env.push(var);
            }
            IoOp::Rand => {
                let num = env.io.rand();
                env.push(num);
            }
            IoOp::FOpen => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let handle = env.io.open_file(&path).map_err(|e| env.error(e))?;
                env.push(handle);
            }
            IoOp::FCreate => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let handle = env.io.create_file(&path).map_err(|e| env.error(e))?;
                env.push(handle.0 as f64);
            }
            IoOp::FClose => {
                let handle = env.pop(1)?.as_nat(env, "Handle must be an integer")?.into();
                env.io.close_file(handle).map_err(|e| env.error(e))?;
            }
            IoOp::ReadStr => {
                let count = env.pop(1)?.as_nat(env, "Count must be an integer")?;
                let handle = env.pop(2)?.as_nat(env, "Handle must be an integer")?.into();
                let bytes = env.io.read(handle, count).map_err(|e| env.error(e))?;
                let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                env.push(s);
            }
            IoOp::ReadAllStr => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env.io.file_read_all(&path).map_err(|e| env.error(e))?;
                let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
                env.push(s);
            }
            IoOp::WriteStr => {
                let s = env.pop(1)?.as_string(env, "Contents must be a string")?;
                let handle = env.pop(2)?.as_nat(env, "Handle must be an integer")?.into();
                env.io.write(handle, s.as_ref()).map_err(|e| env.error(e))?;
            }
            IoOp::ReadBytes => {
                let count = env.pop(1)?.as_nat(env, "Count must be an integer")?;
                let handle = env.pop(2)?.as_nat(env, "Handle must be an integer")?.into();
                let bytes = env.io.read(handle, count).map_err(|e| env.error(e))?;
                let bytes = bytes.into_iter().map(Into::into);
                env.push(Array::<Byte>::from_iter(bytes));
            }
            IoOp::ReadAllBytes => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env.io.file_read_all(&path).map_err(|e| env.error(e))?;
                let bytes = bytes.into_iter().map(Into::into);
                env.push(Array::<Byte>::from_iter(bytes));
            }
            IoOp::WriteBytes => {
                let bytes =
                    rc_take(env.pop(1)?).into_bytes(env, "Contents must be a 1D array of bytes")?;
                let handle = env.pop(2)?.as_nat(env, "Handle must be an integer")?.into();
                env.io.write(handle, &bytes).map_err(|e| env.error(e))?;
            }
            IoOp::FExists => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let exists = env.io.file_exists(&path);
                env.push(exists);
            }
            IoOp::FListDir => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let paths = env.io.list_dir(&path).map_err(|e| env.error(e))?;
                env.push(Array::<char>::from_iter(paths));
            }
            IoOp::FIsFile => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let is_file = env.io.is_file(&path).map_err(|e| env.error(e))?;
                env.push(is_file);
            }
            IoOp::Import => {
                let path = env.pop(1)?.as_string(env, "Import path must be a string")?;
                if env.stack_size() > 0 {
                    return Err(env.error(format!(
                        "Stack must be empty before import, but there are {} items on it",
                        env.stack_size()
                    )));
                }
                let input =
                    String::from_utf8(env.io.file_read_all(&path).map_err(|e| env.error(e))?)
                        .map_err(|e| env.error(format!("Failed to read file: {e}")))?;
                env.import(&input, path.as_ref())?;
            }
            IoOp::Now => env.push(instant::now()),
            IoOp::ImRead => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env.io.file_read_all(&path).map_err(|e| env.error(e))?;
                let image = image::load_from_memory(&bytes)
                    .map_err(|e| env.error(format!("Failed to read image: {}", e)))?
                    .into_rgba8();
                let shape = vec![image.height() as usize, image.width() as usize, 4];
                let bytes: Vec<Byte> = image.into_raw().into_iter().map(Into::into).collect();
                let array = Array::<Byte>::from((shape, bytes));
                env.push(array);
            }
            IoOp::ImWrite => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let value = env.pop(2)?;
                let ext = path.split('.').last().unwrap_or("");
                let output_format = match ext {
                    "jpg" | "jpeg" => ImageOutputFormat::Jpeg(100),
                    "png" => ImageOutputFormat::Png,
                    "bmp" => ImageOutputFormat::Bmp,
                    "gif" => ImageOutputFormat::Gif,
                    "ico" => ImageOutputFormat::Ico,
                    _ => ImageOutputFormat::Png,
                };
                let bytes =
                    value_to_image_bytes(&value, output_format).map_err(|e| env.error(e))?;
                env.io
                    .file_write_all(&path, &bytes)
                    .map_err(|e| env.error(e))?;
            }
            IoOp::ImShow => {
                let value = env.pop(1)?;
                let image = value_to_image(&value).map_err(|e| env.error(e))?;
                env.io.show_image(image).map_err(|e| env.error(e))?;
            }
            IoOp::AudioPlay => {
                let value = env.pop(1)?;
                let bytes = value_to_wav_bytes(&value).map_err(|e| env.error(e))?;
                env.io.play_audio(bytes).map_err(|e| env.error(e))?;
            }
        }
        Ok(())
    }
}

pub fn value_to_image_bytes(value: &Value, format: ImageOutputFormat) -> Result<Vec<u8>, String> {
    let mut bytes = Cursor::new(Vec::new());
    value_to_image(value)?
        .write_to(&mut bytes, format)
        .map_err(|e| format!("Failed to write image: {e}"))?;
    Ok(bytes.into_inner())
}

pub fn value_to_image(value: &Value) -> Result<DynamicImage, String> {
    if ![2, 3].contains(&value.rank()) {
        return Err("Image must be a rank 2 or 3 numeric array".into());
    }
    let bytes = match value {
        Value::Num(nums) => nums
            .data
            .iter()
            .map(|f| (*f * 255.0).floor() as u8)
            .collect(),
        Value::Byte(bytes) => bytes.data.iter().map(|&b| b.or(0).min(1) * 255).collect(),
        _ => return Err("Image must be a numeric array".into()),
    };
    #[allow(clippy::match_ref_pats)]
    let [height, width, px_size] = match value.shape() {
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

pub fn value_to_wav_bytes(audio: &Value) -> Result<Vec<u8>, String> {
    let values: Vec<i16> = match audio {
        Value::Num(nums) => nums
            .data
            .iter()
            .map(|&f| (f.clamp(-1.0, 1.0) * i16::MAX as f64) as i16)
            .collect(),
        Value::Byte(byte) => byte
            .data
            .iter()
            .map(|&b| b.value().map_or(i16::MIN, |_| i16::MAX))
            .collect(),
        _ => return Err("Audio must be a numeric array".into()),
    };
    let (length, channels) = match audio.rank() {
        1 => (values.len(), vec![values]),
        2 => (
            audio.row_len(),
            values
                .chunks_exact(audio.row_len())
                .map(|c| c.to_vec())
                .collect(),
        ),
        n => {
            return Err(format!(
                "Audio must be a rank 1 or 2 numeric array, but it is rank {n}"
            ))
        }
    };
    let spec = WavSpec {
        channels: channels.len() as u16,
        sample_rate: 44100,
        bits_per_sample: 16,
        sample_format: SampleFormat::Int,
    };
    let mut bytes = Cursor::new(Vec::new());
    let mut writer = WavWriter::new(&mut bytes, spec).map_err(|e| e.to_string())?;
    for i in 0..length {
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
