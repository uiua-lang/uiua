use std::{
    cell::RefCell,
    env, fs,
    io::{stdin, stdout, BufRead, Cursor, Write},
};

use image::{DynamicImage, ImageOutputFormat};
use rand::prelude::*;

use crate::{array::Array, grid_fmt::GridFmt, rc_take, value::Value, Uiua, UiuaResult};

macro_rules! io_op {
    ($((
        $args:literal$(($outputs:expr))?,
        $variant:ident, $name:literal
    )),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum IoOp {
            $($variant),*
        }

        impl IoOp {
            pub fn from_name(s: &str) -> Option<Self> {
                match s {
                    $($name => Some(Self::$variant)),*,
                    _ => None
                }
            }
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
    (1(0), Show, "show"),
    (1(0), Print, "print"),
    (1(0), Println, "println"),
    (0, ScanLn, "scanln"),
    (0, Args, "args"),
    (1, Var, "var"),
    (0, Rand, "rand"),
    (1, FReadStr, "freadstr"),
    (1, FWriteStr, "fwritestr"),
    (1, FReadBytes, "freadbytes"),
    (1, FWriteBytes, "fwritebytes"),
    (1, FLines, "flines"),
    (1, FExists, "fexists"),
    (1, FListDir, "flistdir"),
    (1, FIsFile, "fisfile"),
    (1, Import, "import"),
    (0, Now, "now"),
    (1, ImRead, "imread"),
    (1, ImWrite, "imwrite"),
    (1(0), ImShow, "imshow"),
}

#[allow(unused_variables)]
pub trait IoBackend {
    fn print_str(&self, s: &str);
    fn rand(&self) -> f64;
    fn show_image(&self, image: DynamicImage) -> Result<(), String> {
        Err("Showing images not supported in this environment".into())
    }
    fn scan_line(&self) -> String {
        String::new()
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
        Err("File IO not supported in this environment".into())
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        Err("File IO not supported in this environment".into())
    }
    fn read_file(&self, path: &str) -> Result<Vec<u8>, String> {
        Err("File IO not supported in this environment".into())
    }
    fn write_file(&self, path: &str, contents: Vec<u8>) -> Result<(), String> {
        Err("File IO not supported in this environment".into())
    }
}

#[derive(Default)]
pub struct StdIo;

thread_local! {
    static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::seed_from_u64(instant::now().to_bits()));
}

impl IoBackend for StdIo {
    fn print_str(&self, s: &str) {
        print!("{}", s);
        let _ = stdout().lock().flush();
    }
    fn rand(&self) -> f64 {
        RNG.with(|rng| rng.borrow_mut().gen())
    }
    fn scan_line(&self) -> String {
        stdin()
            .lock()
            .lines()
            .next()
            .and_then(Result::ok)
            .unwrap_or_default()
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
    fn read_file(&self, path: &str) -> Result<Vec<u8>, String> {
        fs::read(path).map_err(|e| e.to_string())
    }
    fn write_file(&self, path: &str, contents: Vec<u8>) -> Result<(), String> {
        fs::write(path, contents).map_err(|e| e.to_string())
    }
}

impl IoOp {
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            IoOp::Show => {
                let s = env.pop(1)?.grid_string();
                env.io.print_str(&s);
                env.io.print_str("\n");
            }
            IoOp::Print => {
                let val = env.pop(1)?;
                env.io.print_str(&val.to_string());
            }
            IoOp::Println => {
                let val = env.pop(1)?;
                env.io.print_str(&val.to_string());
                env.io.print_str("\n");
            }
            IoOp::ScanLn => {
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
            IoOp::FReadStr => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let contents =
                    String::from_utf8(env.io.read_file(&path).map_err(|e| env.error(e))?)
                        .map_err(|e| env.error(format!("Failed to read file: {e}")))?;
                env.push(contents);
            }
            IoOp::FWriteStr => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let contents = env.pop(2)?.as_string(env, "Contents must be a string")?;
                env.io
                    .write_file(&path, contents.into_bytes())
                    .map_err(|e| env.error(e))?;
            }
            IoOp::FReadBytes => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let contents = env.io.read_file(&path).map_err(|e| env.error(e))?;
                let arr = Array::<u8>::from(contents);
                env.push(arr);
            }
            IoOp::FWriteBytes => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let contents =
                    rc_take(env.pop(2)?).into_bytes(env, "Contents must be a byte array")?;
                env.io
                    .write_file(&path, contents)
                    .map_err(|e| env.error(e))?;
            }
            IoOp::FLines => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let lines: Array<char> =
                    String::from_utf8(env.io.read_file(&path).map_err(|e| env.error(e))?)
                        .map_err(|e| env.error(format!("Failed to read file: {}", e)))?
                        .lines()
                        .map(String::from)
                        .collect();
                env.push(lines);
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
                let input = String::from_utf8(env.io.read_file(&path).map_err(|e| env.error(e))?)
                    .map_err(|e| env.error(format!("Failed to read file: {e}")))?;
                env.import(&input, path.as_ref())?;
            }
            IoOp::Now => env.push(instant::now()),
            IoOp::ImRead => {
                let path = env.pop(1)?.as_string(env, "Path must be a string")?;
                let bytes = env.io.read_file(&path).map_err(|e| env.error(e))?;
                let image = image::load_from_memory(&bytes)
                    .map_err(|e| env.error(format!("Failed to read image: {}", e)))?
                    .into_rgba8();
                let shape = vec![image.height() as usize, image.width() as usize, 4];
                let array = Array::<u8>::from((shape, image.into_raw()));
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
                    "tga" => ImageOutputFormat::Tga,
                    "tiff" => ImageOutputFormat::Tiff,
                    _ => ImageOutputFormat::Png,
                };
                let bytes =
                    value_to_image_bytes(&value, output_format).map_err(|e| env.error(e))?;
                env.io.write_file(&path, bytes).map_err(|e| env.error(e))?;
            }
            IoOp::ImShow => {
                let value = env.pop(1)?;
                let image = value_to_image(&value).map_err(|e| env.error(e))?;
                env.io.show_image(image).map_err(|e| env.error(e))?;
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
        Value::Byte(bytes) => bytes.data.iter().map(|&b| b.min(1) * 255).collect(),
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
