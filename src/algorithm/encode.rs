//! En/decode Uiua arrays to/from various binary and text formats

use std::str;

use ecow::{eco_vec, EcoVec};
use enum_iterator::{all, Sequence};

use crate::{
    algorithm::validate_size, cowslice::CowSlice, fill::FillValue, Array, ArrayFlags, ArrayMeta,
    Boxed, Complex, Shape, SubSide, Uiua, UiuaResult, Value,
};

use super::FillContext;

impl Value {
    pub(crate) fn to_json_string(&self, env: &Uiua) -> UiuaResult<String> {
        let json = self.to_json_value(env)?;
        serde_json::to_string(&json).map_err(|e| env.error(e))
    }
    pub(crate) fn to_json_value(&self, env: &Uiua) -> UiuaResult<serde_json::Value> {
        Ok(match self {
            Value::Num(n) if n.rank() == 0 => {
                let meta = &n.meta;
                let n = n.data[0];
                if meta.flags.contains(ArrayFlags::BOOLEAN_LITERAL) && (n == 0.0 || n == 1.0) {
                    serde_json::Value::Bool(n != 0.0)
                } else if n.fract() == 0.0 && n.abs() < i64::MAX as f64 {
                    serde_json::Value::Number((n as i64).into())
                } else {
                    serde_json::Number::from_f64(n)
                        .map(Into::into)
                        .unwrap_or(serde_json::Value::Null)
                }
            }
            Value::Byte(bytes) if bytes.rank() == 0 => {
                let b = bytes.data[0];
                if bytes.meta.flags.contains(ArrayFlags::BOOLEAN_LITERAL) {
                    serde_json::Value::Bool(b != 0)
                } else {
                    serde_json::Value::Number(b.into())
                }
            }
            Value::Complex(_) => return Err(env.error("Cannot convert complex numbers to JSON")),
            Value::Char(c) if c.rank() == 0 => serde_json::Value::String(c.data[0].to_string()),
            Value::Char(c) if c.rank() == 1 => serde_json::Value::String(c.data.iter().collect()),
            Value::Box(b) if b.rank() == 0 => b.data[0].0.to_json_value(env)?,
            value => {
                if value.is_map() {
                    let mut map = serde_json::Map::with_capacity(value.row_count());
                    for (k, v) in value.map_kv() {
                        let k = k.as_string(env, "JSON map keys must be strings")?;
                        let v = v.to_json_value(env)?;
                        map.insert(k, v);
                    }
                    serde_json::Value::Object(map)
                } else {
                    serde_json::Value::Array(
                        value
                            .rows()
                            .map(|row| row.to_json_value(env))
                            .collect::<Result<_, _>>()?,
                    )
                }
            }
        })
    }
    pub(crate) fn from_json_string(json: &str, env: &Uiua) -> UiuaResult<Self> {
        #[cfg(not(feature = "json5"))]
        let json_value: serde_json::Value = serde_json::from_str(json).map_err(|e| env.error(e))?;
        #[cfg(feature = "json5")]
        let json_value: serde_json::Value = json5::from_str(json).map_err(|e| env.error(e))?;
        Self::from_json_value(json_value, env)
    }
    pub(crate) fn from_json_value(json_value: serde_json::Value, _env: &Uiua) -> UiuaResult<Self> {
        Ok(match json_value {
            serde_json::Value::Null => f64::NAN.into(),
            serde_json::Value::Bool(b) => b.into(),
            serde_json::Value::Number(n) => {
                if let Some(n) = n.as_f64() {
                    if n >= 0.0 && n.fract() == 0.0 && n < u8::MAX as f64 {
                        (n as u8).into()
                    } else {
                        n.into()
                    }
                } else {
                    0.0.into()
                }
            }
            serde_json::Value::String(s) => s.into(),
            serde_json::Value::Array(arr) => {
                let mut rows = Vec::with_capacity(arr.len());
                for value in arr {
                    let mut value = Value::from_json_value(value, _env)?;
                    if value.meta.map_keys.is_some() {
                        value = Boxed(value).into();
                    }
                    rows.push(value);
                }
                if rows.iter().all(|val| val.shape.is_empty())
                    && (rows.windows(2)).all(|win| win[0].type_id() == win[1].type_id())
                {
                    Value::from_row_values_infallible(rows)
                } else {
                    rows.into_iter()
                        .map(Value::boxed_if_not)
                        .collect::<Array<_>>()
                        .into()
                }
            }
            serde_json::Value::Object(map) => {
                let mut keys = EcoVec::with_capacity(map.len());
                let mut values = Vec::with_capacity(map.len());
                for (k, v) in map {
                    keys.push(Boxed(k.into()));
                    let mut value = Value::from_json_value(v, _env)?;
                    if value.meta.map_keys.is_some() {
                        value = Boxed(value).into();
                    }
                    values.push(value);
                }
                let mut values = if values.iter().all(|val| val.shape.is_empty())
                    && (values.windows(2)).all(|win| win[0].type_id() == win[1].type_id())
                {
                    Value::from_row_values_infallible(values)
                } else {
                    Array::from(values.into_iter().map(Boxed).collect::<EcoVec<_>>()).into()
                };
                values.map(keys.into(), _env)?;
                values
            }
        })
    }
}

impl Value {
    pub(crate) fn to_csv(&self, env: &Uiua) -> UiuaResult<String> {
        #[cfg(not(feature = "csv"))]
        return Err(env.error("CSV support is not enabled in this environment"));
        #[cfg(feature = "csv")]
        {
            let delimiter =
                u8::try_from(env.scalar_fill::<char>().map(|fv| fv.value).unwrap_or(','))
                    .map_err(|_| env.error("CSV delimiter must be ASCII"))?;

            let mut buf = Vec::new();
            let mut writer = csv::WriterBuilder::new()
                .flexible(true)
                .delimiter(delimiter)
                .from_writer(&mut buf);

            match self.rank() {
                0 => writer
                    .write_record([self.format()])
                    .map_err(|e| env.error(e))?,
                1 => {
                    for row in self.rows() {
                        writer
                            .write_record(row.unboxed().rows().map(|v| v.format()))
                            .map_err(|e| env.error(e))?;
                    }
                }
                2 => {
                    for row in self.rows() {
                        writer
                            .write_record(row.rows().map(|v| v.format()))
                            .map_err(|e| env.error(e))?;
                    }
                }
                n => return Err(env.error(format!("Cannot write a rank-{n} array to CSV"))),
            }
            writer.flush().map_err(|e| env.error(e))?;
            drop(writer);
            let s = String::from_utf8(buf).map_err(|e| env.error(e))?;
            Ok(s)
        }
    }
    pub(crate) fn to_xlsx(&self, env: &Uiua) -> UiuaResult<Vec<u8>> {
        #[cfg(not(feature = "simple_excel_writer"))]
        return Err(env.error("XLSX encoding is not enabled in this environment"));
        #[cfg(feature = "simple_excel_writer")]
        {
            use simple_excel_writer::*;
            if self.rank() > 3 {
                return Err(env.error(format!(
                    "Cannot write a rank-{} array to an XLSX workbook",
                    self.rank()
                )));
            }
            let sheet_arrays = if self.is_map() {
                let mut sheet_arrays = Vec::new();
                for (k, v) in self.map_kv() {
                    let name = k.as_string(env, "Sheet name must be a string")?;
                    if v.rank() > 2 {
                        return Err(env.error(format!(
                            "Cannot write a rank-{} array to an XLSX sheet",
                            v.rank()
                        )));
                    }
                    sheet_arrays.push((name, v));
                }
                sheet_arrays
            } else if self.rank() == 3 {
                self.rows()
                    .enumerate()
                    .map(|(i, row)| (format!("Sheet {}", i + 1), row))
                    .collect()
            } else {
                vec![("Sheet1".into(), self.clone())]
            };
            let mut workbook = Workbook::create_in_memory();
            for (sheet_name, value) in sheet_arrays {
                let mut sheet = workbook.create_sheet(&sheet_name);
                workbook
                    .write_sheet(&mut sheet, |writer| {
                        for row in value.unboxed().into_rows() {
                            let mut sheet_row = Row::new();
                            for cell in row.unboxed().into_rows() {
                                match cell {
                                    Value::Num(n) => sheet_row.add_cell(n.data[0]),
                                    Value::Byte(b) => sheet_row.add_cell(b.data[0] as f64),
                                    Value::Char(c) => sheet_row.add_cell(c.data[0].to_string()),
                                    Value::Complex(c) => sheet_row.add_cell(c.data[0].to_string()),
                                    Value::Box(b) => {
                                        let Boxed(b) = &b.data[0];
                                        if b.row_count() == 0 {
                                            sheet_row.add_empty_cells(1);
                                        } else {
                                            sheet_row.add_cell(b.format())
                                        }
                                    }
                                }
                            }
                            writer.append_row(sheet_row)?;
                        }
                        Ok(())
                    })
                    .map_err(|e| env.error(e))?;
            }
            workbook
                .close()
                .map(Option::unwrap)
                .map_err(|e| env.error(e))
        }
    }
    pub(crate) fn from_csv(csv_str: &str, env: &mut Uiua) -> UiuaResult<Self> {
        #[cfg(not(feature = "csv"))]
        return Err(env.error("CSV support is not enabled in this environment"));
        #[cfg(feature = "csv")]
        {
            let delimiter = u8::try_from(
                env.scalar_unfill::<char>()
                    .map(|fv| fv.value)
                    .unwrap_or(','),
            )
            .map_err(|_| env.error("CSV delimiter must be ASCII"))?;

            let mut reader = csv::ReaderBuilder::new()
                .has_headers(false)
                .flexible(true)
                .delimiter(delimiter)
                .from_reader(csv_str.as_bytes());

            let fill = env
                .value_fill()
                .cloned()
                .unwrap_or_else(|| FillValue::new("", None));
            env.with_fill(fill.value, fill.side, |env| {
                let mut rows = Vec::new();
                for result in reader.records() {
                    let record = result.map_err(|e| env.error(e))?;
                    let mut row = EcoVec::new();
                    for field in record.iter() {
                        row.push(Boxed(field.into()));
                    }
                    rows.push(Array::new(row.len(), row));
                }
                Array::from_row_arrays(rows, env).map(Into::into)
            })
        }
    }
    pub(crate) fn from_xlsx(_xlsx: &[u8], env: &mut Uiua) -> UiuaResult<Self> {
        #[cfg(not(feature = "calamine"))]
        return Err(env.error("XLSX decoding is not enabled in this environment"));
        #[cfg(feature = "calamine")]
        {
            use calamine::*;

            let mut workbook: Xlsx<_> =
                open_workbook_from_rs(std::io::Cursor::new(_xlsx)).map_err(|e| env.error(e))?;
            let sheet_names = workbook.sheet_names();
            let fill = env
                .value_fill()
                .cloned()
                .unwrap_or_else(|| FillValue::new("", None));
            let mut sheet_values = EcoVec::new();
            env.with_fill(fill.value, fill.side, |env| {
                for sheet_name in &sheet_names {
                    let sheet = workbook
                        .worksheet_range(sheet_name)
                        .map_err(|e| env.error(e))?;
                    let mut rows = Vec::new();
                    for row in sheet.rows() {
                        let mut cells = EcoVec::new();
                        for cell in row {
                            cells.push(Boxed(match cell {
                                &Data::Int(i) => i.into(),
                                &Data::Float(f) => f.into(),
                                Data::String(s) => s.clone().into(),
                                &Data::Bool(b) => b.into(),
                                Data::DateTime(dt) => {
                                    ((dt.as_f64() - 2.0) * 24.0 * 60.0 * 60.0 - 2208988800.0).into()
                                }
                                Data::DateTimeIso(dt) => dt.clone().into(),
                                Data::DurationIso(dur) => dur.clone().into(),
                                Data::Error(e) => e.to_string().into(),
                                Data::Empty => String::new().into(),
                            }));
                        }
                        rows.push(Array::from(cells));
                    }
                    sheet_values.push(Boxed(Array::from_row_arrays(rows, env)?.into()));
                }
                Ok(())
            })?;
            let keys: Value = sheet_names.into_iter().map(|s| Boxed(s.into())).collect();
            let mut values: Value = Array::from(sheet_values).into();
            values.map(keys, env)?;
            Ok(values)
        }
    }
}

impl Value {
    /// Encode a value as bytes
    pub fn encode_bytes(
        &self,
        mut data: Self,
        side: Option<SubSide>,
        env: &Uiua,
    ) -> UiuaResult<Array<u8>> {
        let format = self.as_string(env, "Format must be a string")?;
        let format = format.as_str();
        let elem_size = match format {
            "u8" | "i8" => 1,
            "u16" | "i16" => 2,
            "u32" | "i32" => 4,
            "u64" | "i64" => 8,
            "u128" | "i128" => 16,
            "f32" => 4,
            "f64" => 8,
            _ => return Err(env.error(format!("Invalid byte format: {format}"))),
        };
        // Early return when a byte array can be reused
        if let Value::Byte(mut arr) = data {
            match format {
                "u8" => return Ok(arr),
                "i8" => {
                    for i in arr.data.as_mut_slice() {
                        *i = (*i).min(i8::MAX as u8);
                    }
                    return Ok(arr);
                }
                _ => data = arr.into(),
            }
        }
        let mut bytes = CowSlice::from_elem(0, data.shape.elements() * elem_size);
        let slice = bytes.as_mut_slice();
        fn write<T: Copy, const N: usize>(src: &[T], dst: &mut [u8], f: impl Fn(T) -> [u8; N]) {
            for (i, &src) in src.iter().enumerate() {
                dst[i * N..][..N].copy_from_slice(&f(src));
            }
        }
        macro_rules! write {
            ($arr:expr, $ty:ty) => {
                match side {
                    None => write(&$arr.data, slice, |n| (n as $ty).to_ne_bytes()),
                    Some(SubSide::Left) => write(&$arr.data, slice, |n| (n as $ty).to_le_bytes()),
                    Some(SubSide::Right) => write(&$arr.data, slice, |n| (n as $ty).to_be_bytes()),
                }
            };
        }
        let mut shape = match data {
            Value::Byte(arr) => {
                match format {
                    "u8" | "i8" => unreachable!("handled above"),
                    "u16" => write!(arr, u16),
                    "i16" => write!(arr, i16),
                    "u32" => write!(arr, u32),
                    "i32" => write!(arr, i32),
                    "u64" => write!(arr, u64),
                    "i64" => write!(arr, i64),
                    "u128" => write!(arr, u128),
                    "i128" => write!(arr, i128),
                    "f32" => write!(arr, f32),
                    "f64" => write!(arr, f64),
                    format => unreachable!("format {format} is not supported"),
                }
                arr.shape
            }
            Value::Num(arr) => {
                match format {
                    "u8" => write!(arr, u8),
                    "i8" => write!(arr, i8),
                    "u16" => write!(arr, u16),
                    "i16" => write!(arr, i16),
                    "u32" => write!(arr, u32),
                    "i32" => write!(arr, i32),
                    "u64" => write!(arr, u64),
                    "i64" => write!(arr, i64),
                    "u128" => write!(arr, u128),
                    "i128" => write!(arr, i128),
                    "f32" => write!(arr, f32),
                    "f64" => write!(arr, f64),
                    format => unreachable!("format {format} is not supported"),
                }
                arr.shape
            }
            value => {
                return Err(env.error(format!(
                    "Cannot encode {} as bytes",
                    value.type_name_plural()
                )))
            }
        };
        if elem_size != 1 {
            shape.push(elem_size);
        }
        Ok(Array::new(shape, bytes))
    }
    /// Decode a value from bytes
    pub fn decode_bytes(
        &self,
        bytes: Self,
        side: Option<SubSide>,
        env: &Uiua,
    ) -> UiuaResult<Value> {
        let format = self.as_string(env, "Format must be a string")?;
        let format = format.as_str();
        let elem_size = match format {
            "u8" | "i8" => 1,
            "u16" | "i16" => 2,
            "u32" | "i32" => 4,
            "u64" | "i64" => 8,
            "u128" | "i128" => 16,
            "f32" => 4,
            "f64" => 8,
            _ => return Err(env.error(format!("Invalid byte format: {format}"))),
        };
        let bytes = match bytes {
            Value::Byte(arr) if format == "u8" => return Ok(arr.into()),
            Value::Byte(arr) => arr,
            Value::Num(arr) if format == "u8" => {
                return Ok(arr.convert_ref_with(|n| n as u8).into())
            }
            Value::Num(arr) => arr.convert_ref_with(|n| n as u8),
            value => {
                return Err(env.error(format!(
                    "Cannot decode {} as bytes",
                    value.type_name_plural()
                )))
            }
        };
        let mut new_shape = bytes.shape;
        if new_shape.is_empty() {
            new_shape.push(1);
        }
        if new_shape.len() > 1 {
            let last_dim = new_shape.pop().unwrap();
            if last_dim != elem_size {
                return Err(env.error(format!(
                    "Bytes shape has last axis size {last_dim}, \
                    which does not match format {format}"
                )));
            }
        } else {
            new_shape[0] = (new_shape[0] / elem_size).max(1);
        }
        let mut data = eco_vec![0.0; new_shape.elements()];
        let slice = data.make_mut();
        fn read<const N: usize>(src: &[u8], dst: &mut [f64], f: impl Fn([u8; N]) -> f64) {
            let mut curr = [0; N];
            for (i, src) in src.chunks_exact(N).enumerate() {
                curr.copy_from_slice(src);
                dst[i] = f(curr);
            }
        }
        macro_rules! read {
            ($ty:ty) => {
                match side {
                    None => read(&bytes.data, slice, |arr| <$ty>::from_ne_bytes(arr) as f64),
                    Some(SubSide::Left) => {
                        read(&bytes.data, slice, |arr| <$ty>::from_le_bytes(arr) as f64)
                    }
                    Some(SubSide::Right) => {
                        read(&bytes.data, slice, |arr| <$ty>::from_be_bytes(arr) as f64)
                    }
                }
            };
        }
        match format {
            "u8" => unreachable!("handled above"),
            "i8" => read!(i8),
            "u16" => read!(u16),
            "i16" => read!(i16),
            "u32" => read!(u32),
            "i32" => read!(i32),
            "u64" => read!(u64),
            "i64" => read!(i64),
            "u128" => read!(u128),
            "i128" => read!(i128),
            "f32" => read!(f32),
            "f64" => read!(f64),
            format => unreachable!("format {format} is not supported"),
        }
        Ok(Array::new(new_shape, data).into())
    }
}

#[derive(Clone, Copy, Sequence)]
#[repr(u8)]
enum BinType {
    U8 = 0,
    U16 = 1,
    U32 = 2,
    U64 = 3,
    I8 = 4,
    I16 = 5,
    I32 = 6,
    I64 = 7,
    F32 = 8,
    F64 = 9,
    Char = 16,
    Box = 32,
    Complex = 48,
}

const MAX_BINARY_DEPTH: usize = if cfg!(debug_assertions) { 10 } else { 32 };

impl Value {
    pub(crate) fn to_binary(&self, env: &Uiua) -> UiuaResult<Vec<u8>> {
        let mut bytes = Vec::new();
        self.to_binary_impl(&mut bytes, 0, env)?;
        Ok(bytes)
    }
    fn to_binary_impl(&self, bytes: &mut Vec<u8>, depth: usize, env: &Uiua) -> UiuaResult {
        if depth > MAX_BINARY_DEPTH {
            return Err(env.error("Nested structure is too deep"));
        }
        if self.rank() > u8::MAX as usize {
            return Err(env.error(format!("Rank {} is too large", self.rank())));
        }
        fn write_shape(shape: &Shape, bytes: &mut Vec<u8>) {
            bytes.push(shape.len() as u8);
            for &dim in shape {
                bytes.extend((dim as u32).to_le_bytes());
            }
        }
        fn write_ty_meta(
            ty: BinType,
            meta: &ArrayMeta,
            bytes: &mut Vec<u8>,
            depth: usize,
            env: &Uiua,
        ) -> UiuaResult {
            let mut ty_u = ty as u8;
            if meta == &ArrayMeta::default() {
                bytes.push(ty_u);
            } else {
                ty_u |= 128u8;
                bytes.push(ty_u);
                // Flags
                bytes.push(meta.flags.bits());
                // Label
                let label = meta.label.as_deref().unwrap_or("");
                bytes.extend((label.len() as u32).to_le_bytes());
                bytes.extend(label.as_bytes());
                // Pointer
                if meta.pointer.is_some() {
                    return Err(env.error("Cannot serialize pointers"));
                }
                // Handle kind
                if meta.handle_kind.is_some() {
                    return Err(env.error("Cannot serialize I/O handles"));
                }
                // Map keys
                bytes.push(meta.map_keys.is_some() as u8);
                if let Some(keys) = &meta.map_keys {
                    keys.clone()
                        .normalized()
                        .to_binary_impl(bytes, depth + 1, env)?;
                }
            }
            Ok(())
        }
        match self {
            Value::Num(arr) => {
                let mut all_non_neg = true;
                let mut all_int = true;
                let mut all_f32 = true;
                let mut min = 0f64;
                let mut max = 0f64;
                for &n in &arr.data {
                    all_non_neg &= n >= 0.0;
                    all_int &= n.fract() == 0.0;
                    all_f32 &= (n as f32 as f64).to_bits() == n.to_bits();
                    min = min.min(n);
                    max = max.max(n);
                }
                let ty = if all_non_neg && all_int {
                    if max <= u8::MAX as f64 {
                        BinType::U8
                    } else if max <= u16::MAX as f64 {
                        BinType::U16
                    } else if max <= u32::MAX as f64 {
                        BinType::U32
                    } else if max <= u64::MAX as f64 {
                        BinType::U64
                    } else if max <= 2f32.powf(24.0) as f64 {
                        BinType::F32
                    } else {
                        BinType::F64
                    }
                } else if all_int {
                    if min >= i8::MIN as f64 && max <= i8::MAX as f64 {
                        BinType::I8
                    } else if min >= i16::MIN as f64 && max <= i16::MAX as f64 {
                        BinType::I16
                    } else if min >= i32::MIN as f64 && max <= i32::MAX as f64 {
                        BinType::I32
                    } else if min >= i64::MIN as f64 && max <= i64::MAX as f64 {
                        BinType::I64
                    } else if min >= -(2f32.powf(24.0)) as f64 && max <= 2f32.powf(24.0) as f64 {
                        BinType::F32
                    } else {
                        BinType::F64
                    }
                } else if all_f32 {
                    BinType::F32
                } else {
                    BinType::F64
                };
                write_ty_meta(ty, &arr.meta, bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                fn write(nums: &[f64], bytes: &mut Vec<u8>, f: impl Fn(f64, &mut Vec<u8>)) {
                    for &n in nums {
                        f(n, bytes);
                    }
                }
                let data = &arr.data;
                match ty {
                    BinType::U8 => write(data, bytes, |n, b| b.extend((n as u8).to_le_bytes())),
                    BinType::U16 => write(data, bytes, |n, b| b.extend((n as u16).to_le_bytes())),
                    BinType::U32 => write(data, bytes, |n, b| b.extend((n as u32).to_le_bytes())),
                    BinType::U64 => write(data, bytes, |n, b| b.extend((n as u64).to_le_bytes())),
                    BinType::I8 => write(data, bytes, |n, b| b.extend((n as i8).to_le_bytes())),
                    BinType::I16 => write(data, bytes, |n, b| b.extend((n as i16).to_le_bytes())),
                    BinType::I32 => write(data, bytes, |n, b| b.extend((n as i32).to_le_bytes())),
                    BinType::I64 => write(data, bytes, |n, b| b.extend((n as i64).to_le_bytes())),
                    BinType::F32 => write(data, bytes, |n, b| b.extend((n as f32).to_le_bytes())),
                    BinType::F64 => write(data, bytes, |n, b| b.extend(n.to_le_bytes())),
                    _ => unreachable!(),
                }
            }
            Value::Byte(arr) => {
                write_ty_meta(BinType::U8, &arr.meta, bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                bytes.extend(&arr.data);
            }
            Value::Char(arr) => {
                write_ty_meta(BinType::Char, &arr.meta, bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                let s: String = arr.data.iter().copied().collect();
                bytes.extend((s.len() as u32).to_le_bytes());
                bytes.extend(s.as_bytes());
            }
            Value::Box(arr) => {
                write_ty_meta(BinType::Box, &arr.meta, bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                for Boxed(v) in &arr.data {
                    v.to_binary_impl(bytes, depth + 1, env)?;
                }
            }
            Value::Complex(arr) => {
                write_ty_meta(BinType::Complex, &arr.meta, bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                for Complex { re, im } in &arr.data {
                    bytes.extend(re.to_le_bytes());
                    bytes.extend(im.to_le_bytes());
                }
            }
        }
        Ok(())
    }
    pub(crate) fn from_binary(mut bytes: &[u8], env: &Uiua) -> UiuaResult<Self> {
        Self::from_binary_impl(&mut bytes, 0, env)
    }
    fn from_binary_impl(bytes: &mut &[u8], depth: usize, env: &Uiua) -> UiuaResult<Self> {
        if depth > MAX_BINARY_DEPTH {
            return Err(env.error("Nested structure is too deep"));
        }
        // Type
        let mut ty_u = *bytes
            .first()
            .ok_or_else(|| env.error("Missing type identifier"))?;
        let has_meta = ty_u & 128u8 != 0;
        ty_u &= 127u8;
        let ty = all::<BinType>()
            .find(|&ty| ty as u8 == ty_u)
            .ok_or_else(|| env.error(format!("Invalid binary type {ty_u}")))?;
        *bytes = &bytes[1..];

        let meta = if has_meta {
            let mut meta = ArrayMeta::default();
            // Flags
            if bytes.is_empty() {
                return Err(env.error("Missing flags length"));
            }
            let flags_u = *bytes.first().unwrap();
            *bytes = &bytes[1..];
            let flags = ArrayFlags::from_bits(flags_u)
                .ok_or_else(|| env.error(format!("Invalid array flags {flags_u:08b}")))?;
            meta.flags = flags;

            // Label
            if bytes.len() < size_of::<u32>() {
                return Err(env.error("Missing label length"));
            }
            let label_len = u32::from_le_bytes(bytes[..size_of::<u32>()].try_into().unwrap());
            *bytes = &bytes[size_of::<u32>()..];
            if label_len > 0 {
                if bytes.len() < label_len as usize {
                    return Err(env.error("Missing label data"));
                }
                let label = str::from_utf8(&bytes[..label_len as usize])
                    .map_err(|e| env.error(format!("Failed to parse label: {e}")))?;
                *bytes = &bytes[label_len as usize..];
                meta.label = Some(label.into());
            }

            // Map keys
            if bytes.is_empty() {
                return Err(env.error("Missing map keys check"));
            }
            let has_map_keys = *bytes.first().unwrap() != 0;
            *bytes = &bytes[1..];
            let keys = if has_map_keys {
                Some(Self::from_binary_impl(bytes, depth + 1, env)?)
            } else {
                None
            };

            Some((meta, keys))
        } else {
            None
        };

        // Rank
        if bytes.is_empty() {
            return Err(env.error("Missing rank"));
        }
        let rank = *bytes.first().unwrap();
        *bytes = &bytes[1..];

        // Shape
        let mut shape = Shape::with_capacity(rank as usize);
        for i in 0..rank {
            if bytes.len() < size_of::<u32>() {
                return Err(env.error(format!("Missing shape dimension {i}")));
            }
            let len = u32::from_le_bytes(bytes[..size_of::<u32>()].try_into().unwrap());
            shape.push(len as usize);
            *bytes = &bytes[size_of::<u32>()..];
        }

        // Data
        fn make<'a, A: TryFrom<&'a [u8]>, T, E: Clone>(
            bytes: &mut &'a [u8],
            shape: Shape,
            env: &Uiua,
            f: impl Fn(A) -> T,
            g: impl Fn(T) -> E,
        ) -> UiuaResult<Array<E>> {
            validate_size::<E>(shape.iter().copied(), env)?;
            let mut data = EcoVec::with_capacity(shape.elements());
            for i in 0..shape.elements() {
                if bytes.len() < size_of::<A>() {
                    return Err(env.error(format!("Missing data for element {i}")));
                }
                let elem =
                    f(A::try_from(&bytes[..size_of::<A>()]).unwrap_or_else(|_| unreachable!()));
                data.push(g(elem));
                *bytes = &bytes[size_of::<A>()..];
            }
            Ok(Array::new(shape, data))
        }
        let mut val: Value = match ty {
            BinType::U8 => make(bytes, shape, env, u8::from_le_bytes, |x| x)?.into(),
            BinType::U16 => make(bytes, shape, env, u16::from_le_bytes, |x| x as f64)?.into(),
            BinType::U32 => make(bytes, shape, env, u32::from_le_bytes, |x| x as f64)?.into(),
            BinType::U64 => make(bytes, shape, env, u64::from_le_bytes, |x| x as f64)?.into(),
            BinType::I8 => make(bytes, shape, env, i8::from_le_bytes, |x| x as f64)?.into(),
            BinType::I16 => make(bytes, shape, env, i16::from_le_bytes, |x| x as f64)?.into(),
            BinType::I32 => make(bytes, shape, env, i32::from_le_bytes, |x| x as f64)?.into(),
            BinType::I64 => make(bytes, shape, env, i64::from_le_bytes, |x| x as f64)?.into(),
            BinType::F32 => make(bytes, shape, env, f32::from_le_bytes, |x| x as f64)?.into(),
            BinType::F64 => make(bytes, shape, env, f64::from_le_bytes, |x| x)?.into(),
            BinType::Char => {
                if bytes.len() < size_of::<u32>() {
                    return Err(env.error("Missing byte count"));
                }
                let byte_count = u32::from_le_bytes(bytes[..size_of::<u32>()].try_into().unwrap());
                *bytes = &bytes[size_of::<u32>()..];
                if bytes.len() < byte_count as usize {
                    return Err(env.error("Missing character bytes"));
                }
                let s = str::from_utf8(&bytes[..byte_count as usize])
                    .map_err(|e| env.error(format!("Failed to parse string: {e}")))?;
                *bytes = &bytes[byte_count as usize..];
                let data: EcoVec<char> = s.chars().collect();
                if shape.elements() != data.len() {
                    return Err(env.error(format!(
                        "Shape implies {shape} characters, but got {}",
                        data.len()
                    )));
                }
                Array::new(shape, data).into()
            }
            BinType::Complex => make(bytes, shape, env, u128::from_le_bytes, |u| {
                let bytes = u.to_le_bytes();
                let re = f64::from_le_bytes(bytes[..size_of::<f64>()].try_into().unwrap());
                let im = f64::from_le_bytes(bytes[size_of::<f64>()..].try_into().unwrap());
                Complex::new(re, im)
            })?
            .into(),
            BinType::Box => {
                let mut data = EcoVec::with_capacity(shape.elements());
                for i in 0..shape.elements() {
                    let val = Self::from_binary_impl(bytes, depth + 1, env)
                        .map_err(|e| env.error(format!("Failed to parse box element {i}: {e}")))?;
                    data.push(Boxed(val));
                }
                Array::new(shape, data).into()
            }
        };
        if let Some((meta, map_keys)) = meta {
            val.meta = meta;
            if let Some(keys) = map_keys {
                val.map(keys, env)?;
            }
        }
        Ok(val)
    }
}
