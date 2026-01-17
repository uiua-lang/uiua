use std::{
    fmt::{self, Display},
    str::FromStr,
};

/// Data for how to send an argument type to `&ffi`
#[derive(Debug)]
pub struct FfiArg {
    /// The argument is an out parameter
    out: bool,
    /// The argument index where the length of the array goes if specified
    len_index: Option<usize>,
    /// The underlying C type of the argument
    ty: FfiType,
}

/// Types for FFI
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FfiType {
    Void,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Float,
    Double,
    UChar,
    UShort,
    UInt,
    ULong,
    ULongLong,
    Ptr(Box<Self>),
    Struct(Vec<Self>),
}

impl Display for FfiType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FfiType::Void => "void".to_string(),
                FfiType::Char => "char".to_string(),
                FfiType::Short => "short".to_string(),
                FfiType::Int => "int".to_string(),
                FfiType::Long => "long".to_string(),
                FfiType::LongLong => "long long".to_string(),
                FfiType::Float => "float".to_string(),
                FfiType::Double => "double".to_string(),
                FfiType::UChar => "uchar".to_string(),
                FfiType::UShort => "ushort".to_string(),
                FfiType::UInt => "uint".to_string(),
                FfiType::ULong => "ulong".to_string(),
                FfiType::ULongLong => "ulong long".to_string(),
                FfiType::Ptr(ty) => format!("{ty}*"),
                FfiType::Struct(fields) =>
                    if fields.iter().all(|f| *f == fields[0]) {
                        format!("{}[{}]", fields[0], fields.len())
                    } else {
                        fields
                            .iter()
                            .map(|field| field.to_string())
                            .collect::<Vec<_>>()
                            .join(";")
                    },
            }
        )
    }
}

impl Display for FfiArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{out}{ty}{index}",
            out = if self.out { "out " } else { "" },
            ty = self.ty,
            index = if let Some(len_index) = self.len_index {
                format!(":{len_index}")
            } else {
                "".to_string()
            }
        )
    }
}

impl FromStr for FfiType {
    type Err = String;

    fn from_str(input: &str) -> Result<FfiType, String> {
        let original = input;
        let input = input.trim();

        // Pointer
        if let Some(ptr) = input.strip_suffix("*") {
            return Ok(Self::Ptr(ptr.parse::<Self>()?.into()));
        }

        if let Some((arr, len)) = input.strip_suffix("]").and_then(|s| s.rsplit_once("[")) {
            let len = len.parse::<usize>().map_err(|e| e.to_string())?;
            let ty = arr.parse::<Self>()?;
            if len == 0 {
                return Err(format!("Array of {ty} cannot have zero elements"));
            }
            return Ok(Self::Struct(vec![ty.clone(); len]));
        }

        let (unsigned, input) = match input
            .strip_prefix("unsigned ")
            .or_else(|| input.strip_prefix("u"))
        {
            Some(scalar) => (true, scalar),
            None => (false, input),
        };

        // Scalar
        if let Some(ty) = match input {
            "void" => Some(Self::Void),
            "byte" | "bool" => Some(Self::UChar),
            "char" => Some(if unsigned { Self::UChar } else { Self::Char }),
            "short" => Some(if unsigned { Self::UShort } else { Self::Short }),
            "int" => Some(if unsigned { Self::UInt } else { Self::Int }),
            "long" | "long int" => Some(if unsigned { Self::ULong } else { Self::Long }),
            "long long" | "long long int" => Some(if unsigned {
                Self::ULongLong
            } else {
                Self::LongLong
            }),
            "float" => Some(Self::Float),
            "double" => Some(Self::Double),
            _ => None,
        } {
            return Ok(ty);
        };

        // Struct
        if let Some(body) = input
            .strip_prefix("{")
            .and_then(|s| s.strip_suffix("}"))
            .map(|s| s.trim_end_matches(";"))
        {
            let mut depth = 0_usize;
            let mut field = String::new();
            let mut fields = Vec::new();

            for c in body.chars() {
                if c != ';' || depth != 0 {
                    field.push(c);
                }
                match c {
                    '{' => depth += 1,
                    '}' => {
                        depth = depth
                            .checked_sub(1)
                            .ok_or_else(|| format!("Unmatched closing braces `{original}`"))?
                    }
                    ';' if depth == 0 => {
                        fields.push(field.parse::<FfiType>()?);
                        field = String::new();
                    }
                    _ => {}
                }
            }

            if !field.trim().is_empty() {
                fields.push(field.parse::<FfiType>()?);
            }

            if fields.is_empty() {
                return Err(format!("Cannot have an empty struct `{original}`"));
            }

            if depth != 0 {
                return Err(format!("Unmatched opening braces `{original}`"));
            } else {
                return Ok(Self::Struct(fields));
            }
        }

        Err(format!("Unknown C type `{original}`"))
    }
}

impl FromStr for FfiArg {
    type Err = String;

    fn from_str(input: &str) -> Result<FfiArg, String> {
        let input = input.trim();

        // Out parameters
        let (out, input) = match input.strip_prefix("out ") {
            Some(arg) => (true, arg),
            None => (false, input),
        };

        // Lists
        if let Some((arg, len_index)) = input.split_once(":") {
            let len_index = Some(
                len_index
                    .trim()
                    .parse::<usize>()
                    .map_err(|e| e.to_string())?,
            );
            let ty = FfiType::Ptr(arg.parse::<FfiType>()?.into());
            return Ok(FfiArg { out, len_index, ty });
        }

        // Regular types
        Ok(FfiArg {
            out,
            len_index: None,
            ty: input.parse::<FfiType>()?,
        })
    }
}

#[cfg(feature = "ffi")]
pub(crate) use enabled::*;
#[cfg(feature = "ffi")]
mod enabled {
    use crate::{Array, Boxed, MetaPtr, Value, cowslice::CowSlice};

    use super::*;
    use core::slice;
    use dashmap::DashMap;
    use libffi::{
        low::CodePtr,
        middle::{Arg, Cif, Type},
    };
    use libloading::Library;
    use std::{ffi::*, iter::zip};

    #[derive(Default)]
    pub struct FfiState {
        libraries: DashMap<String, Library>,
        buffers: DashMap<usize, Vec<u8>>,
    }

    impl FfiState {
        pub(crate) fn do_ffi(
            &self,
            file: &str,
            return_ty: FfiType,
            name: &str,
            arg_tys: &[FfiArg],
            mut arg_values: Vec<Value>,
        ) -> Result<Value, String> {
            let code_ptr = {
                if !self.libraries.contains_key(file) {
                    let lib =
                        unsafe { libloading::Library::new(file) }.map_err(|e| e.to_string())?;
                    self.libraries.insert(file.to_string(), lib);
                }
                let lib = self.libraries.get(file).expect("Library was loaded above");
                let func_ptr: libloading::Symbol<unsafe extern "C" fn()> =
                    unsafe { lib.get(name.as_bytes()) }.map_err(|e| e.to_string())?;
                CodePtr::from_fun(*func_ptr)
            };

            handle_len_indices(arg_tys, &mut arg_values)?;

            let c_arg_tys = arg_tys.iter().map(Type::from);
            let cif = Cif::new(c_arg_tys, Type::from(&return_ty));

            let mut reprs = Vec::new();
            let mut buffers = Vec::new();
            // Pointers to out parameters
            let mut out_params = Vec::new();

            for (arg, value) in zip(arg_tys, arg_values) {
                let value_len = value.row_count();
                let value_is_ptr = value.meta().pointer.is_some();

                let (repr, buffer) = arg.ty.repr(value)?;
                if arg.out {
                    if arg.ty.is_ptr() {
                        let ptr =
                            usize::from_ne_bytes(repr.clone().try_into().unwrap()) as *const u8;
                        reprs.push(repr);
                        out_params.push((
                            ptr,
                            arg.ty.clone(),
                            if value_is_ptr { None } else { Some(value_len) },
                        ));
                        for buffer in buffer {
                            self.buffers.insert(buffer.as_ptr() as usize, buffer);
                        }
                    } else {
                        let (out_repr, out_buffer) = FfiType::data_to_buffer(repr);
                        out_params.push((
                            usize::from_ne_bytes(out_repr) as *const u8,
                            arg.ty.clone(),
                            None,
                        ));
                        buffers.push(out_buffer);
                        buffers.extend(buffer);
                        reprs.push(out_repr.into());
                    }
                } else {
                    reprs.push(repr);
                    buffers.extend(buffer);
                }
            }

            let c_args = reprs
                .iter()
                .map(|repr| Arg::new(&repr[0]))
                .collect::<Vec<_>>();

            let return_repr = unsafe { call(&cif, code_ptr, &c_args, return_ty.size()) };
            let ret = return_ty.unrepr(&return_repr)?;

            let rets = if out_params.is_empty() {
                ret
            } else {
                let out_values = out_params
                    .into_iter()
                    .map(|(ptr, ty, len)| {
                        if let FfiType::Ptr(ty) = ty {
                            let ptr = MetaPtr::new(ptr as usize, *ty);
                            if let Some(len) = len {
                                ffi_copy(ptr, len)
                            } else {
                                let mut ptr_value = Value::default();
                                ptr_value.meta.pointer = Some(ptr);
                                Ok(ptr_value)
                            }
                        } else {
                            // SAFETY: this should never fail because the data should be allocated in [`buffers`]
                            let repr = unsafe { slice::from_raw_parts(ptr, ty.size()) };
                            ty.unrepr(repr)
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                if out_values.len() == 1 && return_ty == FfiType::Void {
                    let mut out_values = out_values;
                    out_values.pop().unwrap()
                } else {
                    Array::from_iter([ret].into_iter().chain(out_values).map(Boxed)).into()
                }
            };

            drop(reprs);
            drop(buffers);

            Ok(rets)
        }

        pub(crate) fn ffi_free(&self, ptr: &MetaPtr) {
            if !ptr.get().is_null() {
                if let Some((_, buffer)) = self.buffers.remove(&ptr.ptr) {
                    drop(buffer);
                } else {
                    unsafe { drop(Box::from_raw(ptr.get_mut())) };
                }
            }
        }
    }

    fn handle_len_indices(arg_tys: &[FfiArg], arg_values: &mut Vec<Value>) -> Result<(), String> {
        // The first number is the enumerated index, the second is the parameter index
        let mut len_indices = arg_tys
            .iter()
            .enumerate()
            .flat_map(|(i, arg)| arg.len_index.map(|index| (i, index)))
            .collect::<Vec<_>>();
        len_indices.sort_by_key(|tup| tup.1);
        let len_indices = len_indices;

        for &(i, len_index) in &len_indices {
            if let Some(arg) = arg_tys.get(len_index) {
                if !arg.is_num() {
                    return Err(format!(
                        "Argument at length index must be a number type, but it is {arg}"
                    ));
                }
            } else {
                return Err(format!(
                    "Length index {len_index} is out of bounds for function with {} arguments",
                    arg_tys.len()
                ));
            }

            if len_index == i {
                return Err(format!(
                    "Length index {len_index} cannot be the same as the array"
                ));
            }

            if len_indices
                .iter()
                .filter(|(_, index)| *index == len_index)
                .count()
                > 1
            {
                return Err(format!("Cannot have duplicate length index {len_index}"));
            }
        }

        for &(_, len_index) in &len_indices {
            arg_values.insert(len_index, Value::default());
        }

        for &(i, len_index) in &len_indices {
            arg_values[len_index] = Value::from(arg_values[i].row_count() as f64);
        }

        if arg_values.len() != arg_tys.len() {
            return Err(format!(
                "FFI function takes {} arguments, but {} were provided",
                arg_tys.len() - len_indices.len(),
                arg_values.len() - len_indices.len(),
            ));
        }

        Ok(())
    }

    /// This is an in-lined version of [`libffi::middle::Cif::call`] with the change that instead of using a generic to specify the size of memory that is copied into, it uses a parameter and a vector.
    pub unsafe fn call(cif: &Cif, fun: CodePtr, args: &[Arg], result_size: usize) -> Vec<u8> {
        let mut result = vec![0; result_size];
        unsafe {
            libffi::raw::ffi_call(
                cif.as_raw_ptr(),
                Some(*fun.as_safe_fun()),
                result.as_mut_ptr() as *mut c_void,
                args.as_ptr() as *mut *mut c_void,
            )
        };
        result
    }

    fn cstr_to_value(ptr: *const c_char) -> Result<Value, String> {
        let cstr = unsafe { CStr::from_ptr(ptr) };
        let str = cstr.to_str().map_err(|e| e.to_string())?;
        Ok(str.into())
    }

    pub(crate) fn ffi_copy(ptr: MetaPtr, len: usize) -> Result<Value, String> {
        if ptr.get().is_null() && len != 0 {
            return Err("Cannot read from a null pointer".to_string());
        }

        let size = ptr.ty.size();
        let repr = unsafe { slice::from_raw_parts(ptr.get(), len * size) };

        macro_rules! arr {
            ($c_ty:ty) => {
                Ok(Array::new(
                    len,
                    repr.chunks_exact(size)
                        .map(|chunk| <$c_ty>::from_ne_bytes(chunk.try_into().unwrap()) as f64)
                        .collect::<CowSlice<_>>(),
                )
                .into())
            };
        }

        match ptr.ty {
            FfiType::Void => Err("Cannot read from a void pointer".to_string()),
            FfiType::Short => arr!(c_short),
            FfiType::Int => arr!(c_int),
            FfiType::Long => arr!(c_long),
            FfiType::LongLong => arr!(c_longlong),
            FfiType::Float => arr!(c_float),
            FfiType::Double => arr!(c_double),
            FfiType::UChar => arr!(c_uchar),
            FfiType::UShort => arr!(c_ushort),
            FfiType::UInt => arr!(c_uint),
            FfiType::ULong => arr!(c_ulong),
            FfiType::ULongLong => arr!(c_ulonglong),
            _ => Ok(if ptr.ty.is_string() {
                Value::from_iter(
                    (0..len)
                        .map(|index| ptr.ty.unrepr(&repr[index * size..(index + 1) * size]))
                        .collect::<Result<Vec<_>, String>>()?
                        .into_iter()
                        .map(Boxed),
                )
            } else {
                Value::from_row_values_infallible(
                    (0..len)
                        .map(|index| ptr.ty.unrepr(&repr[index * size..(index + 1) * size]))
                        .collect::<Result<Vec<_>, String>>()?,
                )
            }),
        }
    }

    pub(crate) fn ffi_set(ptr: MetaPtr, index: usize, value: Value) -> Result<(), String> {
        if ptr.ptr == 0 {
            return Err("Cannot write to a null pointer".to_string());
        }
        let ty = ptr.ty;
        let size = ty.size();
        let offset = size * index;
        let dest = unsafe { slice::from_raw_parts_mut((ptr.ptr + offset) as *mut u8, size) };
        let (repr, _) = ty.repr(value)?;
        dest.copy_from_slice(&repr);
        Ok(())
    }

    impl FfiState {
        pub(crate) fn ffi_allocate(&self, size: usize) -> Result<MetaPtr, String> {
            if size == 0 {
                return Err("Cannot allocate zero bytes".to_string());
            }
            let mut buffer = vec![0u8; size];
            let ptr = buffer.as_mut_ptr() as usize;
            self.buffers.insert(ptr, buffer);
            Ok(MetaPtr::new(ptr, FfiType::UChar))
        }
    }

    impl FfiArg {
        fn is_list(&self) -> bool {
            self.len_index.is_some()
        }

        fn is_ptr(&self) -> bool {
            self.out || self.is_list() || matches!(self.ty, FfiType::Ptr(_))
        }

        fn is_num(&self) -> bool {
            self.ty.is_num() && !self.is_list()
        }
    }

    impl From<&FfiArg> for Type {
        fn from(arg: &FfiArg) -> Self {
            if arg.is_ptr() {
                Type::pointer()
            } else {
                Type::from(&arg.ty)
            }
        }
    }

    impl From<&FfiType> for Type {
        fn from(ty: &FfiType) -> Self {
            match ty {
                FfiType::Void => Type::void(),
                FfiType::Char => Type::c_schar(),
                FfiType::Short => Type::c_short(),
                FfiType::Int => Type::c_int(),
                FfiType::Long => Type::c_long(),
                FfiType::LongLong => Type::c_longlong(),
                FfiType::Float => Type::f32(),
                FfiType::Double => Type::f64(),
                FfiType::UChar => Type::c_uchar(),
                FfiType::UShort => Type::c_ushort(),
                FfiType::UInt => Type::c_uint(),
                FfiType::ULong => Type::c_ulong(),
                FfiType::ULongLong => Type::c_ulonglong(),
                FfiType::Ptr(_) => Type::pointer(),
                FfiType::Struct(fields) => Type::structure(fields.iter().map(Type::from)),
            }
        }
    }

    type Buffers = (Vec<u8>, Vec<Vec<u8>>);

    impl FfiType {
        fn is_ptr(&self) -> bool {
            matches!(self, FfiType::Ptr(_))
        }

        fn is_num(&self) -> bool {
            matches!(
                self,
                FfiType::Short
                    | FfiType::Int
                    | FfiType::Long
                    | FfiType::LongLong
                    | FfiType::Float
                    | FfiType::Double
                    | FfiType::UChar
                    | FfiType::UShort
                    | FfiType::UInt
                    | FfiType::ULong
                    | FfiType::ULongLong
            )
        }

        fn is_string(&self) -> bool {
            matches!(self, FfiType::Ptr(ty) if **ty == FfiType::Char)
        }

        fn size_align(&self) -> (usize, usize) {
            match self {
                FfiType::Void => (0, 1),
                FfiType::Char => (size_of::<c_char>(), align_of::<c_char>()),
                FfiType::Short => (size_of::<c_short>(), align_of::<c_short>()),
                FfiType::Int => (size_of::<c_int>(), align_of::<c_int>()),
                FfiType::Long => (size_of::<c_long>(), align_of::<c_long>()),
                FfiType::LongLong => (size_of::<c_longlong>(), align_of::<c_longlong>()),
                FfiType::Float => (size_of::<c_float>(), align_of::<c_float>()),
                FfiType::Double => (size_of::<c_double>(), align_of::<c_double>()),
                FfiType::UChar => (size_of::<c_uchar>(), align_of::<c_uchar>()),
                FfiType::UShort => (size_of::<c_ushort>(), align_of::<c_ushort>()),
                FfiType::UInt => (size_of::<c_uint>(), align_of::<c_uint>()),
                FfiType::ULong => (size_of::<c_ulong>(), align_of::<c_ulong>()),
                FfiType::ULongLong => (size_of::<c_ulonglong>(), align_of::<c_ulonglong>()),
                FfiType::Ptr(_) => (size_of::<usize>(), align_of::<usize>()),
                FfiType::Struct(fields) => FfiType::struct_size_align_offsets(fields).0,
            }
        }

        fn struct_size_align_offsets(fields: &[FfiType]) -> ((usize, usize), Vec<usize>) {
            let align = fields
                .iter()
                .map(FfiType::align)
                .max()
                .expect("Struct must have at least one field");

            let mut offsets = Vec::new();
            let mut size = 0;
            for field in fields {
                let (field_size, field_align) = field.size_align();
                if size % field_align != 0 {
                    size += field_align - (size % field_align);
                }
                offsets.push(size);
                size += field_size;
            }
            size = size.div_ceil(align) * align;

            ((size, align), offsets)
        }

        #[allow(unused)]
        fn size(&self) -> usize {
            self.size_align().0
        }

        fn align(&self) -> usize {
            self.size_align().1
        }

        fn repr(&self, value: Value) -> Result<Buffers, String> {
            if self == &FfiType::Void {
                return Err("Void cannot be in an argument type".to_string());
            }

            if let Some(ptr) = value.meta.pointer.as_ref() {
                return if matches!(self, FfiType::Ptr(_)) {
                    Ok((ptr.ptr.to_ne_bytes().to_vec(), Vec::new()))
                } else {
                    Err("Argument is a pointer, but the type is not a pointer type".to_string())
                };
            }

            let is_scalar = value.shape.is_empty();

            macro_rules! scalar {
                ($arr:expr, $c_ty:ty) => {{
                    if !is_scalar {
                        return Err(format!("Array must be a scalar for C type {}", self));
                    }
                    (($arr.data[0] as $c_ty).to_ne_bytes().to_vec(), Vec::new())
                }};
            }

            Ok(match (self, value) {
                (FfiType::Char, Value::Char(arr)) => scalar!(arr, c_char),
                (FfiType::Char, Value::Byte(arr)) => scalar!(arr, c_char),
                (FfiType::Char, Value::Num(arr)) => scalar!(arr, c_char),
                (FfiType::Short, Value::Byte(arr)) => scalar!(arr, c_short),
                (FfiType::Short, Value::Num(arr)) => scalar!(arr, c_short),
                (FfiType::Int, Value::Byte(arr)) => scalar!(arr, c_int),
                (FfiType::Int, Value::Num(arr)) => scalar!(arr, c_int),
                (FfiType::Long, Value::Byte(arr)) => scalar!(arr, c_long),
                (FfiType::Long, Value::Num(arr)) => scalar!(arr, c_long),
                (FfiType::LongLong, Value::Byte(arr)) => scalar!(arr, c_longlong),
                (FfiType::LongLong, Value::Num(arr)) => scalar!(arr, c_longlong),
                (FfiType::Float, Value::Byte(arr)) => scalar!(arr, c_float),
                (FfiType::Float, Value::Num(arr)) => scalar!(arr, c_float),
                (FfiType::Double, Value::Byte(arr)) => scalar!(arr, c_double),
                (FfiType::Double, Value::Num(arr)) => scalar!(arr, c_double),
                (FfiType::UChar, Value::Char(arr)) => scalar!(arr, c_uchar),
                (FfiType::UChar, Value::Byte(arr)) => scalar!(arr, c_uchar),
                (FfiType::UChar, Value::Num(arr)) => scalar!(arr, c_uchar),
                (FfiType::UShort, Value::Byte(arr)) => scalar!(arr, c_ushort),
                (FfiType::UShort, Value::Num(arr)) => scalar!(arr, c_ushort),
                (FfiType::UInt, Value::Byte(arr)) => scalar!(arr, c_uint),
                (FfiType::UInt, Value::Num(arr)) => scalar!(arr, c_uint),
                (FfiType::ULong, Value::Byte(arr)) => scalar!(arr, c_ulong),
                (FfiType::ULong, Value::Num(arr)) => scalar!(arr, c_ulong),
                (FfiType::ULongLong, Value::Byte(arr)) => scalar!(arr, c_ulonglong),
                (FfiType::ULongLong, Value::Num(arr)) => scalar!(arr, c_ulonglong),
                (FfiType::Ptr(ty), Value::Byte(arr)) if **ty == FfiType::Void => {
                    scalar!(arr, usize)
                }
                (FfiType::Ptr(ty), Value::Num(arr)) if **ty == FfiType::Void => scalar!(arr, usize),
                (FfiType::Ptr(ty), value) => ty.repr_arr(value, true)?,
                (FfiType::Struct(fields), value) => FfiType::repr_struct(fields, value)?,

                (ty, value) => {
                    return Err(format!(
                        "Array of {} is unsupported for FFI argument {ty}",
                        value.type_name_plural()
                    ));
                }
            })
        }

        /// Marshall some bytes containing a C type into a [`Value`].
        /// Assumes length of the bytes is the same as the size of the type.
        fn unrepr(&self, repr: &[u8]) -> Result<Value, String> {
            macro_rules! value {
                ($c_ty:ty $(, $into:ty)?) => {
                    <$c_ty>::from_ne_bytes(repr.try_into().expect("repr slice is the same size as the type")) $(as $into)?
                };
            }
            Ok(match self {
                FfiType::Void => Value::default(),
                FfiType::Char => value!(c_uchar, char).into(),
                FfiType::Short => value!(c_short, f64).into(),
                FfiType::Int => value!(c_int, f64).into(),
                FfiType::Long => value!(c_long, f64).into(),
                FfiType::LongLong => value!(c_longlong, f64).into(),
                FfiType::Float => value!(c_float, f64).into(),
                FfiType::Double => value!(c_double, f64).into(),
                FfiType::UChar => value!(c_uchar, u8).into(),
                FfiType::UShort => value!(c_ushort, f64).into(),
                FfiType::UInt => value!(c_uint, f64).into(),
                FfiType::ULong => value!(c_ulong, f64).into(),
                FfiType::ULongLong => value!(c_ulonglong, f64).into(),
                FfiType::Ptr(ty) => {
                    if **ty == FfiType::Char {
                        cstr_to_value(value!(usize, *const c_char))?
                    } else {
                        let mut ptr = Value::default();
                        ptr.meta.pointer = Some(MetaPtr::new(value!(usize), (**ty).clone()));
                        ptr
                    }
                }
                FfiType::Struct(fields) => FfiType::unrepr_struct(fields, repr)?,
            })
        }

        fn data_to_buffer(data: Vec<u8>) -> ([u8; size_of::<usize>()], Vec<u8>) {
            let ptr = (data.as_ptr() as usize).to_ne_bytes();
            (ptr, data)
        }

        fn repr_struct(fields: &[FfiType], value: Value) -> Result<Buffers, String> {
            if fields.len() != value.row_count() {
                return Err(format!(
                    "Struct has {} fields, but passed array has {} rows",
                    fields.len(),
                    value.row_count()
                ));
            }

            let ((size, _), offsets) = FfiType::struct_size_align_offsets(fields);

            let mut struct_repr = vec![0u8; size];
            let mut buffers = Vec::new();

            for ((offset, field_ty), row) in zip(offsets, fields).zip(value.into_rows()) {
                let row = row.unpacked();
                let (field_repr, buffer) = field_ty.repr(row)?;
                buffers.extend(buffer.into_iter());

                struct_repr[offset..offset + field_repr.len()].copy_from_slice(&field_repr);
            }

            Ok((struct_repr, buffers))
        }

        fn unrepr_struct(fields: &[FfiType], repr: &[u8]) -> Result<Value, String> {
            let (_, offsets) = FfiType::struct_size_align_offsets(fields);

            let rows = zip(fields, offsets)
                .map(|(ty, offset)| ty.unrepr(&repr[offset..offset + ty.size()]))
                .collect::<Result<Vec<_>, _>>()?;

            let value = if fields.iter().all(FfiType::is_num) {
                Value::from_row_values_infallible(rows)
            } else {
                Array::from_iter(rows.into_iter().map(Boxed)).into()
            };

            Ok(value)
        }

        fn repr_arr(&self, value: Value, strings: bool) -> Result<Buffers, String> {
            let rank = value.shape.len();
            let is_list = rank == 1;

            let is_empty = value.row_count() == 0;

            macro_rules! arr {
                ($arr:expr, $c_type:ty) => {{
                    if !is_list {
                        return Err(format!("Array must be rank 1 to become a list of {self}, but it was rank {rank}"));
                    }
                    if is_empty {
                        return Ok(((0_usize).to_ne_bytes().into(), Vec::new()));
                    }
                    let (ptr, buffer) = FfiType::data_to_buffer(
                        $arr.data
                            .iter()
                            .flat_map(|&n| (n as $c_type).to_ne_bytes())
                            .collect::<Vec<u8>>(),
                    );
                    (ptr.into(), vec![buffer])
                }};
            }

            macro_rules! string {
                ($arr:expr, $tochar:expr) => {{
                    if !is_list {
                        return Err(format!(
                            "Array must be rank 1 to become a string, but it was rank {rank}"
                        ));
                    }
                    let (ptr, buffer) = FfiType::data_to_buffer(
                        $arr.data
                            .iter()
                            .map($tochar)
                            .collect::<String>()
                            .bytes()
                            .chain([b'\0'])
                            .collect(),
                    );
                    (ptr.into(), vec![buffer])
                }};
            }

            Ok(match (self, value) {
                (FfiType::Char, Value::Char(arr)) if strings => string!(arr, |&b| b),
                (FfiType::Char, Value::Byte(arr)) if strings => string!(arr, |&b| b as char),
                (FfiType::Char, Value::Num(arr)) if strings => {
                    string!(arr, |&b| char::from_u32(b as u32).unwrap_or_default())
                }

                (FfiType::Char, Value::Char(arr)) => arr!(arr, c_char),
                (FfiType::Char, Value::Byte(arr)) => arr!(arr, c_char),
                (FfiType::Char, Value::Num(arr)) => arr!(arr, c_char),
                (FfiType::Short, Value::Byte(arr)) => arr!(arr, c_short),
                (FfiType::Short, Value::Num(arr)) => arr!(arr, c_short),
                (FfiType::Int, Value::Byte(arr)) => arr!(arr, c_int),
                (FfiType::Int, Value::Num(arr)) => arr!(arr, c_int),
                (FfiType::Long, Value::Byte(arr)) => arr!(arr, c_long),
                (FfiType::Long, Value::Num(arr)) => arr!(arr, c_long),
                (FfiType::LongLong, Value::Byte(arr)) => arr!(arr, c_longlong),
                (FfiType::LongLong, Value::Num(arr)) => arr!(arr, c_longlong),
                (FfiType::Float, Value::Byte(arr)) => arr!(arr, c_float),
                (FfiType::Float, Value::Num(arr)) => arr!(arr, c_float),
                (FfiType::Double, Value::Byte(arr)) => arr!(arr, c_double),
                (FfiType::Double, Value::Num(arr)) => arr!(arr, c_double),
                (FfiType::UChar, Value::Char(arr)) => arr!(arr, c_uchar),
                (FfiType::UChar, Value::Byte(arr)) => arr!(arr, c_uchar),
                (FfiType::UChar, Value::Num(arr)) => arr!(arr, c_uchar),
                (FfiType::UShort, Value::Byte(arr)) => arr!(arr, c_ushort),
                (FfiType::UShort, Value::Num(arr)) => arr!(arr, c_ushort),
                (FfiType::UInt, Value::Byte(arr)) => arr!(arr, c_uint),
                (FfiType::UInt, Value::Num(arr)) => arr!(arr, c_uint),
                (FfiType::ULong, Value::Byte(arr)) => arr!(arr, c_ulong),
                (FfiType::ULong, Value::Num(arr)) => arr!(arr, c_ulong),
                (FfiType::ULongLong, Value::Byte(arr)) => arr!(arr, c_ulonglong),
                (FfiType::ULongLong, Value::Num(arr)) => arr!(arr, c_ulonglong),
                (ty, arr) => {
                    let (row_reprs, buffers): (Vec<Vec<u8>>, Vec<Vec<Vec<u8>>>) = arr
                        .rows()
                        .map(|row| ty.repr(row.unpacked()))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .unzip();

                    let arr_repr = row_reprs.into_iter().flatten().collect::<Vec<_>>();
                    let (ptr, buffer) = FfiType::data_to_buffer(arr_repr);
                    let buffers = buffers
                        .into_iter()
                        .flatten()
                        .chain([buffer])
                        .collect::<Vec<_>>();

                    (ptr.into(), buffers)
                }
            })
        }
    }
}
