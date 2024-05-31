use std::{
    ffi::*,
    fmt,
    mem::{align_of, size_of},
    str::FromStr,
};

#[allow(dead_code)]
pub(crate) const DEBUG: bool = false;

/// Types for FFI
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
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
    Ptr {
        mutable: bool,
        inner: Box<Self>,
    },
    List {
        mutable: bool,
        len_index: usize,
        inner: Box<Self>,
    },
    Struct {
        fields: Vec<Self>,
    },
}

impl FromStr for FfiType {
    type Err = String;
    fn from_str(mut s: &str) -> Result<Self, String> {
        s = s.trim();
        // Parse const
        let mut mutable = true;
        if let Some(t) = s.strip_prefix("const ") {
            s = t;
            mutable = false;
        }
        // Parse list
        if let Some((mut a, mut b)) = s.rsplit_once(':') {
            a = a.trim();
            b = b.trim();
            let len_index = b
                .parse()
                .map_err(|e| format!("Invalid length index: {e}"))?;
            return Ok(FfiType::List {
                mutable,
                len_index,
                inner: Box::new(a.parse()?),
            });
        }
        // Parse pointer
        if let Some(mut s) = s.strip_suffix('*') {
            s = s.trim();
            let mut inner: Self = s.parse()?;
            if let FfiType::Ptr {
                mutable: m @ true, ..
            } = &mut inner
            {
                *m = mutable;
                mutable = true;
            }
            return Ok(FfiType::Ptr {
                mutable,
                inner: Box::new(inner),
            });
        }
        if let Some(mut s) = s.strip_prefix('{').and_then(|s| s.strip_suffix('}')) {
            s = s.trim();
            let mut fields = Vec::new();
            let mut curr = String::new();
            let mut depth = 0;
            for c in s.chars() {
                match c {
                    '{' => {
                        depth += 1;
                        curr.push(c);
                    }
                    '}' => {
                        depth -= 1;
                        curr.push(c);
                    }
                    ';' if depth == 0 => {
                        fields.push(curr.parse()?);
                        curr.clear();
                    }
                    _ => curr.push(c),
                }
            }
            if !curr.is_empty() {
                fields.push(curr.parse()?);
            }
            return Ok(FfiType::Struct { fields });
        }
        Ok(match s {
            "void" => FfiType::Void,
            "char" => FfiType::Char,
            "short" => FfiType::Short,
            "int" => FfiType::Int,
            "long" => FfiType::Long,
            "long long" => FfiType::LongLong,
            "float" => FfiType::Float,
            "double" => FfiType::Double,
            "unsigned char" => FfiType::UChar,
            "unsigned short" => FfiType::UShort,
            "unsigned int" => FfiType::UInt,
            "unsigned long" => FfiType::ULong,
            "unsigned long long" => FfiType::ULongLong,
            _ => return Err(format!("Unknown FFI type: {}", s)),
        })
    }
}

impl fmt::Display for FfiType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FfiType::Void => write!(f, "void"),
            FfiType::Char => write!(f, "char"),
            FfiType::Short => write!(f, "short"),
            FfiType::Int => write!(f, "int"),
            FfiType::Long => write!(f, "long"),
            FfiType::LongLong => write!(f, "long long"),
            FfiType::Float => write!(f, "float"),
            FfiType::Double => write!(f, "double"),
            FfiType::UChar => write!(f, "unsigned char"),
            FfiType::UShort => write!(f, "unsigned short"),
            FfiType::UInt => write!(f, "unsigned int"),
            FfiType::ULong => write!(f, "unsigned long"),
            FfiType::ULongLong => write!(f, "unsigned long long"),
            FfiType::Ptr { mutable, inner } => {
                write!(f, "{}{inner}*", if *mutable { "" } else { "const " })
            }
            FfiType::List {
                mutable,
                len_index,
                inner,
            } => write!(
                f,
                "{}{}:{}",
                if *mutable { "" } else { "const " },
                inner,
                len_index,
            ),
            FfiType::Struct { fields } => {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl FfiType {
    /// Get the C-ABI-compatible size and alignment of a type
    pub fn size_align(&self) -> (usize, usize) {
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
            FfiType::Ptr { .. } | FfiType::List { .. } => (size_of::<usize>(), align_of::<usize>()),
            FfiType::Struct { fields } => struct_fields_size_align(fields),
        }
    }
    /// Check if a type is a scalar type
    pub fn is_scalar(&self) -> bool {
        match self {
            FfiType::Void | FfiType::Ptr { .. } | FfiType::List { .. } => false,
            FfiType::Struct { fields } => fields.iter().all(|f| f.is_scalar() && *f == fields[0]),
            _ => true,
        }
    }
}

fn struct_fields_size_align(fields: &[FfiType]) -> (usize, usize) {
    let mut size = 0;
    let mut align = 1;
    for field in fields {
        let (field_size, field_align) = field.size_align();
        // println!("size_align of field {field}: {field_size}, {field_align}");
        align = align.max(field_align);
        if size % field_align != 0 {
            size += field_align - (size % field_align);
        }
        size += field_size;
    }
    size = (size + align - 1) / align * align;
    // println!("size_align of struct {fields:?}: {size}, {align}");
    (size, align)
}

#[cfg(feature = "ffi")]
pub(crate) use enabled::*;
#[cfg(feature = "ffi")]
mod enabled {
    use std::{
        any::{type_name, Any},
        mem::{forget, take, transmute},
        slice,
    };

    use dashmap::DashMap;
    use ecow::EcoVec;
    use libffi::middle::*;

    use super::*;
    use crate::{Array, Boxed, MetaPtr, Value};

    macro_rules! dbgln {
        ($($arg:tt)*) => {
            if DEBUG {
                println!($($arg)*);
            }
        }
    }

    #[derive(Default)]
    pub struct FfiState {
        libraries: DashMap<String, libloading::Library>,
    }

    impl FfiState {
        pub(crate) fn do_ffi(
            &self,
            file: &str,
            return_ty: FfiType,
            name: &str,
            arg_tys: &[FfiType],
            args: &[Value],
        ) -> Result<Value, String> {
            dbgln!("call FFI function {name}");
            if !self.libraries.contains_key(file) {
                let lib = unsafe { libloading::Library::new(file) }.map_err(|e| e.to_string())?;
                self.libraries.insert(file.to_string(), lib);
            }
            let lib = self.libraries.get(file).unwrap();
            let fptr: libloading::Symbol<unsafe extern "C" fn()> =
                unsafe { lib.get(name.as_bytes()) }.map_err(|e| e.to_string())?;

            let mut cif_arg_tys = Vec::new();
            let mut bindings = FfiBindings::default();
            let mut lengths: Vec<Option<usize>> = vec![None; arg_tys.len()];
            // Collect lengths of lists
            for (i, arg_ty) in arg_tys.iter().enumerate() {
                if let FfiType::List {
                    len_index, inner, ..
                } = arg_ty
                {
                    let j = i - lengths[..i].iter().filter(|l| l.is_some()).count();
                    let len = lengths
                        .get_mut(*len_index)
                        .ok_or_else(|| format!("Invalid length index: {len_index}"))?;
                    let arg = args.get(j);
                    *len = if let FfiType::Struct { .. } = &**inner {
                        arg.map(Value::row_count)
                    } else {
                        arg.map(Value::element_count)
                    };
                }
            }
            // Bind arguments
            let mut args = args.iter();
            for (i, arg_ty) in arg_tys.iter().enumerate() {
                cif_arg_tys.push(ffity_to_cty(arg_ty));
                if let Some(len) = lengths[i] {
                    // Bind length
                    dbgln!("bind {i} len: {len}");
                    _ = match arg_ty {
                        FfiType::Int => bindings.push_value(len as c_int),
                        FfiType::UInt => bindings.push_value(len as c_uint),
                        FfiType::Long => bindings.push_value(len as c_long),
                        FfiType::ULong => bindings.push_value(len as c_ulong),
                        FfiType::LongLong => bindings.push_value(len as c_longlong),
                        FfiType::ULongLong => bindings.push_value(len as c_ulonglong),
                        FfiType::Ptr { inner, .. } => match &**inner {
                            FfiType::Int => bindings.alloc_and_push_ptr_to(len as c_int),
                            FfiType::UInt => bindings.alloc_and_push_ptr_to(len as c_uint),
                            FfiType::Long => bindings.alloc_and_push_ptr_to(len as c_long),
                            FfiType::ULong => bindings.alloc_and_push_ptr_to(len as c_ulong),
                            FfiType::LongLong => bindings.alloc_and_push_ptr_to(len as c_longlong),
                            FfiType::ULongLong => {
                                bindings.alloc_and_push_ptr_to(len as c_ulonglong)
                            }
                            _ => {
                                return Err(format!("{arg_ty} is not a valid FFI type for lengths"))
                            }
                        },
                        ty => return Err(format!("{ty} is not a valid FFI type for lengths")),
                    };
                } else {
                    // Bind normal argument
                    let arg = args.next().ok_or("Not enough arguments")?;
                    dbgln!("bind {i} arg: {arg:?}");
                    dbgln!("  as {arg_ty}");
                    bindings.bind_arg(i, arg_ty, arg)?;
                }
            }
            if args.next().is_some() {
                return Err("Too many arguments".into());
            }

            // Call and get return value
            assert_eq!(
                cif_arg_tys.len(),
                bindings.args.len(),
                "Expected {} arguments, got {}",
                cif_arg_tys.len(),
                bindings.args.len()
            );
            let cif = Cif::new(cif_arg_tys, ffity_to_cty(&return_ty));
            let fptr = CodePtr::from_fun(*fptr);
            let mut results = Vec::new();

            macro_rules! call {
                ($ty:ty) => {
                    results.push((unsafe { cif.call::<$ty>(fptr, &bindings.args) } as f64).into())
                };
            }
            macro_rules! ret_list {
                ($c_ty:ty, $len_index:expr) => {
                    unsafe {
                        // Call
                        let ptr = cif.call::<*const $c_ty>(fptr, &bindings.args);
                        // Construct a list from the pointer and length
                        let len = *bindings.get::<c_int>(*$len_index) as usize;
                        let slice = slice::from_raw_parts(ptr, len);
                        // Copy the slice into a new array
                        results.push(
                            Array::new(len, slice.iter().map(|&i| i as f64).collect::<EcoVec<_>>())
                                .into(),
                        );
                        // Clean up the pointer's memory
                        drop(Vec::from_raw_parts(ptr as *mut $c_ty, len, len));
                    }
                };
            }

            macro_rules! ret_ptr {
                ($ty:ty) => {{
                    let ptr = unsafe { cif.call::<*const $ty>(fptr, &bindings.args) };
                    let mut val = Value::default();
                    val.meta_mut().pointer = Some(MetaPtr::new(ptr, true));
                    results.push(val);
                }};
            }

            match &return_ty {
                FfiType::Void => unsafe { cif.call::<()>(fptr, &bindings.args) },
                FfiType::Char => call!(c_schar),
                FfiType::Short => call!(c_short),
                FfiType::Int => call!(c_int),
                FfiType::Long => call!(c_long),
                FfiType::LongLong => call!(c_longlong),
                FfiType::UChar => call!(c_uchar),
                FfiType::UShort => call!(c_ushort),
                FfiType::UInt => call!(c_uint),
                FfiType::ULong => call!(c_ulong),
                FfiType::ULongLong => call!(c_ulonglong),
                FfiType::Float => call!(c_float),
                FfiType::Double => call!(c_double),
                FfiType::Ptr { inner, .. } => match &**inner {
                    FfiType::Char => unsafe {
                        let ptr = cif.call::<*const c_char>(fptr, &bindings.args);
                        let s = CStr::from_ptr(ptr).to_str().map_err(|e| e.to_string())?;
                        results.push(Value::from(s))
                    },
                    FfiType::Struct { fields } => unsafe {
                        let ptr = cif.call::<*const u8>(fptr, &bindings.args);
                        let (size, _) = struct_fields_size_align(fields);
                        let slice = slice::from_raw_parts(ptr, size);
                        results.push(bindings.struct_repr_to_value(slice, fields)?);
                        // Clean up the pointer's memory
                        drop(Vec::from_raw_parts(ptr as *mut u8, size, size));
                    },
                    FfiType::Void => ret_ptr!(()),
                    FfiType::UChar => ret_ptr!(c_uchar),
                    FfiType::Short => ret_ptr!(c_short),
                    FfiType::UShort => ret_ptr!(c_ushort),
                    FfiType::Int => ret_ptr!(c_int),
                    FfiType::UInt => ret_ptr!(c_uint),
                    FfiType::Long => ret_ptr!(c_long),
                    FfiType::ULong => ret_ptr!(c_ulong),
                    FfiType::LongLong => ret_ptr!(c_longlong),
                    FfiType::ULongLong => ret_ptr!(c_ulonglong),
                    FfiType::Float => ret_ptr!(c_float),
                    FfiType::Double => ret_ptr!(c_double),
                    _ => {
                        return Err(format!(
                            "Invalid or unsupported FFI return type {return_ty}"
                        ))
                    }
                },
                FfiType::List {
                    len_index, inner, ..
                } => match &**inner {
                    FfiType::Char => ret_list!(c_char, len_index),
                    FfiType::UChar => ret_list!(c_uchar, len_index),
                    FfiType::Short => ret_list!(c_short, len_index),
                    FfiType::UShort => ret_list!(c_ushort, len_index),
                    FfiType::Int => ret_list!(c_int, len_index),
                    FfiType::UInt => ret_list!(c_uint, len_index),
                    FfiType::Long => ret_list!(c_long, len_index),
                    FfiType::ULong => ret_list!(c_ulong, len_index),
                    FfiType::LongLong => ret_list!(c_longlong, len_index),
                    FfiType::ULongLong => ret_list!(c_ulonglong, len_index),
                    FfiType::Float => ret_list!(c_float, len_index),
                    FfiType::Double => ret_list!(c_double, len_index),
                    _ => {
                        return Err(format!(
                            "Invalid or unsupported FFI return type {return_ty}"
                        ))
                    }
                },
                FfiType::Struct { fields } => {
                    let (size, _) = return_ty.size_align();
                    let args = &bindings.args;
                    macro_rules! call_ret_struct {
                        ($n:literal) => {
                            bindings.struct_repr_to_value(
                                &unsafe { cif.call::<[u8; $n]>(fptr, args) },
                                fields,
                            )
                        };
                    }
                    let val = match size {
                        0 => Value::default(),
                        1 => call_ret_struct!(1)?,
                        2 => call_ret_struct!(2)?,
                        4 => call_ret_struct!(4)?,
                        8 => call_ret_struct!(8)?,
                        12 => call_ret_struct!(12)?,
                        16 => call_ret_struct!(16)?,
                        24 => call_ret_struct!(24)?,
                        32 => call_ret_struct!(32)?,
                        48 => call_ret_struct!(48)?,
                        64 => call_ret_struct!(64)?,
                        128 => call_ret_struct!(128)?,
                        192 => call_ret_struct!(192)?,
                        256 => call_ret_struct!(256)?,
                        384 => call_ret_struct!(384)?,
                        512 => call_ret_struct!(512)?,
                        n => return Err(format!("Unsupported return struct size: {n}")),
                    };
                    results.push(val);
                }
            }

            // Get out parameters
            macro_rules! out_param_scalar {
                ($ty:ty, $i:expr, $numty:ty ) => {
                    match bindings.get_maybe_null::<$ty>($i) {
                        Some((&val, ptr)) => {
                            let mut val = Value::from(val as $numty);
                            if let Some(ptr) = ptr {
                                val.meta_mut().pointer = Some(MetaPtr::new(ptr, false));
                            }
                            results.push(val)
                        }
                        None => results.push(Value::null()),
                    }
                };
            }
            macro_rules! out_param_list {
                ($c_ty:ty, $len_index:expr, $i:expr, $numty:ty $(,$numty2:ty)?) => {
                    unsafe {
                        let len = *bindings.get::<c_int>(*$len_index) as usize;
                        let (ptr, vec) = bindings.get_list_mut::<$c_ty>($i);
                        // Construct a list from the pointer and length
                        let slice = slice::from_raw_parts(ptr, len);
                        // Copy the slice into a new array
                        results.push(
                            Array::new(
                                len,
                                slice.iter().map(|&i| i as $numty $(as $numty2)?).collect::<EcoVec<_>>(),
                            )
                            .into(),
                        );
                        // Forget the vector
                        forget(take(vec));
                        // Clean up the pointer's memory
                        drop(Vec::from_raw_parts(ptr as *mut $c_ty, len, len));
                    }
                };
            }
            for (i, ty) in arg_tys.iter().enumerate() {
                match ty {
                    FfiType::Ptr {
                        mutable: true,
                        inner,
                    } => {
                        if lengths[i].is_some() {
                            continue;
                        }
                        dbgln!("out {i} arg: {ty}");
                        match &**inner {
                            FfiType::Char => unsafe {
                                let ptr = bindings.get::<c_char>(i);
                                let s = CStr::from_ptr(ptr).to_str().map_err(|e| e.to_string())?;
                                results.push(Value::from(s))
                            },
                            FfiType::UChar => out_param_scalar!(c_uchar, i, u8),
                            FfiType::Short => out_param_scalar!(c_short, i, f64),
                            FfiType::UShort => out_param_scalar!(c_ushort, i, f64),
                            FfiType::Int => out_param_scalar!(c_int, i, f64),
                            FfiType::UInt => out_param_scalar!(c_uint, i, f64),
                            FfiType::Long => out_param_scalar!(c_long, i, f64),
                            FfiType::ULong => out_param_scalar!(c_ulong, i, f64),
                            FfiType::LongLong => out_param_scalar!(c_longlong, i, f64),
                            FfiType::ULongLong => out_param_scalar!(c_ulonglong, i, f64),
                            FfiType::Float => out_param_scalar!(c_float, i, f64),
                            FfiType::Double => out_param_scalar!(c_double, i, f64),
                            FfiType::Struct { fields } => {
                                let repr = bindings.get_repr(i);
                                results.push(bindings.struct_repr_to_value(repr, fields)?);
                            }
                            FfiType::Ptr { inner, .. } => match &**inner {
                                FfiType::Char => unsafe {
                                    let ptr = *bindings.get::<*mut ()>(i) as *mut *const c_char;
                                    dbgln!("    outer ptr to char: {ptr:p}");
                                    let ptr = *ptr;
                                    dbgln!("    inner ptr to char: {ptr:p}");
                                    results.push(if ptr.is_null() {
                                        let mut val = Value::default();
                                        val.meta_mut().pointer = Some(MetaPtr::new(ptr, true));
                                        val
                                    } else {
                                        let s = CStr::from_ptr(ptr)
                                            .to_str()
                                            .map_err(|e| e.to_string())?;
                                        Value::from(s)
                                    })
                                },
                                FfiType::Void => unsafe {
                                    let ptr = *bindings.get::<*mut ()>(i) as *mut *const ();
                                    dbgln!("    outer ptr to void: {ptr:p}");
                                    let ptr = *ptr;
                                    dbgln!("    inner ptr to void: {ptr:p}");
                                    let mut val = Value::from(ptr as usize);
                                    val.meta_mut().pointer = Some(MetaPtr::new(ptr, true));
                                    results.push(val);
                                },
                                _ => {
                                    let ptr = *bindings.get::<*mut ()>(i);
                                    let mut val = Value::from(ptr as usize);
                                    val.meta_mut().pointer = Some(MetaPtr::new(ptr, true));
                                    results.push(val);
                                }
                            },
                            _ => {
                                return Err(format!(
                                    "Invalid or unsupported FFI out parameter type {ty}"
                                ))
                            }
                        }
                    }
                    FfiType::List {
                        mutable: true,
                        inner,
                        len_index,
                    } => match &**inner {
                        FfiType::Char => out_param_list!(c_char, len_index, i, u8, char),
                        FfiType::UChar => out_param_list!(c_uchar, len_index, i, u8),
                        FfiType::Short => out_param_list!(c_short, len_index, i, f64),
                        FfiType::UShort => out_param_list!(c_ushort, len_index, i, f64),
                        FfiType::Int => out_param_list!(c_int, len_index, i, f64),
                        FfiType::UInt => out_param_list!(c_uint, len_index, i, f64),
                        FfiType::Long => out_param_list!(c_long, len_index, i, f64),
                        FfiType::ULong => out_param_list!(c_ulong, len_index, i, f64),
                        FfiType::LongLong => out_param_list!(c_longlong, len_index, i, f64),
                        FfiType::ULongLong => out_param_list!(c_ulonglong, len_index, i, f64),
                        FfiType::Float => out_param_list!(c_float, len_index, i, f64),
                        FfiType::Double => out_param_list!(c_double, len_index, i, f64),
                        FfiType::Struct { fields } => {
                            let len = *bindings.get::<c_int>(*len_index) as usize;
                            let repr = bindings.get_repr(i);
                            if len > 0 && repr.len() % len != 0 {
                                return Err(format!(
                                    "Invalid length for FFI out parameter {i}: {len}"
                                ));
                            }
                            let mut rows = Vec::new();
                            for chunk in repr.chunks_exact(repr.len() / len) {
                                rows.push(bindings.struct_repr_to_value(chunk, fields)?);
                            }
                            let value = Value::from_row_values_infallible(rows);
                            results.push(value);
                        }
                        _ => {
                            return Err(format!(
                                "FFI parameter {i} has type {ty}, which is \
                                invalid/unsupported as an out parameter. \
                                If this is not an out parameter, annotate it as `const`."
                            ))
                        }
                    },
                    _ => {}
                }
            }

            // Bindings must live until after the call
            drop(bindings);

            Ok(match results.len() {
                0 => Value::default(),
                1 => results.pop().unwrap(),
                n => Array::new(n, results.into_iter().map(Boxed).collect::<EcoVec<_>>()).into(),
            })
        }
    }

    type ListStorage<T> = (*mut T, Box<[T]>);

    #[derive(Default)]
    struct FfiBindings {
        arg_data: Vec<Box<dyn Any>>,
        other_data: Vec<Box<dyn Any>>,
        args: Vec<Arg>,
    }

    impl FfiBindings {
        /// The previously pushed value is not an argument
        fn no_arg(&mut self) {
            self.args.pop().unwrap();
            self.other_data.push(self.arg_data.pop().unwrap());
        }
        fn alloc_and_push_ptr_to<T: Any + Copy + std::fmt::Debug>(&mut self, arg: T) -> *mut () {
            let mut bx = Box::<T>::new(arg);
            let ptr: *mut T = &mut *bx;
            dbgln!("      create *mut {}: {ptr:p}", type_name::<T>());
            self.arg_data.push(Box::new((ptr, bx)));
            self.args.push(Arg::new(
                &(self.arg_data.last().unwrap())
                    .downcast_ref::<(*mut T, Box<T>)>()
                    .unwrap_or_else(|| {
                        panic!(
                            "Value wasn't expected type {}",
                            type_name::<(*mut T, Box<T>)>()
                        )
                    })
                    .0,
            ));
            ptr as *mut ()
        }
        fn push_raw_ptr<T: 'static>(&mut self, ptr: *mut T) {
            self.arg_data.push(Box::new(ptr));
            self.args.push(Arg::new(
                (self.arg_data.last().unwrap().downcast_ref::<*mut T>()).unwrap_or_else(|| {
                    panic!("Value wasn't expected type {}", type_name::<*mut T>())
                }),
            ));
        }
        fn push_value<T: Any>(&mut self, arg: T) -> *mut () {
            self.arg_data.push(Box::new(arg));
            self.args.push(Arg::new(
                (self.arg_data.last().unwrap().downcast_ref::<T>())
                    .unwrap_or_else(|| panic!("Value wasn't expected type {}", type_name::<T>())),
            ));
            (self.arg_data.last().unwrap().downcast_ref::<T>())
                .unwrap_or_else(|| panic!("Value wasn't expected type {}", type_name::<T>()))
                as *const T as *mut ()
        }
        fn push_repr(&mut self, arg: Vec<u8>) -> *mut () {
            self.arg_data.push(Box::new(arg));
            self.args.push(Arg::new(
                &(self.arg_data.last().unwrap())
                    .downcast_ref::<Vec<u8>>()
                    .unwrap()[0],
            ));
            (self.arg_data.last().unwrap().downcast_ref::<Vec<u8>>())
                .unwrap_or_else(|| panic!("Value wasn't expected type {}", type_name::<Vec<u8>>()))
                .as_ptr() as *mut ()
        }
        fn push_repr_ptr(&mut self, mut arg: Vec<u8>) -> *mut () {
            let ptr = arg.as_mut_ptr();
            self.arg_data.push(Box::new((ptr, arg)));
            self.args.push(Arg::new(
                &(self.arg_data.last().unwrap())
                    .downcast_ref::<(*mut u8, Vec<u8>)>()
                    .unwrap_or_else(|| {
                        panic!(
                            "Value wasn't expected type {}",
                            type_name::<(*mut u8, Vec<u8>)>()
                        )
                    })
                    .0,
            ));
            ptr as *mut ()
        }
        fn push_string(&mut self, arg: String) -> *mut c_char {
            let list: Box<[c_char]> = arg
                .chars()
                .map(|c| c as c_char)
                .chain(['\0' as c_char])
                .collect();
            self.push_list::<c_char>(list)
        }
        fn push_list<T: Any + 'static>(&mut self, mut arg: Box<[T]>) -> *mut T {
            let ptr = &mut arg[0] as *mut T;
            dbgln!("      create *mut {}: {ptr:p}", type_name::<T>());
            let storage = (ptr, arg);
            self.arg_data.push(Box::new(storage));
            self.args.push(Arg::new(
                &(self.arg_data.last_mut().unwrap())
                    .downcast_mut::<ListStorage<T>>()
                    .unwrap_or_else(|| {
                        panic!(
                            "Value wasn't expected type {}",
                            type_name::<ListStorage<T>>()
                        )
                    })
                    .0,
            ));
            ptr
        }
        fn get<T: Any>(&self, index: usize) -> &T {
            self.try_get(index).map(|(t, _)| t).unwrap_or_else(|| {
                panic!(
                    "Value wasn't expected type {}, {}, or {}",
                    type_name::<T>(),
                    type_name::<(*mut T, Box<T>)>(),
                    type_name::<ListStorage<T>>()
                )
            })
        }
        fn get_maybe_null<T: Any>(&self, index: usize) -> Option<(&T, Option<*mut T>)> {
            self.try_get(index)
                .map(Some)
                .or_else(|| {
                    self.arg_data[index]
                        .downcast_ref::<*mut ()>()
                        .is_some()
                        .then_some(None)
                })
                .unwrap_or_else(|| {
                    panic!(
                        "Value wasn't expected type {}, {}, {}, or {}",
                        type_name::<T>(),
                        type_name::<(*mut T, Box<T>)>(),
                        type_name::<ListStorage<T>>(),
                        type_name::<*mut ()>()
                    )
                })
        }
        fn try_get<T: Any>(&self, index: usize) -> Option<(&T, Option<*mut T>)> {
            let any = &self.arg_data[index];
            any.downcast_ref::<T>()
                .map(|t| {
                    dbgln!("  exact type");
                    (t, None)
                })
                .or_else(|| {
                    any.downcast_ref::<(*mut T, Box<T>)>().map(|(p, _)| {
                        dbgln!("  ptr type");
                        (unsafe { &**p }, Some(*p))
                    })
                })
                .or_else(|| {
                    any.downcast_ref::<ListStorage<T>>().map(|(_, b)| {
                        dbgln!("  list type");
                        (&b[0], Some(&b[0] as *const T as *mut T))
                    })
                })
        }
        fn get_list_mut<T: 'static>(&mut self, index: usize) -> (*mut T, &mut Box<[T]>) {
            let (ptr, vec) = self.arg_data[index]
                .downcast_mut::<ListStorage<T>>()
                .unwrap_or_else(|| {
                    panic!(
                        "Value wasn't expected type {}",
                        type_name::<ListStorage<T>>()
                    )
                });
            (*ptr, vec)
        }
        fn get_repr(&self, index: usize) -> &[u8] {
            self.arg_data[index]
                .downcast_ref::<(*mut u8, Vec<u8>)>()
                .unwrap_or_else(|| {
                    panic!(
                        "Value wasn't expected type {}",
                        type_name::<(*mut u8, Vec<u8>)>()
                    )
                })
                .1
                .as_slice()
        }

        fn bind_arg(&mut self, i: usize, ty: &FfiType, val: &Value) -> Result<*mut (), String> {
            self.bind_impl(i, ty, val, true)
        }
        fn bind(&mut self, i: usize, ty: &FfiType, val: &Value) -> Result<*mut (), String> {
            self.bind_impl(i, ty, val, false)
        }
        fn bind_impl(
            &mut self,
            i: usize,
            ty: &FfiType,
            val: &Value,
            arg: bool,
        ) -> Result<*mut (), String> {
            dbgln!("    arg {i}, type {ty}, val: {val:?}");
            macro_rules! scalar {
                ($arr:expr, $ty:ty) => {{
                    let val = $arr.data[0] as $ty;
                    self.push_value(val)
                }};
            }
            macro_rules! list {
                ($arr:expr, $ty:ty) => {
                    self.push_list($arr.data.iter().map(|&i| i as $ty).collect()) as *mut ()
                };
            }
            let ptr = match (ty, val) {
                (FfiType::Void, _) => return Err("Cannot pass void to a function".into()),
                (FfiType::Char | FfiType::UChar, Value::Char(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_char)
                }
                (FfiType::Char | FfiType::UChar, Value::Num(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_char)
                }
                (FfiType::Char | FfiType::UChar, Value::Byte(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_char)
                }
                (FfiType::Short | FfiType::UShort, Value::Num(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_short)
                }
                (FfiType::Short | FfiType::UShort, Value::Byte(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_short)
                }
                (FfiType::Int | FfiType::UInt, Value::Num(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_int)
                }
                (FfiType::Int | FfiType::UInt, Value::Byte(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_int)
                }
                (FfiType::Long | FfiType::ULong, Value::Num(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_long)
                }
                (FfiType::Long | FfiType::ULong, Value::Byte(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_long)
                }
                (FfiType::LongLong | FfiType::ULongLong, Value::Num(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_longlong)
                }
                (FfiType::LongLong | FfiType::ULongLong, Value::Byte(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_longlong)
                }
                (FfiType::Float, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_float),
                (FfiType::Float, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_float),
                (FfiType::Double, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_double),
                (FfiType::Double, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_double),
                (FfiType::Ptr { inner, .. }, val) => match (&**inner, val) {
                    (FfiType::Char, Value::Char(arr)) => {
                        self.push_string(arr.data.iter().copied().collect()) as *mut ()
                    }
                    (FfiType::Struct { fields }, val) => {
                        let repr = self.value_to_struct_repr(val, fields)?;
                        self.push_repr_ptr(repr)
                    }
                    (_, arg) if arg.meta().pointer.is_some() => {
                        let ptr = arg.meta().pointer.unwrap().get_mut();
                        self.push_raw_ptr(ptr);
                        ptr
                    }
                    (FfiType::Void, val) => {
                        let ptr = self.push_value(val.clone());
                        self.no_arg();
                        self.alloc_and_push_ptr_to(ptr)
                    }
                    (FfiType::UChar, Value::Char(arr)) => list!(arr, c_char),
                    (FfiType::Char, Value::Num(arr)) => list!(arr, c_char),
                    (FfiType::UChar, Value::Num(arr)) => list!(arr, c_uchar),
                    (FfiType::Short, Value::Num(arr)) => list!(arr, c_short),
                    (FfiType::UShort, Value::Num(arr)) => list!(arr, c_ushort),
                    (FfiType::Int, Value::Num(arr)) => list!(arr, c_int),
                    (FfiType::UInt, Value::Num(arr)) => list!(arr, c_uint),
                    (FfiType::Long, Value::Num(arr)) => list!(arr, c_long),
                    (FfiType::ULong, Value::Num(arr)) => list!(arr, c_ulong),
                    (FfiType::LongLong, Value::Num(arr)) => list!(arr, c_longlong),
                    (FfiType::ULongLong, Value::Num(arr)) => list!(arr, c_ulonglong),
                    (FfiType::Float, Value::Num(arr)) => list!(arr, c_float),
                    (FfiType::Double, Value::Num(arr)) => list!(arr, c_double),
                    (FfiType::Char, Value::Byte(arr)) => list!(arr, c_char),
                    (FfiType::UChar, Value::Byte(arr)) => list!(arr, c_uchar),
                    (FfiType::Short, Value::Byte(arr)) => list!(arr, c_short),
                    (FfiType::UShort, Value::Byte(arr)) => list!(arr, c_ushort),
                    (FfiType::Int, Value::Byte(arr)) => list!(arr, c_int),
                    (FfiType::UInt, Value::Byte(arr)) => list!(arr, c_uint),
                    (FfiType::Long, Value::Byte(arr)) => list!(arr, c_long),
                    (FfiType::ULong, Value::Byte(arr)) => list!(arr, c_ulong),
                    (FfiType::LongLong, Value::Byte(arr)) => list!(arr, c_longlong),
                    (FfiType::ULongLong, Value::Byte(arr)) => list!(arr, c_ulonglong),
                    (FfiType::Float, Value::Byte(arr)) => list!(arr, c_float),
                    (FfiType::Double, Value::Byte(arr)) => list!(arr, c_double),
                    (inner, val) => {
                        let ptr = self.bind(i, inner, val)?;
                        dbgln!("    inner ptr to {inner}: {ptr:p}");
                        let ptr = self.alloc_and_push_ptr_to(ptr);
                        self.no_arg();
                        dbgln!("    outer ptr to {inner}: {ptr:p}");
                        self.push_raw_ptr(ptr);
                        ptr
                    }
                },
                (FfiType::List { inner, .. }, val) => match (&**inner, val) {
                    (FfiType::Char, Value::Char(arr)) => list!(arr, c_char),
                    (FfiType::UChar, Value::Char(arr)) => list!(arr, c_uchar),
                    (FfiType::Char, Value::Num(arr)) => list!(arr, c_char),
                    (FfiType::UChar, Value::Num(arr)) => list!(arr, c_uchar),
                    (FfiType::Short, Value::Num(arr)) => list!(arr, c_short),
                    (FfiType::UShort, Value::Num(arr)) => list!(arr, c_ushort),
                    (FfiType::Int, Value::Num(arr)) => list!(arr, c_int),
                    (FfiType::UInt, Value::Num(arr)) => list!(arr, c_uint),
                    (FfiType::Long, Value::Num(arr)) => list!(arr, c_long),
                    (FfiType::ULong, Value::Num(arr)) => list!(arr, c_ulong),
                    (FfiType::LongLong, Value::Num(arr)) => list!(arr, c_longlong),
                    (FfiType::ULongLong, Value::Num(arr)) => list!(arr, c_ulonglong),
                    (FfiType::Float, Value::Num(arr)) => list!(arr, c_float),
                    (FfiType::Double, Value::Num(arr)) => list!(arr, c_double),
                    (FfiType::Char, Value::Byte(arr)) => list!(arr, c_char),
                    (FfiType::UChar, Value::Byte(arr)) => list!(arr, c_uchar),
                    (FfiType::Short, Value::Byte(arr)) => list!(arr, c_short),
                    (FfiType::UShort, Value::Byte(arr)) => list!(arr, c_ushort),
                    (FfiType::Int, Value::Byte(arr)) => list!(arr, c_int),
                    (FfiType::UInt, Value::Byte(arr)) => list!(arr, c_uint),
                    (FfiType::Long, Value::Byte(arr)) => list!(arr, c_long),
                    (FfiType::ULong, Value::Byte(arr)) => list!(arr, c_ulong),
                    (FfiType::LongLong, Value::Byte(arr)) => list!(arr, c_longlong),
                    (FfiType::ULongLong, Value::Byte(arr)) => list!(arr, c_ulonglong),
                    (FfiType::Float, Value::Byte(arr)) => list!(arr, c_float),
                    (FfiType::Double, Value::Byte(arr)) => list!(arr, c_double),
                    (FfiType::Struct { fields }, val) => {
                        let mut all_reprs = Vec::new();
                        for row in val.rows() {
                            let repr = self.value_to_struct_repr(&row, fields)?;
                            all_reprs.extend(repr);
                        }
                        self.push_repr_ptr(all_reprs)
                    }
                    (_, arg) => {
                        return Err(format!(
                            "Array of {} with shape {} is not a valid \
                            argument {i} for FFI type {ty}",
                            arg.type_name_plural(),
                            arg.shape()
                        ))
                    }
                },
                (FfiType::Struct { fields }, val) => {
                    let repr = self.value_to_struct_repr(val, fields)?;
                    self.push_repr(repr)
                }
                (ty, arg) => {
                    return Err(format!(
                        "Array of {} with shape {} is not a valid \
                            argument {i} for FFI type {ty}",
                        arg.type_name_plural(),
                        arg.shape()
                    ))
                }
            };
            if !arg {
                self.no_arg();
            }
            Ok(ptr)
        }
        /// Convert a [`Value`] to a C-ABI-compatiable struct byte representation
        ///
        /// Takes into account the size and alignment of the fields
        fn value_to_struct_repr(
            &mut self,
            value: &Value,
            fields: &[FfiType],
        ) -> Result<Vec<u8>, String> {
            if value.row_count() != fields.len() {
                return Err(format!(
                    "Value has {} rows, but the struct has {} fields",
                    value.row_count(),
                    fields.len()
                ));
            }
            let (size, _) = struct_fields_size_align(fields);
            let mut repr = vec![0; size];
            let mut offset = 0;
            for (i, (row, field)) in value.rows().map(Value::unboxed).zip(fields).enumerate() {
                let (size, align) = field.size_align();
                if offset % align != 0 {
                    offset += align - (offset % align);
                }
                let range = offset..offset + size;
                macro_rules! scalar {
                    ($arr:expr, $ty:ty) => {
                        repr[range].copy_from_slice(&($arr.data[0] as $ty).to_ne_bytes())
                    };
                }
                match (field, row) {
                    (FfiType::Char, Value::Char(arr)) if arr.rank() == 0 => scalar!(arr, c_char),
                    (FfiType::UChar, Value::Char(arr)) if arr.rank() == 0 => scalar!(arr, c_uchar),
                    (FfiType::Char, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_char),
                    (FfiType::UChar, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_uchar),
                    (FfiType::Short, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_short),
                    (FfiType::Int, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_int),
                    (FfiType::Long, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_long),
                    (FfiType::LongLong, Value::Num(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_longlong)
                    }
                    (FfiType::UChar, Value::Char(arr)) if arr.rank() == 0 => scalar!(arr, c_uchar),
                    (FfiType::UShort, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_ushort),
                    (FfiType::UInt, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_uint),
                    (FfiType::ULong, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_ulong),
                    (FfiType::ULongLong, Value::Num(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_ulonglong)
                    }
                    (FfiType::Float, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_float),
                    (FfiType::Double, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_double),
                    (FfiType::Char, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_char),
                    (FfiType::UChar, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_uchar),
                    (FfiType::Short, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_short),
                    (FfiType::Int, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_int),
                    (FfiType::Long, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_long),
                    (FfiType::LongLong, Value::Byte(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_longlong)
                    }
                    (FfiType::UChar, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_uchar),
                    (FfiType::UShort, Value::Byte(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_ushort)
                    }
                    (FfiType::UInt, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_uint),
                    (FfiType::ULong, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_ulong),
                    (FfiType::ULongLong, Value::Byte(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_ulonglong)
                    }
                    (FfiType::Float, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_float),
                    (FfiType::Double, Value::Byte(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_double)
                    }
                    // Structs
                    (FfiType::Struct { fields }, value) => {
                        repr[range].copy_from_slice(&self.value_to_struct_repr(&value, fields)?);
                    }
                    // Pointers
                    (FfiType::Ptr { inner, .. }, value) => {
                        if let Some(ptr_u) = value.meta().pointer {
                            repr[range].copy_from_slice(&ptr_u.ptr.to_ne_bytes());
                        } else {
                            match (&**inner, value) {
                                (FfiType::Char, Value::Char(arr)) => {
                                    let string = arr.data.iter().copied().collect::<String>();
                                    let ptr = self.push_string(string);
                                    self.no_arg();
                                    repr[range].copy_from_slice(&(ptr as usize).to_ne_bytes());
                                }
                                (inner, value) => {
                                    let ptr = self.bind(i, inner, &value)?;
                                    repr[range].copy_from_slice(&(ptr as usize).to_ne_bytes());
                                }
                            }
                        }
                    }
                    (FfiType::Void, _) => return Err("Cannot have void fields in a struct".into()),
                    _ => return Err(format!("Invalid or unsupported field {i} type {field}")),
                }
                offset += size;
            }
            // dbgln!("repr: {:?}", repr);
            // dbgln!("repr: {:x?}", repr);
            Ok(repr)
        }
        /// Convert a C-ABI-compatiable struct byte representation to a [`Value`]
        #[allow(clippy::only_used_in_recursion)]
        fn struct_repr_to_value(&self, repr: &[u8], fields: &[FfiType]) -> Result<Value, String> {
            let mut rows: Vec<Value> = Vec::new();
            let mut offset = 0;
            for (i, field) in fields.iter().enumerate() {
                let (size, align) = field.size_align();
                if offset % align != 0 {
                    offset += align - (offset % align);
                }
                macro_rules! scalar {
                    ($ty:ty) => {{
                        let mut bytes: [u8; size_of::<$ty>()] = Default::default();
                        bytes.copy_from_slice(&repr[offset..offset + size_of::<$ty>()]);
                        rows.push((<$ty>::from_ne_bytes(bytes) as f64).into());
                    }};
                }
                match field {
                    FfiType::Char => rows.push((repr[offset] as char).into()),
                    FfiType::Short => scalar!(c_short),
                    FfiType::Int => scalar!(c_int),
                    FfiType::Long => scalar!(c_long),
                    FfiType::LongLong => scalar!(c_longlong),
                    FfiType::UChar => scalar!(c_uchar),
                    FfiType::UShort => scalar!(c_ushort),
                    FfiType::UInt => scalar!(c_uint),
                    FfiType::ULong => scalar!(c_ulong),
                    FfiType::ULongLong => scalar!(c_ulonglong),
                    FfiType::Float => scalar!(c_float),
                    FfiType::Double => scalar!(c_double),
                    // Structs
                    FfiType::Struct { fields } => {
                        rows.push(self.struct_repr_to_value(&repr[offset..offset + size], fields)?);
                    }
                    // Pointers
                    FfiType::Ptr { inner, .. } => match &**inner {
                        FfiType::Char => {
                            let mut bytes: [u8; size_of::<*const c_char>()] = Default::default();
                            bytes.copy_from_slice(
                                &repr[offset..offset + size_of::<*const c_char>()],
                            );
                            let ptr = unsafe { transmute::<_, *const c_char>(bytes) };
                            let c_str = unsafe { CStr::from_ptr(ptr) };
                            let s = c_str.to_str().map_err(|e| e.to_string())?;
                            rows.push(Value::from(s));
                        }
                        FfiType::Struct { fields } => {
                            let mut bytes: [u8; size_of::<*const u8>()] = Default::default();
                            bytes.copy_from_slice(&repr[offset..offset + size_of::<*const u8>()]);
                            let ptr = unsafe { transmute::<_, *const u8>(bytes) };
                            let (size, _) = struct_fields_size_align(fields);
                            let inner_repr = unsafe { slice::from_raw_parts(ptr, size) };
                            rows.push(self.struct_repr_to_value(inner_repr, fields)?);
                        }
                        inner => {
                            let mut bytes: [u8; size_of::<*const u8>()] = Default::default();
                            bytes.copy_from_slice(&repr[offset..offset + size_of::<*const u8>()]);
                            let ptr = unsafe { transmute::<_, *const u8>(bytes) };
                            let (size, _) = inner.size_align();
                            let inner_repr = unsafe { slice::from_raw_parts(ptr, size) };
                            let mut row = self
                                .struct_repr_to_value(inner_repr, slice::from_ref(inner))?
                                .into_rows()
                                .next()
                                .unwrap();
                            row.meta_mut().pointer = Some(MetaPtr::new(ptr, false));
                            rows.push(row);
                        }
                    },
                    FfiType::Void => return Err("Cannot have void fields in a struct".into()),
                    _ => {
                        return Err(format!(
                            "Invalid or unsupported struct field {i} type {field}"
                        ))
                    }
                }
                offset += size;
            }
            Ok(
                if fields.iter().all(|f| f.is_scalar() && fields[0] == *f)
                    && rows.iter().all(|r| r.shape() == rows[0].shape())
                {
                    Value::from_row_values_infallible(rows)
                } else {
                    Array::new(
                        rows.len(),
                        rows.into_iter().map(Boxed).collect::<EcoVec<_>>(),
                    )
                    .into()
                },
            )
        }
    }

    /// Convert a [`FfiType`] to a C-compatible [`Type`]
    pub(crate) fn ffity_to_cty(ty: &FfiType) -> Type {
        match ty {
            FfiType::Void => Type::void(),
            FfiType::Char => Type::c_schar(),
            FfiType::Short => Type::c_short(),
            FfiType::Int => Type::c_int(),
            FfiType::Long => Type::c_long(),
            FfiType::LongLong => Type::c_longlong(),
            FfiType::UChar => Type::c_uchar(),
            FfiType::UShort => Type::c_ushort(),
            FfiType::UInt => Type::c_uint(),
            FfiType::ULong => Type::c_ulong(),
            FfiType::ULongLong => Type::c_ulonglong(),
            FfiType::Float => Type::f32(),
            FfiType::Double => Type::f64(),
            FfiType::Ptr { .. } => Type::pointer(),
            FfiType::List { .. } => Type::pointer(),
            FfiType::Struct { fields } => {
                let mut types = Vec::with_capacity(fields.len());
                for field in fields {
                    types.push(ffity_to_cty(field));
                }
                Type::structure(types)
            }
        }
    }

    pub(crate) fn ffi_copy(ty: FfiType, ptr: *const (), len: usize) -> Result<Value, String> {
        fn ptr_iter<T>(ptr: *const T, len: usize) -> impl ExactSizeIterator<Item = T> {
            (0..len).map(move |i| unsafe { ptr.add(i).read() })
        }
        macro_rules! as_f64 {
            ($ty:ty) => {
                ptr_iter(ptr as *const $ty, len).map(|i| i as f64).collect()
            };
        }
        Ok(match ty {
            FfiType::Char => unsafe {
                let slice = slice::from_raw_parts(ptr as *const c_char, len);
                let s = CStr::from_ptr(slice.as_ptr())
                    .to_str()
                    .map_err(|e| e.to_string())?;
                Value::from(s)
            },
            FfiType::UChar => ptr_iter(ptr as *const c_uchar, len).collect(),
            FfiType::Short => as_f64!(c_short),
            FfiType::UShort => as_f64!(c_ushort),
            FfiType::Int => as_f64!(c_int),
            FfiType::UInt => as_f64!(c_uint),
            FfiType::Long => as_f64!(c_long),
            FfiType::ULong => as_f64!(c_ulong),
            FfiType::LongLong => as_f64!(c_longlong),
            FfiType::ULongLong => as_f64!(c_ulonglong),
            FfiType::Float => as_f64!(c_float),
            FfiType::Double => as_f64!(c_double),
            ty => return Err(format!("Unsupported FFI read type {ty}")),
        })
    }

    pub(crate) fn ffi_free(ptr: *const ()) {
        if ptr.is_null() {
            return;
        }
        let ptr = ptr as *mut ();
        unsafe {
            let _ = Box::from_raw(ptr);
        }
    }
}

#[test]
#[cfg(test)]
fn parse_ffi_type() {
    let rect = FfiType::Struct {
        fields: vec![FfiType::Float; 4],
    };
    let texture = FfiType::Struct {
        fields: vec![FfiType::Int; 5],
    };
    let image = FfiType::Struct {
        fields: vec![
            FfiType::Ptr {
                mutable: true,
                inner: FfiType::Void.into(),
            },
            FfiType::Int,
            FfiType::Int,
            FfiType::Int,
            FfiType::Int,
        ],
    };
    let glyph_info = FfiType::Struct {
        fields: vec![
            FfiType::Int,
            FfiType::Int,
            FfiType::Int,
            FfiType::Int,
            image,
        ],
    };
    let font = FfiType::Struct {
        fields: vec![
            FfiType::Int,
            FfiType::Int,
            FfiType::Int,
            texture,
            FfiType::Ptr {
                mutable: true,
                inner: rect.into(),
            },
            FfiType::Ptr {
                mutable: true,
                inner: glyph_info.into(),
            },
        ],
    };
    let expected = "{\
        int; int; int; \
        {int; int; int; int; int}; \
        {float; float; float; float}*; \
        {int; int; int; int; {void*; int; int; int; int}}*\
    }";
    assert_eq!(font.to_string(), expected);
    assert_eq!(expected.parse(), Ok(font));
}
