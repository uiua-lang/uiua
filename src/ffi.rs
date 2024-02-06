use std::str::FromStr;

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
}

impl FromStr for FfiType {
    type Err = String;
    fn from_str(mut s: &str) -> Result<Self, String> {
        s = s.trim();
        let mut mutable = true;
        if let Some(t) = s.strip_prefix("const ") {
            s = t;
            mutable = false;
        }
        if let Some((mut a, mut b)) = s.split_once(':') {
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
        if let Some(mut s) = s.strip_suffix('*') {
            s = s.trim();
            return Ok(FfiType::Ptr {
                mutable,
                inner: Box::new(s.parse()?),
            });
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

#[cfg(feature = "ffi")]
pub(crate) use enabled::*;
#[cfg(feature = "ffi")]
mod enabled {
    use std::{any::Any, ffi::*, mem::forget, mem::take, slice};

    use ecow::EcoVec;
    use libffi::middle::*;

    use super::*;
    use crate::{Array, Boxed, Value};

    pub(crate) fn do_ffi(
        file: &str,
        return_ty: FfiType,
        name: &str,
        arg_tys: &[FfiType],
        args: &[Value],
    ) -> Result<Value, String> {
        let lib = unsafe { libloading::Library::new(file) }.map_err(|e| e.to_string())?;
        let fptr: libloading::Symbol<unsafe extern "C" fn()> =
            unsafe { lib.get(name.as_bytes()) }.map_err(|e| e.to_string())?;

        let mut cif_arg_tys = Vec::new();
        let mut bindings = FfiBindings::default();
        let mut lengths: Vec<Option<usize>> = vec![None; arg_tys.len()];
        // Collect lengths of lists
        for (i, arg_ty) in arg_tys.iter().enumerate() {
            if let FfiType::List {
                len_index: length, ..
            } = arg_ty
            {
                *lengths
                    .get_mut(*length)
                    .ok_or_else(|| format!("Invalid length index: {length}"))? =
                    args.get(i).map(Value::element_count);
            }
        }
        // Bind arguments
        let mut args = args.iter();
        for (i, arg_ty) in arg_tys.iter().enumerate() {
            cif_arg_tys.push(ffity_to_cty(arg_ty));
            if let Some(len) = lengths[i] {
                // Bind length
                // println!("bind {i} len: {len}");
                match arg_ty {
                    FfiType::Int | FfiType::UInt => bindings.push_value(len as c_int),
                    FfiType::Long | FfiType::ULong => bindings.push_value(len as c_long),
                    FfiType::LongLong | FfiType::ULongLong => {
                        bindings.push_value(len as c_longlong)
                    }
                    FfiType::Ptr { inner, .. } => match &**inner {
                        FfiType::Int | FfiType::UInt => bindings.push_ptr(len as c_int),
                        FfiType::Long | FfiType::ULong => bindings.push_ptr(len as c_long),
                        FfiType::LongLong | FfiType::ULongLong => {
                            bindings.push_ptr(len as c_longlong)
                        }
                        _ => return Err(format!("{arg_ty:?} is not a valid FFI type for lengths")),
                    },
                    ty => return Err(format!("{ty:?} is not a valid FFI type for lengths")),
                }
            } else {
                // Bind normal argument
                let arg = args.next().ok_or("Not enough arguments")?;
                // println!("bind {i} arg: {arg:?}");
                macro_rules! scalar {
                    ($arr:expr, $ty:ty) => {{
                        let val = $arr.data[0] as $ty;
                        bindings.push_value(val);
                    }};
                }
                macro_rules! list {
                    ($arr:expr, $ty:ty) => {{
                        bindings.push_list($arr.data.iter().map(|&i| i as $ty).collect());
                    }};
                }
                match (arg_ty, arg) {
                    (FfiType::Void, _) => return Err("Cannot pass void to a function".into()),
                    (FfiType::Char | FfiType::UChar, Value::Char(arr)) if arr.rank() == 0 => {
                        scalar!(arr, i8)
                    }
                    (FfiType::Char | FfiType::UChar, Value::Num(arr)) if arr.rank() == 0 => {
                        scalar!(arr, u8)
                    }
                    #[cfg(feature = "bytes")]
                    (FfiType::Char | FfiType::UChar, Value::Byte(arr)) if arr.rank() == 0 => {
                        scalar!(arr, u8)
                    }
                    (FfiType::Short | FfiType::UShort, Value::Num(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_short)
                    }
                    #[cfg(feature = "bytes")]
                    (FfiType::Short | FfiType::UShort, Value::Byte(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_short)
                    }
                    (FfiType::Int | FfiType::UInt, Value::Num(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_int)
                    }
                    #[cfg(feature = "bytes")]
                    (FfiType::Int | FfiType::UInt, Value::Byte(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_int)
                    }
                    (FfiType::Long | FfiType::ULong, Value::Num(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_long)
                    }
                    #[cfg(feature = "bytes")]
                    (FfiType::Long | FfiType::ULong, Value::Byte(arr)) if arr.rank() == 0 => {
                        scalar!(arr, c_long)
                    }
                    (FfiType::LongLong | FfiType::ULongLong, Value::Num(arr))
                        if arr.rank() == 0 =>
                    {
                        scalar!(arr, c_longlong)
                    }
                    #[cfg(feature = "bytes")]
                    (FfiType::LongLong | FfiType::ULongLong, Value::Byte(arr))
                        if arr.rank() == 0 =>
                    {
                        scalar!(arr, c_longlong)
                    }
                    (FfiType::Ptr { inner, .. }, val) => match (&**inner, val) {
                        (FfiType::Char, Value::Char(arr)) => {
                            bindings.push_string(arr.data.iter().copied().collect())
                        }
                        (ty, arg) => {
                            return Err(format!(
                                "Array of {} with shape {} is not a valid \
                                argument {i} for FFI type {ty:?}",
                                arg.type_name_plural(),
                                arg.shape()
                            ))
                        }
                    },
                    (FfiType::List { inner, .. }, val) => match (&**inner, val) {
                        (FfiType::Char | FfiType::UChar, Value::Char(arr)) => list!(arr, c_char),
                        (FfiType::Short | FfiType::UShort, Value::Num(arr)) => list!(arr, c_short),
                        (FfiType::Int | FfiType::UInt, Value::Num(arr)) => list!(arr, c_int),
                        (FfiType::Long | FfiType::ULong, Value::Num(arr)) => list!(arr, c_long),
                        (FfiType::LongLong | FfiType::ULongLong, Value::Num(arr)) => {
                            list!(arr, c_longlong)
                        }
                        (FfiType::Float, Value::Num(arr)) => list!(arr, c_float),
                        (FfiType::Double, Value::Num(arr)) => list!(arr, c_double),
                        #[cfg(feature = "bytes")]
                        (FfiType::Char | FfiType::UChar, Value::Byte(arr)) => list!(arr, c_char),
                        #[cfg(feature = "bytes")]
                        (FfiType::Short | FfiType::UShort, Value::Byte(arr)) => list!(arr, c_short),
                        #[cfg(feature = "bytes")]
                        (FfiType::Int | FfiType::UInt, Value::Byte(arr)) => list!(arr, c_int),
                        #[cfg(feature = "bytes")]
                        (FfiType::Long | FfiType::ULong, Value::Byte(arr)) => list!(arr, c_long),
                        #[cfg(feature = "bytes")]
                        (FfiType::LongLong | FfiType::ULongLong, Value::Byte(arr)) => {
                            list!(arr, c_longlong)
                        }
                        #[cfg(feature = "bytes")]
                        (FfiType::Float, Value::Byte(arr)) => list!(arr, c_float),
                        #[cfg(feature = "bytes")]
                        (FfiType::Double, Value::Byte(arr)) => list!(arr, c_double),
                        (ty, arg) => {
                            return Err(format!(
                                "Array of {} with shape {} is not a valid \
                                argument {i} for FFI type {ty:?}",
                                arg.type_name_plural(),
                                arg.shape()
                            ))
                        }
                    },
                    (ty, arg) => {
                        return Err(format!(
                            "Array of {} with shape {} is not a valid \
                            argument {i} for FFI type {ty:?}",
                            arg.type_name_plural(),
                            arg.shape()
                        ))
                    }
                }
            }
        }

        // Call and get return value
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
        macro_rules! out_param_list {
            ($c_ty:ty, $len_index:expr, $i:expr) => {
                unsafe {
                    let len = *bindings.get::<c_int>(*$len_index) as usize;
                    let (ptr, vec) = bindings.get_list_mut::<$c_ty>($i);
                    // Construct a list from the pointer and length
                    let slice = slice::from_raw_parts(ptr, len);
                    // Copy the slice into a new array
                    results.push(
                        Array::new(len, slice.iter().map(|&i| i as f64).collect::<EcoVec<_>>())
                            .into(),
                    );
                    // Forget the vector
                    forget(take(vec));
                    // Clean up the pointer's memory
                    drop(Vec::from_raw_parts(ptr as *mut $c_ty, len, len));
                }
            };
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
                _ => {
                    return Err(format!(
                        "Invalid or unsupported FFI return type {return_ty:?}"
                    ))
                }
            },
            FfiType::List {
                len_index, inner, ..
            } => match &**inner {
                FfiType::Char => ret_list!(c_char, len_index),
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
                        "Invalid or unsupported FFI return type {return_ty:?}"
                    ))
                }
            },
        }

        // Get out parameters
        for (i, ty) in arg_tys.iter().enumerate() {
            match ty {
                FfiType::Ptr {
                    mutable: true,
                    inner,
                } => match &**inner {
                    FfiType::Char => unsafe {
                        let ptr = cif.call::<*const c_char>(fptr, &bindings.args);
                        let s = CStr::from_ptr(ptr).to_str().map_err(|e| e.to_string())?;
                        results.push(Value::from(s))
                    },
                    _ => {
                        return Err(format!(
                            "Invalid or unsupported FFI out parameter type {ty:?}"
                        ))
                    }
                },
                FfiType::List {
                    mutable: true,
                    inner,
                    len_index,
                } => match &**inner {
                    FfiType::Char => out_param_list!(c_char, len_index, i),
                    FfiType::Short => out_param_list!(c_short, len_index, i),
                    FfiType::UShort => out_param_list!(c_ushort, len_index, i),
                    FfiType::Int => out_param_list!(c_int, len_index, i),
                    FfiType::UInt => out_param_list!(c_uint, len_index, i),
                    FfiType::Long => out_param_list!(c_long, len_index, i),
                    FfiType::ULong => out_param_list!(c_ulong, len_index, i),
                    FfiType::LongLong => out_param_list!(c_longlong, len_index, i),
                    FfiType::ULongLong => out_param_list!(c_ulonglong, len_index, i),
                    FfiType::Float => out_param_list!(c_float, len_index, i),
                    FfiType::Double => out_param_list!(c_double, len_index, i),
                    _ => {
                        return Err(format!(
                            "FFI parameter {i} has type {ty:?}, which is \
                            not valid as an out parameter. \
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

    #[derive(Default)]
    struct FfiBindings {
        data: Vec<Box<dyn Any>>,
        args: Vec<Arg>,
    }

    impl FfiBindings {
        fn push_ptr<T: Any>(&mut self, arg: T) {
            self.data.push(Box::new(Box::<T>::new(arg)));
            let ptr: *const T = &**self.data.last().unwrap().downcast_ref::<Box<T>>().unwrap();
            self.args.push(Arg::new(&ptr));
        }
        fn push_value<T: Any>(&mut self, arg: T) {
            self.data.push(Box::new(arg));
            let value: &T = self.data.last().unwrap().downcast_ref::<T>().unwrap();
            self.args.push(Arg::new(value));
        }
        fn push_string(&mut self, arg: String) {
            let s: CString =
                CString::new(arg.chars().take_while(|&c| c != '\0').collect::<String>()).unwrap();
            self.data.push(Box::new(s));
            self.args.push(Arg::new(
                self.data.last().unwrap().downcast_ref::<CString>().unwrap(),
            ));
        }
        fn push_list<T: 'static>(&mut self, arg: Vec<T>) {
            self.data.push(Box::new((arg.as_ptr(), arg)));
            self.args.push(Arg::new(
                &self
                    .data
                    .last()
                    .unwrap()
                    .downcast_ref::<(*const T, Vec<T>)>()
                    .unwrap()
                    .0,
            ));
        }
        fn get<T: Any>(&self, index: usize) -> &T {
            let any = &self.data[index];
            any.downcast_ref::<T>()
                .or_else(|| any.downcast_ref::<Box<T>>().map(|b| &**b))
                .unwrap()
        }
        fn get_list_mut<T: 'static>(&mut self, index: usize) -> (*const T, &mut Vec<T>) {
            let (ptr, vec) = self.data[index]
                .downcast_mut::<(*const T, Vec<T>)>()
                .unwrap();
            (*ptr, vec)
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
        }
    }
}
