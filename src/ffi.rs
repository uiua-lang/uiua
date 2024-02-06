use std::{fmt, mem::size_of, str::FromStr};

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
        let mut mutable = true;
        if let Some(t) = s.strip_prefix("const ") {
            s = t;
            mutable = false;
        }
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
        if let Some(mut s) = s.strip_suffix('*') {
            s = s.trim();
            return Ok(FfiType::Ptr {
                mutable,
                inner: Box::new(s.parse()?),
            });
        }
        if let Some(mut s) = s.strip_prefix('{').and_then(|s| s.strip_suffix('}')) {
            s = s.trim();
            let fields = s
                .split_whitespace()
                .map(|s| s.trim().parse())
                .collect::<Result<_, _>>()?;
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
                write!(f, "{}{}*", if *mutable { " " } else { "const " }, inner)
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
                        write!(f, " ")?;
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
            FfiType::Char | FfiType::UChar => (1, 1),
            FfiType::Short | FfiType::UShort => (2, 2),
            FfiType::Int | FfiType::UInt => (4, 4),
            FfiType::Long | FfiType::ULong => (size_of::<usize>(), size_of::<usize>()),
            FfiType::LongLong | FfiType::ULongLong => (8, size_of::<usize>()),
            FfiType::Float => (4, 4),
            FfiType::Double => (8, size_of::<usize>()),
            FfiType::Ptr { .. } | FfiType::List { .. } => (size_of::<usize>(), size_of::<usize>()),
            FfiType::Struct { fields } => fields
                .iter()
                .map(Self::size_align)
                .fold((0, 1), |(size, align), (s, a)| (size + s, align.max(a))),
        }
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
                    FfiType::Int => bindings.push_value(len as c_int),
                    FfiType::UInt => bindings.push_value(len as c_uint),
                    FfiType::Long => bindings.push_value(len as c_long),
                    FfiType::ULong => bindings.push_value(len as c_ulong),
                    FfiType::LongLong => bindings.push_value(len as c_longlong),
                    FfiType::ULongLong => bindings.push_value(len as c_ulonglong),
                    FfiType::Ptr { inner, .. } => match &**inner {
                        FfiType::Int => bindings.push_ptr(len as c_int),
                        FfiType::UInt => bindings.push_ptr(len as c_uint),
                        FfiType::Long => bindings.push_ptr(len as c_long),
                        FfiType::ULong => bindings.push_ptr(len as c_ulong),
                        FfiType::LongLong => bindings.push_ptr(len as c_longlong),
                        FfiType::ULongLong => bindings.push_ptr(len as c_ulonglong),
                        _ => return Err(format!("{arg_ty} is not a valid FFI type for lengths")),
                    },
                    ty => return Err(format!("{ty} is not a valid FFI type for lengths")),
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
                    ($arr:expr, $ty:ty) => {
                        bindings.push_list($arr.data.iter().map(|&i| i as $ty).collect())
                    };
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
                        (FfiType::Struct { fields }, val) => {
                            let repr = value_to_struct_repr(val, fields)?;
                            bindings.push_repr_ptr(repr);
                        }
                        (ty, arg) => {
                            return Err(format!(
                                "Array of {} with shape {} is not a valid \
                                argument {i} for FFI type {ty}",
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
                                argument {i} for FFI type {ty}",
                                arg.type_name_plural(),
                                arg.shape()
                            ))
                        }
                    },
                    (FfiType::Struct { fields }, val) => {
                        let repr = value_to_struct_repr(val, fields)?;
                        bindings.push_repr(repr);
                    }
                    (ty, arg) => {
                        return Err(format!(
                            "Array of {} with shape {} is not a valid \
                            argument {i} for FFI type {ty}",
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
                        "Invalid or unsupported FFI return type {return_ty}"
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
                        "Invalid or unsupported FFI return type {return_ty}"
                    ))
                }
            },
            FfiType::Struct { fields } => {
                let (size, _) = return_ty.size_align();
                let args = &bindings.args;
                let val = match size {
                    0 => Value::default(),
                    1 => struct_repr_to_value(&unsafe { cif.call::<[u8; 1]>(fptr, args) }, fields)?,
                    2 => struct_repr_to_value(&unsafe { cif.call::<[u8; 2]>(fptr, args) }, fields)?,
                    4 => struct_repr_to_value(&unsafe { cif.call::<[u8; 4]>(fptr, args) }, fields)?,
                    8 => struct_repr_to_value(&unsafe { cif.call::<[u8; 8]>(fptr, args) }, fields)?,
                    16 => {
                        struct_repr_to_value(&unsafe { cif.call::<[u8; 16]>(fptr, args) }, fields)?
                    }
                    32 => {
                        struct_repr_to_value(&unsafe { cif.call::<[u8; 32]>(fptr, args) }, fields)?
                    }
                    64 => {
                        struct_repr_to_value(&unsafe { cif.call::<[u8; 64]>(fptr, args) }, fields)?
                    }
                    128 => {
                        struct_repr_to_value(&unsafe { cif.call::<[u8; 128]>(fptr, args) }, fields)?
                    }
                    n => return Err(format!("Unsupported return struct size: {n}")),
                };
                results.push(val);
            }
        }

        // Get out parameters
        macro_rules! out_param_scalar {
            ($ty:ty, $i:expr) => {
                results.push((*bindings.get::<$ty>($i) as f64).into())
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
                    match &**inner {
                        FfiType::Char => unsafe {
                            let ptr = bindings.get::<c_char>(i);
                            let s = CStr::from_ptr(ptr).to_str().map_err(|e| e.to_string())?;
                            results.push(Value::from(s))
                        },
                        FfiType::Short => out_param_scalar!(c_short, i),
                        FfiType::UShort => out_param_scalar!(c_ushort, i),
                        FfiType::Int => out_param_scalar!(c_int, i),
                        FfiType::UInt => out_param_scalar!(c_uint, i),
                        FfiType::Long => out_param_scalar!(c_long, i),
                        FfiType::ULong => out_param_scalar!(c_ulong, i),
                        FfiType::LongLong => out_param_scalar!(c_longlong, i),
                        FfiType::ULongLong => out_param_scalar!(c_ulonglong, i),
                        FfiType::Float => out_param_scalar!(c_float, i),
                        FfiType::Double => out_param_scalar!(c_double, i),
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
                            "FFI parameter {i} has type {ty}, which is \
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
        fn push_ptr<T: Any + Copy + std::fmt::Debug>(&mut self, arg: T) {
            let mut bx = Box::<T>::new(arg);
            let ptr: *mut T = &mut *bx;
            self.data.push(Box::new((ptr, bx)));
            // println!("len ptr a: {:p}", ptr);
            // println!("ptr val: {:?}", unsafe { *ptr });
            self.args.push(Arg::new(
                &(self.data.last().unwrap())
                    .downcast_ref::<(*mut T, Box<T>)>()
                    .unwrap()
                    .0,
            ));
        }
        fn push_value<T: Any>(&mut self, arg: T) {
            self.data.push(Box::new(arg));
            let value: &T = self.data.last().unwrap().downcast_ref::<T>().unwrap();
            self.args.push(Arg::new(value));
        }
        fn push_repr(&mut self, arg: Vec<u8>) {
            self.data.push(Box::new(arg));
            self.args.push(Arg::new(
                &(self.data.last().unwrap())
                    .downcast_ref::<Vec<u8>>()
                    .unwrap()[0],
            ));
        }
        fn push_repr_ptr(&mut self, arg: Vec<u8>) {
            let ptr = arg.as_ptr();
            self.data.push(Box::new((ptr, arg)));
            self.args.push(Arg::new(
                &(self.data.last().unwrap())
                    .downcast_ref::<(*const u8, Vec<u8>)>()
                    .unwrap()
                    .0,
            ));
        }
        fn push_string(&mut self, arg: String) {
            let s: CString =
                CString::new(arg.chars().take_while(|&c| c != '\0').collect::<String>()).unwrap();
            self.data.push(Box::new(s));
            self.args.push(Arg::new(
                self.data.last().unwrap().downcast_ref::<CString>().unwrap(),
            ));
        }
        fn push_list<T: Any + 'static>(&mut self, mut arg: Box<[T]>) {
            let ptr = &mut arg[0] as *mut T;
            // println!("list ptr a: {:p}", ptr);
            self.data.push(Box::new((ptr, arg)));
            self.args.push(Arg::new(
                &(self.data.last().unwrap())
                    .downcast_ref::<(*mut T, Box<[T]>)>()
                    .unwrap()
                    .0,
            ));
        }
        fn get<T: Any>(&self, index: usize) -> &T {
            let any = &self.data[index];
            any.downcast_ref::<T>()
                .or_else(|| any.downcast_ref::<(*mut T, Box<T>)>().map(|(_, b)| &**b))
                .unwrap()
        }
        fn get_list_mut<T: 'static>(&mut self, index: usize) -> (*mut T, &mut Box<[T]>) {
            let (ptr, vec) = self.data[index]
                .downcast_mut::<(*mut T, Box<[T]>)>()
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
            FfiType::Struct { fields } => {
                let mut types = Vec::with_capacity(fields.len());
                for field in fields {
                    types.push(ffity_to_cty(field));
                }
                Type::structure(types)
            }
        }
    }

    /// Convert a [`Value`] to a C-ABI-compatiable struct byte representation
    ///
    /// Takes into account the size and alignment of the fields
    fn value_to_struct_repr(value: &Value, fields: &[FfiType]) -> Result<Vec<u8>, String> {
        if value.row_count() != fields.len() {
            return Err(format!(
                "Value has {} rows, but the struct has {} fields",
                value.row_count(),
                fields.len()
            ));
        }
        let mut repr = Vec::new();
        let mut offset = 0;
        for (i, (row, field)) in value.rows().map(Value::unboxed).zip(fields).enumerate() {
            let (size, align) = field.size_align();
            if offset % align != 0 {
                offset += align - (offset % align);
            }
            repr.resize(offset + size, 0);
            macro_rules! scalar {
                ($arr:expr, $ty:ty) => {
                    repr[offset..offset + size]
                        .copy_from_slice(&($arr.data[0] as $ty).to_ne_bytes())
                };
            }
            match (field, row) {
                (FfiType::Char, Value::Char(arr)) if arr.rank() == 0 => scalar!(arr, c_char),
                (FfiType::Short, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_short),
                (FfiType::Int, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_int),
                (FfiType::Long, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_long),
                (FfiType::LongLong, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_longlong),
                (FfiType::UChar, Value::Char(arr)) if arr.rank() == 0 => scalar!(arr, c_uchar),
                (FfiType::UShort, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_ushort),
                (FfiType::UInt, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_uint),
                (FfiType::ULong, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_ulong),
                (FfiType::ULongLong, Value::Num(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_ulonglong)
                }
                (FfiType::Float, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_float),
                (FfiType::Double, Value::Num(arr)) if arr.rank() == 0 => scalar!(arr, c_double),
                #[cfg(feature = "bytes")]
                (FfiType::Char, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_char),
                #[cfg(feature = "bytes")]
                (FfiType::Short, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_short),
                #[cfg(feature = "bytes")]
                (FfiType::Int, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_int),
                #[cfg(feature = "bytes")]
                (FfiType::Long, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_long),
                #[cfg(feature = "bytes")]
                (FfiType::LongLong, Value::Byte(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_longlong)
                }
                #[cfg(feature = "bytes")]
                (FfiType::UChar, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_uchar),
                #[cfg(feature = "bytes")]
                (FfiType::UShort, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_ushort),
                #[cfg(feature = "bytes")]
                (FfiType::UInt, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_uint),
                #[cfg(feature = "bytes")]
                (FfiType::ULong, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_ulong),
                #[cfg(feature = "bytes")]
                (FfiType::ULongLong, Value::Byte(arr)) if arr.rank() == 0 => {
                    scalar!(arr, c_ulonglong)
                }
                #[cfg(feature = "bytes")]
                (FfiType::Float, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_float),
                #[cfg(feature = "bytes")]
                (FfiType::Double, Value::Byte(arr)) if arr.rank() == 0 => scalar!(arr, c_double),
                // Structs
                (FfiType::Struct { fields }, value) => {
                    repr[offset..offset + size]
                        .copy_from_slice(&value_to_struct_repr(&value, fields)?);
                }
                (FfiType::Void, _) => return Err("Cannot have void fields in a struct".into()),
                _ => return Err(format!("Invalid or unsupported field {i} type {field}")),
            }
            offset += size;
        }
        Ok(repr)
    }

    /// Convert a C-ABI-compatiable struct byte representation to a [`Value`]
    #[allow(dead_code)]
    fn struct_repr_to_value(repr: &[u8], fields: &[FfiType]) -> Result<Value, String> {
        let mut rows: Vec<Value> = Vec::new();
        let mut offset = 0;
        for (i, field) in fields.iter().enumerate() {
            let (size, align) = field.size_align();
            if offset % align != 0 {
                offset += align - (offset % align);
            }
            macro_rules! scalar {
                ($ty:ty, $size:expr) => {{
                    let mut bytes: [u8; $size] = Default::default();
                    bytes.copy_from_slice(&repr[offset..offset + $size]);
                    rows.push((<$ty>::from_ne_bytes(bytes) as f64).into());
                }};
            }
            match field {
                FfiType::Char => rows.push((repr[offset] as char).into()),
                FfiType::Short => scalar!(c_short, 2),
                FfiType::Int => scalar!(c_int, 4),
                FfiType::Long => scalar!(c_long, 4),
                FfiType::LongLong => scalar!(c_longlong, 8),
                FfiType::UChar => scalar!(c_uchar, 1),
                FfiType::UShort => scalar!(c_ushort, 2),
                FfiType::UInt => scalar!(c_uint, 4),
                FfiType::ULong => scalar!(c_ulong, 4),
                FfiType::ULongLong => scalar!(c_ulonglong, 8),
                FfiType::Float => scalar!(c_float, 4),
                FfiType::Double => scalar!(c_double, 8),
                // Structs
                FfiType::Struct { fields } => {
                    rows.push(struct_repr_to_value(&repr[offset..offset + size], fields)?);
                }
                FfiType::Void => return Err("Cannot have void fields in a struct".into()),
                _ => return Err(format!("Invalid or unsupported field {i} type {field}")),
            }
            offset += size;
        }
        Ok(
            if fields.iter().all(|f| fields[0] == *f)
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
