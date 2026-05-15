use ecow::eco_vec;

#[cfg(feature = "ga")]
use crate::Multivector as Mv;
use crate::{Array, Complex, MvMode, SubSide, Uiua, UiuaResult, Value, ga::*};

impl Value {
    pub(crate) fn multivector(self, mode: MvMode, env: &Uiua) -> UiuaResult<Self> {
        let arr = match self {
            Value::Num(arr) => arr,
            Value::Byte(arr) => arr.convert(),
            #[cfg(feature = "ga")]
            Value::Complex(arr) => return Ok(arr.convert::<Mv>().into()),
            #[cfg(feature = "ga")]
            Value::Mv(mut mv) => {
                for mv in mv.data.as_mut_slice() {
                    if let Some(dims) = mode.dims {
                        mv.set_dims(dims);
                    }
                }
                return Ok(mv.into());
            }
            val => {
                return Err(env.error(format!(
                    "Cannot create multivector from {} array",
                    val.type_name()
                )));
            }
        };
        let _old_shape = arr.shape.clone();
        let mut new_shape = arr.shape.clone();
        let n = new_shape.pop();
        let elem_count = new_shape.elements();
        Ok(match (n, mode.dims, mode.side) {
            (Some(0), None, _) => {
                Array::<Complex>::new(new_shape, eco_vec![Complex::ZERO; elem_count]).into()
            }
            (Some(0), Some(d), _) if d <= 2 => {
                Array::<Complex>::new(new_shape, eco_vec![Complex::ZERO; elem_count]).into()
            }
            #[cfg(not(feature = "ga"))]
            (Some(2), None, None) if arr.data.iter().all(|&n| n == 0.0) => {
                Array::<Complex>::new(new_shape, eco_vec![Complex::ZERO; elem_count]).into()
            }
            (Some(1), None | Some(2), Some(SubSide::Left)) => arr.convert::<Complex>().into(),
            (Some(2), None | Some(2), Some(SubSide::Left)) | (Some(2), Some(2), None) => {
                Array::new(
                    new_shape,
                    arr.data.chunks_exact(2).map(|n| Complex::new(n[0], n[1])),
                )
                .into()
            }
            #[cfg(not(feature = "ga"))]
            (Some(4), Some(2), None)
                if arr.data.chunks_exact(4).all(|w| w[1] == 0.0 && w[2] == 0.0) =>
            {
                Array::new(
                    new_shape,
                    arr.data.chunks_exact(4).map(|n| Complex::new(n[0], n[3])),
                )
                .into()
            }
            #[cfg(feature = "ga")]
            (Some(0), ..) => Array::new(new_shape, eco_vec![Mv::default(); elem_count]).into(),
            #[cfg(feature = "ga")]
            (Some(1), dims, Some(SubSide::Left)) | (None, dims, None | Some(SubSide::Left)) => {
                Array::new(
                    new_shape,
                    (arr.data.into_iter())
                        .map(|n| Mv::scalar(dims.unwrap_or(0), n, Flavor::Vanilla)),
                )
                .into()
            }
            #[cfg(feature = "ga")]
            (None | Some(1), dims, Some(SubSide::Right)) => {
                // Pseudoscalar
                let dims = dims.unwrap_or(1);
                Array::new(
                    new_shape,
                    (arr.data.into_iter()).map(|n| Mv::vga_pseudoscalar(dims, n)),
                )
                .into()
            }
            #[cfg(feature = "ga")]
            (Some(n @ 1..), None, None) => {
                // Vector
                if n > MAX_DIMS as usize {
                    return Err(env.error(format!(
                        "{n} multivector dimensions \
                        (from a {_old_shape} array) would be too many"
                    )));
                }
                Array::new(new_shape, arr.data.chunks_exact(n).map(Mv::vga_vector)).into()
            }
            #[cfg(feature = "ga")]
            (n, None, Some(side)) => {
                let n = n.unwrap_or(1);
                // Dimensions not specified, but a grade is
                use uiua_parser::SubSide;
                let d = if n.is_power_of_two() {
                    let d = ((n * 2) as f32).log2() as usize;
                    if d > MAX_DIMS as usize {
                        let parity = match side {
                            SubSide::Left => "even",
                            SubSide::Right => "odd",
                        };
                        return Err(env.error(format!(
                            "{d} multivector dimensions \
                            ({n} {parity} blades from a {_old_shape} array) would be too many",
                        )));
                    } else {
                        d as u8
                    }
                } else {
                    let f = match side {
                        SubSide::Left => Mv::vga_vector,
                        SubSide::Right => Mv::vga_n_1_blades,
                    };
                    return Ok(Array::new(new_shape, arr.data.chunks_exact(n).map(f)).into());
                };
                let f = match side {
                    SubSide::Left => Mv::blades_left,
                    SubSide::Right => Mv::blades_right,
                };
                Array::new(
                    new_shape,
                    (arr.data.chunks_exact(n)).map(|n| f(d, n, Flavor::Vanilla).unwrap()),
                )
                .into()
            }
            #[cfg(feature = "ga")]
            (n, Some(d), side) => {
                let n = n.unwrap_or(0);
                // Dimensions specified
                use ecow::EcoVec;
                if d > MAX_DIMS {
                    return Err(env.error(format!("{d} multivector dimensions would be too many")));
                }

                let f = match side.unwrap_or(SubSide::Left) {
                    SubSide::Left => Mv::blades_left,
                    SubSide::Right => Mv::blades_right,
                };
                let mut data = EcoVec::with_capacity(elem_count);
                for slice in arr.data.chunks_exact(n) {
                    match f(d, slice, Flavor::Vanilla) {
                        Ok(mv) => data.push(mv),
                        Err(_) => {
                            return Err(env
                                .error(format!("Cannot create {d}D multivector from {n} blades")));
                        }
                    }
                }
                Array::new(new_shape, data).into()
            }
            #[cfg(not(feature = "ga"))]
            _ => {
                return Err(
                    env.error("Non-complex multivectors are not supported in this environment")
                );
            }
        })
    }
    pub(crate) fn unmultivector(mut self, mode: MvMode, env: &Uiua) -> UiuaResult<Self> {
        let (dims, side) = (mode.dims, mode.side);
        self.meta.take_sorted_flags();
        self.meta.take_per_meta();

        fn extract_scalar<T: Copy + Default>(
            mut arr: Array<T>,
            dims: Option<u8>,
            side: Option<SubSide>,
        ) -> Value
        where
            Value: From<Array<T>>,
        {
            let d = dims.unwrap_or(1) as usize;
            let len = arr.data.len();
            let blades = match side {
                None => 1 << d,
                Some(_) => (1 << d) / 2,
            };
            arr.shape.push(blades);
            if let (None, None | Some(SubSide::Right)) | (Some(_), Some(SubSide::Right)) =
                (dims, side)
            {
                let data = eco_vec![T::default(); arr.shape.elements()];
                return Array::new(arr.shape, data).into();
            }
            match blades {
                0 => return Array::<u8>::new(arr.shape, []).into(),
                1 => {}
                _ => {
                    arr.data.extend_repeat(&T::default(), len * (blades - 1));
                    let slice = arr.data.as_mut_slice();
                    for i in 0..len - 1 {
                        slice.swap(len - 1 - i, (len - 1) * blades - blades * i);
                    }
                }
            }
            arr.into()
        }

        let val = match self {
            Value::Byte(arr) => extract_scalar(arr, dims, side),
            Value::Num(arr) => extract_scalar(arr, dims, side),
            Value::Complex(mut arr) => match (dims, side) {
                (None, None) => {
                    // Vector
                    arr.shape.push(2);
                    let data = eco_vec![0.0; arr.shape.elements()];
                    Array::new(arr.shape, data).into()
                }
                (Some(d), None) => {
                    // All blades
                    let (mask_table, _) = mask_tables(d);
                    let size = 1 << d;
                    arr.shape.push(size);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    for (v, c) in data.make_mut().chunks_exact_mut(size).zip(arr.data) {
                        for (i, v) in v.iter_mut().enumerate() {
                            match mask_table[i] {
                                0b0 => *v = c.re,
                                0b11 => *v = c.im,
                                _ => {}
                            }
                        }
                    }
                    Array::new(arr.shape, data).into()
                }
                (d, Some(side)) => {
                    // Even or odd blades
                    let d = d.unwrap_or(2);
                    let size = (1 << d) / 2;
                    arr.shape.push(size);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    if side == SubSide::Left {
                        let (mask_table, _) = mask_tables(d);
                        for (v, c) in data.make_mut().chunks_exact_mut(size).zip(arr.data) {
                            for (v, mask) in (v.iter_mut())
                                .zip(mask_table.iter().filter(|m| m.count_ones() % 2 == 0))
                            {
                                match mask {
                                    0b0 => *v = c.re,
                                    0b11 => *v = c.im,
                                    _ => {}
                                }
                            }
                        }
                    }
                    Array::new(arr.shape, data).into()
                }
            },
            #[cfg(feature = "ga")]
            Value::Mv(mut arr) => match (dims, side) {
                (None, None) => {
                    // Vector
                    let d = arr.data.iter().map(|mv| mv.dims()).max().unwrap_or(0);
                    let size = d as usize;
                    arr.shape.push(size);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    if size > 0 {
                        let (mask_table, _) = mask_tables(d);
                        for (v, mv) in data.make_mut().chunks_exact_mut(size).zip(arr.data) {
                            for (i, v) in v.iter_mut().enumerate() {
                                *v = mv.get_blade(mask_table[i + 1]);
                            }
                        }
                    }
                    Array::new(arr.shape, data).into()
                }
                (Some(d), None) => {
                    // All blades
                    let (mask_table, _) = mask_tables(d);
                    let size = 1 << d;
                    arr.shape.push(size);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    for (v, mv) in data.make_mut().chunks_exact_mut(size).zip(arr.data) {
                        for (i, v) in v.iter_mut().enumerate() {
                            *v = mv.get_blade(mask_table[i]);
                        }
                    }
                    Array::new(arr.shape, data).into()
                }
                (d, Some(side)) => {
                    // Even or odd blades
                    let side_i = match side {
                        SubSide::Left => 0,
                        SubSide::Right => 1,
                    };
                    let d =
                        d.unwrap_or_else(|| arr.data.iter().map(|mv| mv.dims()).max().unwrap_or(0));
                    let (mask_table, _) = mask_tables(d);
                    let size = 1 << d.saturating_sub(1);
                    arr.shape.push(size);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    for (v, mv) in data.make_mut().chunks_exact_mut(size).zip(arr.data) {
                        for (v, &mask) in (v.iter_mut())
                            .zip(mask_table.iter().filter(|m| m.count_ones() % 2 == side_i))
                        {
                            *v = mv.get_blade(mask);
                        }
                    }
                    Array::new(arr.shape, data).into()
                }
            },
            val => {
                return Err(env.error(format!(
                    "Cannot decompose {} array as a multivector",
                    val.type_name()
                )));
            }
        };
        val.validate();
        Ok(val)
    }
}
