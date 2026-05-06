use ecow::eco_vec;

#[cfg(feature = "ga")]
use crate::Multivector as Mv;
use crate::{Array, Complex, MvMode, SubSide, Uiua, UiuaResult, Value, ga::*};

impl Value {
    pub(crate) fn multivector(self, mode: MvMode, env: &Uiua) -> UiuaResult<Self> {
        #[cfg(feature = "ga")]
        use crate::Multivector as Mv;
        let arr = match self {
            Value::Num(arr) => arr,
            Value::Byte(arr) => arr.convert(),
            #[cfg(feature = "ga")]
            Value::Complex(arr) => return Ok(arr.convert::<Mv>().into()),
            #[cfg(feature = "ga")]
            Value::Mv(mv) => return Ok(mv.into()),
            val => {
                return Err(env.error(format!(
                    "Cannot create multivector from {} array",
                    val.type_name()
                )));
            }
        };
        let _old_shape = arr.shape.clone();
        let mut new_shape = arr.shape.clone();
        let n = new_shape.pop().unwrap_or(1);
        let elem_count = new_shape.elements();
        Ok(match (n, mode.flavor, mode.dims, mode.side_grade) {
            (0, Flavor::Vanilla, None, _) => {
                Array::<Complex>::new(new_shape, eco_vec![Complex::ZERO; elem_count]).into()
            }
            (0, Flavor::Vanilla, Some(d), _) if d <= 2 => {
                Array::<Complex>::new(new_shape, eco_vec![Complex::ZERO; elem_count]).into()
            }
            (1, Flavor::Vanilla, None | Some(2), Some((SubSide::Left, _))) => {
                arr.convert::<Complex>().into()
            }
            (2, Flavor::Vanilla, None | Some(2), Some((SubSide::Left, _)))
            | (2, Flavor::Vanilla, Some(2), None) => Array::new(
                new_shape,
                arr.data.chunks_exact(n).map(|n| Complex::new(n[0], n[1])),
            )
            .into(),
            #[cfg(feature = "ga")]
            (0, flavor, ..) => Array::new(
                new_shape,
                eco_vec![Mv::default().flavor(flavor); elem_count],
            )
            .into(),
            #[cfg(feature = "ga")]
            (1, flavor, _, None | Some((SubSide::Left, _)))
            | (1, flavor, None, Some((SubSide::Right, _))) => Array::new(
                new_shape,
                arr.data.into_iter().map(|n| Mv::from(n).flavor(flavor)),
            )
            .into(),
            #[cfg(feature = "ga")]
            (1, flavor, Some(d), Some((SubSide::Right, None | Some(1)))) => {
                // Pseudoscalar
                Array::new(
                    new_shape,
                    arr.data
                        .into_iter()
                        .map(|n| Mv::pseudoscalar(d, n).flavor(flavor)),
                )
                .into()
            }
            #[cfg(feature = "ga")]
            (1.., flavor, None, None) => {
                // Vector
                if n > MAX_DIMS as usize {
                    return Err(env.error(format!(
                        "{n} multivector dimensions \
                        (from a {_old_shape} array) would be too many"
                    )));
                }
                Array::new(
                    new_shape,
                    arr.data
                        .chunks_exact(n)
                        .map(|n| Mv::vector(n).flavor(flavor)),
                )
                .into()
            }
            #[cfg(feature = "ga")]
            (1.., flavor, None, Some((side, grade))) => {
                // Dimensions not specified, but a grade is
                use uiua_parser::SubSide;
                let d = if let Some(grade) = grade {
                    (grade..=MAX_DIMS)
                        .find(|&d| grade_size(d, grade) == n)
                        .ok_or_else(|| {
                            env.error(format!(
                        "Unable to find a number of dimensions where grade {grade} has {n} blades"
                    ))
                        })?
                } else if (n * 2).is_power_of_two() {
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
                        SubSide::Left => Mv::vector,
                        SubSide::Right => Mv::n_1_blades,
                    };
                    return Ok(Array::new(
                        new_shape,
                        (arr.data.chunks_exact(n)).map(|n| f(n).flavor(flavor)),
                    )
                    .into());
                };
                let f = match side {
                    SubSide::Left => Mv::blades_left,
                    SubSide::Right => Mv::blades_right,
                };
                Array::new(
                    new_shape,
                    (arr.data.chunks_exact(n)).map(|n| f(d, n).unwrap().flavor(flavor)),
                )
                .into()
            }
            #[cfg(feature = "ga")]
            (1.., flavor, Some(d), side) => {
                // Dimensions specified

                use ecow::EcoVec;
                if d > MAX_DIMS {
                    return Err(env.error(format!("{d} multivector dimensions would be too many")));
                }
                let (side, grade) = side.unwrap_or((SubSide::Left, None));
                if let Some(grade) = grade {
                    let grade_size = grade_size(d, grade);
                    if n != grade_size {
                        return Err(env.error(format!(
                            "Grade {grade} of a {d}D multivector \
                            has {grade_size} blade{}, not {n}",
                            if grade_size == 1 { "" } else { "s" }
                        )));
                    }
                }

                let f = match side {
                    SubSide::Left => Mv::blades_left,
                    SubSide::Right => Mv::blades_right,
                };
                let mut data = EcoVec::with_capacity(elem_count);
                for slice in arr.data.chunks_exact(n) {
                    match f(d, slice) {
                        Ok(mv) => data.push(mv.flavor(flavor)),
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
        let (dims, side, grade) = (
            mode.dims,
            mode.side_grade.map(|(side, _)| side),
            mode.side_grade.and_then(|(_, grade)| grade),
        );
        self.meta.take_sorted_flags();
        self.meta.take_per_meta();

        fn extract_scalar<T: Copy + Default>(
            mut arr: Array<T>,
            dims: Option<u8>,
            grade: Option<u8>,
            env: &Uiua,
        ) -> UiuaResult<Value>
        where
            Value: From<Array<T>>,
        {
            Ok(match (dims, grade) {
                (None | Some(1..), Some(0)) => {
                    arr.shape.push(1);
                    arr.into()
                }
                (d, Some(grade)) => {
                    if let Some(d) = d
                        && grade > d
                    {
                        return Err(
                            env.error(format!("{d}D multivector has no grade-{grade} blades"))
                        );
                    }
                    let d = d.unwrap_or(1);
                    let size = grade_size(d, grade);
                    arr.shape.push(size);
                    let data = eco_vec![0u8; arr.shape.elements()];
                    Array::new(arr.shape, data).into()
                }
                (d, None) => {
                    let d = d.unwrap_or(1) as usize;
                    let len = arr.data.len();
                    arr.shape.push(d);
                    match d {
                        0 => return Ok(Array::<u8>::new(arr.shape, []).into()),
                        1 => {}
                        d => {
                            arr.data.extend_repeat(&T::default(), len * (d - 1));
                            let slice = arr.data.as_mut_slice();
                            for i in 0..len - 1 {
                                slice.swap(len - 1 - i, (len - 1) * d - d * i);
                            }
                        }
                    }
                    arr.into()
                }
            })
        }

        let val = match self {
            Value::Byte(arr) => extract_scalar(arr, dims, grade, env)?,
            Value::Num(arr) => extract_scalar(arr, dims, grade, env)?,
            Value::Complex(mut arr) => match (dims, side, grade) {
                (Some(d), _, Some(grade @ 3..)) => {
                    arr.shape.push(grade_size(d, grade));
                    let data = eco_vec![0u8; arr.shape.elements()];
                    Array::new(arr.shape, data).into()
                }
                (None, None, None) | (_, Some(SubSide::Right), _) | (_, _, Some(1)) => {
                    arr.shape.push(2);
                    let data = eco_vec![0u8; arr.shape.elements()];
                    Array::new(arr.shape, data).into()
                }
                (Some(2), _, None) | (None, Some(SubSide::Left), None) => {
                    arr.shape.push(2);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    for (w, c) in data.make_mut().chunks_exact_mut(2).zip(arr.data) {
                        w[1] = c.im;
                        w[0] = c.re;
                    }
                    Array::new(arr.shape, data).into()
                }
                (None | Some(2), _, Some(0)) => {
                    let mut data = eco_vec![0f64; arr.shape.elements()];
                    for (n, c) in data.make_mut().iter_mut().zip(arr.data) {
                        *n = c.re;
                    }
                    Array::new(arr.shape, data).into()
                }
                (None | Some(2), _, Some(2)) => {
                    let mut data = eco_vec![0f64; arr.shape.elements()];
                    for (n, c) in data.make_mut().iter_mut().zip(arr.data) {
                        *n = c.im;
                    }
                    Array::new(arr.shape, data).into()
                }
                #[cfg(feature = "ga")]
                (Some(_), _, Some(..=2) | None) => {
                    // Weird case
                    Value::from(arr.convert::<Mv>()).unmultivector(mode, env)?
                }
                (d, _, Some(grade)) => {
                    if let Some(d) = d
                        && grade > d
                    {
                        return Err(
                            env.error(format!("{d}D multivector has no grade-{grade} blades"))
                        );
                    }
                    let d = d.unwrap_or(2);
                    let size = grade_size(d, grade);
                    arr.shape.push(size);
                    let data = eco_vec![0u8; arr.shape.elements()];
                    Array::new(arr.shape, data).into()
                }
            },
            #[cfg(feature = "ga")]
            Value::Mv(mut arr) => match (dims, side, grade) {
                (None, None, None) => {
                    // Vector
                    let d = arr.data.iter().map(|mv| mv.dims()).max().unwrap_or(0);
                    let size = d as usize;
                    arr.shape.push(size);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    let (mask_table, _) = mask_tables(d);
                    for (v, mv) in data.make_mut().chunks_exact_mut(size).zip(arr.data) {
                        for (i, v) in v.iter_mut().enumerate() {
                            *v = mv.get_blade(mask_table[i + 1]);
                        }
                    }
                    Array::new(arr.shape, data).into()
                }
                (Some(d), None, None) => {
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
                (d, _, Some(grade)) => {
                    // Blades of a certain grade
                    let d =
                        d.unwrap_or_else(|| arr.data.iter().map(|mv| mv.dims()).max().unwrap_or(0));
                    if grade > d {
                        return Err(
                            env.error(format!("{d}D multivector has no grade-{grade} blades"))
                        );
                    }
                    let (mask_table, _) = mask_tables(d);
                    let size = grade_size(d, grade);
                    arr.shape.push(size);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    let offset = blade_grades(d).position(|g| g == grade).unwrap();
                    for (v, mv) in data.make_mut().chunks_exact_mut(size).zip(arr.data) {
                        for (i, v) in v.iter_mut().enumerate() {
                            *v = mv.get_blade(mask_table[i + offset]);
                        }
                    }
                    Array::new(arr.shape, data).into()
                }
                (d, Some(side), None) => {
                    // Even or odd blades
                    let side = match side {
                        SubSide::Left => 0,
                        SubSide::Right => 1,
                    };
                    let d =
                        d.unwrap_or_else(|| arr.data.iter().map(|mv| mv.dims()).max().unwrap_or(0));
                    let (mask_table, _) = mask_tables(d);
                    let size = (1 << d) / 2;
                    arr.shape.push(size);
                    let mut data = eco_vec![0.0; arr.shape.elements()];
                    for (v, mv) in data.make_mut().chunks_exact_mut(size).zip(arr.data) {
                        for (v, &mask) in (v.iter_mut())
                            .zip(mask_table.iter().filter(|m| m.count_ones() % 2 == side))
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
