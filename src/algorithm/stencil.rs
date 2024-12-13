use std::{collections::VecDeque, mem::take};

use ecow::EcoVec;

use crate::{
    algorithm::{pervade::*, FillContext, MultiOutput},
    cowslice::extend_repeat,
    val_as_arr, Array, ArrayValue, Boxed, Node, Primitive, Shape, SigNode, Uiua, UiuaResult, Value,
};

use super::{get_ops, loops::flip, multi_output, Ops};

pub fn stencil(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f] = get_ops(ops, env)?;
    if f.sig.args == 0 {
        return Err(env.error(format!(
            "{}'s function must have at least 1 argument, but its signature is {}",
            Primitive::Stencil.format(),
            f.sig
        )));
    }
    // Adjacent stencil
    if f.sig.args > 1 {
        if env.value_fill().is_some() {
            return Err(env.error(format!(
                "Filled adjacent {} is not currently supported",
                Primitive::Stencil.format()
            )));
        }
        let xs = env.pop(1)?;
        let n = f.sig.args;
        return if f.sig == (2, 1) {
            adjacent_impl(f, xs, n, env)
        } else {
            if xs.row_count() < n {
                env.push(xs.first_dim_zero());
                return Ok(());
            }
            let win_count = xs.row_count() - (n - 1);
            let mut new_rows = multi_output(f.sig.outputs, Vec::with_capacity(win_count));
            for w in 0..win_count {
                for i in (0..n).rev() {
                    env.push(xs.row(w + i));
                }
                env.exec(f.clone())?;
                for i in 0..f.sig.outputs {
                    new_rows[i].push(env.pop("stencil's function result")?);
                }
            }
            for new_rows in new_rows.into_iter().rev() {
                env.push(Value::from_row_values(new_rows, env)?);
            }
            Ok(())
        };
    }
    // Default stencil
    let size = env.pop(1)?;
    let xs = env.pop(2)?;
    let has_fill = env.fill().value_for(&xs).is_some();
    let dims = derive_dims(&size, xs.shape(), has_fill, env)?;
    val_as_arr!(xs, |arr| stencil_array(arr, &dims, f, env))
}

fn stencil_array<T: ArrayValue>(
    arr: Array<T>,
    dims: &[WindowDim],
    f: SigNode,
    env: &mut Uiua,
) -> UiuaResult
where
    Array<T>: Into<Value>,
{
    enum WindowAction<T> {
        Id(EcoVec<T>),
        Box(EcoVec<Boxed>, EcoVec<T>),
        Default(MultiOutput<Vec<Value>>, EcoVec<T>),
    }

    let mut action: WindowAction<T> = match &f.node {
        Node::Prim(Primitive::Identity, _) => WindowAction::Id(EcoVec::new()),
        Node::Prim(Primitive::Box, _) => WindowAction::Box(EcoVec::new(), EcoVec::new()),
        _ => WindowAction::Default(multi_output(f.sig.outputs, Vec::new()), EcoVec::new()),
    };

    let mut corner_starts = vec![0isize; dims.len()];
    for (c, d) in corner_starts.iter_mut().zip(dims) {
        *c -= (d.fill * d.stride) as isize;
    }
    let mut maxs = vec![0isize; dims.len()];
    for ((m, d), s) in maxs.iter_mut().zip(dims).zip(&arr.shape) {
        *m = (*s + d.fill * d.stride) as isize;
    }
    let mut corner = corner_starts.clone();
    let mut curr = corner.clone();
    let mut offset = vec![0usize; corner.len()];
    let mut shape_prefix = Shape::SCALAR;
    for (d, s) in dims.iter().zip(&arr.shape) {
        let total_len = *s + 2 * d.fill * d.stride;
        shape_prefix.push((total_len + d.stride).saturating_sub(d.size) / d.stride);
    }
    let window_shape = Shape::from_iter(
        dims.iter()
            .map(|d| d.size)
            .chain(arr.shape.iter().skip(dims.len()).copied()),
    );
    if shape_prefix.contains(&0) {
        let mut shape = shape_prefix;
        shape.extend(window_shape);
        env.push(Array::new(shape, EcoVec::new()));
        return Ok(());
    }
    let cell_shape = Shape::from(&arr.shape[dims.len()..]);
    let cell_len = cell_shape.elements();
    let fill = env.scalar_fill::<T>();
    // dbg!(&arr.shape, &dims, &shape_prefix);
    // dbg!(&window_shape, &cell_shape, &maxs);
    env.without_fill(|env| -> UiuaResult {
        'windows: loop {
            // Reset offset
            for o in &mut offset {
                *o = 0;
            }
            // println!("corner: {corner:?}");
            'window: loop {
                // Update curr
                for (i, c) in curr.iter_mut().enumerate() {
                    *c = corner[i] + offset[i] as isize;
                }
                // Add cell
                if let Some(i) = arr.shape.i_dims_to_flat(&curr) {
                    match &mut action {
                        WindowAction::Id(data)
                        | WindowAction::Box(_, data)
                        | WindowAction::Default(_, data) => {
                            data.extend_from_slice(&arr.data[i * cell_len..][..cell_len])
                        }
                    }
                } else {
                    match &fill {
                        Ok(fill) => match &mut action {
                            WindowAction::Id(data)
                            | WindowAction::Box(_, data)
                            | WindowAction::Default(_, data) => extend_repeat(data, fill, cell_len),
                        },
                        Err(_) => {
                            return Err(env.error(
                                "Fill not present for filled stencil. \
                                This is a bug in the interpreter.",
                            ));
                        }
                    }
                }
                // Increment offset
                for (o, d) in offset.iter_mut().zip(dims).rev() {
                    if *o < d.size - 1 {
                        *o += 1;
                        continue 'window;
                    } else {
                        *o = 0;
                    }
                }
                break;
            }
            // End action window
            match &mut action {
                WindowAction::Id(_) => {}
                WindowAction::Box(boxes, data) => {
                    let arr = Array::new(window_shape.clone(), take(data));
                    boxes.push(Boxed(arr.into()));
                }
                WindowAction::Default(output, data) => {
                    let arr = Array::new(window_shape.clone(), take(data));
                    env.push(arr);
                    env.exec(f.clone())?;
                    for i in 0..f.sig.outputs {
                        output[i].push(env.pop("stencil's function result")?);
                    }
                }
            }
            // Increment corner
            for (i, c) in corner.iter_mut().enumerate().rev() {
                if *c < maxs[i] - dims[i].stride as isize - dims[i].size as isize + 1 {
                    *c += dims[i].stride as isize;
                    continue 'windows;
                } else {
                    *c = corner_starts[i];
                }
            }
            break;
        }
        match action {
            WindowAction::Id(data) => {
                let mut shape = shape_prefix;
                shape.extend(window_shape);
                let arr = Array::new(shape, data);
                env.push(arr);
            }
            WindowAction::Box(boxes, _) => {
                let arr = Array::new(shape_prefix, boxes);
                env.push(arr);
            }
            WindowAction::Default(outputs, _) => {
                for rows in outputs.into_iter().rev() {
                    let mut val = Value::from_row_values(rows, env)?;
                    let mut new_shape = shape_prefix.clone();
                    new_shape.extend_from_slice(&val.shape()[1..]);
                    *val.shape_mut() = new_shape;
                    val.validate_shape();
                    env.push(val);
                }
            }
        }
        Ok(())
    })
}

#[derive(Debug, Clone, Copy)]
struct WindowDim {
    size: usize,
    stride: usize,
    fill: usize,
}

fn derive_size(size: isize, dim: usize, env: &Uiua) -> UiuaResult<usize> {
    if size == 0 {
        return Err(env.error("Window size cannot be zero"));
    }
    Ok(if size > 0 {
        size
    } else if size.unsigned_abs() > dim {
        return Err(env.error(format!(
            "Window size {size} is too large for array of shape {dim}"
        )));
    } else {
        dim as isize + 1 + size
    } as usize)
}

fn derive_dims(
    size: &Value,
    shape: &Shape,
    has_fill: bool,
    env: &Uiua,
) -> UiuaResult<Vec<WindowDim>> {
    let ints = size.as_integer_array(env, "Window size must be an array of integers")?;
    let dims = match &*ints.shape {
        [] => {
            let size = derive_size(ints.data[0], shape.row_count(), env)?;
            vec![WindowDim {
                size,
                stride: 1,
                fill: (size - 1) * has_fill as usize,
            }]
        }
        &[n] => {
            let mut dims = Vec::with_capacity(n);
            for (size, dim) in ints.data.iter().zip(shape) {
                let size = derive_size(*size, *dim, env)?;
                dims.push(WindowDim {
                    size,
                    stride: 1,
                    fill: (size - 1) * has_fill as usize,
                });
            }
            dims
        }
        &[m, n] => {
            if m == 0 {
                return Err(env.error(format!(
                    "2D window size must have at least 1 row, \
                    but its shape is {shape}"
                )));
            }
            if m > 3 {
                return Err(env.error(format!(
                    "2D window size can have at most 3 rows, \
                    but its shape is {shape}"
                )));
            }
            let mut dims = Vec::with_capacity(n);
            for i in 0..n {
                let size = derive_size(ints.data[i], shape[i], env)?;
                let stride = ints.data.get(n + i).copied().unwrap_or(size as isize);
                if stride <= 0 {
                    return Err(env.error(format!(
                        "Window stride must be positive, \
                        but axis {i} has stride {stride}"
                    )));
                }
                let stride = stride as usize;
                let fill = ints
                    .data
                    .get(2 * n + i)
                    .copied()
                    .unwrap_or((size as isize - 1) * has_fill as isize);
                if fill < 0 {
                    return Err(env.error(format!(
                        "Window fill size must be non-negative, \
                        but axis {i} has fill size {fill}"
                    )));
                }
                let fill = fill as usize;
                dims.push(WindowDim { size, stride, fill });
            }
            dims
        }
        _ => {
            return Err(env.error(format!(
                "Window size may be at most rank 2, but its shape is {shape}"
            )))
        }
    };
    if dims.len() > shape.len() {
        return Err(env.error(format!(
            "Window size specifies {} axes, \
            which is too many for array of shape {shape}",
            dims.len()
        )));
    }
    Ok(dims)
}

fn adjacent_impl(f: SigNode, xs: Value, n: usize, env: &mut Uiua) -> UiuaResult {
    match (f.node.as_flipped_primitive(), xs) {
        (Some((prim, flipped)), Value::Num(nums)) => env.push(match prim {
            Primitive::Add => fast_adjacent(nums, n, env, add::num_num),
            Primitive::Sub if flipped => fast_adjacent(nums, n, env, flip(sub::num_num)),
            Primitive::Sub => fast_adjacent(nums, n, env, sub::num_num),
            Primitive::Mul => fast_adjacent(nums, n, env, mul::num_num),
            Primitive::Div if flipped => fast_adjacent(nums, n, env, flip(div::num_num)),
            Primitive::Div => fast_adjacent(nums, n, env, div::num_num),
            Primitive::Modulus if flipped => fast_adjacent(nums, n, env, flip(modulus::num_num)),
            Primitive::Modulus => fast_adjacent(nums, n, env, modulus::num_num),
            Primitive::Or => fast_adjacent(nums, n, env, or::num_num),
            Primitive::Atan if flipped => fast_adjacent(nums, n, env, flip(atan2::num_num)),
            Primitive::Atan => fast_adjacent(nums, n, env, atan2::num_num),
            Primitive::Max => fast_adjacent(nums, n, env, max::num_num),
            Primitive::Min => fast_adjacent(nums, n, env, min::num_num),
            _ => return generic_adjacent(f, Value::Num(nums), n, env),
        }?),
        (Some((prim, flipped)), Value::Byte(bytes)) => env.push::<Value>(match prim {
            Primitive::Add => fast_adjacent(bytes.convert(), n, env, add::num_num)?.into(),
            Primitive::Sub if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(sub::num_num))?.into()
            }
            Primitive::Sub => fast_adjacent(bytes.convert(), n, env, sub::num_num)?.into(),
            Primitive::Mul => fast_adjacent(bytes.convert(), n, env, mul::num_num)?.into(),
            Primitive::Div if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(div::num_num))?.into()
            }
            Primitive::Div => fast_adjacent(bytes.convert(), n, env, div::num_num)?.into(),
            Primitive::Modulus if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(modulus::num_num))?.into()
            }
            Primitive::Modulus => fast_adjacent(bytes.convert(), n, env, modulus::num_num)?.into(),
            Primitive::Atan if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(atan2::num_num))?.into()
            }
            Primitive::Atan => fast_adjacent(bytes.convert(), n, env, atan2::num_num)?.into(),
            Primitive::Max => fast_adjacent(bytes, n, env, max::byte_byte)?.into(),
            Primitive::Min => fast_adjacent(bytes, n, env, min::byte_byte)?.into(),
            _ => return generic_adjacent(f, Value::Byte(bytes), n, env),
        }),
        (_, xs) => generic_adjacent(f, xs, n, env)?,
    }
    Ok(())
}

fn fast_adjacent<T>(
    mut arr: Array<T>,
    n: usize,
    env: &Uiua,
    f: impl Fn(T, T) -> T,
) -> UiuaResult<Array<T>>
where
    T: Copy,
{
    match arr.rank() {
        0 => Err(env.error("Cannot get adjacency of scalar")),
        1 => {
            if arr.row_count() < n {
                return Ok(Array::new([0], EcoVec::new()));
            }
            let data = arr.data.as_mut_slice();
            for i in 0..data.len() - (n - 1) {
                let start = i;
                for j in 1..n {
                    data[start] = f(data[start], data[start + j]);
                }
            }
            arr.data.truncate(arr.data.len() - (n - 1));
            arr.shape[0] -= n - 1;
            arr.validate_shape();
            Ok(arr)
        }
        _ => {
            let row_len = arr.row_len();
            let row_count = arr.row_count();
            if row_count < n {
                let mut shape = arr.shape;
                shape[0] = 0;
                return Ok(Array::new(shape, EcoVec::new()));
            }
            let data = arr.data.as_mut_slice();
            for i in 0..row_count - (n - 1) {
                let start = i * row_len;
                for j in 1..n {
                    let next = (i + j) * row_len;
                    for k in 0..row_len {
                        data[start + k] = f(data[start + k], data[next + k]);
                    }
                }
            }
            arr.data.truncate(arr.data.len() - (n - 1) * row_len);
            arr.shape[0] -= n - 1;
            arr.validate_shape();
            Ok(arr)
        }
    }
}

fn generic_adjacent(f: SigNode, xs: Value, n: usize, env: &mut Uiua) -> UiuaResult {
    let sig = f.sig;
    if sig != (2, 1) {
        return Err(env.error(format!(
            "Dyadic {}'s function must have 1 output, \
            but its signature is {sig}",
            Primitive::Stencil.format()
        )));
    }
    if xs.row_count() < n {
        env.push(xs.first_dim_zero());
        return Ok(());
    }
    let win_count = xs.row_count() - (n - 1);
    let mut rows = xs.into_rows();
    let mut window = VecDeque::with_capacity(n);
    let mut new_rows = Vec::with_capacity(win_count);
    window.extend(rows.by_ref().take(n));
    for _ in 0..win_count {
        let mut acc = window.pop_front().unwrap();
        for row in &window {
            env.push(row.clone());
            env.push(acc);
            env.exec(f.clone())?;
            acc = env.pop("adjacent function result")?;
        }
        new_rows.push(acc);
        window.extend(rows.next());
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}
