use std::{collections::VecDeque, mem::take};

use ecow::EcoVec;
use uiua_parser::SubSide;

use crate::{
    algorithm::{
        pervade::*,
        validate_size, validate_size_of,
        zip::{f_mon_fast_fn, ValueMonFn},
        FillContext, MultiOutput,
    },
    cowslice::extend_repeat,
    types::push_empty_rows_value,
    val_as_arr, Array, ArrayValue, Boxed, Node, Primitive, Shape, SigNode, Uiua, UiuaResult, Value,
};

use super::{get_ops, loops::flip, multi_output, Ops};

pub fn stencil(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f] = get_ops(ops, env)?;
    if f.sig.args() == 0 {
        return Err(env.error(format!(
            "{}'s function must have at least 1 argument, but its signature is {}",
            Primitive::Stencil.format(),
            f.sig
        )));
    }
    // Adjacent stencil
    if f.sig.args() > 1 {
        let mut xs = env.pop(1)?;
        xs.match_fill(env);
        let n = f.sig.args();
        return if xs.row_count() < n
            && xs.fill(env).is_err()
            && push_empty_rows_value(&f, [&xs, &xs], false, &mut Default::default(), env)
        {
            Ok(())
        } else if f.sig == (2, 1) {
            adjacent_impl(f, xs, n, env)
        } else {
            val_as_arr!(&mut xs, |arr| pad_adjacent_fill(arr, n, env));
            if xs.row_count() < n {
                for _ in 0..f.sig.outputs() {
                    env.push(xs.first_dim_zero());
                }
                return Ok(());
            }
            let win_count = xs.row_count() - (n - 1);
            let mut new_rows = multi_output(f.sig.outputs(), Vec::with_capacity(win_count));
            for w in 0..win_count {
                for i in (0..n).rev() {
                    env.push(xs.row(w + i));
                }
                env.exec(f.clone())?;
                for i in 0..f.sig.outputs() {
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
    let mut xs = env.pop(2)?;
    xs.match_fill(env);
    let has_fill = env.fill().value_for(&xs).is_some();
    let dims = derive_dims(&size, &xs.shape, has_fill, env)?;
    val_as_arr!(xs, |arr| stencil_array(arr, &dims, f, env))
}

fn stencil_array<T: ArrayValue>(
    mut arr: Array<T>,
    dims: &[WindowDim],
    f: SigNode,
    env: &mut Uiua,
) -> UiuaResult
where
    Array<T>: Into<Value>,
{
    enum WindowAction<T> {
        Id(EcoVec<T>, Option<(ValueMonFn, usize)>),
        Box(EcoVec<Boxed>, EcoVec<T>),
        Default(MultiOutput<Vec<Value>>, EcoVec<T>),
    }

    // Determine shape stuff
    let fill = env.scalar_fill::<T>().ok();
    let mut shape_prefix = Shape::SCALAR;
    let fill_side_count = if fill.as_ref().is_some_and(|fv| fv.side.is_some()) {
        1
    } else {
        2
    };
    for (d, s) in dims.iter().zip(&arr.shape) {
        let total_len = *s + fill_side_count * d.fill * d.stride;
        shape_prefix.push((total_len + d.stride).saturating_sub(d.size) / d.stride);
    }
    let window_shape = Shape::from_iter(
        dims.iter()
            .map(|d| d.size)
            .chain(arr.shape.iter().skip(dims.len()).copied()),
    );

    // Initialize action
    let mut action: WindowAction<T> = match &f.node {
        Node::Prim(Primitive::Identity, _) => {
            let size = validate_size::<T>(
                (shape_prefix.iter().copied()).chain(window_shape.iter().copied()),
                env,
            )?;
            WindowAction::Id(EcoVec::with_capacity(size), None)
        }
        Node::Prim(Primitive::Box, _) => WindowAction::Box(EcoVec::new(), EcoVec::new()),
        node => {
            if let Some((f, size)) = f_mon_fast_fn(node, env)
                .filter(|_| !shape_prefix.contains(&0))
                .and_then(|f| {
                    validate_size_of::<T>(
                        (shape_prefix.iter().copied()).chain(window_shape.iter().copied()),
                    )
                    .ok()
                    .map(|size| (f, size))
                })
            {
                WindowAction::Id(EcoVec::with_capacity(size), Some(f))
            } else {
                WindowAction::Default(multi_output(f.sig.outputs(), Vec::new()), EcoVec::new())
            }
        }
    };

    if shape_prefix.contains(&0) {
        arr.fix();
        if !(matches!(action, WindowAction::Default(..))
            && push_empty_rows_value(&f, [&arr.into()], false, &mut Default::default(), env))
        {
            let mut shape = shape_prefix;
            shape.extend(window_shape);
            for _ in 0..f.sig.outputs() {
                env.push(Array::new(shape.clone(), EcoVec::new()));
            }
        }
        return Ok(());
    }

    if dims.len() == 1 && (fill.is_none() || dims[0].fill == 0) {
        // Linear optimization
        let dim = dims[0];
        if dim.size == dim.stride && matches!(&action, WindowAction::Id(..)) {
            // Simple chunking
            let chunk_count = arr.shape[0] / dim.size;
            arr.shape[0] = dim.size;
            arr.shape.prepend(chunk_count);
            arr.data.truncate(arr.shape.elements());
            arr.meta.take_map_keys();
            let mut val: Value = arr.into();
            if let WindowAction::Id(_, Some((f, d))) = &action {
                val = f(val, dims.len() + d, env)?;
            }
            env.push(val);
            return Ok(());
        } else {
            // General case
            let row_len = arr.row_len();
            let win_count = arr
                .row_count()
                .saturating_sub(dim.size.saturating_sub(dim.stride))
                / dim.stride;
            match action {
                WindowAction::Id(mut data, f) => {
                    for i in 0..win_count {
                        data.extend_from_slice(
                            &arr.data[i * dim.stride * row_len..][..dim.size * row_len],
                        );
                    }
                    let mut new_shape = arr.shape;
                    new_shape[0] = dim.size;
                    new_shape.prepend(win_count);
                    let mut val: Value = Array::new(new_shape, data).into();
                    if let Some((f, d)) = f {
                        val = f(val, dims.len() + d, env)?;
                    }
                    env.push(val);
                    return Ok(());
                }
                WindowAction::Box(mut boxes, _) => {
                    for i in 0..win_count {
                        boxes.push(Boxed(
                            arr.slice_rows(i * dim.stride, i * dim.stride + dim.size)
                                .into(),
                        ));
                    }
                    env.push(Array::from(boxes));
                    return Ok(());
                }
                WindowAction::Default(..) => {}
            }
        }
    }

    let mut corner_starts = vec![0isize; dims.len()];
    let (fill_is_left, fill_is_right) =
        fill.as_ref()
            .and_then(|fv| fv.side)
            .map_or((1, 1), |side| match side {
                SubSide::Left => (1, 0),
                SubSide::Right => (0, 1),
            });
    for (c, d) in corner_starts.iter_mut().zip(dims) {
        *c -= (d.fill * fill_is_left * d.stride) as isize;
    }
    let mut maxs = vec![0isize; dims.len()];
    for ((m, d), s) in maxs.iter_mut().zip(dims).zip(&arr.shape) {
        *m = (*s + d.fill * fill_is_right * d.stride) as isize;
    }
    let mut corner = corner_starts.clone();
    let mut curr = corner.clone();
    let mut offset = vec![0usize; corner.len()];
    let cell_shape = Shape::from(&arr.shape[dims.len()..]);
    let cell_len = cell_shape.elements();
    let fill = fill.map(|fv| fv.value).unwrap_or_else(T::proxy);
    // dbg!(&arr.shape, &dims, &shape_prefix);
    // dbg!(&window_shape, &cell_shape, &maxs);
    env.without_fill(|env| -> UiuaResult {
        'windows: loop {
            // Reset offset
            offset.fill(0);
            // println!("corner: {corner:?}");
            'window: loop {
                // Update curr
                for (i, c) in curr.iter_mut().enumerate() {
                    *c = corner[i] + offset[i] as isize;
                }
                // Add cell
                if let Some(i) = arr.shape.i_dims_to_flat(&curr) {
                    match &mut action {
                        WindowAction::Id(data, _)
                        | WindowAction::Box(_, data)
                        | WindowAction::Default(_, data) => {
                            data.extend_from_slice(&arr.data[i * cell_len..][..cell_len])
                        }
                    }
                } else {
                    match &mut action {
                        WindowAction::Id(data, _)
                        | WindowAction::Box(_, data)
                        | WindowAction::Default(_, data) => extend_repeat(data, &fill, cell_len),
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
                WindowAction::Id(..) => {}
                WindowAction::Box(boxes, data) => {
                    let arr = Array::new(window_shape.clone(), take(data));
                    boxes.push(Boxed(arr.into()));
                }
                WindowAction::Default(output, data) => {
                    let arr = Array::new(window_shape.clone(), take(data));
                    env.push(arr);
                    env.exec(f.clone())?;
                    for i in 0..f.sig.outputs() {
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
            WindowAction::Id(data, f) => {
                let mut shape = shape_prefix;
                shape.extend(window_shape);
                let mut val: Value = Array::new(shape, data).into();
                if let Some((f, d)) = f {
                    val = f(val, dims.len() + d, env)?;
                }
                env.push(val);
            }
            WindowAction::Box(boxes, _) => {
                let arr = Array::new(shape_prefix, boxes);
                env.push(arr);
            }
            WindowAction::Default(outputs, _) => {
                for rows in outputs.into_iter().rev() {
                    let mut val = Value::from_row_values(rows, env)?;
                    let mut new_shape = shape_prefix.clone();
                    new_shape.extend_from_slice(&val.shape[1..]);
                    val.shape = new_shape;
                    val.validate();
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

fn derive_size(size: isize, dim: usize, chunk: bool, env: &Uiua) -> UiuaResult<usize> {
    if size == 0 {
        return Err(env.error("Window size cannot be zero"));
    }
    Ok(if size > 0 {
        size
    } else if size.unsigned_abs() > dim {
        return Err(env.error(format!(
            "Window size {size} is too large for array of shape {dim}"
        )));
    } else if chunk {
        dim as isize / size.abs()
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
            if shape.is_empty() {
                return Err(env.error("Cannot get windows from a scalar"));
            }
            let size = derive_size(ints.data[0], shape.row_count(), false, env)?;
            vec![WindowDim {
                size,
                stride: 1,
                fill: (size - 1) * has_fill as usize,
            }]
        }
        &[n] => {
            if n > shape.len() {
                return Err(env.error(format!(
                    "Window size specifies {n} axes, \
                    which is too many for array of shape {shape}"
                )));
            }
            let mut dims = Vec::with_capacity(n);
            for (size, dim) in ints.data.iter().zip(shape) {
                let size = derive_size(*size, *dim, false, env)?;
                dims.push(WindowDim {
                    size,
                    stride: 1,
                    fill: (size - 1) * has_fill as usize,
                });
            }
            dims
        }
        &[m, n] => {
            if n > shape.len() {
                return Err(env.error(format!(
                    "Window size specifies {n} axes, \
                    which is too many for array of shape {shape}"
                )));
            }
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
                let size = derive_size(ints.data[i], shape[i], true, env)?;
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
                let fill = fill as usize * has_fill as usize;
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
            Primitive::Modulo if flipped => fast_adjacent(nums, n, env, flip(modulo::num_num)),
            Primitive::Modulo => fast_adjacent(nums, n, env, modulo::num_num),
            Primitive::Or => fast_adjacent(nums, n, env, or::num_num),
            Primitive::Atan if flipped => fast_adjacent(nums, n, env, flip(atan2::num_num)),
            Primitive::Atan => fast_adjacent(nums, n, env, atan2::num_num),
            Primitive::Max => fast_adjacent(nums, n, env, max::num_num),
            Primitive::Min => fast_adjacent(nums, n, env, min::num_num),
            Primitive::Eq => fast_adjacent(nums, n, env, |a, b| is_eq::num_num(a, b) as f64),
            Primitive::Ne => fast_adjacent(nums, n, env, |a, b| is_ne::num_num(a, b) as f64),
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
            Primitive::Modulo if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(modulo::num_num))?.into()
            }
            Primitive::Modulo => fast_adjacent(bytes.convert(), n, env, modulo::num_num)?.into(),
            Primitive::Atan if flipped => {
                fast_adjacent(bytes.convert(), n, env, flip(atan2::num_num))?.into()
            }
            Primitive::Atan => fast_adjacent(bytes.convert(), n, env, atan2::num_num)?.into(),
            Primitive::Max => fast_adjacent(bytes, n, env, max::byte_byte)?.into(),
            Primitive::Min => fast_adjacent(bytes, n, env, min::byte_byte)?.into(),
            Primitive::Eq => fast_adjacent(bytes, n, env, is_eq::same_type)?.into(),
            Primitive::Ne => fast_adjacent(bytes, n, env, is_ne::same_type)?.into(),
            _ => return generic_adjacent(f, Value::Byte(bytes), n, env),
        }),
        (Some((prim, _)), Value::Char(chars)) => env.push(match prim {
            Primitive::Eq => {
                fast_adjacent(chars, n, env, is_eq::same_type)?.convert_with(|c| c as u8)
            }
            Primitive::Ne => {
                fast_adjacent(chars, n, env, is_ne::same_type)?.convert_with(|c| c as u8)
            }
            _ => return generic_adjacent(f, Value::Char(chars), n, env),
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
    T: ArrayValue + Copy,
{
    pad_adjacent_fill(&mut arr, n, env);
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
            arr.meta.take_sorted_flags();
            arr.validate();
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
            arr.meta.take_sorted_flags();
            arr.validate();
            Ok(arr)
        }
    }
}

fn pad_adjacent_fill<T: ArrayValue>(arr: &mut Array<T>, n: usize, env: &Uiua) {
    if let Ok(fv) = env.scalar_fill::<T>() {
        let row_len = arr.row_len();
        match fv.side {
            None => {
                arr.data.extend_repeat(&fv.value, (n - 1) * row_len * 2);
                arr.data.as_mut_slice().rotate_right((n - 1) * row_len);
                *arr.shape.row_count_mut() += (n - 1) * 2;
            }
            Some(side) => {
                arr.data.extend_repeat(&fv.value, (n - 1) * row_len);
                if side == SubSide::Left {
                    arr.data.as_mut_slice().rotate_right((n - 1) * row_len);
                }
                *arr.shape.row_count_mut() += n - 1;
            }
        }
    }
}

fn generic_adjacent(f: SigNode, mut xs: Value, n: usize, env: &mut Uiua) -> UiuaResult {
    let sig = f.sig;
    if sig != (2, 1) {
        return Err(env.error(format!(
            "Dyadic {}'s function must have 1 output, \
            but its signature is {sig}",
            Primitive::Stencil.format()
        )));
    }
    val_as_arr!(&mut xs, |arr| pad_adjacent_fill(arr, n, env));
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
