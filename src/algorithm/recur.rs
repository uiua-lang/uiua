use crate::{SigNode, Uiua, UiuaResult, Value};

pub fn recur(is_leaf: SigNode, children: SigNode, combine: SigNode, env: &mut Uiua) -> UiuaResult {
    // Signature validation
    if is_leaf.sig.args() == 0 {
        return Err(env.error(format!(
            "Leaf function must have at least 1 argument, but its signature is {}",
            is_leaf.sig
        )));
    }
    if is_leaf.sig.outputs() != 1 {
        return Err(env.error(format!(
            "Leaf function must have 1 output, but its signature is {}",
            is_leaf.sig
        )));
    }
    if children.sig.args() == 0 {
        return Err(env.error(format!(
            "Children function must have at least 1 argument, but its signature is {}",
            children.sig
        )));
    }
    if children.sig.outputs() != 1 {
        return Err(env.error(format!(
            "Children function must have 1 output, but its signature is {}",
            children.sig
        )));
    }
    if combine.sig.args() == 0 {
        return Err(env.error(format!(
            "Combine function must have at least 1 argument, but its signature is {}",
            combine.sig
        )));
    }
    if combine.sig.outputs() != 1 {
        return Err(env.error(format!(
            "Combine function must have 1 output, but its signature is {}",
            combine.sig
        )));
    }

    // State initialization
    let arg_count = (is_leaf.sig.args())
        .max(children.sig.args())
        .max(combine.sig.args() - 1);
    let const_count = arg_count - 1;
    let initial = env.pop(1)?;
    let mut consts = Vec::with_capacity(const_count);
    for i in 0..const_count {
        consts.push(env.pop(i + 2)?);
    }

    struct RecNode {
        parent: Option<usize>,
        value: Value,
        child_nodes: Option<Vec<Value>>,
        scalar_child: bool,
    }
    let mut stack = vec![RecNode {
        parent: None,
        value: initial,
        child_nodes: None,
        scalar_child: false,
    }];

    // Run algorithm
    while let Some(RecNode {
        parent,
        mut value,
        child_nodes,
        scalar_child,
    }) = stack.pop()
    {
        // println!("value: {value:?}, parent: {parent:?}, child_nodes: {child_nodes:?}");
        if let Some(child_nodes) = child_nodes {
            env.push_all(
                (consts.iter())
                    .take(is_leaf.sig.args().saturating_sub(2))
                    .cloned(),
            );
            let children_value = if scalar_child && child_nodes.len() == 1 {
                child_nodes.into_iter().next().unwrap()
            } else if child_nodes
                .iter()
                .all(|val| matches!(val, Value::Box(_)) && val.rank() <= 1)
            {
                let mut child_nodes = child_nodes.into_iter();
                let mut val = child_nodes.next().unwrap();
                for child in child_nodes {
                    val = val.join(child, false, env)?;
                }
                val
            } else {
                Value::from_row_values(child_nodes, env)?
            };
            env.push(children_value);
            if combine.sig.args() > 1 {
                env.push(value);
            }
            env.exec(combine.clone())?;
            value = env.pop("combined")?;
            if let Some(parent) = parent {
                stack[parent].child_nodes.as_mut().unwrap().push(value);
            } else {
                env.push(value);
                break;
            }
        } else {
            env.push_all(consts.iter().take(is_leaf.sig.args() - 1).cloned());
            env.push(value.clone());
            env.exec(is_leaf.clone())?;
            let this_is_leaf = env.pop("leaf check result")?;
            let this_is_leaf = match this_is_leaf {
                Value::Num(arr) if arr.rank() == 0 && arr.data[0] == 1.0 => true,
                Value::Num(arr) if arr.rank() == 0 && arr.data[0] == 0.0 => false,
                Value::Byte(arr) if arr.rank() == 0 && arr.data[0] == 1 => true,
                Value::Byte(arr) if arr.rank() == 0 && arr.data[0] == 0 => false,
                value if value.rank() >= 1 && value.row_count() == 0 => false,
                mut val if val.rank() >= 1 && val.row_count() == 1 => {
                    val.shape.make_row();
                    value = val;
                    true
                }
                value => {
                    return Err(env.error(format!(
                        "Leaf check result must be a boolean or have \
                        a length of 1 but it is {} {}",
                        value.shape,
                        value.type_name_plural()
                    )))
                }
            };
            if this_is_leaf {
                // This is a leaf node
                if let Some(parent) = parent {
                    stack[parent].child_nodes.as_mut().unwrap().push(value);
                } else {
                    env.push(value);
                    break;
                }
            } else {
                // This is a branch node
                env.push_all(consts.iter().take(is_leaf.sig.args() - 1).cloned());
                env.push(value.clone());
                env.exec(children.clone())?;
                let children = env.pop("child nodes")?;
                let index = stack.len();
                // println!("{value:?} has children {children:?}");
                stack.push(RecNode {
                    parent,
                    value,
                    child_nodes: Some(Vec::new()),
                    scalar_child: children.rank() == 0,
                });
                for value in children.into_rows() {
                    stack.push(RecNode {
                        parent: Some(index),
                        value,
                        child_nodes: None,
                        scalar_child: false,
                    });
                }
            }
        }
    }

    Ok(())
}
