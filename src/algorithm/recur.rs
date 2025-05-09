use crate::{SigNode, Uiua, UiuaResult, Value};

#[allow(clippy::type_complexity)]
pub fn recur(children: SigNode, branch: SigNode, leaf: SigNode, env: &mut Uiua) -> UiuaResult {
    if branch.sig.outputs() != leaf.sig.outputs()
        && !branch.sig.is_compatible_with(leaf.sig)
        && leaf.sig != (0, 0)
    {
        return Err(env.error(format!(
            "Branch and leaf functions must have the same number of outputs, \
            but their signatures are {} and {}",
            branch.sig, leaf.sig
        )));
    }

    struct Item {
        val: Value,
        parent: Option<usize>,
        child_results: Option<Vec<Vec<Value>>>,
        child_data: Vec<Value>,
    }

    let outputs = branch.sig.outputs().max(1);
    let children_data_count = children.sig.outputs().saturating_sub(outputs);
    let branch_takes_val = branch.sig.args().saturating_sub(outputs) > 0;
    let leaf_takes_val = !(leaf.sig.args() == 0 && leaf.sig.outputs() > 0);
    // dbg!(
    //     outputs,
    //     children_data_count,
    //     branch_takes_val,
    //     leaf_takes_val
    // );

    let start = env.pop("start")?;
    let mut child_data = Vec::with_capacity(children_data_count);
    for i in 0..children_data_count {
        let value = env.pop("child")?;
        if value.row_count() != start.row_count() {
            return Err(env.error(format!(
                "Additional child data {} must have the same \
                number of rows as the primary child identifiers, \
                but their shapes are {} and {}",
                i + 1,
                start.shape,
                value.shape
            )));
        }
        child_data.push(value);
    }
    let mut stack: Vec<Item> = vec![Item {
        val: start,
        parent: None,
        child_results: None,
        child_data,
    }];
    while let Some(item) = stack.pop() {
        let Item {
            val,
            parent,
            child_results,
            child_data,
        } = item;
        if let Some(results) = child_results {
            // println!("branch: {:?}, {:?}", val, child_data);
            // Branch
            for results in results.into_iter().rev() {
                let child_results = Value::from_row_values(results, env)?;
                env.push(child_results);
            }
            if branch_takes_val {
                if children_data_count == 0 {
                    env.push(val);
                } else {
                    for data in child_data.into_iter().rev() {
                        env.push(data);
                    }
                }
            }
            env.exec(branch.clone())?;
            if let Some(parent_index) = parent {
                for i in 0..outputs {
                    let result = env.pop("accumulated")?;
                    stack[parent_index].child_results.as_mut().unwrap()[i].push(result);
                }
            }
        } else {
            // Get children
            for data in child_data.iter().rev() {
                env.push(data.clone());
            }
            env.push(val.clone());
            env.exec(children.clone())?;
            let children = env.pop("children")?;
            // println!("children of {:?}: {:?}", val, children);
            let is_branch = children.row_count() > 0 && !(val.rank() == 0 && val == children);
            let mut child_data_iters =
                Vec::with_capacity(if is_branch { children_data_count } else { 0 });
            for i in 0..children_data_count {
                let value = env.pop("child")?;
                if value.row_count() != children.row_count() {
                    return Err(env.error(format!(
                        "Additional child data {} must have the same \
                        number of rows as the primary child identifiers, \
                        but their shapes are {} and {}",
                        i + 1,
                        children.shape,
                        value.shape
                    )));
                }
                child_data_iters.push(value.into_rows().rev());
            }

            if is_branch {
                // Push self and children
                let parent_index = stack.len();
                stack.push(Item {
                    val,
                    parent,
                    child_results: Some(vec![Vec::with_capacity(children.row_count()); outputs]),
                    child_data: child_data.clone(),
                });
                for child in children.into_rows().rev() {
                    let child_data = child_data_iters
                        .iter_mut()
                        .map(|iter| iter.next().unwrap())
                        .collect();
                    stack.push(Item {
                        val: child,
                        parent: Some(parent_index),
                        child_results: None,
                        child_data,
                    });
                }
            } else {
                // println!("leaf: {:?}, {:?}", val, child_data);
                // Leaf
                if leaf_takes_val {
                    if children_data_count == 0 {
                        env.push(val);
                    } else {
                        for data in child_data.into_iter().rev() {
                            env.push(data);
                        }
                    }
                }
                env.exec(leaf.clone())?;
                if let Some(parent_index) = parent {
                    for i in 0..outputs {
                        let result = env.pop("accumulated")?;
                        stack[parent_index].child_results.as_mut().unwrap()[i].push(result);
                    }
                }
            }
        }
    }
    Ok(())
}
