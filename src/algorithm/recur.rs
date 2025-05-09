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

    let start = env.pop("start")?;
    let mut stack: Vec<(Value, Option<usize>, Option<Vec<Vec<Value>>>)> = vec![(start, None, None)];
    let outputs = branch.sig.outputs().max(1);
    let children_extra = children.sig.args().saturating_sub(branch.sig.args().max(1));
    let branch_takes_val = branch.sig.args().saturating_sub(outputs) > 0;
    let leaf_takes_val = !(leaf.sig.args() == 0 && leaf.sig.outputs() > 0);
    // dbg!(outputs, branch_takes_val, leaf_takes_val);
    while let Some((val, parent, child_results)) = stack.pop() {
        if let Some(results) = child_results {
            // Branch
            for results in results.into_iter().rev() {
                // for results in results.into_iter().rev() {
                //     let mut rows = results.into_iter();
                //     let child_results = if let Some(mut joined) = rows.next() {
                //         for result in rows {
                //             joined = joined.join(result, false, env)?;
                //         }
                //         joined
                //     } else {
                //         Value::default()
                //     };
                //     env.push(child_results);
                // }
                let child_results = Value::from_row_values(results, env)?;
                env.push(child_results);
            }
            if branch_takes_val {
                env.push(val);
            }
            env.exec(branch.clone())?;
            if let Some(parent_index) = parent {
                for i in 0..outputs {
                    let result = env.pop("accumulated")?;
                    stack[parent_index].2.as_mut().unwrap()[i].push(result);
                }
            }
        } else {
            // Get children
            let extra = env.copy_n(children_extra)?;
            env.push(val.clone());
            env.exec(children.clone())?;
            let children = env.pop("children")?;
            env.push_all(extra);

            let is_branch = children.row_count() > 0 && !(val.rank() == 0 && val == children);

            if is_branch {
                // Push self and children
                let parent_index = stack.len();

                stack.push((
                    val,
                    parent,
                    Some(vec![Vec::with_capacity(children.row_count()); outputs]),
                ));
                for child in children.into_rows().rev() {
                    stack.push((child, Some(parent_index), None));
                }
            } else {
                // Leaf
                if leaf_takes_val {
                    env.push(val);
                }
                env.exec(leaf.clone())?;
                if let Some(parent_index) = parent {
                    for i in 0..outputs {
                        let result = env.pop("accumulated")?;
                        stack[parent_index].2.as_mut().unwrap()[i].push(result);
                    }
                }
            }
        }
    }
    Ok(())
}
