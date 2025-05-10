use crate::{SigNode, Uiua, UiuaResult, Value};

#[allow(clippy::type_complexity)]
pub fn recur(children: SigNode, branch: SigNode, env: &mut Uiua) -> UiuaResult {
    struct Item {
        val: Value,
        parent: Option<usize>,
        child_results: Option<Vec<Vec<Value>>>,
        child_data: Vec<Value>,
    }

    let outputs = branch.sig.outputs().max(1);
    let children_data_count = children.sig.outputs().saturating_sub(outputs + 1);
    dbg!(outputs, children_data_count);

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
            mut val,
            parent,
            child_results,
            mut child_data,
        } = item;
        if let Some(results) = child_results {
            // Branch
            println!("branch: {:?}, {:?}, {:?}", val, results, child_data);
            for child_result in results.into_iter().rev() {
                for slot_result in child_result {
                    env.push(slot_result);
                }
                env.push(val);
                env.exec(branch.clone())?;
                val = env.pop("accumulated")?;
            }
            if let Some(parent_index) = parent {
                stack[parent_index]
                    .child_results
                    .as_mut()
                    .unwrap()
                    .push(vec![val]);
            } else {
                env.push(val);
            }
        } else {
            // Get children
            println!("get children of {:?}", val);
            for data in child_data.iter().rev() {
                env.push(data.clone());
            }
            env.push(val);
            env.exec(children.clone())?;
            let node = env.pop("parent")?;
            let children = env.pop("children")?;
            println!("{:?} | {:?}", node, children);
            let is_branch = children.row_count() > 0;
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
                // Push node and children
                let parent_index = stack.len();
                stack.push(Item {
                    val: node,
                    parent,
                    child_results: Some(Vec::with_capacity(children.row_count())),
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
                // Leaf
                println!("leaf: {:?}, {:?}", node, child_data);
                if children_data_count == 0 {
                    child_data.clear();
                    child_data.push(node);
                }
                if let Some(parent_index) = parent {
                    stack[parent_index]
                        .child_results
                        .as_mut()
                        .unwrap()
                        .push(child_data);
                } else {
                    for val in child_data {
                        env.push(val);
                    }
                }
            }
        }
    }
    Ok(())
}
