use std::{cmp::Ordering, collections::*, mem::take};

use crate::{Array, ArrayCmp, Boxed, Primitive, SigNode, Signature, Uiua, UiuaResult, Value};

pub fn path(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    env: &mut Uiua,
) -> UiuaResult {
    path_impl(neighbors, is_goal, heuristic, PathMode::All, env)
}

pub fn path_first(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    env: &mut Uiua,
) -> UiuaResult {
    path_impl(neighbors, is_goal, heuristic, PathMode::First, env)
}

pub fn path_take(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    env: &mut Uiua,
) -> UiuaResult {
    let n = env
        .pop("number of paths to take")?
        .as_ints_or_infs(env, "Taken amount must be a list of integers or infinity")?;
    if n.is_empty() {
        path_impl(neighbors, is_goal, heuristic, PathMode::All, env)?;
    } else {
        match n.first() {
            Some(Ok(n)) if *n >= 0 => path_impl(
                neighbors,
                is_goal,
                heuristic,
                PathMode::Take(*n as usize),
                env,
            )?,
            _ => path_impl(neighbors, is_goal, heuristic, PathMode::All, env)?,
        }
    }
    let path = env.pop("path")?.take_impl(&n, env)?;
    env.push(path);
    Ok(())
}

pub fn path_pop(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    env: &mut Uiua,
) -> UiuaResult {
    path_impl(neighbors, is_goal, heuristic, PathMode::CostOnly, env)
}

#[derive(Debug, Clone, Copy)]
enum PathMode {
    All,
    First,
    CostOnly,
    Take(usize),
}

fn path_impl(
    neighbors: SigNode,
    is_goal: SigNode,
    heuristic: Option<SigNode>,
    mode: PathMode,
    env: &mut Uiua,
) -> UiuaResult {
    let start = env.pop("start")?;
    let nei_sig = neighbors.sig;
    let heu_sig = heuristic
        .as_ref()
        .map(|h| h.sig)
        .unwrap_or_else(|| Signature::new(0, 1));
    let isg_sig = is_goal.sig;
    for (name, sig, req_out) in &[
        ("neighbors", nei_sig, [1, 2].as_slice()),
        ("goal", isg_sig, &[1]),
        ("heuristic", heu_sig, &[1]),
    ] {
        if !req_out.contains(&sig.outputs) {
            let count = if req_out.len() == 1 {
                "1"
            } else {
                "either 1 or 2"
            };
            return Err(env.error(format!(
                "{} {name} function must return {count} outputs \
                but its signature is {sig}",
                Primitive::Path.format()
            )));
        }
    }
    let has_costs = nei_sig.outputs == 2;
    let arg_count = nei_sig.args.max(heu_sig.args).max(isg_sig.args) - 1;
    let mut args = Vec::with_capacity(arg_count);
    for i in 0..arg_count {
        args.push(env.pop(i + 1)?);
    }

    struct NodeCost {
        node: usize,
        cost: f64,
    }
    impl PartialEq for NodeCost {
        fn eq(&self, other: &Self) -> bool {
            self.cost.array_eq(&other.cost)
        }
    }
    impl Eq for NodeCost {}
    impl PartialOrd for NodeCost {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }
    impl Ord for NodeCost {
        fn cmp(&self, other: &Self) -> Ordering {
            self.cost.array_cmp(&other.cost).reverse()
        }
    }

    struct PathEnv<'a> {
        env: &'a mut Uiua,
        neighbors: SigNode,
        is_goal: SigNode,
        heuristic: Option<SigNode>,
        args: Vec<Value>,
    }

    impl PathEnv<'_> {
        fn heuristic(&mut self, node: &Value) -> UiuaResult<f64> {
            Ok(if let Some(heuristic) = &self.heuristic {
                let heu_args = heuristic.sig.args;
                for arg in (self.args.iter()).take(heu_args.saturating_sub(1)).rev() {
                    self.env.push(arg.clone());
                }
                if heu_args > 0 {
                    self.env.push(node.clone());
                }
                self.env.exec(heuristic.clone())?;
                let h = (self.env)
                    .pop("heuristic")?
                    .as_num(self.env, "Heuristic must be a number")?;
                if h < 0.0 {
                    return Err(self
                        .env
                        .error("Negative heuristic values are not allowed in A*"));
                }
                h
            } else {
                0.0
            })
        }
        fn neighbors(&mut self, node: &Value) -> UiuaResult<Vec<(Value, f64)>> {
            let nei_args = self.neighbors.sig.args;
            for arg in (self.args.iter()).take(nei_args.saturating_sub(1)).rev() {
                self.env.push(arg.clone());
            }
            if nei_args > 0 {
                self.env.push(node.clone());
            }
            self.env.exec(self.neighbors.clone())?;
            let (nodes, costs) = if self.neighbors.sig.outputs == 2 {
                let costs = (self.env.pop("neighbors costs")?)
                    .as_nums(self.env, "Costs must be a list of numbers")?;
                let nodes = self.env.pop("neighbors nodes")?;
                if costs.len() != nodes.row_count() {
                    return Err(self.env.error(format!(
                        "Number of nodes {} does not match number of costs {}",
                        nodes.row_count(),
                        costs.len(),
                    )));
                }
                if costs.iter().any(|&c| c < 0.0) {
                    return Err(self.env.error("Negative costs are not allowed in A*"));
                }
                (nodes, costs)
            } else {
                let nodes = self.env.pop("neighbors nodes")?;
                let costs = vec![1.0; nodes.row_count()];
                (nodes, costs)
            };
            Ok(nodes.into_rows().zip(costs).collect())
        }
        fn is_goal(&mut self, node: &Value) -> UiuaResult<bool> {
            let isg_args = self.is_goal.sig.args;
            for arg in (self.args.iter()).take(isg_args.saturating_sub(1)).rev() {
                self.env.push(arg.clone());
            }
            if isg_args > 0 {
                self.env.push(node.clone());
            }
            self.env.exec(self.is_goal.clone())?;
            let is_goal = (self.env.pop("is_goal")?)
                .as_bool(self.env, "path goal function must return a boolean")?;
            Ok(is_goal)
        }
    }

    let mut if_empty = start.clone();
    if_empty.fix();
    if_empty = if_empty.first_dim_zero();
    if_empty.shape_mut().insert(0, 0);

    // Initialize state
    let mut to_see = BinaryHeap::new();
    let mut backing = vec![start.clone()];
    let mut indices: HashMap<Value, usize> = [(start, 0)].into();
    to_see.push(NodeCost { node: 0, cost: 0.0 });

    let mut came_from: HashMap<usize, BTreeSet<usize>> = HashMap::new();
    let mut full_cost: HashMap<usize, f64> = [(0, 0.0)].into();

    let mut shortest_cost = f64::INFINITY;
    let mut ends = BTreeSet::new();

    fn count_paths(ends: &BTreeSet<usize>, came_from: &HashMap<usize, BTreeSet<usize>>) -> usize {
        let mut queue = VecDeque::new();
        let mut count = 0;
        for &end in ends {
            queue.clear();
            queue.push_back(end);
            while let Some(curr) = queue.pop_front() {
                if let Some(parents) = came_from.get(&curr) {
                    for &parent in parents {
                        queue.push_back(parent);
                    }
                } else {
                    count += 1;
                }
            }
        }
        count
    }

    // Main pathing loop
    env.without_fill(|env| -> UiuaResult {
        let mut env = PathEnv {
            env,
            neighbors,
            heuristic,
            is_goal,
            args,
        };

        'outer: while let Some(NodeCost { node: curr, .. }) = to_see.pop() {
            env.env.respect_execution_limit()?;
            let curr_cost = full_cost[&curr];
            // Early exit if found a shorter path
            if curr_cost > shortest_cost || ends.contains(&curr) {
                continue;
            }
            // Check if reached a goal
            if env.is_goal(&backing[curr])? {
                ends.insert(curr);
                shortest_cost = curr_cost;
                match mode {
                    PathMode::All => continue,
                    PathMode::Take(n) if n <= 1 => break,
                    PathMode::Take(n) if count_paths(&ends, &came_from) >= n => break,
                    _ => break,
                }
            }
            // Check neighbors
            for (nei, nei_cost) in env.neighbors(&backing[curr])? {
                // Add to backing if needed
                let nei = if let Some(index) = indices.get(&nei) {
                    *index
                } else {
                    let index = backing.len();
                    indices.insert(nei.clone(), index);
                    backing.push(nei);
                    index
                };
                let from_curr_nei_cost = curr_cost + nei_cost;
                let curr_nei_cost = full_cost.get(&nei).copied().unwrap_or(f64::INFINITY);
                if from_curr_nei_cost <= curr_nei_cost {
                    if let PathMode::Take(n) = mode {
                        if ends.contains(&nei) && count_paths(&ends, &came_from) >= n {
                            break 'outer;
                        }
                    }
                    let parents = came_from.entry(nei).or_default();
                    // If a better path was found we...
                    if from_curr_nei_cost < curr_nei_cost {
                        // 1. Clear the parents
                        parents.clear();
                        // 2. Update the known cost
                        full_cost.insert(nei, from_curr_nei_cost);
                        // 3. Add to to see
                        to_see.push(NodeCost {
                            cost: from_curr_nei_cost + env.heuristic(&backing[nei])?,
                            node: nei,
                        });
                    }
                    parents.insert(curr);
                }
            }
        }
        Ok(())
    })?;

    if has_costs {
        env.push(shortest_cost);
    }

    let make_path = |path: Vec<usize>| {
        if let Some(&[a, b]) = path
            .windows(2)
            .find(|w| backing[w[0]].shape() != backing[w[1]].shape())
        {
            return Err(env.error(format!(
                "Cannot make path from nodes with incompatible shapes {} and {}",
                backing[a].shape(),
                backing[b].shape()
            )));
        }
        let path: Vec<_> = path.into_iter().map(|i| backing[i].clone()).collect();
        Value::from_row_values(path, env)
    };

    match mode {
        PathMode::All | PathMode::Take(_) => {
            let mut paths = Vec::new();
            'outer: for end in ends {
                let mut currs = vec![vec![end]];
                let mut these_paths = Vec::new();
                while !currs.is_empty() {
                    env.respect_execution_limit()?;
                    let mut new_paths = Vec::new();
                    currs.retain_mut(|path| {
                        let parents = came_from.get(path.last().unwrap());
                        match parents.map(|p| p.len()).unwrap_or(0) {
                            0 => {
                                these_paths.push(take(path));
                                false
                            }
                            1 => {
                                path.push(*parents.unwrap().iter().next().unwrap());
                                true
                            }
                            _ => {
                                for &parent in parents.unwrap().iter().skip(1) {
                                    let mut path = path.clone();
                                    path.push(parent);
                                    new_paths.push(path);
                                }
                                path.push(*parents.unwrap().iter().next().unwrap());
                                true
                            }
                        }
                    });
                    currs.extend(new_paths);
                }
                for mut path in these_paths {
                    path.reverse();
                    let path_val = make_path(path)?;
                    paths.push(if has_costs {
                        Boxed(path_val).into()
                    } else {
                        path_val
                    });
                    if let PathMode::Take(n) = mode {
                        if paths.len() >= n {
                            break 'outer;
                        }
                    }
                }
            }
            let path_count = paths.len();
            let mut paths_val = Value::from_row_values(paths, env)?;
            if path_count == 0 {
                paths_val = if has_costs {
                    Array::<Boxed>::default().into()
                } else {
                    if_empty
                }
            } else if let PathMode::Take(0) = mode {
                if paths_val.row_count() > 0 {
                    paths_val.drop_n(1);
                }
            }
            env.push(paths_val);
        }
        PathMode::First => {
            if let Some(mut curr) = ends.into_iter().next() {
                let mut path = vec![curr];
                while let Some(from) = came_from.get(&curr) {
                    let from = *from.iter().next().unwrap();
                    path.push(from);
                    curr = from;
                }
                path.reverse();
                let path_val = make_path(path)?;
                env.push(if has_costs {
                    Boxed(path_val).into()
                } else {
                    path_val
                });
            } else if let Some(val) = env.value_fill().cloned() {
                env.push(val);
            } else {
                return Err(env.error("No path found"));
            }
        }
        PathMode::CostOnly => {}
    }
    Ok(())
}
