use std::{
    fmt,
    hash::{Hash, Hasher},
    sync::{Arc, OnceLock},
};

use serde::*;

use crate::{Assembly, Node, Primitive, Shape, SigNode, Uiua, UiuaResult, Value};

#[derive(Clone, Serialize, Deserialize)]
pub struct GpuOp {
    pub node: SigNode,
    #[serde(skip)]
    gpu_node: Arc<OnceLock<GpuResult<GpuNode>>>,
    #[serde(skip)]
    pipeline: Arc<OnceLock<GpuResult<GpuPipeline>>>,
}

impl GpuOp {
    pub fn new(node: SigNode, asm: &Assembly) -> GpuResult<Self> {
        let op = GpuOp {
            node,
            gpu_node: Default::default(),
            pipeline: Default::default(),
        };
        op.gpu_node(asm).as_ref().map_err(|e| e.clone())?;
        Ok(op)
    }
    fn gpu_node(&self, asm: &Assembly) -> &GpuResult<GpuNode> {
        self.gpu_node.get_or_init(|| -> GpuResult<GpuNode> {
            let mut env = GpuDeriver::new(asm);
            env.node(&self.node.node)?;
            Ok(env.stack.pop().unwrap_or(GpuNode::Var(0)))
        })
    }
    pub fn exec(&self, env: &mut Uiua) -> UiuaResult {
        let pipeline = self
            .pipeline
            .get_or_init(|| -> GpuResult<GpuPipeline> {
                imp::build_pipeline(self.gpu_node(&env.asm).as_ref().map_err(|e| e.clone())?)
            })
            .as_ref()
            .map_err(|e| env.error(e))?;
        imp::exec(pipeline, env)
    }
}

impl fmt::Debug for GpuOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "gpu({:?})", self.node)
    }
}

impl PartialEq for GpuOp {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}

impl Eq for GpuOp {}

impl Hash for GpuOp {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.node.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq)]
enum GpuNode {
    Const(Shape, Arc<[f32]>),
    Var(usize),
    Mon(GpuMon, Box<Self>),
    Dy(GpuDy, Box<Self>, Box<Self>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GpuMon {
    Neg,
    Not,
    Sqrt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GpuDy {
    Add,
    Mul,
    Sub,
    Div,
    Pow,
}

impl GpuNode {
    fn args(&self) -> usize {
        match self {
            GpuNode::Const(_, _) => 0,
            GpuNode::Var(i) => *i,
            GpuNode::Mon(_, a) => a.args(),
            GpuNode::Dy(_, a, b) => a.args().max(b.args()),
        }
    }
}

struct GpuDeriver<'a> {
    stack: Vec<GpuNode>,
    next_var: usize,
    asm: &'a Assembly,
}

impl<'a> GpuDeriver<'a> {
    fn new(asm: &'a Assembly) -> Self {
        GpuDeriver {
            stack: Vec::new(),
            next_var: 0,
            asm,
        }
    }
    fn pop(&mut self) -> GpuNode {
        self.stack.pop().unwrap_or_else(|| {
            self.next_var += 1;
            GpuNode::Var(self.next_var - 1)
        })
    }
    fn node(&mut self, node: &Node) -> GpuResult {
        match node {
            Node::Run(nodes) => nodes.iter().try_for_each(|node| self.node(node))?,
            Node::Push(val) => match val {
                Value::Num(arr) => {
                    self.stack.push(GpuNode::Const(
                        arr.shape.clone(),
                        arr.data.iter().map(|&n| n as f32).collect(),
                    ));
                }
                Value::Byte(arr) => {
                    self.stack.push(GpuNode::Const(
                        arr.shape.clone(),
                        arr.data.iter().map(|&n| n as f32).collect(),
                    ));
                }
                val => {
                    return Err(GpuError::NotSupported(format!(
                        "{} arrays are",
                        val.type_name()
                    )))
                }
            },
            Node::Call(f, _) => self.node(&self.asm[f])?,
            Node::Prim(prim, _) => match prim {
                Primitive::Neg | Primitive::Not | Primitive::Sqrt => {
                    let a = self.pop();
                    let mon = match prim {
                        Primitive::Neg => GpuMon::Neg,
                        Primitive::Not => GpuMon::Not,
                        Primitive::Sqrt => GpuMon::Sqrt,
                        _ => unreachable!(),
                    };
                    self.stack.push(GpuNode::Mon(mon, a.into()));
                }
                Primitive::Add
                | Primitive::Sub
                | Primitive::Mul
                | Primitive::Div
                | Primitive::Pow => {
                    let a = self.pop();
                    let b = self.pop();
                    let dy = match prim {
                        Primitive::Add => GpuDy::Add,
                        Primitive::Sub => GpuDy::Sub,
                        Primitive::Mul => GpuDy::Mul,
                        Primitive::Div => GpuDy::Div,
                        Primitive::Pow => GpuDy::Pow,
                        _ => unreachable!(),
                    };
                    self.stack.push(GpuNode::Dy(dy, a.into(), b.into()));
                }
                prim => return Err(GpuError::NotSupported(format!("{} is", prim.format()))),
            },
            node => return Err(GpuError::NotSupported(format!("{node:?} is"))),
        }
        Ok(())
    }
}

pub type GpuResult<T = ()> = Result<T, GpuError>;

#[derive(Debug, Clone)]
pub enum GpuError {
    NotEnabled,
    NotSupported(String),
}

impl fmt::Display for GpuError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GpuError::NotSupported(s) => write!(f, "{s} not supported in gpu"),
            GpuError::NotEnabled => write!(f, "GPU support is not enabled"),
        }
    }
}

use imp::GpuPipeline;

#[cfg(not(feature = "gpu"))]
mod imp {
    use super::*;
    pub struct GpuPipeline {}
    pub fn build_pipeline(_: &GpuNode) -> GpuResult<GpuPipeline> {
        Err(GpuError::NotEnabled)
    }
    pub fn exec(_: &GpuPipeline, env: &mut Uiua) -> UiuaResult {
        Err(env.error(GpuError::NotEnabled))
    }
}

#[cfg(feature = "gpu")]
mod imp {
    use super::*;
    pub struct GpuPipeline {
        args: usize,
    }
    pub fn build_pipeline(node: &GpuNode) -> GpuResult<GpuPipeline> {
        Ok(GpuPipeline { args: node.args() })
    }
    pub fn exec(pipeline: &GpuPipeline, env: &mut Uiua) -> UiuaResult {
        let _args = env.pop_n(pipeline.args)?;
        Ok(())
    }
}
