use std::{
    fmt,
    hash::{Hash, Hasher},
    sync::{Arc, OnceLock},
};

use serde::*;

use crate::{Assembly, Node, Primitive, Shape, SigNode, Uiua, UiuaResult, Value};

const DEBUG: bool = true;

macro_rules! dbgln {
    ($($arg:tt)*) => {
        if DEBUG {
            println!($($arg)*); // Allow println
        }
    }
}

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
            Ok(env.stack.pop().unwrap_or(GpuNode::NoOp))
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
    NoOp,
    Const(usize, Shape, Arc<[f32]>),
    Var(usize, usize),
    Mon(usize, GpuMon, Box<Self>),
    Dy(usize, GpuDy, Box<Self>, Box<Self>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum GpuMon {
    Neg,
    Not,
    Sqrt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum GpuDy {
    Add,
    Mul,
    Sub,
    Div,
    Pow,
}

impl GpuNode {
    fn max_args(&self) -> usize {
        match self {
            GpuNode::NoOp => 0,
            GpuNode::Const(..) => 0,
            GpuNode::Var(_, i) => *i,
            GpuNode::Mon(_, _, a) => a.max_args(),
            GpuNode::Dy(_, _, a, b) => a.max_args().max(b.max_args()),
        }
    }
}

struct GpuDeriver<'a> {
    stack: Vec<GpuNode>,
    next_var: usize,
    next_buffer: usize,
    asm: &'a Assembly,
}

impl<'a> GpuDeriver<'a> {
    fn new(asm: &'a Assembly) -> Self {
        GpuDeriver {
            stack: Vec::new(),
            next_var: 0,
            next_buffer: 0,
            asm,
        }
    }
    fn pop(&mut self) -> GpuNode {
        self.stack.pop().unwrap_or_else(|| {
            self.next_var += 1;
            let index = self.next_buffer;
            self.next_buffer += 1;
            GpuNode::Var(index, self.next_var - 1)
        })
    }
    fn node(&mut self, node: &Node) -> GpuResult {
        match node {
            Node::Run(nodes) => nodes.iter().try_for_each(|node| self.node(node))?,
            Node::Push(val) => match val {
                Value::Num(arr) => {
                    let index = self.next_buffer;
                    self.next_buffer += 1;
                    self.stack.push(GpuNode::Const(
                        index,
                        arr.shape.clone(),
                        arr.data.iter().map(|&n| n as f32).collect(),
                    ));
                }
                Value::Byte(arr) => {
                    let index = self.next_buffer;
                    self.next_buffer += 1;
                    self.stack.push(GpuNode::Const(
                        index,
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
                    let result = self.next_buffer;
                    self.next_buffer += 1;
                    self.stack.push(GpuNode::Mon(result, mon, a.into()));
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
                    let result = self.next_buffer;
                    self.next_buffer += 1;
                    self.stack.push(GpuNode::Dy(result, dy, a.into(), b.into()));
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
    GpuConnection,
}

impl fmt::Display for GpuError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GpuError::NotSupported(s) => write!(f, "{s} not supported in gpu"),
            GpuError::NotEnabled => write!(f, "GPU support is not enabled"),
            GpuError::GpuConnection => write!(f, "Unable to connect to GPU"),
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

    use std::collections::{BTreeMap, HashMap};

    use crate::Array;

    use super::*;
    use ecow::EcoVec;
    use wgpu::{
        util::{BufferInitDescriptor, DeviceExt},
        *,
    };
    pub struct GpuPipeline {
        arg_count: usize,
        return_constants: Option<Vec<Array<f64>>>,
        constants: Vec<(Buffer, Buffer)>,
    }
    pub fn build_pipeline(node: &GpuNode) -> GpuResult<GpuPipeline> {
        let arg_count = node.max_args();
        let (device, _) = imp::device_queue().ok_or(GpuError::GpuConnection)?;

        let mut pipeline = GpuPipeline {
            arg_count,
            constants: Vec::new(),
            return_constants: None,
        };

        match node {
            GpuNode::Const(_, shape, data) => {
                let arr = Array::<f64>::new(
                    shape.clone(),
                    data.iter().map(|&n| n as f64).collect::<EcoVec<_>>(),
                );
                pipeline.return_constants = Some(vec![arr]);
                return Ok(pipeline);
            }
            GpuNode::NoOp => {
                pipeline.return_constants = Some(Vec::new());
                return Ok(pipeline);
            }
            _ => (),
        }

        #[derive(Default)]
        struct Cache {
            mon: HashMap<GpuMon, ShaderModule>,
            dy: HashMap<GpuDy, ShaderModule>,
            consts: BTreeMap<usize, (Buffer, Buffer)>,
            layouts: BTreeMap<usize, BindGroupLayout>,
        }

        fn recur(device: &Device, node: &GpuNode, cache: &mut Cache) -> usize {
            match node {
                GpuNode::Const(i, shape, data) => {
                    let shape_buf = device.create_buffer_init(&BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::cast_slice(shape.as_ref()),
                        usage: BufferUsages::STORAGE,
                    });
                    let data_buf = device.create_buffer_init(&BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::cast_slice(data.as_ref()),
                        usage: BufferUsages::STORAGE,
                    });
                    cache.consts.insert(*i, (shape_buf, data_buf));
                    *i
                }
                GpuNode::Mon(i, op, a) => {
                    let a = recur(device, a, cache);
                    cache.mon.entry(*op).or_insert_with(|| {
                        let src = match op {
                            GpuMon::Neg => include_str!("neg.wgsl"),
                            _ => todo!(),
                        };
                        device.create_shader_module(ShaderModuleDescriptor {
                            label: None,
                            source: wgpu::ShaderSource::Wgsl(src.into()),
                        })
                    });
                    let mut entries = Vec::new();
                    for binding in [a, *i] {
                        entries.push(BindGroupLayoutEntry {
                            binding,
                            visibility: ShaderStages::COMPUTE,
                            ty: BindingType::Buffer {
                                ty: BufferBindingType::Storage { read_only: false },
                                has_dynamic_offset: false,
                                min_binding_size: None,
                            },
                            count: None,
                        });
                    }
                    let layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
                        label: None,
                        entries: &entries,
                    });
                    cache.layouts.insert(*i, layout);
                    *i
                }
                GpuNode::Dy(i, op, a, b) => {
                    recur(device, a, cache);
                    recur(device, b, cache);
                    cache.dy.entry(*op).or_insert_with(|| {
                        let src = match op {
                            GpuDy::Add => include_str!("add.wgsl"),
                            GpuDy::Mul => include_str!("mul.wgsl"),
                            _ => todo!(),
                        };
                        device.create_shader_module(ShaderModuleDescriptor {
                            label: None,
                            source: wgpu::ShaderSource::Wgsl(src.into()),
                        })
                    });
                    *i
                }
                GpuNode::Var(i, _) => *i,
                GpuNode::NoOp => unreachable!("NoOp leaf node"),
            }
        }

        let mut cache = Cache::default();
        recur(device, node, &mut cache);

        pipeline.constants = cache.consts.into_values().collect();

        Ok(pipeline)
    }
    pub fn exec(pipeline: &GpuPipeline, env: &mut Uiua) -> UiuaResult {
        if let Some(return_constants) = &pipeline.return_constants {
            for arr in return_constants.iter().rev() {
                env.push(arr.clone());
            }
            return Ok(());
        }

        let _args = env.pop_n(pipeline.arg_count)?;
        let (_, queue) = imp::device_queue().ok_or_else(|| env.error(GpuError::GpuConnection))?;

        Ok(())
    }

    fn device_queue() -> Option<(&'static Device, &'static Queue)> {
        static DEVICE_QUEUE: OnceLock<Option<(Device, Queue)>> = OnceLock::new();
        DEVICE_QUEUE
            .get_or_init(|| {
                pollster::block_on(async {
                    let instance = Instance::new(InstanceDescriptor::default());
                    let adapter = instance
                        .request_adapter(&RequestAdapterOptions::default())
                        .await?;
                    dbgln!("adapter: {:?}", adapter.get_info());
                    adapter
                        .request_device(&DeviceDescriptor::default(), None)
                        .await
                        .ok()
                })
            })
            .as_ref()
            .map(|(d, q)| (d, q))
    }
}
