use std::{
    array, fmt,
    hash::{Hash, Hasher},
    sync::{Arc, OnceLock},
};

use serde::*;

use crate::{
    invert::InversionError, Assembly, Node, Primitive, Shape, SigNode, Uiua, UiuaResult, Value,
};

macro_rules! dbgln {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
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
            let gpu_node = env.stack.pop().unwrap_or(GpuNode::NoOp);
            dbgln!("gpu_node: {gpu_node:?}");
            Ok(gpu_node)
        })
    }
    pub fn init_pipeline(&self, asm: &Assembly) {
        _ = self.pipeline(asm);
    }
    fn pipeline(&self, asm: &Assembly) -> &GpuResult<GpuPipeline> {
        self.pipeline.get_or_init(|| -> GpuResult<GpuPipeline> {
            imp::build_pipeline(self.gpu_node(asm).as_ref().map_err(|e| e.clone())?)
        })
    }
    pub fn exec(&self, env: &mut Uiua) -> UiuaResult {
        let pipeline = self.pipeline(&env.asm).as_ref().map_err(|e| env.error(e))?;
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
    Const(u32, Shape, Arc<[f32]>),
    Var(u32, usize),
    Mon(u32, GpuMon, Box<Self>),
    Dy(u32, GpuDy, Box<Self>, Box<Self>),
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
            GpuNode::Var(_, i) => *i + 1,
            GpuNode::Mon(_, _, a) => a.max_args(),
            GpuNode::Dy(_, _, a, b) => a.max_args().max(b.max_args()),
        }
    }
}

struct GpuDeriver<'a> {
    stack: Vec<GpuNode>,
    next_var: usize,
    next_buffer: u32,
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
                Primitive::Dup => {
                    let a = self.pop();
                    self.stack.push(a.clone());
                    self.stack.push(a);
                }
                Primitive::Flip => {
                    let a = self.pop();
                    let b = self.pop();
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Primitive::Over => {
                    let a = self.pop();
                    let b = self.pop();
                    self.stack.push(b.clone());
                    self.stack.push(a.clone());
                    self.stack.push(b);
                }
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
            Node::Mod(prim, args, _) => match prim {
                Primitive::Dip => {
                    let [f] = get_args(args);
                    let a = self.pop();
                    self.node(&f.node)?;
                    self.stack.push(a);
                }
                Primitive::On => {
                    let [f] = get_args(args);
                    let a = self.pop();
                    self.stack.push(a.clone());
                    self.node(&f.node)?;
                    self.stack.push(a);
                }
                prim => return Err(GpuError::NotSupported(format!("{} is", prim.format()))),
            },
            Node::CustomInverse(cust, _) => match &cust.normal {
                Ok(normal) => self.node(&normal.node)?,
                Err(e) => return Err(GpuError::Inversion(e.clone())),
            },
            node => return Err(GpuError::NotSupported(format!("{node:?} is"))),
        }
        Ok(())
    }
}

fn get_args<const N: usize>(args: &[SigNode]) -> [&SigNode; N] {
    array::from_fn(|i| &args[i])
}

pub type GpuResult<T = ()> = Result<T, GpuError>;

#[derive(Debug, Clone)]
pub enum GpuError {
    NotEnabled,
    NotSupported(String),
    GpuConnection,
    Inversion(InversionError),
}

impl fmt::Display for GpuError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GpuError::NotSupported(s) => write!(f, "{s} not supported in gpu"),
            GpuError::NotEnabled => write!(f, "GPU support is not enabled"),
            GpuError::GpuConnection => write!(f, "Unable to connect to GPU"),
            GpuError::Inversion(e) => write!(f, "{e}"),
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
    use std::{
        collections::{BTreeMap, HashMap},
        mem::take,
        time::Instant,
    };

    use crate::{algorithm::pervade, Array};

    use super::*;
    use ecow::{eco_vec, EcoVec};
    use futures_intrusive::channel::shared::oneshot_channel;
    use wgpu::{
        util::{BufferInitDescriptor, DeviceExt},
        *,
    };

    #[derive(Default)]
    pub struct GpuPipeline {
        arg_count: usize,
        return_constants: Option<Vec<Array<f64>>>,
        mon: HashMap<GpuMon, (ShaderModule, BindGroupLayout, PipelineLayout)>,
        dy: HashMap<GpuDy, (ShaderModule, BindGroupLayout, PipelineLayout)>,
        consts: HashMap<u32, (ArrayBuffers, Shape)>,
        input_bindings: HashMap<usize, u32>,
        result_bindings: BTreeMap<u32, ResultShape>,
        output_buffer: u32,
        instrs: Vec<GpuInstr>,
    }

    #[derive(Debug)]
    enum ResultShape {
        Mon(u32),
        Dy(u32, u32),
    }

    struct ArrayBuffers {
        rank: Buffer,
        shape: Buffer,
        data: Buffer,
    }

    impl ArrayBuffers {
        fn rank_entry(&self, n: u32, m: u32) -> BindGroupEntry {
            BindGroupEntry {
                binding: n * m,
                resource: self.rank.as_entire_binding(),
            }
        }
        fn shape_entry(&self, n: u32, m: u32) -> BindGroupEntry {
            BindGroupEntry {
                binding: n * m + 1,
                resource: self.shape.as_entire_binding(),
            }
        }
        fn data_entry(&self, n: u32, m: u32) -> BindGroupEntry {
            BindGroupEntry {
                binding: n * m + 2,
                resource: self.data.as_entire_binding(),
            }
        }
    }

    #[derive(Debug)]
    enum GpuInstr {
        Mon(GpuMon, u32, u32),
        Dy { dy: GpuDy, a: u32, b: u32, c: u32 },
    }

    pub fn build_pipeline(node: &GpuNode) -> GpuResult<GpuPipeline> {
        let arg_count = node.max_args();
        let (device, _) = imp::device_queue().ok_or(GpuError::GpuConnection)?;

        let mut pipeline = GpuPipeline {
            arg_count,
            ..Default::default()
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

        fn recur(device: &Device, node: &GpuNode, pl: &mut GpuPipeline) -> u32 {
            fn bgl_entry(binding: u32, dy: bool) -> BindGroupLayoutEntry {
                BindGroupLayoutEntry {
                    binding,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: if dy {
                            if binding % 5 == 0 || binding % 5 == 3 || binding % 5 == 4 {
                                BufferBindingType::Uniform
                            } else {
                                BufferBindingType::Storage { read_only: false }
                            }
                        } else if binding % 3 == 0 {
                            BufferBindingType::Uniform
                        } else {
                            BufferBindingType::Storage { read_only: false }
                        },
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                }
            }

            match node {
                GpuNode::Const(i, sh, data) => {
                    let rank = device.create_buffer_init(&BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::bytes_of::<u32>(&(sh.len() as u32)),
                        usage: BufferUsages::UNIFORM,
                    });
                    let ushape: Vec<u32> = sh.iter().map(|&n| n as u32).collect();
                    let shape = device.create_buffer_init(&BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::cast_slice(if ushape.is_empty() {
                            &[1]
                        } else {
                            &ushape
                        }),
                        usage: BufferUsages::STORAGE,
                    });
                    let data = device.create_buffer_init(&BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::cast_slice(data.as_ref()),
                        usage: BufferUsages::STORAGE,
                    });
                    pl.consts
                        .insert(*i, (ArrayBuffers { rank, shape, data }, sh.clone()));
                    *i
                }
                GpuNode::Mon(i, op, a) => {
                    let a = recur(device, a, pl);
                    pl.mon.entry(*op).or_insert_with(|| {
                        let src = match op {
                            GpuMon::Neg => include_str!("neg.wgsl"),
                            _ => todo!(),
                        };
                        let label = format!("{op:?}");
                        let shader_module = device.create_shader_module(ShaderModuleDescriptor {
                            label: Some(&label),
                            source: wgpu::ShaderSource::Wgsl(src.into()),
                        });
                        let bg_layout =
                            device.create_bind_group_layout(&BindGroupLayoutDescriptor {
                                label: Some(&label),
                                entries: &[
                                    bgl_entry(0, false),
                                    bgl_entry(1, false),
                                    bgl_entry(2, false),
                                    bgl_entry(3, false),
                                    bgl_entry(4, false),
                                    bgl_entry(5, false),
                                ],
                            });
                        let pl_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
                            label: Some(&label),
                            bind_group_layouts: &[&bg_layout],
                            push_constant_ranges: &[],
                        });
                        (shader_module, bg_layout, pl_layout)
                    });
                    pl.instrs.push(GpuInstr::Mon(*op, a, *i));
                    pl.result_bindings.insert(*i, ResultShape::Mon(a));
                    *i
                }
                GpuNode::Dy(i, op, a, b) => {
                    let a = recur(device, a, pl);
                    let b = recur(device, b, pl);
                    pl.dy.entry(*op).or_insert_with(|| {
                        let src = match op {
                            GpuDy::Add => include_str!("add.wgsl"),
                            GpuDy::Sub => include_str!("sub.wgsl"),
                            GpuDy::Mul => include_str!("mul.wgsl"),
                            _ => todo!(),
                        };
                        let label = format!("{op:?}");
                        let shader_module = device.create_shader_module(ShaderModuleDescriptor {
                            label: Some(&label),
                            source: wgpu::ShaderSource::Wgsl(src.into()),
                        });
                        let bg_layout =
                            device.create_bind_group_layout(&BindGroupLayoutDescriptor {
                                label: Some(&label),
                                entries: &[
                                    // a
                                    bgl_entry(0, true),
                                    bgl_entry(1, true),
                                    bgl_entry(2, true),
                                    bgl_entry(3, true),
                                    bgl_entry(4, true),
                                    // b
                                    bgl_entry(5, true),
                                    bgl_entry(6, true),
                                    bgl_entry(7, true),
                                    bgl_entry(8, true),
                                    bgl_entry(9, true),
                                    // c
                                    bgl_entry(10, true),
                                    bgl_entry(11, true),
                                    bgl_entry(12, true),
                                ],
                            });
                        let pl_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
                            label: Some(&label),
                            bind_group_layouts: &[&bg_layout],
                            push_constant_ranges: &[],
                        });
                        (shader_module, bg_layout, pl_layout)
                    });
                    pl.instrs.push(GpuInstr::Dy {
                        dy: *op,
                        a,
                        b,
                        c: *i,
                    });
                    pl.result_bindings.insert(*i, ResultShape::Dy(a, b));
                    *i
                }
                GpuNode::Var(b, i) => {
                    pl.input_bindings.insert(*i, *b);
                    *b
                }
                GpuNode::NoOp => unreachable!("NoOp leaf node"),
            }
        }

        pipeline.output_buffer = recur(device, node, &mut pipeline);

        Ok(pipeline)
    }
    pub fn exec(pipeline: &GpuPipeline, env: &mut Uiua) -> UiuaResult {
        pollster::block_on(exec_impl(pipeline, env))
    }
    async fn exec_impl(pipeline: &GpuPipeline, env: &mut Uiua) -> UiuaResult {
        let func_start = Instant::now();

        // Early return for constants only
        if let Some(return_constants) = &pipeline.return_constants {
            for arr in return_constants.iter().rev() {
                env.push(arr.clone());
            }
            return Ok(());
        }

        // Get device and queue
        let (device, queue) =
            imp::device_queue().ok_or_else(|| env.error(GpuError::GpuConnection))?;

        // Pop args
        let args = env.pop_n(pipeline.arg_count)?;

        let mut start = Instant::now();

        // Bind args
        let mut buffers: HashMap<u32, (ArrayBuffers, Shape)> = HashMap::new();
        for (i, mut val) in args.into_iter().rev().enumerate() {
            let ushape: Vec<u32> = val.shape().iter().map(|&n| n as u32).collect();
            let data: Vec<f32> = match &val {
                Value::Num(arr) => arr.data.iter().map(|&n| n as f32).collect(),
                Value::Byte(arr) => arr.data.iter().map(|&n| n as f32).collect(),
                val => {
                    return Err(env.error(format!(
                        "Cannot process {} array (argument {i}) on the GPU",
                        val.type_name()
                    )))
                }
            };
            let rank = device.create_buffer_init(&BufferInitDescriptor {
                label: None,
                contents: bytemuck::bytes_of::<u32>(&(val.rank() as u32)),
                usage: BufferUsages::UNIFORM,
            });
            let shape = device.create_buffer_init(&BufferInitDescriptor {
                label: None,
                contents: bytemuck::cast_slice(if ushape.is_empty() { &[1] } else { &ushape }),
                usage: BufferUsages::STORAGE,
            });
            let data = device.create_buffer_init(&BufferInitDescriptor {
                label: None,
                contents: bytemuck::cast_slice(&data),
                usage: BufferUsages::STORAGE | BufferUsages::COPY_DST,
            });
            let b = pipeline.input_bindings[&i];
            let array_buffers = ArrayBuffers { rank, shape, data };
            buffers.insert(b, (array_buffers, take(val.shape_mut())));
        }

        dbgln!("bind args: {:?}", start.elapsed());
        start = Instant::now();

        // Create result buffers
        for (&i, res_shape) in &pipeline.result_bindings {
            let sh = match res_shape {
                ResultShape::Mon(a) => buffers[a].1.clone(),
                ResultShape::Dy(a, b) => {
                    let ash = &buffers.get(a).or_else(|| pipeline.consts.get(a)).unwrap().1;
                    let bsh = &buffers.get(b).or_else(|| pipeline.consts.get(b)).unwrap().1;
                    let c = pervade::derive_new_shape(ash, bsh, Err(""), Err(""), env)?;
                    for (&a, &c) in ash.iter().zip(&c) {
                        if a == 1 && c != 1 && bsh.len() < ash.len() {
                            return Err(env.error("gpu does not currently support broadcasting"));
                        }
                    }
                    for (&b, &c) in bsh.iter().zip(&c) {
                        if b == 1 && c != 1 && ash.len() < bsh.len() {
                            return Err(env.error("gpu does not currently support broadcasting"));
                        }
                    }
                    c
                }
            };
            let rank = device.create_buffer_init(&BufferInitDescriptor {
                label: Some(&format!("{i} rank")),
                contents: bytemuck::bytes_of::<u32>(&(sh.len() as u32)),
                usage: BufferUsages::UNIFORM,
            });
            let shape = device.create_buffer_init(&BufferInitDescriptor {
                label: Some(&format!("{i} shape")),
                contents: bytemuck::cast_slice(if sh.is_empty() { &[1] } else { &sh }),
                usage: BufferUsages::STORAGE,
            });
            let data = device.create_buffer(&BufferDescriptor {
                label: None,
                size: sh.elements() as u64 * size_of::<f32>() as u64,
                usage: BufferUsages::STORAGE | BufferUsages::COPY_SRC,
                mapped_at_creation: false,
            });
            buffers.insert(i, (ArrayBuffers { rank, shape, data }, sh));
        }
        // Create output buffers
        let output_data_size = buffers[&pipeline.output_buffer].1.elements();
        let output_data_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("output data"),
            size: output_data_size as u64 * size_of::<f32>() as u64,
            usage: BufferUsages::MAP_READ | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        dbgln!("create result buffers: {:?}", start.elapsed());
        start = Instant::now();

        // Create bind groups and pipelines
        let mut bg_pipelines = Vec::new();
        let get_buffers = |i: &u32| -> &(ArrayBuffers, Shape) {
            buffers.get(i).or_else(|| pipeline.consts.get(i)).unwrap()
        };
        for instr in &pipeline.instrs {
            dbgln!("instr: {instr:?}");
            match instr {
                GpuInstr::Mon(gpu_mon, a, b) => {
                    let (module, bg_layout, pl_layout) = &pipeline.mon[gpu_mon];
                    let label = format!("{gpu_mon:?} {a} -> {b}");

                    let (a, ash) = get_buffers(a);
                    let (b, bsh) = get_buffers(b);
                    let bind_group = device.create_bind_group(&BindGroupDescriptor {
                        label: Some(&label),
                        layout: bg_layout,
                        entries: &[
                            a.rank_entry(0, 3),
                            a.shape_entry(0, 3),
                            a.data_entry(0, 3),
                            b.rank_entry(1, 3),
                            b.shape_entry(1, 3),
                            b.data_entry(1, 3),
                        ],
                    });

                    let pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
                        label: Some(&label),
                        layout: Some(pl_layout),
                        module,
                        entry_point: Some("main"),
                        compilation_options: Default::default(),
                        cache: None,
                    });

                    bg_pipelines.push((
                        bind_group,
                        pipeline,
                        ash.elements().max(bsh.elements()) as u32,
                    ));
                }
                GpuInstr::Dy { dy, a, b, c } => {
                    let (module, bg_layout, pl_layout) = &pipeline.dy[dy];
                    let label = format!("{dy:?} {a} {b} -> {c}");

                    let (a, ash) = get_buffers(a);
                    let (b, bsh) = get_buffers(b);
                    let (c, csh) = get_buffers(c);
                    let (adiv, amod) = derive_shape_div_mod(ash, csh);
                    let (bdiv, bmod) = derive_shape_div_mod(bsh, csh);
                    let uni_buf = |u: u32| -> Buffer {
                        device.create_buffer_init(&BufferInitDescriptor {
                            label: None,
                            contents: bytemuck::bytes_of::<u32>(&u),
                            usage: BufferUsages::UNIFORM,
                        })
                    };
                    let adiv_buf = uni_buf(adiv);
                    let amod_buf = uni_buf(amod);
                    let bdiv_buf = uni_buf(bdiv);
                    let bmod_buf = uni_buf(bmod);
                    fn uni_bge(n: u32, i: u32, buf: &Buffer) -> BindGroupEntry {
                        BindGroupEntry {
                            binding: n * 5 + i,
                            resource: buf.as_entire_binding(),
                        }
                    }
                    let bind_group = device.create_bind_group(&BindGroupDescriptor {
                        label: Some(&label),
                        layout: bg_layout,
                        entries: &[
                            // a
                            a.rank_entry(0, 5),
                            a.shape_entry(0, 5),
                            a.data_entry(0, 5),
                            uni_bge(0, 3, &adiv_buf),
                            uni_bge(0, 4, &amod_buf),
                            // b
                            b.rank_entry(1, 5),
                            b.shape_entry(1, 5),
                            b.data_entry(1, 5),
                            uni_bge(1, 3, &bdiv_buf),
                            uni_bge(1, 4, &bmod_buf),
                            // c
                            c.rank_entry(2, 5),
                            c.shape_entry(2, 5),
                            c.data_entry(2, 5),
                        ],
                    });

                    let pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
                        label: Some(&label),
                        layout: Some(pl_layout),
                        module,
                        entry_point: Some("main"),
                        compilation_options: Default::default(),
                        cache: None,
                    });

                    bg_pipelines.push((
                        bind_group,
                        pipeline,
                        ash.elements().max(bsh.elements()).max(csh.elements()) as u32,
                    ));
                }
            }
        }

        dbgln!("create bind groups and pipelines: {:?}", start.elapsed());
        start = Instant::now();

        // Execute
        let mut encoder = device.create_command_encoder(&Default::default());
        for (bind_group, pipeline, elems) in bg_pipelines {
            let mut pass = encoder.begin_compute_pass(&Default::default());
            pass.set_pipeline(&pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.dispatch_workgroups(elems.min(65535), elems / 65536 + 1, 1);
        }
        encoder.copy_buffer_to_buffer(
            &buffers[&pipeline.output_buffer].0.data,
            0,
            &output_data_buffer,
            0,
            output_data_size as u64 * size_of::<f32>() as u64,
        );
        queue.submit([encoder.finish()]);
        dbgln!("sumbit command buffer: {:?}", start.elapsed());
        start = Instant::now();

        // Get the result
        let (data_send, data_recv) = oneshot_channel();
        (output_data_buffer.slice(..)).map_async(wgpu::MapMode::Read, move |result| {
            data_send.send(result).unwrap()
        });

        device.poll(wgpu::Maintain::Wait);
        dbgln!("polling: {:?}", start.elapsed());
        start = Instant::now();

        if let Err(e) = data_recv.receive().await.unwrap() {
            return Err(env.error(e));
        }
        dbgln!("operation complete: {:?}", start.elapsed());
        start = Instant::now();
        let shape: Shape = buffers[&pipeline.output_buffer].1.clone();
        let mut data = eco_vec![0.0; shape.elements()];
        let slice = data.make_mut();
        let output_slice = output_data_buffer.slice(..).get_mapped_range();
        let output_slice_f32 = bytemuck::cast_slice::<_, f32>(&output_slice);
        for (s, o) in slice.iter_mut().zip(output_slice_f32) {
            *s = *o as f64;
        }
        let arr = Array::new(shape, data);
        env.push(arr);
        dbgln!("push result: {:?}", start.elapsed());
        dbgln!("total time: {:?}", func_start.elapsed());

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
                    // dbgln!("adapter: {:?}", adapter.get_info());
                    adapter
                        .request_device(&DeviceDescriptor::default(), None)
                        .await
                        .ok()
                })
            })
            .as_ref()
            .map(|(d, q)| (d, q))
    }

    fn derive_shape_div_mod(a: &Shape, c: &Shape) -> (u32, u32) {
        let unfixed_elems = a.elements();
        let fix_amnt = a.iter().take_while(|&&n| n == 1).count();
        let shared_prefix_len = a
            .iter()
            .zip(c)
            .skip(fix_amnt)
            .take_while(|(a, c)| a == c)
            .count()
            + fix_amnt;
        let div: usize = c[shared_prefix_len..].iter().product();
        (div as u32, unfixed_elems as u32)
    }
}
