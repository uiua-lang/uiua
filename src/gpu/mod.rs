use std::{
    array, fmt,
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
    use std::{
        collections::{BTreeMap, HashMap},
        time::Instant,
    };

    use crate::Array;

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
        consts: HashMap<u32, (ArrayBuffers, Vec<u32>)>,
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
        fn rank_entry(&self, n: u32) -> BindGroupEntry {
            BindGroupEntry {
                binding: n * 3,
                resource: self.rank.as_entire_binding(),
            }
        }
        fn shape_entry(&self, n: u32) -> BindGroupEntry {
            BindGroupEntry {
                binding: n * 3 + 1,
                resource: self.shape.as_entire_binding(),
            }
        }
        fn data_entry(&self, n: u32) -> BindGroupEntry {
            BindGroupEntry {
                binding: n * 3 + 2,
                resource: self.data.as_entire_binding(),
            }
        }
    }

    enum GpuInstr {
        Mon(GpuMon, u32, u32),
        Dy(GpuDy, u32, u32, u32),
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
            fn bgl_entry(binding: u32) -> BindGroupLayoutEntry {
                BindGroupLayoutEntry {
                    binding,
                    visibility: ShaderStages::COMPUTE,
                    ty: BindingType::Buffer {
                        ty: if binding % 3 == 0 {
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
                GpuNode::Const(i, shape, data) => {
                    let ushape: Vec<u32> = shape.iter().map(|&n| n as u32).collect();
                    let rank = device.create_buffer_init(&BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::bytes_of::<u32>(&(shape.len() as u32)),
                        usage: BufferUsages::UNIFORM,
                    });
                    let shape = device.create_buffer_init(&BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::cast_slice(shape.as_ref()),
                        usage: BufferUsages::STORAGE,
                    });
                    let data = device.create_buffer_init(&BufferInitDescriptor {
                        label: None,
                        contents: bytemuck::cast_slice(data.as_ref()),
                        usage: BufferUsages::STORAGE,
                    });
                    pl.consts
                        .insert(*i, (ArrayBuffers { rank, shape, data }, ushape));
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
                                    bgl_entry(0),
                                    bgl_entry(1),
                                    bgl_entry(2),
                                    bgl_entry(3),
                                    bgl_entry(4),
                                    bgl_entry(5),
                                ],
                            });
                        let pl_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
                            label: Some(&label),
                            bind_group_layouts: &[&bg_layout],
                            push_constant_ranges: &[],
                        });
                        (shader_module, bg_layout, pl_layout)
                    });
                    pl.instrs.push(GpuInstr::Mon(*op, *i, a));
                    pl.result_bindings.insert(*i, ResultShape::Mon(a));
                    *i
                }
                GpuNode::Dy(i, op, a, b) => {
                    let a = recur(device, a, pl);
                    let b = recur(device, b, pl);
                    pl.dy.entry(*op).or_insert_with(|| {
                        let src = match op {
                            GpuDy::Add => include_str!("add.wgsl"),
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
                                    bgl_entry(0),
                                    bgl_entry(1),
                                    bgl_entry(2),
                                    bgl_entry(3),
                                    bgl_entry(4),
                                    bgl_entry(5),
                                    bgl_entry(6),
                                    bgl_entry(7),
                                    bgl_entry(8),
                                ],
                            });
                        let pl_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
                            label: Some(&label),
                            bind_group_layouts: &[&bg_layout],
                            push_constant_ranges: &[],
                        });
                        (shader_module, bg_layout, pl_layout)
                    });
                    pl.instrs.push(GpuInstr::Dy(*op, a, b, *i));
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
        let mut buffers: HashMap<u32, (ArrayBuffers, Vec<u32>)> = HashMap::new();
        let mut max_rank = 0;
        let mut max_elems = 0;
        for (i, val) in args.into_iter().enumerate() {
            max_rank = max_rank.max(val.rank());
            max_elems = max_elems.max(val.element_count());

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
            buffers.insert(b, (ArrayBuffers { rank, shape, data }, ushape));
        }

        dbgln!("bind args: {:?}", start.elapsed());
        start = Instant::now();

        // Create result buffers
        for (&i, res_shape) in &pipeline.result_bindings {
            let ushape = match res_shape {
                ResultShape::Mon(a) => buffers[a].1.clone(),
                ResultShape::Dy(a, b) => {
                    let a = &buffers[a].1;
                    let b = &buffers[b].1;
                    let mut c = if a.len() > b.len() {
                        a.clone()
                    } else {
                        b.clone()
                    };
                    for i in 0..a.len().min(b.len()) {
                        let ci = c.len() - i - 1;
                        c[ci] = a[a.len() - i - 1].max(b[b.len() - i - 1]);
                    }
                    c
                }
            };
            let rank = device.create_buffer_init(&BufferInitDescriptor {
                label: Some(&format!("{i} rank")),
                contents: bytemuck::bytes_of::<u32>(&(ushape.len() as u32)),
                usage: BufferUsages::UNIFORM,
            });
            let shape = device.create_buffer_init(&BufferInitDescriptor {
                label: Some(&format!("{i} shape")),
                contents: bytemuck::cast_slice(if ushape.is_empty() { &[1] } else { &ushape }),
                usage: BufferUsages::STORAGE,
            });
            let data = device.create_buffer(&BufferDescriptor {
                label: None,
                size: max_elems as u64 * size_of::<f32>() as u64,
                usage: BufferUsages::STORAGE | BufferUsages::COPY_SRC,
                mapped_at_creation: false,
            });
            buffers.insert(i, (ArrayBuffers { rank, shape, data }, ushape));
        }
        // Create output buffers
        let output_data_buffer = device.create_buffer(&BufferDescriptor {
            label: Some("output data"),
            size: max_elems as u64 * size_of::<f32>() as u64,
            usage: BufferUsages::MAP_READ | BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        dbgln!("create result buffers: {:?}", start.elapsed());
        start = Instant::now();

        // Create bind groups and pipelines
        let mut bg_pipelines = Vec::new();
        let get_buffers = |i: &u32| -> &(ArrayBuffers, Vec<u32>) {
            buffers.get(i).or_else(|| pipeline.consts.get(i)).unwrap()
        };
        for instr in &pipeline.instrs {
            match instr {
                GpuInstr::Mon(gpu_mon, a, b) => {
                    let (module, bg_layout, pl_layout) = &pipeline.mon[gpu_mon];
                    let label = format!("{gpu_mon:?} {a} -> {b}");

                    let (a, _) = get_buffers(a);
                    let (b, _) = get_buffers(b);
                    let bind_group = device.create_bind_group(&BindGroupDescriptor {
                        label: Some(&label),
                        layout: bg_layout,
                        entries: &[
                            a.rank_entry(0),
                            a.shape_entry(0),
                            a.data_entry(0),
                            b.rank_entry(1),
                            b.shape_entry(1),
                            b.data_entry(1),
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

                    bg_pipelines.push((bind_group, pipeline));
                }
                GpuInstr::Dy(gpu_dy, a, b, c) => {
                    let (module, bg_layout, pl_layout) = &pipeline.dy[gpu_dy];
                    let label = format!("{gpu_dy:?} {a} {b} -> {c}");

                    let (a, _) = get_buffers(a);
                    let (b, _) = get_buffers(b);
                    let (c, _) = get_buffers(c);
                    let bind_group = device.create_bind_group(&BindGroupDescriptor {
                        label: Some(&label),
                        layout: bg_layout,
                        entries: &[
                            a.rank_entry(0),
                            a.shape_entry(0),
                            a.data_entry(0),
                            b.rank_entry(1),
                            b.shape_entry(1),
                            b.data_entry(1),
                            c.rank_entry(2),
                            c.shape_entry(2),
                            c.data_entry(2),
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

                    bg_pipelines.push((bind_group, pipeline));
                }
            }
        }

        dbgln!("create bind groups and pipelines: {:?}", start.elapsed());
        start = Instant::now();

        // Execute
        let mut encoder = device.create_command_encoder(&Default::default());
        for (bind_group, pipeline) in bg_pipelines {
            let mut pass = encoder.begin_compute_pass(&Default::default());
            pass.set_pipeline(&pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.dispatch_workgroups(
                (max_elems as u32).min(65535),
                ((max_elems as u32) / 65536) + 1,
                1,
            );
        }
        encoder.copy_buffer_to_buffer(
            &buffers[&pipeline.output_buffer].0.data,
            0,
            &output_data_buffer,
            0,
            max_elems as u64 * size_of::<f32>() as u64,
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
        let shape: Shape = buffers[&pipeline.output_buffer]
            .1
            .iter()
            .map(|&n| n as usize)
            .collect();
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
}
