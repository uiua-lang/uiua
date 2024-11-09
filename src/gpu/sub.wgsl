@group(0) @binding(0) var<uniform> arank: u32;
@group(0) @binding(1) var<storage, read> ash: array<u32>;
@group(0) @binding(2) var<storage, read_write> adata: array<f32>;
@group(0) @binding(3) var<uniform> adiv: u32;
@group(0) @binding(4) var<uniform> amod: u32;

@group(0) @binding(5) var<uniform> brank: u32;
@group(0) @binding(6) var<storage, read> bsh: array<u32>;
@group(0) @binding(7) var<storage, read_write> bdata: array<f32>;
@group(0) @binding(8) var<uniform> bdiv: u32;
@group(0) @binding(9) var<uniform> bmod: u32;

@group(0) @binding(10) var<uniform> crank: u32;
@group(0) @binding(11) var<storage, read> csh: array<u32>;
@group(0) @binding(12) var<storage, read_write> cdata: array<f32>;

@compute @workgroup_size(256)fn main(@builtin(global_invocation_id) thread: vec3<u32>) {
    let i = thread.x + thread.y * 65536;
    cdata[i] = bdata[i / bdiv % bmod] - adata[i / adiv % amod];
}