@group(0) @binding(0) var<uniform> arank: u32;
@group(0) @binding(1) var<storage, read> ash: array<u32>;
@group(0) @binding(2) var<storage, read_write> adata: array<f32>;

@group(0) @binding(3) var<uniform> crank: u32;
@group(0) @binding(4) var<storage, read> csh: array<u32>;
@group(0) @binding(5) var<storage, read_write> cdata: array<f32>;

@compute @workgroup_size(256)fn main(@builtin(global_invocation_id) thread: vec3<u32>) {
    let i = thread.x + thread.y * 65536;
    cdata[i] = -adata[i];
}