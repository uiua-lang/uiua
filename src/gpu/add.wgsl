@group(0) @binding(0) var<storage, read_write> ash: array<u32>;
@group(0) @binding(1) var<storage, read_write> adata: array<f32>;
@group(0) @binding(2) var<storage, read_write> bsh: array<u32>;
@group(0) @binding(3) var<storage, read_write> bdata: array<f32>;
@group(0) @binding(4) var<storage, read_write> csh: array<u32>;
@group(0) @binding(5) var<storage, read_write> cdata: array<f32>;

@compute @workgroup_size(256)fn main(@builtin(global_invocation_id) thread: vec3<u32>) {
    let i = thread.x;
    cdata[i] = adata[i] + bdata[i];
}