@group(0) @binding(0) var<storage, read> ash: array<u32>;
@group(0) @binding(1) var<storage, read> adata: array<f32>;
@group(0) @binding(4) var<storage, write> csh: array<u32>;
@group(0) @binding(5) var<storage, write> cdata: array<f32>;

@compute @workgroup_size(256)fn main(@builtin(global_invocation_id) thread: vec3<u32>) {
    let i = thread.x;
    csh[i] = -ash[i];
}