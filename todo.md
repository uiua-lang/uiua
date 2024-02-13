# Uiua Todo

- 0.9
  - Add a pointer representation for FFI
  - Fix ident= parse bug
  - Spawn thread in pool?
  - Make `&var` throw
  - Ensure proper label propogation on all primitives
  - Pad link from Gist
- Changle how function inlining works
  - Prevent inlining of large functions
  - If a function is inlinable, don't actually put it in the assembly instructions
- Multimedia
  - Sound input
  - Webcam input
- System APIs
  - UDP Sockets
- Documentation
  - System functions

## Open to Implementation
These features are not strictly on the roadmap, but PRs that implement them will likely be accepted.

Feel free to make PRs to the list itself as well.

- `⍜◿`
- Better complex `⌈`/`⌊`/`⁅` implementation
- Additional optimizations
  - `≡/F`
  - `≡F◫`
  - `/F◫`
  - `/F⇌`
  - `⍜⇌/F`