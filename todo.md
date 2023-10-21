# Uiua Todo
Within a each section, items are ordered (roughly) by decreasing priority.

## Features
- Make fill dyadic again
- Under dyadic math
- `under` aggregating `group` and `partition`
- Add channels for spawned threads
- Sift+delete to delete whole lines
- Rust API
  - Make dedicated Array conversion traits
  - Make dedicated Value conversion traits
  - Make dyadic array functions not methods
  - Make most things private
- Multimedia
  - Sound input
  - Webcam input
- System APIs
  - Get single char from stdin
  - Delete/trash files/directories
  - FFI
  - UDP Sockets

## Bugs
- Figure out what is going on with the editor cursor
  - Problems are mostly on Firefox, especially on mobile
- Expand test suite

## Optimizations
- Inline some functions with `distribute` and `tribute`
- Row windows - `≡f◫` for scalar window size should be optimized to not materialize all the windows
- Reduce windows? - `/f◫` for scalar window size could be optimized, but is it necessary?

## Documentation
- Images and GIFs
- System functions

## Tooling
- REPL
- Discord bot