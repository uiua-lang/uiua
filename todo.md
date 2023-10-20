# Uiua Todo
Within a each section, items are ordered (roughly) by decreasing priority.

## Bugs
- Figure out what is going on with the editor cursor
  - Problems are mostly on Firefox, especially on mobile
- Expand test suite

## Features
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
  - FFI
  - UDP Sockets

## Optimizations
- Inline some functions with `distribute`
- Row windows - `≡f◫` for scalar window size should be optimized to not materialize all the windows
- Reduce windows? - `/f◫` for scalar window size could be optimized, but is it necessary?
- See what can be done about compile times
  - See how much turning off LTO does to performance vs compile time

## Documentation
- Thinking with arrays
- Control flow
  - Looping modifiers
  - If
  - Try
  - Break
  - Recur
- Images and GIFs
- System functions

## Tooling
- REPL
- Discord bot