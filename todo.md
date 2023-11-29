# Uiua Todo
Within each section, items are ordered (roughly) by decreasing priority.

## Features
- Delay missing modifier arg checking to compile time
- Allow grouping negation of glyph constants
- Multimedia
  - Sound input
  - Webcam input
  - Canvas
    - Windowing
    - Input handling
- System APIs
  - FFI
  - UDP Sockets

## Bugs
- Figure out what is going on with the editor cursor
  - Problems are mostly on Firefox, especially on mobile
- Expand test suite

## Optimizations
- Row windows - `≡f◫` for scalar window size should be optimized to not materialize all the windows
- Reduce windows? - `/f◫` for scalar window size could be optimized, but is it necessary?

## Documentation
- System functions

## Tooling
- Discord bot
