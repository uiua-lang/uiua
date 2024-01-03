# Uiua Todo
Within each section, items are ordered (roughly) by decreasing priority.

## Features
- `fill` `get`
- Change `pop`s glyph to `◌`
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
- Backwards primitive lexing
- Output comments in `under`
- `fill reshape` by arrays
- Figure out what is going on with the editor cursor

## Optimizations
- Row windows - `≡f◫` for scalar window size should be optimized to not materialize all the windows
- Reduce windows? - `/f◫` for scalar window size could be optimized, but is it necessary?

## Documentation
- System functions

## Tooling
- Discord bot
