# Uiua Todo

## 0.14
The next version of Uiua

- Overhaul indices
- Fix indented formatter bug
- Forced signatures
- Proper data def locals
- Diagnostic for 0-output function wrapped in `[]`
- Window improvements
- `un repeat`: convergence count?
- Unified `stencil` implementation
- Let `&ast` take args
- Stabilize `backward`, `case`
  - Need to figure out `backward`'s glyph and non-dyadic semantics
  - Add `case` to pattern matching tutorial

# 0.15
- Optimize `conjoin inventory`
- Compile-time code string evaluation
- `do` function pack
- Allow for multi-value constant bindings
- Animated WEBP support? (`&webp`)
- Stackless execution trees?

## Planned Features
Features that are planned to be implemented in the future.

- Step debugging
- Better compiler IR sytem
  - Make the stack only exist at compile time?
- Dead code elimination
  - Eliminate unused anonymous functions
- Dynamic modules
  - A system by which the public interface of a module can be specified
  - Instances of a dynamic module can be loaded at runtime
  - Enabled things from simple config files to game mods
- System APIs
  - UDP Sockets
    - `&udpb` to bind a socket
    - `&udpc` to connect a socket?
    - `&udpr` to receive data from a socket
      - Returns both data and source address
    - `&udps` to send data to a socket
      - Takes both data and destination address
    - Change `&tcpsnb`, `&tcpsrt`, `&tcpswt` to work for UDP sockets as well
      - Rename them
  - File metadata
    - `&fmeta` to get metadata about a file
    - Should somehow provide:
      - size
      - kind (file, directory, symlink)
      - create/modify/access times
      - permissions
    - It's possible this should be multiple functions
      - `&fsize`
      - `&fkind`
      - `&ftime`
      - `&fperm`

## Potential Features
Features that could be implemented, but are not currently planned.

- Channels
- Multimedia
  - Sound input
    - Something akin to `&ast` but for input
    - `&arec` to record audio for some duration
      - Maybe it should be a modifier that records until its function returns false?

## Open to Implementation

These features are not strictly on the roadmap, but PRs that implement them will likely be accepted.

Feel free to make PRs to the list itself as well.

- Better complex `⌈`/`⌊`/`⁅`/`◿`, `<`/`>`/`≤`/`≥`, `↥`/`↧`
- Pad link from Gist
- Additional optimizations
  - `/F⇌`
  - `⍜⇌/F`
