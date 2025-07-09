# Uiua Todo

# 0.17
- `Variants` field for module
- Fix caching with labels
- Fix performance of deep `dip` nesting
- Don't serialize sortedness flags
- Sided `fork`
- Sided `join`
- `table` subscripts for rank selection
- Mixed subscripts
  - `bracket`
- Reserved modifier glyphs?
- Implement some `under` inverses in terms of `un by`, rather than the other way around?
- `un`/`anti` `stencil`
- Compile-time code string evaluation?
- Streaming gif and audio encoding

## Planned Features
Features that are planned to be implemented in the future.

- Stackless execution trees?
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
