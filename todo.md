# Uiua Todo

## 0.13
The next version of Uiua

- Update design page
  - So much has changed since it was written!

## Implementation details
Things that don't affect the language itself, but are important for the compiler or interpreter.

- A proper IR (Intermediate Representation)
  - Uiua currently compiles directly from AST to bytecode instructions
  - This complicates the compiler, especially for inverses and optimizations
  - Should go AST -> IR -> bytecode
  - Very big undertaking
- Move array depth checks to compile time
- Allow for multi-value constant bindings
- Dead code elimination
  - Eliminate unused anonymous functions
- Change how inversion of `under` is implemented
  - Current implementation is kinda messy
  - Initial compilation pass of `under` should use some kind of aggregate instruction

## Planned Features
Features that are planned to be implemented in the future.

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

- Plots
  - Different kinds of plots (line, bar, scatter, pie, etc.)
- Channels
- Multimedia
  - Media window
    - A place for `&ims`, `&gifs`, etc to display stuff
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
  - `/F◫`
  - `/F⇌`
  - `⍜⇌/F`
