# Uiua Todo

## 0.13
The next version of Uiua

- Make memory limit configurable

## Implementation details
Things that don't affect the language itself, but are important for the compiler or interpreter.

- Change how inlining modifiers works
  - Make all (or most) modifiers emit themselves as instructions
  - This allows inverses to be smarter about what they are working on
  - Add an inlining step between compilation and execution
  - Change how inversion of `under` is implemented
    - Current implementation is kinda messy
    - New system should make it simpler
- Move array depth checks to compile time
- Allow for multi-value constant bindings
- Dead code elimination
  - Eliminate unused anonymous functions

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

- Channels
- Multimedia
  - WEBP
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
