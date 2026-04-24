# Uiua Todo

# 0.19
- Type system
  - Fix dipped argument hinting
  - Boxed type specification
  - Make `validate` a function?
  - Data def types
  - Missing primitives
  - Documentation
  - Blog post
- `match` subscripts
- Macro signature comments
- Data def method to construct with validators but not defaults/initializers
- Optimize `rows match`
- Improve GA API
- `first group/partition` optimization
- `under group/partition inventory` optimization

## Planned Features
Features that are planned to be implemented in the future.

- Stackless execution trees?
- Step debugging
- Better compiler IR system
  - Make the stack only exist at compile time?
- Dead code elimination
  - Eliminate unused anonymous functions
- Dynamic modules
  - A system by which the public interface of a module can be specified
  - Instances of a dynamic module can be loaded at runtime
  - Enables things from simple config files to game mods
- System APIs
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
