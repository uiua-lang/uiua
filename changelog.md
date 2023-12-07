# Uiua Changelog

Uiua is not yet stable.

<!-- This version is not yet released. If you are reading this on the website, then these changes are live here. -->

## 0.6.1 - 2023-12-07
### Interpreter
- Make [proxy values](https://uiua.org/docs/functions#proxy) a little less leaky
- Some bug and crash fixes

## 0.6.0 - 2023-12-06
### Language
- [`fix` `¤`](https://uiua.org/docs/fix) now works with binary pervasive functions
  - This removes the need for some uses of [`rows` `≡`](https://uiua.org/docs/rows) and should be a bit faster
- [`fill` `⬚`](https://uiua.org/docs/fill) can now be disabled for a function by filling with an empty list
- [`parse` `⋕`](https://uiua.org/docs/parse) now has a glyph and is semi-pervasive
  - It was being used enough to warrant a glyph
- [`sign` `±`](https://uiua.org/docs/sign), [`floor` `⌊`](https://uiua.org/docs/floor), [`ceiling` `⌈`](https://uiua.org/docs/ceiling), and [`round` `⁅`](https://uiua.org/docs/round) now work with [`under` `⍜`](https://uiua.org/docs/under)
- Add some missing arithmetic inverses and unders involving [`flip` `:`](https://uiua.org/docs/flip)
- Change `pack`'s name to [`unpack` `⊐`](https://uiua.org/docs/unpack), and it no longer implicitly boxes values (only unboxes them)
  - Implicit boxing could lead to unexpected and inconsistent behavior
- Change `invert`'s name and glyph to [`un` `°`](https://uiua.org/docs/un). Code using `⍘` will continue to work and will be formatted as `°`.
  - `°` is a nicer glyph, and `un` composes more nicely with the names of invertible functions
- Deprecate [`unbox` `⊔`](https://uiua.org/docs/unbox) in favor of [`un` `°`](https://uiua.org/docs/un) [`box` `□`](https://uiua.org/docs/box)
  - It can still be typed the same way!
- Deprecate [`reduce` `/`](https://uiua.org/docs/reduce) with a monadic function
  - This created poorly-defined stack signatures that changed depending on the length of the array being reduced
  - [`un` `°`](https://uiua.org/docs/un) with stack array and planet notations, i.e. `°[⊙⊙∘]`, can be used instead, as it has a well-defined signature
  - For operating on just part of an array, use [`under` `⍜`](https://uiua.org/docs/under) [`take` `↙`](https://uiua.org/docs/take), [`drop` `↘`](https://uiua.org/docs/drop), or [`select` `⊏`](https://uiua.org/docs/select)
- [`box` `□`](https://uiua.org/docs/box)ed arrays can once again be compared lexicographically
### Interpreter
- Make [`stack` `?`](https://uiua.org/docs/stack) and [`dump`](https://uiua.org/docs/dump) output show call stack
- Show type and shape information when pretty-printing empty arrays with rank 2 or greater
- Improve language server hover information
- Bug and crash fixes
### Website
- Add a token count to the editor (in settings)
- Files can now be dragged into the editor to open them with [`&fras`](https://uiua.org/docs/&fras)
- [`&fld`](https://uiua.org/docs/&fld) now works on the website

## 0.5.1 - 2023-12-02
### Interpreter
- Fix [`stack` `?`](https://uiua.org/docs/stack) signature

## 0.5.0 - 2023-12-02
### Language
- [`invert` `⍘`](https://uiua.org/docs/un) and [`under` `⍜`](https://uiua.org/docs/under) now work with stack array notation.
- Add the [`stack` `?`](https://uiua.org/docs/stack) function, which debug-prints the entire stack
- [`dump`](https://uiua.org/docs/dump) now works with [`invert` `⍘`](https://uiua.org/docs/un) and [`under` `⍜`](https://uiua.org/docs/under)
- [`fill` `⬚`](https://uiua.org/docs/fill) and [`pack` `⊐`](https://uiua.org/docs/pack) are now exclusive
- Change how [`regex`](https://uiua.org/docs/regex) works to be more powerful
- Add special syntax for splitting/joining lines of code
  - `'` will split a line without changing semantics
  - `''` will combine two lines without changing semantics
- The way pervasive functions work with [`box` `□`](https://uiua.org/docs/box)ed arrays is now more consistent
- Remove `reach`, `distribute`, `tribute`, `level`, `combinate`, and all ocean functions for good
### Interpreter
- Add a style diagnostic about lines that are too long
- Add some other style diagnostics
- Replace `uiua check-update` with `uiua update`, which will update the interpreter by installing a new version with Cargo
- Bug and crash fixes
### Website
- Multiline strings can now be toggled like comments with ctrl+4

## 0.4.1 - 2023-11-30
### Interpreter
- Fix a bug with nested custom modifiers

## 0.4.0 - 2023-11-30
### Language
- [`windows` `◫`](https://uiua.org/docs/windows) can now take negative window sizes
- Add an *experimental* distinction for some functions/modifiers
  - Experimental features are opt-in and must be enabled by putting an `# Experimental!` comment at the top of a file
- Add the experimental [`all` `⋔`](https://uiua.org/docs/all) modifier, which is a variadic generalization of [`both` `∩`](https://uiua.org/docs/both)
- Add the experimental [`rectify` `⌅`](https://uiua.org/docs/rectify) modifier, which sets a function's inverse to itself
- Add the experimental [`setinv`](https://uiua.org/docs/setinv) modifier, which sets the inverse of a function
- Add the experimental [`setunder`](https://uiua.org/docs/setunder) modifier, which sets the [`under` `⍜`](https://uiua.org/docs/under)-compatible inverse of a function
- Add the experimental [`this` `↬`](https://uiua.org/docs/this) modifier, which sets a function to recur to
- Add the experimental [`recur` `↫`](https://uiua.org/docs/recur) modifier, which calls a function recursively
- Allow custom modifiers to use switch function syntax
- [`sign` `±`](https://uiua.org/docs/sign) now normalizes complex numbers
- Change [`type`](https://uiua.org/docs/type) mapping
- Deprecate [`reach` `⟜`](https://uiua.org/docs/reach)
### Interpreter
- Fix some bugs and crashes
- Improve some formatting with multiline functions and switch functions
- [`identity` `∘`](https://uiua.org/docs/identity) no longer formats from `()`
- Allow `uiua <file> [args]` as a shortcut for `uiua run <file> [args]`
### Website
- Allow disabling autorun for pad links

## 0.3.1 - 2023-11-20
### Interpreter
- Fix some bugs and crashes

## 0.3.0 - 2023-11-19
### Language
- **Big Change**
  - Deprecate all ocean functions
  - Deprecate [`level` `≑`](https://uiua.org/docs/level) and [`combinate` `◳`](https://uiua.org/docs/combinate)
  - Deprecate [`tribute` `≐`](https://uiua.org/docs/tribute) and [`distribute` `∺`](https://uiua.org/docs/distribute)
  - [`fold` `∧`](https://uiua.org/docs/fold) no longer takes a rank list
- Add the [`rerank` `☇`](https://uiua.org/docs/rerank) function, which changes the rank of an array's rows
  - This fills the void left by [`level` `≑`](https://uiua.org/docs/level) and [`combinate` `◳`](https://uiua.org/docs/combinate)
- Add the [`fix` `¤`](https://uiua.org/docs/fix) function, which adds a length 1 axis to an array
- [`rows` `≡`](https://uiua.org/docs/rows) now repeats the rows of an arrays that have exactly 1 row
  - This in combination with [`fix` `¤`](https://uiua.org/docs/fix) fills the void left by [`tribute` `≐`](https://uiua.org/docs/tribute) and [`distribute` `∺`](https://uiua.org/docs/distribute)
- [`cross` `⊠`](https://uiua.org/docs/cross) can now take more than 2 arguments
- Switch functions are now less strict about branch signature compatibility and can take arrays as conditions
- A single switch function can now be used as a list of functions for dyadic modifiers
  - [`fork` `⊃`](https://uiua.org/docs/fork) and [`bracket` `⊓`](https://uiua.org/docs/bracket) can take more than 2 functions without chaining this way
- Remove `if ?`, as all its use cases are now covered by switch functions. It will continue to parse, but `?ab` will be formatted as `(b|a)`
- [`flip` `:`](https://uiua.org/docs/flip)'s glyph is now just a colon (it was `RATIO ∶`)
- [`under` `⍜`](https://uiua.org/docs/under) now works with [`absolute value` `⌵`](https://uiua.org/docs/absolute)
- Remove `break ⎋` for good
### Interpreter
- Lots of bug and crash fixes
- Lots of performance improvements
### Website
- Update the [Advanced Array Manipulation Tutorial](https://uiua.org/docs/advancedarray) to reflect the changes in this version

## 0.2.0 - 2023-11-09
### Language
- [`under` `⍜`](https://uiua.org/docs/under) with [`take` `↙`](https://uiua.org/docs/take) and [`drop` `↘`](https://uiua.org/docs/drop) is now less strict about shape/rank changes
- [`range` `⇡`](https://uiua.org/docs/range) called on a list of 0 or 1 values is now more consistent
- [`fill` `⬚`](https://uiua.org/docs/fill) now works with [`rotate` `↻`](https://uiua.org/docs/rotate) to give non-wrapping behavior
- [`fill` `⬚`](https://uiua.org/docs/fill) now works with [`find` `⌕`](https://uiua.org/docs/find) if the searched-for array is longer than the array being searched
- [`parse`](https://uiua.org/docs/parse) now works with [`invert` `⍘`](https://uiua.org/docs/un) and [`under` `⍜`](https://uiua.org/docs/under)
- The output of [`find` `⌕`](https://uiua.org/docs/find) is now the same shape as the array being searched
### Interpreter
- Fix a bunch of bugs
- Several performance optimizations
  - [`transpose` `⍉`](https://uiua.org/docs/transpose) is now much faster
  - [`distribute` `∺`](https://uiua.org/docs/distribute) and [`tribute` `≐`](https://uiua.org/docs/tribute) are now much faster if their function is a pervasive built-in
  - Some other functions are also a bit faster
### Website
- Hold shift when copying a link to copy a Markdown link
- Add embeddable editor. Replace the `pad` in pad links with `embed` or `embedpad`.

## 0.1.0 - 2023-11-03
### Language
- Add complex numbers, which can be created with the [`complex` `ℂ`](https://uiua.org/docs/complex) function
- Add the [`do` `⍢`](https://uiua.org/docs/do) modifier, which repeatedly calls a function as long as a condition holds
- Deprecate [`break` `⎋`](https://uiua.org/docs/break)
- Add multi-dimensional [`where` `⊚`](https://uiua.org/docs/where)
- [`join` `⊂`](https://uiua.org/docs/join) to an empty list now always works regardless of rank of the other array
- [`each` `∵`](https://uiua.org/docs/each) and [`rows` `≡`](https://uiua.org/docs/rows) now work with [`under` `⍜`](https://uiua.org/docs/under)
- All [ocean functions](https://uiua.org/docs/ocean) now work with [`under` `⍜`](https://uiua.org/docs/under)
- Allow multiple values to be returned from [`each` `∵`](https://uiua.org/docs/each), [`rows` `≡`](https://uiua.org/docs/rows), [`distribute` `∺`](https://uiua.org/docs/distribute), [`tribute` `≐`](https://uiua.org/docs/tribute), [`table` `⊞`](https://uiua.org/docs/table), and [`cross` `⊠`](https://uiua.org/docs/cross)
- [`invert` `⍘`](https://uiua.org/docs/un) [`atangent` `∠`](https://uiua.org/docs/atangent) now produces the sine and cosine of an angle
- [`&i`](https://uiua.org/docs/&i) now treats paths as relative to the file calling it rather than the current working directory
- Rank list functions for the rank-generic modifiers can now take any number of arguments. For any number of aguments greater that 0, an empty numeric list will be pushed before the function is called.
- Add fraction literals with `/`
- Parsing multiple formattable functions from words is now smarter
- Remove `bind '`. It made code hard to read. It will continue to parse, but will be formatted as `(…)`
### Interpreter
- Add the `uiua stand` command, which creates a standalone executable
### Website
- Add [Optimizations](https://uiua.org/docs/optimizations) page
- Add [Images and GIFs](https://uiua.org/docs/images) tutorial

## 0.0.25 - 2023-10-29
### Interpreter
- Fix a bug with watch mode

## 0.0.24 - 2023-10-29
### Language
- Add the [`reach` `⟜`](https://uiua.org/docs/reach) modifier, which removes the second value from the stack and calls its function.
- Change how short spellings of [`dip` `⊙`](https://uiua.org/docs/dip), [`gap` `⋅`](https://uiua.org/docs/gap), and [`identity` `∘`](https://uiua.org/docs/identity) work
  - Instead of allowing them to be spelled with 2 characters, they can now be spelled with 1 character as long as there are at least 2 in the sequence. 
  - If present, `'i'` may only come last. 
  - [`reach` `⟜`](https://uiua.org/docs/reach) is included.
- Add 2-letter spellings of [`deep` `≊`](https://uiua.org/docs/deep), [`abyss` `≃`](https://uiua.org/docs/abyss), and [`seabed` `∸`](https://uiua.org/docs/seabed) to make them consistent with [`rock` `⍘`](https://uiua.org/docs/rock).
### Interpreter
- Fix a bunch of bugs and crashes
- The formatter now indents bindings that start with a bound function that starts with [`&i`](https://uiua.org/docs/&i)
- The native interpreter no longer automatically checks for updates. You can still check manually with `uiua update?`.
### Website
- Add challenges to the end of tutorial sections
- Make the introductory examples on the main page less esoteric
- Update the [Advanced Stack Manipulation Tutorial](https://uiua.org/docs/advancedstack) to include [`reach` `⟜`](https://uiua.org/docs/reach)
### Crate
- The [Uiua Rust crate](https://crates.io/crates/uiua) is now [fully documented](https://docs.rs/uiua) and has a decent API

## 0.0.23 - 2023-10-25
### Language
- Implement [`under` `⍜`](https://uiua.org/docs/under) multi-index [`pick` `⊡`](https://uiua.org/docs/pick)
- Implement [`under` `⍜`](https://uiua.org/docs/under) [`partition` `⊜`](https://uiua.org/docs/partition)
- Implement [`under` `⍜`](https://uiua.org/docs/under) [`group` `⊕`](https://uiua.org/docs/group)
- Add [`send`](https://uiua.org/docs/send), [`recv`](https://uiua.org/docs/recv), and [`tryrecv`](https://uiua.org/docs/tryrecv) functions for sending values between threads
- Add [`&fde`](https://uiua.org/docs/&fde) and [`&ftr`](https://uiua.org/docs/&ftr) system functions for deleting/trashing files and directories
- [`under` `⍜`](https://uiua.org/docs/under) with system functions that return stream handles calls [`&cl`](https://uiua.org/docs/&cl) as an inverse
- Add the [`&raw`](https://uiua.org/docs/&raw) system function for setting the terminal to raw mode
- Add the [`&gifd`](https://uiua.org/docs/&gifd) system function for decoding GIFs
### Interpreter
- The interpreter now formats its own diagnostic messages instead of delegating to a library
- Fix a bunch of bugs and crashes
- Add `uiua repl` command
- Optimize ([`⊢`](https://uiua.org/docs/first)[`⍏`](https://uiua.org/docs/rise)), ([`⊢`](https://uiua.org/docs/first)[`⇌`](https://uiua.org/docs/reverse)[`⍏`](https://uiua.org/docs/rise)), ([`⊢`](https://uiua.org/docs/first)[`⍖`](https://uiua.org/docs/fall)), and ([`⊢`](https://uiua.org/docs/first)[`⇌`](https://uiua.org/docs/reverse)[`⍖`](https://uiua.org/docs/fall)) to be O(n) instead of O(nlogn)
- Optimize ([`⊢`](https://uiua.org/docs/first)[`⊚`](https://uiua.org/docs/where)) to not materialize the indices array
### Website
- [`&ast`](https://uiua.org/docs/&ast) now works on the website by generating a fixed amount of audio
  - How long the generated audio is can be configured in the editor settings
- Error and diagnostic messages are no-longer all one color
- The pad editor now inserts a trailing newline on format
- Increase thresholds for arrays automatically becoming images or audio
- Split up system functions into more categories on the main docs page
- The bell character `@\b` now plays a sound if printed with [`&p`](https://uiua.org/docs/&p) or [`&pf`](https://uiua.org/docs/&pf)
### Community
- The [GitHub Discussions](https://github.com/uiua-lang/uiua/discussions) are now open!

## 0.0.22 - 2023-10-21
### Language
- Custom modifier placeholders (`^`) must now be immediately followed by a signature. This reduces the number of signatures that have to be declared everywhere else.

## 0.0.21 - 2023-10-21
### Language
- **Massive Change** - Functions are no longer first-class values. This has many implications:
  - Functions can no longer be put in arrays or manipulated as stack values
  - Inline functions can now only appear as modifier arguments or bindings
  - `call !` has been removed, as there is nothing on the stack to call
  - Modules have been reworked. [`&i`](https://uiua.org/docs/&i) now handles both loading a module from a file and importing items from that module.
  - `---` scopes are now test scopes. `~~~` scopes have been removed.
  - Remove `use`, as it is no longer necessary
  - Boxes still work as normal, but are now their own type distinct from functions
  - Remove `sig`, as everything that can be on the stack now has the same signature
- Add new syntax for defining [custom modifiers](https://uiua.org/docs/custommodifiers)
- Add new syntax for [calling a function from a list of functions](https://uiua.org/docs/controlflow#switch)
- Add the [`pack` `⊐`](https://uiua.org/docs/pack) modifier, which calls its function and implicitly boxes/unboxes values
- Add the [`combinate` `◳`](https://uiua.org/docs/combinate) modifier, which is a rank-generic version of [`table` `⊞`](https://uiua.org/docs/table)
- [`fold` `∧`](https://uiua.org/docs/fold) is now rank-generic and requires a rank list
- Add the [`tribute` `≐`](https://uiua.org/docs/tribute) modifier, which is a flipped version of [`distribute` `∺`](https://uiua.org/docs/distribute)
- Change [`level` `≑`](https://uiua.org/docs/level)'s glyph to reflect its relationship with [`each` `∵`](https://uiua.org/docs/each), [`rows` `≡`](https://uiua.org/docs/rows), [`distribute` `∺`](https://uiua.org/docs/distribute), and [`tribute` `≐`](https://uiua.org/docs/tribute). Code using `⍚` will continue to work and will be formatted as `≑`.
- Add [`rock` `⍘`](https://uiua.org/docs/rock), [`surface` `~`](https://uiua.org/docs/surface), [`deep` `≊`](https://uiua.org/docs/deep), [`abyss` `≃`](https://uiua.org/docs/abyss), and [`seabed` `∸`](https://uiua.org/docs/seabed), which build rank lists to be used with [`level` `≑`](https://uiua.org/docs/level) and the new rank-generic modifiers
- Change [`trace` `⸮`](https://uiua.org/docs/trace)'s glyph to let [`surface` `~`](https://uiua.org/docs/surface) use `~`.
- Change [`match` `≍`](https://uiua.org/docs/match)'s glyph to avoid confusion with the new ocean functions' glyphs. Code using `≅` will continue to work and will be formatted as `≍`.
- Stack signatures found to be incorrect at runtime produce an error
- Dyadic math operations now work with [`under` `⍜`](https://uiua.org/docs/under) even if both arguments are outside [`under` `⍜`](https://uiua.org/docs/under)'s function
- Some mathematical functions that previously did not work with [`invert` `⍘`](https://uiua.org/docs/un) and [`under` `⍜`](https://uiua.org/docs/under) when accompanied by [`flip` `:`](https://uiua.org/docs/flip) now do
### Website
- Add 3 new tutorials
  - [Control Flow](https://uiua.org/docs/controlflow)
  - [Advanced Array Manipulation](https://uiua.org/docs/advancedarray)
  - [Custom Modifiers](https://uiua.org/docs/custommodifiers)
- The orientation of stack values in the output can be flipped in the settings

## 0.0.20 - 2023-10-16
### Language
- Add [`regex`](https://uiua.org/docs/regex) function for matching regular expressions
- Add [`utf`](https://uiua.org/docs/utf) function for UTF-8 encoding and decoding
- Add [`&invk`](https://uiua.org/docs/&invk) system function for invoking a path to be opened with the system's default program
- [`fill` `⬚`](https://uiua.org/docs/fill) can now be used with [`first` `⊢`](https://uiua.org/docs/first)
- Most functions that expect strings as arguments will now dig arbitrarily deep into boxes
- Make [`if` `?`](https://uiua.org/docs/if) signature checking more permissive
- The presence of [`break` `⎋`](https://uiua.org/docs/break) in a [`repeat` `⍥`](https://uiua.org/docs/repeat) always requires a stack signature
- The [`&runi`](https://uiua.org/docs/&runi) and [`&runc`](https://uiua.org/docs/&runc) functions now return exit codes
- Multiline string now only insert `\n` at the end of each line instead of `\r\n`
### Interpreter
- Bugfixes and performance improvements
### Website
- Add a page listing common [stack idioms](https://uiua.org/docs/stack-idioms)

## 0.0.19 - 2023-10-13
### Language
- Add [`under` `⍜`](https://uiua.org/docs/under) [`both` `∩`](https://uiua.org/docs/both)
- Remove `restack ⇵` for good
- Remove `roll ↷` and `unroll ↶` for good
- `@\s` can now be used in addition to `@ ` to get a space character
### Interpreter
- Many performance improvements and memory usage reductions
- Many bug and crash fixes
- Add some additional style diagnostics
- Add more semantic token types to the language server
- Stop using deprecated MarkedString in the language server
### Website
- The editor's font size can now be changed
- Improve brackets/quotes behavior in the editor
- HTML is now properly escaped in the editor
- Formatting can now put the cursor to the left of the current token (toggleable in the settings)

## 0.0.18 - 2023-10-10
### Language
- **Major Change** [`distribute` `∺`](https://uiua.org/docs/distribute) now takes the array being distributed as its *last* argument, rather than its first
- Add [`where` `⊚`](https://uiua.org/docs/where) function, which returns the indices of an array that have non-zero values
- [`if` `?`](https://uiua.org/docs/if)'s branches can now have a different number of arguments (but not outputs)
- [`if` `?`](https://uiua.org/docs/if)'s condition can now be a list of conditions, and the branch will be chosen for each row in the argument(s)
- The reducing versions of [`group` `⊕`](https://uiua.org/docs/group) and [`partition` `⊜`](https://uiua.org/docs/partition) now take accumulators. Aggregating versions are unchanged.
- [`spawn`](https://uiua.org/docs/spawn) and [`wait`](https://uiua.org/docs/wait) no longer have glyphs. Code using `↰` and `↲` will continue to work and will be formatted as `spawn` and `wait`.
- `&n` is no longer a system function and is now called [`now`](https://uiua.org/docs/now)
- [`under` `⍜`](https://uiua.org/docs/under) [`now`](https://uiua.org/docs/now) can be used to time things
- [`call` `!`](https://uiua.org/docs/call) can now call functions that return any number of values, not just one
- Add hex character escape sequences for string and character literals.
  - `\xNN` for short ASCII codes
  - `\uNNNN` for full Unicode sequences
### Interpreter
- The formatter now aligns consecutive end-of-line comments
- `NaN`s no longer propogate in [`min` `⌊`](https://uiua.org/docs/min) and [`max` `⌈`](https://uiua.org/docs/max)
- Fix a bug that prevented [`under` `⍜`](https://uiua.org/docs/under) multidimensional [`take` `↙`](https://uiua.org/docs/take) and [`drop` `↘`](https://uiua.org/docs/drop) from working
- Fix a bug in how [`fold` `∧`](https://uiua.org/docs/fold) ordered multiple accumulators
- Fix a bug that allowed incorrect signatures to be declared for functions
- Fix a bunch of other bugs and crashes
### Website
- Add the Uiua386 font as an option in the editor

## 0.0.17 - 2023-10-07
### Language
- Add GIF encoding with [`&gife`](https://uiua.org/docs/&gife)
- Rename `constant` to [`box` `□`](https://uiua.org/docs/box).
- Add [`unbox` `⊔`](https://uiua.org/docs/unbox), which unboxes a boxed array
- **Major Change:** Some uses of [`call` `!`](https://uiua.org/docs/call) will no longer compile without declaring a stack signature. When unboxing [`box` `□`](https://uiua.org/docs/box)ed arrays, you can use [`unbox` `⊔`](https://uiua.org/docs/unbox) instead, which has a well-defined signature.
- Add [`fall` `⍖`](https://uiua.org/docs/fall) function, which gives the indices of the array if it were sorted descending
- Change `grade` `⌂` name and glyph to [`rise` `⍏`](https://uiua.org/docs/rise) to reflect its relationship with [`fall` `⍖`](https://uiua.org/docs/fall). Code using `⌂` will continue to work and will be formatted as `⍏`.
- [`try` `⍣`](https://uiua.org/docs/try) now puts arguments to its first function *above* the error value when calling the error handler
- [`fold` `∧`](https://uiua.org/docs/fold) can now use multiple accumulators
- Improve [`dump`](https://uiua.org/docs/dump) output formatting
- [`dump`](https://uiua.org/docs/dump) is now a monadic modifier. Its function preprocesses each value before dumping it.
- Add the [`sig`](https://uiua.org/docs/sig) function, which returns the stack signature of a value
- A negative dimensions in the shape passed to [`reshape` `↯`](https://uiua.org/docs/reshape) can now be in *any* position, not just the first or last
- Functions with ASCII glyphs now also format from their names
- Add a advice diagnostic about the captialization of binding names
### Interpreter
- A few performance improvements, particularly to [`keep` `▽`](https://uiua.org/docs/keep), [`fork` `⊃`](https://uiua.org/docs/fork), and [`under` `⍜`](https://uiua.org/docs/under)
### Website
- Add GIF output
- Execution time limit is now 2 seconds by default but can be customized

## 0.0.16 - 2023-10-05
### Interpreter
- Fix a crash and a bug that could occur when creating nested arrays that pull in values.

## 0.0.15 - 2023-10-05
This version changes a lot of glyphs. If you are coming from the previous version, most of the old glyphs will be automatically formatted to the new ones. The only change you may need to make is replacing all `^`s with `|`s.

You may want to read the new version of the [Advanced Stack Manipulation Tutorial](https://uiua.org/docs/advancedstack) to understand the reason for so many of these changes.

### Language
- Add the [`bracket` `⊓`](https://uiua.org/docs/bracket) modifier, which calls two functions each on different arguments
- Change [`fill` `⬚`](https://uiua.org/docs/fill)'s glyph to reflect its relationship with [`box` `□`](https://uiua.org/docs/box). Code using `⍛` with continue to work and will be formatted as `⬚`.
- Change `share` `⇉` name and glyph to [`fork` `⊃`](https://uiua.org/docs/fork). Code using `⇉` will continue to work and will be formatted as `⊃`.
- Change `noop` `·` name and glyphs to [`identity` `∘`](https://uiua.org/docs/identity) to reflect its relationship with [`gap` `⋅`](https://uiua.org/docs/gap) and [`dip` `⊙`](https://uiua.org/docs/dip). Code using `·` will continue to work and will be formatted as `∘`.
- Change [`identity` `∘`](https://uiua.org/docs/identity)'s signature from `|0.0` to `|1.1`
- Add the [`gap` `⋅`](https://uiua.org/docs/gap) modifier, which discards a value then calls its function. It is mainly intended to be used with [`fork` `⊃`](https://uiua.org/docs/fork).
- Change [`dip` `⊙`](https://uiua.org/docs/dip)'s glyph to reflect its relationship with [`gap` `⋅`](https://uiua.org/docs/gap) and [`identity` `∘`](https://uiua.org/docs/identity). Code using `→` will continue to work and will be formatted as `⊙`.
- Change [`both` `∩`](https://uiua.org/docs/both)'s glyph to reflect its relationship with [`fork` `⊃`](https://uiua.org/docs/fork). Code using `∷` will continue to work and will be formatted as `∩`.
- [`distribute` `∺`](https://uiua.org/docs/distribute) now works with any number of arguments. Only the first argument is distributed.
- [`fill` `⬚`](https://uiua.org/docs/fill) now works with [`reshape` `↯`](https://uiua.org/docs/reshape)
- [`reshape` `↯`](https://uiua.org/docs/reshape) now allow negative numbers to denote derived dimensions
- Change the modifier termination character to `|` instead of `^`
- Remove old versions of `fork` and `trident`
- Add the [`&httpsw`](https://uiua.org/docs/&httpsw) function for making HTTPS requests
### Interpreter
- Add formatter configuration options. See the [readme](https://github.com/uiua-lang/uiua#formatter-configuration) for details.
- Checking for updates is less zealous, and can be disabled with the `--no-update` flag to `uiua run` or `uiua watch`
### Website
- Running code in the Pad editor updates the URL to prevent work from accidentally being lost

# Pre-Version Updates

## 2023-10-03
### Language
- Add the [`share` `⊃`](https://uiua.org/docs/fork) modifier, which unifies and deprecates [`fork` `⊃`](https://uiua.org/docs/fork) and [`trident` `∋`](https://uiua.org/docs/trident)
- [`bind` `'`] no longer calls its functions immediately. This should not change any reasonable existing code.
- Change how [`partition` `⊜`](https://uiua.org/docs/partition) and [`group` `⊕`](https://uiua.org/docs/group) work with dyadic functions to be consistent with [`reduce` `/`](https://uiua.org/docs/reduce)
- Deprecate [`restack` `⇵`](https://uiua.org/docs/restack). It was never a good idea.
- Remove the overloaded behavior of [`call` `!`](https://uiua.org/docs/call). It no longer behaves like an if-else when used with a list of functions.
  - You can replace all existing instances of that use case with `!⊡:`
- Add the [`if` `?`](https://uiua.org/docs/if) modifier, which calls one of two functions based on a condition

## 2023-10-02
### Language
- [`both` `∩`](https://uiua.org/docs/both) can now be used with a function that takes any number of arguments.
- Various bug and crash fixes
### Interpreter
- Tell the user when the interpreter can be updated

## 2023-10-01
- Add the [`dip` `⊙`](https://uiua.org/docs/dip) modifier, which temporarily pops a value
- Deprecate `roll↷` and `unroll↶`
- Add [`under` `⍜`](https://uiua.org/docs/under) [`keep` `▽`](https://uiua.org/docs/keep)
- Add [`dump`](https://uiua.org/docs/dump) function, which prints the entire stack

## 2023-09-30
### Language
- Remove the `|1.1` signature restriction for [`under` `⍜`](https://uiua.org/docs/under)'s second function
- Remove the rank`∴` function
- Remove the restriction that all functions in a non-scalar function array all have the compatible signatures
- Whether a binding is a constant or a function is now independent of how many values are on the stack
- Add a system for non-error diagnostics
  - Add advice about redundant uses of [`each` `∵`](https://uiua.org/docs/each)
### Interpreter
- Allow passing `--no-format` to `uiua watch`
- [`&sc`](https://uiua.org/docs/&sc) now returns `0` if EOF is input
### Website
- [`&sc`](https://uiua.org/docs/&sc) now works on the website by showing a prompt

## 2023-09-29
### Language
- Make binding names case-sensitive
- Add `^` syntax to terminate modifier parsing. There is a basic example [in the tutorial](http://uiua.org/docs/functions#terminating-modifiers).
- Add [`&runi`](https://uiua.org/docs/&runi) and [`&runc`](https://uiua.org/docs/&runc) functions for running commands
- Add [`&cd`](https://uiua.org/docs/&cd) function for changing the current working directory
- Add shadowable [constants](https://uiua.org/docs/constants) like `e` and `os`
- Change [`trident` `∋`](https://uiua.org/docs/trident) argument order to make it easier to reason about
- Enable [`fill` `⬚`](https://uiua.org/docs/fill) for [`keep` `▽`](https://uiua.org/docs/keep) if the amount list is shorter than the kept array
### Interpreter
- Add `uiua eval` command which evaluates a Uiua expression and prints the result
- Watch commands no longer try to open the file being watched
- Fix a bug that made numbers that were `≤` `1e-12` `⌵` format to `0`
### Website
- Make a space character `@ ` more visible by underlining the space
- Improve cursor movement when formatting in the editor

## 2023-09-28
### Language
- Add this changelog
- Add [`trace` `~`](https://uiua.org/docs/trace) function
  - Debug-prints the value on top of the stack without popping it
  - Shows the line and column number too
- Add [`both` `∩`](https://uiua.org/docs/both) modifier
  - This can change code like `/(|2 ⊂!:!:) {"a" "bc" "def"}`
  - To just `/'⊂∩! {"a" "bc" "def"}`
- Turn the term pair syntactic construct into a modifier called [`bind` `'`](https://uiua.org/docs/bind)
### Interpreter
- Fix some correctness bugs related to `under` and `invert`
- Fix a crash when trying to reverse an empty array
### Website
- Add a [right-to-left](https://uiua.org/rtl) explanation page
