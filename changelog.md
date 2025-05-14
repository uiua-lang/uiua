# Uiua Changelog

Uiua is not yet stable.

## 0.16.0 - 2025-??-??
This version is not yet released. If you are reading this on the website, then these changes are live here.
### Language
- **Breaking Change** - [`rows ≡`](https://uiua.org/docs/rows) numeric subscripts now function identically to [`each ∵`](https://uiua.org/docs/repeat)'s specifying the rank to operate at
  - Deprecate [`each ∵`](https://uiua.org/docs/repeat), as it is almost never used except for this subscript behavior
  - The formatter will turn existing uses of subscripted [`each ∵`](https://uiua.org/docs/repeat) into subscripted [`rows ≡`](https://uiua.org/docs/rows)
- **Breaking Change** - [`repeat ⍥`](https://uiua.org/docs/repeat) and [`do ⍢`](https://uiua.org/docs/do) now accumulate excess values into arrays if their function has more outputs than arguments
  - This makes a lot of accumulation patterns much shorter and simpler
  - These loops now always have well-defined signatures
  - This breaks the common pattern of wrapping these loops in `[]`s, which is now unnecessary and will need to be changed in your code
- **Breaking Change** - [`fill ⬚`](https://uiua.org/docs/fill)ed [`scan \\`](https://uiua.org/docs/scan) no longer uses the fill value as the first row
- **Breaking Change** - [`fft`](https://uiua.org/docs/fft) now works along every axis of an array rather than only its last
  - This is more consistent with other functions
- **Breaking Change** - Overhaul number literals
  - Literals involving [`eta η`](https://uiua.org/docs/eta), [`pi π`](https://uiua.org/docs/pi), [`tau τ`](https://uiua.org/docs/tau), and `e` may now have a leading coefficient in addition to a denominator, and the symbol may be *in* the denominator
    - Examples: `2π`, `3π/4`, `1/τ`, `1.5η`, `3e`
  - Add complex literals
    - The real and imaginary parts are suffixed with `r` and `i` respectively. Both can be used alone.
    - Work with fractions and other constants above
    - Examples: `3r4i`, `5i`, `πi/2`
- There are no longer signature restrictions on [`try ⍣`](https://uiua.org/docs/try)'s functions
- Allow for [mixed](https://uiua.org/docs/subscripts#mixed) numeric and sided subscripts
  - Implemented for [`both ∩`](https://uiua.org/docs/both), [`rows ≡`](https://uiua.org/docs/rows), and [`inventory ⍚`](https://uiua.org/docs/inventory)
- Stabilize sided subscripts for [`rows ≡`](https://uiua.org/docs/rows) and [`inventory ⍚`](https://uiua.org/docs/inventory)
- Stabilize [`un °`](https://uiua.org/docs/un) [`under ⍜`](https://uiua.org/docs/under) for monadic functions
- Add [`un °`](https://uiua.org/docs/un) [`group ⊕`](https://uiua.org/docs/group) and [`un °`](https://uiua.org/docs/un) [`partition ⊜`](https://uiua.org/docs/partition) for monadic functions
- Stabilize [`negate ¯`](https://uiua.org/docs/negate) subscripts
- Stabilize [`base ⊥`](https://uiua.org/docs/fft)
- Stabilize [`fft`](https://uiua.org/docs/fft)
- Stabilize [inline macros](https://uiua.org/tutorial/macros#inline-macros)
- The first argument to [`rotate ↻`](https://uiua.org/docs/rotate) can now be rank > 1.
  - This creates an array with multiple copies of the target array rotated by different amounts
- [Output comments](https://uiua.org/tutorial/basic#output-comments) now show values on the stack *after* their line rather than before it
- Add [`un °`](https://uiua.org/docs/un) [`fill ⬚`](https://uiua.org/docs/fill)ed [`take ↙`](https://uiua.org/docs/take)
- Add [`un °`](https://uiua.org/docs/un) [`add +`](https://uiua.org/docs/add), [`un °`](https://uiua.org/docs/un) [`multiply ×`](https://uiua.org/docs/multiply) and [`un °`](https://uiua.org/docs/un) [`divide ÷`](https://uiua.org/docs/divide)
  - These split into fraction and whole, sign and magnitude, and denominator and numerator respectively
- Add [`un °`](https://uiua.org/docs/un) inverses for [`reduce /`](https://uiua.org/docs/reduce) with a dyadic function (when the function is invertible)
- Functions can now contains scoped local bindings
  - This allows for simple helper functions as well as passing bindings to macros
- Give [`base ⊥`](https://uiua.org/docs/base) a glyph
- Add the [`ln`](https://uiua.org/docs/ln) function, which computes the natural logarithm
- Add the [`pretty`](https://uiua.org/docs/pretty) function, which gives the string representation of an array's pretty-printed output
- Add numeric subscripts for [`length ⧻`](https://uiua.org/docs/length) to get the length of a specific axis
- Add numeric subscripts for [`shape △`](https://uiua.org/docs/shape) to get the shape of the first few axes
- Add numeric subscripts for [`range ⇡`](https://uiua.org/docs/range) to offset the range
- Add support for mixed numeric and sided subscripts
- Greeks rejoice! `η`, `π`, and `τ` can now be used in names (just not as the first letter)
- Add [`apng`](https://uiua.org/docs/apng) and [`&apngs`](https://uiua.org/docs/&apngs) functions for encoding and showing APNG animations
- Deprecate [`with ⤙`](https://uiua.org/docs/with) and [`off ⤚`](https://uiua.org/docs/off) on noadic and monadic functions
  - There was a lot of disagreement about what the behavior should be
  - They made the movement of data on the stack harder to follow
  - **Breaking Change** - Change how [`off ⤚`](https://uiua.org/docs/off) works on monadic functions
    - This change was made before deprecation was decided
- Deprecate various environment-relate constants and replace them with noadic functions
  - This prevents the details of the compiling environment being used instead of the details from the actual running environment
  - Deprecrated: `Os`, `Family`, `Arch`, `ExeExt`, `DllExt`, `Sep`, `NumProcs`
  - Replacements: `os`, `osfamily`, `arch`, `exeext`, `dllext`, `pathsep`, `numprocs`
- Add experimental [`evert ⧋`](https://uiua.org/docs/evert) modifier for operating on the last axes of arrays.
- Add experimental [`progressive indexof ⊘`](https://uiua.org/docs/progressiveindexof) for finding sequential indices of each row of an array in another
- Add experimental [lexical ordering](https://uiua.org/docs/experimental#lexical-ordering) syntax
  - This enables some function packs and array notation to execute in the order they are normally read
- Add experimental sided [`fill ⬚`](https://uiua.org/docs/fill)
  - This allows filling from the left instead of the right
- Add experimental [`voxels`](https://uiua.org/docs/voxels) function, which orthographically projects a 3D voxel array to an image
- Add experimental [`bytes`](https://uiua.org/docs/bytes) function for encoding and decoding byte arrays
- Remove previously deprecated `around ’`
- Remove previously deprecated `astar`
- Remove previously deprecated `trace ⸮`
### Interpreter
- Improve pretty-printed array layout
  - High-rank arrays take up less vertical space and more horizontal space
  - Box arrays take up less space
- Complex arrays are now compatible with image and GIF functions and will display as such automatically in the pad
  - Each pixel is colored via a domain coloring function
- Remove errors when a line is too long
- The formatter will now allow putting multiple function pack branches on a single line
- Optimizations
  - [`stencil ⧈`](https://uiua.org/docs/stencil) is now optimized when used with with monadic functions that are optimized for [`rows ≡`](https://uiua.org/docs/rows)
  - Add sortedness flags to arrays to allow short-circuiting some operations
  - Optimize [`tuples ⧅`](https://uiua.org/docs/tuples) with `⋅⋅1` and `⋅⧻`
  - The main sorting algorith for [`sort ⍆`](https://uiua.org/docs/sort) is now implemented in the interpreter source code rather than a library
    - This allows it to sort arrays in place without allocating memory
    - Observed performance improvements of ~2.5x
  - [`sort ⍆`](https://uiua.org/docs/sort) on lists of bytes now uses counting sort, for massive performance improvements
- LSP improvements
  - Completions now properly respect scoping
  - Unused private bindings are now dimmed
- Images in the native output window can now be easily copied to the clipboard
- Add an `-x`/`--experimental` flag to the `uiua repl` command to pre-enable experimental features

## 0.15.1 - 2025-04-06
### Interpreter
- Fix builds on some systems

## 0.15.0 - 2025-04-03
You can find the release announcement [here](https://uiua.org/blog/uiua-0.15.0).
### Language
- **Breaking Change** - [`repeat ⍥`](https://uiua.org/docs/repeat) and [`do ⍢`](https://uiua.org/docs/do) with net-negative signatures now preserve lower stack values between iterations
- **Breaking Change** - [`atangent ∠`](https://uiua.org/docs/atan) now interacts with [`under ⍜`](https://uiua.org/docs/under) similarly to [`couple ⊟`](https://uiua.org/docs/couple) and [`complex ℂ`](https://uiua.org/docs/complex)
- Add subscripted [`on ⟜`](https://uiua.org/docs/on), [`by ⊸`](https://uiua.org/docs/by), [`with ⤙`](https://uiua.org/docs/with), and [`off ⤚`](https://uiua.org/docs/off)
  - These preserve N arguments rather than just the first or last
- Add data definition methods
- Stabilize most of data definitions
  - A new [section](https://uiua.org/tutorial/datadefs) has been added to the tutorial
  - Things not stabilized:
    - Data functions
    - Methods
    - Field validators
- Stabilize subscripted [`random ⚂`](https://uiua.org/docs/random)
- Signature comments can now use a `$` rather than a `?` to automatically label arguments and outputs
- Change [`backward ˜`](https://uiua.org/docs/backward)'s glyph back. `𝄈` will continue to work and will be formatted as `˜`.
  - This glyph has much better font support
- Stabilize [`backward ˜`](https://uiua.org/docs/backward)
- Add [`anti ⌝`](https://uiua.org/docs/anti) [`keep ▽`](https://uiua.org/docs/keep) for parity with [`select ⊏`](https://uiua.org/docs/select)
- Add subscripts for [`bits ⋯`](https://uiua.org/docs/bits) to force the number of bits
- Allow [`tuples ⧅`](https://uiua.org/docs/tuples) to take [`infinity ∞`](https://uiua.org/docs/infinity) as a first argument
- Deprecate [`over ,`](https://uiua.org/docs/over)
  - It is part of a stack manipulation paradigm that Uiua is slowly moving away from
  - [`with ⤙`](https://uiua.org/docs/with) and [`below ◡`](https://uiua.org/docs/below) replace the vast majority of [`over ,`](https://uiua.org/docs/over)'s use cases
  - You can find a short blog post about this change [here](https://uiua.org/blog/its-so-over)
- Stabilize [sided subscripts](https://uiua.org/tutorial/evenmorestack#sided-subscripts) for [`both ∩`](https://uiua.org/docs/both) and [`bracket ⊓`](https://uiua.org/docs/bracket)
- Add [`un °`](https://uiua.org/docs/un) [`with ⤙`](https://uiua.org/docs/with) and [`un °`](https://uiua.org/docs/un) [`off ⤚`](https://uiua.org/docs/off)
- Add sided subscripts for [`reach 𝄐`](https://uiua.org/docs/reach)
- Add the [`# External!`](https://www.uiua.org/tutorial/documentation#external) semantic comment to mark functions that are provided via Rust code
  - These functions don't require a Uiua implementation and will show up in the LSP
  - Calling an `# External!` function that hasn't been bound will throw an error
- Add experimental [`self ˙`](https://uiua.org/docs/self) modifier
- Add experimental subscripts to [`negate ¯`](https://uiua.org/docs/negate)
  - This will [`multiply ×`](https://uiua.org/docs/multiply) a number by the Nth root of unity
- Remove `&clget` alias for [`&clip`](https://uiua.org/docs/&clip)
- Remove previously deprecated `&httpsw`
### Interpreter
- Add `UIUA_MAX_MB` environment variable to set the maximum size of an array in megabytes
  - This allows catching memory errors on different systems
- Optimize [`absolute value ⌵`](https://uiua.org/docs/absolute)[`complex ℂ`](https://uiua.org/docs/complex) to not create intermediate [`complex ℂ`](https://uiua.org/docs/complex) array
### Website
- Add an expanded mode to the pad. Click the ⤢ button to toggle it.
- Change pad icons
- Add new [Data Definitions](https://uiua.org/tutorial/datadefs) tutorial
- Add a [Format String Tricks](https://uiua.org/tutorial/strings#format-string-tricks) section to the [Working with Strings](https://uiua.org/tutorial/strings) tutorial

## 0.14.1 - 2024-12-23
### Interpreter
- Various bug fixes related to shapes, types, fills, and optimizations

## 0.14.0 - 2024-12-20
You can find the release announcement [here](https://uiua.org/blog/uiua-0.14.0).
### Language
- **Breaking Change**: Multi-argument [`group ⊕`](https://uiua.org/docs/group) and [`partition ⊜`](https://uiua.org/docs/partition) no longer do reduction. Instead, multiple groups are passed to the function.
  - The reducing versions were rarely used
  - This new behavior is more useful
- **Breaking Change**: [`un °`](https://uiua.org/docs/un) [`json`](https://uiua.org/docs/json) no longer attempts to form multidimensional arrays
  - This makes deserializing JSON more consistent
- **Breaking Change**: [`fill ⬚`](https://uiua.org/docs/fill)ed [`scan \\`](https://uiua.org/docs/scan) now sets the initial value as well as filling row shapes
  - This behavior is hard to get otherwise
  - Fixes to existing code should be simple
- **Breaking Change**: [`obverse ⌅`](https://uiua.org/docs/obverse) with a single function now just nullifies the inverse
  - This makes a lot of common [`under ⍜`](https://uiua.org/docs/under) patterns much simpler
- **Breaking Change**: Negative indices to [`pick ⊡`](https://uiua.org/docs/pick) and [`select ⊏`](https://uiua.org/docs/select) now always use a fill value if available
- Stabilize [subscripts](https://uiua.org/docs/subscripts)!
  - They make available a lot of nice functionality
  - Allow negative subscripts
  - Add experimental [sided subscripts](https://uiua.org/docs/subscripts#sided)
- [`rows ≡`](https://uiua.org/docs/rows), [`inventory ⍚`](https://uiua.org/docs/inventory), and [`each ∵`](https://uiua.org/docs/each) now support subscripts
- [`deshape ♭`](https://uiua.org/docs/deshape) now supports subscripts
  - Axes are collapsed to get the given rank
- Deprecate [`rerank ☇`](https://uiua.org/docs/rerank)
  - It was basically always used with a static rank, and that functionality has been subsumed by subscripted [`deshape ♭`](https://uiua.org/docs/deshape) and/or the iterating modifiers listed above
  - The dynamic behavior can still be accessed with `°⊸(⧻△)` (though when you would need this is unclear)
- Add the [`stencil ⧈`](https://uiua.org/docs/stencil) modifier, which is a generalization of [`windows ◫`](https://uiua.org/docs/windows)
  - `⧈∘` is equivalent to [`windows ◫`](https://uiua.org/docs/windows)
  - [`windows ◫`](https://uiua.org/docs/windows) has been deprecated. All existing uses will continue to work and will be formatted as `⧈∘`.
- [`get`](https://uiua.org/docs/get), [`has`](https://uiua.org/docs/has), and [`remove`](https://uiua.org/docs/remove) now support working on multiple key-value pairs at once
- Deprecate [`trace ⸮`](https://uiua.org/docs/trace)
  - It is equivalent to subscripted [`stack ?`](https://uiua.org/docs/stack)
  - Sequential `?`s, which formatted to [`trace ⸮`](https://uiua.org/docs/trace)s, now format to subscripted [`stack ?`](https://uiua.org/docs/stack)
- Stabilize [`tuples ⧅`](https://uiua.org/docs/tuples)
- Stabilize [`sort ⍆`](https://uiua.org/docs/sort)
  - Sorting is a very common operation, and it's useful to have such simple access to it
- Stabilize [`last ⊣`](https://uiua.org/docs/last)
  - Getting the last row is a very common operation
- Stabilize [`case ⍩`](https://uiua.org/docs/case)
- Add the [`path`](https://uiua.org/docs/path) modifier, which finds shortest paths
  - Replaces and deprecates [`astar`](https://uiua.org/docs/astar)
  - `astar`'s functionality is still available via [`path`](https://uiua.org/docs/path) with a function pack
- Add the `A₁`, `A₂`, `A₃`, `C₂`, `C₃`, and `E₃` constants which are various kinds of adjacency offsets
  - They are great for working with [`path`](https://uiua.org/docs/path)
- Declared signatures that do not match the inferred signature will now cause a warning rather than an error
  - The function is edited to make the signature correct
- [`orient ⤸`](https://uiua.org/docs/orient) can now use [`fill ⬚`](https://uiua.org/docs/fill) to fill in new dimensions
- [`un °`](https://uiua.org/docs/un) [`reduce /`](https://uiua.org/docs/reduce) (format string) now splits a string by a delimiter
- [`do ⍢`](https://uiua.org/docs/do) now runs its functions at least once, even if their signatures are invalid
  - This is helpful when initially setting up a loop
- [`un °`](https://uiua.org/docs/un) now works with [`repeat ⍥`](https://uiua.org/docs/repeat) without a repetition count
  - This counts the number of repetitions required to converge
- Add an `ε` shadowable constant for the machine epsilon
  - You can type it as `\\epsilon`
  - Other greek letters can also be typed this way
- Add [`&ep`](https://uiua.org/docs/&ep) and [`&epf`](https://uiua.org/docs/&epf) system functions for easier printing to stderr
- Functions that work with audio such as [`audio`](https://uiua.org/docs/audio) now treat the first axis as samples and the second axis as channels
- Add [`# Deprecated!`](https://www.uiua.org/tutorial/documentation#deprecated) semantic comments
- Change [`backward 𝄈`](https://uiua.org/docs/backward)'s glyph to `𝄈`. Code using `˜` will continue to work and will be formatted as `𝄈`.
- Add the experimental [`or ∨`](https://uiua.org/docs/or) function
  - It has a useful reduction identity
  - It is also GCD
- Add experimental [inline macros](https://www.uiua.org/docs/experimental#inline-macros)
  - Deprecate the experimental `stringify` and `signature` modifiers in favor of inline code macros
- Add experimental [`binary`](https://uiua.org/docs/binary) function, which encodes and decodes arrays into a compact binary representation
- Add experimental [`&b`](https://uiua.org/docs/breakpoint) function, which pauses execution and prints the stack
- Remove the previously deprecated `member ∊` function
  - As planned, [`memberof ∊`](https://uiua.org/docs/memberof)'s glyph has been changed, and the old one will format to `∊`.
- Remove the previously deprecated experimental `⟔ coordinate` function
- Remove the previously deprecated experimental `struct` modifier
- Remove the previously deprecated `setinv` and `setund` modifiers
- Remove the previously deprecated `choose` and `permute` functions
- Remove the previously deprecated experimental `◹ triangle` modifier
- Remove the previously deprecated experimental `⑄ chunks` function
### Interpreter
- The compiler and interpreter have been almost entirely rewritten to use a tree-based execution model rather than a bytecode model
  - This massively simplifies compilation as well as optimizations and the derivation of inverses
  - This should not affect any language semantics
- Improve pattern matching error messages
- Optimize the "root" pattern `ⁿ%:1`
- Optimize format strings applied to strings or boxed strings
- Optimize common [`partition ⊜`](https://uiua.org/docs/partition) patterns
- Add an `-e`/`--experimental` flag to the `uiua eval` command to enable experimental features
- Add the `uiua check` command, which checks that Uiua files compile
- More system functions are now run for output comments
- Improve formatting of more complex arrays with format strings and [`&p`](https://uiua.org/docs/&p)
### Website
- Add a new pad setting to show line values to the right of the code
- Add [Subscripts](https://uiua.org/docs/subscripts) page
- Update [More Stack Manipulation](https://uiua.org/tutorial/morestack) and [More Array Manipulation](https://uiua.org/tutorial/advancedarray) tutorials to include subscripts
  - Change their titles from "Advanced" to "More"
- Add new [Idioms](https://uiua.org/docs/idioms) page

## 0.13.0 - 2024-10-21
You can find the release announcement [here](https://uiua.org/blog/uiua-0.13.0).
### Language
- **Breaking Change** - [`minimum ↧`](https://uiua.org/docs/minimum) and [`maximum ↥`](https://uiua.org/docs/maximum) now compare boxes lexicographically
  - This makes them consistent with comparison functions like [`less than <`](https://uiua.org/docs/less%20than)
- **Breaking Change** - [`gen`](https://uiua.org/docs/gen) now takes a shape argument and no longer outputs a new seed
- **Breaking Change** - [`&clip`](https://uiua.org/docs/&clip) now unifies `&clget` and `&clset`
- **Breaking Change** - [`type`](https://uiua.org/docs/type) numbers are now:
  - `0` for normal numbers
  - `1` for characters
  - `2` for boxes
  - `3` for complex numbers
  - This orders the types more or less as you're likely to need to handle them
  - In the future, if another number type is added, it can go at the end next to complex numbers
- The experimental `chunks ⑄` function's behavior has been moved to an extension on [`windows ◫`](https://uiua.org/docs/windows)
  - This also allows the specification of "stride"
  - This is inspired by APL's `stencil ⌺` behavior
  - `chunks ⑄` has been deprecated
- Stabilize [`orient ⤸`](https://uiua.org/docs/orient)
  - Change its glyph to something with broader font support
  - `⮌` will continue to work and will be formatted as `⤸`
- Add [`anti ⌝`](https://uiua.org/docs/anti) modifier, which simplifies choosing certain inverses
  - Add related new [`anti ⌝`](https://uiua.org/docs/anti) inverses for [`drop ↘`](https://uiua.org/docs/drop), [`select ⊏`](https://uiua.org/docs/select), and [`pick ⊡`](https://uiua.org/docs/pick) (though many more already existed)
- Add [`obverse ⌅`](https://uiua.org/docs/obverse) modifier, which unifies the specification of inverses
  - This unifies, replaces, and extends [`setinv`](https://uiua.org/docs/setinv) and [`setund`](https://uiua.org/docs/setund)
  - Deprecate [`setinv`](https://uiua.org/docs/setinv) and [`setund`](https://uiua.org/docs/setund)
- [`un °`](https://uiua.org/docs/un) [`by ⊸`](https://uiua.org/docs/by) can now be used to access the "undo" part of a function's [`under ⍜`](https://uiua.org/docs/under) functionality
- Stabilize [`below ◡`](https://uiua.org/docs/below)
- Rename `⤙ but` and `⤚ with` to [`with ⤙`](https://uiua.org/docs/with) and [`off ⤚`](https://uiua.org/docs/off)
  - [`with ⤙`](https://uiua.org/docs/with) has been stabilized
- "Stack macros" are now called "index macros"
  - Stabilize existing placeholder indexing syntax (`^0`, `^1`, etc.)
  - Deprecate existing stack-based macro placeholders (`^!`, `^.`, etc.)
- [`assert ⍤`](https://uiua.org/docs/assert) at the beginning of a line is now interpreted as a test in some contexts
  - See the updated [Testing Tutorial](https://uiua.org/tutorial/testing) for more information
- [`parse ⋕`](https://uiua.org/docs/parse) now parses complex values from both `arbi`and `a+bi` formats
  - [`un °`](https://uiua.org/docs/un)[`parse ⋕`](https://uiua.org/docs/parse)'s return value uses the `arbi` format
- [`un °`](https://uiua.org/docs/un) [`orient ⤸`](https://uiua.org/docs/orient) is now equivalent to [`range ⇡`](https://uiua.org/docs/range) [`length ⧻`](https://uiua.org/docs/length) [`shape △`](https://uiua.org/docs/shape) [`duplicate .`](https://uiua.org/docs/duplicate)
  - This is analogous to the behavior of [`un °`](https://uiua.org/docs/un) [`select ⊏`](https://uiua.org/docs/select)
- Add a feature to some [`under ⍜`](https://uiua.org/docs/under) functions that makes them more permissive of changes in rank
  - Applies to [`select ⊏`](https://uiua.org/docs/select), [`keep ▽`](https://uiua.org/docs/keep), [`first ⊢`](https://uiua.org/docs/first), [`first ⊢`](https://uiua.org/docs/first)[`reverse ⇌`](https://uiua.org/docs/reverse)
  - The value that is "put back" can have its rank changed
  - Lower rank arrays get repeated to match the original shape
  - Higher rank arrays extend the length of the array
- [`under ⍜`](https://uiua.org/docs/under) [`un °`](https://uiua.org/docs/un) [`bits ⋯`](https://uiua.org/docs/bits) now preserves a minimum bit length
- [`under ⍜`](https://uiua.org/docs/under) [`reverse ⇌`](https://uiua.org/docs/reverse), [`transpose ⍉`](https://uiua.org/docs/transpose), and [`rotate ↻`](https://uiua.org/docs/rotate) are now smarter with respect to the signature of [`under ⍜`](https://uiua.org/docs/under)'s second function
  - For example `⍜⇌°⊂` behaves in a more useful way
- [`under ⍜`](https://uiua.org/docs/under) [`length ⧻`](https://uiua.org/docs/len) now reshapes an array to have the transformed length
- `NaN` and `∞` indices for [`pick ⊡`](https://uiua.org/docs/pick) and [`select ⊏`](https://uiua.org/docs/select) now get the [`fill ⬚`](https://uiua.org/docs/fill) value
- [`scan \\`](https://uiua.org/docs/scan) now accepts functions that take more than 2 arguments
  - The behavior is similar to the analogous behavior for [`reduce /`](https://uiua.org/docs/reduce)
- Allow [`on ⟜`](https://uiua.org/docs/on) to use function packs
- Allow [`under ⍜`](https://uiua.org/docs/under) of scalar [`keep ▽`](https://uiua.org/docs/keep)
- Add the [`graphemes`](https://uiua.org/docs/graphemes) function, which splits a string into unicode grapheme clusters
- Add the [`&fmd`](https://uiua.org/docs/&fmd) system function, which creates a directory
- Very large arrays are now displayed more concisely in output
- Change and enhance the behavior of `;` and `;;`
  - You can read about the new behavior [here](https://uiua.org/tutorial/codetactility#line-manipulation)
- Add unicode escape sequences for entering arbitrary characters
  - A double backslash followed by a unicode hex number will format to the corresponding glyph
- Add the [`timezone`](https://uiua.org/docs/timezone) function, which gets the local timezone offset in hours
- Scoped modules are now delimited by `┌─╴` and `└─╴`
  - These both format from the existing `---` delimiters
- Add a `Lorem` constant, which contains the Lorem Ipsum text
- Add several color constants
- Add pride flag constants
- Add `Cats` image constant
- Allow [`&raw`](https://uiua.org/docs/&raw) to be inverted, acting as a getter for the raw state of the terminal
  - [`under ⍜`](https://uiua.org/docs/under)[`&raw`](https://uiua.org/docs/&raw) sets raw mode, and then returns it to the previous state
- Experimentally allow [`fold ∧`](https://uiua.org/docs/fold) to work with any signature
  - Excess values are collected into arrays
- Add the experimental [`around ’`](https://uiua.org/docs/around) function, which duplicates the top value on the stack to the third-to-top position
- Add experimental [`tuples ⧅`](https://uiua.org/docs/tuples) modifier
  - This unifies and extends the behavior of the `choose` and `permute` functions, which are now deprecated
  - This also replaces and deprecates the `triangle` modifier
- Add experimental [`base`](https://uiua.org/docs/base) function
  - This converts to and from base-N digits
- Add an experimental [`sort ⍆`](https://uiua.org/docs/sort) function
  - Sorting is very common
  - [`un °`](https://uiua.org/docs/un) [`sort ⍆`](https://uiua.org/docs/sort) shuffles an array
- Add an experimental [`last ⊣`](https://uiua.org/docs/last) function
  - Getting the last row of an array is very common
- Add experimental subscript modifiers
  - They modify the behavior of some functions and modifiers
  - You can read more about them [here](https://uiua.org/docs/subscripts)
- Add experimental [data definitions](https://uiua.org/docs/experimental#data-definitions)
  - These allow for structured data similar to `struct`s and `enum`s in other languages
  - They also allow a limited form of namable function arguments
  - The experimental [`struct`](https://uiua.org/docs/struct) modifier/macro has been deprecated in favor of data definitions
- Add the experimental [`layout`](https://uiua.org/docs/layout) function, which renders text into an image array
- [`astar`](https://uiua.org/docs/astar) no longer errors if no paths are found
- Remove previously deprecated function strands
### Interpreter
- Add the `uiua find` command, which finds Uiua code that matches the given unformatted text
- Add the `uiua doc` command, which shows the documentation for a function or modifier
- Add checking for end-of-line signature comments
  - These are documented in the [Documenting Code](https://uiua.org/tutorial/documentation) tutorial
- Add `webp` support to [`img`](https://uiua.org/docs/img)
- Some optimizations
  - Pervasive function machinery has been totally rewritten
    - Observed performance improvements of up to 12x
  - [`fill ⬚`](https://uiua.org/docs/fill)ed array creation has been totally rewritten
    - Observed performance improvements:
      - ~1.2x in the best case
      - ~5x in the average case
      - ~450x in the worst case
  - [`rows ≡`](https://uiua.org/docs/rows) [`on ⟜`](https://uiua.org/docs/on)/[`by ⊸`](https://uiua.org/docs/by) [`random ⚂`](https://uiua.org/docs/random)/`constant`
  - [`memberof ∊`](https://uiua.org/docs/memberof)[`range ⇡`](https://uiua.org/docs/range) for scalar inputs to [`range ⇡`](https://uiua.org/docs/range)
  - [`memberof ∊`](https://uiua.org/docs/memberof)[`rerank ☇`](https://uiua.org/docs/rerank)`1`[`range ⇡`](https://uiua.org/docs/range) for rank 1 inputs to [`range ⇡`](https://uiua.org/docs/range)
  - [`first ⊢`](https://uiua.org/docs/first)[`un °`](https://uiua.org/docs/un)[`sort ⍆`](https://uiua.org/docs/sort) to just pick an element
- Tweak the formatter to reduce excess vertical space
- The formatter now aligns consecutive single-line bindings
- `uiua repl` now has a `-s/--stack` flag to disable clearing the stack after each line
  - Clearing the stack is now the default
  - The `-c/--clear` has been removed
- After programs finish executing, the terminal raw mode will be automatically disabled if it was left on. 
### Website
- Add [Ranges](https://uiua.org/tutorial/ranges) tutorial
- Update the [Inverses](https://uiua.org/docs/inverses) tutorial with information about [`anti ⌝`](https://uiua.org/docs/anti) and [`obverse ⌅`](https://uiua.org/docs/obverse)
- Add an [RSS Feed](https://uiua.org/blog/feed.rss) for the blog
- Update the [Testing Tutorial](https://uiua.org/tutorial/testing)
- In the pad, files created with [`&fwa`](https://uiua.org/docs/&fwa) will now persist between runs
- Add the option to decouple running and formatting in the pad
- Add a dropdown to the pad to insert named functions
- Add buttons to the pad to download code and copy a markdown link
- Git import URLs in the pad can now be Ctrl+clicked to open in a new tab
- Improve the [Constants](https://uiua.org/docs/constants) page

## 0.12.3 - 2024-08-17
### Language
- Rename `imen`, `gifen`, and `auden` to [`img`](https://uiua.org/docs/img), [`gif`](https://uiua.org/docs/gif), and [`audio`](https://uiua.org/docs/audio) to match the naming convention of other functions
  - The existing names will continue to work and will be formatted to the new names

## 0.12.2 - 2024-08-16
### Interpreter
- Fix a bug in formatting `¯0`
- Fix a bug with uasm serialization of non-standard numbers

## 0.12.1 - 2024-08-16
### Interpreter
- Use custom version of `nokhwa` crate so that [`&camcap`](https://uiua.org/docs/&camcap) can build

## 0.12.0 - 2024-08-16
You can find the release announcement [here](https://uiua.org/blog/uiua-0.12.0).
### Language
- **Breaking Change** - [`&runs`](https://uiua.org/docs/&runs) now returns 3 handles rather than 1
  - This gives more precise control over stdin, stdout, and stderr
- **Breaking Change** - [`un °`](https://uiua.org/docs/un) [`select ⊏`](https://uiua.org/docs/select) is now equivalent to [`range ⇡`](https://uiua.org/docs/range) [`length ⧻`](https://uiua.org/docs/length) [`duplicate .`](https://uiua.org/docs/duplicate)
- **Breaking Change** - [`auden`](https://uiua.org/docs/audio) now takes a sample rate argument, and [`un °`](https://uiua.org/docs/un)[`auden`](https://uiua.org/docs/audio) now returns the sample rate
  - This allows you to work with audio that is not at Uiua's default sample rate
- **Breaking Change** - [`fill ⬚`](https://uiua.org/docs/fill) no longer fills the shapes of inputs to [`rows ≡`](https://uiua.org/docs/rows)
  - It could lead to some unexpected behavior when trying to fill the shapes of outputs
- **Breaking Change** - disable pattern matching inverses resulting from [`under ⍜`](https://uiua.org/docs/under)
  - This is basically never what you want
  - Pattern matching via [`un °`](https://uiua.org/docs/un) still works as normal
- Deprecate `member ∊` and add [`memberof ∊`](https://uiua.org/docs/memberof)
  - `member ∊` almost always required flipping the arguments
  - [`memberof ∊`](https://uiua.org/docs/memberof) is the same, but with the arguments flipped
  - In the future, `member ∊` will be removed, and [`memberof ∊`](https://uiua.org/docs/memberof)'s glyph will be changed and format to `∊`
- Rename media en/decoding system functions `&ime`, `&gife`, and `&ae` to non-system functions [`imen`](https://uiua.org/docs/img), [`gifen`](https://uiua.org/docs/gif), and [`auden`](https://uiua.org/docs/audio)
  - Their implementation is not actually system-dependent
  - Existing `&*e` will format to the new names
  - The previously deprecated `&*d` decoding functions have been removed (use [`un °`](https://uiua.org/docs/un) on the encoding function)
- Switch functions have been replaced with a [`switch ⨬`](https://uiua.org/docs/switch) modifier
  - This makes the language more uniform
  - Code with existing switch functions in `⟨⟩`s will continue to work and will format to use `⨬` and `()`s
- Identifiers can now contain subscript numbers
  - They format from `__` followed by some numbers
  - For example, `X__1` will format to `X₁`
- The `utf` function has been renamed to [`utf₈`](https://uiua.org/docs/utf₈)
- Stabilize labels
  - You can read about them in the new [Code Tactility Tutorial](https://uiua.org/tutorial/codetactility#labels)
- [`windows ◫`](https://uiua.org/docs/windows) can now use a [`fill ⬚`](https://uiua.org/docs/fill) value to pad the array
  - This is useful for convolutions
- Add new [Scoped Modules](https://uiua.org/tutorial/modules#scoped-modules)
  - These allow you to create a module without creating a file
- A `Call` or `New` function inside a module can now be called via the module's name
- A module name used as a macro with `!` now imports all names from the module into the macro's function's scope
  - This can shorten code when accessing many items from the same module
- Add the [`datetime`](https://uiua.org/docs/datetime) function, which splits a time into its date and time components
- [`un °`](https://uiua.org/docs/un) [`shape △`](https://uiua.org/docs/shape) now generates an array with the given shape and incrementing elements
- [`un °`](https://uiua.org/docs/un) [`pick ⊡`](https://uiua.org/docs/pick) is now equivalent to [`range ⇡`](https://uiua.org/docs/range) [`shape △`](https://uiua.org/docs/shape) [`duplicate .`](https://uiua.org/docs/duplicate)
- [`keep ▽`](https://uiua.org/docs/keep) will now cycle counts if the counts array is shorter than the counted array
- [`keep ▽`](https://uiua.org/docs/keep) now works with non-integer scalar counts to scale an array
- [`under ⍜`](https://uiua.org/docs/under) [`keep ▽`](https://uiua.org/docs/keep) now allows increasing the kept array's rank
- [`under ⍜`](https://uiua.org/docs/under) now works with common [`by ⊸`](https://uiua.org/docs/by) patterns
- [`join ⊂`](https://uiua.org/docs/join) with rank differences greater than 1 can now extend the smaller array
- [`couple ⊟`](https://uiua.org/docs/couple) with different ranks can now extend the smaller array
- [`indexof ⊗`](https://uiua.org/docs/indexof) now works with [`fill ⬚`](https://uiua.org/docs/fill) to set the default for when a value is not found
- [`un °`](https://uiua.org/docs/un) [`join ⊂`](https://uiua.org/docs/join) is now easier to combine with other inverses
- [`repeat ⍥`](https://uiua.org/docs/repeat) can now repeat a negative number of times, which will repeat the inverse
- [`un °`](https://uiua.org/docs/un) [`repeat ⍥`](https://uiua.org/docs/repeat) now requires the repetition count to be inside the [`un °`](https://uiua.org/docs/un) function
  - This makes the inverted signature correct
- [`inventory ⍚`](https://uiua.org/docs/inventory) no longer does [`each ∵`](https://uiua.org/docs/each)-like behavior
  - This got in the way more than it helped
- Non-scalar [`switch ⨬`](https://uiua.org/docs/switch) and [`repeat ⍥`](https://uiua.org/docs/repeat) now follow the same distribution and [`fix ¤`](https://uiua.org/docs/fix) rules as [`rows ≡`](https://uiua.org/docs/rows)
- Add the `# Track caller!` semantic comment, which prevents stack traces from going below the function that contains it
- Add the experimental [`chunks ⑄`](https://uiua.org/docs/chunks) function, which splits an array into chunks of a given size
- Add the experimental [`choose`](https://uiua.org/docs/choose) and [`permute`](https://uiua.org/docs/permute) functions for combinatorics solutions
- Add the experimental [`triangle ◹`](https://uiua.org/docs/triangle) modifier, which calls a function on shrinking suffixes of an array's rows
- Add the experimental [`orient`](https://uiua.org/docs/orient) function, which arranges an array's axes in a specified order
- Add the experimental [`fft`](https://uiua.org/docs/fft) function, which performs the Fast Fourier transform
  - The inverse FFT is also supported via [`un °`](https://uiua.org/docs/un)
- Add the experimental [`astar`](https://uiua.org/docs/astar) modifier, which performs the A* pathfinding algorithm
- Add the experimental [`but ⤙`](https://uiua.org/docs/but) and [`with ⤚`](https://uiua.org/docs/with) modifiers, which are compliments to [`on ⟜`](https://uiua.org/docs/on) and [`by ⊸`](https://uiua.org/docs/by)
- Add the experimental [`above ◠`](https://uiua.org/docs/above) and [`below ◡`](https://uiua.org/docs/below) modifiers, which keep all arguments to a function above or below the outputs on the stack
- Add the experimental [`struct`](https://uiua.org/docs/struct) macro, which generates constructor and getter functions given some names
- Un-deprecate [`dip ⊙`](https://uiua.org/docs/dip) function packs
- Deprecate the experimental [`coordinate ⟔`](https://uiua.org/docs/coordinate) function, as it is seldom needed and easy to implement with other functions
  - It can be mostly replaced with `⊢⊚⌕`
- Deprecate experimental stack and array swizzles
  - They don't fit my vision for the language
- Deprecate implicit GitHub domain in `"git: ..."` modules
  - GitHub should not be a default
- Adjacent [`trace ⸮`](https://uiua.org/docs/trace)s now function as a single [`trace ⸮`](https://uiua.org/docs/trace) of more values
- N+1 adjacent [`stack ?`](https://uiua.org/docs/stack)s now format to N [`trace ⸮`](https://uiua.org/docs/trace)s
- Add the [`&camcap`](https://uiua.org/docs/&camcap) system function, which captures a frame from a camera
### Interpreter
- A ton of bug and crash fixes
- Some optimizations
  - Optimize [`length ⧻`](https://uiua.org/docs/length)[`where ⊚`](https://uiua.org/docs/where)
  - Optimize [`≡`](https://uiua.org/docs/rows)[`□`](https://uiua.org/docs/box)[`◫`](https://uiua.org/docs/windows)
  - Small optimizations to rank-1 cases of [`find ⌕`](https://uiua.org/docs/find) and [`mask ⦷`](https://uiua.org/docs/mask)
  - Optimize [`group ⊕`](https://uiua.org/docs/group) and [`partition ⊜`](https://uiua.org/docs/partition) with [`⋅`](https://uiua.org/docs/gap)[`identity ∘`](https://uiua.org/docs/identity) and [`dip ⊙`](https://uiua.org/docs/dip)[`pop ◌`](https://uiua.org/docs/pop)
  - Optimize [`rows ≡`](https://uiua.org/docs/rows) [`first ⊢`](https://uiua.org/docs/first)
- Git modules are now cloned instead of being added as submodules
  - Add the `uiua module` command to list or update cloned modules
- Add some media constants:
  - `Logo` - the Uiua logo
  - `Lena` - a good example image for image processing
  - `Music` - a snippet of musical audio
- LSP improvements
  - Show array shapes on hover
  - Code action to insert `# Experimental!` comment
- REPL improvements
  - The stack is now preserved between REPL lines
    - Pass the `-c`/`--clear` flag to clear it automatically after each line
  - Add some commands, which can be listed by typing `help` in the repl
- Allow Uiua-specific tokens types to be disabled in the LSP
### Website
- Add new [Tacit Code Tutorial](https://uiua.org/tutorial/tacitcode)
- Add new [Code Tactility Tutorial](https://uiua.org/tutorial/codetactility)
- Fix a bug where running pad code that has end-of-line comments via ctrl+Enter would cause the cursor to move every time
- Remove the Uiuisms page
  - It provided a "definitive" implementation of many algorithms where the best implementation is subjective and/or context-dependent
  - It is not how I want people to approach learning Uiua
  - It is not something I am interested in maintaining

## 0.11.1 - 2024-06-06
### Interpreter
- Some bug and crash fixes

## 0.11.0 - 2024-06-02
You can find the release announcement [here](https://uiua.org/blog/uiua-0.11.0).
### Language
- **Breaking Change** - [`un °`](https://uiua.org/docs/un) [`fix ¤`](https://uiua.org/docs/fix) now does pattern matching
  - [`under ⍜`](https://uiua.org/docs/under) [`fix ¤`](https://uiua.org/docs/fix) retains the old behavior
  - Collapsing the top two dimensions of an array can still be done with [`reduce /`](https://uiua.org/docs/reduce)[`join ⊂`](https://uiua.org/docs/join)
- **Breaking Change** - [`un °`](https://uiua.org/docs/un) [`on ⟜`](https://uiua.org/docs/on) now does pattern matching in some cases
  - This makes its behavior conform to the rule that a function's inverse must have the opposite signature
  - Some other cases, such as `°⟜+`, now properly invert the function
- **Breaking Change** - [`keep ▽`](https://uiua.org/docs/keep) with a scalar counts array now copies each row that many times
  - This matches the behaviors of APL and BQN, and is generally more useful
  - The old behavior can be achieved with `/⊂↯`
- **Breaking Change** - [`fill ⬚`](https://uiua.org/docs/fill) values are no longer accessible through function calls
  - This makes it harder to accidentally use a fill value without meaning to
  - This does not affect [`un °`](https://uiua.org/docs/un)[`pop ◌`](https://uiua.org/docs/pop)
  - This can be circumvented with [`fill ⬚`](https://uiua.org/docs/fill)[`un °`](https://uiua.org/docs/un)[`pop ◌`](https://uiua.org/docs/pop)
- Stabilize [`by ⊸`](https://uiua.org/docs/by)!
- Stabilize [`repr`](https://uiua.org/docs/repr)
- Add experimental **stack swizzles**, which allow for more flexible stack reordering
  - Swizzles are written with a `λ` followed by a list of letters
  - Capital letters [`fix ¤`](https://uiua.org/docs/fix) the corresponding value
  - The `λ` formats from `'` when it is in front of the letters
- Add experimental **array swizzles**, which allow extracting rows from an array in a concise way
  - Swizzles are written with a `⋊` followed by a list of letters
  - Letters up to `m` start from the first row, Letters back from `z` start from the last row
  - Capital letters [`un °`](https://uiua.org/docs/un) [`box □`](https://uiua.org/docs/box) the corresponding value
  - The `⋊` formats from `''` when it is in front of the letters
- [`keep ▽`](https://uiua.org/docs/keep) now works with [`un °`](https://uiua.org/docs/un)
  - It splits an array into counts and an adjacent deduplication
- Add the [`json`](https://uiua.org/docs/json) function, which encodes and decodes JSON strings
- Add the [`xlsx`](https://uiua.org/docs/xlsx) function, which encodes and decodes XLSX data
- [`bits ⋯`](https://uiua.org/docs/bits) can now take negative numbers
- [`un °`](https://uiua.org/docs/un) [`bits ⋯`](https://uiua.org/docs/bits) can now take non-booleans
- Add [`un °`](https://uiua.org/docs/un) [`duplicate .`](https://uiua.org/docs/duplicate) pattern matching
- Add [`un °`](https://uiua.org/docs/un) [`min ↧`](https://uiua.org/docs/min) and [`un °`](https://uiua.org/docs/un) [`max ↥`](https://uiua.org/docs/max) pattern matching
- [`insert`](https://uiua.org/docs/insert) can now be used with [`un °`](https://uiua.org/docs/un) to extract a map entry and pattern match it
- [`fill ⬚`](https://uiua.org/docs/fill)ed [`keep ▽`](https://uiua.org/docs/keep)'s fill value may now be a list
- [`infinity ∞`](https://uiua.org/docs/infinity) can now be passed in a list to [`take ↙`](https://uiua.org/docs/take) or [`drop ↘`](https://uiua.org/docs/drop) to take/drop every row along an axis
- [`reduce /`](https://uiua.org/docs/reduce) can now take a function with more than 2 arguments
  - Each additional argument increases the number of arguments passed to [`reduce /`](https://uiua.org/docs/reduce) by 1
  - Additional arguments are passed to the function on every iteration
- [`try ⍣`](https://uiua.org/docs/try) signature checking is now more permissive
- Switch function signature checking is now more permissive with branches that have an [`assert ⍤`](https://uiua.org/docs/assert) that always triggers
- [`first ⊢`](https://uiua.org/docs/first), [`first ⊢`](https://uiua.org/docs/first) [`reverse ⇌`](https://uiua.org/docs/reverse), [`rise ⍏`](https://uiua.org/docs/rise), and [`fall ⍖`](https://uiua.org/docs/fall) can now be used on scalars
- Add the [`&exit`](https://uiua.org/docs/&exit) system function, which exits the program with a status code
- Add the experimental [`&memcpy`](https://uiua.org/docs/&memcpy) system function, which copies the data from [`&ffi`](https://uiua.org/docs/&ffi) pointers to an array
- Add the experimental [`&memfree`](https://uiua.org/docs/&memfree) system function, which frees memory allocated by [`&ffi`](https://uiua.org/docs/&ffi) functions
- Add [`&tlsc`](https://uiua.org/docs/&tlsc) and [`&tlsl`](https://uiua.org/docs/&tlsl) system functions, which allow making TLS connections
  - [`&tlsc`](https://uiua.org/docs/&tlsc) replaces [`&httpsw`](https://uiua.org/docs/&httpsw), which is now deprecated
  - [`&tlsl`](https://uiua.org/docs/&tlsl) is currently experimental and mostly untested
- [`&rs`](https://uiua.org/docs/&rs) will now attempt to read additional bytes to resolve a UTF-8 character
- [`&gife`](https://uiua.org/docs/gif) and [`&gifs`](https://uiua.org/docs/&gifs) now support binary transparency
- Signatures can now be specified in stack array notation immediately after a `[` or `{`
- Change how long decimal numbers are formatted
  - Sequences of repeated digits are now replaced with `…`
- Add argument documentation comments
  - You can read more about this in the new [Documenting Code](https://uiua.org/tutorial/documentation) tutorial
- Add some useful shadowable constants for working with compile-time file paths
  - `ThisFile` - The relative path of the current source file
  - `ThisFileName` - The name of the current source file
  - `ThisFileDir` - The directory of the current source file
  - `WorkingDir` - The compile-time working directory
- Deprecate function strands
  - They ended up making code less readable
- Remove previously deprecated primitives:
  - `this ↬`
  - `recur ↫`
  - `all ⋔`
  - `cascade ⪾`
  - `bind λ`
  - `types`
  - `shapes`
- Remove several backward compatible glyphs from the lexer
  - This frees up some glyphs that can now be used as function names
### Interpreter
- Lots of optimizations
  - Optimize the pattern `/F⊞F` to use much less memory and be an order of magnitude faster in some cases
  - Optimize [`group ⊕`](https://uiua.org/docs/group) and [`partition ⊜`](https://uiua.org/docs/partition) with [`length ⧻`](https://uiua.org/docs/length), [`first ⊢`](https://uiua.org/docs/first), and [`first ⊢`](https://uiua.org/docs/first)[`reverse ⇌`](https://uiua.org/docs/reverse)
  - Optimize iterating modifiers with a top-level [`fork ⊃`](https://uiua.org/docs/fork) or [`bracket ⊓`](https://uiua.org/docs/bracket)
  - Optimize [`≡`](https://uiua.org/docs/rows)[`⋅`](https://uiua.org/docs/gap)`constant` and [`∵`](https://uiua.org/docs/each)[`⋅`](https://uiua.org/docs/gap)`constant` patterns
    - The speed improvement is on the order of 70x
  - Optimize the pattern `[⍥⚂…`
  - Optimize [`length ⧻`](https://uiua.org/docs/length) [`deduplicate ◴`](https://uiua.org/docs/deduplicate)
- [`setinv`](https://uiua.org/docs/setinv) now emits a warning if the functions do not have opposite signatures
- Add the `--io` flag to the `uiua fmt` command, which formats code from stdin to stdout
- Lots of bug and crash fixes
### Website
- Add a [Documenting Code](https://uiua.org/tutorial/documentation) tutorial
- Add a [Files and Streams](https://uiua.org/tutorial/filesandstreams) tutorial
- Add an [Experimental Features](https://uiua.org/docs/experimental) page
- Rewrite the pad code to use a `<textarea>` rather than a `contenteditable` `<div>`
  - This should make it work better in more browsers, especially on mobile
- You can now ctrl+click on a glyph in the editor to open its documentation

## 0.10.3 - 2024-04-09
### Interpreter
- Fix a crash involving pervasive operations on some 0-length arrays

## 0.10.2 - 2024-04-08
### Interpreter
- Fix a bug in [`repeat ⍥`](https://uiua.org/docs/repeat) with [`infinity ∞`](https://uiua.org/docs/infinity) signature inference

## 0.10.1 - 2024-04-07
### Interpreter
- [`under ⍜`](https://uiua.org/docs/under) of pattern matching now works correctly
- [`under ⍜`](https://uiua.org/docs/under) [`un °`](https://uiua.org/docs/un) [`scan \\`](https://uiua.org/docs/scan) now works correctly
- Style and advice diagnostics are no longer emitted from macros

## 0.10.0 - 2024-04-04
You can find the release announcement [here](https://uiua.org/blog/uiua-0.10.0).
### Language
- **Breaking Change** - Multiline strings are now also *raw strings* which do not require escaping
  - They are no longer format strings by default
  - Raw strings can be made format strings with an extra `$`, i.e. `$$ …`
- **Breaking Change** - [`try ⍣`](https://uiua.org/docs/try)'s handler function is now passed the original arguments *before* the error
- [`try ⍣`](https://uiua.org/docs/try) now works with function packs of more than 2 functions
  - This tries each function in the pack in order
- Switch functions now format to use `⟨⟩` brackets
  - This makes them easier to identify when reading
  - It also allows switch functions to be used as modifier arguments without extra nesting
- Switch functions now work with [`under ⍜`](https://uiua.org/docs/under)
- Add pattern matching with [`un °`](https://uiua.org/docs/un)
  - Constant values can now be inverted to form a function which errors if the top value on the stack does not match
  - Format strings can be inverted to extract substrings
  - Read more in the new [Pattern Matching](https://uiua.org/tutorial/patternmatching) tutorial
- Git modules are no longer experimental
  - Modules are added automatically as Git submodules when imported
  - See the [Modules](https://uiua.org/tutorial/modules#git-modules) tutorial for more information
- [`map`](https://uiua.org/docs/map) and related functions [`insert`](https://uiua.org/docs/insert), [`has`](https://uiua.org/docs/has), [`get`](https://uiua.org/docs/get), and [`remove`](https://uiua.org/docs/remove) are no longer experimental
- Add the [`mask ⦷`](https://uiua.org/docs/mask) function, which creates a mask of occurrences of one array in another
  - This works similarly to [`find ⌕`](https://uiua.org/docs/find), but is better when you need a mask or to distinguish between adjacent occurrences
- Change [`sine ∿`](https://uiua.org/docs/sine)'s glyph
  - `∿` is more representative of what it does
  - Most circle glyphs like `○` are used for array functions or stack manipulation
  - `○` will continue to work and will be formatted as `∿`
- [`under ⍜`](https://uiua.org/docs/under) [`join ⊂`](https://uiua.org/docs/join) now works with arrays of the same rank as long as the row count does not change
- [`un °`](https://uiua.org/docs/un) [`scan \\`](https://uiua.org/docs/scan) now works with [`equals =`](https://uiua.org/docs/equals) and [`not equals ≠`](https://www.uiua.org/docs/not%20equals)
- [`group ⊕`](https://uiua.org/docs/group) can now take multidimensional index arrays
- [`partition ⊜`](https://uiua.org/docs/partition) can now take multidimensional marker arrays
- [`under ⍜`](https://uiua.org/docs/under) [`select ⊏`](https://uiua.org/docs/select) and [`pick ⊡`](https://uiua.org/docs/pick) now work with duplicate indices if the values at those indices are the same
- [`rotate ↻`](https://uiua.org/docs/rotate) now works through boxes
- [`fold ∧`](https://uiua.org/docs/fold) now works with [`under ⍜`](https://uiua.org/docs/under) if its function does
- [`inventory ⍚`](https://uiua.org/docs/inventory) can now take 3 or more arrays
- [`repeat ⍥`](https://uiua.org/docs/repeat) can now take non-scalar repetition counts
  - This repeats the function a different number of times for each row of the inputs
- [`select ⊏`](https://uiua.org/docs/select) can now be used with [`un °`](https://uiua.org/docs/un) to separate into [`classify ⊛`](https://uiua.org/docs/classify) and [`deduplicate ◴`](https://uiua.org/docs/deduplicate) (changed in 0.12.0)
- Characters can now be [`multiply ×`](https://uiua.org/docs/multiply)d or [`divide ÷`](https://uiua.org/docs/divide)d by numbers to possibly toggle their case
- Add the [`csv`](https://uiua.org/docs/csv) function, which encodes and decodes CSV data
- Add the [`&clget`](https://uiua.org/docs/&clget) and [`&clset`](https://uiua.org/docs/&clset) system functions, which allow copying and pasting text to and from the system clipboard
- Add more [shadowable constants](https://www.uiua.org/docs/constants)
- Importing modules that use the `# Experimental!` comment now requires the `# Experimental!` comment in the importing file
- Doc comments may now be placed at the end of single-line functions
- Non-alphabetic identifiers can now be suffixed with `!` to make macros
- Add `df`, `ddf`, etc shortcuts for [`dip ⊙`](https://uiua.org/docs/dip) [`fix ¤`](https://uiua.org/docs/fix)
- Existing macros are now called "stack macros" to distinguish them from the new "array macros"
  - Stack macros are now [hygienic](https://en.wikipedia.org/wiki/Hygienic_macro)
- Add array macros, which allow code to be generated and manipulated at compile time as strings
  - These are specified with a `^` immediately following a binding's arrow
  - They are documented in the [Macros](https://uiua.org/tutorial/macros) tutorial
- [`un °`](https://uiua.org/docs/un) [`pop ◌`](https://uiua.org/docs/pop) can now be used to retrieve the [`fill ⬚`](https://uiua.org/docs/fill) value
  - See more details in [`fill ⬚`](https://uiua.org/docs/fill)'s documentation
- Add the wildcard constant `W`, which matches any number, and `@\W`, which matches any character
  - These work well with [`match ≍`](https://uiua.org/docs/match), [`find ⌕`](https://uiua.org/docs/find), and [`mask ⦷`](https://uiua.org/docs/mask)
- Add the experimental [`coordinate ⟔`](https://uiua.org/docs/coordinate) function, which searches an array for a value and returns a multidimensional index
  - [`coordinate ⟔`](https://uiua.org/docs/coordinate) is to [`pick ⊡`](https://uiua.org/docs/pick) as [`indexof ⊗`](https://uiua.org/docs/indexof) is to [`select ⊏`](https://uiua.org/docs/select)
- Experimental function strands now use the `‿` character, which formats from `__`
- Add the experimental [`by ⊸`](https://uiua.org/docs/by) modifier, which duplicates a function's last argument before calling it
- Add the experimental [`quote`](https://uiua.org/docs/quote) modifier, which converts a string to code at compile time
  - This is useful in array macros
- Add `# No inline!` semantic comment, which prevents a function and its callers from being inlined
  - This enables better stack traces on errors
- Deprecate [`bind`](https://uiua.org/docs/bind)
  - It undermines the principles of the language
  - It makes certain optimizations impossible
  - [`fill ⬚`](https://uiua.org/docs/fill) and/or [`map`](https://uiua.org/docs/map) can be used to achieve similar effects
- Deprecate [`deal`](https://uiua.org/docs/deal)
  - It is rarely used and easy to express with other functions
- Deprecate experimental [`shapes`](https://uiua.org/docs/shapes) and [`types`](https://uiua.org/docs/types) modifiers in favor of pattern matching
- Remove `cross ⊠` for good
- Remove `unpack ⊐` for good
- Remove `rectify ⌅` for good
- Remove `&i` for good
- Make [`reduce /`](https://uiua.org/docs/reduce) with a monadic function a hard error
### Interpreter
- Code is now analyzed for purity
  - All pure top-level expressions will attempt to evaluate at compile time
  - All fragments of code that are pure and have a signature `|0.n` will be evaluated at compile time
- Add lots of LSP features
  - Find references
  - Rename is now cross-file
  - On-type formatting (can be toggled in settings)
  - Inlay hints (each can be toggled in settings)
    - Binding function signatures
    - Inline function signatures
    - Values of top-level expressions
  - Code actions
    - Macro expansion
    - Remove output comment
    - Convert between strand and array syntax
  - Completions
    - Shadowable constants
    - Module items when the module reference is partially typed
- Add the `--file <file>` option to the `uiua repl` command
  - This runs a file before starting the REPL
- Improve the supported binding type coverage of [`&ffi`](https://uiua.org/docs/&ffi)
- Add warnings for when a loop in an array may have a variable signature
- Various performance improvements
  - Optimize and multithread `⊞(/+×)`, which is a common component of matrix multiplication
- Lots of bug and crash fixes
### Website
- Tutorials
  - Add a [Working with Strings](https://uiua.org/tutorial/strings) tutorial
  - Add array macros to the [Macros](https://uiua.org/tutorial/macros) tutorial
  - Add the [Pattern Matching](https://uiua.org/tutorial/patternmatching) tutorial
- Add some modifier compatibility tables to documentation
  - [`un °`](https://uiua.org/docs/un)
  - [`under ⍜`](https://uiua.org/docs/under)
  - [`fill ⬚`](https://uiua.org/docs/fill)
- Hide experimental glyphs in the editor by default
  - They can be toggled on in the settings
- An `# Experimental!` comment can now be easily inserted via a settings button or with `Ctrl+E`
- Add horizontal scrolling to pad output
- Pad tabs are now given titles according to their contents
- The pad now renders strings that are SVG as images
- Add a pad setting for autoplaying audio

## 0.9.5 - 2024-02-28
### Interpreter
- Fix a crash in [`each ∵`](https://uiua.org/docs/each) of 3 or more arrays

## 0.9.4 - 2024-02-28
### Interpreter
- Fix a bug with filled multi-dimensional [`take ↙`](https://uiua.org/docs/take)
- Fix a crash in [`rows ≡`](https://uiua.org/docs/rows) of 3 or more arrays

## 0.9.3 - 2024-02-27
### Interpreter
- Fix a major bug with negative [`take ↙`](https://uiua.org/docs/take)

## 0.9.2 - 2024-02-25
### Interpreter
- Fix a bug involving patterns like `°°[…]`

## 0.9.1 - 2024-02-25
### Crate
- Improve Rust library API for getting bindings' values

## 0.9.0 - 2024-02-25
### Language
- **Breaking Change** - [`repeat ⍥`](https://uiua.org/docs/repeat) with [`infinity ∞`](https://uiua.org/docs/infinity) now does a fixed-point iteration rather than an infinite loop
  - You can still do an infinite loop with [`do ⍢`](https://uiua.org/docs/do)`(…)1`
- **Breaking Change** - [`reshape ↯`](https://uiua.org/docs/reshape) with a shape with negative dimensions now reverses that axis rather than acting as a "fill" value
  - The "fill" behavior can still be achieved by setting an axis to [`infinity ∞`](https://uiua.org/docs/infinity)
- **Breaking Change** - [`&ad`](https://uiua.org/docs/&ad) and [`&imd`](https://uiua.org/docs/&imd) now return an encoding format as a string in addition to the media data
  - They are also now deprecated in favor of using [`un °`](https://uiua.org/docs/un) with [`&ae`](https://uiua.org/docs/audio) or [`&ime`](https://uiua.org/docs/img)
- Overhaul the module system
  - Details can be found in the updated [Modules](https://uiua.org/tutorial/modules) tutorial
  - Deprecate [`&i`](https://uiua.org/docs/&i), as it is no longer necessary
  - Enabling experimental allows a module path of the form `git: <repo url>` to load a module from a git repository
- Custom modifiers are now called "macros"
  - Rather than requiring signatures, placeholders are now a sort of function that operates on the macro's arguments
  - This allows for more complex and flexible code-reuse
  - Existing code should continue to work. Existing placeholders will be formatted into the new syntax.
  - You can read more about macros in the updated [Macros](https://uiua.org/tutorial/macros) tutorial
- Add the [`on ⟜`](https://uiua.org/docs/on) modifier, which captures a common [`fork ⊃`](https://uiua.org/docs/fork) pattern in a more readable way
- [`join ⊂`](https://uiua.org/docs/join) can now be used with [`under ⍜`](https://uiua.org/docs/under)
  - This only works when the joined arrays have different ranks
- [`join ⊂`](https://uiua.org/docs/join) can now be used with [`un °`](https://uiua.org/docs/un) to separate the first row of an array from the rest
- [`try ⍣`](https://uiua.org/docs/try)'s handler's function signature is now more flexible
  - This makes it easier to either provide a default value, process the error itself, or do something different with the inputs
- A [`fill ⬚`](https://uiua.org/docs/fill) value set outside a looping modifier will now no longer be available inside the loop
  - This should make it easier to scope [`fill ⬚`](https://uiua.org/docs/fill) correctly
- [`fill ⬚`](https://uiua.org/docs/fill) can now match the lengths of inputs to [`rows ≡`](https://uiua.org/docs/rows)
- Add recursion via referring to a binding's name within its body
  - Deprecate [`this ↬`](https://uiua.org/docs/this) and [`recur ↫`](https://uiua.org/docs/recur), as they are no longer necessary
- Extend some math functions to work with characters
  - [`sign ±`](https://uiua.org/docs/sign) gets the case of a character
  - [`absolute value ⌵`](https://uiua.org/docs/absolute) uppercases a character
  - [`negate ¯`](https://uiua.org/docs/negate) toggles the case of a character
- [`range ⇡`](https://uiua.org/docs/range) can now be used with negative numbers
- [`eta η`](https://uiua.org/docs/eta), [`pi π`](https://uiua.org/docs/pi), [`tau τ`](https://uiua.org/docs/tau) and [`infinity ∞`](https://uiua.org/docs/infinity) are now parsed as numbers rather than functions
  - This lets them syntactically bind with `¯` or form fraction literals
- Add the [`inventory ⍚`](https://uiua.org/docs/inventory) modifier, which iterates over the unboxed items of an array and re-boxes the results
  - This shortens a lot of box array code
- Change [`content ◇`](https://uiua.org/docs/content)'s glyph to reflect its relationship with [`inventory ⍚`](https://uiua.org/docs/inventory). Code using `⊔` will continue to work and will be formatted as `◇`.
- [`content ◇`](https://uiua.org/docs/content) can now be used with [`under ⍜`](https://uiua.org/docs/under) if its function does
- Macros with 2 or more arguments can now use `‼` at the end of their names. Macro names with any combination of `!` and `‼` will be automatically parsed and formatted as `‼`s followed by one `!` if necessary.
- `f` can now be used at the beginning of planet notation shorthand for [`fork ⊃`](https://uiua.org/docs/fork)
- Inline functions are no longer required to be in a binding or modifier
  - This allows arbitrary code to be wrapped and marked with a signature
- Remove cosine and arccosine optimizations
  - The inverse of the cosine idiom created a logical inconsistency
- [`pop ◌`](https://uiua.org/docs/pop) can now be used with [`under ⍜`](https://uiua.org/docs/under)
  - This is only useful when [`pop ◌`](https://uiua.org/docs/pop) is composed with other functions
- **Breaking Change** - Flip the order of [`send`](https://uiua.org/docs/send)'s arguments
- Add the [`pool`](https://uiua.org/docs/pool) modifier, which is identical to [`spawn`](https://uiua.org/docs/spawn) but spawns a thread in a thread pool
- [`&rs`](https://uiua.org/docs/&rs) can now take a count of [`infinity ∞`](https://uiua.org/docs/infinity) to read until the end of the stream
- Add the [`&runs`](https://uiua.org/docs/&runs) system function, which runs a command and returns an IO stream handle
- Add the experimental [`stringify`](https://uiua.org/docs/stringify) modifier, which turns its function into a string without calling it
  - This is useful in macros
- Add the experimental [`signature`](https://uiua.org/docs/signature) modifier, which returns the arguments and output of its function without calling it
  - This is useful in macros
- Add the experimental [`&ffi`](https://uiua.org/docs/&ffi) system function, which allows calling functions from shared libraries
  - FFI is still a work in progress, but it is currently useful for foreign functions that aren't too complex
  - Bindings for [Raylib](https://www.raylib.com/) are being worked on as a proof of concept. You can find them in the [rayua](https://github.com/uiua-lang/rayua) repository.
- Add experimental function strands. Putting a `_` between two functions (or a function and a constant), is equivalent to putting them in `()`s
  - This is experimental because it remains to be seen how this may affect readability
- Add experimental labels, denoted by a `$` immediately followed by an identifier, which attach a name to an array. This has two uses:
  - Labels are visible in output and in [`stack`](https://uiua.org/docs/stack) diagnostics
  - Labels in code make it easier to understand when reading
- Add experimental [`shapes`](https://uiua.org/docs/shapes) and [`types`](https://uiua.org/docs/types) modifiers, which validate the shape and type of an array or arrays
  - These both check array properties at runtime and serve as a form of documentation
- [`&var`](https://uiua.org/docs/&var) now throws an error if the variable is not found
- Deprecate [`pop ◌`](https://uiua.org/docs/pop) formatting from `;`
- Deprecate [`all ⋔`](https://uiua.org/docs/all)
  - It was rarely used and was hard to reason about
- Add an experimental `repr` function that produces a string representation of a value in a format that can be read by the interpreter
### Interpreter
- Lots of bug and crash fixes
- Lots of performance improvements and optimizations
  - Optimize the pattern for adjacency: [`≡`](https://uiua.org/docs/rows)[`/`](https://uiua.org/docs/reduce)`F`[`◫`](https://uiua.org/docs/windows)
    - It is as much as 800x faster in some cases
  - Optimize [`≡`](https://uiua.org/docs/rows)[`/`](https://uiua.org/docs/reduce)
  - Square matrices are now transposed in-place
  - ..and more
- Numbers that seem to have a floating-point epsilon rounding error will be output with the epsilon noted
- Language Server
  - Add completions
  - Add diagnostics
  - Add semantic highlighting
  - Add inline function hovering
  - Show [`un °`](https://uiua.org/docs/un)/[`under ⍜`](https://uiua.org/docs/under) compatibility for user-defined functions
### Website
- Add [Tutorial Introduction](https://uiua.org/tutorial/introduction)
- Add [primitives.json](https://uiua.org/primitives.json) for use with tooling
- Tutorials are now in a `/tutorial` route instead of `/docs`

## 0.8.0 - 2024-01-31
### Language
- Add the [`content ◇`](https://uiua.org/docs/content) modifier, which unboxes its function's arguments before calling it
  - Deprecate [`unpack ⊐`](https://uiua.org/docs/unpack) in favor of [`content ◇`](https://uiua.org/docs/content)
  - The behavior of [`content ◇`](https://uiua.org/docs/content) is less implicit and is not prone to some of the potential unexpected behavior of [`unpack ⊐`](https://uiua.org/docs/unpack)
- Add the [`unique ◰`](https://uiua.org/docs/unique) function, which creates a mask of the first occurrence of each unique value in an array
  - Change [`deduplicate ◴`](https://uiua.org/docs/deduplicate)'s glyph to reflect its relationship with [`unique ◰`](https://uiua.org/docs/unique). Code using `⊖` will continue to work and will be formatted as `◴`.
- [`table ⊞`](https://uiua.org/docs/table) now works on rows of arrays but keeps it's optimizations for lists
  - You never wanted element-wise combinations of multi-dimensional arrays anyway
  - Deprecate [`cross ⊠`](https://uiua.org/docs/cross), as it is now redundant
  - This is technically a breaking change, but it is unlikely to break much code
- [`fill ⬚`](https://uiua.org/docs/fill) can now be used to specify default accumulators for [`reduce /`](https://uiua.org/docs/reduce), [`group ⊕`](https://uiua.org/docs/group), and [`partition ⊜`](https://uiua.org/docs/partition)
  - **Breaking Change** - Reducing [`group ⊕`](https://uiua.org/docs/group) and [`partition ⊜`](https://uiua.org/docs/partition) no longer take a required accumulator
  - **Breaking Change** - [`fill ⬚`](https://uiua.org/docs/fill) can no longer be temporarily disabled. Try to scope it to the smallest function.
- **Breaking Change** - Most non-pervasive monadic functions no longer implicitly unbox their argument
  - This implicitness led to some unexpected behavior, particularly when getting the [`length ⧻`](https://uiua.org/docs/length) or [`shape △`](https://uiua.org/docs/shape) of a boxed array
  - Exceptions are [`reverse ⇌`](https://uiua.org/docs/reverse) and [`transpose ⍉`](https://uiua.org/docs/transpose), which work on box elements without unboxing them
- Unicode escape sequences that are not 2 or 4 hex digits long can now be specified with `\u{…}`
- Change [`pop ◌`](https://uiua.org/docs/pop)'s glyph to make it look good in planet notation. Code using `;` will continue to work and will be formatted as `◌`.
- [`un °`](https://uiua.org/docs/un)[`reduce /`](https://uiua.org/docs/reduce)[`multiply ×`](https://uiua.org/docs/multiply) now gives the prime factorization of a number
- [`classify ⊛`](https://uiua.org/docs/classify) and [`deduplicate ◴`](https://uiua.org/docs/deduplicate) now work with [`under ⍜`](https://uiua.org/docs/under)
- [`&fras`](https://uiua.org/docs/&fras) and [`&frab`](https://uiua.org/docs/&frab) now work with [`under ⍜`](https://uiua.org/docs/under)
- Completely remove the deprecated `unbox ⊔`
- Add experimental hashmap functions, which operate on a box array as if it is a hashmap
  - [`map`](https://uiua.org/docs/map)
  - [`has`](https://uiua.org/docs/has)
  - [`get`](https://uiua.org/docs/get)
  - [`insert`](https://uiua.org/docs/insert)
  - [`remove`](https://uiua.org/docs/remove)
- Add experimental [`bind`](https://uiua.org/docs/bind) modifier, which binds local values within a function
  - This introduces some non-tacitness to the language
### Interpreter
- The internal byte array type is now used in more places, which should improve performance a bit
- [`&ime`](https://uiua.org/docs/img) and [`&imd`](https://uiua.org/docs/&imd) now support the QOI image format
- Lots of bug and crash fixes
### Website
- Add a new tutorial: [Thinking With Arrays](https://uiua.org/tutorial/thinkingwitharrays)

## 0.7.1 - 2023-12-18
### Interpreter
- Fix some bugs and crashes

## 0.7.0 - 2023-12-15
### Language
- An entire Uiua codebase is now compiled before it is executed, rather than compiling and executing line-by-line
- Add the [`memo`](https://uiua.org/docs/memo) modifier, which memoizes a function
- Add the [`comptime`](https://uiua.org/docs/comptime) modifier, which runs a function at compile time
- [`&i`](https://uiua.org/docs/&i) can now only be used as the first function in a binding
- [`repeat ⍥`](https://uiua.org/docs/repeat) can no longer use a negative number of repetitions
- [`repeat ⍥`](https://uiua.org/docs/repeat) can now be used with [`un °`](https://uiua.org/docs/un) and [`under ⍜`](https://uiua.org/docs/under)
- [`reshape ↯`](https://uiua.org/docs/reshape) now works with [`under ⍜`](https://uiua.org/docs/under)
- [`scan \`](https://uiua.org/docs/scan) now works with [`un °`](https://uiua.org/docs/un) in some cases
- [`setinv`](https://uiua.org/docs/setinv) and [`setund`](https://uiua.org/docs/setund) are no longer experimental
- Add output comments, which the formatter fills with values from the stack
  - Make an empty comment starting with `n` additional `#`s
  - The formatter will replace the comment with the top `n` values from the stack
  - Output comments in functions will show a number of values present on the stack for each time the function is called
### Interpreter
- LSP improvements
  - Add hover information on binding references
  - Add signatures to binding hover information
  - Add same-file binding rename support
  - Add same-file goto definition support
- Add the `uiua build` command, which emits a `.uasm` bytecode file
- `uiua run` can now run a `.uasm` bytecode file
- `uiua stand` now embeds the bytecode assembly in the executable
- Multiple compiler errors can now be emitted at once
- Bug and crash fixes
- Performance improvements
### Website
- Add an [Inverses](https://uiua.org/tutorial/inverses) tutorial
- Each tutorial challenge now contains 1 or 2 answers

## 0.6.1 - 2023-12-07
### Interpreter
- Make [proxy values](https://uiua.org/tutorial/functions#proxy) a little less leaky
- Make placeholders work properly with [`both ∩`](https://uiua.org/docs/both)
- Some other bug and crash fixes

## 0.6.0 - 2023-12-06
### Language
- [`fix ¤`](https://uiua.org/docs/fix) now works with binary pervasive functions
  - This removes the need for some uses of [`rows ≡`](https://uiua.org/docs/rows) and should be a bit faster
- [`fill ⬚`](https://uiua.org/docs/fill) can now be disabled for a function by filling with an empty list
- [`parse ⋕`](https://uiua.org/docs/parse) now has a glyph and is semi-pervasive
  - It was being used enough to warrant a glyph
- [`sign ±`](https://uiua.org/docs/sign), [`floor ⌊`](https://uiua.org/docs/floor), [`ceiling ⌈`](https://uiua.org/docs/ceiling), and [`round ⁅`](https://uiua.org/docs/round) now work with [`under ⍜`](https://uiua.org/docs/under)
- Add some missing arithmetic inverses and unders involving [`flip :`](https://uiua.org/docs/flip)
- Change `pack`'s name to [`unpack ⊐`](https://uiua.org/docs/unpack), and it no longer implicitly boxes values (only unboxes them)
  - Implicit boxing could lead to unexpected and inconsistent behavior
- Change `invert`'s name and glyph to [`un °`](https://uiua.org/docs/un). Code using `⍘` will continue to work and will be formatted as `°`.
  - `°` is a nicer glyph, and `un` composes more nicely with the names of invertible functions
- Deprecate `unbox ⊔` in favor of [`un °`](https://uiua.org/docs/un) [`box □`](https://uiua.org/docs/box)
  - It can still be typed the same way!
- Deprecate [`reduce /`](https://uiua.org/docs/reduce) with a monadic function
  - This created poorly-defined stack signatures that changed depending on the length of the array being reduced
  - [`un °`](https://uiua.org/docs/un) with stack array and planet notations, i.e. `°[⊙⊙∘]`, can be used instead, as it has a well-defined signature
  - For operating on just part of an array, use [`under ⍜`](https://uiua.org/docs/under) [`take ↙`](https://uiua.org/docs/take), [`drop ↘`](https://uiua.org/docs/drop), or [`select ⊏`](https://uiua.org/docs/select)
- [`box □`](https://uiua.org/docs/box)ed arrays can once again be compared lexicographically
### Interpreter
- Make [`stack ?`](https://uiua.org/docs/stack) and [`dump`](https://uiua.org/docs/dump) output show call stack
- Show type and shape information when pretty-printing empty arrays with rank 2 or greater
- Improve language server hover information
- Bug and crash fixes
### Website
- Add a token count to the editor (in settings)
- Files can now be dragged into the editor to open them with [`&fras`](https://uiua.org/docs/&fras)
- [`&fld`](https://uiua.org/docs/&fld) now works on the website

## 0.5.1 - 2023-12-02
### Interpreter
- Fix [`stack ?`](https://uiua.org/docs/stack) signature

## 0.5.0 - 2023-12-02
### Language
- [`invert ⍘`](https://uiua.org/docs/un) and [`under ⍜`](https://uiua.org/docs/under) now work with stack array notation.
- Add the [`stack ?`](https://uiua.org/docs/stack) function, which debug-prints the entire stack
- [`dump`](https://uiua.org/docs/dump) now works with [`invert ⍘`](https://uiua.org/docs/un) and [`under ⍜`](https://uiua.org/docs/under)
- [`fill ⬚`](https://uiua.org/docs/fill) and `pack ⊐` are now exclusive
- Change how [`regex`](https://uiua.org/docs/regex) works to be more powerful
- Add special syntax for splitting/joining lines of code
  - `'` will split a line without changing semantics
  - `''` will combine two lines without changing semantics
- The way pervasive functions work with [`box □`](https://uiua.org/docs/box)ed arrays is now more consistent
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
- [`windows ◫`](https://uiua.org/docs/windows) can now take negative window sizes
- Add an *experimental* distinction for some functions/modifiers
  - Experimental features are opt-in and must be enabled by putting an `# Experimental!` comment at the top of a file
- Add the experimental [`all ⋔`](https://uiua.org/docs/all) modifier, which is a variadic generalization of [`both ∩`](https://uiua.org/docs/both)
- Add the experimental `rectify ⌅` modifier, which sets a function's inverse to itself
- Add the experimental [`setinv`](https://uiua.org/docs/setinv) modifier, which sets the inverse of a function
- Add the experimental [`setunder`](https://uiua.org/docs/setund) modifier, which sets the [`under ⍜`](https://uiua.org/docs/under)-compatible inverse of a function
- Add the experimental [`this ↬`](https://uiua.org/docs/this) modifier, which sets a function to recur to
- Add the experimental [`recur ↫`](https://uiua.org/docs/recur) modifier, which calls a function recursively
- Allow custom modifiers to use switch function syntax
- [`sign ±`](https://uiua.org/docs/sign) now normalizes complex numbers
- Change [`type`](https://uiua.org/docs/type) mapping
- Deprecate `reach ⟜`
### Interpreter
- Fix some bugs and crashes
- Improve some formatting with multiline functions and switch functions
- [`identity ∘`](https://uiua.org/docs/identity) no longer formats from `()`
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
  - Deprecate `level ≑` and `combinate ◳`
  - Deprecate `tribute ≐` and `distribute ∺`
  - [`fold ∧`](https://uiua.org/docs/fold) no longer takes a rank list
- Add the [`rerank ☇`](https://uiua.org/docs/rerank) function, which changes the rank of an array's rows
  - This fills the void left by `level ≑` and `combinate ◳`
- Add the [`fix ¤`](https://uiua.org/docs/fix) function, which adds a length 1 axis to an array
- [`rows ≡`](https://uiua.org/docs/rows) now repeats the rows of an arrays that have exactly 1 row
  - This in combination with [`fix ¤`](https://uiua.org/docs/fix) fills the void left by `tribute ≐` and `distribute ∺`
- [`cross ⊠`](https://uiua.org/docs/cross) can now take more than 2 arguments
- Switch functions are now less strict about branch signature compatibility and can take arrays as conditions
- A single switch function can now be used as a list of functions for dyadic modifiers
  - [`fork ⊃`](https://uiua.org/docs/fork) and [`bracket ⊓`](https://uiua.org/docs/bracket) can take more than 2 functions without chaining this way
- Remove `if ?`, as all its use cases are now covered by switch functions. It will continue to parse, but `?ab` will be formatted as `(b|a)`
- [`flip :`](https://uiua.org/docs/flip)'s glyph is now just a colon (it was `RATIO ∶`)
- [`under ⍜`](https://uiua.org/docs/under) now works with [`absolute value ⌵`](https://uiua.org/docs/absolute)
- Remove `break ⎋` for good
### Interpreter
- Lots of bug and crash fixes
- Lots of performance improvements
### Website
- Update the [Advanced Array Manipulation Tutorial](https://uiua.org/tutorial/advancedarray) to reflect the changes in this version

## 0.2.0 - 2023-11-09
### Language
- [`under ⍜`](https://uiua.org/docs/under) with [`take ↙`](https://uiua.org/docs/take) and [`drop ↘`](https://uiua.org/docs/drop) is now less strict about shape/rank changes
- [`range ⇡`](https://uiua.org/docs/range) called on a list of 0 or 1 values is now more consistent
- [`fill ⬚`](https://uiua.org/docs/fill) now works with [`rotate ↻`](https://uiua.org/docs/rotate) to give non-wrapping behavior
- [`fill ⬚`](https://uiua.org/docs/fill) now works with [`find ⌕`](https://uiua.org/docs/find) if the searched-for array is longer than the array being searched
- [`parse`](https://uiua.org/docs/parse) now works with [`invert ⍘`](https://uiua.org/docs/un) and [`under ⍜`](https://uiua.org/docs/under)
- The output of [`find ⌕`](https://uiua.org/docs/find) is now the same shape as the array being searched
### Interpreter
- Fix a bunch of bugs
- Several performance optimizations
  - [`transpose ⍉`](https://uiua.org/docs/transpose) is now much faster
  - `distribute ∺` and `tribute ≐` are now much faster if their function is a pervasive built-in
  - Some other functions are also a bit faster
### Website
- Hold shift when copying a link to copy a Markdown link
- Add embeddable editor. Replace the `pad` in pad links with `embed` or `embedpad`.

## 0.1.0 - 2023-11-03
### Language
- Add complex numbers, which can be created with the [`complex ℂ`](https://uiua.org/docs/complex) function
- Add the [`do ⍢`](https://uiua.org/docs/do) modifier, which repeatedly calls a function as long as a condition holds
- Deprecate `break ⎋`
- Add multi-dimensional [`where ⊚`](https://uiua.org/docs/where)
- [`join ⊂`](https://uiua.org/docs/join) to an empty list now always works regardless of rank of the other array
- [`each ∵`](https://uiua.org/docs/each) and [`rows ≡`](https://uiua.org/docs/rows) now work with [`under ⍜`](https://uiua.org/docs/under)
- All ocean functions now work with [`under ⍜`](https://uiua.org/docs/under)
- Allow multiple values to be returned from [`each ∵`](https://uiua.org/docs/each), [`rows ≡`](https://uiua.org/docs/rows), `distribute ∺`, `tribute ≐`, [`table ⊞`](https://uiua.org/docs/table), and [`cross ⊠`](https://uiua.org/docs/cross)
- [`invert ⍘`](https://uiua.org/docs/un) [`atangent ∠`](https://uiua.org/docs/atangent) now produces the sine and cosine of an angle
- [`&i`](https://uiua.org/docs/&i) now treats paths as relative to the file calling it rather than the current working directory
- Rank list functions for the rank-generic modifiers can now take any number of arguments. For any number of augments greater that 0, an empty numeric list will be pushed before the function is called.
- Add fraction literals with `/`
- Parsing multiple formattable functions from words is now smarter
- Remove `bind '`. It made code hard to read. It will continue to parse, but will be formatted as `(…)`
### Interpreter
- Add the `uiua stand` command, which creates a standalone executable
### Website
- Add [Optimizations](https://uiua.org/docs/optimizations) page
- Add [Images and GIFs](https://uiua.org/tutorial/images) tutorial

## 0.0.25 - 2023-10-29
### Interpreter
- Fix a bug with watch mode

## 0.0.24 - 2023-10-29
### Language
- Add the `reach ⟜` modifier, which removes the second value from the stack and calls its function.
- Change how short spellings of [`dip ⊙`](https://uiua.org/docs/dip), [`gap ⋅`](https://uiua.org/docs/gap), and [`identity ∘`](https://uiua.org/docs/identity) work
  - Instead of allowing them to be spelled with 2 characters, they can now be spelled with 1 character as long as there are at least 2 in the sequence. 
  - If present, `'i'` may only come last. 
  - `reach ⟜` is included.
- Add 2-letter spellings of `deep ≊`, `abyss ≃`, and `seabed ∸` to make them consistent with `rock ⋄`.
### Interpreter
- Fix a bunch of bugs and crashes
- The formatter now indents bindings that start with a bound function that starts with [`&i`](https://uiua.org/docs/&i)
- The native interpreter no longer automatically checks for updates. You can still check manually with `uiua update?`.
### Website
- Add challenges to the end of tutorial sections
- Make the introductory examples on the main page less esoteric
- Update the [Advanced Stack Manipulation Tutorial](https://uiua.org/tutorial/morestack) to include `reach ⟜`
### Crate
- The [Uiua Rust crate](https://crates.io/crates/uiua) is now [fully documented](https://docs.rs/uiua) and has a decent API

## 0.0.23 - 2023-10-25
### Language
- Implement [`under ⍜`](https://uiua.org/docs/under) multi-index [`pick ⊡`](https://uiua.org/docs/pick)
- Implement [`under ⍜`](https://uiua.org/docs/under) [`partition ⊜`](https://uiua.org/docs/partition)
- Implement [`under ⍜`](https://uiua.org/docs/under) [`group ⊕`](https://uiua.org/docs/group)
- Add [`send`](https://uiua.org/docs/send), [`recv`](https://uiua.org/docs/recv), and [`tryrecv`](https://uiua.org/docs/tryrecv) functions for sending values between threads
- Add [`&fde`](https://uiua.org/docs/&fde) and [`&ftr`](https://uiua.org/docs/&ftr) system functions for deleting/trashing files and directories
- [`under ⍜`](https://uiua.org/docs/under) with system functions that return stream handles calls [`&cl`](https://uiua.org/docs/&cl) as an inverse
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
- Add new syntax for defining [custom modifiers](https://uiua.org/tutorial/custommodifiers)
- Add new syntax for [calling a function from a list of functions](https://uiua.org/tutorial/controlflow#switch)
- Add the `pack ⊐` modifier, which calls its function and implicitly boxes/unboxes values
- Add the `combinate ◳` modifier, which is a rank-generic version of [`table ⊞`](https://uiua.org/docs/table)
- [`fold ∧`](https://uiua.org/docs/fold) is now rank-generic and requires a rank list
- Add the `tribute ≐` modifier, which is a flipped version of `distribute ∺`
- Change `level ≑`'s glyph to reflect its relationship with [`each ∵`](https://uiua.org/docs/each), [`rows ≡`](https://uiua.org/docs/rows), `distribute ∺`, and `tribute ≐`. Code using `⍚` will continue to work and will be formatted as `≑`.
- Add `rock ⋄`, `surface ~`, `deep ≊`, `abyss ≃`, and `seabed ∸`, which build rank lists to be used with `level ≑` and the new rank-generic modifiers
- Change [`trace ⸮`](https://uiua.org/docs/trace)'s glyph to let `surface ~` use `~`.
- Change [`match ≍`](https://uiua.org/docs/match)'s glyph to avoid confusion with the new ocean functions' glyphs. Code using `≅` will continue to work and will be formatted as `≍`.
- Stack signatures found to be incorrect at runtime produce an error
- Dyadic math operations now work with [`under ⍜`](https://uiua.org/docs/under) even if both arguments are outside [`under ⍜`](https://uiua.org/docs/under)'s function
- Some mathematical functions that previously did not work with [`invert ⍘`](https://uiua.org/docs/un) and [`under ⍜`](https://uiua.org/docs/under) when accompanied by [`flip :`](https://uiua.org/docs/flip) now do
### Website
- Add 3 new tutorials
  - [Control Flow](https://uiua.org/tutorial/controlflow)
  - [Advanced Array Manipulation](https://uiua.org/tutorial/advancedarray)
  - [Custom Modifiers](https://uiua.org/tutorial/custommodifiers)
- The orientation of stack values in the output can be flipped in the settings

## 0.0.20 - 2023-10-16
### Language
- Add [`regex`](https://uiua.org/docs/regex) function for matching regular expressions
- Add [`utf`](https://uiua.org/docs/utf₈) function for UTF-8 encoding and decoding
- Add [`&invk`](https://uiua.org/docs/&invk) system function for invoking a path to be opened with the system's default program
- [`fill ⬚`](https://uiua.org/docs/fill) can now be used with [`first ⊢`](https://uiua.org/docs/first)
- Most functions that expect strings as arguments will now dig arbitrarily deep into boxes
- Make `if ?` signature checking more permissive
- The presence of `break ⎋` in a [`repeat ⍥`](https://uiua.org/docs/repeat) always requires a stack signature
- The [`&runi`](https://uiua.org/docs/&runi) and [`&runc`](https://uiua.org/docs/&runc) functions now return exit codes
- Multiline string now only insert `\n` at the end of each line instead of `\r\n`
### Interpreter
- Bugfixes and performance improvements
### Website
- Add a page listing common [stack idioms](https://uiua.org/docs/stack-idioms)

## 0.0.19 - 2023-10-13
### Language
- Add [`under ⍜`](https://uiua.org/docs/under) [`both ∩`](https://uiua.org/docs/both)
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
- **Major Change** `distribute ∺` now takes the array being distributed as its *last* argument, rather than its first
- Add [`where ⊚`](https://uiua.org/docs/where) function, which returns the indices of an array that have non-zero values
- `if ?`'s branches can now have a different number of arguments (but not outputs)
- `if ?`'s condition can now be a list of conditions, and the branch will be chosen for each row in the argument(s)
- The reducing versions of [`group ⊕`](https://uiua.org/docs/group) and [`partition ⊜`](https://uiua.org/docs/partition) now take accumulators. Aggregating versions are unchanged.
- [`spawn`](https://uiua.org/docs/spawn) and [`wait`](https://uiua.org/docs/wait) no longer have glyphs. Code using `↰` and `↲` will continue to work and will be formatted as `spawn` and `wait`.
- `&n` is no longer a system function and is now called [`now`](https://uiua.org/docs/now)
- [`under ⍜`](https://uiua.org/docs/under) [`now`](https://uiua.org/docs/now) can be used to time things
- `call !` can now call functions that return any number of values, not just one
- Add hex character escape sequences for string and character literals.
  - `\xNN` for short ASCII codes
  - `\uNNNN` for full Unicode sequences
### Interpreter
- The formatter now aligns consecutive end-of-line comments
- `NaN`s no longer propagate in [`minimum ⌊`](https://uiua.org/docs/minimum) and [`maximum ⌈`](https://uiua.org/docs/maximum)
- Fix a bug that prevented [`under ⍜`](https://uiua.org/docs/under) multidimensional [`take ↙`](https://uiua.org/docs/take) and [`drop ↘`](https://uiua.org/docs/drop) from working
- Fix a bug in how [`fold ∧`](https://uiua.org/docs/fold) ordered multiple accumulators
- Fix a bug that allowed incorrect signatures to be declared for functions
- Fix a bunch of other bugs and crashes
### Website
- Add the Uiua386 font as an option in the editor

## 0.0.17 - 2023-10-07
### Language
- Add GIF encoding with [`&gife`](https://uiua.org/docs/gif)
- Rename `constant` to [`box □`](https://uiua.org/docs/box).
- Add `unbox ⊔`, which unboxes a boxed array
- **Major Change:** Some uses of `call !` will no longer compile without declaring a stack signature. When unboxing [`box □`](https://uiua.org/docs/box)ed arrays, you can use `unbox ⊔` instead, which has a well-defined signature.
- Add [`fall ⍖`](https://uiua.org/docs/fall) function, which gives the indices of the array if it were sorted descending
- Change `grade ⌂` name and glyph to [`rise ⍏`](https://uiua.org/docs/rise) to reflect its relationship with [`fall ⍖`](https://uiua.org/docs/fall). Code using `⌂` will continue to work and will be formatted as `⍏`.
- [`try ⍣`](https://uiua.org/docs/try) now puts arguments to its first function *above* the error value when calling the error handler
- [`fold ∧`](https://uiua.org/docs/fold) can now use multiple accumulators
- Improve [`dump`](https://uiua.org/docs/dump) output formatting
- [`dump`](https://uiua.org/docs/dump) is now a monadic modifier. Its function preprocesses each value before dumping it.
- Add the `sig` function, which returns the stack signature of a value
- A negative dimensions in the shape passed to [`reshape ↯`](https://uiua.org/docs/reshape) can now be in *any* position, not just the first or last
- Functions with ASCII glyphs now also format from their names
- Add a advice diagnostic about the capitalization of binding names
### Interpreter
- A few performance improvements, particularly to [`keep ▽`](https://uiua.org/docs/keep), [`fork ⊃`](https://uiua.org/docs/fork), and [`under ⍜`](https://uiua.org/docs/under)
### Website
- Add GIF output
- Execution time limit is now 2 seconds by default but can be customized

## 0.0.16 - 2023-10-05
### Interpreter
- Fix a crash and a bug that could occur when creating nested arrays that pull in values.

## 0.0.15 - 2023-10-05
This version changes a lot of glyphs. If you are coming from the previous version, most of the old glyphs will be automatically formatted to the new ones. The only change you may need to make is replacing all `^`s with `|`s.

You may want to read the new version of the [Advanced Stack Manipulation Tutorial](https://uiua.org/tutorial/morestack) to understand the reason for so many of these changes.

### Language
- Add the [`bracket ⊓`](https://uiua.org/docs/bracket) modifier, which calls two functions each on different arguments
- Change [`fill ⬚`](https://uiua.org/docs/fill)'s glyph to reflect its relationship with [`box □`](https://uiua.org/docs/box). Code using `⍛` with continue to work and will be formatted as `⬚`.
- Change `share ⇉` name and glyph to [`fork ⊃`](https://uiua.org/docs/fork). Code using `⇉` will continue to work and will be formatted as `⊃`.
- Change `noop ·` name and glyphs to [`identity ∘`](https://uiua.org/docs/identity) to reflect its relationship with [`gap ⋅`](https://uiua.org/docs/gap) and [`dip ⊙`](https://uiua.org/docs/dip). Code using `·` will continue to work and will be formatted as `∘`.
- Change [`identity ∘`](https://uiua.org/docs/identity)'s signature from `|0.0` to `|1.1`
- Add the [`gap ⋅`](https://uiua.org/docs/gap) modifier, which discards a value then calls its function. It is mainly intended to be used with [`fork ⊃`](https://uiua.org/docs/fork).
- Change [`dip ⊙`](https://uiua.org/docs/dip)'s glyph to reflect its relationship with [`gap ⋅`](https://uiua.org/docs/gap) and [`identity ∘`](https://uiua.org/docs/identity). Code using `→` will continue to work and will be formatted as `⊙`.
- Change [`both ∩`](https://uiua.org/docs/both)'s glyph to reflect its relationship with [`fork ⊃`](https://uiua.org/docs/fork). Code using `∷` will continue to work and will be formatted as `∩`.
- `distribute ∺` now works with any number of arguments. Only the first argument is distributed.
- [`fill ⬚`](https://uiua.org/docs/fill) now works with [`reshape ↯`](https://uiua.org/docs/reshape)
- [`reshape ↯`](https://uiua.org/docs/reshape) now allow negative numbers to denote derived dimensions
- Change the modifier termination character to `|` instead of `^`
- Remove old versions of `fork` and `trident`
- Add the [`&httpsw`](https://uiua.org/docs/&httpsw) function for making HTTPS requests
### Interpreter
- Add formatter configuration options. See the [readme](https://github.com/uiua-lang/uiua/blob/main/site/text/format_config.md) for details.
- Checking for updates is less zealous, and can be disabled with the `--no-update` flag to `uiua run` or `uiua watch`
### Website
- Running code in the Pad editor updates the URL to prevent work from accidentally being lost

# Pre-Version Updates

## 2023-10-03
### Language
- Add the `share ⊃` modifier, which unifies and deprecates [`fork ⊃`](https://uiua.org/docs/fork) and `trident ∋`
- `bind '` no longer calls its functions immediately. This should not change any reasonable existing code.
- Change how [`partition ⊜`](https://uiua.org/docs/partition) and [`group ⊕`](https://uiua.org/docs/group) work with dyadic functions to be consistent with [`reduce /`](https://uiua.org/docs/reduce)
- Deprecate `restack ⇵`. It was never a good idea.
- Remove the overloaded behavior of `call !`. It no longer behaves like an if-else when used with a list of functions.
  - You can replace all existing instances of that use case with `!⊡:`
- Add the `if ?` modifier, which calls one of two functions based on a condition

## 2023-10-02
### Language
- [`both ∩`](https://uiua.org/docs/both) can now be used with a function that takes any number of arguments.
- Various bug and crash fixes
### Interpreter
- Tell the user when the interpreter can be updated

## 2023-10-01
- Add the [`dip ⊙`](https://uiua.org/docs/dip) modifier, which temporarily pops a value
- Deprecate `roll↷` and `unroll↶`
- Add [`under ⍜`](https://uiua.org/docs/under) [`keep ▽`](https://uiua.org/docs/keep)
- Add [`dump`](https://uiua.org/docs/dump) function, which prints the entire stack

## 2023-09-30
### Language
- Remove the `|1.1` signature restriction for [`under ⍜`](https://uiua.org/docs/under)'s second function
- Remove the rank`∴` function
- Remove the restriction that all functions in a non-scalar function array all have the compatible signatures
- Whether a binding is a constant or a function is now independent of how many values are on the stack
- Add a system for non-error diagnostics
  - Add advice about redundant uses of [`each ∵`](https://uiua.org/docs/each)
### Interpreter
- Allow passing `--no-format` to `uiua watch`
- [`&sc`](https://uiua.org/docs/&sc) now returns `0` if EOF is input
### Website
- [`&sc`](https://uiua.org/docs/&sc) now works on the website by showing a prompt

## 2023-09-29
### Language
- Make binding names case-sensitive
- Add `^` syntax to terminate modifier parsing. 
- Add [`&runi`](https://uiua.org/docs/&runi) and [`&runc`](https://uiua.org/docs/&runc) functions for running commands
- Add [`&cd`](https://uiua.org/docs/&cd) function for changing the current working directory
- Add shadowable [constants](https://uiua.org/docs/constants) like `e` and `os`
- Change `trident ∋` argument order to make it easier to reason about
- Enable [`fill ⬚`](https://uiua.org/docs/fill) for [`keep ▽`](https://uiua.org/docs/keep) if the amount list is shorter than the kept array
### Interpreter
- Add `uiua eval` command which evaluates a Uiua expression and prints the result
- Watch commands no longer try to open the file being watched
- Fix a bug that made numbers that were `≤ 1e-12 ⌵` format to `0`
### Website
- Make a space character `@ ` more visible by underlining the space
- Improve cursor movement when formatting in the editor

## 2023-09-28
### Language
- Add this changelog
- Add [`trace ~`](https://uiua.org/docs/trace) function
  - Debug-prints the value on top of the stack without popping it
  - Shows the line and column number too
- Add [`both ∩`](https://uiua.org/docs/both) modifier
  - This can change code like `/(|2 ⊂!:!:) {"a" "bc" "def"}`
  - To just `/'⊂∩! {"a" "bc" "def"}`
- Turn the term pair syntactic construct into a modifier called `bind '`
### Interpreter
- Fix some correctness bugs related to [`under ⍜`](https://uiua.org/docs/under) and [`invert`](https://uiua.org/docs/un)
- Fix a crash when trying to reverse an empty array
### Website
- Add a [right-to-left](https://uiua.org/rtl) explanation page
