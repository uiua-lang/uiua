# Uiua Changelog

Uiua is not yet stable.

# Pre-Version

## 2023-10-02
### Interpreter
- Tell the user when the interpreter can be updated

## 2023-10-01
- Add the [`dip` `→`](https://uiua.org/docs/dip) modifier, which temporarily pops a value
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
- Enable [`fill` `⍛`](https://uiua.org/docs/fill) for [`keep` `▽`](https://uiua.org/docs/keep) if the amount list is shorter than the kept array
- Fix a bug that made numbers that were `≤` `1e-12` `⌵` format to `0`
### Interpreter
- Add `uiua eval` command which evaluates a Uiua expression and prints the result
- Watch commands no longer try to open the file being watched
### Website
- Make a space character `@ ` more visible by underlining the space
- Improve cursor movement when formatting in the editor

## 2023-09-28
### Language
- Add this changelog
- Add [`trace` `~`](https://uiua.org/docs/trace) function
  - Debug-prints the value on top of the stack without popping it
  - Shows the line and column number too
- Add [`both` `∷`](https://uiua.org/docs/both) modifier
  - This can change code like `/(|2 ⊂!∶!∶) {"a" "bc" "def"}`
  - To just `/'⊂∷! {"a" "bc" "def"}`
- Turn the term pair syntactic construct into a modifier called [`bind` `'`](https://uiua.org/docs/bind)
- Fix some correctness bugs related to `under` and `invert`
- Fix a crash when trying to reverse an empty array
### Website
- Add a [right-to-left](https://uiua.org/rtl) explanation page
