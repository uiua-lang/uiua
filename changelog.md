# Uiua Changelog

## Logpoint 3 - 2023-09-30
### Language
- Remove the rank`∴` function
- Remove the restriction that all functions in a non-scalar function array all have the same signature
### Interpreter
- Allow passing `--no-format` to `uiua watch`

## Logpoint 2 - 2023-09-29
### Language
- Make binding names case-sensitive
- Add `^` syntax to terminate modifier parsing. There is a basic example [in the tutorial](http://uiua.org/docs/functions#terminating-modifiers).
- Add [`&runi`](https://uiua.org/docs/&runi) and [`&runc`](https://uiua.org/docs/&runc) functions for running commands
- Add [`&cd`](https://uiua.org/docs/&cd) function for changing the current working directory
- Add shadowable [constants](https://uiua.org/docs/constants) like `e` and `os`
- Change [trident`∋`](https://uiua.org/docs/trident) argument order to make it easier to reason about
- Enable [fill`⍛`](https://uiua.org/docs/fill) for [keep`▽`](https://uiua.org/docs/keep) if the amount list is shorter than the kept array
- Fix a bug that made numbers that were `≤` `1e-12` `⌵` format to `0`
### Interpreter
- Add `uiua eval` command which evaluates a Uiua expression and prints the result
- Watch commands no longer try to open the file being watched
### Website
- Make a space character `@ ` more visible by underlining the space
- Improve cursor movement when formatting in the editor

## Logpoint 1 - 2023-09-28
### Language
- Add this changelog
- Add [trace`~`](https://uiua.org/docs/trace) function
  - Debug-prints the value on top of the stack without popping it
  - Shows the line and column number too
- Add [both`∷`](https://uiua.org/docs/both) modifier
  - This can change code like `/(|2 ⊂!∶!∶) {"a" "bc" "def"}`
  - To just `/'⊂∷! {"a" "bc" "def"}`
- Turn the term pair syntactic construct into a modifier called [bind`'`](https://uiua.org/docs/bind)
- Fix some correctness bugs related to `under` and `invert`
- Fix a crash when trying to reverse an empty array
### Website
- Add a [right-to-left](https://uiua.org/rtl) explanation page
