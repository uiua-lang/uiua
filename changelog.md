# Uiua Changelog

## Logpoint 2 - 2023-09-29
### Language
- Add `^` syntax to terminate modifier parsing. There is a basic example [in the tutorial](http://uiua.org/docs/functions#terminating-modifiers).
- Change [trident`∋`](https://uiua.org/docs/trident) argument order to make it easier to reason about
- Fix a bug that made numbers that were `≤``1e-12``⌵` format to `0`
### Website
- Make a space character `@ ` more visible by underlining the space

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
