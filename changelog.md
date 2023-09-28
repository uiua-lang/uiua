# Uiua Changelog

## Logpoint 1 - 2023-09-28
### Language
- Add this changelog
- Add [trace`~`](https://uiua.org/docs/trace) function
  - Debug-prints the value on top of the stack without popping it
  - Shows the line and column number too
- Add [both`∷`](https://uiua.org/docs/both) modifier
  - This can change code like `/(|2 ⊂!∶!∶) {"a" "bc" "def"}`
  - To just `/'⊂∷! {"a" "bc" "def"}`
- Fix some correctness bugs related to `under` and `invert`
### Website
- Add [right-to-left](https://uiua.org/rtl) explantation page