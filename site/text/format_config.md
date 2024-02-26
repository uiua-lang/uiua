
# Uiua Formatter Configuration

You can configure Uiua's formatter by creating a file called `.fmt.ua` in the directory from which you run the interpreter. This configuration file is also a Uiua program.

Configuration options are specified by binding values to specific names.

Example with default values:
```uiua
TrailingNewline ← 1
CommentSpaceAfterHash ← 1
MultilineIndent ← 2
CompactMultilineMode ← "auto"
MultilineCompactThreshold ← 10
AlignComments ← 1
IndentItemImports ← 1
```
The following configuration options are available:

### TrailingNewline
Type: boolean

Default: `1`

Whether to add a trailing newline to the output.

---

### CommentSpaceAfterHash
Type: boolean

Default: `1`

Whether to add a space after the `#` in comments.

---

### MultilineIndent
Type: natural number

Default: `2`

The number of spaces to indent multiline arrays and functions

---

### CompactMultilineMode
Type: `"always"`, `"never"`, or `"auto"`

Default: `"auto"`

The mode for formatting multiline arrays and functions.

- `"always"`: Always format multiline expressions in compact mode.
- `"never"`: Never format multiline expressions in compact mode.
- `"auto"`: Format multiline expressions in compact mode if they exceed `MultilineCompactThreshold`.

---

### MultilineCompactThreshold
Type: natural number

Default: `10`

The number of characters on line preceding a multiline array or function, at or before which the multiline will be compact.

---

### AlignComments
Type: boolean

Default: `1`

Whether to align consecutive end-of-line comments

---

### IndentItemImports
Type: boolean

Default: `1`

Whether to indent item imports

---

