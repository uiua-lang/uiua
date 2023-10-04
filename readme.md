
<img src="site/uiua-logo.png" width="140"/> 

# Uiua

Uiua (pronounced *"wee-wuh"*) is a stack-based array programming language.

Documentation, examples, and an online interpreter can be found at [uiua.org](https://uiua.org).

You can also check out the [Discord server](https://discord.gg/3r9nrfYhCc).

## Installation

You can try the language without installing anything on [the website](https://uiua.org).

If you want to install the interpreter locally, you will need to install it via Cargo.
This requires [Rust](https://www.rust-lang.org/tools/install) to be installed.

You can install with:
```
cargo install uiua
```

To enable audio output, enable the `audio` feature:
```
cargo install uiua --features audio
```

If you want audio on Linux, you may need to install some dependencies first:
```
apt install libasound2-dev libudev-dev pkg-config
```

## Language Server

The interpreter has a built-in language server that implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

A language client extension is available for VSCode [here](https://marketplace.visualstudio.com/items?itemName=uiua-lang.uiua-vscode).

The language client requires that the interpreter is installed locally and available on your PATH.

## Formatter Configuration

You can configure Uiua's formatter by creating a file called `.fmt.ua` in the directory from which you run the interpreter. This configuration file is also a Uiua program.

Configuration options are specified by binding values to specific names.

Example with default values:
```
TrailingNewline ← 1
CommentSpaceAfterHash ← 1
MultilineIndent ← 2
CompactMultilineMode ← "auto"
```

The following configuration options are available:

### `TrailingNewline`
Type: boolean

Default: `1`

Whether to add a trailing newline to a file.

### `CommentSpaceAfterHash`
Type: boolean

Default: `1`

Whether to insert a space after a `#` in a comment if there is not one already.

### `MultilineIndent`
Type: natural number

Default: `2`

The number of spaces to indent each line of a multiline expression.

### `CompactMultilineMode`
Type: one of `"always"`, `"never"`, or `"auto"`

Default: `"auto"`

How to format multiline expressions.
- `"always"`: Always format multiline expressions in compact mode.
- `"never"`: Never format multiline expressions in compact mode.
- `"auto"`: Format multiline expressions in compact mode if they do not exceed `MultilineCompactThreshold`.

### `MultilineCompactThreshold`
Type: natural number

Default: `10`

The maximum number of lines a multiline expression can have before it is formatted in compact mode.

Only used if `CompactMultilineMode` is not `"never"`.
