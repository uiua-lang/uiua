
<img src="site/uiua-logo.png" width="140"/> 

# Uiua

Uiua (pronounced *"wee-wuh"*) is a stack-based array programming language.

Documentation, examples, and an online interpreter can be found at [uiua.org](https://uiua.org).

You can also check out the [Discord server](https://discord.gg/3r9nrfYhCc).

You can support Uiua's development via [GitHub Sponsors](https://github.com/sponsors/uiua-lang).

## Installation

You can try the language without installing anything on [the website](https://uiua.org).

If you want to install the interpreter locally there are 2 options:
- If you are on Windows, you can simply download the [latest release](https://github.com/uiua-lang/uiua/releases).
- If you are not on Windows, you will need to install via Cargo.
This requires [Rust](https://www.rust-lang.org/tools/install) to be installed.

  You can install with:
  ```
  cargo install uiua
  ```
  The following optional features are available (enabled by passing `--features <feature>`):
  - `audio`: Enables audio system functions

  If you want audio on Linux, you may need to install some dependencies first:
  ```
  apt install libasound2-dev libudev-dev pkg-config
  ```
- If you want the most recent development version of Uiua, you can install from the git repository.
  ```
  cargo install --git https://github.com/uiua-lang/uiua uiua
  ```

## Language Server

The interpreter has a built-in language server that implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

A language client extension is available for VSCode [here](https://marketplace.visualstudio.com/items?itemName=uiua-lang.uiua-vscode).

The language client requires that the interpreter is installed locally and available on your PATH.
