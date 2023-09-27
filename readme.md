
<img src="site/uiua-logo.png" width="140"/> 

# Uiua

Uiua (pronounced *"wee-wuh"*) is a stack-oriented array programming language.

Documentation, examples, and an online interpreter can be found at [uiua.org](https://uiua.org).

You can also check out the [Discord server](https://discord.gg/3r9nrfYhCc).

## Installation

You can try the language without installing anything on [the website](https://uiua.org).

If you want to install the interpreter locally, you will need to build it from source.
This requires [Rust](https://www.rust-lang.org/tools/install) to be installed.

You can install with:
```
git clone https://github.com/uiua-lang/uiua
cd uiua
cargo install --path .
```

To enable audio output, replace the cargo command with:
```
cargo install --path . --features audio
```

If you want audio on Linux, you may need to install some dependencies first:
```
apt install libasound2-dev libudev-dev pkg-config
```

## Language Server

The interpreter has a built-in language server that implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/).

A language client extension is available for VSCode [here](https://marketplace.visualstudio.com/items?itemName=uiua-lang.uiua-vscode).

The language client requires that the interpreter is installed locally and available on your PATH.