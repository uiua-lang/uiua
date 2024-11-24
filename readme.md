
<img src="site/assets/uiua-logo.png" width="140"/> 

# Uiua

Uiua (pronounced *"wee-wuh"*) is a stack-based array programming language.

Documentation, examples, and an online interpreter can be found at [uiua.org](https://uiua.org).

You can also check out the [Discord server](https://discord.gg/3r9nrfYhCc).

You can support Uiua's development via [GitHub Sponsors](https://github.com/sponsors/uiua-lang).

## Installation

You can try the language without installing anything on [the website](https://uiua.org).

If you want to install the interpreter locally there are 2 options:
- If your OS is supported, you can simply download the [latest release](https://github.com/uiua-lang/uiua/releases).
- If not, you will need to install via Cargo.
This requires [Rust](https://www.rust-lang.org/tools/install) (>=1.75) to be installed.

  You can install with one of the following commands:
  ```
  cargo install uiua -F full # Installs all features
  cargo install uiua         # No audio, webcam, or window features
  ```
  On Linux, you may need to install some dependencies first:
  ```
  apt install libx11-dev libffi-dev
  ```

  The following optional features are available but not enabled by default (enabled by passing `--features <feature>`):
  - `full`: Enables all optional features below
  - `audio`: Enables audio system functions (may require `libasound2-dev libudev-dev pkg-config` on Linux)
  - `webcam`: Enables webcam system functions (may require `libjpeg-dev` on Linux)
  - `window`: Enables output in a window

- If you want the most recent development version of Uiua, you can install from the git repository.
  ```
  cargo install --git https://github.com/uiua-lang/uiua uiua -F full
  cargo install --git https://github.com/uiua-lang/uiua uiua
  ```
- If you use Nix or NixOS, you can clone this repo and do following:
  ```
  nix develop    # to drop into a shell prompt with all the dependencies
  cargo check    # to make sure you can compile/build latest version
  cargo build    # to build latest debug version of uiua
  cargo run repl # to get uiua repl
  ```
  `nix develop` is not necessary if you have direnv active, in that case `direnv allow` will activate the flake devshell.
  
  *Note:* If you encounter errors such as rustc or any other package
  version mismatch, it is most likely that flake.lock file needs to be
  updated to pull in updated dependencies for nix shell.

## Language and Font Support

The Uiua native interpreter has a built-in language server that can be used by editor extensions.

You can find a list of editor extensions [here](https://uiua.org/docs/install#editor-support).

You can find a list of fonts that support Uiua's characters [here](https://uiua.org/docs/install#fonts).
