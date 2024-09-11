{
  description = "A stack-based array programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    crane,
    rust-overlay,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [(import rust-overlay)];
      };
      toolchain = pkgs.rust-bin.stable.latest.default;
      craneLib = (crane.mkLib pkgs).overrideToolchain toolchain;
      uiua-crate = craneLib.buildPackage rec {
        src = pkgs.lib.cleanSourceWith {
          src = ./.;
          # this is needed because else crane would filter out the included images from the build.
          filter = path: type: ((path: _type: builtins.match ".*$" path != null) path type) || (craneLib.filterCargoSources path type);
          name = "source";
        };
        nativeBuildInputs = [pkgs.pkg-config];
        buildInputs =
          nixpkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.iconv
            pkgs.libffi
            pkgs.darwin.apple_sdk.frameworks.AppKit
            pkgs.darwin.apple_sdk.frameworks.CoreServices
            pkgs.darwin.apple_sdk.frameworks.Foundation
          ]
          ++ [
            pkgs.libffi
          ];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
          # I am sorry for this abomination but this is needed so that libffi is compiled with the "system" feature
          # else it would try to build libffi-sys as a dependency, which fails with the nix build system
          # so nix users will pull libffi (sys) from nixpkgs and not build "libffi-sys" and non-nix users will not have to include the libffi system lib
          # sadly this does not work with git patches because of cranes source handeling
        preConfigureHooks = with pkgs; ''
          text_to_insert='features = ["system"]'
          # this is the case for building the uiua dependencies
          # it matches on the "[dependencies.libffi]" line and adds the above text below that
          sed -i "/dependencies.libffi/ a $text_to_insert" "Cargo.toml"
          replacement_text='libffi = {version = "3", optional = true, features = ["system"]}'
          # this is the case when building the uiua package
          # it replaces the dependency declaration with one that enables the "system" feature
          sed -i "/libffi = /c $replacement_text" "Cargo.toml"
        '';
      };
    in {
      packages.default = uiua-crate;
      devShell = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.packages.${system};
        nativeBuildInputs = with pkgs; [
          clippy
          rust-analyzer
          rustfmt
          cargo
          rustc
          libffi
        ];
      };
    });
}
