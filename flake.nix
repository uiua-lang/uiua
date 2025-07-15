{
  description = "A stack-based array programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      nixpkgs,
      flake-parts,
      crane,
      rust-overlay,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.platforms.all;
      perSystem =
        {
          self',
          pkgs,
          system,
          lib,
          ...
        }:
        let
          toolchainFor =
            p:
            p.rust-bin.stable.latest.default.override {
              targets = [ "wasm32-unknown-unknown" ];
            };
          craneLib = (crane.mkLib pkgs).overrideToolchain toolchainFor;
          rustPlatform = pkgs.makeRustPlatform {
            rustc = toolchainFor pkgs;
            cargo = toolchainFor pkgs;
          };
          libPath =
            with pkgs;
            lib.makeLibraryPath [
              libGL
              libxkbcommon
              wayland
              xorg.libX11
              xorg.libXcursor
              xorg.libXi
              xorg.libXrandr
            ];
        in
        {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [ rust-overlay.overlays.default ];
          };
          packages = {
            default = pkgs.callPackage ./nix/package.nix { inherit craneLib libPath rustPlatform; };
            fonts = pkgs.callPackage ./nix/fonts.nix { };
            site = pkgs.callPackage ./nix/site.nix { inherit craneLib; };
            toolchain = toolchainFor pkgs;
          };
          apps.site.program = pkgs.writeShellApplication {
            name = "Uiua site";
            runtimeInputs = [ pkgs.simple-http-server ];
            text = ''
              cd ${lib.escapeShellArg self'.packages.site}
              simple-http-server --index --try-file ./404.html "$@" -- .
            '';
          };
          devShells.default = pkgs.mkShell {
            inputsFrom = builtins.attrValues self'.packages;
            packages = with pkgs; [
              lld_18
              trunk
              libffi
              ((toolchainFor pkgs).override {
                extensions = [
                  "rust-src"
                  "rust-analyzer"
                ];
              })
            ];
            LD_LIBRARY_PATH = libPath;
          };
        };
    };
}
