{
  description = "A stack-based array programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
  };
  outputs =
    {
      nixpkgs,
      flake-parts,
      crane,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.platforms.all;
      perSystem =
        {
          self',
          pkgs,
          ...
        }:
        let
          craneLib = crane.mkLib pkgs;
        in
        {
          packages.default = pkgs.callPackage ./package.nix { inherit craneLib; };
          devShells.default = pkgs.mkShell {
            inputsFrom = builtins.attrValues self'.packages;
            packages = with pkgs; [
              lld_18
              trunk
              clippy
              rust-analyzer
              rustfmt
              cargo
              rustc
              libffi
            ];
          };
        };
    };
}
