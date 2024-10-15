{
  description = "A stack-based array programming language";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.crane = {
    url = "github:ipetkov/crane";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, crane }: 
    flake-utils.lib.eachDefaultSystem (system:
    let 
      pkgs = nixpkgs.legacyPackages.${system};
      craneLib = crane.lib.${system};
      uiua-crate = craneLib.buildPackage {
        src = craneLib.cleanCargoSource (craneLib.path ./.);
         buildInputs = nixpkgs.lib.optionals pkgs.stdenv.isDarwin [
           pkgs.iconv
           pkgs.darwin.apple_sdk.frameworks.AppKit
           pkgs.darwin.apple_sdk.frameworks.CoreServices
           pkgs.darwin.apple_sdk.frameworks.Foundation
        ];
      };
    in
      {
        packages.default = uiua-crate;
        devShell = pkgs.mkShell {
          inputsFrom = builtins.attrValues self.packages.${system};
          nativeBuildInputs = [
            pkgs.clippy
            pkgs.rust-analyzer
            pkgs.rustfmt
          ];
        };
      });
}
