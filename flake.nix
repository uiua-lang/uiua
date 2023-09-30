{
  description = "A stack-based array programming language";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "uiua";
          version = "0.0.5";
          src = ./.;
          cargoHash = "sha256-yoOByt66brm7YTGlD7kqqMxgsicO29vVPczhMIBjoA8=";
        };
      });
}
