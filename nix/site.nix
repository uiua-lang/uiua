{
  craneLib,
  lib,
  wasm-bindgen-cli_0_2_100,
  doCheck ? true,
}:
let
  commonArgs = {
    pname = "uiua-site";
    src = lib.fileset.toSource {
      root = ../.;
      fileset = lib.fileset.unions [
        (lib.fileset.fromSource (craneLib.cleanCargoSource ../.))
        ../src/assets
        ../site
        ../changelog.md
      ];
    };
    strictDeps = true;
    inherit doCheck;
    cargoExtraArgs = "--package=site";
    env.CARGO_BUILD_TARGET = "wasm32-unknown-unknown";
    # https://crane.dev/faq/rebuilds-bindgen.html
    env.NIX_OUTPATH_USED_AS_RANDOM_SEED = "uiuarustbg";
  };
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;
  totalArgs = commonArgs // {
    inherit cargoArtifacts;
    preBuild = ''
      cd ./site/
    '';
    postBuild = ''
      cd ..
      mv ./docs/ ./dist
    '';
    wasm-bindgen-cli = wasm-bindgen-cli_0_2_100;
  };
in
craneLib.buildTrunkPackage totalArgs
