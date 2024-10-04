{
  craneLib,
  stdenv,
  lib,
  pkg-config,
  libffi,
  libiconv,
  darwin,
  doCheck ? true,
}:
let
  commonArgs = {
    src = lib.fileset.toSource {
      root = ./.;
      fileset = lib.fileset.unions (
        [
          (lib.fileset.fromSource (craneLib.cleanCargoSource ./.))
          ./src/primitive/assets
          ./site/Uiua386.ttf
        ]
        ++ lib.optionals doCheck [
          ./site/favicon.ico
          ./tests
        ]
      );
    };
    strictDeps = true;
    cargoExtraArgs = "--features system";
    inherit doCheck;
    nativeBuildInputs = [ pkg-config ];
    buildInputs =
      [ libffi ]
      ++ lib.optionals stdenv.isDarwin (
        with darwin.apple_sdk.frameworks;
        [
          libiconv
          AppKit
          Foundation
        ]
      );
  };
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;
  totalArgs = commonArgs // {
    inherit cargoArtifacts;
    cargoTestExtraArgs = "-- --skip format::generate_format_cfg_docs";
  };
in
craneLib.buildPackage totalArgs
