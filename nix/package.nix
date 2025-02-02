{
  craneLib,
  libPath,
  makeBinaryWrapper,
  stdenv,
  lib,
  pkg-config,
  libffi,
  libiconv,
  alsa-lib,
  darwin,
  rustPlatform,
  doCheck ? true,
}:
let
  commonArgs = {
    src = lib.fileset.toSource {
      root = ../.;
      fileset = lib.fileset.unions (
        [
          (lib.fileset.fromSource (craneLib.cleanCargoSource ../.))
          ../src/assets
          ../src/algorithm/Uiua386.ttf
        ]
        ++ lib.optionals doCheck [
          ../site/favicon.ico
          ../tests
          ../tests_special
        ]
      );
    };
    strictDeps = true;
    cargoExtraArgs = "--features system,full";
    inherit doCheck;
    nativeBuildInputs = [
      pkg-config
      rustPlatform.bindgenHook
      makeBinaryWrapper
    ];
    buildInputs =
      [ libffi ]
      ++ lib.optionals stdenv.isLinux [ alsa-lib ]
      ++ lib.optionals stdenv.isDarwin (
        with darwin.apple_sdk.frameworks;
        [
          libiconv
          AppKit
          Foundation
          CoreAudio
          CoreMedia
          AVFoundation
        ]
      );
    # https://crane.dev/faq/rebuilds-bindgen.html
    env.NIX_OUTPATH_USED_AS_RANDOM_SEED = "uiuarustbg";
  };
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;
  totalArgs = commonArgs // {
    inherit cargoArtifacts;
    cargoTestExtraArgs = "-- --skip format::generate_format_cfg_docs";
    postInstall = ''
      wrapProgram "$out/bin/uiua" --prefix LD_LIBRARY_PATH : "${libPath}"
    '';
  };
in
craneLib.buildPackage totalArgs
