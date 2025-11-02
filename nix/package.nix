{
  craneLib,
  makeBinaryWrapper,
  stdenv,
  lib,
  pkg-config,
  libffi,
  alsa-lib,
  rustPlatform,
  libGL,
  libxkbcommon,
  wayland,
  xorg,
  doCheck ? true,
}:
let
  libPath = lib.makeLibraryPath (
    lib.optionals stdenv.hostPlatform.isLinux [
      libGL
      libxkbcommon
      wayland
      xorg.libX11
      xorg.libXcursor
      xorg.libXi
      xorg.libXrandr
    ]
  );
  commonArgs = {
    src = lib.fileset.toSource {
      root = ../.;
      fileset = lib.fileset.unions (
        [
          (lib.fileset.fromSource (craneLib.cleanCargoSource ../.))
          ../src/assets
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
    buildInputs = [ libffi ] ++ lib.optionals stdenv.isLinux [ alsa-lib ];
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
    passthru = {
      inherit libPath;
    };
  };
in
craneLib.buildPackage totalArgs
