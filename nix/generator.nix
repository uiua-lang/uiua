{
  craneLib,
  lib,
}:
let
  commonArgs = {
    pname = "uiua-generator";
    src = lib.fileset.toSource {
      root = ../.;
      fileset = lib.fileset.unions [
        (lib.fileset.fromSource (craneLib.cleanCargoSource ../.))
        ../src/assets
        ../generator
        # TODO: isolate further
        ../changelog.md
        ../site/blog/list.txt
        ../site/text/idioms.ua
      ];
    };
    strictDeps = true;
    cargoExtraArgs = "--package uiua-generator";
  };
  cargoArtifacts = craneLib.buildDepsOnly commonArgs;
  totalArgs = commonArgs // {
    inherit cargoArtifacts;
  };
in
craneLib.buildPackage totalArgs
