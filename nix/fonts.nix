{
  lib,
  runCommandLocal,
}:
runCommandLocal "uiua-fonts"
  {
    src = lib.fileset.toSource {
      root = ../site;
      fileset = lib.fileset.fileFilter ({ hasExt, ... }: hasExt "ttf") ../site;
    };
    uiua386 = ../src/algorithm/Uiua386.ttf;
  }
  ''
    mkdir -p "$out/share/fonts/truetype"
    cp "$src"/* "$out/share/fonts/truetype/"
    cp "$uiua386" "$out/share/fonts/truetype/Uiua386.ttf"
  ''
