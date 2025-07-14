{
  runCommand,
  generator,
}:
runCommand "uiua.tmLanguage.json" { nativeBuildInputs = [ generator ]; } ''
  uiua-generator sublime-grammar "$out"
''
