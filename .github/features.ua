# Test combinations of features

Command ← {"cargo" "c" "--lib" "--no-default-features"}
Features ← (
  {"audio" "bytes" "https"}
  ⊂(
    {"gif,image,terminal_image" "lsp,raw_mode"}
  | {"audio_encode" "gif" "image" "terminal_image" "lsp" "native_sys" "raw_mode"}
  ) ∊□"all" &args
)

&p "Checking with no features"
⍤⊃⋅∘≍ 0 &runi Command

↘1⋯⇡ⁿ:2⧻. Features
≡(
  ⬚"regex"/$"_,_" ▽
  &p $"Checking with features: _".
  ⊂ Command ⊂ □"--features" □
  ⍤⊃⋅∘≍ 0 &runi
)⊙¤
