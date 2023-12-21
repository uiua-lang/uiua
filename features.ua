# Test combinations of features

Command ← {"cargo" "c" "--lib" "--no-default-features"}

&p "Checking with no features"
⍤⊃⋅∘≍ 0 &runi Command

# Features to try
{"bytes" "lsp" "terminal_image" "raw_mode" "audio"}
↘1⋯⇡ⁿ:2⧻.
≡(
  ⊐/$"_,_" ▽
  &p $"Checking with features: _".
  ⊂ Command ⊂ □"--features" □
  ⍤⊃⋅∘≍ 0 &runi
)⊙¤
