# Test combinations of features

Command     ← {"cargo" "clippy" "--no-default-features"}
ClippyArgs  ← {"--deny=warnings"}
AllowNotAll ← {"--allow=unused"}
# Success ? Package Features UseAll
Run ← (
  ⊸(&p $"Checking package _")
  ⊂Command {"--package" ∘}
  ⊙(
    ˜⧅⋅⋅1⇡2⊸⧻
    ≡⌟{"--features" /$"_,_"▽}
    ⍚(⊂□⨬$"features: _"⋅"no features"=₀⊸⧻°□⊸⊣)
    ⍚˜⊂{⊂₃{"--"} ClippyArgs AllowNotAll}
    ⊂⊙(˜▽{⊂{"all features" "--all-features" "--"} ClippyArgs})
  )
  ≡⌞(
    ⊙◇°⊂
    ⊙(&p $"Checking with _")
    &runi ⊂
  )
  /×=₀
)
/◇⊂{
  ¤Run "uiua" (
    {"audio" "tls" "binary"}
    ⊂⨬(
      {"gif,image,terminal_image,webcam" "lsp,raw_mode"}
    | {"audio_encode" "gif" "image" "terminal_image" "lsp" "native_sys" "raw_mode"}
    ) ˜∊□"all" &args
  ) 1
  ≡⌟₂◇Run {"site" "uiua-editor" "tests_ffi" "uiua_parser"} {} 0
}
⍤"One or more invocations failed" /×
