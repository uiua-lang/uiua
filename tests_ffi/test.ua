# Experimental!

LibPath ← ⊡1&args
Lib ← &ffi ⊂LibPath

⍤⊃⋅∘≍ 8 Lib {"int" "add" "int" "int"} {3 5}
⍤⊃⋅∘≍ 13 Lib {"int" "strlen" "const char*"} {"Hello, World!"}
⍤⊃⋅∘≍ [3 2 1] Lib {"int:1" "reversed" "const int:1" "int"} {[1 2 3]}
