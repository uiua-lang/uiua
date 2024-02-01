# Experimental!

LibPath ← ⊡1&args
Lib ← &ffi ⊂LibPath

⍤⊃⋅∘≍ 8 Lib {"int" "add" "int" "int"} {3 5}
⍤⊃⋅∘≍ 13 Lib {"int" "strlen" "char*"} {"Hello, World!"}
