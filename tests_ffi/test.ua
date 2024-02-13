# Experimental!

LibPath ← ⊡1&args
Lib ← &ffi ⊂LibPath

⍤⟜≍: 8 Lib {"int" "add" "int" "int"} {3 5}
⍤⟜≍: 13 Lib {"int" "strlen" "const char*"} {"Hello, World!"}
⍤⟜≍: [3 2 1] Lib {"void" "reverse" "int:1" "int"} {[1 2 3]}
⍤⟜≍: [3 2 1] Lib {"int:1" "reversed" "const int:1" "int"} {[1 2 3]}
⍤⟜≍: {1 [2 3]} Lib {"int" "head_tail" "int:1" "int*"} {[1 2 3]}
VecII ← "{double; double}"
⍤⟜≍: 5 Lib {"double" "vec2_len" VecII} {[3 4]}
⍤⟜≍: 5 Lib {"double" "vec2_len_ref" $"const _*"VecII} {[3 4]}
⍤⟜≍: [4 6] Lib {VecII "vec2_add" VecII VecII} {[1 2] [3 4]}
⍤⟜≍: [1 0] Lib {"void" "vec2_normalize" $"_*" VecII} {[5 0]}
⍤⟜≍: {"Dave" 31} Lib {"{const char*; int}" "person_new" "const char*" "int"} {"Dave" 31}
⍤⟜≍: {"Dave" 31} Lib {"{const char*; int}*" "person_new_ptr" "const char*" "int"} {"Dave" 31}
