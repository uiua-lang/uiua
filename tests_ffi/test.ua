# Experimental!

LibPath ← ⊡1&args
Lib     ← &ffi ⊂LibPath # Track caller!

Ptr ← $"_*"
Out ← $"out _"

⍤⤙≍ 8 Lib {"int" "add" "int" "int"} {3 5}
⍤⤙≍ 13 Lib {"int" "strlen" "char*"} {"Hello, World!"}
⍤⤙≍ [3 2 1] Lib {"void" "reverse" "out int:1" "int"} {[1 2 3]}
⍤⤙≍ [0 1 2 3] Lib {"void" "multi_list" "int:1" "int" "int:3" "int" "out int:5" "int"} {1_2 3_4 [0 0 0 0]}
⍤⤙≍ □₁"Hello, World!" Lib {"void" "change_string" "out char**"} {¤"xxx"}
⍤⤙≍ □₁"1 + 2 = 3" Lib {"void" "change_string_to_sum" "int" "int" "out char**"} {1 2 ¤"xxx"}

Vec₂ ← "double[2]"
⍤⤙≍ 5 Lib {"double" "vec2_len" Vec₂} {[3 4]}
⍤⤙≍ 5 Lib {"double" "vec2_len_ref" Ptr Vec₂} {¤[3 4]}
⍤⤙≍ [4 6] Lib {Vec₂ "vec2_add" Vec₂ Vec₂} {[1 2] [3 4]}
⍤⤙≍ [1 0] Lib {"void" "vec2_normalize" Out Vec₂} {[5 0]}
⍤⤙≍ [9 12] Lib {Vec₂ "vec2_list_sum" $"_:1"Vec₂ "int"} {[1_2 3_4 5_6]}
⍤⤙≍ [5_6 3_4 1_2] Lib {"void" "vec2_list_reverse" Out $"_:1"Vec₂ "int"} {[1_2 3_4 5_6]}

Person ← "{char*; int}"
⍤⤙≍ {"Dave" 31} Lib {Person "person_new" "char*" "int"} {"Dave" 31}
⍤⤙≍ [{"Dave Jr." 6} {"Daveina" 4}] &memcpy⊙2 Lib {Ptr Person "person_children" "char*" "int"} {"Dave" 31}
⍤⤙≍ 31 Lib {"int" "person_val_age" Person} {{"Dave" 31}}
⍤⤙≍ 31 Lib {"int" "person_ptr_age" Ptr Person} {¤{"Dave" 31}}
TwoPeople ← $"_[2]" Person
⍤⤙≍ {{"a" 1} {"b" 2}} Lib {TwoPeople "two_people_new" Person Person} {{"a" 1} {"b" 2}}

⍤⤙≍ [1 2 3] ⊃&memfree(&memcpy⊙3) Lib {"int*" "array_ptr" "int:1" "int"} {[1 2 3]}

MD₅ ← Lib {"unsigned char*" "dummy_md5" "unsigned char:1" "unsigned long" "unsigned char*"}
⍤⤙≍ "hello" +@\0&memcpy⊙5 MD₅ {"hello" NULL}
MD₅ ← Lib {"unsigned char*" "dummy_md5" "unsigned char:1" "unsigned long" "out unsigned char*"}
⍤⤙≍ {"hello" NULL} ⍜(°□⊢|+@\0&memcpy⊙5) MD₅ {"hello" NULL}
⍤⤙≍ {⟜∘ utf₈"hello"} ⍜(°□⊢|&memcpy⊙5) MD₅ {"hello" ▽5 0}

PtrStruct ← "{int*; int*}"
⍤⤙≍ {NULL NULL} Lib {PtrStruct "make_void_struct" "int"} {5}
°□₂ Lib {PtrStruct "make_void_struct_a" "int"} {5}
⍤⤙≍ [5] ⊃&memfree(&memcpy⊙1)
⍤⤙≍ NULL

⍤⤙≍ 1 Lib {"unsigned char" "first_byte" "unsigned char*"} {[1 2 3]}
Bytes ← "{unsigned char*}"
⍤⤙≍ 1 Lib {"unsigned char" "bytes_first_byte" Bytes} {{[1 2 3]}}

⍤⤙≍ "0x" ↙2⊢pretty Lib {"int*" "pointerify" "int"} {5}

⍤⤙≍ "0x5" Lib {"char*" "fmt_ptr" "void*"} {5}
