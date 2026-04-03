&pf "0"
# Experimental!
&pf "1"
LibPath ← ⊡1&args
&pf "2"
Lib     ← &ffi ⊂LibPath # Track caller!
&pf "3"
Ptr ← $"_*"
&pf "4"
Out ← $"out _"
&pf "5"
⍤⤙≍ 8 Lib {"int" "add" "int" "int"} {3 5}
&pf "6"
⍤⤙≍ 13 Lib {"int" "strlen" "char*"} {"Hello, World!"}
&pf "7"
⍤⤙≍ [3 2 1] Lib {"void" "reverse" "out int:1" "int"} {[1 2 3]}
&pf "8"
⍤⤙≍ [0 1 2 3] Lib {"void" "multi_list" "int:1" "int" "int:3" "int" "out int:5" "int"} {1_2 3_4 [0 0 0 0]}
&pf "9"
⍤⤙≍ □₁"Hello, World!" Lib {"void" "change_string" "out char**"} {¤"xxx"}
&pf "10"
⍤⤙≍ □₁"1 + 2 = 3" Lib {"void" "change_string_to_sum" "int" "int" "out char**"} {1 2 ¤"xxx"}
&pf "11"
Vec₂ ← "double[2]"
&pf "12"
⍤⤙≍ 5 Lib {"double" "vec2_len" Vec₂} {[3 4]}
&pf "13"
⍤⤙≍ 5 Lib {"double" "vec2_len_ref" Ptr Vec₂} {¤[3 4]}
&pf "14"
⍤⤙≍ [4 6] Lib {Vec₂ "vec2_add" Vec₂ Vec₂} {[1 2] [3 4]}
&pf "15"
⍤⤙≍ [1 0] Lib {"void" "vec2_normalize" Out Vec₂} {[5 0]}
&pf "16"
⍤⤙≍ [9 12] Lib {Vec₂ "vec2_list_sum" $"_:1"Vec₂ "int"} {[1_2 3_4 5_6]}
&pf "17"
⍤⤙≍ [5_6 3_4 1_2] Lib {"void" "vec2_list_reverse" Out $"_:1"Vec₂ "int"} {[1_2 3_4 5_6]}
&pf "18"
Person ← "{char*; int}"
&pf "19"
⍤⤙≍ {"Dave" 31} Lib {Person "person_new" "char*" "int"} {"Dave" 31}
&pf "20"
⍤⤙≍ [{"Dave Jr." 6} {"Daveina" 4}] &memcpy⊙2 Lib {Ptr Person "person_children" "char*" "int"} {"Dave" 31}
&pf "21"
⍤⤙≍ 31 Lib {"int" "person_val_age" Person} {{"Dave" 31}}
&pf "22"
⍤⤙≍ 31 Lib {"int" "person_ptr_age" Ptr Person} {¤{"Dave" 31}}
&pf "23"
TwoPeople ← $"_[2]" Person
&pf "24"
⍤⤙≍ {{"a" 1} {"b" 2}} Lib {TwoPeople "two_people_new" Person Person} {{"a" 1} {"b" 2}}
&pf "25"
⍤⤙≍ [1 2 3] ⊃&memfree(&memcpy⊙3) Lib {"int*" "array_ptr" "int:1" "int"} {[1 2 3]}
&pf "26"
MD₅ ← Lib {"unsigned char*" "dummy_md5" "unsigned char:1" "unsigned long" "unsigned char*"}
&pf "27"
⍤⤙≍ "hello" +@\0&memcpy⊙5 MD₅ {"hello" NULL}
&pf "28"
MD₅ ← Lib {"unsigned char*" "dummy_md5" "unsigned char:1" "unsigned long" "out unsigned char*"}
&pf "29"
⍤⤙≍ {"hello" NULL} ⍜(°□⊢|+@\0&memcpy⊙5) MD₅ {"hello" NULL}
&pf "30"
⍤⤙≍ {⟜∘ utf₈"hello"} ⍜(°□⊢|&memcpy⊙5) MD₅ {"hello" ▽5 0}
&pf "31"
PtrStruct ← "{int*; int*}"
&pf "32"
⍤⤙≍ {NULL NULL} Lib {PtrStruct "make_void_struct" "int"} {5}
&pf "33"
°□₂ Lib {PtrStruct "make_void_struct_a" "int"} {5}
&pf "34"
⍤⤙≍ [5] ⊃&memfree(&memcpy⊙1)
&pf "35"
⍤⤙≍ NULL
&pf "36"
⍤⤙≍ 1 Lib {"unsigned char" "first_byte" "unsigned char*"} {[1 2 3]}
&pf "37"
Bytes ← "{unsigned char*}"
&pf "38"
⍤⤙≍ 1 Lib {"unsigned char" "bytes_first_byte" Bytes} {{[1 2 3]}}
&pf "39"
⍤⤙≍ "0x" ↙2⊢pretty Lib {"int*" "pointerify" "int"} {5}
&pf "40"
⍤⤙≍ "0x5" Lib {"char*" "fmt_ptr" "void*"} {5}
&pf "41"
⍤⤙≍ 1 Lib {"bool" "is_null_ptr" "void*"} {NULL}
&pf "42"
⍤⤙≍ 0 Lib {"bool" "is_null_ptr" "void*"} {&malloc 16}
&pf "43"
⍤⤙≍ 1 ⍣(1 &memfree &malloc 16|0)
&pf "44"
⍤⤙≍ [0 1 2 3 4 5 6 7] &memfree ⟜⊃(|&memcpy|◌Lib {"void" "test_fill_ptr" "byte*" "byte"} □₂) ⊸&malloc 8
