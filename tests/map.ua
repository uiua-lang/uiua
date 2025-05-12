{1 3 "hi" [1 2 3]}
{2 4 "there" [4 5 6]}
˜map
MapA  ←
MapB  ← insert 5 6 MapA
MapsI ← MapA MapB

∩(⍤. has 1) MapsI
∩(⍤. has 3) MapsI
∩(⍤. has "hi") MapsI
∩(⍤. has [1 2 3]) MapsI

∩(⍤⤙≍ □2 get 1) MapsI
∩(⍤⤙≍ □4 get 3) MapsI
∩(⍤⤙≍ □"there" get "hi") MapsI
∩(⍤⤙≍ □[4 5 6] get [1 2 3]) MapsI

MapsII ← ∩(remove 1) MapsI
∩(⍤. ¬has 1) MapsII
∩(⍤. has 3) MapsII
∩(⍤. has "hi") MapsII
∩(⍤. has [1 2 3]) MapsII
⍤⤙≍ {{3 "hi" [1 2 3]} {4 "there" [4 5 6]} {3 "hi" [1 2 3] 5} {4 "there" [4 5 6] 6}} {∩°map MapsII}

⍤⤙≍ 5 get 1_2 map [1_2 3_4] 5_6
⍤⤙≍ 6 get 3_4 map [1_2 3_4] 5_6

⍤⤙≍ map 1_2_3 10_5_6 insert 1 10 map 1_2_3 4_5_6

⍤⤙≍ [2 3 4 1] ≡get[2 3 4 5]¤ ∧(insert) [5 2 3 4] [1 2 3 4] map[][]
⍤⤙≍ map"abc" [1_2 9_9 5_6] insert @b [9 9] map "abc" [1_2 3_4 5_6]

⍤⤙≍ map 1_2_3 6_10_15 ≡(/+⇡) map 1_2_3 4_5_6
⍤⤙≍ map 1_2_3 6_10_15 ∵(/+⇡) map 1_2_3 4_5_6
⍤⤙≍ map 1_2_3 14_25_36 ≡+ 10_20_30 map 1_2_3 4_5_6
⍤⤙≍ ⍉⟜(⍉ map 1_2_3) [4_5 6_7 8_9]

⍤⤙≍ [1_2_5_7 3_4_7_8] [°map] ⊂ map 1_2 3_4 map 5_7 7_8
⍤⤙≍ [2_3_4_1 6_7_8_5] [°map] ↻1 map 1_2_3_4 5_6_7_8
⍤⤙≍ [2_3_4 6_7_8] [°map] ↘1 map 1_2_3_4 5_6_7_8
⍤⤙≍ [[1] [5]] [°map] ↙1 map 1_2_3_4 5_6_7_8

⍤⤙≍ 0 ≍ ⟜map [1 2 3] [4 5 6]
⍤⤙≍ 1 ≍ .map [1 2 3] [4 5 6]

⍤⤙≍ map [1] {"a"} insert 1 □"a" map 1 □"a"

# Fix
⍤⤙≍ ≡¤[4 5 6] ⊞get 1_2_3 ¤ map 1_2_3 4_5_6
⍤⤙≍ [4 5 6] ≡get 1_2_3 ¤ map 1_2_3 4_5_6
⍤⤙≍ ↯3≡¤ [4 5 6] ⊞⋅get . 1_2_3 ¤ map1_2_3 4_5_6

# Empty keys
⍤⟜≍ insert[][] map[]°△0_0 insert[][] °△0_0
⍤⤙≍ 5 get [] insert [] 5 map°△0_0 []
⍤⤙≍ 5 get [] insert [] 5 insert[]3 map°△0_0[]

# Weird probe patterns
map {"x" "y" "z"} [1 2 3]
remove "y"
insert "z" 4
⍤⤙≍ map {"x" "z"} [1 4]
⍤⤙≍ ⤙⍜remove∘ @a map "abc" [1 2 3]
{"a" "b" "f" "v" "r" "l" "c" "d" "e" "g" "h" "i" "j" "k" "m" "18" "1" "zh" "qvq" "dp" "sh" "D" "C" "A" "B" "kmsq"}
map ⟜(⇡⧻)
remove "zh"
insert "kmsq" 0
⍤⤙≍ ∩⧻°map
map {"a" "b" "c"} [1 2 3]
remove "b"
insert "d" 4
insert "e" 42
⍤⤙≍ map {"a" "c" "d" "e"} [1 3 4 42]

# Duplicate init keys
⍤⤙≍ map [0 1] [3 4] map [0 0 0 1] [1 2 3 4]

# Sort correctness
map "abc" [2 3 1]
map "cba" [1 3 2]
⍤⤙≍ [1 1] [∩≍] ⊃∩⍆∩(⇌⍆)

# Selection
⍤⤙≍ ⤙⊏⇡3 map "abc" [1 2 3]
⍤⤙≍ ⊃⇌(⊏⇌°⊏) map "abc" [1 2 3]

# Multi-key get
⍤⤙≍ "bdd" get 2_4_4 map [1 2 3 4] "abcd"
⍤⤙≍ "dcba" get 4_3_2_1 map [1 2 3 4] "abcd"

# # Multi-key insert (not currently allowed because of ambiguity)
# ⍤⤙≍ map 1_2 "ab" insert 1_2 "ab" map [] ""
# ⍤⤙≍ map [1_2] ["ab"] insert 1_2 "ab" map ⤙+@a °△0_2

# Multi-key remove
⍤⤙≍ map 1_3 "ac" remove 2_4 map [1 2 3 4] "abcd"
⍤⤙≍ map [3_4] "b" remove 1_2 map [1_2 3_4] "ab"

# Unmap type/shape
⍤⤙≍ {+@0°△0_2 []} {°map} remove "df"_"as" map "as"_"df" 1_2

⍤⤙≍ {[] map [¯1] [5]} ⨬⋅∘⍚(insert⊙5) [0 1] [¯1 ¯1] {[] []}

# Put in arrays
⍤⤙≍ ⊃¤[∘] map [1 2 3] [4 5 6]
⍤⤙≍ [3_4 7_8] [map 1_2 3_4 map 5_6 7_8]
