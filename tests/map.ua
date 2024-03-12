# Experimental!

{1 3 "hi" [1 2 3]}
{2 4 "there" [4 5 6]}
map:
MapA ←
MapB ← insert 5 6 MapA
MapsI ← MapA MapB

∩(⍤. has 1) MapsI
∩(⍤. has 3) MapsI
∩(⍤. has "hi") MapsI
∩(⍤. has [1 2 3]) MapsI

∩(⍤⟜≍: □2 get 1) MapsI
∩(⍤⟜≍: □4 get 3) MapsI
∩(⍤⟜≍: □"there" get "hi") MapsI
∩(⍤⟜≍: □[4 5 6] get [1 2 3]) MapsI

MapsII ← ∩(remove 1) MapsI
∩(⍤. ¬has 1) MapsII
∩(⍤. has 3) MapsII
∩(⍤. has "hi") MapsII
∩(⍤. has [1 2 3]) MapsII
⍤⟜≍: {{3 "hi" [1 2 3]} {4 "there" [4 5 6]} {3 "hi" [1 2 3] 5} {4 "there" [4 5 6] 6}} {∩°map MapsII}

⍤⟜≍: 5 get 1_2 map [1_2 3_4] 5_6
⍤⟜≍: 6 get 3_4 map [1_2 3_4] 5_6

⍤⟜≍: map 1_2_3 10_5_6 insert 1 10 map 1_2_3 4_5_6

⍤⟜≍: [2 3 4 1] ≡get[2 3 4 5]¤ ∧(insert) [5 2 3 4] [1 2 3 4] map[][]

⍤⟜≍: map 1_2_3 6_10_15 ≡(/+⇡) map 1_2_3 4_5_6
⍤⟜≍: map 1_2_3 6_10_15 ∵(/+⇡) map 1_2_3 4_5_6
⍤⟜≍: map 1_2_3 14_25_36 ≡+ 10_20_30 map 1_2_3 4_5_6
⍤⟜≍: ⟜⍉ map 1_2_3 [4_5 6_7 8_9]

⍤⟜≍: [1_2_5_7 3_4_7_8] [°map] ⊂ map 1_2 3_4 map 5_7 7_8
⍤⟜≍: [2_3_4_1 6_7_8_5] [°map] ↻1 map 1_2_3_4 5_6_7_8
⍤⟜≍: [2_3_4 6_7_8] [°map] ↘1 map 1_2_3_4 5_6_7_8
⍤⟜≍: [[1] [5]] [°map] ↙1 map 1_2_3_4 5_6_7_8
