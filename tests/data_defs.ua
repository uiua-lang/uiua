┌─╴M
  |Foo {Bar Baz}
  |Qux [x y z]
  |Sed {M N}
  |Wir

  Format ← ⍣(
    $"_ and _" °Foo
  | $"⟨_ _ _⟩" °Qux
  | $"_: _" °Sed
  | "Wir!" °Wir
  )
└─╴
⍤⤙≍ "2 and 5" M~Format M~Foo 2 5
⍤⤙≍ "⟨0 4 1⟩" M~Format M~Qux 0 4 1
⍤⤙≍ "Name: Dan" M~Format M~Sed "Name" "Dan"
⍤⤙≍ "Wir!" M~Format M~Wir

~Foo [Bar Baz ← +]
⍤⤙≍ [5 5] Foo 5 2 3

┌─╴Foo
  |Bar {a}
  |Baz {a}
└─╴
⍤⤙≍ 5 Foo~Bar~a Foo~Bar 5

┌─╴S
  ~ {A B C}
  PopA  ← ˜⊙∘⍜A(˜⊙∘⍜⇌°⊂)
  PushB ← ⍜⊙B˜⊂
└─╴
S [1 2 3] [4 5 6] [7 8 9]
S~PopA
S~PushB
⍤⤙≍ S [1 2] [4 5 6 3] [7 8 9]

┌─╴Foo
  |Bar [A]
  |Baz [A]
  |Qux
└─╴
⍤⤙≍ 1 /↥⌕"Non-boxed variant" ⍣Foo~Bar⋅∘ @a
⍤⤙≍ 1 /↥⌕"Non-boxed variant" ⍣Foo~Baz⋅∘ [1 2 3]
⍤⤙≍ [0 5] Foo~Bar 5
⍤⤙≍ [1 5] Foo~Baz 5
⍤⤙≍ 2 Foo~Qux

~Foo [Bar]
⍤⤙≍ ¤[1 2 3] Foo [1 2 3]

~Foo {Bar ← 5}
F! ← ^0 Foo
⍤⤙≍ 5 F!Foo!Bar

~Foo {A B C}
Foo 1 2 3
⍤⤙⊸≍ Foo 3 2 1 ⍜Foo!⊃A C :
⍤⤙≍ Foo 4 5 6 °⊸Foo!⊃(C|B|A) 6 5 4

# Unnamed top-level def
~ {Foo Bar}
⍤⤙≍ [3 5] [⊃Foo Bar] New 3 5

# Import followed by def
~ "example" ~ Bar
~Foo {A B C}
⍤⤙≍ {1 2 3} Foo 1 2 3
⍤⤙≍ "bar" Bar

# Experimental!

# Field validators
~Foo {Bar: °1type|Baz: ×2 ← +1}
⍤⤙≍ {@a 6} {Foo!⊃Bar Baz} Foo @a 5
⍤⤙≍ Foo @a 9 °⊸Foo~Baz 10 Foo @a 5

~Foo {A: °0type|B: °0type}
⍤⤙≍ [7 42] ⊟ °Foo Foo 7 42
~Foo [A: °0type|B: °0type]
⍤⤙≍ [7 42] ⊟ °Foo Foo 7 42

# Data functions
~Foo [X ← 0] ⊟ ⊓X(/+ 1_2_3)
⍤⤙≍ [0 6] Foo
⍤⤙≍ [5 6] Foo X:5

~F {A ← 1|B ← 2|C ← 3} ∘
⍤⤙≍ {1 2 3} F
⍤⤙≍ {1 2 5} F C:5
⍤⤙≍ {0 2 5} F A:0 C:5
⍤⤙≍ {0 2 5} F C:5 A:0
⍤⤙≍ {0 9 5} F C:5 A:0 B:9

~F {A ← +|B C ← 0} ∘
⍤⤙≍ {3 4 0} F 1 2 4
⍤⤙≍ {3 4 5} F 1 C:5 2 4

~F {A ← 0|B C ← 0} ∘
M! ← F^0
⍤⤙≍ F A:2 3 M!A:2 3

┌─╴Range
  ~ {Min ← 0|Max Inclusive ← 0} ⍜-⇡ ⊃Min(+⊃Max Inclusive)
  ~Incl {Min ← 0} Call Inclusive:1 Min:Min
  APL ← Call Inclusive:1 Min:1
└─╴
⍤⤙≍ ⇡5 Range 5
⍤⤙≍ ⇡₁5 Range~APL 5
⍤⤙≍ ⇡₄6 Range Min:4 10
⍤⤙≍ ⇡₄7 Range~Incl Min:4 10

~Run {A ← 0|B ← 1|C} ≡°□
⍤⤙≍ [0 1 2] Run 2
⍤⤙≍ {2 1 5} Run~New A:2 5
⍤⤙≍ [2 8 5] Run~Args B:8 Run~New A:2 5

~F {A ← 0|B ← 0|C ← 0|D ← 0} ∘
⍤⤙≍ {2 3 4 1} F D:1 ⊓A:C: 2 B:3 4

┌─╴A
  Call ← +1
  B    ← A
└─╴
⍤⤙≍ 6 A~B 5
