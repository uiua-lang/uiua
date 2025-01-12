# Experimental!
~Foo {Bar: °1type|Baz: ×2 ← +1}
⍤⤙≍ {@a 6} {Foo!⊃Bar Baz} Foo @a 5
⍤⤙≍ Foo @a 9 °⊸Foo~Baz 10 Foo @a 5

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

~Quad [a b c] ÷×2a -b ⊟¯.√ℂ0 -/×4_a_c ×.b
⍤⤙≍ ℂ0 1_2 Quad 1 ¯3 2

~Foo [x] +x
⍤⤙≍ 8 Foo 3 5

~Foo [x] ⊟ x /+ 1_2_3
⍤⤙≍ [10 6] Foo 10

~Foo [Bar Baz ← +]
⍤⤙≍ [5 5] Foo 5 2 3

┌─╴Foo
  |Bar {a}
  |Baz {a}
└─╴
⍤⤙≍ 5 Foo~Bar~a Foo~Bar 5

┌─╴S
  ~{A B C}
  PopA  ← :⍜A(:°(⊂:))
  PushB ← ⍜⊙B(⊂:)
└─╴
S [1 2 3] [4 5 6] [7 8 9]
S~PopA
S~PushB
⍤⤙≍ S [1 2] [4 5 6 3] [7 8 9]

~Foo {A: °0type|B: °0type}
⍤⤙≍ [7 42] ⊟ °Foo Foo 7 42
~Foo [A: °0type|B: °0type]
⍤⤙≍ [7 42] ⊟ °Foo Foo 7 42

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

┌─╴Foo
  ~{Bar Baz}
  ~AddBar     ← +Bar
  ~Sum        ← AddBar Baz
  ~WithBarSum ← Foo~Sum New 10 Baz
  ~WithBar    ← Self °Bar
  ~IncrBar    ← Self⍜Bar+₁
  ~Add        ← Foo ⊃(+Bar Foo~Bar|+Baz Foo~Baz)
└─╴
⍤⤙≍ 13 Foo~AddBar 10 Foo 3 5
⍤⤙≍ 8 Foo~Sum Foo 3 5
⍤⤙≍ 15 Foo~WithBarSum Foo 3 5
⍤⤙≍ Foo 10 5 Foo~WithBar 10 Foo 3 5
⍤⤙≍ Foo 4 5 Foo~IncrBar Foo 3 5
⍤⤙≍ Foo 23 15 Foo~Add Foo 20 10 Foo 3 5
