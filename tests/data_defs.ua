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

~Foo [Bar Baz ← +]
⍤⤙≍ [5 5] Foo 5 2 3

┌─╴Foo
  |Bar {a}
  |Baz {a}
└─╴
⍤⤙≍ 5 Foo~Bar~a Foo~Bar 5
