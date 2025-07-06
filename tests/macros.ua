# Index macros
F! ← ^0 5
G! ← F!^0
⍤⤙≍ ¯5 G!¯
Four! ← ∩∩^0
⍤⤙≍ [4 6 8 10] [Four!(×2) 2 3 4 5]
Dyad‼ ← ^0^1
⍤⤙≍ 12 Dyad‼(×2|+1) 5
F‼‼ ← ^1^0^2^2^3^0
⍤⤙≍ [1 0 2 2 3 0] F‼‼(⊂0|⊂1|⊂2|⊂3) []

F! ← (
  ^0
  ^0
)
⍤⤙≍ 6 F!+ 1 2 3
F! ← [
  ^0
  ^0
]
⍤⤙≍ [6] F!+ 1 2 3
F! ← ⨬(
  ^0
| ^0
)
⍤⤙≍ ¯5 F!¯ 0 5

F! ← -^0 i
⍤⤙≍ i F!0

┌─╴M
  X  ← "Inner"
  A! ← ^0
  B! ← (^0)
└─╴
X ← "Outer"
⍤⤙≍ "Outer" M~A!X
⍤⤙≍ "Outer" M~B!X

# Code macros
F! ←^
G! ←^ ⇌
H‼ ←^ ⊂⊢.
⍤⤙≍ [1 2] F!(⊂1|⊂2) []
⍤⤙≍ [2 1] G!(⊂1|⊂2) []
⍤⤙≍ [1 1 2] H‼(⊂1|⊂2) []
⍤⤙≍ [1 1 2] H‼(⊂1)(⊂2) []

X  ← 1
F! ←^ ⊂□°⋕X
⍤⤙≍ [1 2] [F!2]

F‼ ←^ $"_ ← _\n" °⊟
F‼Y5
⍤⤙≍ 5 Y

Foo! ←^ "t ←rand" ◌
Hi   ← Foo!+"a"
⍤⤙≍ "a" Hi

a ← 4
b ← (⚂)
c ← comptime(⚂)
e ← gen [] 1

A! ←^ $"_" a ◌
B! ←^ $"_" b ◌
C! ←^ $"_" c ◌
E! ←^ $"_" e ◌

◌A!1
◌B!2
◌C!3
◌E!5
◌E!5 # Ensure evaluated constant is set

F! ←^ {"+" "1"}◌
⍤⤙≍ [2 3 4] ≡F!∘ [1 2 3]

Struct‼ ←^ $"---_\n_\n---" ⊙(
  /$"_\n_"⊂□$"New ← {_}"≡⋅@⊙⟜⍚$"_ ← °□⊡_"°˜⊏⊜□⊸≠@ °$"[_]"
)°{⊙∘}

Struct‼Person[Name Age][] # Ensure that a word on the same line as binding generation works
◌◌Person~New "Dave" 15

F‼ ←^
⍤⤙≍ [1 2 3] [F‼(1|2|3)]

M! ←^ ⋅"∘"
@a @b
⍤⤙≍ "b" [⋅M!∘]

Join! ←^ ↯⊙@⊂↥0-1⋕
{Join!0 "a" "bc" "def"}
{Join!1 "a" "bc" "def"}
{Join!2 "a" "bc" "def"}
◌◌◌

M! ←^ ˜⊂□"5" ⍚$"_ ←\n" °{$"[_]"/$"_ _"}
⇡3
⍤⤙≍ 5 M![A B C]
⍤⤙≍ ⇡3 A
⍤⤙≍ [] B []
⍤⤙≍ [] C []

M! ←^ /$"_\n_" ⊂□$"°⊟_"+@₀-@0°⋕⊸⧻ ⍚$"_ ← " °{$"[_]"/$"_ _"}

⇡3
M![D E F]
⍤⤙≍ [0] [D]
⍤⤙≍ [1] [E]
⍤⤙≍ [2] [F]

M![G H I] ⇡3
⍤⤙≍ [0] [G]
⍤⤙≍ [1] [H]
⍤⤙≍ [2] [I]

# Macro arg validation should ignore error from code macro args
┌─╴Wup
  X ← 5
└─╴
A! ←^ $"Wup!(_)" ⊢
B! ← A!X ^0
⍤⤙≍ 5 B!

# Modules should overwrite macros
Foo! ←^ repr⊢
┌─╴Foo
  X ← 5
└─╴
⍤⤙≍ 5 Foo!(X)

# Macros use correct identifiers
┌─╴M ~ F!
  X  ← 5
  F! ← +X^0
└─╴
X ← 3
⍤⤙≍ 8 F!X

# Inline macros
⍤⤙≍ [1 2 1 2] (^0^1^0^1)‼(⊂1|⊂2) []
⍤⤙≍ ≡⊂¤1_2_1_2 ⟜≡(^0^1^0^1)‼(⊂1|⊂2) [3 4 5]
⍤⤙≍ 12 (
  ^0
  +1
  ×2
)!5
F ← 5
⍤⤙≍ 5 (^0)!F

F! ←^ (
  G ← 5
  ⋅G
)
⍤⤙≍ 5 F!+

# These just need to parse
A₇₂₇! ← ^0
A⌞!   ← ^0
