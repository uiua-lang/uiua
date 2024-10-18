ThisFileDir
┌─╴test
  ThisFileDir
└─╴
⍤⤙≍

# Imports
Ex ~ "example.ua"
  Foo  ← Ex~Foo
  Incr ← Ex~Increment
  Dub  ← Ex~Double
⍤⤙≍ 12 Dub Incr Foo
~ "example" ~ Square
⍤⤙≍ 25 Square 5
⍤⤙≍ 15 Ex~Mac!+

~ "example.ua" ~ Increment
~ "example.ua" ~ Increment
~ "example.ua" ~ Increment
⍤⤙≍ 7 Increment Increment 5
F! ← ^0
⍤⤙≍ 15 Ex~Mac!F!+

# Scoped
┌─╴M
  F ← +1
└─╴
N ↚ M
⍤⤙≍ 6 N~F 5
⍤⤙≍ 6 N!F 5

┌─╴M ~ X
  X ← +
└─╴
┌─╴N ~ Y
  Y ← X X
└─╴
⍤⤙≍ 6 Y 1 2 3

┌─╴A
  ┌─╴B ~ C
    C ← 5
  └─╴
  D ← C
└─╴
⍤⤙≍ 5 A~B~C
⍤⤙≍ 5 A~D
