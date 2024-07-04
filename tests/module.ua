ThisFileDir
---test
ThisFileDir
---
⍤⟜≍:

# Imports
Ex ~ "example.ua"
  Foo ← Ex~Foo
  Incr ← Ex~Increment
  Dub ← Ex~Double
⍤⟜≍: 12 Dub Incr Foo
~ "example" ~ Square
⍤⟜≍: 25 Square 5
⍤⟜≍: 15 Ex~Mac!+

~ "example.ua" ~ Increment
~ "example.ua" ~ Increment
~ "example.ua" ~ Increment
⍤⟜≍: 7 Increment Increment 5
F! ← ^!
⍤⟜≍: 15 Ex~Mac!F!+
