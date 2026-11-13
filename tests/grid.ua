# Uniqueness

Unique! ←^ (
  ⧅<2
  /$"_\n_" ⍚(
    $"_\n_" ⊃(
      /$"¬≍ pretty _ pretty _"
    | /$"⍤$ (_) and (_) give ambiguous output"
    )
  )
)

Unique!(
  {5} | {□5} | {□□5}
| {[5]} | {□[5]} | {□□[5]}
| {{5}} | {□{5}}
)

Unique!(
  {1 2 3}
| {1 2_3}
| {1 {2 3}}
| {1 {2_3}}
)

Unique!(
  {°△2}
| {°△1_2}
| {°△2_1}
| {°△1_1_2}
| {°△2_1_1}
)

Unique!(
  {°△1_2_2}
| {°△2_1_2}
| {°△2_2_1}
)

Unique!(
  {°△2 °△2_2}
| {°△1_2 °△2_2}
| {°△1_1_2 °△2_2}
| {°△1_1_2 °△1_2_2}
)

Unique!(
  {"hi" °△2_2}
| {"hi" °△1_2_2}
| {¤"hi" °△2_2}
| {¤"hi" °△1_2_2}
)

Unique!({@a}|{□@a})

Unique!(
  {°△2_2_2 2}
| {°△2_2_2 ¤2}
| {°△2_2_2 ¤¤2}
| {°△2_2_2 ¤¤¤2}
)

Lines    ← ⌅(⬚@\s⊜∘ ⊸≠@\n|◌ ⬚@\n°⊜□)
Diff     ← °Lines ⍚$"_:\n_" {"Input" "Expected" "Got"} □₃ ⊓repr∩(°Lines ≡$"\"_\"")
Snapshot ← ⊃Diff⋅≍ ⟜⊓prettyLines

# SoA

$ ┌─        
$      a │  
$   ─────┤  
$   □b: 5│  
$      □6│  
$          ┘
⍤Snapshot {$a {$b 5 6}}

# Boxed fixed arrays
$ ┌─       
$   ¤"1"│  
$         ┘
⍤Snapshot {¤"1"}
$ ┌─        
$   ¤"23"│  
$          ┘
⍤Snapshot {¤"23"}
$ ┌─         
$   ¤¤"23"│  
$           ┘
⍤Snapshot {¤¤"23"}
$ ┌─          
$   ¤"23"│∙1  
$            ┘
⍤Snapshot {¤"23" 1}

# Labels
$ ┌─       
$    a│ b  
$   ∙5│∙6  
$         ┘
⍤Snapshot {$a 5 $b 6}

$ ┌─       
$    a│b   
$   ∙5│@x  
$         ┘
⍤Snapshot {$a 5 $b @x}

$ ┌─              
$   x        │ y  
$   0 1 2 3 4│∙5  
$                ┘
⍤Snapshot {$x⇡5 $y5}

$ ┌─      
$ ╷ ∙1 │  
$   ───┤  
$   2 3│  
$        ┘
⍤Snapshot ⍉¤{1 2_3}

$ ┌─      
$ ╷ 1  │  
$   ───┤  
$   2 3│  
$        ┘
⍤Snapshot ⍉¤{¤1 2_3}

$ ╭─
$   10000.125 →   1
$      10.25  →  10
$     100.5   → 100
$                   ╯
⍤Snapshot map [10000.125 10.25 100.5] [1 10 100]

# Basic nontrivial SoA
$ ┌─
$   foo │bar
$   ────┼───
$   "Aa"│0
$   "Bb"│1
$   "Cc"│2
$   "Dd"│3
$   "Ee"│4
$            ┘
⍤Snapshot □₂ ⊓$foo$bar ⊸≡⌞+"Aa" ⇡5

# "Compressed" string layout rules out SoA formatting for rank-1 text arrays
# The current state of this test documents a known bug.
$ ┌─
$   ╭─ foo │bar
$     ABC  │0 1 2
$         ╯│
$                 ┘
# $ ┌─
# $   foo  │bar
# $   "ABC"│0 1 2
# $               ┘
⍤Snapshot □₂ ⊓$foo$bar ⊸≡⌞+@A ⇡3

# Canonical example of the above bug
$ ┌─         
$ ╷ ╭─    │  
$     a   │  
$       ╯ │  
$   ──────┤  
$   ╭─    │  
$     bc  │  
$        ╯│  
$   ──────┤  
$   ╭─    │  
$     d   │  
$       ╯ │  
$           ┘
# $ ┌─
# $ ╷ "a" │
# $   ────┤
# $   "bc"│
# $   ────┤
# $   "d" │
# $         ┘
⍤Snapshot ≡¤{"a" "bc" "d"}

# Mismatching lengths rule out SoA formatting,
# but foo being multiline forces bar to follow suit
$ ┌─
$   foo │╭─ bar
$   "Aa"│  0 1 2 3 4 5
$   "Bb"│              ╯
$   "Cc"│
$   "Dd"│
$   "Ee"│
$                        ┘
⍤Snapshot □₂ ⊓$foo$bar ≡⌞+"Aa" ⊸↙5 ⇡6

# Hashmap rules out SoA formatting
$ ┌─
$   ╭─ foo     │bar
$     @a → @a  │0 1 2 3
$     @b → @b  │
$     @c → @c  │
$     @d → @d  │
$             ╯│
$                       ┘
⍤Snapshot □₂ ˙map ⊓$foo$bar ⊸+@a ⇡4

# High-rank non-box array
$ ╭─
$ ╷  0  1   4  5   16 17  20 21
$ ╷  2  3   6  7   18 19  22 23
$ ╷
$ ╷  8  9  12 13   24 25  28 29
$ ╷ 10 11  14 15   26 27  30 31
$                              
$                              
$   32 33  36 37   48 49  52 53
$   34 35  38 39   50 51  54 55
$                              
$   40 41  44 45   56 57  60 61
$   42 43  46 47   58 59  62 63
$                               ╯
⍤Snapshot °△ ↯6 2

# High-rank box array
$ ┌─                                  
$ ╷ [] │1  ║4  │5  ││1 6│1 7║2 0│2 1  
$ ╷ ───┼───╫───┼───┤├───┼───╫───┼───  
$ ╷ 2  │3  ║6  │7  ││1 8│1 9║2 2│2 3  
$ ╷ ═══╪═══╬═══╪═══╡╞═══╪═══╬═══╪═══  
$ ╷ 8  │9  ║1 2│1 3││2 4│2 5║2 8│2 9  
$   ───┼───╫───┼───┤├───┼───╫───┼───  
$   1 0│1 1║1 4│1 5││2 6│2 7║3 0│3 1  
$   ───┴───╨───┴───┘└───┴───╨───┴───  
$   ───┬───╥───┬───┐┌───┬───╥───┬───  
$   3 2│3 3║3 6│3 7││4 8│4 9║5 2│5 3  
$   ───┼───╫───┼───┤├───┼───╫───┼───  
$   3 4│3 5║3 8│3 9││5 0│5 1║5 4│5 5  
$   ═══╪═══╬═══╪═══╡╞═══╪═══╬═══╪═══  
$   4 0│4 1║4 4│4 5││5 6│5 7║6 0│6 1  
$   ───┼───╫───┼───┤├───┼───╫───┼───  
$   4 2│4 3║4 6│4 7││5 8│5 9║6 2│6 3  
$                                    ┘
⍤Snapshot ≡₀(□⇌⊥10) °△ ↯6 2

# 2D box array with labels
$ ┌─
$ ╷ ∙0 │ b │∙2 │∙3
$      │∙1 │   │
$   ───┼───┼───┼───
$   ∙4 │ b │∙6 │∙7
$      │∙5 │   │
$   ───┼───┼───┼───
$    a │ b │ a │ a
$   ∙8 │∙9 │∙10│∙11
$   ───┼───┼───┼───
$   ∙12│ b │∙14│∙15
$      │∙13│   │
$                   ┘
⍤Snapshot ⍜⧋⊡₁⍚$b ⍜⊡₂⍚$a ≡₀□ °△ 4_4

Roundtrip!     ← ⍤⟜≍⊃(|0 pretty^0||0 ¤(repr°□₁)^!^0)
RoundtripRepr! ← ⍤⟜≍⊃(|0 repr^0||0 (repr°□₁)^!^0)

Roundtrip!@\W
Roundtrip!@\_

RoundtripRepr!@\s
RoundtripRepr!@\W
RoundtripRepr!@\_
RoundtripRepr!@\u{100001}
RoundtripRepr!@\u{100002}

⍤Snapshot @\s "@ "
⍤Snapshot @\u{2ffff} $ @\∅
⍤Snapshot @\u{2fffe} $ @\⊥

$ ╭─ label
$ ╷ 1
$     ╯
⍤Snapshot ¤ $label [1]

# Experimental!
⍤Snapshot ⍜(bytes"f64")⍜⊢+₁NaN "∅"
