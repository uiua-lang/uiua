Lines ← ⌅(⬚@\s⊜∘ ⊸≠@\n|◌ ⬚@\n°⊜□)

# Uniqueness

# Msg Cond ? ReprA ReprB ExprA ExprB
Distinct ← ⊓($"(_) and (_) give ambiguous output:\n_"|¬≍) ⊙⊙(⊸°Lines ∩pretty)
Unique!  ←^ ⍚/($"⍤ Distinct _ _ _ _" ◡∩repr) ⧅<2

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

Diff     ← °Lines ⍚$"_:\n_" {"Input" "Expected" "Got"} □₃ ⊓repr˜∩(°Lines ≡$"\"_\"")
Snapshot ← ⊃Diff⋅≍ ⟜⊓prettyLines

# SoA

$ ┌─
$       a │
$   ──────┤
$   □b: 5 │
$       □6│
$           ┘
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
$ ┌─
$   foo  │bar
$   "ABC"│0 1 2
$               ┘
⍤Snapshot □₂ ⊓$foo$bar ⊸≡⌞+@A ⇡3

# This was a bug related to rank-1 strings inside rank-2+ box arrays getting 2D box outlines instead of quotes.
$ ┌─
$ ╷ "a" │
$   ────┤
$   "bc"│
$   ────┤
$   "d" │
$         ┘
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

# Multivectors

$ e₂ + 2e₃ + 3e₄ + 4e₅ + 5e₆ + 6e₇ + 7e₈
# Experimental!
⍤Snapshot 𝕍°△8

$ ╭─
$   e₂ + 2e₃ + 3e₄
$   4e₁ + 5e₂ + 6e₃ + 7e₄
$   8e₁ + 9e₂ + 10e₃ + 11e₄
$   12e₁ + 13e₂ + 14e₃ + 15e₄
$                             ╯
# Experimental!
⍤Snapshot 𝕍°△4_4

$ ╭─
$ ╷ e₂ + 2e₃            3e₁ + 4e₂ + 5e₃     6e₁ + 7e₂ + 8e₃
$   9e₁ + 10e₂ + 11e₃   12e₁ + 13e₂ + 14e₃  15e₁ + 16e₂ + 17e₃
$   18e₁ + 19e₂ + 20e₃  21e₁ + 22e₂ + 23e₃  24e₁ + 25e₂ + 26e₃
$                                                              ╯
# Experimental!
⍤Snapshot 𝕍°△3_3_3

$ ╭─
$ ╷ e₂           2e₁ + 3e₂      8e₁ + 9e₂    10e₁ + 11e₂    32e₁ + 33e₂  34e₁ + 35e₂    40e₁ + 41e₂  42e₁ + 43e₂
$ ╷ 4e₁ + 5e₂    6e₁ + 7e₂      12e₁ + 13e₂  14e₁ + 15e₂    36e₁ + 37e₂  38e₁ + 39e₂    44e₁ + 45e₂  46e₁ + 47e₂
$ ╷
$ ╷ 16e₁ + 17e₂  18e₁ + 19e₂    24e₁ + 25e₂  26e₁ + 27e₂    48e₁ + 49e₂  50e₁ + 51e₂    56e₁ + 57e₂  58e₁ + 59e₂
$   20e₁ + 21e₂  22e₁ + 23e₂    28e₁ + 29e₂  30e₁ + 31e₂    52e₁ + 53e₂  54e₁ + 55e₂    60e₁ + 61e₂  62e₁ + 63e₂
$                                                                                                                ╯
# Experimental!
⍤Snapshot 𝕍°△ ↯6 2

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
⍤Snapshot ⍜(bytes"f64")⍜⊢+₁nan "∅"
