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

# SoA

pretty {$a {$b 5 6}}
$ ┌─        
$      a │  
$   ─────┤  
$   □b: 5│  
$      □6│  
$          ┘
⍤⤙≍ ⊜∘⊸≠@\n

# Boxed fixed arrays
⍤⤙≍ ¤"[\"1\"│]" pretty {"1"}
⍤⤙≍ ¤"[¤\"1\"│]" pretty {¤"1"}
⍤⤙≍ ¤"[¤\"23\"│]" pretty {¤"23"}

# Labels
pretty {$a 5 $b 6}
$ ┌─       
$    a│ b  
$   ∙5│∙6  
$         ┘
⍤⤙≍ ⊜∘⊸≠@\n

pretty {$a 5 $b @x}
$ ┌─       
$    a│b   
$   ∙5│@x  
$         ┘
⍤⤙≍ ⊜∘⊸≠@\n

Roundtrip!     ← ⍤⟜≍⊃(|0 pretty^0||0 ¤(repr°□₁)^!^0)
RoundtripRepr! ← ⍤⟜≍⊃(|0 repr^0||0 (repr°□₁)^!^0)

Roundtrip!@\W
Roundtrip!@\_

RoundtripRepr!@\s
RoundtripRepr!@\W
RoundtripRepr!@\_
RoundtripRepr!@\u{100001}
RoundtripRepr!@\u{100002}

⍤⤙≍ ["@ "] pretty@\s
⍤⟜≍ pretty@\u{100001} ¤$ @\∅
⍤⟜≍ pretty@\u{100002} ¤$ @\⊥

# Experimental!
⍤⤙≍ ["∅"] pretty⍜(bytes"f64")⍜⊢+₁NaN
