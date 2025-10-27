Pitch ← ⁅₂ ×440 ˜ⁿ2 ÷12 -57
Minor ← ÷2/+⬚0⊏⊃(
  ⋅⋅(⊟⊃⌈⌊+14)
| Pitch + /⊂⊞+ ×12⇡4 [0 2 3 5 7 8 10]+⊙(×12-2))
Dur       ← ÷⟜(⇡+1⁅˜÷˜÷)&asr÷60 # ? Tempo Div
Quarter   ← Dur⊙1               # ? Tempo
Eighth    ← Dur⊙2               # ? Tempo
Sixteenth ← Dur⊙4               # ? Tempo
# Given a list of times for a single note and a list of note frequencies, generate smooth times for input to wave generation
# ? NoteTimes NoteFrequencies
SmoothTime ← (
  ⊙(⍥¤<2⧻⊸△)               # Fix if not already multi-channel
  ≡⌞˜⊞×                    # UnadjustedTimes ?
  ≡/⊂ +≡⊃(↘0_¯1|\+⬚0↻¯1≡⊣) # Adjust and join
)
Sine     ← ∿×τ
Square   ← ×4/π /+÷⟜(∿×τ⊞×) -⊸¬ ⇡₁
Saw      ← ×2/π /+⟜(×˜ⁿ¯1|÷|∿×τ⊞×) ⇡₁
Triangle ← × ÷˙×π8 /+⟜(×˜ⁿ¯1|÷˙×-⊸¬|∿×τ⊞×-⊸¬) ⇡₁
Distort  ← ⍜⌵↧₁ ×
Kick     ← ×⊃(¬ₑ×10-1÷⟜⇡⧻|∿×τ ×40 ⁿ0.2)
Noise    ← -⊸¬⍥⚂30000
Noisy    ← ⍜⟜↙(×⊸(√¬÷⟜⇡⧻) ↙⊙Noise ⊙◌)
Snare    ← Noisy2500

Tempo ← 138
Key   ← Minor 3

Drums ← /+⊓[
  ♭↯2 /⊂⍉⊞× Kick Sixteenth Tempo
| ×0.4♭↯2 /⊂⍉⊞× Snare Sixteenth Tempo]
IntroDrums ← /+[
  Kicks ← [
    1 0 0 0 1 0 0 0 1 0 0 0 1 1 1 1
    1 0 0 0 1 0 0 0 1 0 0 0 1 0 1 0
    1 0 0 0 1 0 0 0 1 0 0 0 1 1 1 1
    1 0 0 0 1 0 0 0 1 0 0 0 1 0 1 0
  ]
  Snares ← [
    0 0 0 0 1 0 0 0 0 0 0 0 1 1 1 1
    1 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0
    0 0 0 0 1 0 0 0 0 0 0 0 1 1 1 1
    1 0 0 0 1 0 0 0 0 0 0 0 1 0 1 0
  ]
  Drums Kicks Snares
]
MainDrums ← /+[
  Kicks ← [
    1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0
    1 0 0 0 1 0 0 0 1 0 0 0 1 0 1 0
    1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0
    1 0 0 0 1 0 0 0 1 0 0 0 1 0 1 0
  ]
  Snares ← [
    0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0
    0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0
    0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0
    0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0
  ]
  Drums Kicks Snares
]

BassSaw ← ×0.5 /+ Distort 5 Saw 10
Bassy ← /+⬚0[
  Bass ← [
    0 ∞ 0 7 ∞ 7 6 7 0 ∞ 0 7 ∞ 7 6 7
    0 ∞ 0 7 ∞ 7 6 7 0 0 7 9 2 2 9 10
    0 ∞ 0 7 ∞ 7 6 7 0 ∞ 0 7 ∞ 7 6 7
    0 ∞ 0 7 ∞ 7 6 7 3 3 9 10 2 2 7 9
  ]
  (Key 1 ♭↯2 Bass
   BassSaw SmoothTime Sixteenth Tempo)
  MainDrums
]

Verse ← /+⬚0[
  VerseM ← [
    0 1 2 3 4 4 7 6 4 4 0 0 4 3 2 1
    0 1 2 3 4 4 3 2 1 0 1 2 1 0 ¯0.5 1
    0 1 2 3 4 4 7 6 4 4 0 0 4 3 2 1
    0 1 2 3 4 4 3 2 1 1 2 2 3 3 4 4
  ]
  VerseM′ ← [
    ¯3 ∞ ∞ ∞ 3 ∞ ∞ ∞ 3 ∞ ¯3 ∞ ∞ ∞ ∞ ∞
    ¯2 ∞ ∞ ∞ 2 ∞ ∞ ∞ ¯1 ∞ ∞ ∞ ¯1 ∞ ∞ ∞
    ¯3 ∞ ∞ ∞ 3 ∞ ∞ ∞ 3 ∞ ¯3 ∞ ∞ ∞ ∞ ∞
    ¯2 ∞ ∞ ∞ 2 ∞ ∞ ∞ ∞ ∞ ∞ ∞ ∞ ∞ ∞ ∞
  ]
  VerseH ← ♭↯2 [
    0 7 0 7 0 7 0 7 0 7 0 7 0 7 0 7
    ¯2 5 ¯2 5 ¯2 5 ¯2 5 ¯1 6 ¯1 6 ¯1 6 ¯1 6
  ]
  (Key 4 /≡⊂ [[⟜≡₀⋅∞ VerseM] [VerseM VerseM′]]
   SmoothTime Eighth Tempo
   /+ ×0.5 ×[1 0.6] Square 4)
  (Key 1 VerseH
   ♭↯2 BassSaw SmoothTime Eighth Tempo)
  ♭↯2 MainDrums
]

Chorus ← /+⬚0[
  ChorusM ← [
    6 7 4 3 4 4 3 4 6 7 4 3 4 4 3 4
    3 2 1 ¯1 0 0 ¯1 0 1 2 3 4 0 0 4 6
    6 7 4 3 4 4 3 4 6 7 4 3 4 4 3 4
    3 2 1 ¯1 0 0 ¯1 0 1 2 3 4 0 0 4 6
  ]
  ChorusM₂ ← [
    6 7 4 3 4 4 3 4 6 7 4 3 4 4 3 4
    3 2 1 ¯1 0 0 ¯1 0 1 2 3 4 0 0 4 6
    6 7 4 3 4 4 3 4 6 7 4 3 4 4 7 8
    9 8 7 6 4 4 3 4 3 2 1 ¯1 0 0 4 6
  ]
  ChorusM′ ← [
    ∞ ∞ ∞ ∞ 2 ∞ ∞ ∞ ∞ ∞ ∞ ∞ 1 ∞ ∞ ∞
    ∞ ∞ ∞ ∞ ¯3 ∞ ∞ ∞ ∞ ∞ ∞ ∞ ¯3 ∞ ∞ ∞
    ∞ ∞ ∞ ∞ 2 ∞ ∞ ∞ ∞ ∞ ∞ ∞ 1 ∞ ∞ ∞
    ∞ ∞ ∞ ∞ ¯3 ∞ ∞ ∞ ∞ ∞ ∞ ∞ ¯3 ∞ ∞ ∞
  ]
  ChorusMEnd ← [∞ ∞]
  ChorusH ← [
    ¯2 5 ¯2 5 ¯2 5 ¯2 5 ¯1 6 ¯1 6 ¯1 6 ¯1 6
    0 7 0 7 0 7 0 7 0 7 0 7 0 7 0 7
  ]
  ChorusH₂ ← [
    ¯2 0 2 0 5 0 2 0 ¯1 1 3 1 6 1 3 1
    0 2 4 2 7 2 4 2 0 2 4 2 7 2 4 2
  ]
  ChorusH₃ ← [
    ¯2 0 2 0 ¯2 0 2 0 ¯1 1 3 1 ¯1 1 3 1
    0 2 4 2 0 2 4 2 0 2 4 2 0 2 4 2
  ]
  ChorusHEnd ← [3 3 2 2]
  BreakdownDrums ← ♭↯8 ×0.4 /⊂⍉⊞× Noisy⊸⧻ Quarter Tempo [0 0 1 0]

  (Key 4 /≡⊂ [
      [ChorusM ChorusM′] [⟜+₇ ChorusM₂]
      [ChorusM ChorusM′] [⟜+₇ ⊂⤚(↘¯⧻) ChorusMEnd ChorusM₂]
    ]
   SmoothTime Eighth Tempo
   /+ ×0.5 ×[1 0.6] Square 4)
  /⊂[
    (Key 1 ♭↯4 ChorusH
     BassSaw SmoothTime Eighth Tempo)
    /◇⊂{
      (Key 2 /◇⊂ {ChorusH₂ ChorusH₂ ChorusH₃ ↘¯⧻ChorusHEnd ChorusH₃}
       SmoothTime Eighth Tempo
       /+ Distort 4 Triangle 10)
      (Key 1 ChorusHEnd
       BassSaw SmoothTime Eighth Tempo)
    }
  ]
  /⊂▽2⬚0[MainDrums BreakdownDrums]
]

×0.5 /◇⊂ {
  IntroDrums
  Bassy
  Verse
  Chorus
  Bassy
  Verse
  Chorus
  Bassy
}
