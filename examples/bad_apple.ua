Pitch ← ⁅₂ ×440 ˜ⁿ2 ÷12 -57
Minor ← ÷2/+⬚0⊏⊃(
  ⋅⋅(⊟⊃⌈⌊+14)
| Pitch + /⊂⊞+ ×12⇡4 [0 2 3 5 7 8 10]+⊙(×12-2))
EbMinor   ← Minor 3
Dur       ← ÷⟜(⇡⁅˜÷˜÷+1)&asr÷60 # ? Tempo Div
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

&ap ×0.2 /◇⊂ {
  # Intro
  /+[
    Intro ← ♭↯2[
      0 ∞ 0 7 ∞ 7 6 7 0 ∞ 0 7 ∞ 7 6 7
      0 ∞ 0 7 ∞ 7 6 7 0 0 7 9 2 2 9 10
      0 ∞ 0 7 ∞ 7 6 7 0 ∞ 0 7 ∞ 7 6 7
      0 ∞ 0 7 ∞ 7 6 7 3 3 9 10 2 2 7 9
    ]
    (EbMinor 1 Intro
     SmoothTime Sixteenth 130
     /+ Distort 5 Saw 20)
  ]

  # Verse
  /+[
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
    (EbMinor 4 /≡⊂ [[⟜≡₀⋅∞ VerseM] [VerseM VerseM′]]
     SmoothTime Eighth 130
     /+ ×0.5 ×[1 0.6] Square 4)
    (EbMinor 1 VerseH
     SmoothTime Eighth 130
     ♭↯2 ×0.5 /+ Distort 2 Saw 20)
  ]

  # Chorus
  /+[
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
    ChorusH ← ♭↯2[
      ¯2 5 ¯2 5 ¯2 5 ¯2 5 ¯1 6 ¯1 6 ¯1 6 ¯1 6
      0 7 0 7 0 7 0 7 0 7 0 7 0 7 0 7
    ]
    ChorusH₂ ← [
      ¯2 0 2 0 5 2 0 2 ¯1 1 3 1 6 3 1 3
      0 2 4 2 7 4 2 4 0 2 4 2 7 4 2 4
    ]
    (EbMinor 4 /≡⊂ [[ChorusM ChorusM′] [⟜+₇ ChorusM₂] [ChorusM ChorusM′]]
     SmoothTime Eighth 130
     /+ ×0.5 ×[1 0.6] Square 4)
    (⊂ ˜∩EbMinor 1 2 ∩(♭↯2) ChorusH ChorusH₂
     SmoothTime Eighth 130
     ×0.5 /+ Distort 2 Saw 20)
  ]
}
