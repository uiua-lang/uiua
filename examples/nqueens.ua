Orth     ← /↥=
Diag     ← /=⌵-
Conflict ← ↥⊃Orth Diag
Permute  ← ≡∧⍜↘(↻¯1)⊃(♭₂⇡+1⇡⧻)¤
NQueens ← (
  ≡(⍉⊟)¤⟜Permute⇡
  ⊡⊢⊚¬≡(/↥♭⊞(×⊃(¬≍|Conflict)))..)
°⊚.NQueens 6
