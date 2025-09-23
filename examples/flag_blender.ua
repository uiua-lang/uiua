# Blend two lists of colors as flags
Blend ← (
  ∩⌞(▽⊃(˜÷⊙⧻)≡↯)   # Make flags
  ÷¤¤⟜⇡⊸△₂         # Make coords
  ⇌≡⇌˜÷1+1ₑ×8¬/+°⍉ # 3D sigmoid
  +˜∩× ⊸¬          # Apply and combine
)

&ims Blend 1024 Bi Enby
