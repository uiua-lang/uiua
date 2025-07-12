# This example uses your webcam to take a photo,
# then runs it through some effects and displays it.

# Pixelate an image with the given ratio
# ? Ratio Image
Pixelate ← ◌⍥(⨪⟜▽⟜≡▽)2 ⨪

# Run an edge-detection algorithm on an image.
# Uses a 5x5 kernel.
# ? Image
Edges ← (
  ÷⧻⟜/+°⍉
  /+/+ ×⊏[2 1 0 1 2] [1_1_¯12_1_1 0_1_1_1_0 0_0_1_0_0] ⍉⍉ ⧈∘⊟.5
  ↥0↧1
  ¬⍥((⊂0⊂⊙0)≡(⊂0⊂⊙0))2
)

# Show an image in the default program
# ? Image
Show ← &invk⟜&fwa $"img._" ⟜img "jpg"

Show ×⊃(Pixelate32|⍜¬(×0.7)Edges) &camcap 0
