# Simple use of `&raw` to move a dot around an image
&raw1
Dirs ← [0_1 0_¯1 ¯1_0 1_0 0_0]
Keys ← "wsad"
Size ← ˙⊟⊢&ts
◌◌⍢(
  ⊏⊙Dirs⬚4⊢⊚=Keys      # Select input dir
  ↧-1Size↥⊚2+          # Move
| &ims⇌⍉⍜⊡+₁⟜⊸(↯Size0) # Render
  ⊸≠@\x03⊢&rs1 0       # Get input
) [0 0]
