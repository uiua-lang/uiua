# Creates a number spiral that is common is code golf problems
F ← ◌°⟜⊡ ⊃(
  ⟜(↘¯1 ▽ ▽2+1⇡⟜(◿4⇡+-1.) # Orientations
    ⊏:[0_1 ¯1_0 0_¯1 1_0] # Directions
    ⊂0_0\+                # Offsets
  )
  +¤⌊÷2⊟. # Indices
| +1⇡×.   # Numbers
)

F 3
F 5
F 7
