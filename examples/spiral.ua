# Creates a number spiral that is common in code golf problems
F ← ⌝⊡⊃(
  ⟜(↘¯1 ▽⊃(▽2+1⇡|◿4⇡+⊸-1) # Orientations
    ⊏⊙A₂                  # Directions
    ⊂0\+                  # Offsets
  )
  +¤⌊÷2⊟. # Indices
| +1⇡×.   # Numbers
)

F 3
F 5
F 7
