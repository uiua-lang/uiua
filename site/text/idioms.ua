# Enumerate an array, but keep it on the stack
°⊏ "Hello"

# Create an incrementing array with the given shape
# Useful for testing
°△3_4

# Remove a row at an index
⍜↻↘₁ 2 "abcde"

# Replace a row at an index
⍜⊡◌ 2 [1 2 3 4] 10
⍜⊙⊡⊙◌ 10 2 [1 2 3 4]

# Split on a scalar
⊜□⊸≠ @, "a,bc,d"
°/$"_,_" "a,bc,d"

# Split on a list
⊜□¬⊸⦷ " - " "a - bc - d"
°/$"_ - _" "a - bc - d"

# Check that all rows of an array are equal
≍⊸↻1 [5 5 5]
≍⊸↻1 [1 2 3]

# Get the prime factorization of a number
# (or array of numbers)
°/× 84

# Scale an array along the first two axes
# Useful for scaling images
# Can scale both up and down
▽₂ 2 [1_2 3_4]
