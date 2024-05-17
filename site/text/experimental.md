# Experimental Features

Uiua has a number of features that are considered experimental. They are available in the interpreter for testing, but may be removed or changed in the future.

Using experimental features requires an `# Experimental!` comment to be placed at the top of a Uiua source file.

## Function Strands

Function strands allow you to combine two terms without parentheses. They are written with a `‿` between the terms.

Function strands bind less tightly than modifiers, so the first term can never be a modifier.

```
# Experimental!
≡⊢‿⇌ [1_2_3 4_5_6 7_8_9]
```
```
# Experimental!
↯⟜⇡‿/+ 2_3
```

## Swizzles

Swizzles allow you to reorder the stack in a concise way.
They are written with a `λ` followed by some lowercase letters.
The `λ` will format from `'` if followed by letters.

```
# Experimental!
[λcba 1 2 3 4 5]
[λbbbbba 1 2]
[λccaabb 1 2 3]
```