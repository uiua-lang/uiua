# Pattern Matching

Uiua has a powerful mechanism for patterns in arrays to conditionally extract data.

## [un]() Patterns

[un]() can be applied to a constant value to form a function that throws an error if its arguments does not match the constant.

```uiua
°5 5
```
```uiua should fail
°5 3
```

This is not very useful on its own, but it can be composed with other inverses to form more complex patterns.

A primary pattern of note is using array notation with planet notation to form patterns that match arrays with certain values and extract the others.

```uiua
°[1⊙3] [1 2 3]
```
```uiua should fail
°[1⊙3] [4 5 6]
```

These can be arbitrarily nested.

```uiua
°[1 2⊙⊙(5∘)] [1 2 3 4 5 6]
```

[un]()[join]() with a constant can also be used to match arrays with a certain prefix.

```uiua
°(⊂1) [1 2 3]
```
```uiua should fail
°(⊂1) [4 5 6]
```
```uiua
°(⊂1_2) [1 2 3]
```

To match a suffix, you can use [backward]().

```uiua
°(˜⊂3) [1 2 3]
```

## Matching Multiple Patterns with [try]()

Single patterns are of limited usefulness on their own. Because they throw errors when matching fails, you can attempt to match additional patterns using [try]().

[try]() accepts arbitrarily long function packs, so you can match as many patterns as you want in a simple way.

In this example, we run different code depending on which pattern matches.

```uiua
F ← ⍣(×10°[1⊙3]|°(⊂5)|⇌)
F [5 6 7]
F [1 2 3]
F "abc"
```

Having more or longer patterns may be easier to read if each pattern gets its own line.

```uiua
F ← ⍣(
  ×10 °[1⊙3]
| °(⊂5)
| ⇌
)
```

## Format String Patterns

[un]() works with format strings to extract substrings where the `_`s are. While the [regex]() function is available, it is often more complex than is necessary. In these cases, format string patterns are more appropriate.

```uiua
°$"_, _, _" "1, 2, 3"
```
```uiua
°$"_, _, _" "1, 2, 3, 4, 5"
```
```uiua
°$"Hello, _!" "Hello, World!"
```

Multiline format strings can be inverted as well.

```uiua
"Hello\nWorld!"
°$$ Hello
 $$ _!
```

Inverting [reduce]() of a dyadic format string will split by a delimiter.

```uiua
°/$"_ - _" "a - bcd - ef"
```

More precisely, format string patterns form a regex that replaces all `_`s from the format string with `(.+?|.*)`, where `.` also matches newlines.

## Propagating Errors with [case]()

Consider a function which attempts to match multiple patterns. After a pattern matches, each branch has some code to run.

For example, this function attempts to parse a couple different expected string formats, then [parse]()s and [select]()s the result in some way.

```uiua
F ← ⍣(
   ⊏ ⋕ °$"_: _"
| ˜⊏⊙⋕ °$"_ - _"
| ∘
)
F "1: abc"
F "def - 2"
```

But what happens if we give an input that matches the pattern but fails elsewhere in the branch?

```uiua kala confused
F ← ⍣(⊏⋕ °$"_: _"|˜⊏⊙⋕ °$"_ - _"|∘)
F "r: xyz"  # Can't parse
F "ghi - 3" # Out of bounds
```

In both those cases, a pattern match succeeds, but either the [parse]() or [select]() fails. This causes the [try]() to move on to the next branch silently, causing what may be unexpected behavior!

The [case]() modifier can be used to make the branch fail properly. [case]() simply calls its function. However, in the event that the function errors, the error can escape a single [try]().

Wrapping the code after a pattern match in [case]() will make the error propagate properly.

```uiua kala arms should fail
F ← ⍣(
  ⍩(⊏⋕) °$"_: _"
| ⍩(˜⊏⊙⋕) °$"_ - _"
| ∘
)
F "ghi - 3"
```

## Challenges

```challenge
removes a leading `0` from an array of numbers or prepends a `0` if it is missing
⍣°(⊂0)(⊂0)

[0 1 2 3]
[4 0 9]
[0 0 3 4]
```

```challenge
splits a string on the first `-` and returns the two parts
°$"_-_"

"hello-world"
"foo-bar"
"1-2-3"
```

```challenge
matches the argument against string prefixes `a`, `bc`, or `def` and adds `1`, `2`, or `3` to the second argument respectively, or adds 10` otherwise
⍣(+1 ◌°$"a_"|+2 ◌°$"bc_"|+3 ◌°$"def_"|+10 ◌)

"definite" 5
"abc" 1
"bcause" [1 2 3]
"wow" 4
```