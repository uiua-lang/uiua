# Array Summary Notation

Usually arrays print out the elements inside of them, but sometimes the array is too large to be displayed in full. In situations like these, Uiua will print a *summary* of the contents of an array by describing properties that it has.

These summaries are shown in many places, such as when using [pretty](), [&s](), output comments, or just seeing unused arguments in the program output.

```uiua
⇡10000 ##
⊸pretty
```

This section will explain what information gets shown in this format and under what circumstances it will be shown.

## General Format

Array summaries will typically show up when there are more elements in the array than is practical or useful to show in output. The metrics used for determining this are best-effort.

The summary always has one segment that shows the array's shape and type and another segment of miscellaneous information that is dependent on type or unique value count. On rank 1 arrays, the entire summary will be shown on one line with sections seperated by a colon `:`, whereas arrays of rank 2 or greater will have their shape and type information on one line and other information on the next line.

```uiua
˙⊂₁₀₀&fras "example.txt"
°△ 5_10_15_20
```

The type characters are the same as those used by [validate](), which includes:
- `ℝ` - Real numbers
- `ℤ` - Integers
- `ℕ` - Natural numbers
- `𝔹` - Booleans (only 0 or 1)
- `ℂ` - Complex numbers
- `𝕌` - Unicode characters
- `@` - ASCII-only characters
- `□` - Boxes

```uiua
⊸&s⇡5000
&s⊸◿2
&s⊸-100
&s÷⊸⧻
```

## Multimap Format

In cases where an array has 5 or fewer unique values, a multimap format is used to show the unique values and how often they occur. This replaces what would usually be in the other information section of the summary.

The format starts with the [dedup]() character showing all the unique values sorted in order, then with the [occurrences]() character, showing the occurences of each respective unique values.

```uiua
&s⁅×4Lena
&s⁅₁₀∿×η⇡5000
```

## Type-Dependent Descriptions

### Numeric Arrays `ℝ`/`ℤ`/`ℕ`/`𝔹`

Numeric/real arrays show the minimum, maximum, and the mean denoted by `µ`.

```uiua
&s °ₑ +1 ⇡5000 # log(x+1)
```

If the entire array is all one value, it will say so instead.

```uiua
⌊ⁿ2∿⇡5000
```

There is also special handling for `nan`s and infinities in numeric arrays. If they are present, the number of `nan`s will be shown in parentheses.

```uiua
÷⟜∿⇡5000
```

### [complex]() Arrays

Complex arrays use a very similar summary format to numeric arrays, using the component-wise range and average of the elements.

```uiua
ₑ×πi÷-₁⟜⇡10000
```

### Character Arrays `𝕌`/`@`

Character arrays show the categories of characters present in the array.

The categories include:
- `upper` - uppercase letters
- `lower` - lowercase letters
- `letters` - if there are both uppercase and lowercase letters, this category is used instead
- `digits` - ASCII digits (0-9)
- `nums` - non-ASCII digits (e.g. Arabic-Indic numerals)
- `punctuation` - ASCII punctuation
- `whitespace` - any whitespace character (including other languages' spaces)
- `control` - control code characters (only `@\0`-`@\x19` | `@\x7f`-`@\x9f`)
- `other` - any character that doesn't fit into the categories listen above (like `@√` or `@¡`)

If there is only a few characters in the array, it will print it out in multimap format.

```uiua
♭↯200"sphinx (of black quartz) judge my vow!"
⊸⌵
◡⊂
```

### [box]() Arrays

Arrays of boxes show the minimum and maximum rank (how many dimensions an array has) as well as its minimum and maximum shape.

```uiua
&s ⊂↯1000∩(□°△) 8_8 2_2_2_2
&s ⍚⇡♭₂⇡₁˙⊟40
&s ⧈⌞□ 2 °△ 4000_3
```

The minimum and maximum shape is calculated based on how many elements an array of that shape can store. Consequently, the minimum and maximum shape of an array of boxes currently depends on the order of the elements in the array.

```uiua
⟜⇌⊂↯1000∩(□°△) 4_4 2_2_2_2
```
