# More Array Manipulation

Sometimes the operation you need to perform on an array is more complicated than modifiers like [reduce](), [rows](), or [table]() allow.

## [fix]()

[rows]() can be used to iterate over multiple arrays. The nth row of each array will be passed to the function, and the result will be put in a new array.

```uiua
≡⊂ 1_2_3 4_5_6
```

This works because both arrays have the same shape (in this case, `[3]`). If they didn't, we would get an error.

```uiua should fail
≡⊂ 1_2_3 4_5
```

However, there is an exception to this rule. If one of the arrays has exactly one row, that array will be repeated for each row of the other array. This includes scalars.

```uiua
≡⊂ 1 2_3_4
```

```uiua
≡⊂ 1_2_3 4
```

```uiua
≡⊂ [1_2] 3_4_5
```

```uiua
≡⊂ 1_2_3 [4_5]
```

Notice how in this last example, the array `[4_5]` is an array of a single row, so that row is repeated across all iterations. It would be equivalent to:

```uiua
[
  ⊂ 1 4_5
  ⊂ 2 4_5
  ⊂ 3 4_5
]
```

The [fix]() function turns an array into an array of a single row by prepending a `1` to the array's shape. This is equivalent to wrapping in `[]` as above, but it is shorter and better communicates the intention of "fixing" an array during iteration.

```uiua
¤1_2_3  # Fixing is equivalent to...
[1_2_3] # ...surrounding with brackets
```

With [fix](), we can rewrite the previous examples.

```uiua kala arms
≡⊂ ¤1_2 3_4_5
```

```uiua
≡⊂ 1_2_3 ¤4_5
```

If we have several arrays and want to choose which ones are fixed and which are not, we can use planet notation.

```uiua
≡⊂ ⊙¤ 1_2_3 4_5_6
```

```uiua
≡⊂₄ ⊙∩¤ 1_2_3 4_5_6 7_8_9 10_11_12
```

[fix]() also works with pervasive dyadic functions without [rows]().

```uiua
+  [1 2 3]  [10 20 30]
+ ¤[1 2 3]  [10 20 30]
+  [1 2 3] ¤[10 20 30]
```

```uiua should fail
- 1_2 [3_4 5_6 7_8]
```

```uiua
-¤1_2 [3_4 5_6 7_8]
```

## Sided [rows]()

Needing [fix]() or `dipfix` with [rows]() is extremely common. However, it can be a bit odd to read when your [fix]()ing is detatched from the [rows]() itself.

For this reason, [rows]() supports *sided* subscripts. This syntax allows you to specify a "side" for a modifier or function.

Sided subscripts are typed with `,` followed by a `<` for left or a `>` for right. The formatter will turn them into subscript bottom corner characters.

Left [rows]() [fix]()es the first argument and right [rows]() [fix]()es the last argument.

```uiua
≡⌞⊂ "ro" "wt" # Reuses "ro" 
≡⌟⊂ "cm" "at" # Reuses "at"
```

This makes the actual behavior of [rows]() here more visually apparent.

A number can be specified after the side to [fix]() multiple arguments.

```uiua
# Try formatting!
≡,<2(⊂⊂) "ui" "ua" "ns"
```

While [fix]() is still necessary for certain complex cases, sided subscripts should work about 95% of the time.

[rows]() can accept both numeric and sided subscripts at the same time. The number to select the rank comes before the side indicator.

```uiua
≡₂⌟₁□₃ °△2_2_3 "hi" "there"
```

## Operating at Different Ranks

[rows]() is the bread and butter of traversing an array's structure. It calls its function on each row of an array, but what if you want to go deeper?

One option is to simply chain [rows]() multiple times.

```uiua
≡□ °△ 2_3_4
≡≡□ °△ 2_3_4
≡≡≡□ °△ 2_3_4
```

This can get a bit unwieldy if an array has a lot of dimensions. You can instead use [numeric subscripts](</tutorial/More Argument Manipulation#subscripts>) with [rows]() to specify the rank of the arrays you want to work on.

```uiua kala stars
≡₀□ °△ 2_3_4 # Scalars
≡₁□ °△ 2_3_4 # Vectors
≡₂□ °△ 2_3_4 # Matrices
≡₃□ °△ 2_3_4 
```

This is useful when you are approaching the array's structure from the bottom up, but what if you want to start at the top, like [rows]() normally does?

This is what *negative* subscripts do. [rows]()`₋₁` is equivalent to normal [rows](), [rows]()`₋₂` is equivalent to `rowsrows`, etc.

```uiua
≡₋₁□ °△2_3_4
≡₋₂□ °△2_3_4
≡₋₃□ °△2_3_4
```

Sometimes you simply want to collapse the dimensions of an array to make it a certain rank. This can be done with subscripted [deshape]().

```uiua
△ ♭ °△2_3_4_5
△ ♭₂ °△2_3_4_5
△ ♭₃ °△2_3_4_5
△ ♭₄ °△2_3_4_5
```

Combined with [range](), this is a nice way to generate all combinations of indices given a list of maximums.

```uiua
⍉ ♭₂ ⇡2_2_3
```

Subscripting these works in the vast majority of cases. However, subscripts are static. The rank to use cannot be determined at runtime.

In the rare event that you need a dynamic rank, you can use the `unby` `(` `lengthshape` `)` idiom introduced in the [Inverses](/tutorial/Inverses#un-by) tutorial.

```uiua
⍉ °⊸(⧻△) 2 ⇡2_2_2
```

```uiua
°⊸(⧻△) 1 °△2_3_3
```

## Challenges

```challenge
adds the first argument list to each row of the second argument matrix
+¤

1_2_3 [4_5_6 7_8_9]
10_20 ↯4_2⇡8
"Wow" ¯[10_0_10 19_14_19]
```

```challenge
joins the first argument to each list in the second argument
≡₁⌞⊂

0 +1°△3_4
0 [1 2 3]
@| ⬚@ [⬚@ ["Hey""there""buddy"] ⬚@ [@a "bc" "def"]]
η_π_τ ⇡2_2_2
```