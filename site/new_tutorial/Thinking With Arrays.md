# Thinking With Arrays

So far, we've covered the mechanics of working with arrays in Uiua. However, if you are new to the array paradigm, it may not be clear how to use arrays to solve problems. 

This section covers some of the common functions and modifiers that pop up when solving many different problems.

## Masks and [keep]()

Many languages have some sort of `filter` function that takes a predicate and a list and returns a list of all the elements that satisfy the predicate. In array languages, we take a different approach.

First, we create a *mask* array. A mask array is an array of `0`s and `1`s where `1`s represent the rows that satisfy the predicate. For pervasive functions, this is extremely simple.

For example, if we wanted to create a mask of all numbers greater than 4, we simply treat the whole array as a single unit.

```uiua
⊸> 4 [2 8 3 9 1 7 2]
```

The [keep]() function takes a mask array and an array and returns an array of all the rows that have a `1` in the mask. This is essentially a filter.

```uiua
▽⊸> 4 [2 8 3 9 1 7 2]
```

[keep]() also works with [under]() so that you can modify the rows that have a `1` in the mask.

```uiua
⍜▽(×10) ⊸> 4 [2 8 3 9 1 7 2]
```

[keep]() has a few other use cases with non-masks. See its documentation for more.

## [where]()

The [where]() function converts a mask array into an array of indices where the mask is `1`.

```uiua
⊸⊚ ⊸> 4 [2 8 3 9 1 7 2]
```

This works with multi-dimensional arrays as well.

```uiua
⊸⊚ ⊸> 4 [2_8_3 9_1_7]
```

[un]()[where]() converts an array of indices into a mask array.

```uiua
°⊚ [3 9 5 8]
```
```uiua
°⊚ [1_2 3_4]
```

[select]()[where]() is equivalent to [keep]() (at least for boolean predicates).

```uiua
⊏⊚ =0⊸◿2 [2 8 3 9 1 7 2]
```
```uiua
▽  =0⊸◿2 [2 8 3 9 1 7 2]
```

## [scan]()

The [scan]() modifier is similar to [reduce](), but it returns an array of all the intermediate results.

```uiua
/+ [1 2 3 4]
\+ [1 2 3 4]
```

This can be useful when used on a mask.

For example, if we wanted to get the first word of a string, we could start by creating a mask of all the non-space characters.

Then we can use [scan]()[multiply]() to zero the mask after the first word.

Finally, we can use [keep]() to apply the mask and get the first word.

Use the arrows to see how the mask changes.

```uiua
▽ \× ⊸≠@  "What's the first word?"
```

## [fill]()

Recall that the [fill]() modifier sets a "fill value" that can be used by certain fuctions.

One common use is to set a default value that will be used when the shapes of arrays do not match.

```uiua
⬚0+ 10_20 3_4_5_6
```

For example, if you wanted to logical OR two masks with different shapes, you could use [fill]() with a different fill value depending on what you want to do with the mismatched parts.

```uiua
⬚0↥ 1_0_0_1_0 0_1_0
⬚1↥ 1_0_0_1_0 0_1_0
```

Another interesting use is a [fill]()ed [rotate](). Instead of wrapping values around, it fills in one side of the array with the fill value.

```uiua
  ↻¯2 [1 2 3 4 5]
⬚0↻¯2 [1 2 3 4 5]
```

## [partition]()

[partition]() is a powerful modifier that splits up an array based on a list of consecutive keys. Before explaining it further, let's look at a simple example of a very common use case: splitting a string into words.

```uiua
⊜□ ⊸≠@  "Look at that!"
```

First, we create a mask of all the non-space characters. Then, [partition]() calls [box]() on each section of the string that corresponds to a run of `1`s in the mask.

Here is another example using [partition]()[box]() with the inputs explicitly defined.

```uiua
[1 2 3 4 5 6 7 8]
[1 1 0 5 6 6 0 1]
⊜□
```

Notice that `0`s in the keys arrays cause the corresponding sections of the input array to be skipped, so `3` and `7` are omitted form the output.

We use [box]() here because the resulting sections have different lengths. If we expect the sections to have the same lengths, we can use [identity]() instead.

```uiua
[1 2 3 4 5 6 7 8]
[1 1 2 2 0 0 3 3]
⊜∘
```

[partition]() is very useful when working with strings. See the [Strings tutorial](/tutorial/strings) for more.

A hint for one of the challenges below: [partition]() works with [under]()!

## [group]()

TODO

## [inventory]() and [content]()

TODO

## Challenges

```challenge
negates each number in a list that is not a multiple of 3
⍜▽¯ ≠0⊸◿3

[1 2 3 4 5 6 7 8 9]
[3 0 1 8]
[3 6 9 12 15 18 21 25 27]
```

```challenge
returns the last word of a string
▽ ⍜⇌\× ⊸≠@ 

"What's the last word?"
"Um, I um, arrays"
"I like trains"
```

```challenge
for every multiple of 3 in a list, multiplies the following number by 10
⍜▽(×10) ⬚0↻¯1 =0⊸◿3

[1 2 3 4 5 6 7]
[2 9 3 8 7 1]
[3 3 3 3]
```

```challenge
given a matrix of `0`s an `1`s, only keeps the `1`s that have even x and y coordinates
⍜⊚(▽≡/×¬⊸◿2)

[1_1_0 0_1_1 0_1_1]
[1_1 1_1]
↯3_4 1_0_1
↯4_4 1_0_0_1_0
```

```challenge
reverses each word in a string but keeps the words in the same order
⍜⊜□⍚⇌ ⊸≠@ 

"get in the racecar"
"arrays are neat"
"wow mom"
```