# Arrays

Uiua is, first and foremost, an array language. The only composite data type is the multidimensional array. Arrays have a lot of nice properties, and the language's built-in functions are designed to make it easy to work with them. If you've only ever programmed in non-array languages, then this will be a completely foreign paradigm. In most array languages, most data structures and control flow are replaced with operations on arrays.

## Creating Arrays

Other than with functions, Uiua has two ways to create arrays. They are called *strand notation* and *array notation*.

**Strand notation** uses underscores `_` to connect elements.

```uiua
1_2_3
```
```uiua
"Hello"_"World"
```

Strand notation is good when you want to create short and/or simple arrays. For longer or more complex arrays, you can use array notations.

**Array notation** uses `[]` brackets to group elements.

```uiua
[1 2 3]
```
```uiua
[¯5 37 42 π]
```

What's cool about array notation is that it is *not* just a way to list elements. The code between the brackets runs from right to left as it normally would. When it is done, all the results of the code are put in an array. This gives you some cool ways to create arrays.

```uiua
[+2 3 +5 7]
```
```uiua
[+2 ⟜(×2) ⟜(-2) 5]
```
Any functions inside the brackets will "pull in" their arguments from outside if there are not enough inside.

```uiua
[+] 1 9
```
```uiua
[+×2] 20 2
```

You can also use array notation to make multidimensional arrays.

```uiua
[1_2_3 4_5_6]
```
```uiua
[⊸√ 4_9_49]
```

Unlike strand notation, array notation may span multiple lines. The lines are still executed right-to-left, but they are executed bottom-to-top so that the arrays come out the same way they look in the code.

```uiua
[1 2 3
 4 5 6
 7 8 9]
```
```uiua
[[1 2 3]
 [4 5 6]
 [7 8 9]]
```

## [shape]() and [length]()

Other than their data, arrays also have a property called their **shape**. Shape is a list of non-negative integers that describes the array's size along each of its axes.

We can get the array's shape with the [shape]() function. It's a triangle because a triangle is a shape.

```uiua
△[1 2 3]
```
```uiua
△5
```
```uiua
△[[1 2 3] [4 5 6]]
```

Arrays with 0 dimensions (an empty [shape]()) are called **scalars**.

Arrays with 1 dimension are often called **lists** or **vectors**.

Arrays with 2 dimensions are often called **tables** or **matrices**.

While there are not common names for arrays with 3 or more dimensions, Uiua supports arrays with an arbitrary number of axes.

The first elements of the shape is the number of *rows* of the array. *Rows* does not refer just to the rows of a matrix or table. It is the groups of elements along the leading axis of the array. For lists, this is just the individual elements. For matrices, it is the rows as you might traditionally think of them. But arrays with a higher number of dimensions have rows as well. For example, in an array with 3 dimensions, each row is a matrix.

From shape we can derive two closely-related properties called **length** and **rank**.

[length]() is the number of rows in the array. Length is always equal to the first number in the shape (or 1 if the shape is empty).

**Rank** is the number of dimensions of the array. It is equivalent to the [length]() of the [shape]().

```uiua
 △[1_2_3 4_5_6 7_8_9] # Shape
 ⧻[1_2_3 4_5_6 7_8_9] # Length
⧻△[1_2_3 4_5_6 7_8_9] # Rank
```

## Pretty Array Output

The online editor and native interpreter both pretty-print any values that are left when a program is finished. (This can be invoked manually using the [`&s`]() or [pretty]() functions.)

To understand how the pretty-printed output corresponds to the actual array, we can use [reshape]() to create a multidimensional array. [reshape]() uses its first argument as a new shape for its second argument.

Here, we create a [range]() array of all numbers up to `24` and turn it into a 3-dimensional array with the shape `[2 3 4]`.

```uiua
↯2_3_4 ⇡24
```

Notice there are `2` big cells, each with `3` rows of `4` elements.

This expands to any number of dimensions. The elements of the last axis are always laid out horizontally. The rows of the second-to-last axis are always laid out vertically. The third-to-last axis is horizontal, the fourth-to-last is vertical, etc.

We can see here that the shape `[2 3 4 5]` appears almost like a 2×3 matrix of 4×5 matrices. Also note that reshape cycles through elements if it doesn't have enough.

```uiua
↯2_3_4_5 ⇡100
```

## Pervasion

Most operations that apply to scalars are what is called *pervasive* when it comes to arrays. This means that the operation automatically applies to every item in the array.

```uiua
+1 1_2_3
```
```uiua
√[4 9 16]
```
```uiua
+1_2_3 4_5_6
```

When doing a pervasive operation on two arrays, the shape of one array must be the *prefix* of the shape of the other. This means that all the numbers in one shape must be at the beginning of the other shape.

Here, neither of the shapes `[2]` or `[3]` are prefixes of the other.

```uiua should fail kala confused
+ [1 2] [3 4 5]
```
But here, the shape of the first array (`[2]`) is a prefix of the shape of the second array (`[2 3]`).

```uiua
△10_20
      △[3_4_5 6_7_8]
+10_20 [3_4_5 6_7_8]
```

If you want to do some pervasive operation on arrays whose shapes do not match, you can set a default value with the [fill]() modifier. Any places where the shapes don't match will be filled in with that value.

```uiua kala pleased
⬚10+ [1 2] [3 4 5 6 7]
```

[fill]() can be used in a lot of other cases. See its documentation for more.

Pervasive operations are optimized in the interpreter to be very fast. You should prefer to use them whenever possible.

## Useful Array Operations

You don't need to memorize all of these right now. This is just a brief introduction to some of the array operations so that you won't be surprised when you see them later.

If you ever see a glyph that you don't recognize in an example, you can mouse over it in the editor to learn its name.

You can ctrl/⌘-click any glyph in the editor to see its documentation.

You can also click the names of functions in the site text to see their documentation.

[couple]() turns two arrays into rows of a new array.

```uiua
⊟ 1_2_3 [4 5 6]
```

[join]()  joins two arrays end-to-end.

```uiua
⊂ 1_2_3 4
```
```uiua
⊂ 1_2_3 [4 5 6 7]
```

[first]() and [last]() get the first or last row of an array.

```uiua
⊢ [4 7 1]
```
```uiua
⊢ [1_2 3_4 5_6]
```
```uiua
⊣ "hello"
```

[reverse]() reverses the rows of an array.

```uiua
⇌ [4 7 1]
⇌ [1_2 3_4 5_6]
```

[rotate]() rotates the rows of an array by some amount.

```uiua
↻2 [1 2 3 4 5]
```

[deshape]() flattens an array into a 1D array.

```uiua
⊸♭ [1_2 3_4 5_6]
```

[take]() and [drop]() isolate part of an array.

```uiua
↙3 [1 2 3 4 5]
↘3 [1 2 3 4 5]
```

Negative values [take]() or [drop]() from the end.

```uiua
↙¯3 [1 2 3 4 5]
↘¯3 [1 2 3 4 5]
```

[pick]() indexes an array. Longer indices index deeper into the array.

Uiua is 0-indexed.

```uiua
⊡2 [3 8 4 1]
```
```uiua
⊡1   [1_2_3 4_5_6]
⊡1_1 [1_2_3 4_5_6]
```

[select]() uses a list of indices to select rows of an array.

```uiua
⊏ [0 2 1 1 2] ↯3_3⇡9
```
```uiua
⊏ [3 5 0 1 7 8 9 5 1 2 5 3 10] "their sinks"
```

[transpose]() rotates the axes of an array. This is useful for changing which axis other functions will work on.

```uiua
⊸⍉ [1_2_3 4_5_6]
```

## The Array Model

For curious array aficionados, Uiua use an array model resembling [J's boxed array model](https://aplwiki.com/wiki/Box).

All arrays are flat and homogenous. Arrays always have a rectangular shape, meaning that all rows along an axis always have the same length. Different types of data, like numbers and characters, cannot be mixed in the same array.

However, there is an escape hatch for when you really want jagged, nested, or mixed-type arrays. In Uiua, an array of heterogeneous values can be simulated with an array of *boxes*.

The array below cannot be constructed normally because its rows have different [shape]()s.

```uiua should fail kala confused
[1_2 3 [4 5 6] [7]]
```

By using [box](), we can turn any value into a **box** that contains that value. We can then put these boxes into an array together.

```uiua kala arms
[□1_2 □3 □[4 5 6] □[7]]
```

The [box]()ed items in the array are separated by `│`s. Scalars are marked with a `∙` to distinguish them from single-row lists.

This is *not* an array of numbers. It is an array of boxes! Notice that the [shape]() of the array has nothing to do with the shape of any of the boxed values.

```uiua
△ [□1_2 □3 □[4 5 6] □[7]]
```

[un]()[box]() extracts a [box]()ed value.

```uiua
⊸°□ □[1 2 3]
```

The `□` in front of the list indicates that it is [box]()ed.

Having to write `box` everywhere is annoying, and so...

## Nested Arrays

Uiua has a special syntax for making arrays where every item is [box]()ed.

Using `{}`s instead of `[]`s for array notation will automatically [box]() every item.

```uiua
{1_2 3 [4 5 6] [7]}
```

This is very useful for making lists of strings.

```uiua should fail kala confused
["Uiua" "APL" "J" "BQN" "K" "Q"] # Fails
```
```uiua kala stars
{"Uiua" "APL" "J" "BQN" "K" "Q"} # Works!
```

Functions that require their arguments to have matching types may require [box]()ing an argument.

For example, to check if a string is in a list of [box]()ed strings with [memberof](), you would need to [box]() the string first.

```uiua
Langs ← {"Uiua" "APL" "J" "BQN" "K" "Q"}
∊ Langs □"APL"
```

Pervasive functions work through boxes and preserve the maximum [box]() depth of their arguments.

```uiua
¯ 1
¯ □1
¯ □□1
```
```uiua
+1 4
+1 □4
+1 □□4
+□□1 □4
```
```uiua
×10 {1_2_3 4_5 6}
```

There is an exception for comparison functions, which compare lexicographically.

```uiua
=  [1 2 3]  [1 2 5]
= □[1 2 3] □[1 2 5]
>  [1 2 3]  [1 2 5]
> □[1 2 3] □[1 2 5]
>  "banana"  "orange"
> □"banana" □"orange"
```

Non-pervasive functions often require [un]()[box]()ing the arguments to get at the value you want.

Consider this difference:

```uiua
△    ⊢{1_2_3 5_6}
△ °□ ⊢{1_2_3 5_6}
```

For more about working with box arrays, see [box]()'s documentation.

## Challenges

```challenge
adds an array to its reverse
+⊸⇌

1_2_5
3_1_7_0
↯2_4⇡8
```

```challenge
creates a matrix of 0's with as many rows as the first argument and as many columns as the second argument
˜↯0⊟

3 4
2 7
3 3
1 8
```

```challenge
adds a 1-row leading axis to an array
↯⊂1⊸△
¤
[1 2 3]
1_3_1_5
5
↯2_3⇡6
```

```challenge
prepends the first row of the first argument to the second argument
⊂⊢

[1 2 3] 4_5_6
3_3 2_2
[1_2_3 4_5_6] +10↯3_3⇡9
[2 4 3] [9 9 9 9 9 1]
```

```challenge
removes the first and last rows from an array
↘¯1↘1

1_2_3_4
[27 9 3 1]
↯4_3⇡12
[5 6]
```

```challenge
prepends an array as an item to a list of boxed arrays
⊂□

"Hi" {"how" "are" "ya"}
1_2_3 {4_5 [6]}
[] {[] []}
1 {2 3}
```