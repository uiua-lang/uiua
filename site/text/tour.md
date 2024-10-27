# Uiua Language Tour

## The Union of Two Paradigms

Uiua is a programming language that incorporates two of the less-common programming paradigms: **array-oriented** and **stack-based**.

An **array-oriented** language is one where the primary data structure is the array. In array languages, many operations that can apply to a single value can also apply to every value in an array. This is known as *rank-polymorphism*.

A **stack-based** language is one where all operations manipulate a global stack of values. Functions pop values off the top of the stack, perform their calculation, then push the results back onto the stack.

In Uiua, functions work on a global stack of arrays.

That's enough introduction, let's see some code!

```uiua
+1 ×2 ⇡10
```

Uiua code runs from [right to left](../rtl), top to bottom. Operators are put to the *left* of their arguments, rather than in-between.

This program makes an array of all the numbers less than 10, multiplies each one by 2, then adds 1 to each.

If you want to see how that works step-by-step, try clicking the arrows beside the Run button.

Now, I can already hear you asking, *"Wait, what is that funny arrow? How am I supposed to type the multiplication sign?"*

Unlike some other array languages, Uiua does not require a special keyboard configuration or an editor with custom keybindings. Instead, you can type either the ASCII symbol or the name of a built-in function, then the Uiua formatter will convert it to the correct Unicode glyph.

In this case, the ASCII symbol for multiplication is `*` and the name of the funny arrow is [range](/docs/range).

On this website, you can format by clicking **Run** or by pressing **Ctrl+Enter** with the cursor in the text area. Try it out!

```uiua
+1*2 range10
```

You don't even have to type the whole name of a built-in function, just enough to disambiguate it from the others.

```uiua
ran10
```

If you're ever not sure what a glyph is called, you can hold ctrl/⌘ and hover over it to see its name.

You can ctrl/⌘-click any glyph in the editor to see its documentation.

Click the `↧` on the right of the editor to see a list of all the built-in functions.

## The Stack

A number in Uiua code pushes its value to the stack. On the website's editor, the values on *top* of the stack are displayed at the *bottom*. This is so that sequential lines of code show their result in the correct order.

```uiua
10 11
@c
+1 2
"Hello, World!"
# By the way, comments start with #
```

If you like, you can put values on the stack first, then operate on them.

```uiua
×++ 1 2 3 4
```

[Dup](/docs/dup) duplicates the top value on the stack.

```uiua
×.3
```

[Dup](/docs/dup) is often used in the examples on this site to show both the input and output of a function.

```uiua
√.225
```

For math functions where the order matters, like [sub](/docs/sub) and [div](/docs/div), what would normally be the second argument is instead the first. This is so you can think of fragments like `¯2` as a single unit.

If you want them to work the other way, you can use [flip](/docs/flip), which swaps the top two values on the stack.

```uiua
-3 10
-:3 10
```

By the way, since `-` is for [sub](/docs/sub), use `` ` `` for negative numbers. The formatter will turn it into a nice `¯`.

```uiua
`10
```

You can inspect the stack at any point with [stack](/docs/stack).

```uiua
+1?×2?×.-3 5
```

## Arrays

So far, we have only talked about the stack part of Uiua. Now, let's talk about the most important part: Arrays!

An array is a rectangular collection of elements arranged along some number of axes.

An array with no axes is called a scalar. All the numbers in the examples above are scalars.

An array with one axis is often called a list or a vector. An array with two axes is often called a table or a matrix.

You can make simple lists by putting `_`s between the elements.

```uiua
1_2_3_4
```

You can also just surround them with `[]`s.

```uiua
[5 6 7 8]
```

But wait! You can put whatever code you want between the brackets! The code runs from right to left as normal, and any values pushed to the stack get put in the array!

```uiua
[×3 . -2 . 10]
```

If you put arrays inside others, you can make arrays with multiple dimensions.

```uiua
[1_2_3 [4 5 6] 7_8_9]
```

```uiua
[×3. 4_5_6]
```

Some operations are *pervasive*, which means they apply to every element of an array or every pair of elements between two arrays. All the math operators are pervasive!

```uiua
√[4 9 16]
```

```uiua
×2 [1 2 3]
```

```uiua
+ 1_2_3 4_5_6
```

```uiua
× 2_10 [1_2_3 4_5_6]
```

Arrays have a [shape](/docs/shape) that describes how many elements they have along each axis.

```uiua
△5
△[]
△[9 1 6]
△[4_π_9 1_5_∞]
```

The *rank* of an array refers to the number of axes it has.

The [len](/docs/len) is the number of rows it has along its first axis.

```uiua
a ← [1_2_3_4 5_6_7_8 9_10_11_12]
△a
⧻a
⧻△a # rank
```

If you want to type that fancy `←` so you can give names to arrays, you can type `=` after a name at the start of a line, and the formatter will convert it for you.

```uiua
x = 5
+x x
```

`←` just pops the first thing off the stack and assigns it to the name on the left, so if there is already a value on the stack, you don't actually need anything on the right.

```uiua
×2 [2 3 4]
x ←
x
```

Names are case-sensitive and can only contain letters.

## Basic Array Operations

You can reverse an array's rows with [reverse](/docs/reverse).

```uiua
rev[1 2 3] # Run to format!
```

```uiua
⇌[1_2_3 4_5_6]
```

You can concatenate two arrays with [join](/docs/join).

```uiua
⊂1 [2 3 4]
⊂[1 2 3] [4 5 6]
```

You can make two arrays the rows of a new array with [couple](/docs/couple).

```uiua
⊟[1 2 3] [4 5 6]
```

You can get the first element of an array with [first](/docs/first).

```uiua
⊢[1 2 3]
```

```uiua
fir[1_2_3 4_5_6]
```

[take](/docs/take) and [drop](/docs/drop) can be used to get just part of an array.

```uiua
↙3 [1 2 3 4 5]
↘3 [1 2 3 4 5]
```

[reshape](/docs/reshape) changes the shape of an array while keeping the elements in the same order.

```uiua
↯3_3 .⇡9
```

[transpose](/docs/transpose) rotates the axes of an array.

```uiua
trans.[1_2_3 4_5_6]
```

Uiua has a lot of built-in functions like these. You can explore their documentation on the [main docs page](/docs#functions).

## Functions

If you bind a name with `←` and the code on the right does not have enough arguments to run, the code will be bound as a function and will not run until the name is used.

```uiua
F ← +1
F5
```

```uiua
👋 ← ⊂"Hello, "
👋"World"
```

## Modifiers
Modifiers (called operators or adverbs in some other array languages) are functions that take other functions as arguments. Modifiers are parsed so that if their function argument(s) immediately follow them, the function is run inside the modifier rather than before it.

[reduce](/docs/reduce) is a modifier many array-language aficionados will be familiar with. It takes its function and applies it "between" the items of an array.

One basic use of [reduce](/docs/reduce) is to sum an array.

```uiua
/+ [1 2 3 4 5]
```

It works on multi-dimensional arrays too! In this case, it adds each row to the next, effectively summing along the columns.

```uiua
/+ .[1_2_3 4_5_6 7_8_9]
```

This works with any function. For example, you can use [max](/docs/max) instead of [add](/docs/add) to get the maximum of each column rather than the sum.

```uiua
/↥ [1_2_3 4_5_2 3_1_8]
```

[rows](/docs/rows) applies a function to each row of an array.

```uiua
x ← [1_2_3 4_5_6]
  x
 ⇌x
≡⇌x
```

[rows](/docs/rows) also works *between* two arrays if it is given a dyadic function like [join](/docs/join).

```uiua
≡⊂ [1_2 3_4] [5_6 7_8]
```

There are a bunch of other modifiers that are useful in different situations. You can find a [list of them](/docs/modifier) on the main docs page.

## Inline Functions
If you need a more complex function for a modifier, you can make an inline function by surrounding code with `()`s.

In this example, we use [table](/docs/table) call a function on all combinations of rows from two array.

For each combination, we [reverse](/docs/reverse) the row from the first array, then [join](/docs/join) them together.

```uiua
⊞(⊂⇌) [1_2 3_4] [5 6 7]
```

## [fill](/docs/fill) and Nested Arrays
Here is an array that cannot be constructed normally because its rows have different [shape](/docs/shape)s.

```uiua should fail
[1 2_3_4 5_6]
```

One way to make this array work is to use the [fill](/docs/fill) modifier. You give it a fill value and a function or array that would fail with mismatched shapes, and it will fill in the missing values with the fill value.

```uiua
⬚0[1 2_3_4 5_6]
```

[fill](/docs/fill) works with lots of functions. Another one is [take](/docs/take) when the amount you are taking is more than the length of the array.

```uiua
⬚π↙ 5 [1 2 3]
```

[fill](/docs/fill) is nice, but you don't always want to fill in the missing elements. Sometimes you need to mix values of different shapes or types in an array. To understand Uiua's solution to this problem, you must first understand its *array model*.

Uiua has what is called a *flat* array model. Arrays must be rectangular and cannot mix types. However, the [box](/docs/box) function can turn any array into a *box* element that can be put in an array with other boxes. That value can then be extracted with [un](/docs/un)[box](/docs/box).

```uiua
[□1 □2_3_4 □5_6]
```

Having to use [box](/docs/box) on every value is kind of annoying, so there is a special syntax for [box](/docs/box) arrays that uses `{}`s instead of `[]`s.

```uiua
{1 2_3_4 5_6}
```

Pervasive functions work on [box](/docs/box)ed elements without needing to [un](/docs/un)[box](/docs/box) them.

```uiua
+5 {1 2_3_4 5_6}
```

For more complex operations, you can use to use the [inventory](/docs/inventory) modifier, which calls a function on the content of each box.

```uiua
{"dog" "cat" "fish"}
⍚(⊂⇌.).
```

## Inverses
Uiua leans heavily into a feature present in some other array languages: *inverses*.

The inverse of a function is the function that conceptually *undoes* it.

[un](/docs/un) is the basic inversion modifier. It does the inverting behavior of its function.

```uiua
°⊟ [1_2_3 4_5_6]
```

```uiua
°(+1) 5
```

```uiua
   ⊢ {"abc" "d" "ef"}
°□ ⊢ {"abc" "d" "ef"}
```

The [un](/docs/un)-inverse of a function must have the opposite number of arguments and outputs.

[anti](/docs/anti) has a different constraint, and can be used to access some interesting inverses.

```uiua
 ↘ 2 [1 2 3 4 5]
⌝↘ 2 [3 4 5]
```

```uiua
    ⊏ [1 2 5] "abcdef"
⬚@-⌝⊏ [1 2 5] "bcf"
```

[under](/docs/under) is probably Uiua's most powerful modifier. It calls it's first function, calls it's second function, then undoes its first.

This is useful for everything from mathematics to string processing to automatically closing file handles.

For example, [under](/docs/under) can easily generate a range between two numbers.

```uiua
⍜-⇡ 3 10
```

That example [subtract](/docs/subtract)s `3`, gets the [range](/docs/range), then [add](/docs/add)s `3` back.

Here, we [multiply](/docs/multiply) only the numbers that correspond to `1`s in the mask

```uiua
⍜▽(×10) [1 0 1 1 0 0 1 0] [1 2 3 4 5 6 7 8]
```

Here, we get the [first](/docs/first) character of each word, capitalize them with [absolute value](/docs/absolute), then put them back.

```uiua
⍜⊜⊢⌵ ⊸≠@  "under is very useful!"
```

## Multimedia
Uiua can natively generate images, audio, and GIFs.

On this site, simply leaving an array on the stack that *looks* like image or audio data will display it.

### Images
Image data can either be a rank 2 array of grayscale pixel data or a rank 3 array of grayscale with alpha, RGB, or RGBA pixel data.

This minimal example uses three different functions on x/y coordinates to generate RGB values and make a pretty gradient.

```uiua
⍉[⊞⊃⊃+-×].÷⟜⇡100
```

The Uiua logo is made with Uiua itself!

```uiua
LOGO
```

### Audio
Audio data is just an array of numbers between -1 and 1. The numbers are interpreted as samples of a waveform.

This example plays a series of notes.

```uiua
↯4[0 2 4 7 12 9 7 4]
×220 ⁿ:2÷12
÷2 ∿×τ ♭⍉⊞× ÷:⇡⁅÷8. &asr
```

### GIFs
Any array whose rows can all be turned into images can be turned into a GIF.

On this site, arrays that look like they should be GIFs will be displayed as GIFs. You can see some on the [main page](/).

GIFs can be explicitly rendered with the [`&gifs`](/docs/&gifs) function.

## Next Steps
If you want a more in-depth introduction to Uiua, you can check out the [tutorial](/tutorial/introduction).

For information on installing the native Uiua interpreter, see the [install page](/install).

For information on specific functions and modifiers, see the [functions section](/docs#functions) of the main docs page.

To see some cool examples, click through the editor at the top of the [home page](/). There are also some interesting, longer examples in the [main Uiua repository on GitHub](https://github.com/uiua-lang/uiua/tree/main/examples).
