# Even More Argument Manipulation

In the [More Argument Manipulation](/tutorial/More Argument Manipulation) tutorial, we learned about various ways of working with arguments including [fork](), [bracket]() and [both](). At the beginning of this tutorial, we touched briefly on [on]() and [by]().

But [on]() and [by]() are just two modifiers in a larger category.

## More Modifiers

Consider that [on]() keeps the **first** argument **before** the outputs of a function, and [by]() keeps the **last** argument **after** the outputs of a function. We can then conceive of similar modifiers that fill in other possible combinations.

[with]() keeps the **last** argument **before** the outputs of a function, and [off]() keeps the **first** argument **after** the outputs of a function.

[below]() and the `# Experimental!` [above]() do something similar, but with *every* argument instead of just the first or last.

This table shows how these six modifiers are related:

`ARG MODIFIER TABLE`

That's the theory, but how should we actually use and think about these modifiers?

## [on]()

Use [on]() when you want to transform an array with some other array, but reuse the first.

This comes up often in mathematical operations.

```uiua
÷⟜◿ 4 ⇡12
```

Another common example is getting N numbers between 0 and 1. We want to [div]() a [range]() by its length, so we use [on]() to reuse the N. This snippet is so common it has the alias `dor`.

```uiua
÷⟜⇡5
```

## [by]()

Use [by]() when you want to operate on an array based on some value *derived* from that array.

For example, if we want to [keep]() only odd numbers in an array, we derive the mask while preserving the array with [by]().

```uiua
▽⊸◿2 [2 3 8 4 9 1]
```

This works the same if we wanted to keep all numbers above or below a certain value.

```uiua
▽⊸≥4 [2 3 8 4 9 1]
```

## [off]()

Use [off]() in similar situations to [by](), but when the derived calculation also relies on later arguments.

For example, if we wanted to keep all indices in an array that have `1`s in some mask, we can use [off]() to make sure everything ends up where we want it.

```uiua
▽⤚⊏ [1 2 3 4 5] [0 1 0 0 1 1 0 1]
```

## [with]()

[with]() has fewer common patterns that come up often, (other than `<Prims prims=[Assert, With, Match]/>` for [testing](/tutorial/Testing)), but it can still be useful in some situations.

Its name was chosen because after using it, you end up with an array *with* a transformed version of it.

```uiua
⊟⤙↻1 [1 2 3 4 5] # Couple *with* rotation
```

## [below]()

Use [below]() when you want to call a function without disturbing the argument list *at all*.

[by]() is generally prefered for monadic functions on a single argument, but [below]() is useful for the more general case.

For example, here, we use [below]() to retrieve the [shape]()s of two arrays so that we can scale one array to the size of the other.

```uiua
∧(⍉▽)÷◡∩△ [1_2 3_4] °△4_6
```

For a simpler example, we can use [below]() with [gap]() to call a function on a later argument.

```uiua
◡⋅⧻ 1_2_3 "hello!"
```

This leaves the output of the function at the *beginning* of the arguments, and leaves the input to the function alone, unlike [dip]() would.

---

Mastering these argument manipulation modifiers takes time and practice. When you end up with a convoluted bit of argument manipulation code, try to see if you can simplify it by using one of these modifiers.

Often, even simple patterns can be simplified further. For example, [backward]() near [on]() is often just [off]().

```uiua
˜(▽<2)⟜⊡ [1_2 0_1] [0_1_2 3_4_5]
  ▽<2 ⤚⊡ [1_2 0_1] [0_1_2 3_4_5]
```

When you reduce a pattern to its simplest form, you can often gain a better view of the flow of data through the program.

## Sided Subscripts

Normal [numeric subscripts](/docs/subscripts) change the behavior of a function or modifier based on a number. There is another kind of subscripts that captures the idea of an operation having a certain "orientation" to the left or right.

These *sided* subscripts use `⌞` to denote "leftness" or `⌟` to denote "rightness". They are formatted from the normal subscript `,` followed by a `<` or `>`.

Currently, the only modifiers that support sided subscripts are [both]() and [bracket](). Instead of passing two separate sets of arguments to the modifier's function(s), one of the arguments will be passed to both function calls. Let's see some examples to get a better idea of how this works.

Normal [both]() calls its function on two sets of arguments.

```uiua
{∩⊟ 1 2 3 4}
```

Sided [both]() uses either the first or last argument in both calls.

```uiua
{∩⌞⊟ 1 2 3}
{∩⌟⊟ 1 2 3}
```

Sided [bracket]() has similar behavior.

```uiua
{⊓⌞⊟+ 100 20 3}
{⊓⌟⊟+ 100 20 3}
```
