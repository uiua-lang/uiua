# Tacit Code

# Tacit Code

So far, most of the code examples in this tutorial have been fairly short. While Uiua is great for short, simple code, it is designed to be a general-purpose language. Uiua aims to be decent for everything from Code Golf to command-line utilities, from websites to games.

However, it may not be immediately clear how to write more complex code that does not refer to variable names.

## What Uiua asks of you

When you first start using Uiua beyond just simple examples and challenges, you will likely encounter difficulty passing more than a couple values around.

In disallowing named local variables, Uiua asks something of the programmer that most other languages do not. It asks that you re-orient the way you think about data. How you refer to it, how it flows through a program.

If you pay the price of this re-orientation, Uiua offers you a few things in return.

For one, you end up writing a lot less code. When you don't bind lots of local variables and constantly refer to them in expressions, your code ends up being much shorter. This makes scanning through code require less scrolling and jumping around.

This is one of the many ways that Uiua reduces *ceremony.* Ceremony in a programming language is all the code you have to write that is not directly related to the problem you are trying to solve. This includes everything from minor syntax like braces and keywords to complex boilerplate like interface implementations. Uiua does not eliminate all ceremony (no language can), but it aims to eliminate as much as possible while maintaining a certain level of readability and structure.

This is not to say that local bindings are not useful in other languages. There are two original motivations for Uiua eliminating local variables: simplicity of implementation, and beauty.

*If you are not too concerned with performance and implementation, you can skip this paragraph.* Uiua's arrays are garbage-collected via reference counting. This reference count is also used when mutating an array. For example, if you [reverse]() an array when no duplicates exist, the array is simply reversed in place. However, if at least one other copy of the array exists somewhere, the array's entire buffer will be copied, and this new copy will be reversed instead. If variables could be locally bound, a copy would have to be stored for the duration of the variable's scope. Without complex escape analysis, this could lead to unnecessary copies when using local variables. Because arguments are cleaned up as they are used, arrays only need to be copied as often as is actually necessary!

But in the end, the primary motivation for forbidding local variables is that leads to code that is both beautiful and enjoyable to write. This is, of course, highly subjective, but if you've made it this far into the tutorial, then hopefully you've seen some of that beauty, felt some of that joy.

## A Motivating Example

If an operation accesses arguments in a more complicated way, it may not be immediately obvious how to use the argument manipulation modifiers to implement it.

As a motivating example, let's attempt to implement the quadratic formula. Given numbers `a`, `b`, and `c`, the roots of the function `ax² + bx + c` can be found via the expression `(-b ± √(b² - 4ac)) / 2a`.

We'll start with a blank definition:

```uiua
Quad ←
Quad 1 ¯3 2
```

The simplest way to approach a problem like this is to break it up into smaller pieces, then put those parts together with liberal use of [fork]().

We'll start with the `4ac` term. This is implemented easily using [planet notation](</new-tutorial/More Argument Manipulation#planet-notation>).

```uiua
Quad ← ×4×⊙⋅∘
Quad 1 ¯3 2
```

We can also calculate the `b²` part by wrapping both expressions in a [fork](). Note the use of [gap]() to skip `a` and square `b` instead.

```uiua
Quad ← ⊃(×4×⊙⋅∘)⋅˙×
Quad 1 ¯3 2
```

Because we ordered the [fork]()'s functions in this way, the arguments to the `-` in `b² - 4ac` are already in the right order. Now we have the full discriminant.

```uiua
Quad ← -⊃(×4×⊙⋅∘)⋅˙×
Quad 1 ¯3 2
```

Because the results of the quadratic formula can be complex numbers, we need to convert the discriminant to [complex]() before taking the [sqrt](). 

```uiua
Quad ← √ℂ0 -⊃(×4×⊙⋅∘)⋅˙×
Quad 1 ¯3 2
```

The `±` in the formula comes from the fact that there are two valid square roots for any number. We can express this by [couple]()ing the square root with its [negate]()d version.

```uiua
Quad ← ⊟⟜¯ √ℂ0 -⊃(×4×⊙⋅∘)⋅˙×
Quad 1 ¯3 2
```

We can express `-b` easily enough with another [fork](). We use `⋅∘` to select out `b`.

```uiua
Quad ← -⊃⋅∘(⊟⟜¯ √ℂ0 -⊃(×4×⊙⋅∘)⋅˙×)
Quad 1 ¯3 2
```

With one more [fork](), we can express `/ 2a`.

```uiua
Quad ← ÷⊃(×2|-⊃⋅∘(⊟⟜¯ √ℂ0 -⊃(×4×⊙⋅∘)⋅˙×))
Quad 1 ¯3 2
```

And there we have it, the Quadratic Formula! One important quality this code has is that, for every operation, it is immediately apparent what the arguments are. We can easily see which arguments are passed to [divide]() and [subtract]() because there is a [fork]() immediately to their right. This is important for keeping the code clear and readable.

If you like, you can pull out the discriminant into its own function to break the code up a little.

```uiua
Disc ↚ -⊃(×4×⊙⋅∘)⋅˙×
Quad ← ÷⊃(×2|-⊃⋅∘(⊟⟜¯ √ℂ0 Disc))
Quad 1 ¯3 2
```

## Reordering

Sometimes, the argument order that is most convenient for calling a function is not the same as the argument order that is easiest to work with in the implementation. In these cases, it may be useful to do a one-time argument reordering at the beginning of the function. It is good to annotate this reordering with a [line signature comment](/tutorial/documentation).

```uiua
Quad ← (
  ⊃⊙⋅∘⋅∘ # ? a c b
  ÷⊃(×2|-⤙(⊟⟜¯ √ℂ0 -×4⊓×˙×))
)
Quad 1 ¯3 2
```

Notice that by reordering the arguments at the beginning, we require less argument manipulation later on. Whether refactoring this way is more readable is up to your discretion.

Like all programming languages (though perhaps more than some), writing Uiua code is as much art as it is science. The deconstruction of a problem, the synthesis of a solution, the care for the reader; these are all things you get a feel for as you work more with the language.
