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

*If you are not too concerned with performance and implementation, you can skip this paragraph.* Uiua's arrays are garbage-collected via reference counting. This reference count is also used when mutating an array. For example, if you [reverse]() an array when no duplicates exist, the array is simply reversed in place. However, if at least one other copy of the array exists on the stack, the array's entire buffer will be copied, and this new copy will be reversed instead. If variables could be locally bound, a copy would have to be stored for the duration of the variable's scope. Without complex escape analysis, this could lead to unnecessary copies when using local variables. Because values on the stack are cleaned up as they are used, arrays only need to be copied as often as is actually necessary!

But in the end, the primary motivation for forbidding local variables is that leads to code that is both beautiful and enjoyable to write. This is, of course, highly subjective, but if you've made it this far into the tutorial, then hopefully you've seen some of that beauty, felt some of that joy.

## The Stack Pitfall

Being stack-based is Uiua's key to being usable as a pure-tacit language. However, the stack can be an unwieldy tool if used recklessly. Many stack languages have built-in functions for rotating the stack, fishing values up from deep in the stack, or arbitrarily reordering it. While these things are technically possible in Uiua, they are discouraged, and the code for them is verbose by design.

Uiua encourages a more structured approach to stack manipulation. There are no single functions for rotating the stack or for swapping more than 2 values.

When complex stack manipulation *is* required, it is usually done with [planet notation](/tutorial/advancedstack#planet-notation). Planet notation allows you to *visualize* the way values move around.

## A Motivating Example

The online Uiua pad and the `uiua watch` command in the native interpreter make it easy to write Uiua code interactively. You can easily see the state of the stack after each change you make to the code.

This iterative process is good for exploring possibilities, and it is the intended way to write Uiua code. However, a naive, ad-hoc approach to stack manipulation often leads to code that is very hard to read.

As a motivating example, let's attempt to implement the quadratic formula. Given numbers `a`, `b`, and `c`, the roots of the function `ax² + bx + c` can be found via the expression `(-b ± √(b² - 4ac)) / 2a`.

This is a useful example because it involves juggling 3 arguments that are used in a non-regular way.

Let's start with the discriminant term ` b² - 4ac`.

```uiua
Disc ← # Code goes here
Disc 1 2 0
```

To show how you might build up a solution with only stack reordering, we'll only use [`duplicate`](), [`flip`](), [`over`](), and [`dip`]() to attempt to get all the arguments in the right order.

First, we'll might try to get `a` and `c` next to each other above `b` on the stack.

```uiua
Disc ← ⊙:
Disc 1 2 0
```

Because `a` and `c` are on top of the stack, making the `4ac` term is easy.

```uiua
Disc ← ××4 ⊙:
Disc 1 2 0
```

We can get down to `b` with [dip](), create the `b²` term, and [subtract]().

```uiua
Disc ← -⊙(ⁿ2)××4 ⊙:
Disc 1 2 0
```

That finishes the discriminant. Next, we'll account for [complex]() roots and take the [sqrt]().

```uiua
Disc ← √ℂ0 -⊙(ⁿ2)××4 ⊙:
Disc 1 2 0
```

We can implement `±` by [couple]()ing the value with itself [negate]()d.

```uiua
Quad ← ⊟¯. √ℂ0 -⊙(ⁿ2)××4 ⊙:
Quad 1 2 0
```

And now we have a problem. We still need to use `a` and `b` one more time, but they have already been consumed.
`a` and `b` start at the top of the stack, so we can copy them with [over]() and put the rest of out code in two [dip]()s.

```uiua
Quad ← ⊙⊙(⊟¯. √ℂ0 -⊙(ⁿ2)××4 ⊙:),,
Quad 1 2 0
```

Then we'll [subtract]() `b`... 

```uiua
Quad ← ⊙(-⊙(⊟¯. √ℂ0 -⊙(ⁿ2)××4 ⊙:)),,
Quad 1 2 0
```

...and [divide]() by `2a`.

```uiua
Quad ← ÷×2⊙(-⊙(⊟¯. √ℂ0 -⊙(ⁿ2)××4 ⊙:)),,
Quad 1 2 0
```

And there we have it, the quadratic formula.

```uiua
Quad ← ÷×2⊙(-⊙(⊟¯. √ℂ0 -⊙(ⁿ2)××4 ⊙:)),,
Quad 1 2 0
Quad 1 2 5
Quad 2 3 1
```

On close inspection, the astute reader may notice that the above code sucks. What's worse, it's not even as bad as it could be. If you hadn't thought to use [over]() and [dip]() in that way, you may have instead used `:⊙:` to rotate 3 values on the stack, making it even more convoluted.

The problem with reordering stack values this often is that the state of the stack at any point in the code gets harder and harder for the writer to keep in their head. It also makes it much harder for the reader to deduce the state of the stack at a glance.

## Stack-Source Locality

The code above is also obtuse for another reason.

Imagine a person who is less familiar with this code going to read it. It may be someone else, but it may also be a future version of yourself. If they look at the leftmost term `÷×2`, they'll likely be able to quickly tell that it takes two arguments. But how do they figure out what those arguments are? They would have to make their way all the way to the *other side of the function* to find the [over]() that creates the copy of `a`. They would only end up there after having built up the mental model of the state of the stack throughout the *entire function*.

This obtuseness is the result of the above code violating a fundamental principle of writing good Uiua code, that of *stack-source locality*. Stated simply, **code that creates values should be as close as possible to the code that uses those values**.

In our example, [divide]() and the [over]() that creates its argument are on opposite sides of the function: a massive violation of stack-source locality.

This principle is not a formula you can plug values into. It is not a set of procedures that will make code better. It is a guiding tenet meant to shape the way you think about the flow of your data and how you structure your programs. How well a given code snippet maintains stack-source locality is up to interpretation, and different Uiua programmers may interpret it differently, even for the same program.

## A Better Way

So how do we write better Uiua code? How do we keep stack-source locality? How do we avoid making the stack so convoluted that our code becomes unreadable?

The short answer is to make liberal use of [fork]().

The power of [fork](), [dip](), [gap](), [on](), and [by]() is that they allow access to arbitrary values on the stack *without* reordering it. When the stack maintains its order, it is much easier to reason about values' position on it, since their positions seldom change relative to each other.

Let's redo the quadratic formula implementation using these modifiers.

We'll start again with the discriminant.

```uiua
Disc ← -⊃(××4⊙⋅∘)⋅(ⁿ2)
Disc 1 2 0
```

Notice that when we use planet notation, it is easier to tell which functions are being applied to which values.

We'll implement the `√` and `±` in the same way as before.

```uiua
Disc ← ⊟¯. √ℂ0 -⊃(××4⊙⋅∘)⋅(ⁿ2)
Disc 1 2 0
```

Even though `b` has been consumed, we can gain access to it again using another [fork]() and implement the `-b` term.

```uiua
Quad ← -⊃⋅∘(⊟¯. √ℂ0 -⊃(××4⊙⋅∘)⋅(ⁿ2))
Quad 1 2 0
```

Then, we can use another [fork]() to add the `/ 2a` part.

```uiua
Quad ← ÷⊃(×2|-⊃⋅∘(⊟¯. √ℂ0 -⊃(××4⊙⋅∘)⋅(ⁿ2)))
Quad 1 2 0
```

Long lines like this can hurt readability. One thing we can do to alleviate this is split the discriminant onto its own line.

```uiua
Quad ← ÷⊃(×2)(
  -⊃⋅∘(
    -⊃(××4⊙⋅∘)⋅(ⁿ2)
    ⊟¯. √ℂ0
  )
)
Quad 1 2 0
```

Alternatively, we can pull the discriminant into its own function.

```uiua
# A thing of beauty
Disc ← -⊃(××4⊙⋅∘)⋅(ⁿ2)
Quad ← ÷⊃(×2|-⊃⋅∘(⊟¯. √ℂ0 Disc))
Quad 1 2 0
```

Let's compare this solution to the previous one. To improve the comparison, we'll make the discriminant its own function here as well.

```uiua
Disc ← -⊙(ⁿ2)××4 ⊙:
Quad ← ÷×2⊙(-⊙(⊟¯. √ℂ0 Disc)),,
Quad 1 2 0
```

The difference is night-and-day. The old, naive solution, even with the benefit of being broken up, still has all of its same issues.

If we look in the improved solution and do the same search for the source of [divide]()'s arguments, we don't have to go far before finding the [fork]() with `×2` and `-`. Stack-source locality holds for all parts of the code!

# When to Reorder

While reordering stack values is discouraged, a little bit of reordering up front can sometimes greatly simplify subsequent code. The recommended place to do this is at the very beginning of a function.

A function should *take* its arguments in the order that is most natural for calling. If necessary, you may choose to reorder a functions arguments at the beginning of its body so that they are easier to work with inside the function.

If we for some reason decided that the best calling order for our `Quad` function was `c` `b` `a`, then we could add a reordering step to the beginning and keep the rest of the implementation the same.

```uiua
Disc ← -⊃(××4⊙⋅∘)⋅(ⁿ2)
Quad ← (
  ⊃(⋅⋅∘|⋅∘|∘) # Reorder
  ÷⊃(×2|-⊃⋅∘(⊟¯. √ℂ0 Disc))
)
Quad 0 2 1
```

Even though the reordering step could be written shorter as `⊃⋅⋅∘:` it is written in a long form here for the benefit of the reader. But you may decide that `⊃⋅⋅∘:` is perfectly clear, and that's fine!

## Three Rules

As stated before, the advice in this section is just that, advice. It is not a set of hard and fast rules that must be followed.

However, if you are the kind of person that *likes* a simple list of rules, then here it is:

- **Reorder the stack as little as possible**
- **Break up long lines with whitespace or into separate functions**
- **Maintain stack-source locality**

Like all programming languages (though perhaps more than some), writing Uiua code is as much art as it is science. The deconstruction of a problem, the synthesis of a solution, the care for the reader; these are all things you get a feel for as you work more with the language.