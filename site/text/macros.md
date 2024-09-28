# Macros
Defining your own functions that work on arrays is pretty easy. Just a name, a `←`, and you're done.

But what if you want to define functions that use other functions?

## Placeholders and `!`s
Anywhere you can put a built-in or inline function, you can also put a `^` followed by a number. This is called a *placeholder*.

Any named function with `^`s in it is a macro.

However, there is one additional requirement: macros must have names that end in as many `!`s as the number of functions they take.

Macros work similarly to modifiers. They take some function arguments and modify how they are used.

The number that comes after a `^` is the index of the function argument passed to the macro.

Let's look at a simple example using [reduce](). It reduces a function over the numbers up to the given range.

```uiua
ReduceRange! ← /^0+1⇡
ReduceRange!+5
ReduceRange!×4
```

Here is another simple example which calls a function on a reversed version of each row of an array.

```uiua
OnRev! ← ≡⍜⇌^0
OnRev!(↘1) ↯3_4⇡12
OnRev!(⊂π) ↯3_4⇡12
```

A macro can take as many functions as you want. Modifiers with two or more function arguments will be formatted to use `‼`s as needed. Try running the following example to format it.

```uiua
F!!! ← ⊂/^0⊃^1^2
F!!!+×⊂ [1 2 3][4 5 6]
```

Placeholders with the same number can of course be reused to use that function multiple times.

For example, if we wanted to call each of two functions twice, but in a different order:

```uiua
F‼ ← ^1^0^0^1
F‼(×2|+1) 5
F‼(+1|⊂0) 1
```

## Two Kinds of Macros
The macros described so far are called *index macros*, because arguments are referenced directly by their position when the macro is called.

But Uiua actually has a second kind of macro. *Code macros* put their operands in an array. The array can then be arbitrarily manipulated with normal Uiua code.

## Code Macros
Code macros are defined by putting a `^` right after the binding's `←`. Code macro names must still end in some number of `!`s.

Here is a basic example that simply prints its operands. It returns the number `5` as the actual generated code.

```uiua
F‼ ←^ "5" &pf
F‼⊂(+1)
```

As you can see, the operands are passed to the function as an array of boxed strings.

Code macros may be passed a function pack operand. Each operand from the pack will be put in the array.

```uiua
F! ←^ $"_"
F!(+|-|×|÷)
```

The code macro's function must return either a string or an array of boxed strings. This value will be converted back to Uiua code and compiled as normal.

Format strings can help a lot in generating new code. For example, if we wanted to make a version of [both]() that calls its function on an arbitrary number of sets of values, we could use [reshape]() and [bracket]().

```uiua
All‼ ←^ $"⊓(_)" /$"_|_" ↯⋕ °{⊙∘}
[All‼3+ 1 2 3 4 5 6]
```

First, we extract the two operands: the count and the function. The count comes in as a string, so we have to [parse]() it before using [keep]() to make an array of copies of the function.

We use [reduce]() with a format string to form the branches of a function pack, then use another format string to put them in [bracket]().

The resulting string is then compiled as Uiua code.

Code macros have the ability to create new bindings, including new macros.

```uiua
Def‼ ←^ $"_\n_" ⊃(/$"_ ← _"|/$"Also_ ← _")
Def‼(X|5)
+ X AlsoX
```

This is a simple example, but this concept can be used to create very powerful meta-programming tools.

## Compile Time vs Run Time
The body of a code macro is always evaluated at compile time. One consequence of this is that bindings whose values cannot be known at compile time cannot be used in a code macro.

For example, because the value `5` is always the same, it is always known at compile time, and we can use a name that binds `5` in a code macro.

```uiua
x ← 5
F! ←^ $"_ _":x ⊢
F!¯
```

However, if we use a value that cannot be known at compile time, like the result of the [rand]() function, we will get an error.

```uiua should fail
x ← ⚂
F! ←^ $"_ _":x ⊢
F!¯
``` 

There are two ways to work around this. The first is to simply put the code that generates the value in the macro itself.

```uiua
F! ←^ $"_ ⚂" ⊢
F!¯
```

The second is to use the [comptime]() modifier, which forces its function to be evaluated at compile time.

```uiua
x ← comptime(⚂)
F! ←^ $"_ _":x ⊢
F!¯
```

## What kind of macro should I use?
Which kind of macro you use depends on what kind of code you are writing.

Code macros are much more powerful than index macros, but they can be more complicated to write.

Additionally, index macros are [hygienic](https://en.wikipedia.org/wiki/hygienic_macro). When an index macro refers to names of things, bindings you have defined in the surrounding code will not interfere; you will never accidentally use the wrong binding. Code macros make no such guarantees.

If you conceptually just want to define your own modifier, an index macro is probably the simplest way to go.

If you want the full power (and all the complexity) of compile-time meta-programming, you'll need to use a code macro.

