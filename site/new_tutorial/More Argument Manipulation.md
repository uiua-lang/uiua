# More Argument Manipulation

[self](), [backward](), [on](), [by](), and [dip](), which were introduced in the [first tutorial](</new-tutorial/Basic Data Manipulation#Manipulating-Data-with-Modifiers>), can get you pretty far writing tacit code. However, there are some argument access patterns that require much more powerful control over which arguments go where.

## [fork]()

[fork]() is a dyadic modifier that takes 2 functions and calls them both on the same set of arguments. The number of arguments used is the maximum of the two functions.

```uiua
[⊃+× 3 5]
```

If one of the functions takes more arguments than the other, the function with fewer arguments uses the left-most values.

```uiua
⊃×⇌ [1 2 3] 10
```

We'll see just how important [fork]() is later in this section.

## [both]()

[both]() is a monadic modifier and a sort of compliment to [fork](). While [fork]() calls multiple functions on the same set of arguments, [both]() calls a *single* function on [multiple]() sets of arguments.

```uiua
∩⇌ [1 2 3] [4 5 6]
```

```uiua
∩× 2 3 5 7
```

## [bracket]()

To round off the trio, we have [bracket](), a dyadic modifier that calls each of its functions on a different set of arguments.

```uiua
[⊓+× 1 2 3 4]
```

## Function Packs

All dyadic modifiers allow a special notation with a single set of `()`s with a `|` in the middle separating the functions. This is called a *function pack*.

```uiua
⊓(+|×) 1 2 3 4
```

While all dyadic modifiers can use function packs, [fork]() and [bracket]() allow more than 2 functions to be used without having to repeat the modifier itself.

```uiua
[⊃(+|-|×|÷) 5 8]
```

```uiua
[⊓(+1|×|÷2) 5 10 12 22]
```

Because wrapping the result of a function pack in array notation brackets is somewhat common, we can actually replace the function pack's `()`s with the brackets themselves.

```uiua
⊃[+|-|×|÷] 5 8
```

## Subscripts

Subscripts are a special syntax that allows you to augment some functions and modifiers with a number.

Subscripts are typed with `,` followed by some digits. The formatter will turn them into subscript digit characters. A leading negative sign is allowed.

Several functions and modifiers are supported, but we'll only cover some relevant ones here. You can find a full list of subscript-compatible functions [here](/docs/subscripts).

Subscripted [both]() calls its function on N sets of arguments.

```uiua
[∩+ 1 2 3 4]
[∩,3+ 1 2 3 4 5 6]
[∩,4+ 1 2 3 4 5 6 7 8] # Try formatting!
```

Subscripted [couple]() collects N arguments into an array.

```uiua
⊟₄ 1 2 3 4 5
```

[box]() has similar behavior, but it boxes each value.

```uiua
□₃ 5 "Hi!" [1 2 3]
```

## 🌍 Planet Notation 🪐

The [gap]() modifier discards the first argument and calls its function.

```uiua
⋅+ 1 2 3
```

This is of limitied usefulness on its own.

However, by combining [dip]()s and [gap]()s along with the [identity]() function inside a [fork](), we get a useful emergent notation. Chained [dip]()s and [gaps]() (with a terminating [identity]()) act as a sort of boolean selector to choose which arguments to keep and which to discard in a branch.

This is called *planet notation*, because it looks like the planets in a solar system chart.

For example, let's say you want to [multiply]() the 2nd and 4th arguments and discard the rest.

```uiua
×⋅⊙⋅∘ 10 2 3 4
```

Notice how the circles correspond to the arguments we want.

Alternatively, maybe you want to [add]() the 1st and 3rd arguments while discarding the 2nd and 4th. In this case, you would terminal the chain with [pop]() rather than [identity]().

```uiua
+⊙⋅⊙◌ 10 2 3 4
```

Maybe you want to [add]() 3 numbers but keep the second 2 around before them.

```uiua
[⊃⋅⊙∘(++)] 2 5 10
```

You can read `⋅⊙∘` as "discard argument 1, keep argument 2, keep argument 3".

If you only wanted to keep argument 2, you simply make the expression shorter:

```uiua
[⊃⋅∘(++)] 2 5 10
```

For a more useful example, let's do a complex mathematical expression. We will implement this function (shown here in mathematical notation):

```not uiua
f(a,b,c,x) = (a+x)(bx-c)
```

We'll start with the `(a + x)` part. We can grab `a` and `x` with [dip]() and [identity](), and ignore `b` and `c` with [gap]().

```uiua
+⊙⋅⋅∘ 1 2 3 4
```

Next, we'll do the `(bx-c)` part. We can grab each term with [fork]().

```uiua
-⊃(⋅⋅∘)(×⋅⊙⋅∘) 1 2 3 4
```

The first pair of `()`s is not actually necessary, so let's remove them.

```uiua
-⊃⋅⋅∘(×⋅⊙⋅∘) 1 2 3 4
```

Finally, we can combine the two parts with another [fork]().

```uiua
×⊃(+⊙⋅⋅∘)(-⊃⋅⋅∘(×⋅⊙⋅∘)) 1 2 3 4
```

If you like, you can factor out the [gap]() in the second part.

```uiua
×⊃(+⊙⋅⋅∘)⋅(-⊃⋅∘(×⊙⋅∘)) 1 2 3 4
```

Alternatively, you can use a function pack.

```uiua
×⊃(+⊙⋅⋅∘|-⊃⋅⋅∘(×⋅⊙⋅∘)) 1 2 3 4
```

And there you have it! A readable syntax juggling lots of values without any names!

It's annoying to write long lists of names like `gapdipgapgapide`, so those three functions (plus [pop]()) have a special rule in the parser that allows you to write them with only 1 character as long as there are at least 2 characters in the sequence. Also, 'i' and 'p' for [identity]() and [pop]() only work if they are the last character.

Try it out!

```uiua
+gdggi 1 2 3 4 5
```
```uiua
+dggdp 1 2 3 4 5
```
```uiua
-fggi* 10 3 5
```

## Challenges

```challenge
moves the 4th argument in front of the first
⊃⋅⋅⋅∘⊙⊙∘
⤙⊙⊙⊙◌
1 2 3 4
@x [1 2 3] □5 27
```

```challenge
finds both the sum and product of three arguments
⊃(++|××)

4 5 6
10 10 10
1_2 3_4 5
```

```challenge
collects 9 values from the stack evenly into 3 arrays
∩₃⊟₃

1 2 3 4 5 6 7 8 9
@G @o @o @d @  @j @o @b @!
5 0 1 1_2 3_4 5_6 @a @b @c
```

```challenge
for numbers A, B, C, and D calculates (A+C)×(B+D)
×⊃(+⊙⋅∘|+⋅⊙⋅∘)
×˜∩+
1 2 3 4
10 ¯3 1 0
3 ¯7 2 2
1_2 3_4 5_6 7
```