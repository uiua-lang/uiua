# Inverses

Uiua has three modifiers, [un](/docs/un), [anti](/docs/anti), and [under](/docs/under), which work with *inverses*. The inverse of a function is a function that conceptually "undoes" it.

Working with inverses is a fundamental part of writing Uiua code. It is an elegant mechanism that captures many different patterns.

## [un](/docs/un)

The [un](/docs/un) modifier inverts the behavior of a function.

```uiua
°(+1) 5
```

```uiua
°⊟ [1 2] # Very common!
```

```uiua
°∿ 1 # Arcsine
```

As discussed [previously](/tutorial/Arrays#array-model), [un](/docs/un)[box](/docs/box) removes an array from a box.

```uiua
°□ ⊢{"unbox" "me!"}
```

One interesting use of [un](/docs/un) is to extract an array's rows into arguments by [un](/docs/un)ing subscripted [couple](/docs/couple) or [box](/docs/box). The number of rows in the array must match though!

```uiua
⊟₃ 1 2 3
```

```uiua
°⊟₃ [1 2 3]
```

```uiua should fail
°⊟₄ [1 2 3]
```

[un](/docs/un)ing subscripted [box](/docs/box) will unbox the items.

```uiua
°⊟₃ {1 2_3 "hmmm"}
```

```uiua
°□₃ {1 2_3 "hmmm"}
```

Some built-in functions that do not necessarily have a sensible or obvious inverse nevertheless have one defined anyway. The behavior is generally something that technically is *an* inverse, and which is also useful.

For example [un](/docs/un) [shape](/docs/shape) generates an array of incrementing integers with the given shape.

```uiua
°△ 5_5
```

```uiua
°△ 2_3_4
```

This is often used in examples to quickly generate a high-rank array.

[un](/docs/un) [select](/docs/select) produces an array of indices for the array's rows without consuming the array itself.

```uiua
°⊏ "enumerate"
```

[un](/docs/un) [transpose](/docs/transpose) transposes an array in the other direction.

```uiua
△    °△2_2_3
△  ⍉ °△2_2_3
△ °⍉ °△2_2_3
```

```uiua
   °△2_2_3
 ⍉ °△2_2_3
°⍉ °△2_2_3
```

You can find more uses of [un](/docs/un) in its documentation, including a list of all [un](/docs/un)-compatible functions and modifiers.

## [anti](/docs/anti)

The [un](/docs/un) inverse of a function must always have the opposite signature of that function. For example, if a function has signature `|2.1` (2 arguments, 1 output), then its inverse must have signature `|1.2`. This makes them easier to reason about, both for the programmer and for the compiler.

This makes some things that *seem* like they have obvious inverses not actually work. For example, [un](/docs/un)[subtract](/docs/add) is *not* just [add](/docs/sub).

```uiua should fail kala confused
°- 3 5
```

One workaround is to put one of the arguments inside the inverted function. This makes the function's signature `|1.1`, which is its own inverse.

```uiua kala arms
°(-3) 5
```

However, this is not always possible, as the argument may not be static.

The [anti](/docs/anti) modifier is similar to [un](/docs/un), but it allows the use of external arguments.

```uiua
⌝- 3 5
```

For `dyadic` function `F` and values `A` and `B`, `⌝F A B` is equivalent to `°(F A) B`, which is useful because the latter cannot be written if `A` is a dynamic value.

[anti](/docs/anti) is also equivalent to `popunon`, and so the [anti](/docs/anti) inverse of a function with signature `|a.b` is `|(b+1).(a-1)`.

[anti](/docs/anti) makes some interesting and useful behaviors available.

```uiua
⌝ⁿ 4 81 # Nth root
```

```uiua
⌝↘ 1_¯2 [1_2 3_4] # Pad
```

```uiua
⬚@-⌝⊏ 1_2_5 "abc"
```

```uiua
⬚0⌝⊡ [2_2 0_4] 3_5
```

## [under](/docs/under)

[under](/docs/under) expresses a more powerful inversion pattern. It captures the pattern of doing some transformation, modifying the data, then undoing the transformation.

This may not seem immediately useful, but you'll find it is a pattern you encounter everywhere, even in your everyday life. You might open a drawer, take something out, then close the drawer. You might get on a bus, the bus travels, then you get off the bus.

[under](/docs/under) takes two functions which we will call `F` and `G`. It calls `F`, then calls `G`, then calls an inverse of `F`.

Many functions that do not work with [un](/docs/un) work with [under](/docs/under) because [under](/docs/under) can keep track of *context*. One example of this in action is [under](/docs/under)[pick](/docs/pick), which allows us to modify an element or row of an array.

```uiua kala under
⍜(⊡2|×10) [1 2 3 4]
```

This code picks out item `2` of the array, multiplies it by `10`, then puts it back in the array.

If the values passed to [under](/docs/under)'s functions are not constants, they can also be put outside, albeit in a different order.

```uiua
⍜⊡× 2 [1 2 3 4] 10
```

This works because [under](/docs/under) keeps track of the original array and passes it to the inversion of [pick](/docs/pick).

If you wanted to set a value in an array rather than modifying it, you could use [pop](/docs/pop) or [gap](/docs/gap) instead of [multiply](/docs/multiply).

```uiua
⍜(⊡2)⋅∞ [1 2 3 4]
⍜⊡◌ 2 [1 2 3 4] ∞
```

It's not just [pick](/docs/pick)! Many functions work with [under](/docs/under)!

```uiua
⍜(↙2)/× [3 5 4 2]
```

```uiua
⍜(↻3|⊂0) [1 2 3 4 5]
```

```uiua
⍜×⁅ 1e3 π
```

```uiua
↯3_4⇡12
⊸⍜♭⇌
```

You can even use [under](/docs/under) on a function that has already been [un](/docs/un)ed. This is a nice way to work with [box](/docs/box)ed data.

```uiua
≡⍜°□(⊂⊙@!) {"wow" "cool" "omg"}
```

You can find more uses of [under](/docs/under) in its documentation, including a list of all [under](/docs/under)-compatible functions and modifiers.

## [un](/docs/un) [by](/docs/by)

The [un](/docs/un) [by](/docs/by) pattern allows you to access the "undo" part of a function's [under](/docs/under) inverse without the "do" part. This only works for monadic functions.

For example, [under](/docs/under) [first](/docs/first) allows you to modify the first row of an array. Therefore, [un](/docs/un) [by](/docs/by) [first](/docs/first) allows you to *set* the first row of an array.

```uiua
⍜⊢(×10) [2 3 4]
```

```uiua
°⊸⊢ 0 [2 3 4]
```

This is useful to set rows that are at static indices.

```uiua
°⊸(⊡2) 5 [1 2 3 4]
```

[anti](/docs/anti)[by](/docs/by) allows the index to be dynamic.

```uiua
⌝⊸⊡ 2 5 [1 2 3 4]
```

## Setting Inverses with [obverse](/docs/obverse)

There are many functions, especially more complex ones, for which the compiler cannot automatically infer an inverse. To maximize the power of a function, you may wish to define various inverses for it.

The [obverse](/docs/obverse) modifier offers a flexible system for defining multiple inverses for a function.

You can read [obverse](/docs/obverse)'s docs for the full details on how to use it, but we'll show a simple example here.

Let's say you want to define a linear interpolation function `Lerp`. The first argument will be a two-element list that is a range to interpolate between. The second argument will be a parameter to interpolate with. A parameter of `0` gives the lower value of the range. A parameter of `1` gives the higher value. All other parameter values are allowed.

```uiua
Lerp ← +⊃⊢(×/-)
Lerp 10_20 0
Lerp 10_20 0.5
Lerp 10_20 1
Lerp 10_20 1.5
Lerp 10_20 2
```

Unfortunately, the way `Lerp` has been implemented here is not invertible by the compiler.

```uiua should fail
Lerp ← +⊃⊢(×/-)
°(Lerp 10_20) 30
```

To define an inverse, we can provide a function pack of two functions to [obverse](/docs/obverse). The first function is the normal code to run, our existing `Lerp` code. The second function is our provided inverse.

In this case, the inverse function takes a range and an output value and determines the corresponding parameter.

```uiua
Lerp ← ⌅(
  +⊃⊢(×/-) # Normal lerp
| ÷⊃/-(-⊢) # Invert the lerp
)
```

This inverse ends up working with [un](/docs/un), [anti](/docs/anti), *and* [under](/docs/under)!

```uiua
Lerp ← ⌅(+⊃⊢(×/-)|÷⊃/-(-⊢))
Lerp 10_20 0.5
°(Lerp 10_20) 30
⌝Lerp 10_20 5
⍜Lerp(+5) 10_20 0.5
```

Sometimes more nuanced inverses are required. Maybe the behavior for the [un](/docs/un), [anti](/docs/anti), and [under](/docs/under) inverses needs to be subtly different in some way. Fully specifying all inverses simply requires providing [obverse](/docs/obverse) a function pack with more functions. The full description of how this works is in its documentation.

Defining custom inverses for functions allows you to create powerful and flexible behaviors, often encapsulating multiple functionalities into a single function. They are useful for everything from reversible mathematical transformations to all-in-one encoding/decoding functions.

## Challenges

```challenge
adds 100 to the 2nd and 4th rows of an array
⍜(⊏1_3|+100)
⍜⊙⊏+ 100 1_3
[1 2 3 4 5]
↯4_3⇡12
⊚10
```

```challenge
transposes an array so that the last axis becomes the first
°⍉

[[1_2_3 4_5_6] [7_8_9 10_11_12]]
↯2_4⇡8
↯2_2_4⇡16
```

```challenge
multiplies the first column of a matrix by 10
≡⍜⊢(×10)
⍜≡⊢×₁₀
[1_2 3_4]
+1↯3_3⇡9
↯2_4 1
```