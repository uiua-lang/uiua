# Announcing Uiua 0.18.0

2026-02-??

---

Uiua 0.18.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.18.0---2026-02-??).

You can download pre-built binaries [here](https://github.com/uiua-lang/uiua/releases).

Uiua is a general-purpose array-oriented programming language with a focus on tacit code.

This version adds some features to data definitions and many small quality-of-life features to various primitives, including adding subscripts to several primitives.

## ~~Stack~~ Arguments

From the first day it was made public, Uiua was conceived of as a stack-based language, where functions operate on a global stack. Many of the original fundamental functions simply manipulated the stack to get arguments in the right order.

However, as Uiua has developed, direct stack manipulation via functions has been gradually superseded by modifiers like [`fork ⊃`](https://uiua.org/docs/fork), [`both ∩`](https://uiua.org/docs/both), and [`by ⊸`](https://uiua.org/docs/by). These modifiers encourage thinking about data flow through the program as directing and reusing arguments rather than as manipulation of a stack.

In Uiua 0.18, this design philosophy has been solidified in a few ways:
- All mentions of a stack in tutorials and documentation have been removed.
- Data flow is now referred to in terms of a *list or arguments* being operated on by functions.
  - This was already done in many cases, but is now done everywhere.
  - Terms like "top stack value" are now replaced with "first argument"
- [`duplicate .`](https://uiua.org/docs/duplicate) has been deprecated
- [`flip :`](https://uiua.org/docs/flip) has been deprecated
- `stack ?` has been renamed to [`args ?`](https://uiua.org/docs/args)

## Data Functions

[Data functions](<https://uiua.org/tutorial/Data Definitions#data-functions>) have been stabilized.

Data functions allow you to use the fields of a data definition as bundled arguments to a function.

Data functions are defined by adding code after a data definition. The names of the data definitions fields are in scope inside this code. When the data definition is called as a function, its constructor is called, then it is passed to the function.

```uiua
~F {A B} +⊃A B
F 1 2
F @a ¯12_8_20_0_¯64
```

```uiua
~Quad [A B C] ÷×2⊃A(-⊃B(⊟⊸¯√ℂ0 -/×⊃⊃[4|A|C](˙×B)))
Quad 1 2 0
```

Data functions also enable a nice mechanism for named optional arguments. Optional arguments are set by calling the function as a macro and using the [`un ∩`](https://uiua.org/docs/un)[`by ⊸`](https://uiua.org/docs/by) idiom. A default constructed instance of the data is passed to the macro's function, the optional arugments are set, and then that data is passed to the data function.

```uiua
~F {X Y ← 1|Z ← 1} $"X=_,Y=_,Z=_" ⊃(X|Y|Z)
F 5
F!°⊸Z 10 5
F!(°⊸Y 0 °⊸Z 4) 1
F!°⊸⊃Y Z 0 4 1
```

Not every argument to the function needs to be a field in the data definition.

```uiua
~Append {N ← 1} ˜⊂▽N
Append 5 1_2_3
Append!°⊸N 4 5 1_2_3
```

## Sided [`stencil ⧈`](https://uiua.org/docs/stencil)

Previously, [`stencil ⧈`](https://uiua.org/docs/stencil) would chunk its input if the window size was rank 2. This often required double [`fix ¤`](https://uiua.org/docs/fix)ing a scalar number.

```uiua
⧈□ ¤¤3 ⇡10
```

Using a sided subscript now forces chunking behavior.

```uiua
⧈⌞□ 3 ⇡10
```

Why is this the behavior of sided subscripts? Because changing the side changes how the chunks are aligned when the window size does not evenly divide the length.

```uiua
⧈⌞□ 3 ⇡8
⧈⌟□ 3 ⇡8
```

## Sided [`join ⊂`](https://uiua.org/docs/join)

[`join ⊂`](https://uiua.org/docs/join) behaves differently depending on the rank of its arguments. Which argument is the "list" and which is the "item" (or whether they are both lists) depends on the ranks involved. This makes [`join ⊂`](https://uiua.org/docs/join) useful in many scenarios, but it can get in the way when you want a specific behavior.

[`join ⊂`](https://uiua.org/docs/join) with a sided subscript treats that side as the "item" side.

Note the differences in these examples:

```uiua
⊂ 1_2 3_4
⊂⌞1_2 3_4
⊂⌟1_2 3_4
```

```uiua
⊂ 1_2 3
⊂⌟1_2 3
⊂⌞1_2 3
```

```uiua
/◇⬚0⊂  {1_2 [3] 4_5_6}
/◇⬚0⊂⌟ {1_2 [3] 4_5_6}
```

This is also useful when you want to separate the first or last item from an array. Previously, this could be very awkward, and it was not always possible to do in a clean way. With sided [`join ⊂`](https://uiua.org/docs/join), any end or argument order is simple.

```uiua
°⊂⌞ 1_2_3
°⊂⌟ 1_2_3
°˜⊂⌞1_2_3
°˜⊂⌟1_2_3
```

## Other Notable Stabilizations

[`evert ⧋`](https://uiua.org/docs/evert) has been stabilized! It is a powerful tool for operating on the last axis of an array.

```uiua
⧋÷⟜⇡ 4_4
```

```uiua
⧋/+ [[1_2 0_5] [¯6_10 3_3]]
```

[`reciprocal ⨪`](https://uiua.org/docs/reciprocal) has been stabilized!

```uiua
⨪ [1 2 4 5 8]
```

## Removal of ASCII Inequality Aliases

Previously, some inequality functions formatted from ASCII symbol pairs:
- [`not equals ≠`](<https://uiua.org/docs/not equals>) formatted from `!=`
- [`less or equal ≤`](<https://uiua.org/docs/less or equal>) formatted from `<=`
- [`greater or equal ≥`](<https://uiua.org/docs/greater or equal>) formatted from `>=`

These aliases have been removed. They created some annoying ambiguities in the parser that made writing code like `⊃<=` impossible without `()`s to disambiguate.

You can now use aliases:
- `ne` or `neq` for [`not equals ≠`](<https://uiua.org/docs/not equals>)
- `le`, `leq`, or `lte` for [`less or equal ≤`](<https://uiua.org/docs/less or equal>)
- `ge`, `geq`, or `gte` for [`greater or equal ≥`](<https://uiua.org/docs/greater or equal>)

## Notable Breaking Changes

Subscripted [`range ⇡`](https://uiua.org/docs/range) now creates an inclusive range starting at the subscript number. This breaks cases *other* than `⇡``₁`.

```uiua
⇡₀ 5
⇡₁ 5
⇡₂ 5
⇡₋₁5
```

## Kala

As discussed in a [previous blog post](https://www.uiua.org/blog/uiuas-official-mascot), Uiua now has an official mascot: Kala the Cuttlefish!

![default image of kala, a colorful cuttlefish](https://raw.githubusercontent.com/uiua-lang/uiua/refs/heads/main/site/assets/kala/default.png)

## Contributors

Many people contributed to this release in various ways, whether by proposing new features, discussing details, or directly PRing code.

I'd like to give special thanks to two direct contributors who did a lot for this release:

[**ndren**](https://github.com/ndren) implemented several [optimizations](https://github.com/uiua-lang/uiua/commits/main/?author=ndren) in the interpreter for improved performance in several primitives, as well as the compiler itself.

[**lynn**](https://github.com/lynn) did [work](https://github.com/uiua-lang/uiua/commits/main/?author=lynn) on the Uiua website. She did a lot to make its design a bit more modern, particularly the front page.

Thanks so much to both of them!

## Thank You!

The changes listed above are only a fraction those in Uiua 0.18. You can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.18.0---2026-02-??).

As always, a huge thank you to Uiua's generous [GitHub sponsors! ❤️](https://github.com/sponsors/uiua-lang) You too could support Uiua's development and help pay my rent!

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help. We also do code challenges and discuss language features!
