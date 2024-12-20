# Announcing Uiua 0.14.0

2024-12-20

---

Uiua 0.14.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.14.0---2024-12-20).

You can download pre-built binaries [here](https://github.com/uiua-lang/uiua/releases), or try it in the [pad](https://uiua.org/pad?src=0_14_0-rc_3__JCBVaXVhIDAuMTQuMCEK4oavMTJfMzEg4oqCOiIgICAiCg==).

Uiua is a general purpose, stack-based, array-oriented programming language with a focus on tacit code.

Here are some highlights of this release:

## Subscripts

*Subscripts* are a newly-stabilized syntactic feature that allow for shorter code as well as some new behavior.

You type subscripts with a `__` followed by some digits. The formatter will turn them into subscript characters.

```uiua
undertake__3*__10 [1 2 3 4 5] # Try formatting!
⍜↙₃×₁₀            [1 2 3 4 5]
```

```uiua
⊟₄ 1 2 3 4
□₂ "Uiua" 0_14_0
```

```uiua
[∩₃+ 1 2 3 4 5 6]
```

There is an entire [blog post](https://www.uiua.org/blog/subscripts) about the addition of subscripts. You can see all currently implemented subscript-compatible functions [here](https://www.uiua.org/docs/subscripts).

- The `rerank ☇` function has been deprecated in favor of subscripted [`deshape ♭`](https://uiua.org/docs/deshape).
- The `trace ⸮` function has been deprecated in factor of subscripted [`stack ?`](https://uiua.org/docs/stack).

## [`stencil ⧈`](https://uiua.org/docs/stencil)

The `windows ◫` function has been replaced by a more general [`stencil ⧈`](https://uiua.org/docs/stencil) modifier. (All instances of `windows ◫` will be automatically replaced)

[`stencil ⧈`](https://uiua.org/docs/stencil) calls its function on each window of the input array.

```uiua
⧈□ 3 [1 2 3 4 5]
```

```uiua
⧈□ 2_2 °△3_4
```

[`stencil ⧈`](https://uiua.org/docs/stencil) with a dyadic function makes it easy to operate on adjacent pairs!

```uiua
⧈- [3 9 2 7 3 1 1 2]
```

## [`tuples ⧅`](https://uiua.org/docs/tuples)

The [`tuples ⧅`](https://uiua.org/docs/tuples) modifier has been stabilized.

[`tuples ⧅`](https://uiua.org/docs/tuples) makes it easy to get combinations or permutations of an array.

```uiua
⧅< 2 [1 2 3 4]
```

```uiua
⍉ ⧅≥ 3 [1 2 3]
```

```uiua
⧅< ¯1 "hello"
```

[`tuples ⧅`](https://uiua.org/docs/tuples) with a monadic function operates on prefixes of an array.

```uiua
⧅□ "Uiua"
```

## Notable Breaking Changes

- [`group ⊕`](https://uiua.org/docs/group) and [`partition ⊜`](https://uiua.org/docs/partition) with a dyadic function no longer do reduction.
  - They instead group or partition multiple arrays at once.
- [`un °`](https://uiua.org/docs/un) [`json`](https://uiua.org/docs/json) no longer attempts to form multidimensional arrays
  - This makes deserializing JSON more consistent
- [`fill ⬚`](https://uiua.org/docs/fill)ed [`scan \\`](https://uiua.org/docs/scan) now also sets the initial value, similar to [`reduce /`](https://uiua.org/docs/reduce)
- Negative indices to [`pick ⊡`](https://uiua.org/docs/pick) and [`select ⊏`](https://uiua.org/docs/select) now always use a fill value if available

### [`obverse ⌅`](https://uiua.org/docs/obverse) Changes

[`obverse ⌅`](https://uiua.org/docs/obverse) with a single function now just nullifies the inverse rather than making a function its own inverse. This makes a lot of common [`under ⍜`](https://uiua.org/docs/under) patterns much simpler.

For example, lets say you have a function that splits a string into lines and words.

```uiua
F ← ⍚⊜□⊸≠@  ⊜□⊸≠@\n
F $ These are
  $ some words
```

Great! Now you want to do this split, reverse each line (keeping letters of words in order), and join everything back together. Seems like a great use of [`under ⍜`](https://uiua.org/docs/under)!

```uiua should fail
F ← ⍚⊜□⊸≠@  ⊜□⊸≠@\n
⍜F≡⇌ $ These are
     $ some words
```

Alas, this does not work because [`by ⊸`](https://uiua.org/docs/by) [`not equals ≠`](https://uiua.org/docs/not%20equals) does not have an [`under ⍜`](https://uiua.org/docs/under) inverse.

You could previously get around this by using [`obverse ⌅`](https://uiua.org/docs/obverse) with a function pack with an empty second function to nullify the inverse, but it was a bit verbose.

```uiua
F ← ⍚⊜□⌅(⊸≠@ |) ⊜□⌅(⊸≠@\n|)
⍜F≡⇌ $ These are
     $ some words
```

It turns out this pattern can be very common, so this is now the default, non-pack behavior of [`obverse ⌅`](https://uiua.org/docs/obverse). There is also a new rule about constants in [`under ⍜`](https://uiua.org/docs/under). These come together to make this function much simpler to write!

```uiua
F ← ⍚⊜□⌅⊸≠@  ⊜□⌅⊸≠@\n
⍜F≡⇌ $ These are
     $ some words
```

## `uiua --window`

In the native interpreter, the new `--window`/`-w` flag causes a window to open to display the output of a program, rather than printing it to the console.

It can also display images and gifs, and can play audio too!

It can be run as `uiua -w`, `uiua run -w`, or `uiua watch -w`.

![Uiua window showcase](https://i.gyazo.com/798d48f1192cf89b41fbb7d245351d68.gif)

## Rayua

This is not necessarily part of 0.14.0 itself, but [rayua](https://github.com/uiua-lang/rayua), the Uiua bindings for [raylib](https://www.raylib.com/) has come far enough to create a Flappy Bird clone! Uiua game dev is here!

![Rayua Flappy Bird Showcase](https://i.gyazo.com/274c9a661c63bda7a5c29979bded874f.gif)

You can find the code for this example [here](https://github.com/uiua-lang/rayua/blob/main/examples/example_flappy_bird.ua).

Thanks a lot to [ekgame](https://github.com/ekgame) and [Omnikar](https://github.com/Omnikar) for developing the bulk of this!

You can import `rayua` into a Uiua file with this line:
```
~ "git: github.com/uiua-lang/rayua
```

## Other Notable Features

- [`sort ⍆`](https://uiua.org/docs/sort), [`last ⊣`](https://uiua.org/docs/last), and [`case ⍩`](https://uiua.org/docs/case) have been stabilized.
- Functions with declared signatures that do not match the inferred signature will now cause a warning rather than an error.
  - You can read more about this feature in the updated [Stack Signatures](https://www.uiua.org/tutorial/functions#stack-signatures) section of the tutorial.
- New [`# Deprecated!`](https://uiua.org/docs/semanticcomment#deprecated) semantic comments will cause a function to emit a warning when it is used.
- The `astar` modifier has been deprecated and replaced with a new modifier called [`path`](https://uiua.org/docs/path). It has all the same functionality, but the default behavior is easier to use. It is also not `# Experimental!`.
- New [adjecency offset constants](https://www.uiua.org/docs/constants#A%E2%82%81) make it easier to use [`path`](https://uiua.org/docs/path) with grids.
  - ```uiua
    {A₂ C₂}
    ```

## ☃️❄️💖🎅🏻🎄

A big thank you to everyone who contributed to this release!

A special, heartfelt thanks to Uiua's generous [GitHub sponsors](https://github.com/sponsors/uiua-lang)!

Again, you can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.14.0---2024-12-20).

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help. We also do code challenges and discuss language features!