# Announcing Uiua 0.14.0

2024-12-??

---

Uiua 0.14.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.14.0---2024-12-??).

Uiua is a general purpose, stack-based, array-oriented programming language with a focus on tacit code.

Here are some highlights:

## Subscripts

*Subscripts* are a new syntactic feature that allow for shorter code as well as some new behavior.

You type subscripts with a `__` followed by some digits. The formatter will turn it into subscript characters.

```uiua
undertake__3*__10 [1 2 3 4 5] # Try formatting!
```

There is an entire [blog post](https://www.uiua.org/blog/subscripts) about the addition of subscripts. You can see all currently implemented subscript-compatible functions [here](https://www.uiua.org/docs/subscripts).

- The `rerank ☇` function has been deprecated in favor of subscripted [`deshape ♭`](https://uiua.org/docs/deshape).
- The `trace ⸮` function has been deprecated in factor of subscripted [`stack ?`](https://uiua.org/docs/stack).

## [`stencil ⧈`](https://uiua.org/docs/stencil)

The `window ◫` function has been replaced by a more general [`stencil ⧈`](https://uiua.org/docs/stencil) modifier. (All instances of `window ◫` will be automatically replaced)

[`stencil ⧈`](https://uiua.org/docs/stencil) calls its function on each window of the input array.

```uiua
⧈□ 3 [1 2 3 4 5]
```

```uiua
⧈□ 2_2 °△3_4
```

## [`tuples ⧅`](https://uiua.org/docs/tuples)

The [`tuples ⧅`](https://uiua.org/docs/tuples) modifier has been stabilized.

[`tuples ⧅`](https://uiua.org/docs/tuples) makes it easy to get combinations or permutations of an array.

```uiua
⧅< 2 [1 2 3 4]
```

```uiua
⧅< ¯1 "hello"
```

## 

## Other Notable Features

- The `astar` modifier has been replaced with a new modifier called [`path`](https://uiua.org/docs/path). It has all the same functionality, but the default behavior is easier to use. It is also stable!
- [`sort ⍆`](https://uiua.org/docs/sort), [`last ⊣`](https://uiua.org/docs/last), and [`case ⍩`](https://uiua.org/docs/case) have been stabilized.