# Announcing Uiua 0.11.0

2024-06-??

Uiua 0.11.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.11.0---2024-05-??).

Uiua is a general purpose, stack-based, array programming language with a focus on tacit code.

While this release does not have any major new features, it extends the functionality of many primitives, optimizes many common patterns, and fixes a number of bugs.

Here are some of the highlights:

# Multi-argument [`reduce /`](https://uiua.org/docs/reduce)

[`reduce /`](https://uiua.org/docs/reduce) takes a dyadic function and applies it "between" all rows of an array.

```uiua
/+ [1 2 3 4 5]
```

[`reduce /`](https://uiua.org/docs/reduce) can now take multiple arguments if its function takes more than two arguments. Additional arguments are interspersed between the rows and are passed above the main array on the stack.

```uiua
/(⊂⊂) 0 [1 2 3 4]
```

This is particularly useful when used with [`content `](https://uiua.org/docs/content) and [`join ⊂`](https://uiua.org/docs/join) to intersperse a delimiter between a list of strings.

```uiua
/◇(⊂⊂) @, {"cat" "dog" "bird" "fish"}
```

# [`json`](https://uiua.org/docs/json) and [`xlsx`](https://uiua.org/docs/xlsx)

The [`json`](https://uiua.org/docs/json) and [`xlsx`](https://uiua.org/docs/xlsx) functions allow the encoding and decoding of JSON and XLSX data respectively.

`json` converts an array to a JSON string.

```uiua
json [1 2 3 4]
```

It works with `map`s as well.

```uiua
json map {"name" "age"} {"Dan" 31}
```

[`un °`](https://uiua.org/docs/un) `json` decodes a JSON string.

```uiua
°json $ {"type": "requires", "content": "json", "ids": [38, 22, 5]}
```

`xlsx` is similar, but is works with binary data rather than strings.

# [`take ↙`](https://uiua.org/docs/take)/[`drop ↘`](https://uiua.org/docs/drop) [`infinity ∞`](https://uiua.org/docs/infinity)

[`take ↙`](https://uiua.org/docs/take) and [`drop ↘`](https://uiua.org/docs/drop) isolate part of an array.

```uiua
↙ 3 [1 2 3 4 5]
↘ 3 [1 2 3 4 5]
```

Multidimensional indices have always been supported.

```uiua
↙2_2 . ↯3_4⇡12
```

You can now provide [`infinity ∞`](https://uiua.org/docs/infinity) as one or more of the indices to [`take ↙`](https://uiua.org/docs/take) or [`drop ↘`](https://uiua.org/docs/drop) that entire axis.

```uiua
↙∞_2 . ↯3_4⇡12
```
``` uiua
↙1_∞_2 . ↯2_3_4⇡24
```

# Swizzles

Swizzles are a new experimental feature that allow concise manipulation of the stack and extraction from arrays.

Stack swizzles are written with a `λ` followed by some letters. The stack will be rearranged accordingly. `λ` formats from `'` when followed by letters.

```uiua
# Experimental!
[λccab 1 2 3]
```

Capital letters will [`fix ¤`](https://uiua.org/docs/fix) the corresponding array. This is useful with complex [`rows ≡`](https://uiua.org/docs/rows) operations.

```uiua
# Experimental!
≡(⊂⊂) ? λaBC 1_2 3_4 5_6
```

*Array* swizzles are written with a `⋊` followed by some letters. Rows from the array that correspond to the letters will be put on the stack. `⋊` formats from `''` when followed by letters.

```uiua
# Experimental!
⋊beef [1 2 3 4 5 6]
```

Capital letters will [`un °`](https://uiua.org/docs/un) [`box ◻`](https://uiua.org/docs/box) the corresponding row.

```uiua
# Experimental!
⋊aCB {"Dave" 31 [38 22 5]}
```

Swizzles are experimental and may change in future versions as their place in the language is explored.

# The New Pad

Much of the code for the [Uiua website pad](https://uiua.org/pad) has been rewritten. This new pad uses less custom behavior and should work better in more browsers.

If you are reading this on the Uiua website, then all the examples above use this new pad!

# 💗

Thank you as always to everyone who uses Uiua and helps with its development! Your enthusiasm for the language gives me life.

A *special* thanks to all of [Uiua's sponsors](https://github.com/sponsors/uiua-lang) for their continued support 🥰

Again, you can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.11.0---2024-05-??).

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help.

