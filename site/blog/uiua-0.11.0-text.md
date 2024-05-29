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
