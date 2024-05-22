# Ranges

Working with regular sequences of numbers is a common task in programming. Many languages handle this with with a `for` loop, but Uiua uses the same construct it uses for most things: arrays.

## [`range`]()

[`range`]() is the fundamental range-generating function. It generates an array that is the range of integers `[0, n)`.

```uiua
⇡5
⇡12
```

If you want to include the end value to get the range `[0, n]`, you can [`add`]()`1` first.

```uiua
⇡+1 5
⇡+1 12
```

[`add`]()`1` afterward to get the range `[1, n]`.

```uiua
+1⇡ 5
+1⇡ 12
```

The [`range`]() of a negative number gives the range `(0, n]`.
```uiua
⇡¯5
⇡¯12
```

## [`under`]() Tricks

[`under`]() makes getting more complex ranges simple.

For example, here is a simple way to get the range `[a, b)`:

```uiua
⍜-⇡ 3 10
```
The order of operations here is:
- Subtract `3` from `10` to get `7`
- Generate the range `[0, 7)`
- Add `3` back to that range to get `[3, 10)`

The definition of [`range`]() on negative numbers means that this idiom also works in reverse.

```uiua
⍜-⇡ 10 3
```

You can replace [`subtract`]() with [`divide`]() to use a step size.

```uiua
⍜÷⇡ 0.5 4
```

This only works if the numbers are divisible. If they are not, you need to use [`floor`](), [`ceiling`](), or [`round`]().

```uiua
⍜÷(⇡⌊) 3 10
⍜÷(⇡⌈) 3 10
⍜÷(⇡⁅) 3 10
```

These techniques can be combined to get more complex ranges:

```uiua
⍜(÷⊙-|⇡⌈) 3 10 20
```

## [`un`]() Ranges

It is often necessary to generate a range that has as many elements as an array has rows.

The intuitive way to do this is with [`length`]().

```uiua
⇡⧻. "Hello!"
```

This is a very common operation. [`un`]() [`select`]() is defined as a shortcut for this.
[`select`]() takes a list of indices, so [`un`]() [`select`]() *returns* a list of indices.

```uiua
°⊏ "Hello!"
```

This same pattern is used for the [`un`]()-inverses of [`pick`]() and [`orient`]().

```uiua
A ← ["Hello" "World"]
≍ ⊃(⇡⧻|⊙◌°⊏) A  # Range of length
≍ ⊃(⇡△|⊙◌°⊡) A  # Range of shape
≍ ⊃(⇡⧻△|⊙◌°⤸) A # Range of rank
```

It is simple to verify that these inverses are correct.

```uiua
A ← ["Hello" "World"]
⊏°⊏ A
⊡°⊡ A
⤸°⤸ A
```
