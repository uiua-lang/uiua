# Announcing Uiua 0.12.0

2024-08-16

---

Uiua 0.12.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.12.0---2024-08-16).

Uiua is a general purpose, stack-based, array-oriented programming language with a focus on tacit code.

This is a pretty big release! In addition to stable features, it contains a lot of experimental features to try out.

Here are some of the highlights:

## New Tutorials

There are two new tutorials on the site:
- [Tacit Code](https://uiua.org/tutorial/tacitcode)
- [Code Tactility](https://uiua.org/tutorial/codetactility)

## Scoped Modules

Modules can now be declared without needing a new file.

This is done with `---`s and a name. 
A `~` following the name lets you export names from within into the outer scope.

```uiua
---MyMod ~ Go
  Foo ← 5
  Go ← +1
---
Go MyMod~Foo
```

A module containing a function called `Call` or `New` can be called as a function.

```uiua
---Foo
  Call ← /++1⇡
---
Foo 5
```

Using a module name as a macro (with a `!` at the end of the name) will make the module's names available inside that scope.

```uiua
---Foo
  A ← 10
  F ← +1
  G ← ×2
---
Foo!(G F ×A) [1 2 3]
```

## [`switch ⨬`]()

Dedicated switch function syntax has been replaced with the [`switch ⨬`]() modifier.

In addition, it has been expanded to do an implicit [`rows ≡`]().

```uiua
⨬(∘|+1|×2) [0 1 2] 5
```

Existing `⟨⟩`s will continue to parse and will format to [`switch ⨬`]() with a function pack.

## Subscript digits in identifiers

Unlike most programming languages, Uiua identifiers cannot contain digits.

But sometimes you want digits in your names! You can now use *subscript* digits in identifiers.

These format from a double underscore `__` followed by some digits.

```uiua
Sha__256 ← "todo" # This
Sha₂₅₆ ← "todo"   # Formats to this
```

## New Primitive Functionality

Several primitive functions have new functionality

[`un °`]() [`shape △`]() now generates a [`range ⇡`]() array with the given shape.

```uiua
°△ 2_3_4
```

[`couple ⊟`]() and [`join ⊂`]() are now more permissive of arguments with different ranks. The array with a smaller rank will be repeated.

```uiua
⊟ 1_2_3 4
```
```uiua
⊂ [1_2_3 4_5_6] 7
```

[`keep ▽`]() will now cycle the counts array.

```uiua
▽ 0_1_2 [1 2 3 4 5 6]
```

[`keep ▽`]() also now allows a scalar non-integer to scale an array. This is useful for image and audio arrays.

```uiua
▽ 0.5 [1 2 3 4 5 6]
▽ 1.5 [1 2 3 4 5 6]
```
## [`memberof ∈`]()

[`member ∊`]() is now deprecated. It was almost always used along with [`flip :`]().

It has been replaced with [`memberof ∈`](), which has the exact same functionality, except its arguments are flipped.

```uiua
F ← ∈"abc"
F "beefcake"
```

This makes it work nicely with [`by ⊸`]()!

```uiua
⊜□¬⊸∈ " ," "To be, or not"
```

## Experimental Features

This release adds a *lot* of experimental features to try out.

While it's unlikely that all of these will be eventually stabilized, they are made available for you to try out and see how they feel.

You can view to full list of experimental features [here](https://uiua.org/docs/experimental), but here are a few highlights:

### More Stack Modifiers

The [`but ⤙`]() and [`with ⤚`]() modifiers are complements to [`on ⟜`]() and [`by ⊸`]().

[`but ⤙`]() keeps its function's *last* argument on *top* of the stack while [`with ⤚`]() keeps its function's *first* argument *below* the outputs on the stack.

```uiua
# Experimental!
[⤙+ 2 5]
[⤚+ 2 5]
```

The [`above ◠`]() and [`below ◡`]() modifiers keep *all* of a function's arguments above or below the outputs on the stack.

```uiua
# Experimental!
[◠(++) 1 2 3]
[◡(++) 1 2 3]
```

[`chunks ⑄`]() is similar to [`windows ◫`]() except the parts of the array do not overlap.

```uiua
# Experimental!
⑄ 2_3 °△ 4_9
≡≡□
```

[`orient ⤸`]() transposes an array's axes by moving the axes at the given indices to the front of the [`shape △`]().

This simplifies complex shape transformations that would otherwise be done with several [`transpose ⍉`]()s and [`rows ≡`]()s.

```uiua
# Experimental!
°△ 2_3_4_5
△ ⤸ 1_3
```

## 💖

As always, a heartfelt thank-you to everyone in the Uiua community! Your contributions are what make Uiua great.

If you want to support Uiua's development, you can become one of its excellent [sponsors](https://github.com/sponsors/uiua-lang)!

Again, you can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.12.0---2024-08-16).

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help.

## Media Constants

A final fun note!

A few built-in image and audio constants have been added. These are useful for testing and demonstrating image and audio functions!

```uiua
Logo
Lena
▽⟜≡▽ 0.5 # Scales the image down
Music
```
