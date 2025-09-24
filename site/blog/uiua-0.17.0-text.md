# Announcing Uiua 0.17.0

2025-09-23

---

Uiua 0.17.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.17.0---2025-09-23).

You can download pre-built binaries [here](https://github.com/uiua-lang/uiua/releases).

Uiua is a general purpose array-oriented programming language with a focus on tacit code.

This release is mostly focused on bug and crash fixes; there are no major new features.

That said, there are still some notable smaller changes and additions in this release:

## New Subscript Formatting

Subscripts now format from a `,` rather than `__` like they did previously.

```uiua
coup,3 2 3 5 # Try formatting
```

This makes them typable with one keystroke instead of three, which is nice as more things in the language use subscripts.

## Primes in Binding Names

Binding names can now contain Unicode prime glyphs. These format from `'`s at the end of the name.

Try formatting this example:

```uiua
A' = 1
B'' = 2
C''' = 3
```

## Array Pack Syntax

It is common to wrap the results of a [`fork ⊃`](https://uiua.org/docs/fork) or [`bracket ⊓`](https://uiua.org/docs/bracket) function pack in an array to collect the results.

```uiua
[⊃(⊢|⊣|/+)] [3 8 2 9 5]
{⊓(⍉|+|⇡)} [1_2 3_4] 6 10_20 5
```

The parentheses of the function pack can be replaced with the array brackets.

```uiua
⊃[⊢|⊣|/+] [3 8 2 9 5]
⊓{⍉|+|⇡} [1_2 3_4] 6 10_20 5
```

## [`indexin ⨂`](https://uiua.org/docs/indexin)

[`indexof ⊗`](https://uiua.org/docs/indexof) is now deprecated. It was almost always used along with [`backward ˜`](https://uiua.org/docs/backward), and this change also makes its argument order convention match that of [`memberof ∊`](https://uiua.org/docs/memberof).

It has been replaced with [`indexin ⨂`](https://uiua.org/docs/indexin), which has the exact same functionality, except its arguments are flipped.

```uiua
F ← ⨂"abcde"
F "beefcake"
```

[`indexof ⊗`](https://uiua.org/docs/indexof) will be automatically replaced in existing code.

## Stabilized Primitives

- [`occurrences ⧆`](https://uiua.org/docs/occurrences)
    ```uiua
    ⧆ "lego helmet"
    ⧆ "abbacaab"
    ⧆₁ "abbacaab"
    ```
  - Replaces and deprecates [`unique ◰`](https://uiua.org/docs/unique)
  - [`unique ◰`](https://uiua.org/docs/unique) will be automatically replaced
- [`self ˙`](https://uiua.org/docs/self)
  ```uiua
  ˙× 5
  ˙⊟ 10
  ˙⊟₃ τ
  ```

## Notable Breaking Changes

Subscripted [`join ⊂`](https://uiua.org/docs/join) now joins that many arrays.

```uiua
⊂₃ "wow" @  "cool"
⊂₄ 1 2_3 4_5_6 7
/◇⊂₃ @, {"dog" "cat" "bird"}
```

[`dip ⊙`](https://uiua.org/docs/dip) and [`on ⟜`](https://uiua.org/docs/on) function packs no longer apply their modifier to the leftmost function. This makes certain patterns easier to read and write.

```uiua
⊙(⊂|÷|+) 1 2 3 5
⊂⊙(÷⊙+) 1 2 3 5
```

[`under ⍜`](https://uiua.org/docs/under)[`shape △`](https://uiua.org/docs/shape) now tiles the original array to match the new shape instead of [`reshape ↯`](https://uiua.org/docs/reshape)ing.

```uiua
⍜⊙△⊂ 2_3 ["ab" "cd"]
```

[`reverse ⇌`](https://uiua.org/docs/reverse), [`transpose ⍉`](https://uiua.org/docs/transpose), and [`rotate ↻`](https://uiua.org/docs/rotate) no longer work through boxes.

## Thank You!

There are lots of other small changes and improvements. You can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.17.0---2025-09-23).

Thanks to Uiua's generous [GitHub sponsors](https://github.com/sponsors/uiua-lang), who help pay my rent!

Extra special thanks to everyone who directly contributed code to this release, including [magistau](https://github.com/magistau), [Marcos-cat](https://github.com/Marcos-cat), [Omnikar](https://github.com/Omnikar), [amatgil](https://github.com/amatgil), [Jacob Lockwood](https://github.com/Jacob-Lockwood), [lemonlime4](https://github.com/lemonlime4), [Madeline Vergani](https://github.com/RubenVerg), and [alex-s168](https://github.com/alex-s168)! Your care for fixing bugs and docs, and for making quality-of-life improvements, are what makes the Uiua community great!

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help. We also do code challenges and discuss language features!

```uiua
°⍉ -⊸¬÷¤¤⟜⇡˙⊟100              # Coords
≡⌟× +1 ×0.1 ∿×τ÷⟜⇡20          # Scale
≡(⊃⊢(⊃+-⊃(√˜-0.5˙×|ⁿ2/3÷2⌵)⊣) # Heart
  ⇌×⊓⌞>< ×1.1+0.1)
⊏⊙Black_Red # Color
```
