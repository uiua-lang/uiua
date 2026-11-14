# Announcing Uiua 0.16.0

2025-05-18

---

Uiua 0.16.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.16.0---2025-05-18).

You can download pre-built binaries [here](https://github.com/uiua-lang/uiua/releases).

Uiua is a general purpose array-oriented programming language with a focus on tacit code.

Here are some highlights of this release:

## New Pretty Array Formatter

The formatter that displays arrays in output has been overhauled. High-rank arrays now take up less vertical space and more horizontal space. Box arrays now use box-drawing characters.

```uiua
°△2_2_3_4
```

```uiua
◫□ 2_3 °△3_5
```

You can read more about how axes are laid out [here](https://www.uiua.org/tutorial/Arrays#output). You can also gaze upon the [*G R I D*](https://www.uiua.org/pad?src=0_16_0-rc_1__4omh4oKB4pahwrDilrMyXzJfMl8yXzJfMl8yXzJfMQo=).

## [`repeat ⍥`](https://uiua.org/docs/repeat) Collection

⚠️ *Breaking Change* ⚠️

[`repeat ⍥`](https://uiua.org/docs/repeat) (and [`do ⍢`](https://uiua.org/docs/do)) now *always* have static signatures. If their function returns more values than it takes, the values lower on the stack will be automatically collected into arrays.

This makes it much easy to use [`repeat ⍥`](https://uiua.org/docs/repeat) to collect intermediate values.

Here is a simple example that generates the fibonacci sequence.

Old version:
```not uiua
⇌[⍥◡+9]∩1
```
New version:
```uiua
⍥◡+9 ∩1
```

As you can see, the fix for this is generally to remove wrapping `[]`s and potentially a `⇌`.

## Sided [`rows ≡`](https://uiua.org/docs/rows) 

Sided subscripts for [`rows ≡`](https://uiua.org/docs/rows) have been stabilized.

As a reminder, sided subscripts are non-numeric subscripts that indicate a function or modifier should be augmented in a way that has a "side".

Sided [`rows ≡`](https://uiua.org/docs/rows) rows makes one of the arguments repeat in each iteration instead of being iterated.

```uiua
≡⊂ 1_2_3 4_5_6
```
```uiua
≡⌞⊂ 1_2_3 4_5_6 # Left argument is repeated
```
```uiua
≡⌟⊂ 1_2_3 4_5_6 # Right argument is repeated
```

## Mixed Subscripts

Subscripts can now be both numeric and sided.

One useful example is with [`both ∩`](https://uiua.org/docs/both). We can use mixed subscripts to both reuse a value and operate on more than 2 sets of values.

```uiua
{∩₃⌞⊟} 1 2 3 4
```

You can even add another number after the side to reuse more than one value.

```uiua
{∩₃⌞₂⊟₃} 1 2 3 4 5
```

You can read more about mixed subscripts [here](https://www.uiua.org/docs/subscripts#mixed).

## Inline Macros

Previously experimental inline macros have been stabilized.

Inline macros are macros that do not need to be given a name. They are defined and executed in the same place in the code.

This example calculates the standard deviation and reuses the code that calculates the mean.

```uiua
StdDev ← √(^0 ˙× -⊸^0)!(÷⊃⧻/+)
StdDev [1 2 3 4]
```

Here we use an inline code macro to output the signature of a function.

```uiua
(⋅⊢)^!(⊃(+|-×))
```

You can read more about inline macros [here](https://www.uiua.org/tutorial/Macros#inline-macros).

## Other Notable Breaking Changes

- [`rows ≡`](https://uiua.org/docs/rows)'s numeric subscript behavior has been replaced with that of [`each ∵`](https://uiua.org/docs/each), and [`each ∵`](https://uiua.org/docs/each) has been deprecated. 🫡

  [`each ∵`](https://uiua.org/docs/each)'s behavior can now be achieved with `≡₀`.

  ```uiua
  ≡₀(□⇡) [1_2 3_4]
  ```
- Number literals have been overhauled, including adding complex literals.
  ```uiua
  ≡¤ [2π/3 3r5i 4.1i 22/η]
  ```
- [`fft`](https://uiua.org/docs/fft) now works along every axis of an array rather than only the last. It is now stabilized!

## Thank You!

As always, thank you to everyone in the Uiua community, and to Uiua's lovely [GitHub sponsors](https://github.com/sponsors/uiua-lang).

Again, you can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.16.0---2025-05-18).

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help. We also do code challenges and discuss language features!

```uiua
# Experimental!
×1.3 -⊸¬ ÷⟜(°⍉⇡)↯3 50 # Coords
-+⊃(
  ×9/200 /×ⁿ0_2_3
| /×ⁿ2_0_3
| ⁿ3 -1 /+×[1 9/4 1] °√
)                     # Heart
⍉⊞× [1 0.3 0.3] ⤸2 ≤0 # Threshold/color
×τ ÷⟜⇡24              # Rotation angles
≡⌟voxels!(°⊸Fog Black °⊸Camera [0.2 °∠])
```
