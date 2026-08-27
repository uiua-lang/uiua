# Announcing Uiua 0.19.0

2026-08-26

---

**🥳 Uiua 0.19.0 is now available! ✨**

You can find the full changelog [here](https://uiua.org/docs/changelog#0.19.0---2026-08-26).

You can download pre-built binaries [here](https://github.com/uiua-lang/uiua/releases).

Uiua is a general-purpose array-oriented programming language with a focus on tacit code.

All the biggest features added in Uiua 0.19.0 are still `# Experimental!`, but there are also many smaller stable changes.

## Notable Small Stable Features

### New Module Access Syntax

Accessing module items now uses `.` rather than `~`. This is easier to type and nicer to read. Imports and data definitions still use `~`. [Chained field access](<https://uiua.org/tutorial/Data Definitions#chained-access>) now uses `‥` instead of `≈`.

```uiua
~Date {Year Month Day}
~Stats {Str ← 10|Dex ← 10|Con ← 10}
~Person {Name Dob ← Date|Stats ← Stats}
Person "Dan" 1396 2 29
⊃(Person.Name
| Date.Year Person.Dob
| Person.Stats‥Dex)
```

### All Trig Functions

[`sine ∿`](https://uiua.org/docs/sine) and [`atangent ∠`](https://uiua.org/docs/atangent) have been in the language since the beginning, but all trigonometric and hyperbolic functions are now present as glyphless primitive `monadic` functions. They also have properly implemented inverses, and work on both real and [`complex ℂ`](https://uiua.org/docs/complex) numbers.

The new functions are [`cos`](https://uiua.org/docs/cos), [`tan`](https://uiua.org/docs/tan), [`sinh`](https://uiua.org/docs/sinh), [`cosh`](https://uiua.org/docs/cosh), and [`tanh`](https://uiua.org/docs/tanh).

### Power Set [`tuples ⧅`](https://uiua.org/docs/tuples)

This is a **breaking change** for `monadic` [`tuples ⧅`](https://uiua.org/docs/tuples). It now gives the power set rather than prefixes. Sided subscripts give prefixes or suffixes.

```uiua
⧅□  ⇡3
⧅⌞□ ⇡4
⧅⌟□ ⇡4
```

### [`rise ⍏`](https://uiua.org/docs/rise) and [`fall ⍖`](https://uiua.org/docs/fall) Subscripts

Numeric subscripts for [`rise ⍏`](https://uiua.org/docs/rise) and [`fall ⍖`](https://uiua.org/docs/fall) give a list of multi-dimensional indices that would sort each rank-n subarray if indexed with [`pick ⊡`](https://uiua.org/docs/pick).

```uiua
[3_1_5 2_0_4]
◡⊡⊸⍏₀
```

This makes it easy to work with multidimensional ordered data.

## A Note on the Purpose of `# Experimental!`

From its inception, Uiua has been a testing ground for tacit array programming language design concepts. This has since expanded to include trying out things that most languages do not provide out of the box, but which are nice to have at hand, well-integrated into the language. Currently stabilized instances of this include [`json`](https://uiua.org/docs/json), [`hsv`](https://uiua.org/docs/hsv), [`image`](https://uiua.org/docs/image)/[`audio`](https://uiua.org/docs/audio)/[`gif`](https://uiua.org/docs/gif) encoding, and one of my personal favorites, [`path`](https://uiua.org/docs/path). Most of these features exist baked into the language because they are either commonly used and simple enough to be a lightweight addition, or complex enough to implement on one's own that having a simple, fast version at hand makes the language much more powerful.

However, there are some other features that most languages do not include out of the box that are currently still `# Experimental!`. This is because they often push the limits of what a language should include out of the box. [`layout`](https://uiua.org/docs/layout) is really useful for text rendering, and is especially nice to have because images already have such good support, but does a *language* really need to include a font parser and layout engine? [`voxels`](https://uiua.org/docs/voxels) is great for multidimensional visualizations, but absent context, I think most people would say that rendering voxel scenes should probably be the responsibility of a library. I will call these types of features *boundary* features.

This boundary-pushing is not the case for all `# Experimental!` features. Some are more Uiua-specific, like [`reach ∪`](https://uiua.org/docs/reach) or [`pattern ⍡`](https://uiua.org/docs/reach). Whether they are stabilized is less a question of what belongs in a language and more a question of how Uiua should work. I will call these types of features *Uiua-specific* features.

Some other `# Experimental!` features are essentially certain to be eventually stabilized, with the only thing delaying that being whether their API and behavior is the best it can be. These are things like [`recur`](https://uiua.org/docs/recur) and [`&ffi`](https://uiua.org/docs/&ffi) (and related memory functions). I will call these types of features *necessary* features.

With all this said, Uiua 0.19.0 has three big experimental features. One is a boundary feature, one is a Uiua-specific feature, and the third is maybe somewhere in between.

## Custom Subscripts

Since their initial addition in version 0.13.0 (almost 2 years ago, how time flies), [subscripts](https://uiua.org/docs/subscripts) have become a key way in which Uiua gains access to new built-in behavior without adding a ton of new glyphs.

However, it was not previously possible to define subscript behavior for your own functions without simply adding a version of the function with each possible subscript value.

Now, with [Custom Subscript Functions](https://www.uiua.org/docs/experimental#custom-subscript-functions), you can! By the classification scheme detailed above, this is a *Uiua-specific* feature.

By defining a function with a `ₙ` (which formats from `,n`) at the end of its name, you can define subscript behavior. This functions similarly to existing [index macros](https://www.uiua.org/tutorial/Macros#placeholders-and-!s). You can either use a `ₙ` in the subscript of inner functions, or you can use `^n` to get the value of the subscript number directly.

```uiua
# Experimental!
Fₙ ← [∩ₙ+]
F₁ 1 2
F₂ 1 2 3 4
```

You can also define a normal version of the function for flexibility.

```uiua
# Experimental!
G  ← ↯2
Gₙ ← ↯2_^n
G 4
G₂ 5
G₃ 0
```

Custom subscripts are currently limited to numeric subscripts. Sided subscripts are not currently supported.

## A Type System

Uiua 0.19.0 adds an `# Experimental!` type system! I've already detailed its design philosphy and functionality in [this blog post](https://www.uiua.org/blog/a-uiua-type-system), so I won't go into a ton of detail here.

For the basics, you can read the documentation for [`validate ⊨`](https://uiua.org/docs/validate) and [type signature comments](https://www.uiua.org/docs/experimental#type-checking).

This is somewhere between the three kinds of `# Experimental!` features detailed above. Many languages have static type systems, and many do not. This new system is meant to see whether a type system is appropriate for Uiua.

## Multivector Scalars

Uiua 0.19.0 includes a complete rewrite and overhaul of the `# Experimental!` [Geometric Algebra](https://en.wikipedia.org/wiki/Geometric_algebra) system by adding a multivector scalar type for arrays. Multivectors are algebraic objects which are useful for doing geoemtric operations in multiple dimensions. This includes things like rotations, intersections, and projections. Multivectors can be thought of as a sort of superset of complex numbers, but they are also much more.

Multivector arrays can be created with the [`multivector 𝕍`](https://uiua.org/docs/multivector) functions. There is also a full Uiua [Geometric Algebra tutorial](https://www.uiua.org/docs/experimental#geometric-algebra). It's end result is this gif of a rotating tesseract!

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 12            # Edge points
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)3    # Cube points
ₑ×¯e₁₂÷2×η÷⟜⇡20        # Rotors
⊞(×⊃¯⌟˜×)              # Rotate points
¯₄+e₀                  # Convert to PGA
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 0 0 0] # Lines from points to camera
⨱ +2e₀𝕍[1 0 0 0]       # Project to frustum space
¯₄+e₀𝕍 ↘0_0_2°𝕍¯₄±     # Shift to 3D
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 ¯3 ¯2] # Lines from points to camera
⨱ +2e₀𝕍[1 0 0]         # Project to frustum plane
↘0_0_2 °𝕍¯₄±           # Convert back to numbers
⬚0≡(⍉°⊚) ⁅×100 ⧋-/↧⊸♭₂ # Render
```

Multivectors are very much a *boundary* feature,

## 🙏🏻 Thank You! 

You can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.19.0---2026-08-26).

This is the longest it has even been between Uiua releases. Thanks to everyone in the Uiua community for being patient! And of course a huge thank you as always to Uiua's generous [sponsors](https://github.com/sponsors/uiua-lang) ❤️!

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help. We also do code challenges and discuss language features!
