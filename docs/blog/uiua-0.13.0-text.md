# Announcing Uiua 0.13.0

2024-10-??

---

Uiua 0.13.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.13.0---2024-10-??).

Uiua is a general purpose, stack-based, array-oriented programming language with a focus on tacit code.

This release, like most, is the biggest one yet! Here are some highlights:

## New Inverses

A new inversion modifier, [`anti ⌝`](https://uiua.org/docs/anti), has been added.

It captures certain useful inversion patterns. In general, it inverts a function as if its first argument were a constant.

```uiua
°(+1) 5
⌝+ 1 5
```

This enables some useful functionality.

```uiua
⌝↘ 3 [1 2 3]
⬚@.⌝⊏ 1_10_4_5 "abcd"
```

The new [`obverse ⌅`](https://uiua.org/docs/obverse) replaces, extends, unifies, and deprecates the existing `setinv` and `setund` modifiers.

It allows you set multiple kinds of inverses at once.

The [Inverses Tutorial](https://uiua.org/tutorial/inverses) has been updated to include [`anti ⌝`](https://uiua.org/docs/anti) and [`obverse ⌅`](https://uiua.org/docs/obverse).

## New Stack Manipulation Modifiers

The [`with ⤙`](https://uiua.org/docs/with) and [`below ◡`](https://uiua.org/docs/below) modifiers have been stabilized.

[`with ⤙`](https://uiua.org/docs/with) keeps its function's last argument on top of the stack.

```uiua
⊟⤙+1 5
⊂⤙⊡1 "hello"
```

[`below ◡`](https://uiua.org/docs/below) preserves a function's arguments below its outputs on the stack.

```uiua
[◡+] 1 2
```

```uiua
∩▽◡¬ ⊸◿2 [1 2 3 4 5]
```

## [`orient ⤸`](https://uiua.org/docs/orient)

[`orient ⤸`](https://uiua.org/docs/orient) has been stabilized. It reorders the axes of an array, rearranging the elements as necessary.

```uiua
°△3_3_2
{⊙∘} ⤸1 .
```

[`anti ⌝`](https://uiua.org/docs/anti)[`orient ⤸`](https://uiua.org/docs/orient) allows you to combine axes, which is equivalent to taking the diagonal along those axes.

```uiua
°△3_3
⌝⤸ 0_0 .
```

## [`assert ⍤`](https://uiua.org/docs/assert) Tests

Top-level [`assert ⍤`](https://uiua.org/docs/assert)ions are now interpreted as tests in some contexts. This includes in the website pad, or the `uiua watch` and `uiua test` commands, but *not* the `uiua run` command.

```uiua
⍤⤙≍ 3 +1 2
⍤⤙≍ [1 2 3] ⊂1 [2 3]
⍤⤙≍ "Hello" ⍜⊢⌵ "hello"
```

## Formatter Changes

Consecutive single-line bindings now have their `←`s aligned. Try formatting this example:

```uiua
F ← +1
Avg ← ÷⧻⟜/+
Re ← ◌°ℂ
```

Modules now use fancy delimiters. They format from the existing `---`s. Try it out!

```uiua
---MyModule
  F = +*10
---
```

## `# Experimental!`

This release added a lot of experimental features. Experimental features are not guaranteed to make it into the stable language, but they are added so they can be tried out.

Experimental features can be enabled by putting an `# Experimental!` comment at the top of a file.

[**Subscripts**](https://uiua.org/docs/experimental#subscripts) are an interesting way to augment the behavior of a function or modifier.

Subscript numbers may immediately follow a glyph. These can be typed with `__` followed by some digits.

```uiua
# Experimental!
[∩__3+ 1 2 3 4 5 6] # Try formatting!
```

```uiua
# Experimental!
√₃ 27
√₄ 625
```

```uiua
⁅₃ π
```

```uiua
# Experimental!
⊟₄ 1 2 3 4
□₃ "abc" 5 °△2_3
```

```uiua
# Experimental!
⍜(×10|-2) 5
⍜×₁₀-₂ 5
```

Subscript behavior is not defined in a general way. Each function or modifier may interpret a subscript differently.

All behaviors are specified [here](https://uiua.org/docs/experimental#subscript-modifiers).

[**Data Definitions**](https://uiua.org/docs/experimental#data-definitions) define a module that has a constructor and getters. The constructed object is just a normal array.

```uiua
# Experimental!
~Foo {Bar Baz}
Foo 1 "Hi"
Foo~Baz .
```

```uiua
# Experimental!
~Color [r g b a ← 1]
Color 0.5 1 0.2
Color!(+r⟜b) .
```

`enum`-like constructs are also possible. These automatically add tags to the array to disambiguate variants.

```uiua
# Experimental!
┌─╴Foo
  |Bar {A B}
  |Baz [x y z]
  |Qux 
└─╴
Foo~Bar "Neat" "Cool"
Foo~Baz 1 2 4
Foo~Qux
```

You can read about everything data definitions can do [here](https://uiua.org/docs/experimental#data-definitions).

# Contributors

No previous Uiua release has had so many direct code contributions.

In particular, I'd like to thank:
- Omnikar for implementing [`un °`](https://uiua.org/docs/un)[`by ⊸`](https://uiua.org/docs/by)
- amatgil for implementing the new behavior for [`gen`](https://uiua.org/docs/gen) and the experimental [`around ’`](https://uiua.org/docs/around) function
- Marcos-cat for implementing [`fill ⬚`](https://uiua.org/docs/fill)ed [`csv`](https://uiua.org/docs/csv), [`memberof ∈`](https://uiua.org/docs/memberof)[`range ⇡`](https://uiua.org/docs/range) optimization, and a more persistent pad virtual filesystem

Also, check out Omnikar's awesome [`uiua-plot`](https://github.com/omnikar/uiua-plot) library for making plots and graphs in Uiua!

# 💟

Thanks as always to everyone in the Uiua community, and to Uiua's generous [GitHub Sponsors](https://github.com/sponsors/uiua-lang)!

Again, you can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.12.0---2024-08-16).

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help. We also to code challenges and brainstorm language features!