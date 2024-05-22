# Announcing Uiua 0.10.0

2024-04-04

---

Uiua 0.10.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.10.0---2024-04-04).

This release contains so many changes, improvements, and new features that I thought it deserved a blog post.
From here on, major releases will be announced in this way.

While there are many changes, I want to highlight a few of them here.

## Pattern Matching

Using [`un`](https://uiua.org/docs/un) on a constant value will now match a pattern. When used with [`try`](https://uiua.org/docs/try), this can be used to conditionally match, extract, and process values.

```uiua
F ← ⍣(
  ×10 °[1⊙3] # Extract and multiply..
| °(⊂5)      # ..or remove leading 5..
| ⇌          # ..else reverse
)
F [1 2 3]
F [5 6 7]
F "cool!"
```
You can read more in the [Pattern Matching](https://uiua.org/tutorial/patternmatching) tutorial.

## Array Macros

Array macros are a powerful new feature that allow full compile-time metaprogramming.

They allow Uiua code to directly manipulate other Uiua code, enabling a wide range of new possibilities.

```uiua
F! ←^ ≡$"_ ← _\n" "ABC"
F!(1|2|3)
[A B C B B]
```

You can read more in the updated [Macros](https://uiua.org/tutorial/macros) tutorial.

## Git Modules

You can now prefix a module path with `git:` to import a git repository from a URL.
```uiua
~ "git: github.com/uiua-lang/example-module" ~ Upscale
Upscale 3 [1_2 3_4]
```
In the native interpreter, this automatically creates a Git submodule.

On the web, it fetches a `lib.ua` file from the repository.

You can read more in the updated [Modules](https://uiua.org/tutorial/modules) tutorial.

## [`mask`](https://uiua.org/docs/mask)

[`mask`](https://uiua.org/docs/mask) is a new function that is similar to [`find`](https://uiua.org/docs/find), but it returns full masks of matches rather than just the first positions.

```uiua
⦷ " - " "Hey - how-are -  you"
```
```uiua
⊜□¬⦷⊙. " - " "Hey - how-are -  you"
```

This simplifies a lot of string-processing code in particular. A new [strings](https://uiua.org/tutorial/strings) tutorial has been added as well.

## Other Changes

Switch functions now format to use `⟨⟩` brackets. This makes them easier to distinguish from function packs.
```uiua
F ← (×10|↥2)<2. # This..
F ← ⟨×10|↥2⟩<2. # Formats to this
F 0
F 5
```

[`map`](https://uiua.org/docs/map) and related functions are no longer experimental! See the [`map`](https://uiua.org/docs/map) docs for an overview.
```uiua
map 1_2_3 4_5_6
```

The new [`&clget`](https://uiua.org/docs/&clget) and [`&clset`](https://uiua.org/docs/&clset) functions provide access to the clipboard.

The interpreter's built-in language server now supports [many more features](https://marketplace.visualstudio.com/items?itemName=uiua-lang.uiua-vscode).

There are a ton more! Again, you can read the full changelog [here](https://uiua.org/docs/changelog#0.10.0---2024-04-04).

## 💖

As always, I'd like to thank everyone who contributed to this release, whether by directly contributing code, reporting bugs, or just using Uiua and providing feedback.

Uiua is in many ways a novel and unique language, and I think it is only through our collective effort that we can properly explore its design space.

With your help, I hope to continue to improve Uiua to the point of stability.