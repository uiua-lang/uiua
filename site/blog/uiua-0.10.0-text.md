# Announcing Uiua 0.10.0

2024-04-??

Uiua 0.10.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog).

This release contains so many changes, improvements, and new features that I thought it deserved a blog post.
From here on, major releases will be announced in this way.

While there are many changes, I want to highlight a few of them here.

## Pattern Matching

Using [un](https://uiua.org/docs/un) on a constant value will now match a pattern. When used with [try](https://uiua.org/docs/try), this can be used to conditionally match, extract, and process values.

```uiua
F ← ⍣(×10°[1⊙3]|°(⊂5)|⇌)
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

## [mask](https://uiua.org/docs/mask)

[mask](https://uiua.org/docs/mask) is a new function that is similar to [find](https://uiua.org/docs/find), but it returns full masks of matches rather than just the first positions.

```uiua
⦷ " - " "Hey - how-are -  you"
```
```uiua
⊜□¬⦷⊙. " - " "Hey - how-are -  you"
```

## Other Changes

Switch functions now format to use `⟨⟩` brackets. This makes them easier to distinguish from function packs.
```uiua
F ← ⟨↥2|×10⟩<2.
F 0
F 5
```