# Announcing Uiua 0.18.0

2026-??-??

---

Uiua 0.18.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.18.0---2026-??-??).

You can download pre-built binaries [here](https://github.com/uiua-lang/uiua/releases).

Uiua is a general-purpose array-oriented programming language with a focus on tacit code.

TODO: Summarize changes in a few sentences

TODO: Sections on major changes

## Notable Breaking Changes

Subscripted [`range ⇡`](https://uiua.org/docs/range) now creates an inclusive range starting at the subscript number. This breaks cases other than `⇡₁`.

```uiua
⇡₀ 5
⇡₁ 5
⇡₂ 5
⇡₋₁5
```

## Contributors

Many people contributed to this release in various ways, whether by proposing new features, discussing details, or directly PRing code.

I'd like to give special thanks to two direct contributors who did a lot for this release:

[ndren](https://github.com/ndren) implemented several [optimizations](https://github.com/uiua-lang/uiua/commits/main/?author=ndren) in the interpreter for improved performance in several primitives, as well as the compiler itself.

[lynn](https://github.com/lynn) did [work](https://github.com/uiua-lang/uiua/commits/main/?author=lynn) on the Uiua website. She did a lot to make its design a bit more modern, particularly the front page.

## Thank You!

There are lots of other small changes and improvements. You can find the full changelog for this release [here](https://uiua.org/docs/changelog#0.18.0---2026-??-??).

Thanks to Uiua's generous [GitHub sponsors](https://github.com/sponsors/uiua-lang)! You too could support Uiua's development and help pay my rent.

You can join the [Uiua Discord](https://discord.gg/3r9nrfYhCc) to chat about the language, ask questions, or get help. We also do code challenges and discuss language features!

