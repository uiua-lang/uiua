# Announcing Uiua 0.19.0

2026-08-??

---

**🥳 Uiua 0.19.0 is now available! ✨**

You can find the full changelog [here](https://uiua.org/docs/changelog#0.19.0---2026-08-??).

You can download pre-built binaries [here](https://github.com/uiua-lang/uiua/releases).

Uiua is a general-purpose array-oriented programming language with a focus on tacit code.

All the biggest features added in Uiua 0.19.0 are still `# Experimental!`, but there are also many smaller stable changes.

## A Note on the Purpose of `# Experimental!`

From its inception, Uiua has been a testing ground for tacit array programming language design concepts. This has since expanded to include trying out things that most languages do not provide out of the box, but which are nice to have at hand, well-integrated into the language. Currently stabilized instances of this include [`json`](https://uiua.org/docs/json), [`hsv`](https://uiua.org/docs/hsv), [`image`](https://uiua.org/docs/image)/[`audio`](https://uiua.org/docs/audio)/[`gif`](https://uiua.org/docs/gif) encoding, and one of my personal favorites, [`path`](https://uiua.org/docs/path). Most of these features exist baked into the language because they are either commonly used and simple enough to be a lightweight addition, or complex enough to implement on one's own that having a simple, fast version at hand makes the language much more powerful.

However, there are some other features that most languages do not include out of the box that are currently still `# Experimental!`. This is because they often push the limits of what a language should include out of the box. [`layout`](https://uiua.org/docs/layout) is really useful for text rendering, and is espeicially nice to have because images already have such good support, but does a *language* really need to include a font parser and layout engine? [`voxels`](https://uiua.org/docs/voxels) is great for multidimensional visualizations, but absent context, I think most people would say that the need to render voxel scenes should probably be the responsibility of a library.
