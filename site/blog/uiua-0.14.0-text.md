# Announcing Uiua 0.14.0

2024-12-??

---

Uiua 0.14.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.14.0---2024-12-??).

Uiua is a general purpose, stack-based, array-oriented programming language with a focus on tacit code.

Here are some highlights:

## Subscripts

*Subscripts* are a new syntactic feature that allow for shorter code as well as some new behavior.

You type subscripts with a `__` followed by some digits. The formatter will turn it into subscript characters.

```uiua
undertake__3*__10 [1 2 3 4 5] # Try formatting!
```

There is an entire [blog post](https://www.uiua.org/blog/subscripts) about the addition of subscripts. You can see all currently implemented subscript-compatible functions [here](https://www.uiua.org/docs/subscripts).