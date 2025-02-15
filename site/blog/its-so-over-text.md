# It's So [`over ,`](https://uiua.org/docs/over)

2025-02-15

---

In the coming Uiua version 0.15.0, the [`over ,`](https://uiua.org/docs/over) function will be deprecated. This change is already live on the website and in new builds.

[`over ,`](https://uiua.org/docs/over) copied the second-to-top value on the stack to the top of the stack. This is a pretty common primitive in many stack languages, so its removal may seem like an odd choice.

This change is part of Uiua's general shift toward more structured forms of stack management. The last year has seen many [stack manipulation modifiers](https://uiua.org/tutorial/evenmorestack) added to the language. In particular, basically all uses of [`over ,`](https://uiua.org/docs/over) can be replaced with [`with ⤙`](https://uiua.org/docs/with) or [`below ◡`](https://uiua.org/docs/below).

Here are some examples of how to replace common uses of [`over ,`](https://uiua.org/docs/over):

The shortest literal replacement for [`over ,`](https://uiua.org/docs/over) is [`with ⤙`](https://uiua.org/docs/with) [`identity ∘`](https://uiua.org/docs/identity), though it is generally not preferred.

```uiua
[⤙∘ 1 2]
```

Consider that this particular example could be written without [`identity ∘`](https://uiua.org/docs/identity) as well. This is because of [`with ⤙`](https://uiua.org/docs/with)'s behavior on noadic functions.

```uiua
[⤙1 2]
```
The common pattern of `,,` preserved a dyadic function's arguments on the stack. This can be replaced with [`below ◡`](https://uiua.org/docs/below).

```uiua
[◡+ 2 3]
{◡⊟ 1_2_3 4_5_6}
```

Another common pattern is needing to call a monadic function on the second value on the stack, preserving that value and putting the result on top. This can be done with [`below ◡`](https://uiua.org/docs/below) [`gap ⋅`](https://uiua.org/docs/gap).

```uiua
◡⋅△ 1_2_3 [4_5 6_7]
```

Finally, [`over ,`](https://uiua.org/docs/over) was sometimes used to intersperse a value between constants. This can now be done with [`with ⤙`](https://uiua.org/docs/with).

```uiua
2⤙4 [1 2 3 4 5]
```

```uiua
×⊓≥< 2⤙4 [1 2 3 4 5]
```

It's reasonable to ask, with the recent stabilization of [`backward ˜`](https://uiua.org/docs/backward), and the addition of the `# Experimental!` [`self ˙`](https://uiua.org/docs/self) modifier, is deprecation on the horizon for [`flip :`](https://uiua.org/docs/flip) and [`duplicate .`](https://uiua.org/docs/duplicate)? The short answer is no, not at the moment. [`flip :`](https://uiua.org/docs/flip) and [`duplicate .`](https://uiua.org/docs/duplicate) are a bit more fundamental than [`over ,`](https://uiua.org/docs/over) was, and they allow for certain nice notations.

That's all! [`over ,`](https://uiua.org/docs/over) was one of the oldest primitives in Uiua, but part of the language has always been about exploring the best ways to write tacit code. [`over ,`](https://uiua.org/docs/over) can make the stack harder to reason about, as it requires keeping the state of the stack at various points in your head. We have since discovered better ways to do the same thing, and so we say goodbye to [`over ,`](https://uiua.org/docs/over). 🫡