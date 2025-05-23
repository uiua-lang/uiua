# Subscripts

2024-11-25

---

If you get into any level of mathematics above a middle school level, you're likely to encounter notation that involves *subscripts*: little numbers or letters that sit across the baseline of the text and indicate different things about the thing to their left.

![Matrix multiplication with subscript notation (invert)](https://wikimedia.org/api/rest_v1/media/math/render/svg/ee372c649dea0a05bf1ace77c9d6faf051d9cc8d)

One common use of subscripts is to indicate indices. In many (perhaps the majoriy of?) programming languages, the subscript index notation of mathematics is replaced with the familiar `[]` square bracket syntax. Even APL, of which Uiua is a descendent, uses square brackets for this purpose. This is a nice, uniform syntax that is - critically - easy to type on an ASCII keyboard. But it loses a bit of the art, beauty, and expressiveness of mathematical notation.

## Subscripts in Uiua

Uiua's [formatter](https://www.uiua.org/tutorial/basic#formatting) and its embrace of Unicode glyphs free it from the constraints many other languages have. This makes it possible to explore powerful and/or aesthetic syntactic constructs that would either be impossible or cumbersome to express in other languages. Look no further than the [fancy module delimiters](https://www.uiua.org/tutorial/modules#scoped-modules).

Uiua has actually allowed subscript numbers in identifiers for a while. Since identifiers cannot contain regular digits, this allows you to put numbers in them anyway.

```uiua
Md₅    ← ∘ # TODO
Sha₂₅₆ ← ∘ # TODO
```

Subscripts for use beyond identifiers were originally proposed in the [Uiua discord](https://discord.gg/3r9nrfYhCc) as almost a joke. A whimsical syntax for modifying the behavior of certain functions and modifiers. But people got to talking about what could be possible, and I implemented subscripts as an experimental feature. At time of writing, subscripts have just been stabilized.

Uiua subscripts are written with a sing quote `'` followed by some digits. They can also be negative. The formatter will convert this into nice unicode subscript digits.

```uiua
# Try formatting!
+'1 5
×₂ 12
```

But what are they for? In the example above, they are equivalent to just not using them at all. For mathematical operators, they are only really good for reducing the number of parentheses needed. This is valuable in its own right for readability, but it is not reason enough to add an entire syntax for it.

However, subscripts on some functions and modifiers allow you to express things that would otherwise be impossible.

## A brief history of Uiua's rank functionality

Uiua has gone through a few iterations of ways to express operating at a certain *rank* of an array. The other array languages simply have a `rank` operator that allows the direct specification of the rank to operate at. This approach works and is very general. At one point, Uiua had something similar in a modifier called `≑ level`. This took a number or list of numbers indicating the ranks to operate at, as well as the function to operate on the array. While this worked, something about it never sat right with me. In an array language, the structure and rank of an array are, in most cases, the structure of the computation itself. There's something odd about having some numbers in your code that refer to actual *numbers*, and then other numbers that refer to the computation.

The first attempt to alleviate this discomfort I felt was the infamous *Ocean Notation*. It was a series of glyphs that had special parsing rules but whose function was *only* to create rank lists for use with level `≑ level`. This mostly eliminated rank-indicating numbers from the code. While this system was kind of neat, it added too many new symbols for the programmer to learn with very little payoff, and it was not general enough to handle all cases.

`≑ level` was eventually replaced with Uiua's current system involving [`≡ rows`](https://uiua.org/docs/rows) and [`¤ fix`](https://uiua.org/docs/fix). This system is simple, composable, and easy to learn. However, it cannot handle a common use of `rank` in other array languages: how do you operate on rank-N subarrays of an array of arbitrary rank? To fill this hole, the `☇ rerank` function was added. This set the rank of the *rows* of an array to the given number. This also worked, and the system was complete.

But there were those numbers in the code again. You'd most often write `☇1` or `☇2` to change the rank of an array, collapsing the leading dimensions, sometimes temporarily. The thing is, you *never* need this number to be dynamic. It is *always* a number sitting there in the code itself, a static value, known at compile time. Sometimes it had to be relative to the rank of the array, in which case you would use a static negative number, but a static value nontheless.

## Some uses of subscripts

As subscripts were experimented with, we realized that they could replace all uses of `☇ rerank`.

Want to collapse an array to a certain rank? That's subscripted [`♭ deshape`](https://uiua.org/docs/deshape).

```uiua
⍉ ♭₂ ⇡2_2_3
```

Want to call a function on all rank-N subarrays of an array? That's subscripted [`≡ rows`](https://uiua.org/docs/rows).

```uiua
≡₁□ °△2_3_4
≡₂□ °△2_3_4
```

But there's more than just messing with rank!

What do you do when you want to collect some number of values from the stack into an array? The previous direction was to use `[]`s or `{}`s with [`⊙ dip`](https://uiua.org/docs/dip) and [`∘ identity`](https://uiua.org/docs/identity).

```uiua
[⊙⊙⊙∘] 1 2 3 4
{⊙⊙∘} 5 "Hi!" 1_2_3
```

But this is unnecessarily verbose. This can now be done with subscripted [`⊟ couple`](https://uiua.org/docs/couple) or [`□ box`](https://uiua.org/docs/box)!

```uiua
⊟₄ 1 2 3 4
□₃ 5 "Hi!" 1_2_3
```

How do you take the Nth root of a number? Previously, you'd have to raise to the power of the reciprocal. Now you can just use subscripted [`√ sqrt`](https://uiua.org/docs/sqrt).

```uiua
ⁿ÷:1 3 125
√₃ 125
```

Something similarly useful happens with [`⁅ round`](https://uiua.org/docs/round).

```uiua
⍜×⁅ 1e3 π
⁅₃ π
```

Subscripts also solve the infamous problem of calling [`∩ both`](https://uiua.org/docs/both) on 3 sets of arguments.

```uiua
[∩₃+ 1 2 3 4 5 6]
```

This is all to not even mention all the ways that subscripts simply help avoid parentheses, which reduces line noise and makes code easier to read.

You can find a full list of all the currently implemented subscripts [here](https://uiua.org/docs/subscripts).

## Going forward

One thing not listed above, and which is certainly more subjective, is that subscripts are *pretty*. They evoke the beauty of mathematical notation, a little number that you write to augment meaning. They make me smile! 😊

Much more is possible as well. One likely future use of subscripts is as a way to indicate non-base-10 numeric literals. Also, allowing for non-numeric subscripts would open up a whole new avenue of exploration.

I hope you enjoy this new feature. It is available in the [online pad](https://uiua.org/pad) and in the latest release of the [native interpreter](https://github.com/uiua-lang/uiua/releases).