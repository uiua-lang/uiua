# Design

This page explains the reasons for some of Uiua's design decisions.  
It serves as a [defense of design](https://news.knowledia.com/US/en/articles/more-software-projects-need-defenses-of-design-85ea9e23ffd85f5fde5a2d3d42001393cbce169a).

## Stack Basing

### Combinators

When I first started developing Uiua, it was neither stack-based nor array-oriented. What it *did* focus a lot on was *combinators*. I had this whole hierarchy of language-level operators that let you construct arbitrarily complex combinators relatively succinctly.  
I discovered what a lot of others have discovered when delving deep into tacit code: it's really hard to read and write and reason about.

Eventually, I moved to a stack-based model and discovered that you can write almost any 1 or 2 argument combinator with just [`duplicate`](/docs/duplicate), `, over`, and [`flip`](/docs/flip).  
Of course, I also made the discovery that juggling 3 or more values on the stack also imposes a high cognitive load on the developer. This is especially true if you try to *rotate* the stack like you could with the now-removed functions `roll` and `unroll`. [`dip`](/docs/dip) replaced the rolling functions as it is more general and easier to reason about, and eventually grew into [Planet Notation](/tutorial/morestack#planet-notation).

As more and more Uiua code was written, I developed the principle of [Stack-Source Locality](/tutorial/tacitcode#stack-source-locality) to guide readability. The [`on`](/docs/on) and [`by`](/docs/by) modifiers were added to express common patterns of stack manipulation.

### Expressions

Long tacit expressions in most array languages can get very unwieldy. Because binary operations are infix, you have to parse the tree structure in your head before you can start determining the order of operations.

For example, in BQN, you can trim matches from the beginning of a string with [`x(∧<backtick>∘∊˜¬⊸/⊢)y`](https://mlochbaum.github.io/bqncrate/?q=Remove%20cells%20that%20appear%20in%20x%20from%20beginning%20of%20y#).

In contrast, here is their equivalent in Uiua, implemented the same way:

```uiua
Trim ← ▽¬\×⊸∊
```

You'll notice that stack basing simplifies the expression in a few ways:

- There is no Uiua code corresponding to the BQN combinator `∘`. Function composition is implicit.
- Functions are executed right-to-left instead of in a tree ordering.
- The expression does not require `()`. In fact, no Uiua expression requires explicit grouping. `()` is used to make inline functions instead.

I think this clarity makes writing long tacit expressions much more workable.

## The Array Model

Uiua's array model went through a lot of iterations during development. At first, it used a flat, vector-based model ala K and Q. Then, I switched to BQN's Based array model. That was really complicated to implement primitives for, so I tried something else.

I switched to a flat array model with "fill elements". While arrays could not be nested, operations which would create nested arrays in other languages would instead create jagged arrays with special fill elements at the end of some rows. While this worked, the code was scattered everywhere with checks for fill elements, because they had to propagate through everything. It also had the unfortunate effect of making byte arrays take up 2 bytes of space, since a bit had to be used to indicate whether the byte was a fill element or not. Also, a lot of operations, such as [`transpose`](/docs/transpose), don't really make a lot of sense with jagged arrays.

Finally, I switched to the current model, which resembles J's Boxed array model. While you can do something resembling J's `box <` using [`box`](/docs/box) (and `open >` with [`un`](/docs/un)[`box`](/docs/box)), I designed functions like [`partition`](/docs/partition) and [`group`](/docs/group) to allow selecting uniformly-shaped rows from a non-uniform list in an effort to minimize interaction with jagged data.

The fact that the stack is always available also makes putting non-uniform data in arrays less necessary.

## The Glyphs

Most of Uiua's glyphs were chosen for one of a few reasons:

- It is a common mathematical symbol, such as [`add`](/docs/add), [`subtract`](/docs/subtract), and [`pi`](/docs/pi).
- It is a very commonly used function and should create little line noise, such as [`duplicate`](/docs/duplicate) and [`flip`](/docs/flip).
- It is used in other array languages, such as [`reduce`](/docs/reduce), [`scan`](/docs/scan), and [`transpose`](/docs/transpose).
- It kind of reminds me of what it does. Some of my favorites are [`table`](/docs/table), [`reshape`](/docs/reshape), [`rotate`](/docs/rotate), [`deshape`](/docs/deshape), and [`find`](/docs/find).
- Its function is kind of abstract, but there are other related functions, so they all use related glyphs. For example, [`fold`](/docs/fold) has this nice symmetry with [`reduce`](/docs/reduce) and [`scan`](/docs/scan). The indexing/finding/grouping functions like[`classify`](/docs/classify), [`group`](/docs/group), [`deduplicate`](/docs/deduplicate), etc are all circles.
- Circles and squares look nice, such as with [`where`](/docs/where) and [`pick`](/docs/pick).
- I think they look like cute little guys: [`assert`](/docs/assert) and [`try`](/docs/try).

## No Local Variables

Forbidding general local variables has a few benefits:

- I don't have to implement them (score!)
- It forces you to write (often beautiful) tacit code, which I would argue Uiua enables better than almost any other programming language.
- It frees you from the burden of naming things.
- Because values only exist on the stack as long as they are needed, memory is allocated, reused, and collected in an efficient way.

## Identifiers and Formatting

I made the decision to have a formatter that turns names into Unicode glyphs about as soon as I started using Unicode glyphs. I did not want to require special keyboard or editor support like APL and BQN do.

The advantage of a file-watching formatter is that the only feature your editor needs is the ability to automatically reload files if they change on disk. You don't need special keybinds or plugins or anything.

The other nice thing about a formatter is that it makes it easier to get started with the language. You do not have to memorize a bunch of keyboard shortcuts to type the glyphs. You just need to learn their names.

## Inverses

I originally added [`un`](/docs/un) (originally `invert`) and [`under`](/docs/under) because they were useful-seeming constructs that existed in the other array languages. Little did I know that they would come to be such a central pillar of Uiua's design.

Members of the Uiua community have always pushed for more inverses, I think to the language's great benefit. [`anti`](/docs/anti) was eventually added as an inversion modifier that captured an increasingly useful inversion pattern.

## Inspiration

### BQN

The main language that inspired Uiua is [BQN](https://mlochbaum.github.io/BQN/). While I had heard about APL before, BQN was my first real exposure to the power of the array paradigm. I think the language is an astounding feat of engineering. Marshall is both a genius and a great communicator.

However, as you can read above, a lot of Uiua's design decisions are responses to things I *didn't* like about BQN. There were a bunch of little pain-points that I thought I could improve on.

A lot of the behavior of Uiua's built-in functions (and the choice of which built-ins to include) is inspired by BQN's primitives. Just a few examples are [`transpose`](/docs/transpose), [`classify`](/docs/classify), [`group`](/docs/group), and [`take`](/docs/take).

Another thing that was largely inspired by BQN is this website! BQN's site is excellent. I really like the way it is organized and the way it presents the language. I particularly liked the built-in editor, so I made my own version for Uiua that has syntax highlighting and history, which I reuse in all the tutorials and examples.

### APL

Being the oldest array language, APL has had a lot of time to develop good ideas, particularly for the behavior of primitives.

Uiua's behaviors for [`orient`](/docs/orient), [`windows`](/docs/windows), and [`partition`](/docs/partition) are inspired by APL's.

### J

Uiua uses basically the same array model as J: flat arrays with a box type. The main difference is that Uiua allows some functions, particularly pervasive ones, to implicitly penetrate boxes.

### The Array Cast

During the period of Uiua's development, I spent a lot of time listening to [The Array Cast](https://arraycast.com/), a podcast about array languages. The conversations about the design and implementation of APL, J, K, Q, and BQN are both inspirational and informative. The guys have such a depth and breadth of knowledge on the topic. I really recommend giving it a listen.

Thanks to [Co](https://github.com/codereport)[nor](https://www.youtube.com/@code_report), Bob, Stephen, Adám, [Marshall](https://github.com/mlochbaum), Richard, and all the guests.
