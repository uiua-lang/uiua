# A Uiua Type System

2025-08-01

---

This blog post explains the rational and design of Uiua's `# Experimental!` new type system.

## A Dynamic Language

Uiua, like most array languages, is dynamically-typed. While the shape and the scalar type of an array can often be known at compile-time, the language is not designed in a way that makes this *always* possible. Anything that involves shapes or scalar types derived from user input, the environment, or randomness can necessarily only be know at run time. Consider these examples:

Runtime variable scalar type:
```uiua
F ← ⍥(˜-@a)⚂₁₀
⊃(F|F|F|F|F) @U
```
Runtime variable shape:
```uiua
↯ ⁅◿10×10now 0
```

In most cases, this make Uiua programs simpler to write. You don't need to specify concrete information about array in function type signatures as you would in a statically typed language. Also, functions can work on many different types and shapes of array.

```uiua
F ← ⊂↯
F 4 0 [1 2 3]
F 3 [1 2] [3_5 5_6]
F 8 @U "iua"
```

While there is a small performance overhead of checking the types at runtime, the array paradigm makes it easy to do the checks only single time for an operation on many values.

However, this dynamism can be a double-edged sword, especially in larger and/or longer-lived codebases where maintainability is a concern.

## Prior Art

This is not a new problem in dynamically-typed languages. Many of the prominent dynamic languages have systems for *gradual* types: opt-in type checking for the parts of the code that really need it. This generally comes in the form of either some tooling-supported type specification syntax (JSDoc, Python types, LDoc, etc) or a full super-set language (TypeScript, Luau, etc).

While the specifics of each of these systems can vary, the user interacts with most type systems in a similar way: they specify input and/or output types, and the compiler/interpreter/tooling uses that information to check types and compile/runtime and/or show the types in documentation. This is a useful mode of interaction, and any potential Uiua type system should support both type checking and type documentation.

## Potential System: A language for types

Most languages with types have an entire sub-language for specifying types. Consider the type that is a hash map with strings as keys and where each value is a tuple of a float and a list of integers. This is generally expressed in a similar way in all languages, with varying levels of verbosity, from Haskell's `Map String (Float, [Int])` to C++'s `std::unordered_map<std::string, std::pair<float, std::vector<int>>>`. There are languages that break this mold though, such as OCaml `(string, float * int list) Hashtbl.t`, which puts the hash map type *after* the inner types, or Zig `std.StringHashMap(struct { f32, []usize })`, in which generic types are functions.

The actual semantics of these systems vary widely, but the goal is generally for the type of a value to be able to be specific enough to uphold certain invariants, for catching errors at check-time and/or allowing for optimizations when a type is known.

Array languages are slightly different in that while we do model our domains in our data, basically every value is some kind of array. This could make an array language's type system potentially very simple. Simply specify the scalar type and the shape of each value. Something so simple might not even need to be implemented as a type system, as it might be so little cognitive overhead as to be manageable by the user themselves.

But of course, even array types are not so simple. The most common variance in an array value is its shape. Some axes may be a static, known size, but other can be dynamic. The rank of the array itself may even be unknown at compile time; this is one of the consequences of rank polymorphism. On top of this shape variability is managing the types of nested arrays, which in Uiua come in the form of boxes. Do all the boxed items have the same inner type? Do they vary because we are using the box array as a way to group heterogeneous data, as in a [data definition](https://www.uiua.org/tutorial/Data%20Definitions)? All of these should be encodable in a type system.

If Uiua were to take a similar approach to the languages referenced above, it may have some type sub-language that can be added to existing doc signature comments.

```not uiua
# Magnitude:Real ? Numbers:Real[_]
Mag ← ⍜˙×/+
```

In this imaginary syntax, `[_]` indicates an array of rank one with an unknown axis length. Of course, we then have to ask how this type signature could be expanded to take into account the rank-polymorphic nature of this function.

```uiua
Mag ← ⍜˙×/+
Mag [3 4]
Mag [3_5 4_12]
```

A syntax would be needed to specify the variable-rank shape of the input and the corresponding reduced-rank output. The system becomes a burden on both the user, who may end up writing more type code than actual code, and the implementer (me), who has to implement a complex type system that takes into account many constraints.

There is also a Uiua-specific reason that I did not ultimately choose a system like this.

## Formatting

Uiua's formatter does more than *any* language that is even semi-popular. Not only does it turn words into glyphs and align brackets and things, but it also includes [line manipulation](https://www.uiua.org/tutorial/codetactility#line-manipulation), comment-rewriting ([Output Comments](https://www.uiua.org/tutorial/Basic%20Data%20Manipulation%20and%20Formatting#output-comments)), rich unicode delimiters ([Modules](https://www.uiua.org/tutorial/Modules)), and snippet-like [multi-glyph aliases](https://www.uiua.org/docs/idioms#multi-aliases).

The Uiua formatter is a fundamental part of how the language is meant to be used, and indeed it can be leveraged by a type system to do things other languages can't or don't.

Some strongly typed languages like OCaml or TypeScript have very powerful type systems that can figure out a *lot* for you. You often don't even need to provide types annotations for certain things. However, this can cause problems when, for example, the type of the value you return from a function is not what you think it is. This can then bubble up through multiple functions and the resulting type error is attached to code far from where the actual problem is. So people write the type annotation even when they don't strictly have to. But if they write the annotations anyway, what was the point of making the type systems so powerful in the first place?

Uiua's new type system can figure out some argument and output types and then *insert them* into the code itself, allowing the user to verify the correctness of their function without having to explicitly specify return types themselves. Other languages may have LSP implementations that provide this as a code action, but Uiua can give it first-class support and closely integrate it into the language.

We'll see further down how Uiua can take advantage of the formatter to insert types.

## [`⊨ validate`](https://uiua.org/docs/validate)

[`⊨ validate`](https://uiua.org/docs/validate) is a new function that takes both a *type specification* and a value and either returns the value unchanged or errors if the type specification does not match the value. Like everything else in Uiua, the type specification is just an array. This is so that while you have to know how to construct a type specification for the type you want, you still write it in normal Uiua rather than a separate language.

You can read the specifics of type specifications in [`⊨ validate`](https://uiua.org/docs/validate)'s documentation, but in short, it is either a scalar character representing the scalar type, a list of numbers representing the shape, or a box list containing both the scalar type and the axis lengths. `∞ infinity` serves as a wildcard axis length. Most of the scalar characters specifying types have single-glyph [constants](https://www.uiua.org/docs/constants) that format from an alias, such as `nat` -> `ℕ` for natural numbers.

```uiua
# Experimental!
⊨ℕ 5                  # Scalar natural number
⊨𝕌 @X                 # Scalar unicode character
⊨[2 3] [1_2_3 4_5_6]  # Any 2×3 array
⊨{ℝ 2 2} [4_¯5.1 3_0] # 2×2 array of real numbers
⊨{𝕌∞} "Uiua"          # List of characters; `str` alias
```
Here are some examples of the error messages.
```uiua
# Experimental!
⍣⊨⋅⋅∘ ℕ ¯3
⍣⊨⋅⋅∘ 𝕌 6
⍣⊨⋅⋅∘ [2 3] [1_2 3_4 5_6]
⍣⊨⋅⋅∘ {ℝ ∞ 3} [4_¯5.1 3_0]
⍣⊨⋅⋅∘ {𝕌∞} ["Uiua" " is " "cool"]
```

Type specifications can be nested to validate structure-like box arrays.
```uiua
# Experimental!
⊨{ {𝕌∞} ℕ} {"Dan" 31}
⍣⊨⋅⋅∘ { {𝕌∞} ℕ} {"Dan"}
⍣⊨⋅⋅∘ { {𝕌∞} ℕ} {[1 2 3] 10}
```
Structure-like type specifications are built into data definitions as a `t` item. Use [`⊨ validate`](https://uiua.org/docs/validate) in the initializer to specify a field's type.
```uiua
# Experimental!
~Person {Name ← ⊨{𝕌∞}|Age ← ⊨ℕ}
Person.t
⊨Person.t Person "Dan" 31
```
Sided [`⊨ validate`](https://uiua.org/docs/validate) specifies that the shape is a shape prefix or suffix so that arrays of different ranks are allowed. This example requires that the shape *starts* with `2`.
```uiua
# Experimental!
⊨⌞ [2] [1 2]
⊨⌞ [2] [1_2 3_4]
⊨⌞ [2] [1_2_3 4_5_6]
⊨⌞ [2] [[1_2 3_4] [5_6 7_8]]
```

## Type Signature Comments

Comments that start with `#?` are *type signature comments*. The formatter will replace them with a representation of the overall signature of the function below.

```uiua
# Experimental!
#? Try formatting!
F ← /+ val{num∞}
#? Try formatting!
F ← /+ val{num3 ∞ 2}
```
`_` in this example indicates an axis of unknown length.

The `…` in this next example indicates some number of unknown axes between the known ones. Notice we can use [`⊓ bracket`](https://uiua.org/docs/bracket) to specify multiple argument types. Also note that even though we didn't specify the scalar types of the arguments, the output scalar type is still characters `𝕌`, as [`+ add`](https://uiua.org/docs/add) assumes numeric scalar types when a type is unknown.
```uiua
# Experimental!
#? 2×3×…𝕌 ? [3] [2×…]
F ← +@a+¤ ⊓(⊨[3]|⊨⌞[2])
F [0 1 2] [0_0_0 0_1_0]
```
If a function with a signature comment has a type error, it is shown as a warning.
```uiua should diag
# Experimental!
#?
F ← + ∩(⊨𝕌)
```
Sometimes [`⊨ validate`](https://uiua.org/docs/validate) is not even necessary, like when a primitive used in a function has single possible output type.
```uiua
# Experimental!
#? ℝ ? str
SumFile ← /+⊜⋕¬⊸∊" \t\n\r" &fras
```
You can type check all function in a scope with the `# Type check!` semantic comment.
```uiua should diag
# Experimental!
# Type check!
F ← + °⊟↙3
G ← ≡↻°⊏
H ← ↯⊓json⋕
```

## Type Checking

The type checker analyzes Uiua code at compile time when a type signature comment or `# Type check!` is present. It is a kind of runtime that runs the code on types instead of concrete values. Which built-in functions are fully or partially supported is best-effort and will gradually be added to over time. Currently, [`⊨ validate`](https://uiua.org/docs/validate) informs the compile-time type checker, but it also always runs at runtime as well, as the type system can't catch everything. In addition, while compile-time validation occurs, the type system does not currently inform optimizations.

[`⊨ validate`](https://uiua.org/docs/validate) serves as an input to this type system while type signature comments and warnings provide output. These both remain `# Experimental!` for the time being as there is a lot of design that could be tweaked or overhauled. It's possible we (or I) decide that a Uiua type system is not even necessary at all. That said, the original motivation for a type system was verbose code I saw of people writing their own versions of [`⊨ validate`](https://uiua.org/docs/validate), and this well-integrated system is surely better than that.

While the type system isn't expected to be used on even the majority of Uiua code, it may be useful for large codebases or public-facing library functions. Uiua will stick with the array paradigm's dynamically-typed roots. I'll leave the creation of a fully statically typed array language to someone else.
