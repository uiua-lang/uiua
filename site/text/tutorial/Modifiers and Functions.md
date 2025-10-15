# Modifiers and Functions

## Array Modifiers

So far, the modifiers we've seen like [self](), [by](), and [dip]() have all had to do with manipulating how functions take their arguments. But modifiers are a general syntactic construct that allow us to change the behavior of a function.

Because Uiua is an array language, it has many modifiers that change how their function applies to the structure of arrays.

For example, [reduce]() applies its function "between" all rows of an array.

`/+` is therefore the sum of all the rows of an array.

```uiua
/+ [1 2 3 4]
```

We can do something similar with [multiply]() to get the product.

```uiua
/× [2 3 4 10]
```

[rows]() applies a function to each row of an array.

For example, if you use [reverse]() on a matrix, it is the rows that will change order.

```uiua
⇌ [1_2_3 4_5_6 7_8_9]
```

But if we use [rows](), the columns change order instead, as the [reverse]() function gets applied to each row.

```uiua
≡⇌ [1_2_3 4_5_6 7_8_9]
```

In a similar way, `/+` will add each row of a matrix to the next, effectively summing each column. But `≡/+` sums along the rows themselves.

```uiua
 /+ [1_2_3 4_5_6 7_8_9]
≡/+ [1_2_3 4_5_6 7_8_9]
```

[table]() applies a function between all combinations of rows of two arrays. This is sometimes called the *outer product*.

In the same way that "monadic" and "dyadic" functions refer to functions that take one or two array arguments respectively, "monadic" and "dyadic" *modifiers* refer to modifiers that take on or two *functions* respectively.

```uiua
⊞+ [5 6 7 8] [10 20 30 40]
```

On this site, `monadic mod` modifiers are in `monadic mod yellow` and `dyadic mod` modifiers are in `dyadic mod purple`.

The main docs page has [a list](/docs/modifier) of all of the built-in modifiers.

## Inline Functions

We've seen previously how wrapping some code in `()`s allows a modifier to apply to a more complex function. This is called an *inline function* and is similar to anonymous functions or lambdas from other languages.

If you wanted to add each row of an array to its reverse, you could put [add]()[by]()[reverse]() in an inline function for [rows]().

```uiua
⊸≡(+⊸⇌) [2_5_3 0_2_1 0_0_2]
```

Inline functions may span multiple lines. Unlike multiline array notation, which runs bottom-to-top, multiline inline functions run top-to-bottom as other code does.

```uiua
X ← (
  ˙⊞=⇡ # First this line runs
  ↥⊸⇌  # Then this one
)
X 5
```

Output comments inside inline functions will show the values for each time the function is called. Try it out!

```uiua
F ← (
  ### Run to see values here!
  +⊸× ##
)
F 3 5
F 2 9
F 10 11
```

## A Note on Local Bindings

Bindings in Uiua can *only* bind a single value once for the entire lifetime of the program. There is no way to give a name to a value that is the result of some intermediate calculation in a function.

This is a deliberate design decision. It forces you to write tacit code, a.k.a. code with functions that do not mention their arguments. Uiua is designed to make writing tacit code as workable as possible.

## Format Strings

Prefixing a string with a `$` creates a format string. A format string is a special kind of function. It takes an argument for each `_` in the string and replaces it with the stringified version.

```uiua
"World"
$"Hello, _!"
```
```uiua
Greet ← $"Hello, _!"
Greet "user"
```
```uiua
x ← 5
$"x = _" x
```
```uiua
$"_, _, and _" 1 2 3
```

If you need to use a literal `_`, you can escape them with `\`.

```uiua
$"\__\_" 27
```

Raw strings can be made format strings by adding an additional `$`.

```uiua
◡+ 1 2
&p $$ What are two numbers that add up to _?
   $$ _ and _ do!
```

`_`s still need to be escaped in raw format strings.

```uiua
1 2 3
$$ _\__\__
```

Because format strings are just functions, you can use them with modifiers like [reduce](). This is a common way to join a list of [box]()ed strings.

```uiua
/$"_ _" {"Separated" "by" "spaces"}
```

The [Working with Strings](/tutorial/strings) tutorial has a [section](/tutorial/strings#format-string-tricks) with more format string tricks.

## Signatures

Bindings and inline functions can have a *signature* declared with a `|` followed by 1 or 2 numbers separated by a `.`. The first number is the number of arguments the function takes. The second number is the number of values the function outputs.

The second number is optional. If it is not given, it is assumed to be 1.

In bindings, the `|` comes after the `←`. In inline functions, it comes after the `(`.

```uiua
Times₃ ← |1.1 ×3
Times₃ 7
```
```uiua
Times₃ ← |1   ×3
Times₃ 7
```
```uiua
≡(|2.1 ˙⊟×) 1_2_3 4_5_6
```

Signatures are useful for documenting functions to make sure that they are used correctly.

A declared signature always overrides the inferred signature. However, if they do not match, a warning will be emitted.

```uiua should fail
F ← |3.2 ⊟+1
F 1 4
```

In that example, the net change of the declared signature of `|3.2` is actually the same of that of the inferred signature of `|2.1`. Either way, the function outputs one less value than it takes.

If this is *not* the case, the declared signature will be *made* to be correct by, after the function has run, either removing extra arguments or adding extra outputs.

For example, even though this functions has signature `|2.1`, the declared signature of `|2.2` causes an extra debug output to be returned. This debug output is a boxed string. This is done to ensure that the function does in fact have 2 outputs.

```uiua should fail
F ← |2.2 ⊟+1
F 1 4 5
```

Conversely, if the function needs to have more *arguments*, such as here where we declare it to have signature `|4.2`, some extra arguments will be consumed. In this case, it is the `6` that is removed. We can see that this does in fact make the function turn 4 values into 2.

```uiua should fail
F ← |4.2 ⊟+1
F 1 4 5 6
```

**The point of allowing this is that function which are only partially written can still be run and debugged. But finished code should never have warnings!**

If the compiler cannot derive the signature of a function and you give it one which is *wrong*, the function will throw an error at runtime.

## Challenges

```challenge
calculates the product of the first n positive integers
/×+1⇡
/×⇡₁
5
10
0
7
```

```challenge
adds each column of a matrix to the next
≡/+

[1_2_3 4_5_6]
[6_9_1_2 3_0_0_1 2_3_4_5]
[2_2_2_2_5]
```

```challenge
wraps a string in brackets
$"[_]"

"Hello, World!"
"Uiua"
"🙃"
```