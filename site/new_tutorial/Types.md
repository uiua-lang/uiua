# Types

Every value in Uiua is an array. However, different arrays on the stack can have different *types* of items. Every element of an array is always the same type. Unlike some other array programming languages, Uiua arrays cannot have elements of different types.

There are four types of arrays:
- **Number**
- **Complex**
- **Character**
- **Box**

## Numbers

Numbers are decimal numbers with floating precision. They use the IEEE-754 double-precision floating-point format.

```uiua
[5 6e3 e 0 3.2 3/4 ¯1.1 π ∞ 3π/2]
```

As you can see, numbers can be written as integers, with decimals, and as fractions. They can also contain numeric constants like `π`, `e`, and `∞`.

In cases where a number with a fractional part has repeating decimals, or when floating-point errors create tiny differences, the number will be shown with repeated decimal digits replaced by a `…`.

```uiua
1/3
1/12
1/24
+ 0.1 0.2
```

Even though numbers can have a fractional part, many built-in functions require whole numbers. These functions will return an error if given a non-integer number.

One such example is [pick]().

```uiua
⊡ 2 [4 7 9 1 0]
```

```uiua should fail
⊡ 3.1 [4 7 9 1 0]
```

If you want to convert a number to a whole number, you can use [floor](), [ceiling](), or [round]().

## Complex Numbers

Complex numbers can be created with the [complex]() function. The first argument will become the imaginary part and the second argument will become the real part.

```uiua
ℂ 3 5
```

[complex]() is pervasive.

```uiua
ℂ [1 2 3] [4 5 6]
```

While complex numbers support all the same math operations as normal numbers, they are a distinct type and cannot be used in place of normal numbers in many cases.

You can convert a complex number to its normal number magnitude with [absolute value]().

```uiua
⌵ ℂ3 4
```

You can normalize a complex number to a unit vector with [sign]().

```uiua
± ℂ3 4
```

[sqrt]() only returns a complex number if it is called on complex number. Beware of floating-point errors.

```uiua
√  ¯4
√ℂ0¯4
```

Comparing complex numbers for equality returns a normal number.

```uiua
= i ℂ0 1
= i ℂ1 1
```

Comparing complex numbers for order returns a component-wise comparison.

```uiua
< i ℂ¯1 1
≥ i ℂ 1 1
```

In cases where a complex array has no elements with an imaginary part, it will be displayed in output with a `ℂ` marker.

```uiua
ℂ0 5
ℂ0 [1 2 3]
ℂ0 [1_2 3_4]
```

Complex numbers can be written as literals by suffixing the real part with `r` and/or the imaginary part with `i`.

```uiua
[3r4i 5r2i 2ri i/5 rπi]
```

## Characters

Characters are represented as 32-bit Unicode codepoints.

Character literals, denoted with a preceding `@`, create rank 0 (scalar) character arrays.

```uiua
@a @b
```

```uiua should fail
[@u @i @u @a]
```

Characters like newline or null need to be escaped with `\`, but spaces do not.

```uiua
@ 
@\r
@\0
```

If you don't like the significant whitespace of `@ `, `@\s` is also space.

s noted in the advice diagnostic above, string literals, delimited by `"`s, create rank-1 character arrays, which are generally just called strings.

```uiua
⊸△ "Hellow, World!"
```

You can make raw string literals, which do not escaping, with a `$` followed by a space. They run to the end of the line.

[&p]() pretty-prints a value.

```uiua
&p $ "How are you?" she asked.
```

Raw strings that followed each other form multi-line strings.

```uiua
$ Hello
# World!
```

This style of string is useful when your string contains a lot of quotes that yo udon't want to escape.

```uiua
$ And then she was like "No way!"
$ And I was like, "Way..."
```

Characters in character or string literals can also be specified with 2 or 4 hex digits by using escape codes `\x` and `\u` respectively.

```uiua
"\x41\x42\x43"
```

```uiua
@\u2665
```

Longer (or shorter) sequences can be specified between `{}`s after a `\u`.

```uiua
@\u{1f600}
```

Note that these escape sequences do not work in raw strings.

## Character Arithmetic

Characters and numbers exist in an [affine space](https://en.wikipedia.org/wiki/Affine_space), the same as in [BQN](https://mlochbaum.github.io/BQN/doc/arithmetic.html#character-arithmetic).

You can [add]() `number`s and `character`s to get another `character`.

You can [subtract]() a `number` from a `character` to get another `character`.

You can [subtract]() two `character`s to get a `number`.

You can [multiply]() or [divide]() a `character` by a `number` to possible toggle its case.

*No* other dyadic arithmetic operations can be done on `character`s.

```uiua
+1 @a
```
```uiua
-8 "Uiua"
```
```uiua
-@a @z
```
```uiua
× [1 ¯5 0 ¯2] "uiua"
```
```uiua should fail
+@a @b
```

[sign]() gives the case of a character. It gives `1` for uppercase, `¯1` for lowercase, and `0` for caseless characters.

```uiua
± "Hello, World!"
```

[absolute value]() uppercases a character.

```uiua
⌵ "Hello, World!"
```

[negate]() toggles the case of a character.

```uiua
¯ "Hello, World!"
```

Use [negate]() and [absolute value]() together to lowercase a character.

```uiua
¯⌵ "Hello, World!"
```

## Boxes

Boxes are containers that can wrap an array of any type or shape. Multiple boxes can be put in the sam earray, no matter their contents.

Boxes can be created either by using the [box]() function or with boxing array notations between `{}`s.

```uiua
□5
```
```uiua
□[1 2 3]
```
```uiua
□"Hello!"
```
```uiua
{"cat" 5}
```

## Type Agreement

For functions that work on the structure of arrays rather than their values, the types of the arrays must match.

```uiua
⊂ 1_2 3
```
```uiua
⊟ "Hello" "World"
```
```uiua should fail
⊟ 1_2_3 "dog"
```

There is an exception for boxes. Any box can be put in an array with a non-box. In this case, the non-box will be [box]()ed first.

```uiua
⊟ 5 □[1 2 3]
```

## Empty Arrays

The type of an array that is constructed with no elements depends on the syntax used to construct it. Its shape is always `[0]`.

We can use the [type]() function to get the type of an array. `0` corresponds to real numbers, `1` to characters, `2` to boxes, and `3` to complex numbers.

```uiua
type []
```
```uiua
type ""
```
```uiua
type {}
```

## Challenges

```challenge
increments the first character of a string
⊂+1⊢⟜(↘1)
⍜⊢+₁
"`rray"
"Xou're"
"coing"
"freat!"
```