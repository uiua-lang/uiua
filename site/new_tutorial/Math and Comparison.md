# Math and Comparison

Uiua supports all the basic math operations as well as comparison, min/max, and rounding.

`MATH TABLES`

Most of these are used mostly how you might think.

```uiua
+2 5
```
```uiua
↥2 5
```
```uiua
ⁿ2 5
```
```uiua
⌈2.5
```
```uiua
√4
```

Uiua has no boolean type. Comparison operators return `0` for false and `1` for true.

```uiua
=2 5
=2 2
```

One thing to note is that non-commutative operators work backwards.

This is so you can think of the operator and the second number as a single unit.

```uiua help(What is 5 "minus 2"?)
-2 5
```
```uiua help(Is 5 "less than 2"?)
<2 5
```
```uiua help(What is 5 "divided by 2"?)
÷2 5
```

Remember that you can type the names of operators and then run to format them.

```uiua help(⇡⇡⇡⇡ Click   )
# Click Run to format!
max sqrt2 mod10 abs`31
```

## Adicity

Some programming languages use the terms "unary" and "binary" to refer to functions that take one or two arguments respectively. While these are the latin terms, many array languages, including Uiua, prefer to use the Greek terms "monadic" and "dyadic".

As you read Uiua's documentation, you will see these terms used to describe functions (and modifiers).

For example, [sqrt]() is a monadic function, and [add]() is a dyadic function.

On this site, `monadic` functions are in `monadic green` and `dyadic` functions are in `dyadic blue`.

Some documentation may also reference functions which are `noadic`, `triadic`, or `tetradic`. These are the words for functions that take 0, 3, or 4 arguments respectively. The word `noadic` is not common outside of Uiua, but it is chosen because such a functions takes *no* arguments.

## Challenges

```challenge
for arguments A, B, and C, computes (A + B) × C
×+

1 2 3
2 2 2
5 7 2
3 ¯1 ¯1
```

```challenge
calculates the equation √(A² + B), where A is the first argument and B is the second
√+˙×
⍜˙×+
3 16
12 81
12 25
6 64
```