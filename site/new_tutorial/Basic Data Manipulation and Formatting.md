# Basic Data Manipulation and Formatting

## Basic Operations

In Uiua, all operations appear to the left of their arguments.

```uiua
+ 2 3
```

We often read Uiua code from right to left to understand the order of operations. In this example, we [add]() `3` to `5`, then [multiply]() by `2`.

```uiua
× 2 + 3 5
```

Code is also executed from top to bottom. We could actually split the code above into multiple lines if we wanted to.

```uiua
+ 3 5
× 2
```

If code generates multiple outputs, those outputs will be displayed on multiple lines.

```uiua
7
+ 1.1 5
× 2 10
```

Outputs will be displayed on multiple lines even if they were generated on the same line. The value that was generated last will be displayed at the bottom.

```uiua
+ 1 2 5
6 × 7 8 9
```

Because functions always take the same number of arguments, we can actually rearrange associative operations like [add]() or [multiply]().

```uiua
+ 2 + 3 5
+ + 2 3 5
```
By wrapping functions and their arguments in `()`s, we can make the grouping more visually apparent.

```uiua
(+ 2 (+ 3 5))
(+ (+ 2 3) 5)
```

## Comments

Comments are denoted with a `#` and run to the end of the line. Comments are useful for documenting what a particular piece of code does.

```uiua
5 # This is a comment
```

Uiua does not have multiline comments.

## Formatting

Most of Uiua's built-in functions use special Unicode characters. To type multiplication and division signs, you can use `*` and `%` respectively. Then, run the code to format the ASCII characters into Unicode.

```uiua help(⇡⇡⇡⇡ Click   )
# Click Run to format!
%6 *3 8
```

Most built-in functions have names you can type rather than symbols. Formatting works on these too. **This is the primary way of entering Uiua's glyphs.**

Try formatting the lines below by clicking **Run**.

```uiua
max sqrt 10 mod 10 pow 2 8
```

```uiua
abs +`1 `2
```

You don't have to type the whole name, just enough to disambiguate it from others.

```uiua
cei 1.5
ceil 1.5
ceili 1.5
ceilin 1.5
ceiling 1.5
```

You don't even have to remove spaces between built-in function names. The formatter will figure it out!

```uiua
roundsqrtpi
```

On this site, you can also click the `⌄` symbol on any editor to show a palette of all the Uiua glyphs. You can then click on any glyph to insert it into the editor.

The [negate]() function formats from `` ` ``, but this is also used for negative numbers.

```uiua
+ `4 20 # Format me!
` + 5 2
```

The formatter will align consecutive end-of-line comments. Try it out!

```uiua
%2 8 # Line
@x # these
1 # up
```

## Output Comments

A comment that starts with additional `#`s is an *output comment*. The formatter replaces the text of an output comment with as many values as there are extra `#`s. These are the values that can be used by the next function that is called.

Click Run to try it out!

```uiua
1 2 4
####
+
###
+
##
```

Output comments on the same line as other code show the values output after that line is run. This is useful for debugging.

```uiua
+1 2 ##
×3 4 ##
```

## Manipulating Data with Modifiers

So far, we can do the same sort of basic operations you can do on a calculator. But programming is about more than that!

What if you wanted to [multiply]() a number by itself? Of course, you could simply square it with the [power]() function.

```uiua
ⁿ2 5
```

But not every function can be mathematically rearranged.

Uiua uses a syntactic construct called a *modifier* to change the behavior of functions.

Modifiers must appear directly to the left of the function(s) they modify. They *cannot* appear on a different line.

There are many built-in modifiers, but we'll look at a few related to data manipulation here.

### [self]()

[self]() turns a function that takes multiple arguments into a function that takes one argument by passing it the same argument multiple times. When used on [multiply](), this can get us our squaring function from before.

```uiua
˙× 5
```

By using `()`s, we can apply a modifier to a more complex function.

```uiua
˙(×+1) 5
```

### [backward]()

[backward]() makes a function that takes two arguments take them in reversed order.

```uiua
 - 2 5
˜- 2 5
```

```uiua
 ÷ 2 10
˜÷ 2 10
```

### [on]()

[on]() makes the first argument of a function remain available to be the first argument to the next function that will be called.

```uiua
⟜√ 81
```

The value that appears at the bottom of the output list would be the first argument of the next function to be called.

```uiua
⟜+ 2 5
```

You'll most often see [on]() with another function immediately to its left, to use that preserved value.

```uiua
×⟜+ 2 5
```

### [by]()

[by]() is a sort of opposite of [on](). It keeps the *last* argument of a function available as an argument (after its outputs) to the next function that will be called.

```uiua
⊸÷ 4 12
```

```uiua
-⊸√ 25
```

[by]() is often used in examples on this site to show both the inputs and outputs of a function.

```uiua
⊸√ 144
```

```uiua
3
⊸(+2)
⊸(×10)
```

### [dip]()

[dip]() temporarily ignores the first argument and calls its function on later arguments.

```uiua
⊙¯ 3 5 # Ignore 3
```

```
⊙+ 2 3 5 # Ignore 2
```

[dip]() can be chained to dip past multiple arguments.

```
⊙⊙⊙× 1 2 3 4 5 # Ignore 1 2 3
```

### [args]()

[args]() prints out the current argument list. This is useful for debugging.

```uiua
+1 ? ×2 ? ˙×-3 5
```

## Challenges

At the end of most sections of this tutorial, there will be a few challenges to test your understanding.

The code you write will be run on multiple inputs and tested for correctness.

Each challenge has an example input and output followed by some test cases.

Remember that you can click the `⌵` on the right side of the editor to see a list of all the glyphs.

Answers are available, but **try to solve the challenges yourself first!**

Some challenges have additional answers that use functions and concepts not yet covered in the tutorial, but which are more idiomatic.

```challenge
adds 3 numbers
++

1 2 3
0 10 1
10 ¯1 5
0 5 1
```

```challenge
divides the first number by the second
˜÷

5 10
6 24
2 100
17 51
```

```challenge
subtracts the second number from the first then squares the result
˙×˜-

10 1
5 3
9 2
5 6
```

```challenge
adds the first number to the square root of the second
+⊙√

2 9
10 4
9 121
16 16
```