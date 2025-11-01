# Working with Strings

There is a common misconception that array languages like Uiua are only really good for mathematical tasks; that rich text processing is better left to more traditional languages. This is not the case! Strings are, after all, just arrays of characters!

That being said, it may not be immediately clear how to perform common string manipulations in Uiua. This tutorial will cover the basics.

## Converting numbers with [`parse`]()

[`parse`]() is the standard way to convert a string to a number.
```uiua
⋕"5.2"
```
It can also be used on arrays of boxed strings.
```uiua
⋕{"3" "-16" "π" "1e3"}
```
[`un`]()[`parse`]() will convert numbers to strings.
```uiua
°⋕ 10
°⋕ [1 2 3]
°⋕ ↯3_4⇡12
```

## Splitting with [`partition`]()

As discussed in the [Thinking With Arrays](</tutorial/Thinking With Arrays>) tutorial, [`partition`]() can be used to split an array by a delimiter.

First, we create a mask of places where the delimiter is *not* using [`by`]() [`not equals ≠`](). In this case, we'll use the space character.
```uiua
⊸≠@  "Split this string"
```
[`partition`]() will then split the strings at the places where the mask changes, omitting `0`s.
```uiua
⊜□ ⊸≠@  "Split this string"
```
Notice that this reads almost as a description of what it does: "Partition box by not equals space"

This can be nested to split by multiple delimiters.
```uiua
"A string\nwith multiple\nlines"
⊜(□⊜□⊸≠@ )⊸≠@\n
```

For parts of the string that are not the first or last, we can simply [`box`]() and [`select`]().
```uiua
⊏1_3 ⊜□⊸≠@, "lorem,ipsum,dolor,sit,amet"
```

[`partition`]() can be nested to split by multiple delimiters.

For example, if you were reading from a file that contained rows of numbers separated by spaces, you could use [`partition`]() to create a multi-dimensional array.

Here, the contents of the file will be represented as a multi-line string. We use [`parse`]() as the inner function to parse the numbers.

```uiua
$ 1  8   4 99
$ 5  20  0 0
$ 78 101 1 8
⊜(⊜⋕⊸≠@ )⊸≠@\n
```

This assumes that the two delimiters delimit different dimensions of the array. If they delimit the same dimension, we can use [`not`]() and [`memberof`]().
```uiua
$ 1  8   4 99
$ 5  20  0 0
$ 78 101 1 8
⊜⋕¬⊸∊" \n"
```

## Finding substrings with [`mask`]()

What if we want to split by a non-scalar delimiter? Simply dropping a string delimiter into the code above produces an error.
```uiua should fail
⊜□ ⊸≠" - " "foo - bar - ba-az"
```
We might try [`find`](). While there may be cases when this output is useful, it is not quite what we want here.
```uiua
    ⊸⌕" - " "foo - bar - ba-az"
⊜□ ¬⊸⌕" - " "foo - bar - ba-az"
```
This is because [`find`]() only marks the start of each matching substring.

[`mask`]() marks each substring with an increasing number.
```uiua
⊸⦷" - " "foo - bar - ba-az"
```
This works great with [`partition`]() to split the string how we want.
```uiua
    ⊸⦷" - " "foo - bar - ba-az"
   ¬⊸⦷" - " "foo - bar - ba-az"
⊜□ ¬⊸⦷" - " "foo - bar - ba-az"
```
Notice that while [`not`]() leaves parts of the mask negative, [`partition`]() ignores all sections that are not positive.

## Replacing substrings with [`under`]()

Because [`under`]() works with [`partition`](), we can use it with [`mask`]() to replace substrings.

In this example, we replace each row of the [`partition`]()ed array with the string `"orb"`.
```uiua
           ⊸⦷ "ab" "abracadabra"
 ⊜∘        ⊸⦷ "ab" "abracadabra"
⍜⊜∘≡⋅"orb" ⊸⦷ "ab" "abracadabra"
```
This can even be used to replace the matches with different strings.
```uiua
⍜⊜□◌ ⊸⦷ "ab" "abracadabra" {"[first]" "[second]"}
```
Here is how you might replace with a variable number of strings.
```uiua
F ← ⍜⊜□(↙⧻) ⊸⦷ "ab" ⊙(°⋕⇡10)
F "abracadabra"
F "abcdefg"
F "ababab|abababab"
```

## Format String Tricks

As discussed [in the main tutorial](/tutorial/Modifiers and Functions#format-strings), format strings allow you to create strings with interpolated values.

```uiua
$"_ + _ = _" 1 2 3
```

Format strings have some non-obvious uses though.

You can use [`reduce`]() with a format string to intersperse a delimiter between formatted items.

```uiua
/$"_ - _" ⇡5
```

```uiua
/$"_ and _" {"red" "blue" "green"}
```

[`un`]() with a format string attempts to match and extract substrings, a sort of simplified [`regex`]().

```uiua
°$"_(_) = _" "f(x) = x^2"
```

```uiua
°$"_,_" "1,2,3,4"
```

[`un`]() [`reduce`]() with a format string splits by a delimiter. The results are [`box`]()ed.

```uiua
°/$"_;_" "5;cool;26;([])"
```

By combining these with [`under`](), you can modify certain substrings.

```uiua
⍜(⋕°$"_,_"|×10) "2,3,4"
```

```uiua
⍜°/$"_;_"(⍚⊂⊙@s) "apple;banana;cherry;durian"
```

You can use [`fork`]() or [`bracket`]() to pre-format certain fragments of the string.

While this example could be written shorter with other stack manipulation modifiers, using [`fork`]() in this way makes it easier to understand what is happening, as each `_` in the format string gets its own branch in the [`fork`]().

```uiua
F ← $"_ is _ year_ old." ⊃(∘|⋅∘|▽⊙@s≠1⋅∘)
F "Dave" 31
F "Remi" 1
```

## [`regex`]()

When a string search operation is especially complicated, you can always fall back to regular expressions using [`regex`]().

Uiua uses [Rust's regex engine](https://docs.rs/regex) under the hood, so you can use the same syntax as you would in Rust.

[`regex`]() returns a table of boxed strings. The first element in each row is the match. Subsequent elements are the captures.
```uiua
regex "\\d{3,4}" "(555) 310-1984"
```
```uiua
regex "a([bc])" "abracadabra"
```
Optional captures may need [`fill`]() to avoid errors.
```uiua should fail
regex "foo(bar)?(baz)?" "foobar\nfoobaz"
```
```uiua
⬚""regex "foo(bar)?(baz)?" "foobar\nfoobaz"
```