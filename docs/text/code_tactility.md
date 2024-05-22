# Code Tactility

One of the strengths of interpreted programming languages, and array languages in particular, is their high level of interactivity. It is very easy to experiment with code, moving data and functions around to get a better sense of the solution to a problem.

This exercise can be likened to "feeling" the code, turning it around in your hand, poking it, prodding it, inspecting it.

This section covers a few tools that Uiua provides for making code easier to work with and debug.

## [stack]() and [trace]()

Uiua already prints out any values left on the stack when the program ends. This is usually enough for smaller programs, but sometimes, you need to be able to see what values are on the stack somewhere in the middle of the code.

[stack]() and [trace]() were mentioned near the [beginning](/tutorial/basic#stack-and-trace) of this tutorial. They both print out values from the stack without removing them. [stack]() prints all values while [trace]() only prints one.

```uiua
5 ? 1 [2 3] "uiua"
```

```uiua
5 1 ⸮ ⇡3 "uiua"
```
As you can see, the line and column number is also printed.

The formatter will convert two `?`s into [trace](). Try it below.

```uiua
?? √5
```

Multiple [trace]()'s next to each other will combine to show multiple stack values.

```uiua
⸮⸮⸮ 1 2 3 4 5
```

[stack]() will show you the boundaries of the functions that values are used in.

```uiua
G ← /+?⇡
F ← ×G
F 10 4
```

[stack]() and [trace]() allow you to inspect the stack at a given place in the code. They make it easy to get a quick sense of what values a function is working with.

## Labels

Labels allow you to tag an array with a debug-only name. They are written with a `$` immediately followed by an identifier.

```uiua
$Numbers [1 2 3]
```

Labels are *only* visible in debugging outputs. This includes normal interpreter output as well as [stack]() and [trace]().

Labels will *not* be shown when arrays are formatted using [`&p`]() or format strings.

```uiua
&p $"--_--" . $TheThing ⇡5
```

You can easily label multiple values with [bracket]().

```uiua
⊓$Foo$Bar °⊂ [2 3 5 7]
```

Labeled arrays cannot be put into arrays together unless they are boxed.

```uiua
[$a 1 $b 2 $c 3] # Wrong
```
```uiua
{$a 1 $b 2 $c 3} # Right
```

Labels are nice for keeping track of different values as they move around the stack.

It is not possible to retrieve the label of a value using code. Labels are only for debugging, not for carrying data.

## Line Manipulation

Uiua running code right-to-left has a couple unfortunate side effects when it comes to editing. The first you're likely to run into is having to constantly press the `←` key on your keyboard to move the cursor to the left.

If you type a `;` character, the formatter will flip the current line across it. This allows you to enter code in the same order it is executed.

```uiua
⇡12;↯3_4 # Format to flip!
```

You can put many `;`s on the same line.
```uiua
1;2;3;4
```

Another problems is that if you want to split a line of code into two lines, it is not enough to simply place your cursor at the split point and press Enter, as this puts the lines in the wrong order!

Instead, you can type `;;` at the point you want to split the code. Upon formatting, the line will be split in a way that preserves the same meaning!

```uiua
↥⊸⇌;;⊞=.⇡5 # Format to split!
```

You can put as many of these splitters in your code as you like, and the formatter will handle them all at once.

```uiua
"lines";;"the";;"all";;"Split"
```

If used in a binding, the code will be wrapped in `()`s.

```uiua
F ← ⁅∵⋅⚂;;↯⟜⊚ # Format it!
F 5
```

Putting a splitter at the beginning or end of a line will *join* lines rather than splitting them.

```uiua
1 2
;3 4
```

```uiua
"Oh";
"boy!"
```
