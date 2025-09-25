# Bindings

Bindings are names that can be given to Uiua values. They are denoted with `←`, which the formatter will convert from `=` when appropriate.

```uiua help(Try running to format the =)
A = 3
B ← 5
+ A B
```

Valid binding names can be made up of any sequence of upperscase or lowercase alphabetic characters OR a single non-alphanumeric character that is not already used for a Uiua function or syntax.

```uiua
NumOne ← 1
NumTwo ← 2
😀 ← "happy"
```

*Warning*: It is not guaranteed that any particular non-alphanumeric character will not be used for a built-in function in the future. Use them at your own risk. Emojis are safe though.

Unlike most programming languages, binding names in Uiua *cannot* contain numbers or underscored.

```uiua should fail
Var_1 ← 5
```

Bindings *can* contain subscript numbers. These will format from `,` followed by some digits. Try formatting the example below!

```uiua
X,1 = 5
Sha,256 = "TODO"
```

Subscripts in bindings names are only allowed at the end.

Bindings are allowed to end with `′` characters to help indicate related bindings. These format from `'`s. Try formatting the following example.

```uiua
X = [1_2 3_4]
X' = [2_3 4_5]
X'' = [2_4 6_8]
```

**Bindings are case-sensitive.**

The parser can sometimes mistake all-lowercase bindings names for unformatted built-in functions.

Here, the parser thinks that `part` is [partition]().

```uiua help(Run to format and reveal why this does not work)
part = 5
```

In general, binding names should be [PascalCase](https://en.wikipedia.org/wiki/Camel_case) (also known as upper CamelCase) to avoid this issue.

```uiua
Part = 5
*2 Part
```

Bindings run and bind the code to the right of the `←`. Note, though, that an empty right side is perfectly valid! This means you can bind values that were created on previous lines.

```uiua
×6 7
Answer ←
[Answer]
```

## Binding Functions

The the code on the right side of the `←` takes more that 0 arguments, then instead of evaluating its right side immediately, the right side will be bound as a function.

This is how you make named functions in Uiua.

```uiua
F ← +1
F 5
```

```uiua
Cube ← ˙(××)
Cube 6
```

```uiua
👋 ← ⊂"Hello, "
👋 "World!"
```

If the code on the right side takes 0 arguments but you still want it to be a function, it must be surrounded by `()`s.

Notice how the first example here gives the same value every time, while the second one does not.

```uiua
F ← ⚂
F F F
```
```uiua
F ← (⚂)
F F F
```
