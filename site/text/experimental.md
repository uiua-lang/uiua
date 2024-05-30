# Experimental Features

Uiua has a number of features that are considered experimental. They are available in the interpreter for testing, but may be removed or changed in the future.

Using experimental features requires an `# Experimental!` comment to be placed at the top of a Uiua source file.

## Experimental Functions/Modifiers

- [`coordinate`]()
- [`stringify`]()
- [`&ffi`]()
- [`&memcpy`]()
- [`&memfree`]()
- [`&tlsl`]()

## Stack Swizzles

Stack swizzles allow you to reorder the stack in a concise way.
They are written with a `λ` followed by some letters.
The `λ` will format from `'` if followed by letters.

```uiua
# Experimental!
[λcba 1 2 3 4 5]
[λbbbbba 1 2]
[λccaabb 1 2 3]
```

Captial letters [`fix`]() the corresponding value.

```uiua
# Experimental!
{λaCb 1_2_3 4_5_6 7_8_9}
```

## Array Swizzles

Array swizzles allow you to extract rows from and array in a concise way.
They are written with a `⋊` followed by some letters.
The `⋊` will format from `''` if followed by letters.
Letters up to `m` start from the first row, Letters back from `z` start from the last row.

```uiua
# Experimental!
[⋊az] [1 2 3 4 5]
[⋊acb] [1 2 3]
```

Capital letters `un` `box` the corresponding value.

```uiua
# Experimental!
⋊CbBa {1 2_3 "hi!"}
```

## Labels

Labels are a way to give names to values. This is to aid in readability and debugging.

Labels are written with a `$` followed by some letters.

```uiua
# Experimental!
$foo 1 $bar [2 3 4]
$baz "hi!"
```

Labeled values put in an array will lose their labels unless they are [`box`]()ed.

```uiua
# Experimental!
[$a 1 $b 2 $c 3]
{$a 1 $b 2 $c 3}
```

Labels cannot be inspected by code.
