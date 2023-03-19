[https://kaikalii.github.io/uiua/]

# Description

Uiua (pronounced *"wee-wuh"*) is a stack-oriented array programming language.

# Installation

Rust is required to build Uiua. Simply run:
```
cargo install uiua
```

# Introduction

In Uiua, a function or expression is made of *words*. 
A word can push or pop any number of values to and from the program stack.

Just to get a taste, here is a program that shows all the even numbers between 1 and 10:
```
Evens = ⌗=0◿2.
show evens +1𝆱10
```
A quick breakdown:
- `Evens = ` defines a function because is capitalized.
- Operations are performed right-to-left.
- `.` is the duplicate operator. It duplicates the top item on the stack.
- `2` pushes the number `2` to the stack.
- `◿` is the modulus operator. Here, `◿2` is applied to every item in the array.
- `0` pushes the number `0` to the stack.
- `=` compares two values for equality. `=0` is also applied to every item in the array.
- `⌗` filters its second argument with a mask from its first argument. It is equivalent to APL's replicate`⌿`.
- `10` pushes the number `10` to the stack.
- `𝆱` gets all the natural numbers before its argument.
- `1` pushes the number `1` to the stack.
- `+` adds two values, in this case `1` and `𝆱10`.
- `evens` calls the function. Identifiers are case-insensitive.
- `show` pretty-prints its argument.

Uiua's syntax is newline-sensitive, but all other whitespace is ignored.

Code is compiled to bytecode and then executed by a virtual machine.

## Special Characters

Like most array languages, Uiua uses many special characters.
However, Uiua *can* be written without *any* special characters.
This is actually the main way to write Uiua code.
Once ascii-only code is written, the Uiua formatter can convert certain names and ascii characters to their unicode conterparts.

To format all Uiua code in the current directory, run:
```
uiua fmt
```
Code can also be formatted by simply running it.

This will convert code like
```
show reshape~range/*.2_3_4
```
to
```
show ↯~𝆱/×.2_3_4
```
Primitive names only need to be long enough to disambiguate them, so the ascii code above could also be written as
```
show resh~rang/*.2_3_4
```
and it would still be converted.

## Types and Values

Uiua has only 4 primitive types: numbers, characters, functions, and arrays

Values in Uiua can have any type and are always immutable.

### Numbers

Numbers are double-precision floating-point.

### Characters

Characters are utf-32 encoded.

Characters and numbers exist in an affine space:
Numbers and characters can be added to get characters, and characters can be subtracted to get numbers.
However, characters cannot be added together.

### Functions

Function can be defined in a few ways:
- A capitalized identifier followed by `=` and a body:
```
DoubleThenAdd = +2×
```
- Anywhere else when wrapped in `()`:
```
double = (×2)
```
- In *function arrays*, a special syntax for building lists of functions:
```
(+1|÷2◿10)
```

### Arrays

Arrays in Uiua are multidimensional and rank-polymorphic, meaning that many operations automatically apply to every item.
Other operations allow the traversal of different axes.

Other than through functions (and the *function arrays* syntax above), there are two ways to create arrays.

Strand notation connetcs items with `_`:
```
1_2_3
```
Inside `[]` brackets, arguments are evaluated from righ to left as normal, but at the end, all new values placed on the stack are popped into an array.
```
[1 2 3]
```
With this notation, subarrays attempt to *normalize*, meaning that if all subarrays have the same length, they are turned into a higher-dimensional array.
```
[[1 2 3] [4 5 6]] # a 2x3 array
```
Any normal Uiua code can be put inside `[]` brackets. If you wanted a 3x3 array with rows descending to `[1 2 3]`, you could do
```
[+3.+3.[1 2 3]]
```
This yields
```
┌─
· 7 8 9
· 4 5 6
  1 2 3
        ┘
```
