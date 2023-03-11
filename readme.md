# Description

Uiua (pronounced *"wee-wuh"*) is a simple programming language that borrows a bit from both the functional and array-based programming paradigms. It features rank-polymorphic arrays, partial function application, and dynamic typing.

# Installation

Rust is required to build Uiua. Simply run:
```
cargo install uiua
```

# Introduction

Just to get a taste, here is a program that shows all the even numbers between 1 and 20:
```cs
let evens = (mod 2 _ = 0) / filter <> 1 + range _
do show <| evens 20
```

Uiua's syntax is newline-sensitive, but all other whitespace is ignored.

Code is compiled to bytecode and then executed by a virtual machine.

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

All functions in Uiua are curried and can be partially applied.

Calling a function with too few arguments returns a new function that takes the remaining arguments.

### Arrays

Arrays in Uiua are multidimensional and rank-polymorphic, meaning that many operations automatically apply to every item.
Other operations allow the traversal of different axes.

## Functional Programming

Uiua supports functional programming features including partial function application, currying, and higher-order functions.

However, Uiua does not care about the purity of function. Any function may or may not have side effects.

### Simple function declaration:
```cs
let add a b = a + b

do show <| add 1 2 # prints 3
```
### Anonymous functions:
```cs
let x = |x
    do show x
    x + 1

let y = x 5 # prints 5, sets y to 6

let numbers = {1, 2, 3, 4, 5} # a list
let sum = numbers |> fold (+) 0
```

## Strings

Strings are UTF-8 encoded and are immutable.

They are delimited by double quotes (`"`).

```cs
do show "Hello, world!"
```
Combining strings is done by using format strings.

These are strings that start with a `$` before the first `"` and contain `{}` placeholders.

Format strings evaluate to a function that takes the values to be inserted into the placeholders.

```cs
let say_hello = $"Hello, {}!"
do show <| say_hello "World"

let everyone = {"Alice", "Bob", "Charlie"}
do everyone |> each say_hello |> each println
```

## Arrays

Arrays in Uiua are rank-polymorphic, which means that many operations automatically apply to every item, even for multidimensional arrays.

```cs
do show <| [1, 2, 3] * 2 # [2, 4, 6]
```
Arrays (and lists) can be indexed with the `pick` function.
```cs
let numbers = [1, 2, 3, 4, 5]
do show <| pick 2 numbers # 3
do show <| numbers |> pick 2 # also 3

let matrix = [
    [1, 2, 3], 
    [4, 5, 6], 
    [7, 8, 9],
]
do show <| matrix |> pick [1, 2] # 6
```
The `cells` function applies a function to an array, but one rank down.

For example, if you want to get the middle row of a 2D array, you only have to index it:
```cs
let matrix = [
    [1, 2, 3], 
    [4, 5, 6], 
    [7, 8, 9],
]

do show <| pick 1 matrix # [4, 5, 6]
```
However, if you wanted to get the middle *column*, you would have to use `cells`:
```cs
do show <| cells (pick 1) matrix # [2, 5, 8]
```