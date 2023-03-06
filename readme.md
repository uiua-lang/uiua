# Description

Uiua (pronounced *"wee-wuh"*) is a simple programming language that borrows a bit from both the functional and array-based programming paradigms. It features rank-polymorphic arrays, partial function application, and dynamic typing.

# Installation

Rust is required to build Uiua. Simply run:
```
cargo install uiua
```

# Introduction

Just to get a taste, here is a simple recursive fibonacci function:
```rust
fn fib n:
    if n < 2 then n
    else fib (n - 1) + fib (n - 2);

do println (fib 10);
```

Uiua's syntax is whitespace-insensitive.

Code is compiled to bytecode and then executed by a virtual machine.

## Types and Values

Uiua has 9 primitive types:
- Unit - the null type
- Byte - 8-bit unsigned integer
- Int - 64-bit signed integer
- Real - 64-bit floating point number
- Char - 32-bit unicode character
- String - UTF-8 string
- Function - a function, closure, or partial function
- List - a list with fast insertion at both ends and fast cloning
- Array - a rank-polymorphic array

Values in Uiua can have any type and are always immutable.

## Functional Programming

Uiua supports functional programming features including partial function application, currying, and higher-order functions.

However, Uiua does not care about the purity of function. Any function may or may not have side effects.

### Simple function declaration:
```rust
fn add a b: a + b;

do print <| add 1 2; // prints 3
```
### Anonymous functions:
```rust
let x = fn x:
    do println x;
    x + 1;

let y = x 5; // prints 5, sets y to 6

let numbers = {1, 2, 3, 4, 5}; // a list
let sum = numbers |> fold 0 (+);
```

## Strings

Strings are UTF-8 encoded and are immutable.

They are delimited by double quotes (`"`).

```rust
do println "Hello, world!";
```
Combining strings is done by using format strings.

These are strings that start with a `$` before the first `"` and contain `{}` placeholders.

Format strings evaluate to a function that takes the values to be inserted into the placeholders.

```rust
let say_hello = $"Hello, {}!";
do println <| say_hello "World";

let everyone = {"Alice", "Bob", "Charlie"};
do everyone |> each say_hello |> each println;
```

## Arrays

Arrays in Uiua are rank-polymorphic, which means that many operations automatically apply to every item, even for multidimensional arrays.

```rust
do println <| [1, 2, 3] * 2; // [2, 4, 6]
```
Arrays (and lists) can be indexed by using an index as a function.
```rust
let numbers = [1, 2, 3, 4, 5];
do println <| 2 numbers; // 3
do println <| numbers |> 2; // also 3

let matrix = [
    [1, 2, 3], 
    [4, 5, 6], 
    [7, 8, 9],
];
do println <| matrix |> 1 |> 2; // 6
do println <| (2 . 1) matrix; // also 6
```
The `each` function applies a function to an array, but one rank down.

For example, if you want to get the middle row of a 2D array, you only have to index it:
```rust
let matrix = [
    [1, 2, 3], 
    [4, 5, 6], 
    [7, 8, 9],
];

do println <| 1 matrix; // [4, 5, 6]
```
However, if you wanted to get the middle *column*, you would have to use `each`:
```rust
do println <| each 1 matrix; // [2, 5, 8]
```