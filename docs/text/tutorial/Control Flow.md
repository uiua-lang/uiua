# Control Flow

Uiua, and array languages in general, require much less control flow than other programming languages. Most operations that would be loops in other languages are simply operations on arrays. Because boolean operations return numbers, a lot of checks that would be done with `if` statements in other languages become mathematical or indexing operations in arrays languages.

For example, if you wanted to split an array of numbers into an array of odds and an array of evens, you might do it like this in a language like Python:

```python
def splitArray(array):
    even = []
    odd = []
    for i in array:
        if i % 2 == 0:
            even.append(i)
        else:
            odd.append(i)
    return even, odd

splitArray([1, 2, 3, 7, 2, 4, 5])
```

In Uiua, it is much simpler, and there are no `if`s or `for`s to be found:

```uiua
F ← ∩⌟▽⊸¬⊸◿2
F [1 2 3 7 2 4 5]
```

That being said, not every problem lends itself to array operations. Uiua has a few methods for handling such cases.

## Looping with [repeat]() and [do]()

The [repeat]() modifier takes a number and calls its function that many times.

Here, we [multiply]() a number by `2`, `10` times.

```uiua
⍥(×2)10 5
```

```uiua
⍥/+2 ↯3_3⇡9
```

If the function has more outputs than arguments, the extra outputs are collected into arrays.

[repeat]()[random]() is a common pattern for generating a list of random numbers.

```uiua
⁅⍥⚂5
```

[repeat]() is also useful for conditionally calling a function. Because booleans in Uiua are just numbers, [repeat]()ing with a boolean value will call a function `0` or `1` times.

```uiua
F ← ⍥(×10)⊸<10
F 5
F 12
```

[repeat]()'s glyph is a combination of a circle, representing a loop, and the 𝄇 symbol from musical notation.

The [do]() modifier takes a loop function and a condition function. It repeatedly calls the loop function as long as the condition function returns `1`.

```uiua
⍢(×2|<1000) 1
```

You sometimes need [pop]() to clean up state.

```uiua
◌⍢(⊃(×2)⊂|<100) 1 []
```

While [do]() is very powerful, it should only be used when necessary, as it is almost always faster to use array operations.

[do]() is the only way to do an infinite loop in Uiua. To do so, simply use `1` as the condition function.

## Catching errors with [try]()

The [try]() modifier takes two functions. If the first function throws an error, the second function is called to handle it.

We can see how this works by using it with [parse](), which attempts to parse a string into a number.

If the handler function has 0 arguments, then it is simply called. This is a nice way to provide default values in the event of a failure.

```uiua
⍣⋕0 "5"
⍣⋕0 "dog"
```

If the handler function has 1 argument, then the original argument is passed to it.

```uiua
⍣⋕∘ "5"
⍣⋕∘ "dog"
```

If the handler function takes 1 more argument than the first function, then the error is also passed to it as a string.

```uiua
⍣⋕□₂ "5"
⍣⋕□₂ "dog"
```

You can read about more uses of [try]() in its documentation.

## [switch]()

The [switch]() modifier uses a selector to choose one of its functions to call. The selector is always the first argument.

```uiua
⨬+× 0 3 5
⨬+× 1 3 5
```

Non-scalar selectors are allowed. They allow a different function to be evaluated for each of the input array(s).

```uiua
⨬+- [1 0 1] [1 2 3] [4 5 6]
```

[switch]() can use a [function pack](</tutorial/More Argument Manipulation#function-packs>) to select from more functions.

```uiua
⨬(+|-|×|÷) [1 2 0 3] ↯4 2 ↯4 5
```

```uiua
⨬(×10|+1|⨬¯∘ ⊸=2) ⊸◿3 [2 9 4 0 8 3]
```

With [indexin](), [switch]() can be used to implement behavior similar to `switch` statements in other languages.

```uiua
F ← (
  ⨂{"foo" "bar" "baz"} □
  ⨬(+1|×10|÷2|¯)
)
F "foo" 5
F "bar" 5
F "baz" 5
F "wow" 5
```

Each branch can have a signature specified. For the overall [switch]() to have a valid signature, all branches must either have the same argument/output difference *or* return the same number of outputs.

```uiua
F ← ⨬(|2 ×||3.2 ⊃(++)×)
[F 0 2 3 4]
[F 1 2 3 4]
```

Signatures in [switch]() functions are bit messy, so try to avoid them when possible.

## Recursion

A bound function that refers to its own name is a [recursive function](https://en.wikipedia.org/wiki/Recursion_(computer_science)). A function that calls itself can easily recurse infinitely, so it is important to have a *base case* that stops the recursion when a condition is met. [switch]() is great for this.

As a simple example, here is a function that calculates the factorial of a number. Note that you should not actually do this, as `/×⇡₁` is shorter, faster, and more idiomatic.

```uiua
Fact ← |1 ⨬(×Fact⊸-1|1)⊸<2
Fact 5
```

The base case is when the input is `1`. In this case, the function returns `1`. Otherwise, it multiplies the input by the result of calling itself with the input decremented by `1`.

Recursive functions are required to have signatures declared.

Recursion is only recommended if a particular problem *really* calls for it. Recursion in Uiua can be slow, and there is a limit to how deep you can recur.

It is usually better to use either array-based methods or iterations with [repeat]() or [do]().

## [assert]()

The [assert]() function takes any value and a condition. If the condition is anything but `1`, the value is thrown as an error that can be caught with [try]().

```uiua
F ← ⍣(¯⍤10⊸≤10)⋅∘
F 5
F 12
```

If the [assert]()ed value is never caught, it becomes an error.

```uiua should fail
F ← ¯⍤"Too big!"⊸≤10
F 5
F 12
```

The error message above may not be useful to the user, as it refers to the code of the function itself. You can use the `# Track caller!` semantic comment to refer to the call site instead.

```uiua should fail
F ← ¯⍤"Too big!"⊸≤10 # Track caller!
F 5
F 12
```

You can read more about `# Track caller!` [here](/tutorial/documentation#track-caller).

Using [assert]() for this purpose will be covered more in the [section on testing](/tutorial/Testing).

## Challenges

```challenge
pushes "small" if a number is less than 10, "medium" if it is less than 100, and "large" otherwise
⨬("small"|"medium"|"large")/+≥[10 100]

17
3
50
2357
10
```

```challenge
multiplies an array by its reverse until any element is greater than 1000
⍢(×⊸⇌|≤1000/↥)

[1.5 8 2]
[1 2 3]
[¯6 5 1]
```