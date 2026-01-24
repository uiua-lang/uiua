## Data Definitions

[Data definitions](</tutorial/Data Definitions>) have a few experimental features.

### Data Functions

If the fields of a data definition are immediately followed by some code, the data definition becomes a *data function*. Data functions have a `Call` function defined which is invoked instead of the normal `New` constructor when the data definition's name is used as a function.

The normal constructor is called, then the constructed data is passed to the function.

```uiua
# Experimental!
~Person {Name Surname ← ""} $"_ _"⊃(Name|Surname)
Person "Dave"
```

But if the function is called immediately, how do we set `Surname`?

In this case `Surname` is an *optional argument*. When the data function's name is used as a macro, we can set optional arguments within the macro's function. We can set optional arguments with the [un](/docs/un) [by](/docs/by) idiom followed by their name.

```uiua
# Experimental!
~Person {Name Surname ← ""} $"_ _"⊃(Name|Surname)
Person!°⊸Surname "Daveson" "Dave"
```

Setters for multiple optional arguments can occur in any order.

```uiua
# Experimental!
~F {A ← 0|B ← 0|C ← 0|D ← 0|E} ∘
F!(°⊸D1 °⊸A4 °⊸C2 °⊸B3) 5
```

---

### Validators

You can add validation functions to a field. This function will be called both upon construction (after the initializer) and upon mutation.

The function should come after the name and a `:`, but before the initializer.

A common use case for this is to validate the type of a field.

```uiua should fail
# Experimental!
~MyData {Foo: °0type|Bar: °1type}
MyData 1 "hi" # Works
MyData 3 5    # Fails
```

```uiua should fail
# Experimental!
~MyData {Foo: °0type|Bar: °1type}
MyData 1 "hi"
°⊸MyData~Bar 5
```

## Lexical Ordering

Consider this example:

```uiua
3 6
⊃(+
| -
| ×
| ÷
)
```
Notice that even though [divide](/docs/divide) is on the last line of the pack, its result (`2`) is *under* the results of all the other functions. This is because the collapsed version of this code looks like this:

```uiua
⊃(+|-|×|÷) 3 6
```

Those need to do the same thing for consistency reasons, but the first example seems backwards! The function that is further down in the actual source runs first, so to read in execution order, we have to read from the bottom up.

To solve this, we can prefix the function pack with a `↓` symbol, which formats from `|,`. See how it changes the flow of the pack:

```uiua
# Experimental!
3 6
⊃↓(
  +
| -
| ×
| ÷
)
```
Now the result of `÷` is at the top of the stack! Each function in the pack lines up with its result in the output.

Note that collapsing this code actually changes its behavior.

```uiua
# Experimental!
⊃↓(+|-|×|÷) 3 6
```

This is because a pack with a `↓` ignores syntax tree ordering and only considers the layout of the code in the actual source.

This kind of function pack is said to be *lexically ordered*.

The lexical ordering symbol `↓` can also be used on stack array notation to make the lines run in the normal top-down order instead of bottom-up.

```uiua
# Experimental!
[1 2
 3 4]
```

```uiua
# Experimental!
↓[1 2
  3 4]
```

## [fold](/docs/fold) Function Packs

Using [fold](/docs/fold) with a function pack of 2 functions acts like a cross between [fold](/docs/fold) and [do](/docs/do).

The second function is a condition check. If it returns false, iteration ends early.

This simple example folds while the number is `<` `10` is encountered. Note that in most cases, it is likely faster to simply filter the array first and do a [reduce](/docs/reduce)
```uiua
# Experimental!
∧(+|<10) ⊙0 [1 2 3 10 4 5]
```
This example folds while the *sum* is `<` `10`. The only other way to do this is with [do](/docs/do) with manual accumulation.
```uiua
# Experimental!
∧(+|<⋅10) ⊙0 [1 2 3 10 4 5]
```

## [derivative](/docs/derivative) and [integral](/docs/integral)

These modifiers transform a mathematical expression.

Currently, only polynomials are supported.

```uiua
# Experimental!
∂˙× 5                 # x² → 2x
∂√ 1/9                  # √x → 1/(2√x)
∂(-4+⊃(ⁿ2|×¯2)) [0 1 2] # x² - 2x - 4  →  2x² - 2x
```

```uiua
# Experimental!
∫˙× 3   # x² → x³/3
∫√ 1      # √x → (2x^1.5)/3
∫(+5×2) 2 # 2x + 5  →  x² + 5x
```