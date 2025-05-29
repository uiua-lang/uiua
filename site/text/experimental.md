## Data Definitions

[Data definitions](/tutorial/datadefs) have a few experimental features.

### Data Functions

If the fields of a data definition are immediately follwed by some code, the data definition becomes a *data function*. Data functions have a `Call` function defined which is invoked instead of the normal `New` constructor when the data definition's name is used as a function.

The normal constructor is called, then the constructed data is passed to the function.

```uiua
# Experimental!
~Person {Name Surname ← ""} $"_ _"⊃(Name|Surname)
Person "Dave"
```

But if the function is called immediately, how do we set `Surname`?

In this case `Surname` is an *optional argument*. We can set optional arguments with their name followed by a `:` before calling the function.

```uiua
# Experimental!
~Person {Name Surname ← ""} $"_ _"⊃(Name|Surname)
Person "Dave" Surname:"Daveson"
```

The argument setter is just a `|1.0` function. Setters for multiple optional arguments can occur in any order.

```uiua
# Experimental!
~F {A ← 0|B ← 0|C ← 0|D ← 0} ∘
F D:1 ⊓A:C: 2 B:3 4
```

We can bundle the function's arguments before calling it with the data definition's normal `New` constructor. The bundled arguments can then be used with the `Args` function. Both of these functions also accept optional arguments.

```uiua
# Experimental!
~Run {A ← 0|B ← 1|C} $"A:_ B:_ C:_"⊃(A|B|C)
Run 2
Run~New A:2 5
Run~Args B:8 .
```

If some argument patterns are common, we can give them names by putting the data function in a module and adding more functions (or even more data functions) to it.

```uiua
# Experimental!
┌─╴Range
  ~ {Min ← 0|Max Inclusive ← 0} ⍜-⇡ ⊃Min(+⊃Max Inclusive)
  ~Incl {Min ← 0} Call Inclusive:1 Min:Min
  APL ← Call Inclusive:1 Min:1
└─╴
Range 5
Range~APL 5
Range Min:4 10
Range~Incl Min:4 10
```

Optional arguments *must* be used by the first named call (including primitives without glyphs).

```uiua should fail
# Experimental!
~F {A ← 0|B ← 0} ∘
A:5
```

```uiua should fail
# Experimental!
~F {A ← 0|B ← 2} ∘
F C:6
```

Optional arguments can be set from scopes lower than the function that uses them, but the opposite is not true.

```uiua
# Experimental!
~F {A ← 0|B ← 0} ∘
F (A:5)
```

```uiua should fail
# Experimental!
~F {A ← 0|B} ≡°□
≡F A:5 ⇡3
```

To set an optional argument for a function inside a modifier, set the argument inside the modifier itself.

```uiua
# Experimental!
~F {A ← 0|B} ≡°□
≡(F A:) 5 ⇡3
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

To solve this, we can prefix function pack with a `↓` symbol, which formats from `|,`. See how it changes the flow of the pack:

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

## [derivative](/docs/derivative) and [integral](/docs/integral)

These modifiers transform a mathematical expression.

Currently, only polynomials are supported.

```uiua
# Experimental!
∂(×.) 5                 # x² → 2x
∂√ 1/9                  # √x → 1/(2√x)
∂(-4+⊃(ⁿ2|×¯2)) [0 1 2] # x² - 2x - 4  →  2x² - 2x
```

```uiua
# Experimental!
∫(×.) 3   # x² → x³/3
∫√ 1      # √x → (2x^1.5)/3
∫(+5×2) 2 # 2x + 5  →  x² + 5x
```