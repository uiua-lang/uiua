## Custom Subscript Functions

Functions can be given custom numeric subscript behavior by ending their name with a `ₙ` (which formats from `,n`).

`ₙ` can then be used inside the function anywhere subscripts are permitted.

```uiua
# Experimental!
Fₙ ← ∩ₙ¯
[F₁ 1 2 3]
[F₂ 1 2 3]
[F₃ 1 2 3]
```

```uiua
# Experimental!
Gₙ ← ⊂ₙ ×ₙ
G₂ 10 5
G₃ 10 11 12
```

You can get the subscript as a constant anywhere in the function using a `^n` placeholder.

```uiua
# Experimental!
Gₙ ← [1 ^n 3]
G₂
G₋₅
```

## Locals

By ending an identifier with a `:`, it becomes a _local binding_. Local bindings take a single argument and make it usable elsewhere in your code simply by referring to it by name. These are different from normal bindings in that they can binds arguments even inside functions.

```uiua
# Experimental!
F ← [A 1 A 2 A] A:
F 0
F 4
```

You can bind multiple locals in order with [bracket](/docs/bracket)

```uiua
# Experimental!
⊓A:B: 1 2
[A B B A]
⊓(A:|B:|C:) 1 2 3
[A C A B]
```

Additionally, locals can be bound inside but used outside any modifier that calls its functions exactly once.

```uiua
# Experimental!
Foo [⊙Foo: 1 2 3]
[X Y] ⊃(Y:+1|X:) 5
```

For modifiers that can execute their function a variable number of times, like [rows](/docs/rows) or [repeat](/docs/repeat), locals bound inside can only be used inside. This is so that a bound local is always guarunteed to be bound before being referenced.

```uiua
# Experimental!
⍚(↯A_A B ⊓A:B:) [1 2 3] [4 5 6]
```

```uiua should fail
# Experimental!
X ≡X: [1 2 3]
```

Locals are not exported from modules like bindings are.

```uiua should fail
# Experimental!
┌─╴Mod
  A: 10
└─╴
Mod.A
```

The above examples show some basics, but they are probably not good reasons to use locals. In general, locals are meant for cases where threading a value through code in a pure tacit way becomes a bigger burden than it's worth. Whether such non-tacit facilities belong in Uiua remains to be seen.

If they are ever stabilized, locals will replace [Data Functions](https://www.uiua.org/tutorial/Data%20Definitions#data-functions), as they achieve something very similar.

## Type Checking

The Uiua compiler can do limited compile-time analysis of how the scalar types and shapes of arrays change at compile time.

_Note: Uiua type checking is and always will be best-effort. Uiua is a highly dynamic language, so not everything can be type checked completely._

By putting a `#?` comment above or at the end of a function definition, you can make the compiler check the scalar types and shapes of arguments and outputs of the function, based on the function's body.

The formatter will insert a representation of this type signature after the `#?`. The output types are to the left of the `?` and the argument types are to the right.

```uiua
# Experimental!
#? Try formatting
F ← ↙5
```

The `…` syntax on its own represents an array of any/unknown scalar type and any/unknown shape. `5×…` indicates an array with a leading axis of `5` and unknown additional dimensions.

If we use a function such as [reshape](/docs/reshape) to ensure the shape, the output shape will be more refined.

```uiua
# Experimental!
#? 5×12 ? …
F ← ↙5 ↯10_12
```

```uiua
# Experimental!
#? …×2 ? … …
F ← ⍉⊟
```

This system can catch some errors at compile time. They are currently emitted as warnings.

```uiua should diag
# Experimental!
#?
F ← ↙10↯3_4
```

Attempting to call the function will fail at runtime.

```uiua should fail
# Experimental!
#?
F ← ⊡3⊟
F 1 2
```

The `ℝ` and `ℂ` symbols in this example indicate that those arrays are of numbers and complexes respectively.

```uiua
# Experimental!
#? 3×…ℂ ? …ℝ …ℝ
F ← ↯3 ℂ
```

```uiua
# Experimental!
#? _□str ? str
F ← ⊜□⊸≠@\n &fras
```

This system is meant to often be used in conjunction with [validate](/docs/validate), which can more explicitly verify type/shape information. See its documentation for detail on how to use it.

```uiua should diag
# Experimental!
# Type check!
F ← ↙5 ↯3
G ← +@0 ⊨{𝕌}
```

Data definitions have a built-in item called `t`, which is a [validate](/docs/validate)-compatible type specification. `t` currently only exists in `# Experimental!` contexts.

```uiua
# Experimental!
~Foo {A B}
Foo.t
```

Data definition field initializers can inform the type system.

```uiua
# Experimental!
~Foo {A ← ⊨{𝕌∞}|B ← ⊨ℝ}
Foo.t
```

The type system implementation is such that most type information can only flow _forward_ through the system, not backward. Except for at the very beginning of a function, type constants cannot go backward to inform the argument types of the function.

## Data Definitions

[Data definitions](</tutorial/Data Definitions>) have a few experimental features.

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
°⊸MyData.Bar 5
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

Notice that even though [divide](/docs/divide) is on the last line of the pack, its result (`2`) is _under_ the results of all the other functions. This is because the collapsed version of this code looks like this:

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

This kind of function pack is said to be _lexically ordered_.

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

This simple example folds while the number is `<` `10`. Note that in most cases, it is likely faster to simply filter the array first and do a [reduce](/docs/reduce)

```uiua
# Experimental!
∧(+|<10) ⊙0 [1 2 3 10 4 5]
```

This example folds while the _sum_ is `<` `10`. The only other way to do this is with [do](/docs/do) with manual accumulation.

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
