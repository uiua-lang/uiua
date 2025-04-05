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

## Inline Macros

Following an inline function's `()`s with one or more `!`s will make it an inline index macro.
This allows `^` placeholders to be used inside the function.

```uiua
# Experimental!
(^0^0)!↯ 2 3 4
```

```uiua
# Experimental!
StdDev ← √(^0^1^0)‼(÷⧻⟜/+|×.-).
StdDev [1 2 3 4]
```

An inline code macro can be specified by putting a `^` between the `)` and the first `!`.

```uiua
# Experimental!
(⇌)^‼(⊂1|⊂2) []
```

```uiua
# Experimental!
($"_ ← 5"⊢)^!X
X
```

```uiua
# Experimental!
(⋅⊢)^!+
(⋅⊢)^!⊓+¯
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

## Data Definitions

[Data definitions](/tutorial/datadefs) have a few experimental features.

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

If some code immediately follows a data definition, a `Call` function will be generated in which the field names will be mapped to the arguments.

This is called a *data function* and essentially allows for named function arguments.

```uiua
# Experimental!
~MyData {Foo Bar} ↯2 Foo_Foo_Bar
MyData 3 5
```

You can mix and match accessed fields and normal function inputs. Values at the top of the stack will be bound first.

```uiua
# Experimental!
~Foo [x] -x
Foo 3 5
```

```uiua
# Experimental!
~Quad [a b c] ÷×2a -b ⊟¯.√ℂ0 -/×4_a_c ×.b
Quad 1 ¯3 2
```

Note that in general, functions should not be written this way. Keeping an array as local value means it will be duplicated if it is mutated, which is inefficient.

Data functions are mainly useful when your function has a lot of configuration parameters. Arrays that are the primary thing being transformed, as well as arrays that are potentially large, should be kept on the stack.

This concept can be extended to *methods*. Methods are specified within a module that has a data definition already defined. The method is defined in the same way as a normal function, but with a `~` before the name.

When a method is called, a data array is bound as a sort of local variable. Refering to the data definition's fields will pull them from the bound array.

```uiua
# Experimental!
┌─╴Foo
  ~{Bar Baz}
  ~Sum ← +Bar Baz
└─╴
Foo~Sum Foo 3 5
```

Within the body of a method, the bound array can be updated with [un](/docs/un) or [under](/docs/under). The entire bound array can be retrieved via an implicit `Self` binding. The bound array is not returned from the method by default, so `Self` can be used to retrieve it.

Note that the array to be bound in the method is passed *below* any additional arguments. So in the example below, `10` is passed to `AddToBar` *above* the `Foo` array.

```uiua
# Experimental!
┌─╴Foo
  ~{Bar Baz}
  ~Sum      ← +Bar Baz
  ~AddToBar ← Self ⍜Bar+
└─╴
Foo~AddToBar 10 Foo 3 5
Foo~Sum .
```

If one method is referenced from another, it will access the same bound array.

```uiua
# Experimental!
┌─╴Foo
  ~{Bar Baz}
  ~AddBar ← +Bar
  ~Add    ← AddBar Baz
└─╴
Foo~Add Foo 3 5
```

If you want to access the normal getter function for a field, instead of the local-retrieving one, you disambiguate with the name of the module.

```uiua
# Experimental!
┌─╴Foo
  ~{Bar Baz}
  # Demonstrative. Don't do this.
  ~Add ← Foo ⊃(+Bar Foo~Bar|+Baz Foo~Baz)
└─╴
Foo~Add Foo 20 10 Foo 3 5
```