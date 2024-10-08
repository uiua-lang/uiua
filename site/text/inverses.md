# Inverses

Uiua has two modifiers, [un](/tutorial/un) and [under](/tutorial/under), which work with *inverses*. The inverse of a function is a function that conceptually "undoes" it.

Working with inverses is a fundamental part of writing Uiua code. It is an elegant mechanism that captures many different patterns.

## [un](/tutorial/un)

The [un](/tutorial/un) modifier inverts the behavior of a function.

```uiua
°(+1) 5
```

```uiua
°⊟ [1 2]
```

```uiua
°∿ 1
```

As discussed [previously](/tutorial/arrays#array-model), [un](/tutorial/un)[box](/tutorial/box) removes an array from a box.

```uiua
°□ ⊢{"unbox" "me!"}
```

One interesting use of [un](/tutorial/un) is to put an array's rows onto the stack by [un](/tutorial/un)ing stack array notation with [dip](/tutorial/dip) and [identity](/tutorial/identity). The number of rows in the array must match though!

```uiua
[⊙⊙∘] 1 2 3
```

```uiua
°[⊙⊙∘] [1 2 3]
```

```uiua should fail
°[⊙⊙⊙∘] [1 2 3]
```

[un](/tutorial/un)ing box array notation will unbox the items.

```uiua
°[⊙⊙∘] {1 2_3 "hmmm"}
```

```uiua
°{⊙⊙∘} {1 2_3 "hmmm"}
```

You can find more uses of [un](/tutorial/un) in its documentation, including a list of all [un](/tutorial/un)-compatible functions and modifiers.

## [under](/tutorial/under)

[under](/tutorial/under) expresses a more powerful inversion pattern. It captures the pattern of doing some transformation, modifying the data, then undoing the transformation.

This may not seem immediately useful, but you'll find it is a pattern you encounter everywhere, even in your everyday life. You might open a drawer, take something out, then close the drawer. You might get on a bus, the bus travels, then you get off the bus.

[under](/tutorial/under) takes two functions which we will call `F` and `G`. It calls `F`, then calls `G`, then calls an inverse of `F`.

Many functions that do not work with [un](/tutorial/un) work with [under](/tutorial/under) because [under](/tutorial/under) can keep track of *context*. One example of this in action is [under](/tutorial/under)[pick](/tutorial/pick), which allows us to modify an element or row of an array.

```uiua
⍜(⊡2|×10) [1 2 3 4]
```

This code picks out item `2` of the array, multiplies it by `10`, then puts it back in the array.

If the values passed to [under](/tutorial/under)'s functions are not constants, they can also be put outside, albeit in a different order.

```uiua
⍜⊡× 2 [1 2 3 4] 10
```

This works because [under](/tutorial/under) keeps track of the original array and passes it to the inversion of [pick](/tutorial/pick).

If you wanted to set a value in an array rather than modifying it, you could use [pop](/tutorial/pop) or [gap](/tutorial/gap) instead of [mul](/tutorial/mul).

```uiua
⍜(⊡2)⋅∞ [1 2 3 4]
⍜⊡◌ 2 [1 2 3 4] ∞
```

It's not just [pick](/tutorial/pick)! Many functions work with [under](/tutorial/under)!

```uiua
⍜(↙2)/× [3 5 4 2]
```

```uiua
⍜(↻3|⊂0) [1 2 3 4 5]
```

```uiua
⍜×⁅ 1e3 π
```

```uiua
.↯3_4⇡12
⍜♭⇌
```

You can even use [under](/tutorial/under) on a function that has already been [un](/tutorial/un)ed. This is a nice way to work with [box](/tutorial/box)ed data.

```uiua
≡⍜°□(⊂:@!) {"wow" "cool" "omg"}
```

Let's say you wanted to utilize a struct-like pattern. Uiua does not have structs or objects with fields like many other languages do, but you can simulate them with box arrays. This can be slow, so you should not do this with any data that needs to be accessed in tight loops.

[under](/tutorial/under) allows a field getter to also be a setter!

```uiua
Person ← {⊙⊙∘}
Name ← °□⊡0
Surname ← °□⊡1
Age ← °□⊡2

FmtPerson ← $"_ is _ years old" ⊃(Name|Age)
PassYear ← ⍜Age(+1)

Dan ← Person "Dan" "Danson" 31
FmtPerson Dan
FmtPerson PassYear Dan
```

You can find more uses of [under](/tutorial/under) in its documentation, including a list of all [under](/tutorial/under)-compatible functions and modifiers.

## Setting Inverses

Many functions, especially more complex ones, do not have well-defined inverses. However, you can use the [setinv](/tutorial/setinv) and [setund](/tutorial/setund) modifiers to define them yourself.

[setinv](/tutorial/setinv) sets a simple inverse that is compatible with [un](/tutorial/un).

For example, [first](/tutorial/first) does not have an [un](/tutorial/un)-compatible inverse, but we can define one.

```uiua
MyFirst ← setinv(⊢|[∘])
MyFirst [1 2 3]
°MyFirst 5
```

This inverse is also compatible with [under](/tutorial/under).

```uiua
MyFirst ← setinv⊢[∘]
⍜⊢(×10) [2 3 4]
⍜MyFirst(×10) [2 3 4]
```

Inverses should have the opposite signature of the function they are the inverse of. If it does not, you will get a warning.

```uiua should fail
F = setinv+-
```

[setund](/tutorial/setund) is more complicated. See its documentation for how to use it.

[setinv](/tutorial/setinv) and [setund](/tutorial/setund) can be nested so that an inverse can be fully defined in all cases.

This example shows how the different inverses get called.

```uiua should fail
F ← setund(setinv("normal"|"inverse")|"do"|"undo")
F
°F
{⍜F"G"}
```
