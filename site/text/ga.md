## Geometric Algebra

Arrays have an experimental scalar type: [multivector]()s. Multivectors are the primary objects operated on in [Geometric Algebra](https://en.wikipedia.org/wiki/Geometric_algebra).

Geometric Algebra unifies many useful mathematical concepts, including complex numbers, quaternions, and linear transformations. This page does not go into detail on all the ins and outs of the topic. [This video](https://youtu.be/60z_hpEAtD8) is a great introduction to Geometric Algebra.

Because multivectors are a superset of complex numbers, in Uiua, they have the same [type]() and are fully compatible.

Here is a quick reference table how various Uiua operators work on multivectors:

| Primitive            | GA Functionality        |
| -------------------- | ----------------------- |
| [multivector]()      | Create multivector      |
| [multiply]()         | Geometric Product       |
| [divide]()           | Scalar division         |
| [add]()              | Multivector addition    |
| [subtract]()         | Multivector subtraction |
| [absolute value]()   | Multivector magnitude   |
| [sign]()             | Normalize multivector   |
| [negate]()`⌟`        | Reverse                 |
| [negate]()`⌞`        | Negative reverse        |
| [negate]()`₄`        | Dual                    |
| [negate]()`₋₄`       | Antidual                |
| [outer product]()    | Outer product           |
| [inner product]()    | Inner product           |
| [inner product]()`⌞` | Left contraction        |
| [inner product]()`⌟` | Right contraction       |

In general, all product operators on multivectors are not commutative, meaning the order of the operands matters. Uiua uses a convention where an operation like `AB` or `A∧B` in normal mathematical notation becomes `×B A` or `⨱B A`, with the "right" argument as the first. This is in line with many other Uiua operators.

### Using Multivectors

While Geometric Algebra has many applications, the most obvious and common use in in representing and manipulation geometric objects. As a motivating we will start by simply rotating a square.

Vectors are a core objects in most geometric algebra spaces, so we will represent the square as a list of coordinates of points that lie on its boundary. We'll start with a simple list of points from `¯1` to `1`. The `wrench` idiom `-⊸¬` maps a range from [0, 1] to [¯1, 1].

```uiua
-⊸¬ ÷⟜⇡₀ 4
```

These are points along a 1-dimensional square, but we need 2. To increase the number of dimenions, we start by combining every number with `¯1` and `1`.

```uiua
-⊸¬ ÷⟜⇡₀ 4
♭₂⊞⊂¯1_1
```

This gives us coordinates along the the top and bottom of the square. Then, we simply append the same list with the rows [reverse]()d to get points for the sides. We [deduplicate]() to remove duplicate corder points.

```uiua
-⊸¬ ÷⟜⇡₀ 4
◴⊂⟜≡⇌♭₂⊞⊂¯1_1
```

To render these points, we can normalize them to positive integer positions and then use [un]() [where]() to draw the points.

```uiua
-⊸¬ ÷⟜⇡₀ 4
◴⊂⟜≡⇌♭₂⊞⊂¯1_1
°⊚⁅×20⧋-⊸/↧
```

Now that we have our list of points, we can convert it into a list of multivectors using [multivector](). Because the last axis of the coordinate list is [length]() 2, the multivectors will be 2D vectors.

```uiua
-⊸¬ ÷⟜⇡₀ 4
𝕍 ◴⊂⟜≡⇌♭₂⊞⊂¯1_1
```

`e₁` and `e₂` are multivector basis equivalent to our 2D X and Y axes.

### GA Cheat Sheet

#### PGA

For first argument A and second argument B:
| Operation                    | Code              |
| ---------------------------- | ----------------- |
| Project A onto B             | `×⤙⨰`             |
| Project B onto A             | `×⟜˜⨰`            |
| Reflect B across A           | `×⟜˜×`            |
| Intersection of A and B      | [outer product]() |
| Smallest superset of A and B | `⍜∩¯₄⨱` (`regr`)  |