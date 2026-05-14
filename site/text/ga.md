## Geometric Algebra

Arrays have an experimental scalar type: [multivector]()s. Multivectors are the primary objects operated on in [Geometric Algebra](https://en.wikipedia.org/wiki/Geometric_algebra).

Geometric Algebra unifies many useful mathematical concepts, including complex numbers, quaternions, and linear transformations. This page does not go into detail on all the ins and outs of the topic. [This video](https://youtu.be/60z_hpEAtD8) is a great introduction to Geometric Algebra.

Because multivectors are a superset of complex numbers, in Uiua, they have the same [type]() and are fully compatible.

### Using Multivectors

While Geometric Algebra has many applications, the most obvious and common use is in representing and manipulation geometric objects. As a motivating example we will start by simply rotating a square.

Vectors are a core objects in most geometric algebra spaces, so we will represent the square as a list of coordinates of points that lie on its boundary. We'll start with a simple list of points from `¯1` to `1`. The `wrench` idiom `-⊸¬` maps a range from [0, 1] to [¯1, 1].

```uiua
-⊸¬ ÷⟜⇡₀ 4
```

These are points along a 1-dimensional square, but we need 2. To increase the number of dimenions, we start by combining every number with `¯1` and `1`.

```uiua
-⊸¬ ÷⟜⇡₀ 4
♭₂⊞⊂¯1_1
```

This gives us coordinates along the the top and bottom of the square. Then, we simply append the same list with the rows [reverse]()d to get points for the sides. We [deduplicate]() to remove duplicate corner points.

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
# Experimental!
-⊸¬ ÷⟜⇡₀ 4
𝕍 ◴⊂⟜≡⇌♭₂⊞⊂¯1_1
```

`e₁` and `e₂` are length-1 multivector equivalent to our 2D X and Y axes.

The rotation will look cool as an animation, so we'll generate a list of angles to rotate by. A square rotated 90° looks the same as an unrotated square, so we only need to generate angles up to a quarter-turn instead of a full one, and the animation will still look smooth. In Uiua, `π/2` is [eta]().

```uiua
×η÷⟜⇡8
```

In geometric algebra, rotations are also represented by multivectors. The formula for a multivector that represents a rotation by angle `θ` is `e^(θ/2 * axis)`. In 2D, we only have 1 axis of rotation, and it is also represented by a multivector, in this case, the *bivector* `e₁₂`, which Uiua has a constant for.

```uiua
# Experimental!
ₑ ×e₁₂÷2 ×η÷⟜⇡8
```

A multivector used to rotate things is called a *rotor*. To apply a rotor to a vector, we use a formula often called the *sandwich product*, which Uiua has an alias for: `sandwi`.

We want to apply every rotation to every point, so we'll use [table]().

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 4
𝕍 ◴⊂⟜≡⇌♭₂⊞⊂¯1_1
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
```

There are our rotated points! All that's left to do it render them.

To convert from multivectors back to numbers, we use [un]() [multivector](). This specifically extracts only the *grade-1 coefficients*, aka the vector parts.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 4
𝕍 ◴⊂⟜≡⇌♭₂⊞⊂¯1_1
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
°𝕍
```

We will then normalize the numbers to be all positive, scale them to nice integer sizes, and then render each row with [un]() [where]().

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 4
𝕍 ◴⊂⟜≡⇌♭₂⊞⊂¯1_1
ₑ×e₁₂÷2×η÷⟜⇡30
⊞(×⊃¯⌟˜×)
⬚0≡°⊚ ⁅×30 ⧋-/↧⊸♭₂ °𝕍
```

### Projective Geometric Algebra

Rotating a square is cool, but we actually could have done this animation even more easily using just [complex]() numbers! However, geometric algebra's strength comes in its ability to work with more than 2 dimenions. In Uiua, multivectors may have up to 10 dimensions!

To make this example more interesting, lets rotate a cube instead of a square! First, we'll modify the generation of our square points to add an extra dimensions using [repeat](). The number of points has been reduced here so that the output doesn't look too long, but we'll increase it again later.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 2
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2
```

Notice that there are now `e₃` multivector components.

We still want to rotate the points around an axis. In 3D, there are now 3 axes of rotation, but we'll still stick with `e₁₂` for now, which is rotation in the XY plane/about the Z axis. We'll use the exact same rotation code from our 2D example.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 2
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
```

And that's our rotated points. But now we have a problem. While in 2D, we could trivially convert those points to an array to be rendered on the screen, but because our points are 3D now, we have to somehow convert them to 2D so they can be rendered.

This is where *Projective Geometric Algebra* comes in. It is a geometric algebra "flavor" with slightly different rules and representations. [This video](https://youtu.be/0i3ocLhbxJ4) is a great introduction to the topic, but we'll cover that concepts that apply to our problem here.

To convert our "vanilla" geometric algebra (VGA) multivector into projective geometric algebra (PGA) vectors, we can add the vector `e₀`, which is a special PGA unit vector that squares to `0`.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 2
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
+e₀
```

In PGA, points are actually represented by bivectors. We can do this conversion easily by getting the *dual*, which in Uiua has alias `dual`.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 2
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
¯₄+e₀
```

Now that we have our PGA points, we need simulate a camera to be able to render them in 2D. We'll place our camera at the point `(-4, 0, 0)`. The point can be constructed in a similar way as before.

```uiua
# Experimental!
¯₄+e₀𝕍[¯4 0 0]
```

We can then construct multivectors reprenting lines from each cube point to the camera point. This operaiton is sometimes called the *join*. In PGA, it is done with the *regressive product*, with Uiua alias `regr`.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 2
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
¯₄+e₀
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 0 0]
```

Now that we have our lines running from cube points to the camera, we can find the intersection of those lines with a [frustum plane](https://en.wikipedia.org/wiki/Viewing_frustum), the plane that our "screen" would be in. In PGA, planes are actually just vectors. We'll use the plane normal to the X axis at `X = ¯2`.

```uiua
# Experimental!
+2e₀𝕍[1 0 0]
```

Intersections in PGA are calculated simply using the [outer product]().

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 2
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
¯₄+e₀
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 0 0]
⨱ +2e₀𝕍[1 0 0]
```

Now, we will re-`dual` the points to convert from bivectors to vectors, [sign]() to normalize accumulated magnitude, and [un]() [multivector]() to get back to numbers.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 2
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
¯₄+e₀
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 0 0]
⨱ +2e₀𝕍[1 0 0]
°𝕍±¯₄
```

The first number in each vector is `1` (from normalized `e₀`). The second number in every vector is the same, because it's the dimension that was projected onto our X-axis plane. These are both unnecessary now, so we will [drop]() them.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 2
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2
ₑ×e₁₂÷2×η÷⟜⇡8
⊞(×⊃¯⌟˜×)
¯₄+e₀
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 0 0]
⨱ +2e₀𝕍[1 0 0]
↘0_0_2 °𝕍±¯₄
```

Finally, we'll use our same rendering code as before, while also increasing the number of points and frames. Here is the final code including comments:

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 20          # Edge points
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2  # Cube points
ₑ×e₁₂÷2×η÷⟜⇡30       # Rotors
⊞(×⊃¯⌟˜×)            # Rotate points
¯₄+e₀                # Convert to PGA
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 0 0] # Lines from points to camera
⨱ +2e₀𝕍[1 0 0]       # Project to frustum plane
↘0_0_2 °𝕍±¯₄         # Convert back to numbers
⬚0≡°⊚ ⁅×30 ⧋-/↧⊸♭₂   # Render
```

What if we swapped out `e₁₂` for other bivectors?

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 20         # Edge points
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2 # Cube points
[e₁₂ e₃₁ e₂₃]
/≡₁⊂ ⬚0≡⌟(
  ₑ× ÷2×η÷⟜⇡30         # Rotors
  ⊞(×⊃¯⌟˜×)            # Rotate points
  ¯₄+e₀                # Convert to PGA
  ⍜∩¯₄⨱ ¯₄+e₀𝕍[¯8 0 0] # Lines from points to camera
  ⨱ +e₀𝕍[4 0 0]        # Project to frustum plane
  ↘0_0_2 °𝕍±¯₄         # Convert back to numbers
  ⬚0≡°⊚ ⁅×30 ⧋-/↧⊸♭₂   # Render
)
```

What if we used a sum of different bivectors? Here we use [sign]() to normalize the rotation.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 20                    # Edge points
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)2            # Cube points
ₑ ×±/+[e₁₂ 2e₃₁ e₂₃] ÷2×τ÷⟜⇡30 # Rotors
⊞(×⊃¯⌟˜×)                      # Rotate points
¯₄+e₀                          # Convert to PGA
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 0 0]           # Lines from points to camera
⨱ +2e₀𝕍[1 0 0]                 # Project to frustum plane
↘0_0_2 °𝕍±¯₄                   # Convert back to numbers
⬚0≡°⊚ ⁅×30 ⧋-/↧⊸♭₂             # Render
```

Without much additional effort, we can even rotate a tesseract, the 4D analog to a cube! All it requires is an additional projection step from 4D to 3D.

```uiua
# Experimental!
-⊸¬ ÷⟜⇡₀ 12            # Edge points
𝕍 ⍥(◴⊂⟜≡⇌♭₂⊞⊂¯1_1)3    # Cube points
ₑ×e₁₂÷2×η÷⟜⇡24         # Rotors
⊞(×⊃¯⌟˜×)              # Rotate points
¯₄+e₀                  # Convert to PGA
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 0 0 0] # Lines from points to camera
⨱ +2e₀𝕍[1 0 0 0]       # Project to frustum space
¯₄+e₀𝕍 ↘0_0_2°𝕍±¯₄     # Shift to 3D
⍜∩¯₄⨱ ¯₄+e₀𝕍[¯4 ¯3 ¯2] # Lines from points to camera
⨱ +2e₀𝕍[1 0 0]         # Project to frustum plane
↘0_0_2 °𝕍±¯₄           # Convert back to numbers
⬚0≡°⊚ ⁅×100 ⧋-/↧⊸♭₂    # Render
```

### Autodifferentiation

0-dimensional PGA vectors, aka the [dual numbers](https://en.wikipedia.org/wiki/Dual_number) can be used to automatically calculate the derivative of a function. They consist of a scalar plus some multiple of a special constant usually called `ε`. `ε`'s core property is that it squares to `0`, which is exactly what PGA basis vector `e₀` does.

Consider this function `F` which calculates `F(x) = x³ + 2x² + 5x`. Its derivative `F′` calculates `F(x) = 3x² + 4x + 5`.

```uiua
F  ← /+⊃[ⁿ3|×2ⁿ2|×5]
F′ ← +5/+⊃[×3ⁿ2|×4]
```

The key equation that makes autodifferentiation possible is that `F(x+ε) = F(x) + F′(x)ε`. This means that by simply adding `e₀` (our `ε`), to some input values, calling the function, and then extracting the vector coefficients, we can find the values at x of the derivative function `F′` without actually having to figure out the derivative ourself. Notice that the result is the same as our hand-written derivative function!

```uiua
F  ← /+⊃[ⁿ3|×2ⁿ2|×5]
F′ ← +5/+⊃[×3ⁿ2|×4]

F′ ⇡10
♭°𝕍 F +e₀ ⇡10
```

### GA Reference

Here is a quick reference table how various Uiua operators work on multivectors:

| Primitive              | GA Functionality         |
| ---------------------- | ------------------------ |
| [multivector]()        | Create multivector       |
| [multiply]()           | Geometric Product        |
| [divide]()             | Scalar division          |
| [add]()                | Multivector addition     |
| [subtract]()           | Multivector subtraction  |
| [negate]()             | Negate multivector       |
| [reciprocal]()         | Invert multivector       |
| [absolute value]()     | Multivector magnitude    |
| [sign]()               | Normalize multivector    |
| [exponential]()        | Exponentiate multivector |
| [un]() [exponential]() | Multivector natural log  |
| [power]()              | Multivector power        |
| [negate]()`⌟`          | Reverse (`conj`)         |
| [negate]()`⌞`          | Negative reverse         |
| [negate]()`₄`          | Dual (`dual`)            |
| [negate]()`₋₄`         | Antidual                 |
| [outer product]()      | Outer product            |
| [inner product]()      | Inner product            |
| [inner product]()`⌞`   | Left contraction         |
| [inner product]()`⌟`   | Right contraction        |

In general, all product operators on multivectors are not commutative, meaning the order of the operands matters. Uiua uses a convention where an operation like `AB` or `A∧B` in normal mathematical notation becomes `×B A` or `⨱B A`, with the "right" argument as the first. This is in line with many other Uiua operators.

### Blade Literals

Multivector blade literals can be written with an optional sign and magnitude number before the letter `e` followed by subscripts numbers indicating the bases.

```uiua
# Experimental!
e₂ 4e₁₂ 3e₃₁ ¯e₀ e₁₂₃₄
```

If the blades are not in their canonical order, they will be automatically reordered by the formatter, changing the sign or cancelling out if necessary.

```uiua
# Experimental!
e,321 4e,13 5e,120 # Try formatting!
e,00 10e,121 5e,112323
```

### Basis Order Convention

It is common among users of GA to order the 3D bivector blade that is the product of `e₁` `e₃` as `e₃₁` instead of `e₁₃`. This also applies to the 2D PGA bivector `e₂₀`. This is nice when taking the dual of a 3D vector, because it results in matching signs for the resulting bivectors. It also makes the bivectors form a nice cycle.

```uiua
# Experimental!
[⊸¯₄] 𝕍[1 2 3]
```

In more then 3 dimensions, no such cycle exists, so all bivectors are ordered normally. When a multivector that requires 4 or more dimensions combines with a 3D multivector, the sign of that blade is flipped to account for this.

```uiua
# Experimental!
+ e₄ 2e₃₁
```

### PGA Cheatsheet

For first argument A and second argument B:
| Operation                    | Code                |
| ---------------------------- | ------------------- |
| Project A onto B             | `×⤙⨰`               |
| Project B onto A             | `×⟜˜⨰`              |
| Reflect B across A           | `×⟜˜×`              |
| Intersection of A and B      | [outer product]()   |
| Smallest superset of A and B | `⍜∩¯₄⨱` (`regr`)    |
| Apply rotor A to B           | `×⊃¯⌟˜×` (`sandwi`) |