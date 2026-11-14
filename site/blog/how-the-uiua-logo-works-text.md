# How the Uiua Logo Works

2025-05-29

---

This post explains how the code that generates the Uiua logo works.

```uiua
LOGO
```

In principle, this code implements [4 mathematical functions](https://www.desmos.com/calculator/2oy5odi1gc) for the boundaries of the logo shape, as well as a simple function that defines a color gradient.

These functions all operate pervasively on a coordinate grid, which is a common practice when generating images in Uiua. In particular, this grid represents the square range from `[¯1 ¯1]` to `[1 1]`.

### The Grid

Before we can generate a whole grid, let's start with that simple range. To generate `N` numbers between `0` and `1`, we can start with the [`divide ÷`](https://uiua.org/docs/divide) [`on ⟜`](https://uiua.org/docs/on) [`range ⇡`](https://uiua.org/docs/range) idiom. This generates a range, then divides each number by the original max. We'll start with a small number of elements so that they all fit nicely on the screen.

```uiua
÷⟜⇡8
```

To convert this range from the bounds `[0 1)` to `[¯1 1)`, we can use the "wrench" idiom [`subtract -`](https://uiua.org/docs/subtract) [`by ⟜`](https://uiua.org/docs/by) [`not ¬`](https://uiua.org/docs/not).

```uiua
-⊸¬ ÷⟜⇡8
```

Then creating our coordinates is a simple matter of calling [`table ⊞`](https://uiua.org/docs/table) [`couple ⊟`](https://uiua.org/docs/couple) on the list and its [`self ˙`](https://uiua.org/docs/self).

```uiua
˙⊞⊟ -⊸¬ ÷⟜⇡8
```

This array has the shape `[N N 2]`. To make it easier to access all the x components or all the y components at once, we can [`un °`](https://uiua.org/docs/un) [`transpose ⍉`](https://uiua.org/docs/transpose) the array so that the `2` axis is at the front.

```uiua
°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡8
```

Now we are ready to start calling our functions on these coordinates. Note that because Uiua renders images with the first axis of the area being the vertical one, with positive direction going down, what would be coordinates of the form `[x y]` become `[¯y x]` here.

### Circles

Let's start with circles. A coordinate lies within a circle if its distance from the center of the circle is less that the circle's radius. Our coordinate grid is centered on the origin, as will be all of our circles, so generating a circle is a simple matter of calculating the distance a coordinate is from the origin and applying some threshold.

One way to do this is to implement the Pythagorean theorem directly using [`under ⍜`](https://uiua.org/docs/under).

```uiua
°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
<1 ⍜°√/+
```

This squares every number in the grid, [`add +`](https://uiua.org/docs/add)s the x and y components, and then takes their [`sqrt √`](https://uiua.org/docs/sqrt). A threshold value is applied with [`less than <`](https://www.uiua.org/docs/less%20than).

However, because Uiua supports [`complex ℂ`](https://uiua.org/docs/complex) numbers, we can achieve the same behavior by taking the [`absolute value ⌵`](https://uiua.org/docs/absolute) of a complex number that has the x and y components as its real and imaginary components.

```uiua
°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
<1 ⌵/ℂ
```

We'll need 2 different circles with different radii, so let's make this a function that takes the radius as a parameter.

```uiua
I ← <⊙(⌵/ℂ)

°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
I 1 # ← Try changing
```

### Parabolas

Next we'll look at the two parabola functions. Both functions are of the form `y + 2x² = h`, where `h` offsets the parabola vertically.

Because we only need to modify the x component, we can simply do so with [`under ⍜`](https://uiua.org/docs/under) [`last ⊣`](https://uiua.org/docs/last).

```uiua
°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
/+ ⍜⊣(×2ⁿ2)
```

Alternatively, we could notice that power and coefficients are the same for both the x and y terms (`2` and `2` for x and `1` and `1` for y), so we can reuse the same array for both operations.

```uiua
°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
/+ ×⟜ⁿ1_2
```

Notice how we've created of sort of parabola field. If we apply a threshold to it, we get a nice `U` shape.

```uiua
°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
<0.2 /+ ×⟜ⁿ1_2
```

To apply both our parabola thresholds, we can use [`both ∩`](https://uiua.org/docs/both) with a right [sided subscript](https://www.uiua.org/docs/subscripts#sided).

```uiua
°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
∩⌟<0.2 0.7 /+ ×⟜ⁿ1_2
```

We can create the mask we need for the full `U` shape by simply getting the places where one mask [`equals =`](https://uiua.org/docs/equal) the other. Let's put that in a conveniently named function.

```uiua
U ← =∩⌟<0.2 0.7 /+ ×⟜ⁿ1_2
U °⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
```

### Alpha Channel

The logo has a transparency channel defined as the intersection between the circle and parabola masks. We'll use [`fork ⊃`](https://uiua.org/docs/fork) so we can pass the grid to both functions, and [`multiply ×`](https://uiua.org/docs/multiply) to get the intersection.

```uiua
U ← =∩⌟<0.2 0.7 /+ ×⟜ⁿ1_2
I ← <⊙(⌵/ℂ)
A ← ×⊃U(I1)

A °⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
```

### Colors

Now we'll look at the color gradient. The RGB color channels are defined as follows:
- `R = x² + 0.1`
- `G = y² + 0.1`
- `B = 0.6`

We can square with [`self ˙`](https://uiua.org/docs/self) [`multiply ×`](https://uiua.org/docs/multiply).

Because our coordinates are `[¯y x]` instead of `[x y]`, we'll have to [`reverse ⇌`](https://uiua.org/docs/reverse) them to be in the correct order. We [`join ⊂`](https://uiua.org/docs/join) `0.5` to the end as the blue channel rather than `0.6` so that we can simply [`add +`](https://uiua.org/docs/add) `0.1` to all the channels after.

We'll temporarily [`transpose ⍉`](https://uiua.org/docs/transpose) so that we can see the colors.

```uiua
°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
⍉ +0.1 ⊂⊙0.5 ⇌˙×
```

We also need to apply a 10% gray color to the outer rim of the circle. This rim is 95% the radius of the full circle. We'll pass our grid to a second function using [`fork ⊃`](https://uiua.org/docs/fork). We can again use our circle function from earlier.

```uiua
I ← <⊙(⌵/ℂ)

°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
⊃(⍉ I0.95|⍉ +0.1 ⊂⊙0.5 ⇌˙×)
```

We can apply the mask with [`minimum ↧`](https://uiua.org/docs/minimum). [`fix ¤`](https://uiua.org/docs/fix) is needed to make the shapes agree.

Because the rim also needs `0.1` added to it, we can do that addition a single time to the masked color gradient. We'll bind this all as another function.

```uiua
I ← <⊙(⌵/ℂ)
u ← +0.1 ↧¤ ⊃(I0.95|⊂⊙0.5 ⇌˙×)

°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
⍉ u
```

### Putting It All Together

Now that we have both the RGB and the alpha channels, we can simply compose them with [`fork ⊃`](https://uiua.org/docs/fork) and [`join ⊂`](https://uiua.org/docs/join)!

Note that while while there is a space between them, `u` and `A` are both being used as operands to [`fork ⊃`](https://uiua.org/docs/fork).

```uiua
U ← =∩⌟<0.2 0.7 /+ ×⟜ⁿ1_2
I ← <⊙(⌵/ℂ)
u ← +0.1 ↧¤ ⊃(I0.95|⊂⊙0.5 ⇌˙×)
A ← ×⊃U(I1)

⍉ ⊂⊃u A °⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡100
```

Seeing `°⍉` being followed not long after by `⍉` is a great instance to refactor to use [`under ⍜`](https://uiua.org/docs/under).

```uiua
U ← =∩⌟<0.2 0.7 /+ ×⟜ⁿ1_2
I ← <⊙(⌵/ℂ)
u ← +0.1 ↧¤ ⊃(I0.95|⊂⊙0.5 ⇌˙×)
A ← ×⊃U(I1)
⍜°⍉(⊂⊃u A) ˙⊞⊟ -⊸¬ ÷⟜⇡256
```

🎉 And that's the logo! 🥳

### Parameterizing

One next step we might think to take is extracting some of those constants to make them parameters. Doing this within Uiua's full-tacit model can trip some people up, but we'll see it's not too scary!

First, we can parameterize the size of the image. This is simple enough.

```uiua
U ← =∩⌟<0.2 0.7 /+ ×⟜ⁿ1_2
I ← <⊙(⌵/ℂ)
u ← +0.1 ↧¤ ⊃(I0.95|⊂⊙0.5 ⇌˙×)
A ← ×⊃U(I1)
L ← ⍜°⍉(⊂⊃u A) ˙⊞⊟ -⊸¬ ÷⟜⇡
L 128
```

Next, we can parameterize the parabola offsets. It would be convenient to be able to bundle them together, so lets tweak the `U` function a bit first to work on an array of two numbers rather than 2 separate scalars.

```uiua
U ← /=⊞<0.2_0.7 /+ ×⟜ⁿ1_2
I ← <⊙(⌵/ℂ)
u ← +0.1 ↧¤ ⊃(I0.95|⊂⊙0.5 ⇌˙×)
A ← ×⊃U(I1)
L ← ⍜°⍉(⊂⊃u A) ˙⊞⊟ -⊸¬ ÷⟜⇡
L 128
```

Then we replace the `0.2_0.7` array with a [`dip ⊙`](https://uiua.org/docs/dip). This changes the signature of `U`, so we'll have to put a few [`dip ⊙`](https://uiua.org/docs/dip)s and [`gap ⋅`](https://uiua.org/docs/gap)s throughout its call path. [`gap ⋅`](https://uiua.org/docs/gap) is generally needed in [`fork ⊃`](https://uiua.org/docs/fork) tines where that value is not used.

```uiua
U ← /=⊞< ⊙(/+ ×⟜ⁿ1_2)
I ← <⊙(⌵/ℂ)
u ← +0.1 ↧¤ ⊃(I0.95|⊂⊙0.5 ⇌˙×)
A ← ×⊃U⋅(I1)
L ← ⍉ ⊂⊃⋅u A ⊙(°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡)
L 0.2_0.7 128
```

We could go crazy and parameterize everything, but for now, lets just also parameterize the rim radius. We'll turn the `u` function into a dyadic function that takes the radius, and again add [`dip ⊙`](https://uiua.org/docs/dip)s and [`gap ⋅`](https://uiua.org/docs/gap)s accordingly.

Note here that the [`fork ⊃`](https://uiua.org/docs/fork) in the `L` function can actually become sided [`bracket ⊓`](https://uiua.org/docs/bracket).

```uiua
U ← /=⊞< ⊙(/+ ×⟜ⁿ1_2)
I ← <⊙(⌵/ℂ)
u ← +0.1 ↧¤ ⊃I⋅(⊂⊙0.5 ⇌˙×)
A ← ×⊃U⋅(I1)
L ← ⍉ ⊂⊓⌟u A ⊙⊙(°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡)
L 0.95 0.2_0.7 128
```

With more parameters, it can be hard to remember what goes where. Let's document everything real quick.

```uiua
U ← /=⊞< ⊙(/+ ×⟜ⁿ1_2)      # U shape
I ← <⊙(⌵/ℂ)                # Circle
u ← +0.1 ↧¤ ⊃I⋅(⊂⊙0.5 ⇌˙×) # Rgb
A ← ×⊃U⋅(I1)               # Alpha
# ? Rim UBounds Size
L ← ⍉ ⊂⊓⌟u A ⊙⊙(°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡)
```

Now we can make wacky versions of the logo!

```uiua
U ← /=⊞< ⊙(/+ ×⟜ⁿ1_2)      # U shape
I ← <⊙(⌵/ℂ)                # Circle
u ← +0.1 ↧¤ ⊃I⋅(⊂⊙0.5 ⇌˙×) # Rgb
A ← ×⊃U⋅(I1)               # Alpha
# ? Rim UBounds Size
L ← ⍉ ⊂⊓⌟u A ⊙⊙(°⍉ ˙⊞⊟ -⊸¬ ÷⟜⇡)

L 0.9 0.1_0.8 100
L 0.6 0_0.3 128
L 0.4 ¯2_0 200
```
