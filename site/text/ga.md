## Geometric Algebra

The [geometric]() modifier changes the behavior of its function to be an operation in [Geometric Algebra](https://en.wikipedia.org/wiki/Geometric_algebra).

Geometric Algebra unifies many useful mathematical concepts, including complex numbers, quaternions, and linear transformations.

Here is a quick reference table of each transformed operation. They will be explained in more detail below.

| Primitive          | GA Functionality         |
| ------------------ | ------------------------ |
| [multiply]()       | Geometric Product        |
| [minimum]()        | Wedge product            |
| [maximum]()        | Inner product            |
| [or]()             | Regressive product       |
| [divide]()         | Scalar division          |
| [add]()            | Multivector addition     |
| [subtract]()       | Multivector subtraction  |
| [absolute value]() | Multivector magnitude    |
| [sign]()           | Normalize multivector    |
| [negate]()         | Reverse                  |
| [not]()            | Dual                     |
| [rotate]()         | Sandwich product         |
| `anti` [rotate]()  | Reverse sandwich product |
| [atangent]()       | Vectors to rotor         |
| [select]()         | Select blades            |
| `anti` [select]()  | Pad blades               |
| [couple]()         | Make complex             |
| `un` [couple]()    | Separate complex         |

### Complex Numbers

[geometric]() operations treat their arguments as arrays of multivector coefficients. The last axis of the array is the coefficients, and the rest of the axes are treated similarly to normal Uiua pervasive functions.

One of the most basic uses of [geometric]() is working with complex numbers. If the last axis of the input(s) is 2, each pair will be treated as the real and imaginary components of a complex number.

We can use [geometric]() [multiply]() to do the *Geometric Product*. For complex numbers, this is equivalent to normal multiplication.

```uiua
⩜× [0 1] [0 1] # i × i = ¯1
```
```uiua
⩜× [1 2] [3 4]
```
Uiua is an array language, so all these operations are pervasive.
```uiua
⩜× [0 1] [1_2 ¯3_4 5_6]
```

That example hints at something greater though. Notice how multiplying each of those numbers by `i` *rotates* the vector by `eta` (90°). This is because complex numbers are actually a subset of 2D multivectors, which generalize rotation, reflection, and skewing in 2 dimensions.

### Blades, Grades, and Rotormobiles

But a 2D multivector has 4 components, not just 2. It has a scalar part, 2 vector parts, and a *bivector* part. We call each of these parts a *blade*. Each blade has a *grade* - the number of basis vectors it is the product of. In a 2D multivector, the scalar blade is grade 0, the vector blades are grade 1, and the bivector blade is grade 2.

So the parts of a 2D multivector that make up a complex number are grades 0 and 2, the even numbers. In 3D, there is also a single grade 0 blade, but there are 3 grade 2 blades. Those 4 numbers are actually a quaternion, which can be used to rotate objects in 3D space.

In general, the *even subalgebra* - the even-graded blades of a multivector - in any number of dimensions allows to express rotations of objects in that space. This kind of rotator multivector is called a *rotor*. By default, [geometric]() interprets arrays with a last axis of 2, 4, 8, 16, etc. as being rotors. So length 2 is complexes, length 4 is quaternions, length 8 is 4D rotors, etc.

But let's say we want to rotate a 3D vector using a quaternion. How do we even construct the quaternion in the first place?

[geometric]() [atangent]() takes 2 vectors and returns a rotor that rotates the second vector to the first.

As an example, let's create a rotor that rotates the X axis 90° into the Y axis. Here, we pass vectors of length 3. Because 3 is not a power of 2, they will be interpreted as vectors rather than rotors themselves.

```uiua
⩜∠ [0 1 0] [1 0 0]
```

Appying the rotor requires something called the *sandwich product*. We won't get into how it works here, but it can be written simply with [geometric]() [rotate]().

```uiua
⁅₉ ⊸⩜(↻∠) [0 1 0] [1 0 0] [1_0_0 1_2_3 0_1_¯5]
```

There are a few things to notice from this example. First, both operations can be placed in [geometric]() together. Second, the Z component of each vector remains unchanged. This is because rotation in the XY plane does not affect the Z dimension. Third, we have to [round]() at the end; [geometric]() operations involve a lot of floating-point math and can accumulate errors relatively quickly.

### Arbitrary Multivectors and Dimensions

The way coefficients are interpreted depends on how many dimensions we are working in. For all the cases above, the number of dimensions is inferred from the shape of the inputs.

But what if the default inferrence is not what we want? For example, what if we want to multiply a 3D vector by a bivector? In 3D, both vectors and bivectors have 3 components. By default, [geometric]() will interpret two length 3 arrays as vectors and return a rotor.

```uiua
⩜× [1 2 3] [4 5 6]
```

We can make the bivector be interpreted correctly via *blade padding*. This is done with [geometric]() [anti]() [select](). It takes the given grade and pads the array so that all other blades of the multivector are 0. This operations requires us to specify the number of dimensions via a subscript.

```uiua
⩜₃⌝⊏2 [1 2 3]
```

Then we can multiply.

```uiua
⩜₃(× ⌝⊏2) [1 2 3] [4 5 6]
```

If we only care about certain grades of the result, we can use *blade exaction* via [geometric]() [select](). Here, we extract just the vectors: the grade-1 blades.

```uiua
⩜₃(⊏1 × ⌝⊏2) [1 2 3] [4 5 6]
```

[geometric]() currently supports multivectors of up to 11 dimensions.

### Metrics

Different flavors of Geometric Algebra are defined mathematically by their *metrics* - definitions of which basis vectors square to `1`, `¯1`, or `0`. By default, [geometric]() uses metrics of all `1`s. This system is often referred to as *Vanilla* Geometric Algebra (VGA), which operates in Euclidean space.

Non-euclidean metrics can be defined in the first function in a [geometric]() function pack. For example, in Projective Geometric Algebra (PGA), one dimension has the metric `0` while the rest of the dimensions have the metric `1`. We can define this with the array `0_1`. The last element is repeated for all the rest of the dimensions, so this will work for PGA in any number of dimenions.

Note that while we are doing operations on 3-dimenional multivectors, we generally think of this as 2-dimensional PGA, as one of the dimensions does not refer to normal linear space.

Because translations are just rotations in PGA, we can create a rotor that translates a point in a similar way as above.

This rotor translates a point by the vector `[2 5]`:

```uiua
⩜(0_1|∠) [0 2 5 1] [0 0 0 1]
```
And then we can apply it with [rotate]() for the sandwich product:

```uiua
⩜(0_1|↻∠) [0 2 5 1] [0 0 0 1] [0 10 20 1]
```
