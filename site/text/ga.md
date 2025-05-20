## Geometric Algebra

The [geometric]() modifier changes the behavior of its function to be an operation in [Geometric Algebra](https://en.wikipedia.org/wiki/Geometric_algebra).

Geometric Algebra unifies many useful mathematical concepts, including complex numbers, quaternions, and linear transformations.

Here is a quick reference table of each transformed operation. They will be explained in more detail below.

| Primitive          | GA Functionality         |
| ------------------ | ------------------------ |
| [multiply]()       | Geometric Product        |
| [divide]()         | Scalar division          |
| [add]()            | Multivector addition     |
| [subtract]()       | Multivector subtraction  |
| [absolute value]() | Multivector magnitude    |
| [sign]()           | Normalize multivector    |
| [negate]()         | Reverse                  |
| [not]()            | Dual                     |
| [minimum]()        | Wedge product            |
| [maximum]()        | Regressive product       |
| [rotate]()         | Sandwich product         |
| `anti` [rotate]()  | Reverse sandwich product |
| [atangent]()       | Vectors to rotor         |
| [select]()         | Select blades            |
| `anti` [select]()  | Pad blades               |

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

But a 2D multivector has 4 components, not just 2. It has a scalar part, 2 vector parts, and a *bivector* part. We call each of these parts a *blade*. Each blade has a *grade* - the number of basis vectors it is the product of. In a 2D multivector, the scalar blade is grade 0, the vector blades are grade 1, and the bivector blade is grade 2.

So the parts of a 2D multivector that make up a complex number are grades 0 and 2, the even numbers. In 3D, there is also a single grade 0 blade, but there are 3 grade 2 blades. Those 4 numbers are actually a quaternion, which can be used to rotate objects in 3D space.

In general, the *even subalgebra* - the even-graded blades of a multivector - in any number of dimensions allows to express rotations of objects in that space. This kind of rotator multivector is called a *rotor*. By default, [geometric]() interprets arrays with a last axis of 2, 4, 8, 16, etc. as being rotors. So length 2 is complexes, length 4 is quaternions, length 8 is 4D rotors, etc.