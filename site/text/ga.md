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