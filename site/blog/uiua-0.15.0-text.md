# Announcing Uiua 0.15.0

2025-03-??

---

Uiua 0.15.0 is now available!

You can find the full changelog [here](https://uiua.org/docs/changelog#0.15.0---2025-03-??).

You can download pre-built binaries [here](https://github.com/uiua-lang/uiua/releases).

Uiua is a general purpose, stack-based, array-oriented programming language with a focus on tacit code.

Here are some highlights of this release:

## Data Definitions

Data definitions are a newly stabilized feature that allows names to be given to the rows of heterogeneous arrays. This is similar to structures or classes in other languages.

There is [new tutorial](https://uiua.org/tutorial/datadefs) detailing all the features of data definitions, but here are a few examples:

Simple data definitions can be defined following a `~`. We can then use the name of the definition to construct an array, and we can use the field names to access the fields.

```uiua
~Customer {Name Phone Address}
Customer "John Doe" "555-1234" "123 Main St"
Customer~Name .
```

If we want to add functions associated with the data definition, we can put the definition inside a module. We can use a [module import macro](https://www.uiua.org/tutorial/modules#module-import-macros) to easily access the items in the module.

```uiua
┌─╴Customer
  ~ {Name Phone Address}
  Format   ← $"_ lives at _" ⊃(Name|Address)
  HouseNum ← ⋕▽⊸∊+@0⇡10 Address
└─╴

Customer!(
  New "Bob" "555-1234" "123 Main St"
  $"_'s house number is _" ⊃(Name|HouseNum)

  Format New "Alice" "555-2843" "456 Broadway St"
)
```

Enum-like data definitions can be defined in a module with `|`s. [Pattern matching](https://www.uiua.org/tutorial/patternmatching) can be used to do different things based on the variant.

```uiua
┌─╴Shape
  |Circle {Radius}
  |Rectangle {Width Height}
  |Point
  Area ← $Area ⍣(
    ×π×. °Circle
  | × °Rectangle
  | 0)
  Format ← ⍣(
    $"Circle has radius _" °Circle
  | $"Rectangle is _×_" °Rectangle
  | "Point"
  )
└─╴

Shape!(⊃(Area|Format) Circle 4)
Shape!(⊃(Area|Format) Rectangle 3 5)
```

## Sided Subscripts

Sided subscripts are [subscripts](https://www.uiua.org/docs/subscripts) that instead of changing how a function works based on a number, do it based on a "side", either left or right.

Sided subscripts are formatted from `__<` and `__>`. They are currently only stabilized for [`both ∩`](https://www.uiua.org/docs/both) and [`bracket ⊓`](https://www.uiua.org/docs/bracket). These cases simplify some common stack manipulation patterns.

There is a [short tutorial section](https://www.uiua.org/tutorial/evenmorestack#sided-subscripts) about them, but here are some simple examples:

```uiua
{∩⌟⊟ 1 2 3}
{∩⌞⊟ 1 2 3}
```

```uiua
{⊓⌟⊂⊟ 1_2 3_4 5_6}
{⊓⌞⊂⊟ 1_2 3_4 5_6}
```