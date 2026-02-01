# Data Definitions

In most cases, the data stored in an array can be thought of as *homogeneous*. All rows in the array *mean* the same thing. Maybe they are all quantities of something, maybe they are all strings from the same source, maybe they are all coordinates in a 2D space, etc. One of the strengths of the array paradigm is that arrays can also be homogeneous across multiple axes at once.

However, sometimes this is not what you want. Sometimes, you want to package together multiple values that are related but represent fundamentally different things.

## Basic Structure

One way to bundle arbitrary data is to make a list of [`box`]()ed arrays. Let's say we want to have a way to store the name and age of a person. We could start by making a constructor function.

```uiua
Person ← □₂
Person "Dave" 31
```

We'll likely want to be able to access the name and age with functions of their own.

```uiua
Person ← □₂
Name   ← °□⊡0
Age    ← °□⊡1
Person "Dave" 31
⊸Age
```

We can even use [`under`]() to modify one of the fields.

```uiua
Person ← □₂
Name   ← °□⊡0
Age    ← °□⊡1
Person "Dave" 31
⍜Age(+1)
```

Defining all this can get a bit cumbersome, especially for larger data structures, so Uiua offers a better way.

## Defining Data

Because the pattern above is a simple and common way to define heterogeneous data, Uiua has a special syntax called *data definitions* to make it easier. Data definitions typically start with a `~` followed by a name and an array of fields.

We can define a `Person` data definition like this.

```uiua
~Person {Name Age}
```

This defines a [module](/tutorial/Modules) called `Person`. In this case, the module contains three functions: `New`, `Name`, and `Age`. Because a `New` function exists, we can call the module as a function.

```uiua
~Person {Name Age}
Person "Dave" 31
⊸Person~Age
```

As you can see, the generated constructor function also adds [labels](/tutorial/codetactility#labels) to aid in reading the data.

Notice the `{}` brackets in the example above. If we expect all fields to have the same type and shape, we can use `[]` brackets instead. This prevents the fields from being boxed, but disables labels.

This example defines a `Color` data definition with red, green, and blue channels. We use a [module import macro](/tutorial/Modules#module-import-macros) to more concisely access the `g` and `b` fields.

```uiua
~Color [r g b]
Color 0.5 0.4 1
⊸Color!(+⊃g b)
```

Default values can be specified with `←` after the field name. This formats from `=`, just like bindings.

Fields with a default value will not be arguments to the constructor.

```uiua
~Person {Name Age Items ← {}}
Person "Hannah" 19
⊸⍜⊙Person~Items⊂ {"Book" "Spatula"}
```

Multiple default values can be separated with `|`s.

```uiua
~Abilities [Str ← 10|Dex ← 10|Con ← 10]
```

Fields can also be put on multiple lines.

```uiua
~Abilities [
  Str ← 10
  Dex ← 10
  Con ← 10
  Int ← 10
  Wis ← 10
  Cha ← 10
]
```

If the code for a default value has a signature that takes arguments, those arguments become part of the constructor.

```uiua
~Particle {Mass Velocity ← ⊟}
Particle 1 3 5
```

If the default functions for all fields are invertible (or if fields have no default), then the constructor can be inverted with [`un`]().

```uiua
~Particle {Mass Velocity ← ⊟}
Particle 1 3 5
⊸°Particle
```

Fields can be easily set with the [`un`]()[`by`]() idiom.

```uiua
~Abilities {Str ← 10|Dex ← 10|Con ← 10}
Abilities!(°⊸Con 12 °⊸Dex 16 New)
```

### Visibility

Like imports, data definitions can be made private by using `≁` instead of `~`. `≁` formats from `~~`.

```uiua should fail
┌─╴M
  ~~C {A}
  ≁D {A}
└─╴
M~D 5
```

## Definitions in Modules

If we omit a data definition's name, it becomes a data definition for the surrounding module. This is useful if we want to add related functions to work with that data.

```uiua
┌─╴Person
  ~{Name Age}
  PassYear ← ⍜Age(+1)
  Format   ← $"_ is _ year_ old" ⊃(Name|Age|▽⊙@s≠1Age)
└─╴
Person!(Format PassYear New "Sophie" 41)
Person!(Format PassYear New "Rom" 0)
```

A module cannot contain multiple unnamed data definitions.

```uiua should fail
┌─╴Person
  ~{Name Age}
  ~{Foo Bar Baz}
└─╴
```

## Variants

If we begin a data definition with `|` instead of `~`, it becomes a *variant*. Variants allow us to represent multiple mutually exclusive states or options.

The constructor for a variant adds a tag to the array to disambiguate which variant it is. This tag is an incrementing integer starting from `0`. Each module has its own tag counter.

Variants can have any number of fields, including none. Unlike normal data definitions, variants cannot be declared as private.

```uiua
┌─╴Shape
  |Circle {Radius}
  |Rectangle {Width Height}
  |Point 
└─╴
Shape~Circle 4
Shape~Rectangle 3 5
Shape~Point
```

If we expect a data array to be a specific variant, we can access its fields directly. This throws an error if the variant is not the expected one.

```uiua
┌─╴Shape
  |Circle {Radius}
  |Rectangle {Width Height}
└─╴
Shape~Circle 5
Shape~Circle~Radius
```

```uiua should fail
┌─╴Shape
  |Circle {Radius}
  |Rectangle {Width Height}
└─╴
Shape~Circle 5
Shape~Rectangle~Height
```

We can use [pattern matching](</tutorial/Pattern Matching>) to do something different depending on which variant we have.

```uiua
┌─╴Shape
  |Circle {Radius}
  |Rectangle {Width Height}
  |Point 
  Square ← ˙Rectangle
  Area ← ⍣(
    ×π ˙× °Circle
  | × °Rectangle
  | 0
  )
└─╴
Shape!(
  Area Circle 4
  Area Rectangle 3 5
  Area Square 10
  Area Point
)
```

If you have a lot of variants, you can put more than one on a single line.

```uiua
┌─╴Element
  |H |He 
  |Li |Be |B |C |N |O |F |Ne 
  |Na |Mg |Al |Si |P |S |Cl |Ar
  # ...
└─╴
```

A module that contains variants has a `Variants` binding that lists their names.

```uiua
┌─╴IpAddr
  |V₄ [A B C D]
  |V₆ [A B C D E F]
└─╴
IpAddr~Variants
```

## Chained Access

When data definitions are nested, it is common to name parent fields according to the name of the child's definition.

Consider this simple nested structure.

```uiua
~Bars [Hp ← 100|Mp ← 100|Sta ← 100]
~Player {Name Bars ← Bars}
```

If we wanted to get the player's HP, we would have to use two different field accesses.

```uiua
~Bars [Hp ← 100|Mp ← 100|Sta ← 100]
~Player {Name Bars ← Bars}
Player "Roni"
Bars~Hp Player~Bars
```

We can use the `≈` symbol, which formats from `~~`, to simplify this common pattern. This is syntactic sugar for the example above, so it is compatible with other [`under`]() and the like.

```uiua
~Bars [Hp ← 100|Mp ← 100|Sta ← 100]
~Player {Name Bars ← Bars}
Player "Roni"
⍜Player~Bars≈Hp(-10)
```

These are arbitrarily chainable and work as long as the name of the field and the name of the data definition in that field are the same.

```uiua
~A {B Q}
~B {C R}
~C {D S ← []}
~D {E F}
A⊙[1 2 3] B C⊙0 D "EE" @f
⊸A~B≈C≈D≈E
```

## Structs of Arrays

There is a well-known optimization for lower-level languages called "structs of arrays". It is a design pattern where instead of making a list of similar data structures, you make a single structure with a list for each field. This makes code faster by making data take up less memory and therefore making CPU cache misses less likely.

Let's say we want a collection of instances of our `Person` data from above. We could try doing this as a `[]` or `{}` array.

```uiua
~Person {Name Age Score}
[Person "Alice" 21 5 Person "Bob" 54 0 Person "Carol" 49 12]
{Person "Alice" 21 5 Person "Bob" 54 0 Person "Carol" 49 12}
```

While these both work, they do a lot more boxing than is necessary. The `[]` array contains a total of 9 boxes, while the `{}` array contains 12. In general, code that contains a lot of boxing and unboxing is slower than code that does not.

Both of those arrays are created as an "array of structs". But because Uiua is a dynamic language, we can very easily create our list as a "struct of arrays".

```uiua
~Person {Name Age Score}
Person {"Alice" "Bob" "Carol"} [21 54 49] [5 0 12]
```

As you can see, the array output formatter has a special rule for this kind of array that makes it display as a table.

This construction method creates only 5 boxes total, and it scales much better. It also allows us to more easily work with the data in aggregate.

For example, we could find the average age.

```uiua
~Person {Name Age Score}
Person {"Alice" "Bob" "Carol"} [21 54 49] [5 0 12]
÷⊃⧻/+ Person~Age
```

Or we could find the name of the person with the highest score.

```uiua
~Person {Name Age Score}
Person {"Alice" "Bob" "Carol"} [21 54 49] [5 0 12]
Person!(°□⊏⊃(⊢⍖Score)Name)
```

Adding new entries is also relatively simple.

```uiua
~Person {Name Age Score}
Person {"Alice" "Bob" "Carol"} [21 54 49] [5 0 12]
⍚˜⊂ Person □"Dave" 31 2
```

For non-boxing data definitions, constructing a struct of arrays when some of the fields have a default value works properly.

```uiua
~Color [r g b a ← 1]
Color [1 0 0] [0 1 0] [0 0 1]
```

However, this is not the case for boxing data definitions. Notice here how there is only one `Score` value for every person.

```uiua
~Person {Name Age Score ← 0}
Person {"Alice" "Bob" "Carol"} [21 54 49]
```

We can use the idiom `/⍚⊂≡` to properly initialize lists of default fields as well as repeat items.

```uiua
~Person {Name Age Score ← 0}
/⍚⊂≡Person {"Alice" "Bob" "Carol"} [21 54 49]
/⍚⊂≡Person {"Alice" "Bob" "Carol"} 30
```

## Data Functions

If the fields of a data definition are immediately followed by some code, the data definition becomes a *data function*. Data functions have a `Call` function defined which is invoked instead of the normal `New` constructor when the data definition's name is used as a function.

The normal constructor is called, then the constructed data is passed to the function.

```uiua
# Experimental!
~Person {Name Surname ← ""} $"_ _"⊃(Name|Surname)
Person "Dave"
```

But if the function is called immediately, how do we set `Surname`?

In this case `Surname` is an *optional argument*. When the data function's name is used as a macro, we can set optional arguments within the macro's function. We can set optional arguments with the [un](/docs/un) [by](/docs/by) idiom followed by their name.

```uiua
~Person {Name Surname ← ""} $"_ _"⊃(Name|Surname)
Person!°⊸Surname "Daveson" "Dave"
```

Setters for multiple optional arguments can occur in any order. [un](/docs/un)[by](/docs/by)[fork](/docs/fork) can be used to set multiple optional arguments.

```uiua
~F {A ← 0|B ← 0|C ← 0|D ← 0|E} ∘
F!(°⊸D1 °⊸A4 °⊸C2 °⊸B3) 5
F!°⊸⊃(D|A|C|B) 1 4 2 3 5
```

Not every argument to the function needs to be a field in the data definition.

```uiua
~Append {N ← 1} ˜⊂▽N
Append 5 1_2_3
Append!°⊸N 4 5 1_2_3
```

## Dynamic Structure

Which fields a data definition has are generally static. Fields accesses via the generated functions are static.

However, we sometimes want to be a little more dynamic with our data.

One common need is to be able to access a field by name *as a runtime string*. For example, imagine you want to get user input and return the corresponding field.

Every data definition has a generated `Fields` constant which is a list of the field names. We can get the index of some name with [`indexin`](), then get it from the structure with [`select`]().

```uiua
~Person {Name Age Score}
Person "Dave" 31 5
"Age" # Imagine this is user input
°□⊏˜⨂⊓□Person~Fields
```

The `Fields` constant can also be used to turn a data structure into a [`map`]().

```uiua
~Person {Name Age Score}
Person "Dave" 31 5
mapPerson~Fields
```

With a bit of finesse, we can also do the opposite.

```uiua
~Person {Name Age Score}
map {"Name" "Age" "Score"} {"Dave" 31 5}
°{°Person}getPerson~Fields
```

Using `map` may be necessary if you want to use a function like `json` that expects a map.

```uiua
~Person {Name Age Score}
Person "Dave" 31 5
&p json mapPerson~Fields
```
