## Data Definitions

Data definitions allow you to define structured data whose fields can be accessed by name.

The most basic way to define a data definition is with a `~` followed by a name and some field names inside stack array syntax.

```uiua
# Experimental!
~MyData {Foo Bar}
```

This generates a [module](/tutorial/modules) with a constructor as well as field accessors for the given names.

The constructor has the name `New`, which allows it to be called with the module's name.

```uiua
# Experimental!
~MyData {Foo Bar}
MyData "wow!" 5
MyData~Bar .
```

Notice that the created structure is just a normal box array. The values of the fields are [label](/tutorial/codetactility#labels)led with their name.

The field accessors both [un](/docs/un)[box](/docs/box) and un-label the retrieved values.

If `[]`s are used instead of `{}`s, the fields will not be boxed or labelled.

```uiua
# Experimental!
~Color [r g b a]
Color 1 0.5 0 1
```

The field accessors can be used with [under](/docs/under) modify or replace the value.

```uiua
# Experimental!
~MyData {Foo Bar}
MyData "wow" 5
⍜MyData~Bar(+1) .
⍜MyData~Foo⋅"cool"
```

The [un](/docs/un)[by](/docs/by) idiom also allows you to easily set a value.

```uiua
# Experimental!
~MyData {Foo Bar}
MyData "wow" 5
°⊸MyData~Foo "cool"
```

You can set a default value for a field by writing it like a binding.

```uiua
# Experimental!
~MyData {Foo Bar ← 0}
MyData 5
```

The default can be a function. This will pre-process the value before construction.

Multiple defaults can be separated by newlines or `|`s.

```uiua
# Experimental!
~MyData {Foo ← ⊂5⇌|Bar ← 0}
MyData 1_2_3
```

You can put a data definition inside a scoped module if you'd like to define other functions that use the data. If the name is omitted, the name of the module will be used.

```uiua
# Experimental!
┌─╴MyData
  ~{Foo Bar}
  Format ← /$"Foo is _ and Bar is _"
└─╴
MyData~Format MyData 1_2_3 5
```

If instead of a `~`, you use a `|` followed by a name, the data definition will be treated as a *variant* of the enclosing module.

The constructors for these variants will prepend a tag to the data so that they can be disambiguated. The field accessors will skip the tag.

Because the constructed variants are tagged with incrementing integers, they can be [pattern-matched](/tutorial/patternmatching) on, perhaps in [try](/docs/try).

```uiua
# Experimental!
┌─╴M
  |Foo {Bar Baz}
  |Qux [x y z]
  |Sed {M N}
  
  Format ← ⍣(
    $"_ and _" °Foo
  | $"⟨_ _ _⟩" °Qux
  | $"_: _" °Sed
  )
└─╴
M~Format M~Foo 2 5
M~Format M~Qux 0 4 1
M~Format M~Sed "Name" "Dan"
```

A data definition's name can be used as a monadic macro. The field getters will be in scope inside the macro.

```uiua
# Experimental!
~MyData {Foo Bar}
MyData!(+⊃Foo Bar New) 3 5
```

If some code immediately follows the data definition, a `Call` function will be generated which uses the constructor as a [fill](/docs/fill) function and in which the field names pull from the fill value.

This essentially allows for named function arguments.

```uiua
# Experimental!
~MyData {Foo Bar} ↯2 Foo_Foo_Bar
MyData 3 5
```

```uiua
# Experimental!
~Foo [x] +x
Foo 3 5
```

```uiua
# Experimental!
~Quad [a b c] ÷×2a -b ⊟¯.√ℂ0 -/×4_a_c ×.b
Quad 1 2 0
```
