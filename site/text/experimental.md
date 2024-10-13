# Data Definitions

Data definitions allow you to define structured data whose fields can be accessed by name.

The most basic way to define a data definition is with a `~` followed by a name and some field names inside stack array syntax.

```uiua
# Experimental!
~MyData {Foo Bar}
```

This generates a constructor as well as field accessors for the given names.

```uiua
# Experimental!
~MyData {Foo Bar}
MyData "wow!" 5
MyData~Bar .
```

Notice that the created structure is just a normal box array. The values of the fields are [label](/tutorial/codetactility#labels)led with their name.

The getters both [un](/docs/un)[box](/docs/box) and un-label the values.

The getters can be used with [under](/docs/under) modify or replace the value.

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
