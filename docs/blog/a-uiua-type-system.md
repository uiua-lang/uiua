# A Uiua Type System

2025-06-??

---

## A Dynamic Language

Uiua, like most array languages, is dynamically-typed. While shape and the scalar type of an array can oftne be known at compile-time, the language is not designed in a way that makes this *always* possible. Anything that involves shapes or scalar types derive from user input, the environment, or randomness can necessarily only be know at run time. Consider these examples:

Runtime variable scalar type:
```uiua
F ← ⍥(˜-@a)⚂₁₀
⊃(F|F|F|F|F) @U
```
Runtime variable shape:
```uiua
↯ ⁅◿10×10now 0
```

In many cases, this make Uiua programs simpler to write. You don't need to specify concrete information about array in function type signatures as you would in a statically typed language. Also, functions can work on many different types and shapes of array.

```uiua
F ← ⊂↯
F 4 0 [1 2 3]
F 3 [1 2] [3_5 5_6]
F 8 @U "iua"
```

However, this dynamism can be a double-edged sword, especially in larger and/or longer-lived codebases where maintainability is a concern.

## Prior Art

This is not a new problem in dynamically-typed languages. Many of the prominent dynamic languages have systems for *gradual* types: opt-in type checking for the parts of the code that really need it. This generally comes in the form of either some tooling-supported type specification stynax (JSDoc, Python types, LDoc, etc) or a full superset language (TypeScript, Luau, etc).

While the specifics of each of these systems can vary, the user interacts with most type systems in a similar way: they specify input and/or output types, and the compiler/interpreter/tooling uses that information to check types and compile/runtime and/or show the types in documentation. This is a useful mode of interaction, and any potential Uiua type system should support both type checking and type documentation.

