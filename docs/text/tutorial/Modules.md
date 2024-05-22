# Modules

Modules are a way to organize your code in Uiua. They can either be defined in a scope, or imported from a file. Any Uiua file can be used as a module.

Modules can be compared to namespaces in other languages.

## Scoped Modules

Scoped modules are defined between a `┌─╴` and a `└─╴`.

Both delimiters format from `---`.

The `┌─╴` should be immediately followed by a name for the module. Module names follow the same rules as other bindings.

Names from inside the module can be referenced by following the module name with a `~`.

```uiua kala arms help(Try formatting!)
---Mod
  A ← 5
  F ← +1
  G ← F F
---
Mod~G Mod~A
```

Bindings defined inside a scoped module are only visible inside the module.

```uiua should fail
┌─╴Mod
  A ← 5
  F ← +1
  G ← F F
└─╴
G A
```

Names from inside the module can be *made* visible by following the module name with a `~` and a list of the names to make visible.

```uiua
┌─╴Mod ~ A G
  A ← 5
  F ← +1
  G ← F F
└─╴
G A
```

With a normal `~`, these names are brought into the module's parent's scope as public bindings.

```uiua
┌─╴Mod
  ┌─╴SubMod ~ X
    X ← 5
  └─╴
└─╴
Mod~X
```

You can use a `≁`, which formats from `~~` to make them private instead.

```uiua should fail
┌─╴Mod
  ┌─╴SubMod ≁ X
    X ← 5
  └─╴
└─╴
Mod~X
```

Names defined above the module can be referenced inside it.

```uiua
B ← 5
┌─╴Mod
  C ← ×2 B
└─╴
Mod~C
```

## Module Import Macros

If the name of a module is referenced as a macro (with a trailing `!`), names defined in the module will be available in the macro's scope.

This is useful if you need to refer to a bunch of bindings from a module without having to prefix them with the module name.

```uiua
┌─╴Foo
  New ← {⊓$Bar$Baz}
  Format ← /$"_ _"
  Incr ← ⍜⊣(+1)
└─╴
Foo!(Format Incr New) "Oh" 10
```

## Files on the Website

Using files as modules involves loading files from the file system.

This website has a virtual file system. You can write to virtual files with [`&fwa`](). You can also drag and drop files from your computer into the editor to make them available to import.

There is also a test module that can always be imported as `example.ua`. Its contents is:

`EXAMPLE.UA`

## Importing Modules

Modules can be imported by file path with `~`.

```uiua
~ "example.ua"
```

This is not very useful on its own. We can bind items from the module in the current scope by listing them after the file path, separated by an additional `~`.

```uiua
~ "example.ua" ~ Increment Square

Increment Square 3
```

If we have a lot of items to import, we can use multiple lines.

```uiua
~ "example.ua"
~ Increment Square
~ Span
~ Foo Bar

Increment Square Foo
Span 4 10
```

The formatter will automatically indent the imports if they are on multiple lines. It will also alphabetize them. Try it out!

## Binding Modules

If we put a name before the import, we can bind the module to that name.

We can then reference items from that module anywhere using a `~`.

```uiua
Ex ~ "example.ua"

Ex~Increment 10
```

This can be mixed and matched with the other import syntax.

```uiua
Ex ~ "example.ua" ~ Increment Square

Ex~Double Square 3
Ex~Mac!×
Increment Ex~Bar
```

## Aliasing Modules

If you want to be able to refer to an item from a module with a different name, simply make a binding with the new name.

```uiua
Ex ~ "example.ua"
Sqr ← Ex~Square
Sp ← Ex~Span

Sp⟜Sqr 3
```

These bindings will also get indented by the formatter if they immediately follow the import. Try formatting the above code!

You can also re-bind the module itself.

```uiua
Ex ~ "example.ua"
LocalEx ← Ex
LocalEx~Square 7
```

## Visibility

All bindings in a module bound with the normal `←` arrow are public and can be used by importers of the module.

However, modules imported in modules, as well as their same-name imports (the names on lines that start with `~`), are private.

You may have noticed in the example file that one binding uses a special `↚` arrow. This indicates that the binding is private.

Private bindings cannot be accessed from outside the file in which they are defined.

```uiua should fail
~ "example.ua" ~ RangeDiff
```

To enter this arrow, you can put a `~` after a binding's normal `←` or `=`.

Try formatting the following example to see how this works.

```uiua
A = +1
B ← +2
C =~ +3
D ←~ +4
```

### Private Imports

Imports can be made private by using a `≁` instead of the first `~`. `≁` formats from `~~`.

```uiua should fail
# Try formatting!
┌─╴M
  Ex ~~ "example"
└─╴
M~Ex~Foo
```

`≁` can also be used to make imported items private to the importing module.

```uiua should fail
┌─╴M
  ~ "example" ≁ Square
└─╴
M~Square 5
```

### Private Scoped Modules

Scoped modules can be made private with special delimiters `┌╶╶` and `└╶╶`. These format from the normal delimiters or `---`s followed by a `~`.

```uiua should fail
┌─╴A
  ┌╶╶B
    C ← 5
  └╶╶
└─╴
A~B~C
```

```uiua should fail
# Try formatting!
---A
  ---~M
    F = +1
  ---
---
A~M~F 5
```

The formatter will automatically change the closing delimiter to match its corresponding opening delimiter.

## Git Modules

Modules can be imported from Git repositories. Instead of a path, use a URL prefixed with `git:`.

The Uiua GitHub organization hosts an example module at [https://github.com/uiua-lang/example-module](https://github.com/uiua-lang/example-module). The protocol specification can be omitted.

```uiua
~ "git: github.com/uiua-lang/example-module" ~ Upscale
Upscale 3 [1_2 3_4]
```

On the site, code is pulled from a `lib.ua` file at the root of the repository. Loading other files on the site is not supported.

To use Git modules in the [native interpreter](/docs/install), you must have Git installed. The repository is cloned and the `lib.ua` file is loaded as the module's contents. Code from other files can be made available by importing them as modules in the `lib.ua` file.

The native interpreter also supports adding an additional `branch: <branch-name>` or `commit: <commit-hash>` specifier after the URL.

You can also specify modules from subfolders in the repository by naming the folder after the url.

```uiua
~ "git: github.com/uiua-lang/example-module subfolder" ~ Lang
$"This is _!" Lang
```

The `uiua module` command can be used to list or update Git modules.

You can find a curated list of Uiua modules [here](https://github.com/uiua-lang/uiua-modules).
