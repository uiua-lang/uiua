# Introduction

Welcome to the Uiua tutorial! Each page of this tutorial will introduce you to a new concept in the language. The tutorial is designed to be read in order, but you can jump around if you want.

Uiua is an array programming language in the same family as APL, J, and BQN. Uiua focuses particularly on tacit programming, that is, writing code without naming variables. It does this by putting all values on a global stack.

## What is Array Programming?

Before jumping into Uiua, it may be helpful to understand the paradigm of which it is a part.

[Array Programming](https://en.wikipedia.org/wiki/Array_programming) is a way of writing programs first developed by Ken Iverson in the 1960s. It emphasizes the use of arrays as the primary data structure, and many operations work on entire arrays at once. An array language also has a rich set of operations for manipulating arrays in various ways, to the point where many data structures and control flow constructs are replaced with operations on arrays.

## Why Array Programming?

There are many reasons that Array Programming is cool, but here are a few:

Perhaps the most practical reason is that arrays as a data structure are what some refer to as "machine-sympathetic". Because array elements exist in contiguous memory, they can be processed very quickly by modern CPUs. Many operations can be *vectorized*, meaning that the CPU can apply the operation to many elements in parallel. In addition, allocating memory for a large array can be done all at once. Although most array languages are dynamic and interpreted, they can often compete in performance with compiled, static languages, at least for certain kinds of tasks.

Array programming can change the way you think about programs. All operations you use act on a single, unified data structure. You stop thinking about individual items and start thinking about entire collections and how they can interact and transform.

Finally, many common algorithms are built-in to array languages, and are often written with a single glyph. This makes array code very concise and expressive. Several lines of code in a language like C or Python can often be just a few characters in an array language. Array languages do away with much of the *ceremony* that is present in other languages, so you can focus more on the problem you are trying to solve.

## Why Uiua?

Uiua retains many of the array operations and semantics of its predecessor array languages. However, while APL, J, and BQN structure code with syntax and semantics based on mathematical notation:

`1 + 2`, `4×ω-2`, etc.

Uiua puts values on a stack, and operators appear to the left of their arguments:

`+ 1 2`, `×4-2`, etc.

This allows Uiua to be, in most cases, even more terse than other array languages. That being said, Uiua has been designed to remain readable as much as possible, even without named local variables. While writing tacit code can quickly become unwieldy in other array languages, Uiua embraces tacitness as the only way.

Uiua also features built-in functionality for working with images, audio, and GIFs, so once you learn the language, you can very quickly get started writing programs that do interesting things!

Note that Uiua is not yet stable. While most of the core features are unlikely to change much, more complex ones are still in flux. This tutorial is based on the current state of the language, and it is updated as the language changes.

**Important Note**: This website usually uses the most recent development version of Uiua, rather than the stable release. This is so people can test new features easily. Some features available here may not be available if you install Uiua locally in the default way. The version of Uiua currently running on this site is `UIUA_VERSION`.

## Who is this tutorial for?

This tutorial is targeted at people who have at least a little bit of experience with programming. While you don't need to be proficient in any particular language, it will be helpful to understand concepts like variables and functions. However, even if you are a beginner, you can likely find your way through by taking the time to understand each example.

Click the link to the next section to get started!
