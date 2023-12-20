# Why doesn't Uiua have first-class functions?

2023-12-15

People often ask why Uiua doesn't have first-class functions. That is, functions that can be put on the stack and in arrays.

In the beginning, functions *were* normal array elements. Modifiers popped their functions from the stack like regular values. Functions could be put in arrays, and lists of functions even had some special uses. There was a `! call` function which called the top function on the stack. Boxes were not even a dedicated type. They were just functions that took no arguments and returned a single value.

However, as Uiua's development continued, the language began to rely more and more on stack signatures being well-defined. This property catches errors early, enables some optimizations, and allows modifiers to behave differently depending on their function's siganture. That last point lets us avoid having multiple modifiers that work the same way but on different numbers of arguments. For example, [Factor](https://factorcode.org/) has the words `bi`, `2bi`, `3bi`, `tri`, `2tri`, and `3tri`. Uiua can express all of these and more with just [fork]().

Unfortunately, having first-class functions was at odds with this design. Because functions could be put into arrays and (conditionally) moved around on the stack, the compiler was not able to determine the signature of a function that called a function value. This meant that anywhere the `! call` function was used needed a signature annotation nearby, which you better hope was correct, or the code would break somewhere else. It also incurred additional interpreter overhead to get the functions from arrays and made certain types of optimizations impossible.

Other than these design and implementation concerns, the ability to move functions around on the stack made code much harder to read when it was used. You had to keep in your mind not only the values, but the functions that worked on them as well. They were another value you had to deal with, and the related stack manipulation could get quite messy.

And so I settled on a different approach. Functions were removed as an element type and were put elsewhere in the interpreter. Boxes became a type in their own right. The `! call` function was removed, and `!` was repurposed to be part of defining custom modifiers. [Custom modifiers](/docs/custommodifiers) capture the primary use case of first-class functions: injecting some variable code into a function. While they are technically more limited, their uniform structure makes them easier to both read and write. This change also massively simplified the interpreter, as well as the complexity of the language itself.

Despite the downgrading of functions to second-class status, it should be noted that I do like functional programming languages. I just don't think that first-class functions are a good fit for Uiua. In practice, first-class functions are mostly unnecessary if you have higher-order functions, which array languages have had for decades. APL's operators, J's adverbs and conjunctions, and BQN and Uiua's modifiers are all versions of higher-order functions. They allow the mapping, reduction, and general transformation of data in the same way that first-class functions do in other languages.

Now if only I could find a way to get rid of boxes...