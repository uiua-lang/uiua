# Tacit Code

# Tacit Code

So far, most of the code examples in this tutorial have been fairly short. While Uiua is great for short, simple code, it is designed to be a general-purpose language. Uiua aims to be decent for everything from Code Golf to command-line utilities, from websites to games.

However, it may not be immediately clear how to write more complex code that does not refer to variable names.

## What Uiua asks of you

When you first start using Uiua beyond just simple examples and challenges, you will likely encounter difficulty passing more than a couple values around.

In disallowing named local variables, Uiua asks something of the programmer that most other languages do not. It asks that you re-orient the way you think about data. How you refer to it, how it flows through a program.

If you pay the price of this re-orientation, Uiua offers you a few things in return.

For one, you end up writing a lot less code. When you don't bind lots of local variables and constantly refer to them in expressions, your code ends up being much shorter. This makes scanning through code require less scrolling and jumping around.

This is one of the many ways that Uiua reduces *ceremony.* Ceremony in a programming language is all the code you have to write that is not directly related to the problem you are trying to solve. This includes everything from minor syntax like braces and keywords to complex boilerplate like interface implementations. Uiua does not eliminate all ceremony (no language can), but it aims to eliminate as much as possible while maintaining a certain level of readability and structure.

This is not to say that local bindings are not useful in other languages. There are two original motivations for Uiua eliminating local variables: simplicity of implementation, and beauty.

*If you are not too concerned with performance and implementation, you can skip this paragraph.* Uiua's arrays are garbage-collected via reference counting. This reference count is also used when mutating an array. For example, if you [reverse]() an array when no duplicates exist, the array is simply reversed in place. However, if at least one other copy of the array exists somewhere, the array's entire buffer will be copied, and this new copy will be reversed instead. If variables could be locally bound, a copy would have to be stored for the duration of the variable's scope. Without complex escape analysis, this could lead to unnecessary copies when using local variables. Because arguments are cleaned up as they are used, arrays only need to be copied as often as is actually necessary!

But in the end, the primary motivation for forbidding local variables is that leads to code that is both beautiful and enjoyable to write. This is, of course, highly subjective, but if you've made it this far into the tutorial, then hopefully you've seen some of that beauty, felt some of that joy.

TODO