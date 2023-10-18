I have this idea that would be a pretty big change.

Recall the 2 new rank-generic looping functions (currently called `collapse` and `combinate`), which are similar to `level`, but either reduce or table their results respectively. In using these 3 modifiers, there arises a need for a nicer way to write lists of rank-specifiers.

One idea I have, and have partially implemented, is to have a set of constants to represent the most commonly used lists.
As implemented now, these use Greek letters and are lists of the rank specifiers `∞` and `¯1`.

Recall that for `level`, the rank list `[0 0 … 0]` is equivalent to `each`, `[¯1 ¯1 … ¯1]` is equivalent to `rows`, and `[∞ ∞ … ¯1]` is equivalent to `distribute`.

Here is a table of the proposed constants:
| glyph | name    | rank specifier list |
| ----- | ------- | ------------------- |
| `≻`   | `alpha` | `¯1_∞`              |
| `β`   | `beta`  | `¯1_∞_∞`            |
| `γ`   | `gama`  | `¯1_¯1_∞`           |
| `≺`   | `omega` | `∞_¯1`              |
| `ψ`   | `psi`   | `∞_∞_¯1`            |
| `χ`   | `chi`   | `∞_¯1_¯1`           |

One common problem people have is that `fold` and `distribute` often do not take their arguments in the order they want.
However, if `fold`'s current functionality was replaced with that of `collapse`, and `distribute` was removed entirely in favor of `level`, then simply using these rank-specifier constants, you could select the orientation you want.

`level`'s glyph has been changed to `≑` in the examples below.

This code using `distribute`
```
∺↻ 2 ↯3_4⇡12
```
becomes this code using `level` `omega`
```
≑≺↻ 2 ↯3_4⇡12
```
The rank list `∞_¯1` is called `≺` because the array being iterated over is the *last* one.

This code, which is annoying because the aguments to `distribute` have to be `flip`ped twice
```
∺(⊂∶)∶ ↯3_3⇡9 5
```
becomes
```
≑≻⊂ ↯3_3⇡9 5
```
The rank list `¯1_∞` is called `≻` because the array being iterated over is the *first* one.

The `length` `3` rank list constants let you iterate over 2 arrays while keeping 1 fixed, or vice-versa.

For example, if you wanted to join each of the pairs of rows of two arrays then rotate them by a constant, you could use `level` `gamma`.
```
f ← ≑γ(↻∶⊂)
f [1_2_3 4_5_6] [7_8 9_10] 2
```
output:
```
╭─
╷ 3 7  8 1 2
  6 9 10 4 5
             ╯
```

By having the rank list constants be made of `¯1`s and `∞`s, we can easily specify rank `0` for elements, `1` for lists, `2` for matrices, etc, by just `add`ing to the list.

The new functionality for `fold` would make it encapsulate both behaviors you may be familiar with from functional languages, often called `foldl` (`∧≺`) and `foldr` (`∧≻`). It would also allow multiple arities.

To be clear, this change would:

- Change `fold`'s functionality to be more general
- Remove `distribute`
- Change `level`'s glyph to `≑` (which I'd like to do anyway)
- Add 6 new constants for rank-specifier lists

`each` and `rows`, which are the other specializations of `level`, would not be affected.

`table` and `cross`, which are specializations of `combinate`, would not be affected.

`reduce`, which is a specialization of `collapse`, would not be affected.