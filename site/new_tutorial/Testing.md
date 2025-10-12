# Testing

Testing is important for ensuring that code works as expected, especially in a dynamic language like Uiua.

The language has a couple built-in mechanisms for streamlining testing.

## [assert](/docs/assert) Tests

The [assert](/docs/assert) function will return an error when its second argument is anything other than `1`.

While this can be used in some forms of [control flow](/tutorial/controlflow), it is also useful for testing.

When testing is enabled (as it is in all editors on this website), a line with [assert](/docs/assert) at the beginning will be interpreted as a test.

```uiua should fail
Square ← ˙×
˙⍤ =9 Square 3
˙⍤ =12 Square 4
˙⍤ =225 Square 15
```

As you can see, these when one of these top-level [assert](/docs/assert)s fails, the program continues running.
When the program is done running, the successes and failures are aggregated and displayed.

## Test Scopes

A [scoped module](/tutorial/modules#scoped-modules) with the name `test` is special in that code inside it will *not* be run when using the `uiua run` command. The `uiua test` and `uiua watch` commands *will* run test scope code. Test scopes are also always run on this website.

```uiua
Square ← ˙×
┌─╴test
  ˙⍤ =9 Square 3
  ˙⍤ =225 Square 15
└─╴
```

Importantly, `uiua run` will not run test scopes, but it will *also* not interpret any [assert](/docs/assert)s as tests.

## Testing Patterns

The first argument to [assert](/docs/assert) is the value that will be thrown if the assertion fails. In the examples above, we have simply been duplicating the test value with [self](/docs/self). We can throw a message instead.

If the result does not match the expectation, that incorrect result will be thrown.

```uiua should fail
Square ← ˙×
⍤"3² is not 9!" =9 Square 3
⍤"4² is not 12!" =12 Square 4
⍤"15² is not 225!" =225 Square 15
```

One nice pattern for writing tests is to put the expected result before the test computation and use `assert``with``match`. This has the nice mnemonic spelling "assert with match".

This should be immediately followed by the expected result.

```uiua should fail
Square ← ˙×
⍤⤙≍ 9 Square 3
⍤⤙≍ 12 Square 4
⍤⤙≍ 225 Square 15
⍤⤙≍ [1 2 3] ⊂ 1 [2 3]
```

Notice how the expected value appears in the error message.

```uiua
F ← ⍣⍜⊢(+1)∘
┌─╴test
  ⍤⤙≍ [2 2 3] F [1 2 3]
  ⍤⤙≍ [4 5 7] F [3 5 7]
  ⍤⤙≍ "viua" F "uiua"
  ⍤⤙≍ [] F []
└─╴
```

If a function returns multiple values, you can simply box them with `{}`s.

```uiua
⍤⤙≍ {1 2_3} {°⊂} [1 2 3]
⍤⤙≍ {1_2 3_4_5} {⊃↙↘2} [1 2 3 4 5]
```

---

Hooray! You've reached the end of the tutorial!

To keep going with Uiua, you can check out:

`END OF TUTORIAL LIST`