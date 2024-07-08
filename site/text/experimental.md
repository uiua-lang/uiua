## Labels

Labels are a way to give names to values. This is to aid in readability and debugging.

Labels are written with a `$` followed by some letters.

```uiua
# Experimental!
$foo 1 $bar [2 3 4]
$baz "hi!"
```

Labeled values put in an array will lose their labels unless they are [`box`]()ed.

```uiua
# Experimental!
[$a 1 $b 2 $c 3]
{$a 1 $b 2 $c 3}
```

Labels cannot be inspected by code.
