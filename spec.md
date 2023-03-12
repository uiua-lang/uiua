# Operators and Precedence

These are all the Uiua operators, ordered by increasing precedence

| Operator(s)         | Name(s)                         | Associativity |
| ------------------- | ------------------------------- | ------------- |
| <\|                 | Backpipe                        | Right         |
| \|>                 | Pipe                            | Left          |
| <>, ><              | Self, Flip                      | Left          |
| /, //,              | Left Leaf/Tree                  | Right         |
| \\, \\\\            | Right Leaf/Tree                 | Left          |
| ., ...              | Composition, Double Composition | Left          |
| =, !=, <, <=, >, >= | Comparisons                     | Left          |
| +, -                | Addition, Subtraction           | Left          |
| *, %                | Multiplication, Division        | Left          |
