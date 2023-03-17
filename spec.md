# Primitives

## Functions

| Name    | Symbol      | Arity  | Description                |
| ------- | ----------- | ------ | -------------------------- |
| neg     |             | 1      | Negation                   |
| not     |             | 1      | Logical Negation           |
| abs     |             | 1      | Absolute Value             |
| sin     |             | 1      | Sine                       |
| cos     |             | 1      | Cosine                     |
| asin    |             | 1      | Arc Sine                   |
| acos    |             | 1      | Arc Cosine                 |
| floor   |             | 1      | Floor                      |
| ceil    |             | 1      | Ceiling                    |
| round   |             | 1      | Round                      |
|         | =           | 2      | Equality                   |
|         | !=          | 2      | Inequality                 |
|         | <           | 2      | Less Than                  |
|         | >           | 2      | Greater Than               |
|         | <=          | 2      | Less or Equal              |
|         | >=          | 2      | Greater or Equal           |
| mod     |             | 2      | Modulo                     |
| pow     |             | 2      | Power                      |
| min     |             | 2      | Minimum                    |
| max     |             | 2      | Maximum                    |
| atan2   |             | 2      | Arc Tangent                |
|         | +           | 2      | Addition                   |
|         | -           | 2      | Subtraction                |
|         | *           | 2      | Multiplication             |
|         | %           | 2      | Division                   |
|         | .           | 1      | Duplicate                  |
|         | ~           | 2      | Swap                       |
|         | ;           | 1      | pop                        |
|         | !           | varies | Exclusive Fork             |
|         | : (n times) | n + 1  | Adic Fork                  |
| len     |             | 1      | Length                     |
| shape   |             | 1      | Shape                      |
| first   |             | 1      | First                      |
| range   |             | 1      | Range up to exclusive      |
| deshape |             | 1      | Deshape                    |
| match   |             | 2      | Structural Match           |
| join    |             | 2      | Concatenate/Append/Prepend |
| filter  |             | 2      | Filter                     |
| take    |             | 2      | Take                       |
| drop    |             | 2      | Drop                       |
| rotate  |             | 2      | Rotate                     |
| reshape |             | 2      | Reshape                    |
| show    |             | 1      | Show                       |
| print   |             | 1      | Print                      |
| println |             | 1      | Print with newline         |
| string  |             | 1      | Convert to string          |
| scanln  |             | 0      | Read a line from stdin     |
| args    |             | 0      | Command line arguments     |
| var     |             | 1      | Get environment variable   |

## Modifiers

| Name   | Symbol | Arity | Description            |
| ------ | ------ | ----- | ---------------------- |
| Fold   |        | 3     | Fold with intial value |
|        | /      | 2     | Reduce                 |
|        | \\     | 2     | Scan                   |
|        | `      | 2     | Cells                  |
|        | ^      | 2     | Table                  |
| Each   |        | 2     | Each                   |
| Repeat |        | 3     | Repeat                 |