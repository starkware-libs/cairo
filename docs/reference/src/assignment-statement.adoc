# Assignment statement

```bnf
ASSIGN_STMT : IDENTIFIER "=" EXPR ";"
```

An _assignment statement_ moves a value into a specified place.

An assignment statement consists of an expression identifier, _the assignee operand_, followed by an equals sign (=) and a value expression, the assigned value operand. The assignee operand must be a visible local identifier, and the value expression must be castable to its type.

The definition of the operand remains the original one, but with a new value.
