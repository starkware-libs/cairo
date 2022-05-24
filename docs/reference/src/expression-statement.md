# Expression statement

```bnf
EXPR_STMT : EXPR_WITHOUT_BLOCK ";"
          | EXPR_WITH_BLOCK ";"?
```

An _expression statement_ evaluates an [expression](expressions.md) and ignores its result.
Its purpose is to trigger side effects of expression evaluation only.

When an expression that is a [block expression](block-expression.md) or ends with a
code  (`EXPR_WITH_BLOCK`) is used in a context where a statement is permitted, the trailing
semicolon can be omitted.
This can cause an ambiguity between it being parsed as a standalone statement and as a part of
another expression; in this case, it is parsed as a statement.
When trailing semicolon is omitted, the result return type of the expression must be
the [unit type](unit-type.md); otherwise, the result is ignored.
