# Statements

```bnf
STMT : ";"
     | ITEM_STMT
     | LET_STMT
     | EXPR_STMT
```

Crust is an expression-oriented language, where most syntax productions producing values or
causing effects when evaluated are _expressions_.
Many expressions can nest within each other, and sequence of evaluation is driven by precedence
and associativity rules.

There are not a lot of statements kinds, which role is limited to contain explicitly sequential
[expression evaluation](expression-statement.md) and declaring [items](item-statement.md)
and [variables](let-statement.md) in [code blocks].

## Semicolons

Statements are _usually_ separated with a semicolon (`;`).
Extraneous semicolons are ignored.
Semicolons _after_ last statement in code block _may_ be omitted and have a separate semantic
meaning as it makes the statement define returned value of enclosing code block
(see [code blocks] for more details).

[code blocks]: block-expression.md
