# Literal expressions

```bnf
LITERAL_EXPR : INTEGER_LITERAL
             | STRING_LITERAL
             | BOOLEAN_LITERAL
             | UNIT_LITERAL

UNIT_LITERAL : "()"
```

A _literal expression_ directly describes a number, string, boolean or [unit] value.
Literals consist of one `literal` token and its type if determined by token's value.

[unit]: unit-type.md
