# Negation operators

```bnf
NEGATION_OP_EXPR : "-" EXPR
                 | "!" EXPR
```

The following table summarizes the behavior of negation operators and which traits are used to
overload them for other types:

| Symbol | Operation   | Accepted types             | Overloading trait |
|--------|-------------|----------------------------|-------------------|
| `-`    | Negation    | `felt`, `int`, `i*` family | `std::ops::Neg`   |
| `!`    | Logical NOT | `bool`                     | `std::ops::Not`   |
