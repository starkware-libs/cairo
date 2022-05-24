# Let statement

```bnf
LET_STMT : "let" PATTERN (":" TYPE)? "=" EXPR ";"
```

A _let statement_ introduces a new set of variables, given by an irrefutable pattern.
The pattern is followed optionally by a type annotation and then by an initializer expression.
Variables are _always_ initialized to some value.

When no type annotation is given, the compiler will infer the type, or signal an error if
insufficient type information is available for definite inference.

Any variables introduced by a variable declaration are visible from the point of declaration until
the end of the enclosing block scope, except when they are shadowed by another variable declaration.
