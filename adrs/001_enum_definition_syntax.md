# 001: Enum Definition Syntax

## Status

Proposed 2021-07-20

Accepted 2021-07-28

## Context

Proposed syntax for enum (i.e. sum-type):
```
// Definition.
enum MyEnum {
    Variant0: Type0,
    Variant1: Type1,
    ...
}
// Pattern matching.
match enum_val {
    Variant0(value0) => { ... },
    Variant1(value1) => { ... },
}
// Construction.
MyEnum::Variant0(value0)
```

This is a deviation from the rust syntax
```
enum MyEnum {
    Variant0,
    Variant1(T, U),
    variant2{t: T, u: U},
    ...
}
```

Advantages of the suggestion:
* More similar to struct. Simplicity.
* May be less code in the compiler (possibly shared with struct, for example, the syntax and
  parsing).
* Every variant can be used as a type. This allows function to express that they only access a
  specific variant.

Disadvantages:
* Straying away from rust.

## Decision

We decide to accept the proposed syntax.

## Consequences

- Unfamiliar syntax for most users.
- Different from rust.
