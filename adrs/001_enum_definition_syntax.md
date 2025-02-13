# 001: Enum Definition Syntax

## Status

Proposed 2021-07-20

Accepted 2021-08-04

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
    MyEnum::Variant0(value0) => { ... },
    MyEnum::Variant1(value1) => { ... },
}

// Construction.
MyEnum::Variant0(value0)
```

This is a deviation from the rust syntax
```
enum MyEnum {
    Variant0,
    Variant1(T, U),
    Variant2{t: T, u: U},
    ...
}
```

Advantages of the suggestion:
* More similar to struct. Simplicity.
* May be less code in the compiler (possibly shared with struct, for example, the syntax and
  parsing).
* Every variant has a type. This allows functions to express that they only access a
  specific variant. For example, when the variant is defined as `Variant0: Type0`, we can always
  refer to `Type0`, it's a real type.

Disadvantages:
* Straying away from rust.
* Unfamiliar syntax for most users.
* Tuple types need an extra parentheses. On `Variant0: (A, B)`, construction is using
  `Variant0((a, b))`.

## Open questions

Pattern matching might be made less verbose. Suggestions:
```
MyEnum::Variant0(value0) =>
Variant0(value0) =>
variant0(value0) =>
VARIANT0(value0) =>
```
