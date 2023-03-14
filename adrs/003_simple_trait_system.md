# 003: Simple trait system

## Status

Proposed 2021-07-29
Accepted ?

## Context
This suggestion introduces a basic `trait` and `impl` system.
* It is flexible enough to allow anything that can be expressed in rust's trait system, excluding
  dynamics.
* It is simple, without edge cases, and hopefully easy on the compiler work.

The suggestion introduces these elements:
### Trait top level item
```
trait MyTrait<A, B> {
  type T;
  fn f(a: A, t: T) -> (T, B);
}
```
This item introduces an "interface", with function signatures and associated types that need to be
defined by the implementations.
Not that there is no `Self` or a type this trait is `for`. It is stand alone.

### Impl top level item
```
impl MyImpl<A> for MyTrait<A, felt252> {
  type T = A;
  fn f(a: A, b: felt252) -> T { ... }
}
```
This item introduces an implementation for this trait.
Needs to implement exactly every associated type and function in the trait.

### Impl associated type and function
```
fn foo() {
  let res: MyImpl<felt252>::T = MyImpl<felt252>::f(...);
}
```
References functions and types defined in an impl.

### Impl generic arg
```
fn foo<A, impl VarImpl: MyTrait<A, felt252>>() {
  VarImpl::f(...);
}
```
Impls have a GenericArg, and can be passed as a GenericValue.

### Inference
At first, no inference would be supported. This will result in a more verbose and explicit code, but
less ergonomic. These can be added in a later phase.

### Usage
Examples:
* Generate impl for X, from any impl for Y.
```
impl CloneFromCopy<T, CopyImpl: impl Copy<T>> for Clone<T> {}
```
roughly equivalent to Rust's
```
impl<T: Copy> Clone for T {}
```

## Decision
?

## Consequences
- Traits and impls have no inherent Self type or instance. Their functions are not methods.
- It is not clear if obj.fn() syntax will be present and how it will work if it is.
