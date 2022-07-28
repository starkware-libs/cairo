# 001: Simple trait system

## Status

Proposed 2021-07-29
Accepted ?

## Context
This suggestion introduces a simplistic trait and impl system.
* It is flexible enough to allow anything that can be expressed in rust's trait system, without dynamics.
* It is simple, without edge cases, and hopefully easy on the compiler work.

The suggestion introduces these elements:
### Trait top level item
```
trait MyTrait<A, B>{
  type T;
  func f(a: A, b: B) -> T;
}
```
This item introduces an "interface", with function signatures and associated types that need to be defined by the implementations.

### Impl top level item
```
impl MyImpl<A> for MyTrait<A, felt>{
  type T = A;
  func f(a: A, b: B) -> T { ... }
}
```
This item introduces an implementation, for that interface.
Needs to implement exactly every assocaited type and function in the trait.

### Impl assocaited type and function
```
fn foo(){
  let res: MyImpl<felt>::T = MyImpl<felt>::f(...);
}
```
References functions and types defined in an impl.

### Impl generic arg
```
fn foo<A, VarImpl: impl MyTrait<A, felt>>(){
  VarImpl::f(...);
}
```
Impls have a GenericArg, and can be passed as a GenericValue.

### Inference
At first, no inference would be supported. This will reult in a more verbose and explicit code, but less ergonomic.
These can be added in a later phase.

### Usage
Examples:
* impl from impl
```
impl CloneFromCopy<T, CopyImpl: impl Copy<T>> for Clone<T>{}
```
roughly equivalent to Rust's
```
impl<T: Copy> Clone for T{}
```

## Decision
?

## Consequences
- Traits and impls have no inherent Self type or instance. Their functions are not methods.
- It is not clear if obj.func() syntax will be present and how it will work if it is.
