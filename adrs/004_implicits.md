# 004: Implicits

## Status

Proposed 2021-09-19
Accepted ?

# Context

Builtin instance are required for a lot of basic operations. For example, `felt_le` needs the
RangeCheck builtin, so its signature and call might look like this:
```
extern func felt_le(RangeCheck, felt, felt) -> (RangeCheck, bool);
...
let (range_check, res) = felt_le(range_check, a, b);
```

Problems:
* Very verbose on signature and call site.
* Does not play well with operators.
* Functions that should return a single expression, can't be used inside an expression:
  `bool_and(felt_le(1,2),true)`

On the other hand, not specifying them at all has issues as well:
* Function pointers must specify exactly the arguments and return values expected
* How will generic function and traits look like? Will the use have to specify the builtins there
  as well?
* Maybe not zero-cost abstraction. Not explicit. Unclear to the user what is the cost of calling.
* Sometimes the user might want to write the signature and calls exactly, when writing low-level
  code.

# Proposal
The guideline for this proposal is to be ergonomic for the common case, but still explicitly
distinguishing between two "auto" mode and "manual" mode.

## Explicit signature syntax
An additional syntax for specifying implicits:
```
func felt_le{RangeCheck}(a: felt, b: felt) -> bool;
```
This is similar to Cairo0.
## Explicit call syntax
```
let res: bool = func_le{range_check}(a,b);
```
The implicit arguments must exactly match the declaration.

## Implicit syntax
When the implicits are not specified, they are considered "auto":
```
func foo(a: felt, b: felt) -> bool {
  felt_le(a, b)
}
```

## Opt-out example
Possible to optout and handle eveything manually:
```
extern func foo{}(range_check: RangeCheck, a: felt, b: felt) -> (RangeCheck, bool);
```

## Traits.
Trait function can be declared as either "auto" or "manual".
When declared as "manual":
* Every implementing signature must be "manual" with the exact same signature.
* Calls can be either "manual" or "auto", similar to a regular "manual" function.
Example:
```
trait LessThan {
  fn lt{RangeCheck}(a: felt, b: felt) -> bool;
}
impl LessThan {
  fn lt{RangeCheck}(a: felt, b: felt) -> bool { ... } // Ok.
  fn lt(a: felt, b: felt) -> bool { ... } // Not ok.
}
fn main<Impl: LessThan>() {
  Impl::lt(a,b);
  Impl::lt{RangeCheck}(a,b);
}
```

When declared as "auto":
* Implementing signatures can be either "manual" or "auto".
* Every call must be "auto", in an "auto" function.
Example:
```
trait LessThan {
  fn lt(a: felt, b: felt) -> bool;
}
impl LessThan {
  fn lt{RangeCheck}(a: felt, b: felt) -> bool { ... } // Ok.
  fn lt(a: felt, b: felt) -> bool { ... } // Ok.
}
fn main<Impl: LessThan>() {
  Impl::lt(a,b); // Ok.
  Impl::lt{RangeCheck}(a,b) // Not ok.
}
fn foo{RangeCheck}(a: felt, b: felt) -> bool {
  Impl::lt(a,b); // Not ok.
}
```

## Function pointers.
Function pointers must always be explicit.
```
fn run_f(f: fn{RangeCheck}(felt, felt) -> bool);
```
This is not necessarily the same as generic function arguments, which are actually traits:
```
fn run_f<F: Fn>(f: F);
```
