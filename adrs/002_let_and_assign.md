# 002: Let and Assign syntax

## Status

Proposed 2021-07-20
Accepted ?

## Context

In Cairo0 there is a "let" statement that represents a "(Re)binding" of an expression to an
identifier.
Some properties:

- Rebinding may not change the type.
- Rebinding affects outer scopes.

This is opposed to "let"/"var" in other languages, that are effectively, shadowing:

- May change type.
- Do not affect outer scopes.

The rebinding in Cairo0 acts more like assignment in other languages.

### Proposal

Two syntaxes: "let", and "assign".

- _Let_ - `let <ident> [: <type>]? = <expr>`. Defines a variable, and shadows a previous definition
  if exists in the scope.
- _Assign_ - `<ident> = <expr>`. Does not change the definition of `<ident>`. Inner and outer scopes
  still see the previous definition, but with a new value.

This behavior is in line with Rust.

_Shadowing_ - although the language will permit this, a specific warning will be emitted in such
cases. Whether the compilation will pass is up to configuration.

Example:

```
// Let syntax declares a variable.
let x : felt252 = 0;
{
    // And shadows in the current scope.
    let x : T = T {};
}
// Here, x is 0.

if (cond) {
    // Assignment does not change type
    x = 5
}
// Here, x may be either 0 or 5.
```

Some non-trivial interactions to consider with other syntax elements:

- `if` branches.
- `for` loop bodies. In particular, assignment is the "only way" to change something inside
  a loop for next iterations. Using a "let" in this context will define a "new" variable that
  will go out of scope at the end of the iteration, and the next iteration will use the "old"
  variable. This is similar to other programming languages.

Advantages:

- Similar to other languages.
- Less confusing than Cairo0.

Disadvantages:

- Less similar to Cairo0.
- Compiler will have to work harder when there is an assignment in a branch: It will need to add a
  "copy" in the other branch. In the short term, we might require "x = x" in the else branch.

## Decision

We decided to accept the proposed syntax.

## Consequences

- Different from Cairo0.
- If we support a reference syntax (e.g. "&x"), it will be confusing why the reference does not
  change when x does. A solution is to make it explicit that a reference actually takes a
  copy (conceptually) using move semantics. For example, "Ref::new(x.clone())".
