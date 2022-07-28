# 001: Let and Assign syntax

## Status

Proposed 2021-07-20
Accepted 2021-07-28

## Context

In Cairo0 there is a "let" statmement that represents a "(Re)binding" of an expression to an
identifier.
Some properties:
* Rebinding may not change the type.
* Rebinding affects outer scopes.

This is opposed to "let"/"var" in other languages, that are effectively, shadowing:
* May change type.
* Do not affect outer scopes.

The rebinding in Cairo0 acts more like assignment in other languages.

### Proposal
Two syntaxes: Let, and assign.
* *Let* - "let *ident* = *expr*". defines a variable, and shadows a previous definition if exists in the scope.
* *Assign* - "*ident* = *expr*". does not change the definition of x. Inner and outer scopes still see the previous
  definition, but with a new value.

Example:
```
// Let syntax declares a variable.
let x : felt = 0;
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

Non trivial interactions:
* If branches.
* For loop bodies. In particular, assignment is the "only way" to change something inside
  a loop.

Advantages:
* Similar to other languages.
* Less confusing.

Disadvantages:
* Less similar to Cairo0.

## Decision

We decide to accept the proposed syntax.

## Consequences

- Compiler will have to work harder when there is an assignment in a branch: It will need
  to add a "copy" in the other branch.
- Different from Cairo0.
- If we support a reference syntax (e.g. "&x"), it will be confusing why the reference does not
  change when x does. A solution is not to make it explicit that a refernce actually takes a
  copy (conceptually). For example, "Ref::new(x.clone())".
