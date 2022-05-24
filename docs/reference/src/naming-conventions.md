# Naming conventions

This chapter covers standard naming conventions in Crust code, from casing to punctuation
characters.

## Casing

In general, use `PascalCase` for names of types or traits, while use `snake_case` otherwise.
More precisely:

| Item            | Convention                                       |
|-----------------|--------------------------------------------------|
| Modules         | `snake_case`                                     |
| Types           | `PascalCase`                                     |
| Traits          | `PascalCase`                                     |
| Enum variants   | `PascalCase`                                     |
| Struct fields   | `snake_case`                                     |
| Functions       | `snake_case`                                     |
| Methods         | `snake_case`                                     |
| Variables       | `snake_case`                                     |
| Constants       | `UPPER_CASE`                                     |
| Type parameters | single uppercase letter like `T` or `PascalCase` |

In `PascalCase`, acronyms count as one word: use `Sha` rather than `SHA`.
In `snake_case`, acronyms are lower-cased: `sha3_hash`.

In `snake_case` or `UPPER_CASE`, a _word_ should never consist of a single letter unless it is the
last _word_.
So, we have `btree_map` rather than `b_tree_map`, but `PI_2` rather than `PI2`.
<!-- TODO: More suitable examples -->

Language implementations can emit warnings when named declarations do not follow these rules.

## Underscore (`_foo`)

A value which is not meant to be used must be assigned to `_` or to a variable starting with `_`:

```crust
let _unused = compute_something();
```

Language implementations can emit warnings when a variable **not** starting with underscore is only
written-to but never read. Same applies when a variable starting with underscore is read.

Function names may also start with an underscore.
Such functions are never imported via wildcard `use`, but can be imported explicitly:

```crust
// base.crust
pub fn _wont_be_imported() {}

// error.crust
use base::*
_wont_be_imported();
// Compilation error: undefined function a::_wont_be_imported.

// ok.crust
use base::_wont_be_imported;
_wont_be_imported();
// Compiles fine.
```
