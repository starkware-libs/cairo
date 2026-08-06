# Bug Hunter #13 — cairo-lang-doc (doc generation tooling)

Scope: `crates/cairo-lang-doc/src/` — signature reconstruction, path/link
generation, markdown/signature rendering. Excludes doc-comment HardBreak and
Event-handling issues (already filed by others), and excludes
`crates/cairo-lang-syntax/src/node/ast.rs` per repo instructions.

Both bugs below were reproduced by compiling and running real tests against
the crate's public `DocGroup` API (`cargo test -p cairo-lang-doc`), then the
scratch test files were deleted and the crate was restored to a clean git
state (`git status --porcelain -- crates/cairo-lang-doc/` shows no diff).

---

## Bug 1: Single-element tuple types lose their disambiguating trailing comma in rendered signatures

**File/location**: `crates/cairo-lang-doc/src/documentable_formatter.rs:156-163`, inside `HirFormatter::write_type`.

```rust
if let TypeLongId::Tuple(vec_types) = element_type.long(self.db) {
    self.write_str("(")?;
    let mut count = vec_types.len();
    for t in vec_types {
        self.write_type(None, *t, if count == 1 { None } else { Some(", ") }, full_path)?;
        count -= 1;
    }
    self.write_str(")")?;
}
```

**Description**: When rendering a tuple type with exactly one element, the loop
writes `(` + the single element + `)` with **no trailing comma**, producing
e.g. `(felt252)` instead of the syntactically-required `(felt252,)`.

In valid Cairo (like Rust), a parenthesized single type is *not* a tuple — the
trailing comma is what distinguishes the 1-tuple type `(T,)` from a plain
parenthesized expression. The rendered signature is therefore invalid/
misleading Cairo code for any function, constant, struct member, or enum
variant whose type is a single-element tuple.

**Root cause**: The postfix-selection logic `if count == 1 { None } else { Some(", ") }` is copied from generic-list-style rendering where the last element should have no trailing separator. It fails to special-case the 1-tuple, unlike the crate's own general-purpose type formatter, `TypeLongId::fmt` in `crates/cairo-lang-semantic/src/types.rs` (around line 319-325), which explicitly does:

```rust
TypeLongId::Tuple(inner_types) => {
    if inner_types.len() == 1 {
        write!(f, "({},)", inner_types[0].format(db))
    } else {
        write!(f, "({})", inner_types.iter().map(|ty| ty.format(db)).format(", "))
    }
}
```

i.e. the semantic layer already knows single-element tuples must keep the
comma; `documentable_formatter.rs`'s independent tuple-writing logic in
`write_type` simply doesn't apply the same rule, since it recurses
per-element and derives the separator from the *remaining count* rather than
the *total* length.

This affects every code path that calls `HirFormatter::write_type` on a
tuple-typed value: function/trait-function/impl-function/extern-function
return types and parameters, struct member types, enum variant types,
constant types, and type aliases — anywhere a `(T,)` type appears in a
documented signature.

**Full test code** (add to `crates/cairo-lang-doc/src/tests/`, wired into
`mod.rs`, or run as a standalone `#[test]` inside the crate — uses only the
public `DocGroup`/`TestDatabase` API used by the crate's own tests):

```rust
use cairo_lang_defs::ids::{LookupItemId, ModuleId, ModuleItemId};

use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::tests::test_utils::{TestDatabase, setup_test_module, test_crate_id};

#[test]
fn single_element_tuple_return_type_signature() {
    let mut db_val = TestDatabase::new().unwrap();
    setup_test_module(
        &mut db_val,
        "fn foo() -> (felt252,) {\n    (1,)\n}\n",
    );
    let db = &db_val;
    let crate_id = test_crate_id(db);
    let module_data = ModuleId::CrateRoot(crate_id).module_data(db).unwrap();
    let items = module_data.items(db);
    let free_fn = items
        .iter()
        .find_map(|item| match item {
            ModuleItemId::FreeFunction(id) => Some(*id),
            _ => None,
        })
        .unwrap();
    let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::FreeFunction(
        free_fn,
    )));
    let signature = db.get_item_signature(id).unwrap();
    // BUG: actual rendered signature is "fn foo() -> (felt252)" (invalid Cairo —
    // reads as a parenthesized felt252, not a 1-tuple). Correct signature
    // should preserve the trailing comma.
    assert_eq!(signature, "fn foo() -> (felt252,)");
}
```

**Actually observed when run** (via `cargo test -p cairo-lang-doc --lib
bughunt -- --nocapture`):
```
SIGNATURE: fn foo() -> (felt252)
thread '...' panicked at ...:
assertion `left == right` failed
  left: "fn foo() -> (felt252)"
 right: "fn foo() -> (felt252,)"
```

**How to verify**: Drop the test above into a new file under
`crates/cairo-lang-doc/src/tests/` (e.g. `bughunt_test.rs`), add `pub mod
bughunt_test;` to `crates/cairo-lang-doc/src/tests/mod.rs`, then run
`cargo test -p cairo-lang-doc --lib bughunt`. It fails, printing the
malformed signature `fn foo() -> (felt252)`. The same defect can be
triggered for struct members (`struct S { x: (felt252,) }`), enum variants,
and constants, since they all funnel through the same `write_type` tuple
branch.

---

## Bug 2: Associated impls declared inside a trait (`TraitItemId::Impl`) always render with no signature, unlike their impl-side counterpart

**File/location**: `crates/cairo-lang-doc/src/documentable_formatter.rs:71` (dispatch table in `get_item_signature_with_links`):

```rust
LookupItemId::TraitItem(item_id) => match item_id {
    TraitItemId::Function(item_id) => item_id.get_signature_with_links(&mut f),
    TraitItemId::Constant(item_id) => item_id.get_signature_with_links(&mut f),
    TraitItemId::Type(item_id) => item_id.get_signature_with_links(&mut f),
    TraitItemId::Impl(_) => (None, vec![]),
},
```

**Description**: Cairo traits can declare an associated impl requirement,
e.g.:

```cairo
trait Bar<T> {}
trait Foo<T> {
    impl X: Bar<T>;
}
```

(`ast::TraitItemImpl`, syntax `impl NAME: TRAIT_PATH;`, defined in
`crates/cairo-lang-syntax-codegen/src/cairo_spec.rs:603-610`, backed by
`TraitImplId`/`TraitItemId::Impl` in `cairo-lang-defs`). This is a real,
distinct, documentable item kind — `DocumentableItemId` can be constructed
for it (`LookupItemId::TraitItem(TraitItemId::Impl(trait_impl_id))`) and
`db.get_item_documentation(id)` will happily return its doc comment. But
`db.get_item_signature(id)` unconditionally returns `None` for it, whereas
the *matching* item on the implementation side — an associated impl written
inside an `impl` block (`ImplItemId::Impl` / `ImplImplDefId`) — has full
signature support via `impl<'db> HirDisplay<'db> for ImplImplDefId<'db>`
(same file, ~line 517), rendering e.g. `impl X = SomeImpl;`.

This is an inconsistent/incomplete render: any tool walking `TraitItemId`
variants generically (as `DocumentableItemId` construction allows) gets a
silent `None` for this one variant while every sibling variant
(`Function`/`Constant`/`Type`) is fully supported, and the impl-side analog
of the very same concept is fully supported too. A user documenting a trait
with an associated-impl requirement gets no signature line for it at all —
a documentation/behavior mismatch (docs exist, signature silently missing)
rather than a clean "not applicable" case like `ModuleItemId::Use`.

**Root cause**: The formatter dispatch table has no `HirDisplay` impl for
`TraitImplId` and hard-codes `(None, vec![])` instead of formatting something
like `impl X: Bar<T>;`. Note the crate's own test harnesses
(`crates/cairo-lang-doc/src/tests/test.rs` and
`.../test_documentable_formatter.rs`, `document_trait_with_items`) also never
enumerate `db.trait_impls(trait_id)` at all — only constants/types/functions
— so this gap is invisible in the existing golden tests, which is presumably
why it was never caught.

**Full test code**:

```rust
use cairo_lang_defs::ids::{LookupItemId, ModuleId, ModuleItemId, TraitItemId};
use cairo_lang_semantic::items::trt::TraitSemantic;

use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::tests::test_utils::{TestDatabase, setup_test_module, test_crate_id};

#[test]
fn trait_associated_impl_signature_is_missing() {
    let mut db_val = TestDatabase::new().unwrap();
    setup_test_module(
        &mut db_val,
        "trait Bar<T> {}\ntrait Foo<T> {\n    impl X: Bar<T>;\n}\n",
    );
    let db = &db_val;
    let crate_id = test_crate_id(db);
    let module_data = ModuleId::CrateRoot(crate_id).module_data(db).unwrap();
    let items = module_data.items(db);
    let trait_id = items
        .iter()
        .find_map(|item| match item {
            ModuleItemId::Trait(id) => {
                let name = cairo_lang_defs::ids::NamedLanguageElementId::name(id, db);
                if name.long(db) == "Foo" { Some(*id) } else { None }
            }
            _ => None,
        })
        .unwrap();

    let trait_impls = db.trait_impls(trait_id).unwrap();
    let (_, trait_impl_id) = trait_impls.iter().next().unwrap();

    let id = DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Impl(*trait_impl_id)));
    let signature = db.get_item_signature(id);
    // BUG: signature is None even though `X: Bar<T>` is a perfectly valid,
    // documentable trait item with a well-defined textual form (e.g.
    // "impl X: Bar<T>;"), and its impl-side analog (ImplImplDefId) IS
    // rendered correctly by this same module.
    assert!(signature.is_none());
}
```

**Actually observed when run**: `SIGNATURE: None` — test passes, confirming
the (buggy) behavior deterministically.

**How to verify**: Add the test above under
`crates/cairo-lang-doc/src/tests/` (wired via `mod.rs`) and run `cargo test
-p cairo-lang-doc --lib trait_associated_impl`. It will pass, showing
`get_item_signature` returns `None`. Contrast with an equivalent impl-side
snippet (`impl SomeImpl of Bar<T> { impl X = OtherImpl; }`) via
`ImplItemId::Impl`, which does produce a real signature string through the
`ImplImplDefId` `HirDisplay` impl in the same file.

**Severity note**: lower severity than Bug 1 — this is a missing-feature /
silent-gap issue (no signature rendered) rather than an incorrect signature
being rendered, and it is somewhat plausible the maintainers consider trait
associated-impl signatures out of scope for now. Flagging as suspected gap
worth a decision either way, since it's asymmetric with the fully-supported
`ImplImplDefId` case and with the other three `TraitItemId` variants.

---

## Areas checked but no bug found / not pursued further

- `crates/cairo-lang-doc/src/db.rs`: outer/inner/module-level doc-comment
  extraction (`extract_item_outer_documentation`,
  `extract_item_inner_documentation_from_raw_text`,
  `extract_item_module_level_documentation_from_file`) — logic for stripping
  `///`/`//!` prefixes and merging module/outer/inner comment sources looked
  consistent; did not find a reproducible mis-extraction distinct from the
  already-filed HardBreak/Event issues.
- `crates/cairo-lang-doc/src/location_links.rs`: `get_offsets` /
  `move_location_links`, the diff-based remapping of `LocationLink` offsets
  after the signature is re-formatted by the Cairo formatter. The algorithm
  is intricate (token-kind-based diffing plus a cumulative-shift application
  with several boundary conditions on `<`/`>=`/`>`), and is a plausible place
  for off-by-one link-offset bugs under formatter-inserted-whitespace edge
  cases, but I could not construct a concrete failing Cairo snippet within
  budget; not enough evidence to report as a demonstrated or even
  well-justified suspected bug. Worth a closer look by someone with more time
  budget for this specific function.
- `crates/cairo-lang-doc/src/helpers.rs`: `extract_and_format` /
  `format_final_part` (full-path-to-short-name stripping used inside
  rendered signatures) and `get_generic_params` — traced through nested
  generics, snapshots (`@T`), tuples, and impl generic params with `+Trait`
  bounds; behavior looked correct for the cases traced.
- `crates/cairo-lang-doc/src/signature_data.rs`: per-item-kind signature-data
  retrieval (structs, enums, functions, traits, impls, aliases, externs) —
  each retriever's field population was cross-checked against its
  `HirDisplay` consumer in `documentable_formatter.rs`; no mismatches found.
- `crates/cairo-lang-doc/src/documentable_item.rs`: trivial ID wrapper /
  `name()`/`stable_location()` dispatch — no issues.
- `crates/cairo-lang-doc/src/parser.rs`: reviewed for issues *other than* the
  already-filed HardBreak-dropped and Event-handling bugs (per team memory);
  did not pursue further to avoid duplicating off-limits territory.

## Files checked

- `crates/cairo-lang-doc/src/documentable_item.rs`
- `crates/cairo-lang-doc/src/db.rs`
- `crates/cairo-lang-doc/src/documentable_formatter.rs`
- `crates/cairo-lang-doc/src/location_links.rs`
- `crates/cairo-lang-doc/src/helpers.rs`
- `crates/cairo-lang-doc/src/signature_data.rs`
- `crates/cairo-lang-doc/src/parser.rs` (skimmed only, off-limits overlap)
- `crates/cairo-lang-doc/src/signature_errors.rs`
- `crates/cairo-lang-doc/src/tests/test.rs`
- `crates/cairo-lang-doc/src/tests/test_documentable_formatter.rs`
- `crates/cairo-lang-doc/src/tests/test_utils.rs`
- `crates/cairo-lang-semantic/src/types.rs` (for cross-reference on tuple
  formatting ground truth)
- `crates/cairo-lang-syntax-codegen/src/cairo_spec.rs` (for `TraitItemImpl` /
  `TypeClause` AST shape, per CLAUDE.md instruction to use codegen instead of
  `ast.rs`)
- `crates/cairo-lang-defs/src/ids.rs` (for `TraitItemId`/`TraitImplId` shape)

Both reported bugs were verified by actually running `cargo test -p
cairo-lang-doc` against temporary test files; those temporary files were
deleted afterward and `crates/cairo-lang-doc/` is left with a clean `git
status`.
