# Bug Hunt Findings — Hunter #15

Area: diagnostics infra, project config, debug printing.
Crate examined in depth: `cairo-lang-diagnostics` (`crates/cairo-lang-diagnostics/src/diagnostics.rs`).
Also reviewed (no bugs found): `cairo-lang-project`, `cairo-lang-debug`.

---

## Bug 1: `Diagnostics::get_diagnostics_without_duplicates` fails to dedup same-kind, same-location diagnostics when a differently-kinded diagnostic's message sorts between them

**File**: `crates/cairo-lang-diagnostics/src/diagnostics.rs:359-384`

```rust
pub fn get_diagnostics_without_duplicates(&self, db: &'db dyn Database) -> Vec<TEntry> {
    let diagnostic_with_dup = self.get_all();
    if diagnostic_with_dup.is_empty() {
        return diagnostic_with_dup;
    }
    let files_db: &'db dyn Database = db;
    let mut indexed_dup_diagnostic =
        diagnostic_with_dup.iter().enumerate().sorted_by_cached_key(|(idx, diag)| {
            (diag.location(db).user_location(files_db).span, diag.format(db), *idx)
        });
    let mut prev_diagnostic_indexed = indexed_dup_diagnostic.next().unwrap();
    let mut diagnostic_without_dup = vec![prev_diagnostic_indexed];

    for diag in indexed_dup_diagnostic {
        if prev_diagnostic_indexed.1.is_same_kind(diag.1)
            && prev_diagnostic_indexed.1.location(db).user_location(files_db).span
                == diag.1.location(db).user_location(files_db).span
        {
            continue;
        }
        diagnostic_without_dup.push(diag);
        prev_diagnostic_indexed = diag;
    }
    diagnostic_without_dup.sort_by_key(|(idx, _)| *idx);
    diagnostic_without_dup.into_iter().map(|(_, diag)| diag.clone()).collect()
}
```

### Description

The doc comment on this function (lines 355-358) states:

> Get diagnostics without duplication.
>
> Two diagnostics are considered duplicated if both point to the same location in the user
> code, and are of the same kind.

I.e. the contract is: dedup key = `(location, kind)`, independent of the formatted message text.

The implementation instead:
1. Sorts all diagnostics by `(location.span, format(db) message text, original index)`.
2. Walks the sorted sequence and merges an element into the *immediately preceding kept
   element* only if `is_same_kind` is true **and** the span matches (an adjacent-pairs,
   single-pass merge — like a `Vec::dedup_by`).

Because the secondary sort key is the *formatted message string* rather than the diagnostic's
kind, three-or-more diagnostics that share the same location can be interleaved by message text
so that two same-kind duplicates are no longer adjacent to each other in the sorted sequence — a
different diagnostic (of another kind) whose message sorts lexicographically in between them
splits the group. Since the merge only ever compares an element to the *previous kept* element
(not to all previously-seen elements at that location), the two same-kind duplicates survive as
two separate entries in the output, contradicting the documented contract.

Concretely, for three diagnostics at the identical location:
- `kind=1, message="aaa"`
- `kind=2, message="bbb"`
- `kind=1, message="ccc"`

sorting by `(span, message, idx)` yields them already in the order `aaa, bbb, ccc` (since the
span is identical for all three, the message string is the effective sort key). The pairwise
scan compares `(aaa, bbb)` → different kind → keep both; `(bbb, ccc)` → different kind (kind 2 vs
kind 1) → keep both. The two `kind=1` diagnostics at the exact same location are *never*
compared against each other, so both survive, even though per the documented rule only one of
them should.

This is realistic, not just a contrived corner case: it is common for a single spot in user code
to raise both a diagnostic from one subsystem (e.g. name resolution) and a semantically distinct
diagnostic from another (e.g. a lint/warning) at the exact same span, while at the same time the
same span independently produces two occurrences of the *same* diagnostic kind with differing
detail text (e.g. because of re-analysis through two code paths, or macro expansion producing the
same diagnostic kind twice with slightly different interpolated details). Whenever the unrelated
diagnostic's message happens to sort between the two duplicate messages, the dedup silently stops
working and the user sees a duplicated error/warning that should have been collapsed to one.

### Root cause

The sort key used to bring "candidate duplicate" diagnostics into adjacency is
`(span, message_text, idx)`, but the actual duplicate-detection predicate is
`(span, is_same_kind)`. These two are not the same equivalence relation, and grouping by the
wrong key breaks the invariant that the linear/adjacent-pairs merge relies on (that all mutually
"equal" elements — per the predicate actually used to merge — end up contiguous in the sort
order). The message text has no correlation with `is_same_kind`, so any third diagnostic at the
same span whose kind differs and whose message falls lexicographically between two true
duplicates' messages will break the required contiguity and cause the dedup to miss them.

### Test (full code, uses the crate's real public API)

Add to `crates/cairo-lang-diagnostics/src/diagnostics_test.rs` (this mirrors the existing
`SimpleDiag`/`test_diagnostics` pattern already in that file, using the same
`FilesDatabaseForTesting` setup helper):

```rust
// Test diagnostic with a distinct message and a "kind" used for dedup grouping.
#[derive(Clone, Debug, Eq, Hash, PartialEq, salsa::SalsaValue)]
struct KindedDiag<'db> {
    file_id: FileId<'db>,
    kind: u32,
    message: &'static str,
}
impl<'db> DiagnosticEntry<'db> for KindedDiag<'db> {
    fn format(&self, _db: &dyn Database) -> String {
        self.message.into()
    }

    fn location(&self, _db: &'db dyn Database) -> SpanInFile<'db> {
        SpanInFile {
            file_id: self.file_id,
            span: TextSpan::new(TextOffset::START, TextWidth::new_for_testing(6).as_offset()),
        }
    }

    fn is_same_kind(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[test]
fn test_dedup_not_transitive_across_interleaved_kind() {
    let db_val = FilesDatabaseForTesting::default();
    let file_id = setup(&db_val);

    let mut diagnostics: DiagnosticsBuilder<'_, KindedDiag<'_>> = DiagnosticsBuilder::default();
    // Same location, kind 1, message sorts first.
    diagnostics.add(KindedDiag { file_id, kind: 1, message: "aaa" });
    // Same location, different kind, message sorts in between.
    diagnostics.add(KindedDiag { file_id, kind: 2, message: "bbb" });
    // Same location, kind 1 again (same kind as the first), message sorts last.
    diagnostics.add(KindedDiag { file_id, kind: 1, message: "ccc" });

    let deduped = diagnostics.build().get_diagnostics_without_duplicates(&db_val);
    // Per the doc comment on `get_diagnostics_without_duplicates`, "Two diagnostics are
    // considered duplicated if both point to the same location in the user code, and are of
    // the same kind." The first and third diagnostics share both location and kind, so per
    // the documented contract only one of them should survive (2 total entries). The actual
    // adjacent-only dedup after sorting by (span, message, idx) fails to merge them because
    // the differently-kinded "bbb" diagnostic sorts in between, breaking adjacency.
    assert_eq!(
        deduped.len(),
        2,
        "expected same-kind, same-location diagnostics to be deduped regardless of an \
         interleaved different-kind diagnostic, got: {deduped:?}"
    );
}
```

### How to verify

This test was actually written into `diagnostics_test.rs` and run (then reverted, since I was
not asked to leave repo changes) via:

```
cargo test -p cairo-lang-diagnostics test_dedup_not_transitive_across_interleaved_kind -- --nocapture
```

Observed (actual) result — the assertion fails, confirming the bug:

```
thread '...test_dedup_not_transitive_across_interleaved_kind' panicked at ...:
assertion `left == right` failed: expected same-kind, same-location diagnostics to be deduped
regardless of an interleaved different-kind diagnostic, got: [
  KindedDiag { file_id: FileId(180), kind: 1, message: "aaa" },
  KindedDiag { file_id: FileId(180), kind: 2, message: "bbb" },
  KindedDiag { file_id: FileId(180), kind: 1, message: "ccc" }
]
  left: 3
 right: 2
```

All three diagnostics are returned instead of the two the documented contract implies — the
`kind=1` pair at the identical location was not deduplicated because the `kind=2` diagnostic's
message ("bbb") sorted in between them.

### Suggested direction for a fix (not applied)

The grouping key needs to actually correlate with `is_same_kind`, not with the formatted message.
Since `is_same_kind` is a pairwise predicate rather than a total order, a correct fix likely needs
to bucket diagnostics by span first (a stable sort/group by `span` alone), and then, within each
span bucket, do an `O(n^2)`-in-bucket (typically small, so fine) or union-find style pass that
merges any two diagnostics for which `is_same_kind` holds, rather than relying on adjacency from a
secondary sort key that has nothing to do with `is_same_kind`.

---

## Other areas reviewed — no bugs found

- **`crates/cairo-lang-project/src/lib.rs`**: `ProjectConfig::from_file`/`from_directory`,
  `AllCratesConfig::get`, `absolute_crate_root`. Logic is straightforward; `absolute_crate_root`
  correctly joins relative roots onto `base_path` and passes through absolute roots. TOML itself
  rejects duplicate keys in `[crate_roots]`/`[config.override]` tables at parse time (via the
  `toml` crate), so "duplicate crate roots" is not reachable as a distinct bug surface through
  this API. `test.rs`'s serde round-trip tests pass and look correct (checked by inspection,
  matches `CrateSettings`/`ExperimentalFeaturesConfig` default-field behavior).
- **`crates/cairo-lang-debug/src/debug.rs`**: `DebugWithDb` trait and blanket impls for
  `Box`/`Rc`/`Arc`/`Vec`/slices/`Option`/maps/sets/tuples/`id_arena::Id`. All impls forward
  `db`/formatting consistently; the `DebugDbUpcast` tuple-impl chain (`(A,B)`, `(A,B,C)`) upcasts
  the db for the 2nd/3rd fields correctly. Nothing suspicious found; existing test
  (`debug_test.rs`) exercises the derive macro's `hide_field_debug_with_db` attribute correctly.
- **`crates/cairo-lang-diagnostics/src/error_code.rs`**: `ErrorCode` validation
  (`E` + 4 digits), `display_bracketed` for `Option<ErrorCode>`. Straightforward, doctests present
  and correct.
- Rest of `diagnostics.rs` (`DiagnosticsBuilder::add/extend/build`, `Diagnostics::merge`,
  `is_empty`, `has_errors`, `format`/`format_with_severity`, `Severity` ordering): traced through
  carefully; did not find additional logic errors beyond Bug 1.

## Files checked

- `/home/user/cairo/crates/cairo-lang-diagnostics/src/diagnostics.rs`
- `/home/user/cairo/crates/cairo-lang-diagnostics/src/diagnostics_test.rs`
- `/home/user/cairo/crates/cairo-lang-diagnostics/src/error_code.rs`
- `/home/user/cairo/crates/cairo-lang-diagnostics/src/lib.rs`
- `/home/user/cairo/crates/cairo-lang-project/src/lib.rs`
- `/home/user/cairo/crates/cairo-lang-project/src/test.rs`
- `/home/user/cairo/crates/cairo-lang-debug/src/debug.rs`
- `/home/user/cairo/crates/cairo-lang-debug/src/debug_test.rs`
- `/home/user/cairo/crates/cairo-lang-debug/src/lib.rs`
