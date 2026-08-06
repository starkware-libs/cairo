# Supervisor #3 — Verdicts (Hunters 11–15)

Repo: /home/user/cairo. Each verdict below is from my own source trace, not the
hunter's summary. Test legitimacy judged against "normal idiomatic usage".

Summary table:

| Finding | Verdict |
|---|---|
| H11-F1 poseidon comment stale | **confirmed** (doc-only, low) |
| H11-Obs Blake2 `COMPILED_CLASS_V1` prefix | **rejected as bug** (unverifiable, not demonstrated) |
| H12-B1 formatter width in bytes not chars | **confirmed** (novel) |
| H13-B1 1-tuple missing trailing comma | **confirmed** (novel) |
| H13-B2 trait associated-impl no signature | **suspected** (gap, not demonstrated as a bug) |
| H14 (no bug; label mislabel) | **rejected as bug** (agree: no bug) |
| H15-B1 dedup adjacency failure | **suspected, NOT demonstrated** (downgraded — unreachable in real usage) |

---

## H11 — starknet-classes

### H11-F1: `bytecode_hash_node` comment says "poseidon" — CONFIRMED (doc-only)
Traced `casm_contract_class.rs`. Confirmed: `bytecode_hash_node<H: StarkHash>`
(line 202) is generic and computes `H::hash_array(&hash_elements) + 1` (line 221),
but the inner-node comment at line 213 says `// Compute 1 + poseidon(...)`. The
public entry `compiled_class_hash()` (line 133) instantiates `H = Blake2Felt252`;
only `legacy_compiled_class_hash()` (line 139) uses `Poseidon`. So the comment is
genuinely stale/misleading for the current (Blake2) hash. This is a real
doc/behavior mismatch but documentation-only, no runtime effect (golden tests pin
both flavors). Matches the supervisor's pre-note that this is a stale-doc; not a
consensus/logic bug. Verdict: **confirmed, low severity, doc-only.**

### H11-Obs: Blake2 reuses `COMPILED_CLASS_V1` prefix — REJECTED as a bug
`compiled_class_hash_inner` (line 189) uses the same `b"COMPILED_CLASS_V1"` felt
for both hash flavors. The hunter explicitly did NOT claim a bug and flagged it as
unverifiable-from-repo. I agree: the correct domain separator is defined by the
Starknet-OS/SNIP spec, not derivable here; in-repo goldens are self-referential.
Not a filed bug — recommend human spec cross-check only. **No action.**

Cleared items (class-hash order, keccak 250-bit truncation, version gates,
segmentation underflow, ABI sanity_check, felt252_serde) — I spot-checked the
class-hash component order and keccak `result[0] &= 3` and concur they are correct.
The known `UnsupportedLibfuncAtVersion` backslash defect was correctly NOT re-filed.

---

## H12 — formatter line width in bytes — CONFIRMED (novel)

Traced `formatter_impl.rs`. Confirmed both manifestations:
- `LineComponent::width` (line 278): `Self::Token(s) => s.len()` (line 280) and the
  trailing-`Comment` branch `content.len()` (line 287) — both UTF-8 **byte** counts.
- `LineTree::width`/`width_between` doc comments (lines 432, 436) explicitly say
  "The width, in number of chars" — so the doc contract is chars, the code counts
  bytes. Genuine doc/behavior contradiction, not merely undocumented.
- `format_leading_comment` (line 810-812): `current_line.content.len() + word.len()
  <= max_comment_width` compares byte lengths against a column budget.

Because Cairo tokens/strings are ASCII, only comments (which legitimately carry
non-ASCII UTF-8) are affected, producing spurious breaks / early wraps. Impact is
silent-wrong (still idempotent).

Test legitimacy: the regression test drives the public `get_formatted_file` on
`SimpleParserDatabase` at default config (max 100) — exactly the crate's own
`test.rs` path, with ordinary Cairo containing non-ASCII comments. Idiomatic input,
no internals reached. **Legitimate. Confirmed, novel.**

---

## H13 — cairo-lang-doc

### H13-B1: 1-tuple loses trailing comma — CONFIRMED (novel)
Traced `documentable_formatter.rs` `write_type` (lines 156-163). For a
`TypeLongId::Tuple`, `count = vec_types.len()`; each element is written with postfix
`if count == 1 { None } else { Some(", ") }`. For a 1-tuple, len==1 so the single
element gets postfix `None` → output `(felt252)` with no trailing comma. Confirmed
ground truth in `cairo-lang-semantic/src/types.rs:319-325`: the canonical
`TypeLongId` formatter special-cases `inner_types.len() == 1` as `"({},)"`. So the
doc formatter's independent tuple logic is inconsistent and emits invalid Cairo
(`(felt252)` parses as a parenthesized type, not a 1-tuple).

Reachability: `get_item_signature` → `get_signature_with_links` → function
`HirDisplay` → `write_type` on the return/param/member type. Reached by documenting
any item with a `(T,)` type — idiomatic. Test uses the public `DocGroup`/
`TestDatabase` API and a normal Cairo source `fn foo() -> (felt252,)`; asserts the
rendered signature equals the correct `(felt252,)` and fails on today's `(felt252)`.
**Legitimate. Confirmed, novel.**

### H13-B2: trait associated-impl (`TraitItemId::Impl`) renders no signature — SUSPECTED (gap, not demonstrated)
Confirmed the dispatch: `documentable_formatter.rs:71` hard-codes
`TraitItemId::Impl(_) => (None, vec![])`, while the impl-side analog
`ImplItemId::Impl(item_id)` (line 77) is fully rendered via the `ImplImplDefId`
`HirDisplay`. So the asymmetry the hunter describes is real, and the other three
`TraitItemId` variants (Function/Constant/Type) are all supported.

However: this is a missing-feature / silent gap, not incorrect output. There is no
doc contract asserting a trait associated-impl must produce a signature, so this is
an undocumented omission rather than a documented contradiction. Crucially, the
hunter's own test asserts `signature.is_none()` — i.e. it *documents current
behavior* and passes; it does not demonstrate a contract violation via an oracle.
The hunter self-rated it "suspected gap worth a decision." I concur.
**Verdict: suspected (legitimate asymmetry/gap), not demonstrated as a bug.**

Cleared items (db.rs doc extraction, location_links, helpers, signature_data) —
consistent with a scan; location_links off-by-one remains an untested suspicion by
the hunter's own admission, not reported.

---

## H14 — cairo-lang-utils — NO BUG (agree)

Confirmed `range.rs` does **not** exist in the crate (stale assignment entry).
Confirmed the one flagged item is a cosmetic test-label mislabel, NOT a logic bug:
`bigint_tests/serde.rs:11-12` — the case carrying `is_negative = true` is named
`"positive"` and the `false` case `"negative"`; but the test body negates the value
when `is_negative` is true (lines 16-17), so the labels are swapped relative to what
they exercise. The assertions use the boolean directly, so behavior is unaffected.
H14 correctly did not file this as a bug. I independently spot-checked bigint serde/
parity-scale-codec reasoning (sign packing, 6-bit length ≤63, zero round-trip) and
found nothing. **No bug. Agree with H14.**

---

## H15 — diagnostics dedup — SUSPECTED, NOT DEMONSTRATED (downgraded)

The logic flaw itself is real and I reproduced the trace on
`diagnostics.rs:359-384`: `get_diagnostics_without_duplicates` sorts by
`(span, format(db) message, idx)` (line 367) but merges only against the *previous
kept* element using predicate `is_same_kind && same span` (lines 373-377). For the
hunter's triple `aaa(k1), bbb(k2), ccc(k1)` at one span, the two `k1` entries are
never compared to each other, so both survive — the sort key (message text) does
not correlate with the merge predicate (`is_same_kind`).

**But this cannot occur through idiomatic usage, so I downgrade it.** The bug needs
two entries that are `is_same_kind` yet have *different* `format` output. I checked
every `DiagnosticEntry` impl in the tree:
- parser (`cairo-lang-parser/src/diagnostic.rs:291`), semantic
  (`cairo-lang-semantic/src/diagnostic.rs:1510`), lowering
  (`cairo-lang-lowering/src/diagnostic.rs:126`): all define
  `is_same_kind = (other.kind == self.kind)`, i.e. **full** kind equality.
- Their `format` is a pure function of `self.kind` (verified parser
  `diagnostic.rs:134-135` `match &self.kind`, ignoring `db`; semantic/lowering follow
  the same pattern). The kind enum carries all interpolated payload.

Therefore, for any real diagnostic, `is_same_kind` (equal kind) at the same span ⟹
**identical** `format` string ⟹ the true duplicates sort contiguously and the
adjacent merge collapses them correctly. No third diagnostic can sort strictly
between two byte-identical messages. The only impls that decouple "kind" from
"message" are the two *test-only* helpers (`diagnostics_test.rs`,
`cairo-lang-plugins/src/test_utils.rs`).

The hunter's `KindedDiag` gives `kind: u32` and `message: &str` as independent
fields — a state no production `DiagnosticEntry` produces (real messages are derived
from the kind). Per the "normal idiomatic usage / no contrived states" rule, this
test manufactures the failing condition. **Verdict: suspected latent code smell
(sort key doesn't match the merge predicate), NOT demonstrated through real usage —
downgraded from the hunter's "confirmed".**

Cleared items (project config, debug, error_code, rest of diagnostics.rs) — concur.

---

## Files checked (merged across H11–H15, deduped)

### cairo-lang-starknet-classes (H11)
- crates/cairo-lang-starknet-classes/src/casm_contract_class.rs
- crates/cairo-lang-starknet-classes/src/contract_class.rs
- crates/cairo-lang-starknet-classes/src/abi.rs
- crates/cairo-lang-starknet-classes/src/compiler_version.rs
- crates/cairo-lang-starknet-classes/src/compiler_version_test.rs
- crates/cairo-lang-starknet-classes/src/allowed_libfuncs.rs
- crates/cairo-lang-starknet-classes/src/allowed_libfuncs_lists/{audited,all}.json
- crates/cairo-lang-starknet-classes/src/keccak.rs
- crates/cairo-lang-starknet-classes/src/keccak_test.rs
- crates/cairo-lang-starknet-classes/src/contract_segmentation.rs
- crates/cairo-lang-starknet-classes/src/felt252_serde.rs
- crates/cairo-lang-starknet-classes/src/casm_contract_class_test.rs
- crates/cairo-lang-starknet-classes/src/compiled_class_hash_test_data/contracts (goldens)
- crates/cairo-lang-starknet/src/compile.rs

### cairo-lang-formatter (H12)
- crates/cairo-lang-formatter/src/formatter_impl.rs
- crates/cairo-lang-formatter/src/node_properties.rs
- crates/cairo-lang-formatter/src/lib.rs
- crates/cairo-lang-formatter/src/test.rs
- crates/cairo-lang-formatter/test_data/** (comment_overflow, trailing_comment, linebreaking, ...)

### cairo-lang-doc (H13)
- crates/cairo-lang-doc/src/documentable_item.rs
- crates/cairo-lang-doc/src/db.rs
- crates/cairo-lang-doc/src/documentable_formatter.rs
- crates/cairo-lang-doc/src/location_links.rs
- crates/cairo-lang-doc/src/helpers.rs
- crates/cairo-lang-doc/src/signature_data.rs
- crates/cairo-lang-doc/src/parser.rs (skimmed)
- crates/cairo-lang-doc/src/signature_errors.rs
- crates/cairo-lang-doc/src/tests/test.rs
- crates/cairo-lang-doc/src/tests/test_documentable_formatter.rs
- crates/cairo-lang-doc/src/tests/test_utils.rs
- crates/cairo-lang-semantic/src/types.rs (tuple-format ground truth)
- crates/cairo-lang-syntax-codegen/src/cairo_spec.rs (TraitItemImpl/TypeClause shape)
- crates/cairo-lang-defs/src/ids.rs (TraitItemId/TraitImplId shape)

### cairo-lang-utils (H14)
- crates/cairo-lang-utils/src/byte_array.rs
- crates/cairo-lang-utils/src/bigint.rs
- crates/cairo-lang-utils/src/casts.rs
- crates/cairo-lang-utils/src/unordered_hash_map.rs
- crates/cairo-lang-utils/src/unordered_hash_set.rs
- crates/cairo-lang-utils/src/iterators.rs
- crates/cairo-lang-utils/src/extract_matches.rs
- crates/cairo-lang-utils/src/unordered_hash_map_test.rs
- crates/cairo-lang-utils/src/bigint_tests/{serde.rs, parity_scale_codec.rs, mod.rs}
- (range.rs — confirmed absent)

### cairo-lang-diagnostics / project / debug (H15)
- crates/cairo-lang-diagnostics/src/diagnostics.rs
- crates/cairo-lang-diagnostics/src/diagnostics_test.rs
- crates/cairo-lang-diagnostics/src/error_code.rs
- crates/cairo-lang-diagnostics/src/lib.rs
- crates/cairo-lang-project/src/lib.rs
- crates/cairo-lang-project/src/test.rs
- crates/cairo-lang-debug/src/debug.rs
- crates/cairo-lang-debug/src/debug_test.rs
- crates/cairo-lang-debug/src/lib.rs

### Additional files I checked for verification (supervisor)
- crates/cairo-lang-parser/src/diagnostic.rs (is_same_kind / format keyed on kind)
- crates/cairo-lang-semantic/src/diagnostic.rs (is_same_kind = kind equality)
- crates/cairo-lang-lowering/src/diagnostic.rs (is_same_kind = kind equality)
- crates/cairo-lang-plugins/src/test_utils.rs (test-only DiagnosticEntry impl)
