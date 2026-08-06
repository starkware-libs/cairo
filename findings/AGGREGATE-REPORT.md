# Coordinated Cairo Compiler Bug Hunt — Aggregated Report (2026-08-06)

15 bug-hunters (6 opus / 9 sonnet) across 3 supervisor groups; 3 opus supervisors validated.
Areas hunted (steered to least-covered ground per Team Memory): **runtime/execution**,
**casm/sierra codegen internals**, **tooling & contract-class artifacts**.

## Summary table — confirmed bugs (after supervisor validation + memory dedup)

| # | Severity | Area | File:line | One-line |
|---|----------|------|-----------|----------|
| 1 | Low (correctness) | formatter | `cairo-lang-formatter/src/formatter_impl.rs:280,286,812` | Line width measured in **bytes not chars** → non-ASCII comments mis-wrapped |
| 2 | Low (correctness) | doc gen | `cairo-lang-doc/src/documentable_formatter.rs:156-163` | Single-element tuple `(T,)` rendered as `(T)` in doc signatures (invalid Cairo) |
| 3 | Low (cosmetic) | casm | `cairo-lang-casm/src/hints/mod.rs:658-661` | `GetNextDictKey` hint renders in single-line wrapper (malformed multi-line dump) |
| 4 | Low (cosmetic) | test-runner | `cairo-lang-test-runner/src/lib.rs:180-186` | `FAILED` summary omits the `filtered out` count the `ok` path prints |
| 5 | Low (doc-only) | starknet-classes | `cairo-lang-starknet-classes/src/casm_contract_class.rs:213` | Stale comment says `1 + poseidon(...)`; hash is actually Blake2 |

**Dropped as DUP at aggregation-time re-check:** `#[executable]` fn with ≥101 params overflows the
31-byte felt252 short-string panic literal → misattributed E2008 (Hunter 3). Already an **active**
Team Memory row (`3ab112692ab281da81fad265b37f969e`); confirmed still reproduces but not re-filed.

**Rejected (false positives caught by supervisors):**
- **Gas "double-charge"** in `cairo-lang-runner/src/lib.rs:479-498` (Hunter 4) — **by-design**. Two-quantity gas model: compile-time wallet (`function_costs`, seeded at entry) + runtime `GasBuiltin` counter; invariant `total = wallet + counter` requires seeding the counter with `available_gas − function_costs`. Documented at `lib.rs:260-262`. Empirically, entry cost is never re-withdrawn at runtime. Hunter's oracle was wrong.
- **Blake2 `COMPILED_CLASS_V1` domain prefix** (Hunter 11) — unverifiable from repo (consensus/OS-spec defined); not claimed as a bug.

**Suspected / not demonstrated (open follow-ups, no idiomatic repro):**
- Trait associated-impl (`TraitItemId::Impl`) renders no signature — asymmetric with `ImplImplDefId` (`documentable_formatter.rs:71`). Undocumented gap, not a contradiction.
- `Diagnostics::get_diagnostics_without_duplicates` adjacency-dependent dedup (`diagnostics.rs:359-384`) — real logic flaw but unreachable: every production `DiagnosticEntry` ties `format` to `kind`, so same-kind+same-span always share a message. Hunter's repro used a test-only type decoupling message from kind (rejected as manufactured).
- `circuit.rs` `get_circuit_info` gate-offset bookkeeping — traced consistent, not fully verified.
- `create_entry_code_from_params` usize-underflow panic on a <2-param / 0-return execution-mode signature (`cairo-lang-runnable-utils/src/builder.rs:457,525,547`) — reachable only via public API with a contract-violating signature; `Result`-contract hardening gap.
- Duplicate test-config attribute (`#[available_gas]` etc.) silently ignored, first wins, no diagnostic (`cairo-lang-test-plugin/src/test_config.rs:50-53`).

**Nits:** dead `!testing` guard (`builder.rs:414,570`); swapped `"positive"`/`"negative"` `#[test_case]` name labels in `cairo-lang-utils/src/bigint_tests/serde.rs`.

**Clean areas (no bug, several empirically verified):** runner `casm_run` (H1), sierra-to-casm int lowering (H6), sierra type-size + program/registry (H9), sierra-to-casm array/box/dict (H10, verified via cairo-run), sierra extensions/modules (H8, aside from circuit follow-up).

---

## Confirmed bug details (file, root cause, fix, test)

### Bug 1 — Formatter measures line width in bytes, not chars
- **File:** `crates/cairo-lang-formatter/src/formatter_impl.rs` — `LineComponent::width` (`:280` Token `s.len()`, `:286` trailing-Comment `content.len()`) and `format_leading_comment` (`:812`, byte-length word wrap). Doc comments at `:432,:436` explicitly promise "in number of chars".
- **Root cause:** `str::len()` is UTF-8 byte count. Tokens are ASCII in valid Cairo, so only comments (the one place non-ASCII is legal) are affected: a non-ASCII trailing comment measured ~2× its real column width forces a spurious break; a non-ASCII leading `//` comment wraps far too early.
- **Fix:** use `s.chars().count()` / `content.chars().count()`; compare `current_line.content.chars().count() + word.chars().count()` against a char-derived `max_comment_width`. Prefer a shared display-width helper.
- **Test:** add to `crates/cairo-lang-formatter/src/test.rs` (uses `SimpleParserDatabase`, `get_formatted_file`, `FormatterConfig`):
```rust
#[test]
fn multibyte_comment_width_is_measured_in_chars() {
    use cairo_lang_parser::utils::SimpleParserDatabase;
    use crate::{FormatterConfig, get_formatted_file};
    let db = SimpleParserDatabase::default();
    let config = FormatterConfig::default(); // max_line_length = 100
    let fmt = |src: &str| {
        let root = db.parse_virtual(src).expect("parse failed");
        get_formatted_file(&db, &root, config.clone())
    };
    // Trailing comment (74 visible cols) must NOT break.
    let greek60: String = "α".repeat(60);
    let input1 = format!("fn f() {{ let x = 1; // {greek60}\n}}");
    let expected1 = format!("fn f() {{\n    let x = 1; // {greek60}\n}}\n");
    assert_eq!(fmt(&input1), expected1);
    // Leading comment (~93 visible cols) must NOT wrap.
    let greek_words: String = std::iter::repeat("αα").take(30).collect::<Vec<_>>().join(" ");
    let input2 = format!("fn f() {{\n    // {greek_words}\n    let x = 1;\n}}");
    let expected2 = format!("fn f() {{\n    // {greek_words}\n    let x = 1;\n}}\n");
    assert_eq!(fmt(&input2), expected2);
}
```
- **Verify:** `cargo test -p cairo-lang-formatter multibyte` (both asserts fail today; ASCII controls of equal/greater visible width pass).

### Bug 2 — Single-element tuple loses its trailing comma in doc signatures
- **File:** `crates/cairo-lang-doc/src/documentable_formatter.rs:156-163`, `HirFormatter::write_type`.
- **Root cause:** separator chosen by remaining-count (`if count == 1 { None } else { Some(", ") }`), so a 1-tuple's sole element gets no trailing comma → `(felt252)` (a parenthesized type, not a 1-tuple). The semantic layer's own `TypeLongId::fmt` (`cairo-lang-semantic/src/types.rs:319-325`) special-cases `len()==1` → `({},)`; the doc formatter doesn't. Affects every signature with a `(T,)` type (return types, params, struct members, enum variants, constants, aliases). Distinct from the closed **parser** 1-tuple bug — this is doc-side rendering.
- **Fix:** special-case single-element tuples to emit a trailing comma, mirroring `types.rs`.
- **Test:** add under `crates/cairo-lang-doc/src/tests/` (public `DocGroup` API):
```rust
use cairo_lang_defs::ids::{LookupItemId, ModuleId, ModuleItemId};
use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::tests::test_utils::{TestDatabase, setup_test_module, test_crate_id};

#[test]
fn single_element_tuple_return_type_signature() {
    let mut db_val = TestDatabase::new().unwrap();
    setup_test_module(&mut db_val, "fn foo() -> (felt252,) {\n    (1,)\n}\n");
    let db = &db_val;
    let crate_id = test_crate_id(db);
    let module_data = ModuleId::CrateRoot(crate_id).module_data(db).unwrap();
    let free_fn = module_data.items(db).iter().find_map(|item| match item {
        ModuleItemId::FreeFunction(id) => Some(*id), _ => None }).unwrap();
    let id = DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_fn)));
    let signature = db.get_item_signature(id).unwrap();
    assert_eq!(signature, "fn foo() -> (felt252,)"); // today: "fn foo() -> (felt252)"
}
```
- **Verify:** `cargo test -p cairo-lang-doc --lib bughunt` — fails, prints `fn foo() -> (felt252)`.

### Bug 3 — `GetNextDictKey` hint renders in the single-line wrapper
- **File:** `crates/cairo-lang-casm/src/hints/mod.rs:658-661`, interacting with `Display for Instruction` (`instructions.rs:66-83`).
- **Root cause:** `Display` routes multi-line hints via `hint_str.starts_with('\n')`. Every other multi-line `formatdoc!` template has a leading blank line so the string begins with `\n`; `GetNextDictKey` is the sole one missing it, so its two-line body is jammed into the `%{ … %}` single-line wrapper. Cosmetic — the executed hint semantics and all binary encodings are unaffected; only the human-readable `.casm` dump is malformed.
- **Fix:** add a leading blank line to the `GetNextDictKey` template.
- **Test:** add to `crates/cairo-lang-casm/src/instructions_test.rs`:
```rust
#[test]
fn test_get_next_dict_key_hint_render_is_consistent() {
    use crate::cell_ref;
    use crate::hints::CoreHint;
    use crate::instructions::{Instruction, InstructionBody, RetInstruction};
    let insn = Instruction {
        body: InstructionBody::Ret(RetInstruction {}),
        inc_ap: false,
        hints: vec![CoreHint::GetNextDictKey { next_key: cell_ref!([ap + 0]) }.into()],
    };
    assert!(insn.to_string().starts_with("%{\n")); // fails today
}
```
- **Verify:** `cargo test -p cairo-lang-casm`; contrast with `GetCurrentAccessDelta`, which renders `%{\n…` correctly.

### Bug 4 — `FAILED` test-run summary drops the "filtered out" count
- **File:** `crates/cairo-lang-test-runner/src/lib.rs:180-186` (failure `bail!`) vs `:156` (success `println!`).
- **Root cause:** the failure-path format string omits the `{filtered_out}` placeholder the success path includes, even though `filtered_out` is in scope both places. A run with a filter and a failing test can't report how many tests were excluded. Cosmetic (no wrong verdicts).
- **Fix:** append `; {filtered_out} filtered out` to the `bail!` format string, mirroring line 156.
- **Test:** add to `crates/cairo-lang-test-runner/src/test.rs` (public `TestRunner`/`TestRunConfig`): build a temp project with `keep_pass`, `keep_fail`, `excluded_by_filter`; run with `filter:"keep"`; assert the `Err` message `.contains("filtered out")` — fails today (`test result: FAILED. 1 passed; 1 failed; 0 ignored`). (Full code in `findings/hunter-5.md`.)
- **Verify:** `cargo test -p cairo-lang-test-runner --lib test_failure_summary_reports_filtered_out_count`.

### Bug 5 — Stale `poseidon` comment on the Blake2 compiled-class hash (doc-only)
- **File:** `crates/cairo-lang-starknet-classes/src/casm_contract_class.rs:213` — `bytecode_hash_node` comment says `1 + poseidon(...)`, but the fn is generic over `H: StarkHash` and the current `compiled_class_hash()` uses `Blake2Felt252`, so it is `1 + blake2(...)`. No runtime impact.
- **Fix:** update the comment to reflect the generic hash / Blake2 default.

---

## Method notes
- Team Memory recalled up front (80 recent rows) to build the area deny-list; hunters steered off the ~8-day-covered ground (parser/semantic/lowering/corelib/sierra-gas/etc.).
- Supervisors independently reproduced/traced each claim, scrutinized every test for idiomatic legitimacy (rejected the diagnostics-dedup repro as manufactured; rejected the gas-double-charge oracle), and dedup'd against memory.
- Aggregation-time memory re-check caught the `#[executable]` ≥101-param finding as a DUP.
