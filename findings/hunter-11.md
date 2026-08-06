# Bug Hunter #11 — Starknet contract class artifacts

Area: `crates/cairo-lang-starknet-classes/src/` — class-hash / selector computation,
bytecode segment length handling, ABI (de)serialization, version comparison, doc/behavior
mismatches, edge cases.

**Honest summary:** After a careful line-by-line trace of the six focus files (plus the
directly-reachable helpers `contract_segmentation.rs` and `felt252_serde.rs`), I did **not**
find a *demonstrable* new logic bug. The class-hash, selector, segmentation, ABI and version
paths are correct and well covered by golden tests, which I cross-checked against the
Starknet-OS conventions. Below I report the one genuine (low-severity) doc/behavior mismatch I
found, and I explicitly record the things I checked and cleared so the negative result is
auditable. I deliberately did **not** re-file the already-known
`UnsupportedLibfuncAtVersion` backslash bug (OFF-LIMITS); I confirmed it is still present at
`allowed_libfuncs.rs:34-37` but excluded it per instructions.

---

## Finding 1 (LOW / doc-behavior mismatch) — `bytecode_hash_node` comment says "poseidon" but the function is generic and is used with Blake2

**File / location:** `crates/cairo-lang-starknet-classes/src/casm_contract_class.rs:198-224`
(the `bytecode_hash_node` doc comment, esp. line 213).

**Description:** `bytecode_hash_node<H: StarkHash>` is generic over the hash function. Its
inner-node doc comment states:

```rust
NestedIntList::Node(nodes) => {
    // Compute `1 + poseidon(len0, hash0, len1, hash1, ...)`.
```

but the code computes `H::hash_array(&hash_elements) + 1`, i.e. `1 + H(...)`. The primary
public entry point `compiled_class_hash()` (casm_contract_class.rs:133-135) instantiates
`H = Blake2Felt252`, so for the current (non-legacy) compiled class hash the node value is
`1 + blake2(...)`, **not** poseidon. Only `legacy_compiled_class_hash()` uses Poseidon. The
comment predates the Blake2 addition and is now stale/misleading for anyone reasoning about
the new hash.

**Root cause:** When the Blake2 hash variant was introduced, `compiled_class_hash_inner`,
`entry_points_hash`, `compute_bytecode_hash` and `bytecode_hash_node` were generalized to
`<H: StarkHash>`, but the hard-coded "poseidon" wording in the `bytecode_hash_node`
documentation was not updated to reflect the now hash-agnostic behavior.

**Severity / impact:** Documentation only — no runtime effect. Both hash flavors are pinned by
golden tests (`compiled_class_hash_test_data/contracts`), so numeric output is unaffected. I am
reporting it because the hunt explicitly lists "doc/behavior mismatches" as in-scope, and this
is a real one.

**Suggested fix:** Reword the comment to be hash-agnostic, e.g.
`// Compute `1 + H(len0, hash0, len1, hash1, ...)` (poseidon for the legacy hash, blake2 for the
current one).`

**How to verify (reading, no build needed):** Compare casm_contract_class.rs:133-139
(`compiled_class_hash` → `Blake2Felt252`, `legacy_compiled_class_hash` → `Poseidon`) with the
`// ... poseidon ...` comment at line 213 and the generic `H::hash_array(...) + 1` at line 221.

---

## Observation (NOT a confirmed bug, flagged for spec review) — Blake2 compiled class hash reuses the `"COMPILED_CLASS_V1"` domain prefix

**File / location:** `crates/cairo-lang-starknet-classes/src/casm_contract_class.rs:188-194`.

`compiled_class_hash_inner::<H>` uses the same domain-separator felt
`Felt252::from_bytes_be_slice(b"COMPILED_CLASS_V1")` for **both** the Blake2-based
`compiled_class_hash()` and the Poseidon-based `legacy_compiled_class_hash()`. The `V1` prefix
historically belongs to the Poseidon compiled-class-hash definition. Whether the Blake2 variant
in the Starknet OS is specified to keep `COMPILED_CLASS_V1` or to use a different marker (e.g.
`V2`) cannot be determined from this repository — it is defined by the OS/consensus spec, and
the in-repo golden values are self-referential regression fixtures, not an independent oracle.

I am **not** claiming this is a bug (I will not invent one). If the on-chain Blake2 compiled
class hash uses a different domain separator, this would be a serious consensus mismatch; if it
reuses `COMPILED_CLASS_V1`, the code is correct. **Recommend a human cross-check against the
Blake2 compiled-class-hash SNIP / Starknet-OS reference.** Labeled: *suspected-scope,
unverifiable from repo — not demonstrated.*

---

## Things checked and cleared (negative results, with reasoning)

- **`keccak.rs` `starknet_keccak` truncation** (`keccak.rs:15`): `result[0] &= 3` keeps the low
  2 bits of the most-significant byte → exactly 250 bits (bits 0..249), equivalent to
  `& (2^250 − 1)`. Correct; confirmed by `keccak_test.rs` (`__execute__` selector golden).

- **`compiler_version.rs` `VersionId::supports`** (`compiler_version.rs:15-17`): ignores
  `patch`. This is *intended* — documented ("Minor version … backwards compatible") and pinned
  by `compiler_version_test.rs` (`v140.supports(v141) == true`). All `patch` values in
  `allowed_libfuncs_lists/{audited,all}.json` are `0`, so the patch-ignore is not even
  reachable as a discrepancy in practice. Not a bug.

- **Version gate in `from_contract_class_with_debug_info`** (`casm_contract_class.rs:388-396`):
  `major == current.major && minor <= current.minor` correctly accepts only same-major,
  not-newer-minor Sierra. Consistent with `current_sierra_version_id() = 1.9.3` and the
  `assert_eq!(sierra_version.major, 1)` at line 529.

- **Compiled-class-hash component order** (`casm_contract_class.rs:180-195`): prefix, external,
  l1_handler, constructor, bytecode — matches the Starknet-OS `COMPILED_CLASS_V1` layout.
  `entry_points_hash` emits `[selector, offset, H(builtins)]` per entry point (lines 150-164),
  the canonical triple/order.

- **Builtin name serialization for the hash** (`casm_contract_class.rs:505-511` and
  `157-161`): snake-casing plus the explicit `"RangeCheck96" -> "range_check96"` special case
  (which avoids `convert_case` producing `range_check_96`); names hashed via
  `Felt252::from_bytes_be_slice` as short strings. Correct and covered by the CASM golden tests.

- **Bytecode segment length handling** (`casm_contract_class.rs:145-147, 166-175, 202-224` and
  `contract_segmentation.rs`): `get_bytecode_segment_lengths` falls back to a single
  `Leaf(bytecode.len())` when absent (leaf → flat `H(data)`, no `+1`; node → `1 + H(...)`),
  matching the OS. Empty-bytecode edge case handled (`compute_bytecode_segment_lengths` returns
  `Leaf(0)` at contract_segmentation.rs:40-42; the leaf hash of an empty slice is well-defined;
  `assert_eq!(len, bytecode.len())` holds for `0`). `get_segment_lengths` differences never
  underflow because function offsets are ascending (sorted statement ids) and const-segment
  offsets all start at `bytecode_len − total_segments_size` (after all code); zero-length
  segments are dropped and the pieces sum back to `bytecode_len`.

- **Entry-point validation / edge cases** (`casm_contract_class.rs:398-444`): "no constructor"
  handled (`[]` arm); at most one constructor with the `starknet_keccak("constructor")`
  selector; strict-ascending selector check (adjacent-duplicate detection is sound *because*
  ordering is enforced, so any duplicate is adjacent); function-index reuse capped at 2. All
  `program.funcs[idx]` indexing at line 526 is preceded by `validate_entry_points`
  (lines 520-524) which rejects out-of-range indices, so no panic. Confirmed by
  `casm_contract_class_test.rs`.

- **ABI (de)serialization & `sanity_check`** (`abi.rs`): `Contract` is
  `#[serde(transparent)]` over `OrderedHashSet<Item>` → serializes as the JSON array ABI form,
  order preserved. `sanity_check` counts top-level `Function`/`L1Handler`/`Constructor` and
  expands each `Impl` by its `Interface`'s item count (keyed `Interface.name` ↔
  `Imp.interface_name` — consistent). This runs in the real pipeline
  (`cairo-lang-starknet/src/compile.rs:207`) for every example contract, so the counting is
  exercised and correct for normally-generated ABIs. (The only theoretical soft spots —
  `assert`/`panic!` on a hand-built ABI whose impl references a missing interface, or an
  `OrderedHashSet` silently de-duplicating two byte-identical `Item`s — require inputs the real
  frontend never emits, so they are not reportable per the "normal idiomatic usage" rule.)

- **`allowed_libfuncs.rs`**: `ListSelector::new` (both-supplied → `None`), `Display`, and
  `lookup_allowed_libfuncs_list` (name/file/default dispatch, old-set vs new-map untagged
  deserialization) are all correct. The `UnsupportedLibfuncAtVersion` message backslash defect
  at lines 34-37 is the already-filed OFF-LIMITS item — present, deliberately not re-reported.

- **`felt252_serde.rs` version handling** (reachable via `ContractClass::extract_sierra_program`
  / `version_id_from_serialized_sierra_program`): two `VersionId`s = 6 felts; the
  `&sierra_program[6..]` slice at line 93 cannot panic because `VersionId::deserialize` (6
  reads) errors first on short input. `GenericArg::Value` negative handling (tags 2 vs 5) and
  the `usize::MAX`⇄`Fallthrough` branch-target encoding round-trip correctly.

## Files checked
- `crates/cairo-lang-starknet-classes/src/casm_contract_class.rs` (full)
- `crates/cairo-lang-starknet-classes/src/contract_class.rs` (full)
- `crates/cairo-lang-starknet-classes/src/abi.rs` (full)
- `crates/cairo-lang-starknet-classes/src/compiler_version.rs` (full) + `compiler_version_test.rs`
- `crates/cairo-lang-starknet-classes/src/allowed_libfuncs.rs` (full) + `allowed_libfuncs_lists/{audited,all}.json` (version-format scan)
- `crates/cairo-lang-starknet-classes/src/keccak.rs` (full) + `keccak_test.rs`
- `crates/cairo-lang-starknet-classes/src/contract_segmentation.rs` (full, supporting)
- `crates/cairo-lang-starknet-classes/src/felt252_serde.rs` (full, supporting)
- `crates/cairo-lang-starknet-classes/src/casm_contract_class_test.rs` + `compiled_class_hash_test_data/contracts` (golden values)
- `crates/cairo-lang-starknet/src/compile.rs` (sanity_check call site)
