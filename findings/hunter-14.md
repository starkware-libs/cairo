# Bug Hunt Report — Hunter #14

**Area assigned:** `cairo-lang-utils` general utilities (excluding graph algos and
already-filed map issues).

**Files in scope (per assignment):**
`crates/cairo-lang-utils/src/byte_array.rs`, `bigint.rs`, `casts.rs`,
`unordered_hash_map.rs`, `unordered_hash_set.rs`, `iterators.rs`,
`extract_matches.rs`, `range.rs`.

## Result: No bug found

After a careful line-by-line review of every file in scope (plus running the
existing test suite and an ad-hoc probe of `num-bigint` byte/bit-length
invariants used by the parity-scale-codec impl), I could not find a genuine
logic error, off-by-one, overflow, or doc/behavior mismatch in this file set.
Below is what was checked and why each file was ruled out, followed by one
non-bug observation worth flagging to the team.

## Files checked and findings

### `crates/cairo-lang-utils/src/range.rs`
Does **not exist** in this crate (`ls crates/cairo-lang-utils/src` confirms
it). Nothing to review. (Possibly a stale/generic entry in the assignment
template — flagging in case another hunter needs this corrected.)

### `crates/cairo-lang-utils/src/byte_array.rs`
Contains only two constants (`BYTE_ARRAY_MAGIC`, `BYTES_IN_WORD = 31`) — no
logic to break.

### `crates/cairo-lang-utils/src/casts.rs`
`IntoOrPanic::into_or_panic` is a thin wrapper around `TryInto::try_into`
that panics with a formatted message on failure. Verified against all 10
`impl IntoOrPanic for ...` primitive types; behavior is a straightforward,
correct delegate to `TryFrom`. No mismatch between the "OrPanic" contract and
implementation.

### `crates/cairo-lang-utils/src/bigint.rs`
This is the file most relevant to the "numeric casts/serialization
(bigint <-> felt/bytes)" hunt focus, so it got the most scrutiny:

- `serialize_big_uint` / `deserialize_big_uint`: round-trips via
  `format!("{num:#x}")` (always emits `0x` prefix, lowercase, including for
  zero → `"0x0"`) and `BigUint::from_str_radix(..., 16)` after stripping the
  `0x` prefix. Verified consistent for zero and arbitrary values.
- `serialize_big_int` / `deserialize_big_int`: emits `"-0x…"` for negative,
  `"0x…"` otherwise (using `num.magnitude()`), and the deserializer strips a
  leading `-` before delegating to the same hex parser, then negates.
  Verified this is a correct, lossless round-trip including for zero
  (`is_negative()` is `false` for `BigInt` zero, so no `"-0x0"` is ever
  produced).
- `serialize_big_ints` / `deserialize_big_ints` (Vec<BigInt> seq
  (de)serialization): delegates per-element to the single-value functions
  above via a borrowing wrapper (`BigIntRef`) to avoid clones on the encode
  side, and a `Visitor`/`next_element::<BigIntAsHex>()` loop on the decode
  side. Confirmed correct pairing.
- `impl_parity_scale_codec` (`Encode`/`Decode` for `BigIntAsHex`): packs sign
  (2 bits) and magnitude byte length (6 bits, ≤ 63) into one header byte,
  then writes the little-endian magnitude bytes (`to_bytes_le()` /
  `from_bytes_le()`, which are already-inverse `num-bigint` APIs). I
  specifically probed the boundary cases that looked suspicious on paper:
  - `size_hint()` computes `bits().div_ceil(8).max(1)` and I verified via a
    scratch test that this exactly matches `to_bytes_le().1.len()` for
    zero (`bits()=0` → hint `1`; actual `to_bytes_le()` → `(NoSign, [0])`,
    len `1`), for a small negative value (`-5`: `bits()=3` → hint `1`;
    actual len `1`), and for the 8-bit/9-bit boundary (`255`: `bits()=8` →
    hint `1`, actual len `1`; `256`: `bits()=9` → hint `2`, actual len `2`).
    No mismatch found (and even if `size_hint` were wrong, it only affects
    buffer pre-allocation, not correctness of the written bytes, since
    `encode_to` recomputes `data.len()` itself).
  - The 6-bit length field's max value is 63, and the `assert!(data.len() <=
    63, ...)` in `encode_to` matches exactly — no off-by-one that would let
    an unencodable 64-byte magnitude slip through, and no valid ≤63-byte
    magnitude gets rejected.
  - Sign round-trips correctly: `Minus↔0`, `NoSign↔1`, `Plus↔2` in both
    `encode_to` and `decode`, with `3` (`0b11`) correctly rejected as
    `"Bad sign encoding."`.
  - This matches the existing test
    `bigint_tests/parity_scale_codec.rs::encode_bigint` and
    `bigint_tests/serde.rs::test_bigint_serde`, both of which I confirmed
    still pass (`cargo test -p cairo-lang-utils bigint`).

  Minor non-bug observation: in `bigint_tests/serde.rs`, the two
  `#[test_case(..., true; "positive")]` / `#[test_case(..., false;
  "negative")]` annotations have their test-name labels backwards relative to
  the `is_negative` boolean they carry (the `true`/is-negative case is named
  `"positive"` and vice versa). This is purely a test-name/label mix-up in an
  existing test file, not a bug in library logic — the assertions inside the
  test body use the boolean directly and are unaffected, so I'm not filing it
  as a bug, just noting it in case it's worth a quick cosmetic fix.

### `crates/cairo-lang-utils/src/unordered_hash_map.rs`
Reviewed all methods (`get`/`get_mut`/`insert`/`remove`/`entry`/`map`/
`aggregate_by`/`iter_sorted`/`into_iter_sorted`/`iter_sorted_by_key`/
`into_iter_sorted_by_key`/`filter`/`filter_cloned`/`merge`/`clear`/`Index`/
`Default`/`FromIterator`/`From<[; N]>`/`Extend`). All are thin, correct
delegations to `std::collections::HashMap` / `hashbrown::HashMap`, or use
`itertools::sorted_by(_key)` correctly (ascending, matching their doc
comments). `map` and `aggregate_by` both correctly use
`Entry::Occupied`/`Vacant` without any accidental key/value swap or dropped
update. This file (not to be confused with the already-filed
`SmallOrderedMap`/`collection_arithmetics` issues, which live in different
files) is not the source of any new bug I could find. Existing unit tests in
`unordered_hash_map_test.rs` (`test_map`, `test_aggregate_by`,
`test_iter_sorted`, `test_into_iter_sorted`, `test_iter_sorted_by_key`,
`test_into_iter_sorted_by_key`, `test_filter`, `test_merge`) all pass and
exercise these code paths already.

### `crates/cairo-lang-utils/src/unordered_hash_set.rs`
Reviewed `insert`/`remove`/`extend`/`extend_unordered`/`contains`/`clear`/
`with_capacity`/`Default`/`FromIterator`/`Sub`. All correctly delegate to
`std::collections::HashSet` / `hashbrown::HashSet` with matching semantics
(e.g. `insert` returns `true` iff newly inserted, matching both the doc
comment and `std::collections::HashSet::insert`'s contract; `Sub` computes
`self.0 - rhs.0`, i.e. set difference, matching doc and std semantics). No
bug found.

### `crates/cairo-lang-utils/src/iterators.rs`
Contains only `zip_eq3`, implemented as
`zip_eq(a, zip_eq(b, c)).map(|(a, (b, c))| (a, b, c))`. Pairing/order is
correct and it correctly inherits `itertools::zip_eq`'s panic-on-length-
mismatch contract, matching the doc comment ("Similar to zip_eq, except that
it works with 3 iterators"). No bug found. (Note: despite the assignment
listing `iterators/*`, this crate has a single `iterators.rs` file, not a
directory — confirmed via `find`.)

### `crates/cairo-lang-utils/src/extract_matches.rs`
Both macros (`try_extract_matches!`, `extract_matches!`) are simple
single-arm `match` expressions extracting a tuple-variant's payload, with a
panicking fallback (with or without a custom message) in `extract_matches!`.
Matches their documented behavior and doc-test examples (which I confirmed
still pass via `cargo test -p cairo-lang-utils --doc`). No bug found.

## Verification performed
- `cargo test -p cairo-lang-utils bigint` — all bigint tests pass (confirms
  the serde and parity-scale-codec round-trips discussed above).
- `cargo test -p cairo-lang-utils unordered_hash_map` — all pass.
- `cargo test -p cairo-lang-utils --doc` — doctests for `extract_matches!`/
  `try_extract_matches!` pass.
- Ad-hoc scratch test (added temporarily to `bigint_tests/mod.rs`, then
  reverted via `git checkout`, so the repo is left clean) to double check
  `num_bigint::BigInt::bits()` / `to_bytes_le()` behavior at the zero /
  small-negative / byte-boundary cases relevant to `size_hint()` vs
  `encode_to()` consistency in the parity-scale-codec implementation:
  ```
  zero to_bytes_le: (NoSign, [0])      zero bits: 0
  neg(-5) to_bytes_le: (Minus, [5])    neg bits: 3
  255 bits 8  bytes (Plus, [255])
  256 bits 9  bytes (Plus, [0, 1])
  ```
  All consistent with `bits().div_ceil(8).max(1)` matching the actual
  `to_bytes_le()` length in every case checked.

## Conclusion
No bug is being filed from this pass. The assigned file set is small and,
on inspection, consists almost entirely of thin/correct delegations to
well-tested external crates (`std`/`hashbrown`/`itertools`/`num-bigint`)
plus a few plain constants and macros. Per instructions, reporting honestly
rather than inventing a bug.
