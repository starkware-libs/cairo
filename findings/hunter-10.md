# Hunter #10 findings — sierra-to-casm invocation lowering (array.rs)

Area: `crates/cairo-lang-sierra-to-casm/src/invocations/array.rs` — CASM lowering of the
`Array<T>`/`Span<T>` libfuncs (`array_new`, `span_from_tuple`, `tuple_from_span`,
`array_append`, `pop_front`/`pop_front_consume`, `snapshot_pop_back`,
`snapshot_multi_pop_front`/`_back`, `array_get`, `array_slice`, `array_len`).

## Summary

**No bug found.** I picked `array.rs` from the assigned file list (it is the richest
target for off-by-one / boundary errors: length/index handling, empty-array branches,
pop-front/back cell arithmetic). I:

1. First confirmed the file is byte-for-byte identical to `origin/main` at the repo's
   current HEAD (`git diff HEAD origin/main -- .../array.rs` is empty) — this is exactly
   the shipped production lowering, not a locally modified copy.
2. Traced every function's emitted CASM against the documented libfunc semantics
   (`crates/cairo-lang-sierra/src/extensions/modules/array.rs`), checking branch order,
   cell-offset arithmetic, and the soundness of every range-check pair at the boundaries
   called out in the hunt brief: empty array, index/slice at the exact end, and
   exact-vs-over multi-pop size.
3. Built `cairo-run` from this checkout and empirically exercised those exact boundary
   cases through idiomatic Cairo programs (below) — all assertions pass.

Below are the traces performed, followed by the two verification programs I ran.

---

## Traces performed (all verified correct)

### `build_array_new` (line 45)
Allocates a fresh segment, returns `[arr_start, arr_start]` (start == end ⇒ empty).
Correct.

### `build_span_from_tuple` / `build_tuple_from_span` (lines 64, 89)
`arr_end = arr_start + full_struct_size` / and the inverse check
`diff = (arr_end-arr_start) - full_struct_size; jump Failure if diff != 0`. Symmetric and
correct — a span can only convert back to the box if its length in cells exactly matches
the struct size.

### `build_array_append` (line 117)
Writes each cell of the appended element at `*(arr_end++)`, returns `[arr_start, new_end]`.
Matches append semantics: growth only at `arr_end`, `arr_start` untouched.

### `build_pop_front` / `build_pop_back` (lines 139, 174)
Both gate on `is_non_empty = arr_end - arr_start`, unconditionally `jump Failure` when the
`NonEmpty` branch is not taken (there is no implicit "else": the code is
`jump NonEmpty if is_non_empty != 0; jump Failure;`, so `is_non_empty == 0` always lands on
`Failure`). This is exactly correct for the empty-array case.
- pop_front: `new_start = arr_start + element_size`; returns new array
  `[new_start, arr_end]` and popped-element box `[arr_start]` (the vacated cell is exactly
  the old start — correct).
- pop_back: `new_end = arr_end - element_size`; returns new array `[arr_start, new_end]`
  and popped-element box `[new_end]` (the last element occupies `[new_end, arr_end)` —
  correct).
- `consume` variant correctly omits the array in the `Failure` output list (element
  0 or 1 depending on flag), matching `PopFrontConsume`'s signature (no leftover array to
  return on failure since `consume` never had one to begin with... actually consumes the
  array either way; the `Failure` arm returning zero cells is intentional since
  `PopFrontConsume` doesn't have an array output at all).

### `build_array_get` (line 209)
`element_offset_in_cells = index * element_size`; branch selection via untrusted
`TestLessThan` hint, then verified on **both** paths by a range-check:
- Not-in-range path: RC's `offset - length`, forcing `offset ≥ length` (a genuine
  `offset < length` would produce a `~P` value that fails the `[0,2^128)` check).
- In-range path: RC's `length - (offset+1)`, forcing `offset < length` (strict).
This is the standard branch-soundness pattern (both branches independently verified, hint
only picks which). `target_cell = arr_start + offset` is the correct element address.
Checked the two boundary indices explicitly: `index == length-1` → in-range, address
`arr_end - element_size` (last element); `index == length` → not-in-range, address
computation not reached, correctly routed to `FailureHandle`.

### `build_array_slice` (line 274)
Analogous double-sided range-check on `slice_end_in_cells` vs `array_length_in_cells`
using `TestLessThanOrEqual` (allows `slice_end == length`, i.e. a slice ending exactly at
the array's end, including the zero-length slice `slice(length, 0)`). Checked this exact
boundary by hand: `slice_start = length, slice_length = 0` ⇒ `slice_end_in_cells ==
array_length_in_cells` ⇒ `InRange` ⇒ `offset_length_diff == 0` (valid) ⇒
`slice_start_cell == slice_end_cell == arr_end` — a valid empty span at the end. Confirmed
empirically below.

### `build_array_len` (line 348)
`element_size == 1`: `length = arr_end - arr_start` directly (cells == elements).
Otherwise: `length = (arr_end-arr_start) / element_size`. Both correct.

### `build_multi_pop_front` / `build_multi_pop_back` + `extend_multi_pop_failure_checks`
(lines 380, 430, 480)
Shared failure precondition: `has_enough_elements = TestLessThanOrEqualAddress(arr_start +
popped_size, arr_end)`(hint, untrusted); if false, RC's `(arr_start-arr_end) +
(popped_size-1)`, which is only in `[0,2^128)` when `arr_size = arr_end-arr_start ≤
popped_size-1`, i.e. `arr_size < popped_size` — this is exactly the "not enough elements"
condition, and it is enforced (not just hinted) since the range-check would fail on a true
`arr_size ≥ popped_size` input. On success, both variants additionally RC the *remaining*
length (`arr_end-new_start` for front, `new_end-arr_start` for back), which is `≥0` only
when `popped_size ≤ arr_size` — consistent, redundant-but-harmless double confirmation of
the same bound. Popped-span addresses (`arr_start` for front-pop, `new_end` for back-pop)
and returned array bounds are correct. `Failure` arm returns the *unmodified* `[arr_start,
arr_end]` (no partial mutation) in both directions — correct (a failed multi-pop must not
touch the array).

---

## Empirical verification

Built `./target/debug/cairo-run` from this checkout (`cargo build -p cairo-run`) and ran
the following two idiomatic Cairo programs. Both completed successfully (`Run completed
successfully, returning [0x1]`), i.e. every `assert` — including the boundary cases named
in the hunt brief (empty-array pop, span-at-end slice, exact-vs-over multi-pop) — passed.

### Test 1 — pop_front/pop_back/get/slice boundaries

```cairo
fn main() -> felt252 {
    // Empty array pop_front should return None.
    let mut a: Array<felt252> = ArrayTrait::new();
    let popped = a.pop_front();
    assert(popped.is_none(), 'popfront empty fail');

    // Append then pop_front should return the element and empty array.
    a.append(10);
    a.append(20);
    a.append(30);
    let x = a.pop_front().unwrap();
    assert(x == 10, 'popfront val fail');

    // pop_back (on snapshot span)
    let mut sp = a.span();
    let y = sp.pop_back().unwrap();
    assert(*y == 30, 'popback val fail');
    // Now span has just [20]
    let z = sp.pop_back().unwrap();
    assert(*z == 20, 'popback val2 fail');
    let none_pop = sp.pop_back();
    assert(none_pop.is_none(), 'popback empty fail');

    // get at boundary
    let mut b: Array<felt252> = ArrayTrait::new();
    b.append(100);
    b.append(200);
    b.append(300);
    let span = b.span();
    let last = span.get(2).unwrap();
    assert(*last.unbox() == 300, 'get last fail');
    let oob = span.get(3);
    assert(oob.is_none(), 'get oob fail');

    // slice at end (empty slice)
    let empty_slice = span.slice(3, 0);
    assert(empty_slice.len() == 0, 'slice end empty fail');

    // slice full
    let full_slice = span.slice(0, 3);
    assert(full_slice.len() == 3, 'slice full fail');
    assert(*full_slice.at(0) == 100, 'slice full val0 fail');
    assert(*full_slice.at(2) == 300, 'slice full val2 fail');

    1
}
```

Run with: `cairo-run --single-file test1.cairo` → `Run completed successfully, returning [0x1]`.

### Test 2 — multi_pop_front/back exact-size success and over-size failure

```cairo
fn main() -> felt252 {
    let mut a: Array<felt252> = ArrayTrait::new();
    a.append(1);
    a.append(2);
    a.append(3);
    a.append(4);
    let mut span = a.span();
    // multi_pop_front of 2
    let popped: Option<@Box<[felt252; 2]>> = span.multi_pop_front();
    let popped = popped.unwrap();
    let arr: [felt252; 2] = popped.unbox();
    let [p0, p1] = arr;
    assert(p0 == 1, 'multipop v0');
    assert(p1 == 2, 'multipop v1');
    // remaining span should be [3,4]
    assert(span.len() == 2, 'remaining len');
    // multi pop front of 3 on remaining 2 elements -> should fail (None), span unchanged
    let fail_pop: Option<@Box<[felt252; 3]>> = span.multi_pop_front();
    assert(fail_pop.is_none(), 'multipop should fail');
    assert(span.len() == 2, 'span unchanged after fail');
    // exact size multi pop back
    let popped_back: Option<@Box<[felt252; 2]>> = span.multi_pop_back();
    let popped_back = popped_back.unwrap();
    let arr2: [felt252; 2] = popped_back.unbox();
    let [q0, q1] = arr2;
    assert(q0 == 3, 'multipopback v0');
    assert(q1 == 4, 'multipopback v1');
    assert(span.len() == 0, 'span empty after full pop');
    1
}
```

Run with: `cairo-run --single-file test2.cairo` → `Run completed successfully, returning [0x1]`.

### How to verify
```
cargo build -p cairo-run --bin cairo-run
./target/debug/cairo-run --single-file <test file above>
```
Expect `Run completed successfully, returning [0x1]` for both; any assertion failure would
print the failing assert's felt-encoded short string and a non-success exit.

---

## Files checked
- `crates/cairo-lang-sierra-to-casm/src/invocations/array.rs` (primary focus, full trace +
  empirical test, confirmed identical to `origin/main`)
- Cross-referenced libfunc signatures/semantics in
  `crates/cairo-lang-sierra/src/extensions/modules/array.rs`
- Briefly spot-checked (diff-only, confirmed identical to `origin/main`, not deeply traced):
  `crates/cairo-lang-sierra-to-casm/src/invocations/boxing.rs`,
  `crates/cairo-lang-sierra-to-casm/src/invocations/gas.rs`,
  `crates/cairo-lang-sierra-to-casm/src/invocations/felt252_dict.rs`
  — did a manual arithmetic trace of `build_withdraw_gas`/`build_withdraw_gas_given_cost_table`
  (inclusive `<=` boundary on exact-gas withdrawal is correct) and of the
  `dict_squash`/`SquashDictInner` CASM (matches the well-known upstream algorithm) but did
  not build dedicated tests for these two since `array.rs` was the chosen focus file.
- Did not read/grep `crates/cairo-lang-syntax/src/node/ast.rs` (per repo instructions;
  not relevant to this area).
