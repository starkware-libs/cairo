# Hunter #6 findings — sierra-to-casm invocation lowering (int/)

Area: `crates/cairo-lang-sierra-to-casm/src/invocations/int/` — CASM lowering of the
integer libfuncs (add/sub/mul/divmod/diff/wide_mul/bitwise/from_felt252/is_zero/sqrt,
range checks).

## Summary

**No confirmed bug found.** I traced every libfunc lowering in the `int/` directory
against its documented semantics and the Sierra libfunc branch signatures, checking
range-check bounds, branch-target mapping, felt arithmetic, and MIN/MAX edge cases.
All of them are sound. Below I record the traces I performed so the negative result is
auditable. This is heavily-audited StarkWare production code and my conclusion is that
the assigned area is correct.

I deliberately did not re-file the three items listed as already resolved in this area
(the `2*limit-1` overflow comment in `unsigned.rs`, the stale annotations comments, and
the `CasmBuilder`/`casm!` items).

---

## Traces performed (all verified correct)

### 1. `mod.rs :: SmallDiffHelper` / `build_small_diff` (u* overflowing_sub, i* diff, i128/u128 diff)
- Overflow hint `a_ge_b = (a-b < limit)` is untrusted, but constraints pin the branch:
  - NoOverflow asserts `a-b ∈ [0,2^128)` ⇒ requires `a ≥ b` (a negative `a-b` becomes a
    ~prime felt and fails the RC). Returns `a-b`.
  - Overflow asserts `a-b+2^128 ∈ [0,2^128)` ⇒ requires `a-b ∈ [-2^128,0)`, i.e. `a<b`.
    Returns `a-b+limit` (the wrapped unsigned diff). For `limit==2^128` (`same_limit`) it
    reuses `fixed_a_minus_b = a-b+2^128`, which equals `a-b+limit`. Correct.
- Precondition `|a-b| < limit` holds: signed diff uses `limit = max-min+1 = 2^N`, and
  `a-b ∈ [min-max, max-min]` with `|a-b| ≤ 2^N-1 < limit`. Unsigned sub uses `limit=2^N`,
  `a-b ∈ (-2^N, 2^N)`. Both satisfy the assumption.
- Branch order matches the Diff signature `[Positive(a≥b), Negative(a<b)]`
  (`extensions/modules/int/signed.rs:166`). Fallthrough↔Positive, Overflow↔Negative. Correct.

### 2. `unsigned.rs :: build_small_uint_overflowing_add` (u8..u64)
- NoOverflow: single RC on `a+b + (2^128-limit) ∈ [0,2^128)` ⇒ `a+b < limit` (lower bound
  trivial since `a,b≥0`). Returns `a+b`.
- Overflow: RC on `a+b-limit ∈ [0,2^128)` ⇒ `a+b ≥ limit`; result `a+b-limit < limit`
  automatically because `a,b < limit ≤ 2^64`. Overflow branch → non-fallthrough handle.
  Matches OverflowingAdd (branch0 = no-overflow fallthrough). Correct.

### 3. `unsigned.rs :: build_sqrt` (u8..u128)
- `root` pinned to `[0,2^125)` by the pair of RCs (`root ∈ [0,2^128)` **and**
  `root + 2^128 - 2^125 ∈ [0,2^128)`); the second RC alone is defeatable by `root=P-1`
  (verified: `P-1 + 2^128-2^125 ≡ 2^128-2^125-1 < 2^128`), which is exactly why the
  `root ∈ [0,2^128)` RC is also required. Sound.
- `value-root² ∈ [0,2^128)` ⇒ `root² ≤ value`; `2·root-(value-root²) ∈ [0,2^128)` ⇒
  `value < (root+1)²`. With `root < 2^125`, `(root+1)² < 2^250 < P` so no wraparound; the
  pair uniquely determines `root = ⌊√value⌋`. For u128 `value < 2^128` the honest root
  `< 2^64` is representable. Checked MAX edge `value=2^128-1` ⇒ `root=2^64-1`, `diff=0`. OK.
  (The comment's `[0,2^250)` describes the algorithm's validity domain, not the actual
  input range; not a behavioral issue.)

### 4. `signed.rs :: build_sint_overflowing_operation` (i8..i128 add/sub)
- `canonical_value = value + (-min)`; in-range RC gives `value ≥ min`; the trailing
  `value + (2^128-1-max) ∈ [0,2^128)` RC (skipped only for i128, where the earlier RC
  already fully constrains) gives `value ≤ max`. The `TestLessThan` hint is untrusted but
  cannot mis-route:
  - value above max: Below path RC (`value+2^128-min`) overflows; in-range path fails the
    trailing `value ≤ max` RC ⇒ forced to Above. Verified with `i8, value=200` ⇒ Above,
    result `200-256`.
  - value below min: Above path RC (`value-(max+1)`) becomes ~prime and fails; in-range
    RC (`value-min`) becomes ~prime and fails ⇒ forced to Below. Verified with
    `i8, value=-200` ⇒ Below, result `56`.
- Below/Above lower bounds (`value ≥ min-2^128`, `value ≤ max+2^128`) are satisfied by the
  honest operand-sum ranges for both add and sub, including i128 (`sub` min `= -2^128+1`,
  `add` min `= -2^128`). Branch order `[in-range, below(underflow), above(overflow)]`
  matches the signature (`extensions/modules/int/signed.rs:101`). Correct.

### 5. `signed.rs :: build_sint_from_felt252`, `unsigned.rs :: FromFelt252` → `range_reduction.rs`
- In-range path: `value-lower ∈ [0,2^128)` and (when `size < 2^128`) `value+(2^128-upper) ∈
  [0,2^128)` together give `lower ≤ value < upper`. Out-of-range path delegates to
  `validate_under_limit::<2>` (in `misc`, out of scope) with `prime - size`. Correct within
  scope.

### 6. `unsigned128.rs :: build_u128_overflowing_add`
- NoOverflow: `a+b ∈ [0,2^128)`. Overflow: `a+b-2^128 ∈ [0,2^128)` ⇒ `a+b ∈ [2^128,2^129)`,
  returns wrapped. Correct.

### 7. `unsigned128.rs :: build_u128_from_felt252`
- `max_x = 2^123 + 17·2^64`, `max_y = 0` verified to equal `⌊(P-1)/2^128⌋` and `(P-1) mod
  2^128`. x,y RC'd to `[0,2^128)`; `value = 2^128·x + y`; overflow guard `x<max_x` or
  `(x==max_x ∧ y≤max_y)` prevents wraparound. Verified `value=2^128` ⇒ FailureHandle with
  `x=1,y=0`. Correct.

### 8. `unsigned128.rs :: build_u128_guarantee_mul` + `build_u128_mul_guarantee_verify`
- `a` split into `a0<2^64`, `a1<2^128` with exact reconstruction ⇒ unique. `carry ∈ [0,2^65)`,
  `partial_upper_word ∈ [0,2^128)`, all RC'd; `res_high = partial_upper_word + carry`. Even
  though `res_high` isn't independently RC'd `<2^128`, the fully-forced honest decomposition
  makes it equal the true high word `<2^128`. `lower_uint128_with_carry` bounded by 193 bits,
  no field wraparound. Correct.

### 9. `unsigned128.rs :: build_u128_byte_reverse` — bitwise-buffer masks/shifts and the final
`shift_inverse` multiply follow the documented byte-swap; no range-check correctness concern.

### 10. `bounded.rs :: build_div_rem` (u8..u128 divmod + bounded-int divrem)
- `q,r` RC'd `≥0`; `r<b` via `b-(r+1) ∈ [0,2^128)`; per-algorithm bound on `(q+1)·b ≤ prime`
  (KnownSmallRhs / KnownSmallQuotient / KnownSmallLhs) prevents wraparound in `a = b·q + r`.
  Correct.

### 11. `bounded.rs :: build_constrain / build_trim / build_guarantee_verify /
build_u128_to_u32_guarantees`
- `build_u128_to_u32_guarantees`: outputs 4 `BoundedIntGuarantee<0,2^32-1>` and does **only**
  reconstruction (`value = w3·2^96 + w2·2^64 + w1·2^32 + w0`) with no local RC. I confirmed
  soundness: the guarantee type is **non-droppable & non-duplicatable**
  (`extensions/modules/bounded_int.rs:47`) so each `wi` must be consumed by
  `bounded_int_guarantee_verify`, which RCs it to `[0,2^32)` (`Range::closed(0,u32::MAX)` =
  `[0,2^32)`, `validate_lt` in `build_guarantee_verify`). With every digit `<2^32` and
  `value<2^128<P`, the base-`2^32` decomposition is unique ⇒ digits forced correct. Correct.

### 12. `unsigned256.rs` (is_zero / divmod / sqrt / inv_mod_n) and `unsigned512.rs` (divmod)
- divmod `remainder < divisor` two-limb comparison: `diff1=d1-r1`; if `diff1≠0` RC `diff1`
  ⇒ `r1<d1`; else RC `d0-r0-1 ≥ 0` ⇒ `r0<d0`. Negative diffs become ~prime and fail. Correct.
- Product reconstruction forces `q_hi=0 ∨ d_hi=0` (asserts the paired high limb is zero) and
  bounds the surviving cross-term's smaller factor `<2^64` to avoid field wraparound
  (`qd*_small + (u128::MAX-u64::MAX) ∈ [0,2^128)`). limb-carry `leftover` values pinned to the
  documented small ranges via `leftover=leftover²` or explicit RC pairs. sqrt allows
  `sqrt0,sqrt1<2^65` by design with the stated soundness argument; the `2·sqrt-remainder`
  positivity check cannot be mis-routed by the untrusted branch hint (both branches reject a
  ~prime value). inv_mod_n `g>1 ∨ g=n=1` and `g·s=b, g·t=n` limb checks with the same
  smaller-factor-`<2^64` anti-wraparound guard. All correct.

---

## Files checked
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/mod.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/signed.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned128.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/signed128.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/bounded.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned256.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned512.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/range_reduction.rs` (called by from_felt252)
- Cross-checked signatures/semantics in:
  `crates/cairo-lang-sierra/src/extensions/modules/int/signed.rs`,
  `crates/cairo-lang-sierra/src/extensions/modules/bounded_int.rs`,
  `crates/cairo-lang-sierra/src/extensions/modules/utils.rs` (`Range`)
