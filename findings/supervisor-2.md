# Supervisor #2 adjudication — Group 2 (Casm/Sierra codegen)

Hunters: H6 (sierra-to-casm invocations/int), H7 (cairo-lang-casm operand/instructions/hints),
H8 (cairo-lang-sierra extensions/modules), H9 (sierra-type-size + program/registry),
H10 (sierra-to-casm invocations boxing/array/dict).

Scope note: I did not read/grep `crates/cairo-lang-syntax/src/node/ast.rs` (per repo
instructions); it is irrelevant to this group in any case. No commit/push/PR performed.

---

## Findings adjudicated

### H7 · Finding 1 — `GetNextDictKey` renders as a malformed multi-line hint
**File:** `crates/cairo-lang-casm/src/hints/mod.rs:658-661`
(interacts with `crates/cairo-lang-casm/src/instructions.rs:65-83`, `Display for Instruction`).

**Verdict: CONFIRMED (novel; low-severity, cosmetic — textual `.casm` dump only).**

Independent verification:
1. *Source trace.* I opened `hints/mod.rs`. Every multi-line Pythonic hint template opens
   with a blank line immediately after `formatdoc! {"` — verified for `LinearSplit` (606),
   `RandomEcPoint` (616), `FieldSqrt` (625), `GetCurrentAccessIndex` (639),
   `GetCurrentAccessDelta` (649-650), `GetSegmentArenaIndex` (664), `InitSquashData` (683).
   `GetNextDictKey` (658-661) is written with the `assert ...` line directly after the opening
   quote, with no blank line. I confirmed uniqueness mechanically: 20 `formatdoc! {"`
   occurrences in the file, 19 of which are immediately followed by a blank line — the single
   exception is `GetNextDictKey`. `indoc`/`formatdoc` strips only the one newline right after
   the opening quote, so the siblings' rendered strings still begin with `\n` while
   `GetNextDictKey`'s begins with `assert`.
2. *Display contract.* `instructions.rs:70` branches on `hint_str.starts_with('\n')`: multi-line
   form `%{{{hint_str}%}}` vs single-line form `%{{ {hint_str} %}}`. Because `GetNextDictKey`'s
   string does not start with `\n`, it wrongly takes the single-line branch even though it is a
   two-line body.
3. *Empirical reproduction.* I temporarily added a test to `instructions_test.rs` building an
   `Instruction` (Ret body) carrying `CoreHint::GetNextDictKey { next_key: cell_ref!([ap + 0]) }`
   and printed `to_string()`. Actual output (run under `cargo test -p cairo-lang-casm`):
   ```
   %{ assert len(keys) > 0, 'No keys left but remaining_accesses > 0.'
   memory[ap + 0] = key = keys.pop()
    %}
   ret
   ```
   For contrast, `GetCurrentAccessDelta` in the same harness rendered correctly:
   ```
   %{
   new_access_index = current_access_indices.pop()
   ...
   %}
   ret
   ```
   The `starts_with("%{\n")` assertion fails for `GetNextDictKey` and passes for the sibling —
   exactly matching H7's description. I then reverted the test file (`git checkout`); repo is
   clean.

**Test-legitimacy judgment: LEGITIMATE.** H7's proposed test uses only the normal public API —
constructs an `Instruction` via its public struct literal and inspects `Display`/`to_string()`.
No internals are poked, no invalid state is manufactured; `GetNextDictKey` with an `[ap + 0]`
cell ref is an ordinary, valid hint. The asserted expectation (`starts_with("%{\n")` — the
canonical multi-line framing every other multi-line hint produces) is the correct contract, so
the red test genuinely demonstrates the inconsistency rather than forcing it.

**Severity / impact: correctly characterized as cosmetic.** The defect only affects the
human-readable textual rendering of an instruction carrying this hint (debug output / golden
text). The hint body content, the executed Pythonic semantics, and every binary encoding are
unaffected — only the surrounding `%{ … %}` whitespace framing is off, and the block remains
delimited/parseable. Fix is a one-line addition of the leading blank line in the template.
Not present in Team Memory (confirmed against the DUP list) — **NOVEL**.

---

### H8 · Suspected area — `circuit.rs` gate-offset bookkeeping (NOT a filed bug)
**File:** `crates/cairo-lang-sierra/src/extensions/modules/circuit.rs`
(`get_circuit_info`/`parse_circuit_inputs`, `GateOffsets`, `add_offsets`/`mul_offsets`, the
`SubModGate`→`AddModGate` rewrite).

**Verdict: SUSPECTED — not demonstrated. No actionable bug; nothing to confirm or reject.**

Rationale: H8 explicitly did **not** claim a bug here. They traced the DFS-based offset
construction by hand, found it self-consistent (including the `output = sub_lhs - sub_rhs ⇒
output + sub_rhs = sub_lhs` gate-direction rewrite), but did not build an independent reference
implementation or a circuit-evaluation Cairo harness, so they flagged it as a residual
uncertainty for a future pass. There is no reproduction and no test to scrutinize. I concur
with the framing: this is a time-boxed "unproven-clean" region, not a defect. It should be
recorded as an open area for a dedicated future hunter (a `eval_circuit` Cairo harness exercising
`AddMod`/`MulMod`/`Inverse`/`SubMod` gate trees), **not** carried forward as a bug.

---

## Clean areas (no bug reported — recorded as clean)

### H6 — sierra-to-casm `invocations/int/`
**Verdict: CLEAN (accepted).** H6 reports no bug after a per-libfunc trace of the integer
lowerings (add/sub/mul/divmod/diff/wide_mul/sqrt/from_felt252/is_zero and their range checks),
recording an auditable trace per function. Spot-checking the reasoning: the soundness arguments
follow the standard "untrusted branch hint, but both branches independently pinned by
range-checks" pattern that is pervasive and correct throughout this crate (same pattern H10
verified empirically in `array.rs`). H6 correctly and deliberately declined to re-file the three
already-resolved items in this area — importantly the `unsigned.rs` `2*limit-1` bound, which
Team Memory records as CORRECT/REJECTED. Nothing here contradicts memory; no false positive to
catch. Accepted as clean.

### H9 — sierra-type-size + sierra program/registry
**Verdict: CLEAN (accepted, with a caveat).** Per the group brief, H9 reported "no bug found."
**Caveat: no report file was produced** — `/home/user/cairo/findings/hunter-9.md` does not
exist (the findings directory contains H2,3,5,6,7,8,10,11,13,14,15 but no H9). I therefore
cannot audit H9's trace or file list; I am recording the area as clean solely on the brief's
statement. Flagging the missing report so the final write-up notes the area was covered only at
the "declared clean, unverified by supervisor" level.

### H10 — sierra-to-casm `invocations/array.rs` (+ boxing/dict spot-checks)
**Verdict: CLEAN (accepted; strongest of the negatives).** H10 (a) confirmed `array.rs` is
byte-identical to `origin/main` (shipped production lowering), (b) traced every libfunc
(`array_new`, span/tuple conversions, append, pop_front/back, snapshot pops, multi-pop
front/back + failure checks, get, slice, len) against the documented semantics at exactly the
boundary cases most likely to hide off-by-ones (empty-array pop, index/slice at the exact end,
exact-vs-over multi-pop), and (c) **empirically** exercised those boundaries via two idiomatic
Cairo programs run through a locally built `cairo-run`, both returning
`Run completed successfully, returning [0x1]`. The two programs are idiomatic, normal Cairo (no
internals abuse) and directly hit the named boundaries — a legitimate, well-constructed negative
result. Accepted as clean.

### H8 — cairo-lang-sierra `extensions/modules/` (aside from the suspected circuit area above)
**Verdict: CLEAN (accepted).** H8 investigated nine concrete "looks-wrong" hypotheses
(array multi-pop `PartialParam` vs `NewTempVar` asymmetry; multi-pop type nesting order;
`BoundedIntMul` NonZero symmetry; `EnumFromBoundedInt` identity-vs-computed split;
`Downcast` `from==to` forced-overflow back-compat path; `Felt252Dict` value-type restriction;
struct/enum boxed-deconstruct leading-zero-sized `SameAsParam{0}` runs; `IntRangePopFront`
branch-order convention; `GasReserveCreate` `SameAsParam{2}`) and ruled each out by
cross-checking the matching `sierra-to-casm` lowering and/or the corelib `extern` declaration.
Each ruled-out item is a genuine metadata↔lowering match, not a bug — these are correct
rejections of false leads, which is exactly the discipline this review wants. Accepted as clean.
(H8 also lists modules it did **not** review at all — `int/signed*`, `bitwise`, `pedersen`,
`poseidon`, `blake`, `qm31`, `segment_arena`, `starknet/*` — noted as coverage gaps, not clean.)

### H7 — remainder of cairo-lang-casm (operand/instructions/assembler/encoder + hint typing)
**Verdict: CLEAN (accepted).** Beyond Finding 1, H7's "checked and correct" items are sound:
encoder offset-centering (`off + 2^15`, `off:i16` ⇒ exact `[0,2^16)`), `flags << 48` typed
`u64` (no overflow), opcode-extension bits (Stone/Blake2s/QM31) non-colliding, assembler
res-descriptions, `op_size` immediate accounting, and the hint formatter-type choices
(`ResOperandAsAddressFormatter` for pointers vs `ResOperandAsIntegerFormatter` with `% PRIME`
for values; SCALE codec indices unique). H7 also correctly stayed clear of the two off-limits
memory items in this crate (`CasmBuilder::bin_op` panic; the `jmp rel [cell]` macro arm).
Accepted as clean.

---

## Dedup against Team Memory
- The **only** confirmed finding (H7 · `GetNextDictKey`) is explicitly **not** in the DUP list
  and is treated as NOVEL per the group brief.
- H6 correctly avoided re-filing the REJECTED `unsigned.rs` `2*limit-1` item and the
  `CasmBuilder`/`casm!` items. H7 avoided the `bin_op` panic and `jmp rel [cell]` macro items.
  No hunter re-filed any known/rejected item — no DUP verdicts required.

---

## Files checked (merged, deduped across H6/H7/H8/H10; H9 produced no file list)

sierra-to-casm invocations:
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/mod.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/signed.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned128.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/signed128.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/bounded.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned256.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned512.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/range_reduction.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/array.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/enm.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/structure.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/range.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/casts.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/gas_reserve.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/gas.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/boxing.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/felt252_dict.rs`

cairo-lang-casm:
- `crates/cairo-lang-casm/src/operand.rs` (+ `operand_test.rs`)
- `crates/cairo-lang-casm/src/instructions.rs` (+ `instructions_test.rs`)
- `crates/cairo-lang-casm/src/assembler.rs` (+ `assembler_test.rs`)
- `crates/cairo-lang-casm/src/encoder.rs` (+ `encoder_test.rs`)
- `crates/cairo-lang-casm/src/hints/mod.rs` (+ `hints/test.rs`)  ← confirmed finding here
- `crates/cairo-lang-casm/src/cell_expression.rs`
- `crates/cairo-lang-casm/src/ap_change.rs`
- `crates/cairo-lang-casm/src/inline.rs`
- `crates/cairo-lang-casm/src/builder.rs`

cairo-lang-sierra extensions/modules:
- `crates/cairo-lang-sierra/src/extensions/modules/enm.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/structure.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/array.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/nullable.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/boxing.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/bounded_int.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/felt252_dict.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/squashed_felt252_dict.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/circuit.rs`  ← suspected (unproven) area
- `crates/cairo-lang-sierra/src/extensions/modules/bytes31.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/coupon.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/range.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/ec.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/casts.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/is_zero.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/try_from_felt252.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/consts.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/const_type.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/span.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/gas_reserve.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/utils.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/int/signed.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/int/unsigned.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/int/unsigned128.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/int/unsigned256.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/bounded_int.rs`
- `crates/cairo-lang-sierra/src/extensions/lib_func.rs`

corelib (cross-referenced):
- `corelib/src/array.cairo`, `corelib/src/dict.cairo`, `corelib/src/math.cairo`,
  `corelib/src/internal/bounded_int.cairo`

Not covered by any file list: sierra-type-size + sierra program/registry (H9's assigned area —
no report file was produced, so no specific files can be enumerated).
