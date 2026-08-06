# Hunter #8 — Sierra libfunc extension definitions (crates/cairo-lang-sierra/src/extensions/modules/)

## Summary

No confirmed bug was found after an extensive manual audit of the signature-specialization
logic in `crates/cairo-lang-sierra/src/extensions/modules/`. This report documents the modules
reviewed, the specific hypotheses that were investigated and *ruled out* (with the concrete
evidence that ruled them out), and the areas that remain unexamined due to time constraints.

I did not want to file a "bug" built on a misunderstanding of the (fairly subtle)
`OutputVarReferenceInfo` / `SierraApChange` contract, so for every suspicious-looking asymmetry
I cross-checked the corresponding `cairo-lang-sierra-to-casm` codegen and/or the corelib extern
declaration to see whether the Sierra-level signature actually matches the real lowering. In
every case examined below, it did.

## Methodology

For each libfunc module: read the `specialize_signature` (and, where present, `specialize`)
implementation, and checked:
- input/output type computation (wrapping, snapshot, box, struct/enum member extraction),
- branch count / branch var count / fallthrough index consistency,
- `OutputVarReferenceInfo` (SameAsParam / PartialParam / NewTempVar / Deferred / ZeroSized)
  plausibility,
- `SierraApChange` (`new_vars_only` correctness),
- edge cases: zero-variant enums, empty structs, single-variant enums, n_variants boundary
  conditions, range-arithmetic off-by-ones (half-open vs closed ranges).

Where a discrepancy looked plausible, I read the matching `cairo-lang-sierra-to-casm`
`invocations/*` builder to check whether the declared metadata actually matches the emitted
CASM reference pattern, and/or the corelib `extern fn` declaration in `corelib/src/*.cairo` to
check the declared type nesting order.

## Hypotheses investigated and ruled out

1. **`array.rs` — `ArraySnapshotMultiPopFrontLibfunc` vs `ArraySnapshotMultiPopBackLibfunc`**
   (`crates/cairo-lang-sierra/src/extensions/modules/array.rs:538` and `:605`): the two
   libfuncs use different `OutputVarReferenceInfo` for the popped-box output
   (`PartialParam{1}` vs `NewTempVar{0}`). Ruled out: cross-checked
   `crates/cairo-lang-sierra-to-casm/src/invocations/array.rs::build_multi_pop_front/back` —
   pop-front reuses the original `arr_start` var unchanged (`PartialParam` is correct), while
   pop-back computes a fresh `tempvar new_end` (`NewTempVar` is correct). Asymmetry is real but
   intentional and correctly reflected.

2. **`array.rs` — multi-pop output type nesting `Snapshot<Box<PoppedT>>`** vs single-pop's
   `Box<Snapshot<T>>` (reversed order). Ruled out: `corelib/src/array.cairo:87-92` declares
   `array_snapshot_multi_pop_front/back(...) -> Option<@Box<PoppedT>>` (i.e.
   `Snapshot<Box<T>>`), which matches the Sierra module exactly; the single-pop functions
   declare `Option<Box<@T>>`, also matching. Both orders are correct for their respective
   libfuncs.

3. **`bounded_int.rs` — `BoundedIntMulLibfunc` requiring `lhs`/`rhs` `NonZero`-ness to match**
   (`:150-154`). Looked like an arbitrary/asymmetric restriction. Ruled out: corelib's
   `MulHelper` trait (`corelib/src/internal/bounded_int.cairo:154-160`) only has impls for
   `MulHelper<Lhs, Rhs>` and `MulHelper<NonZero<Lhs>, NonZero<Rhs>>` — i.e. the corelib-level
   trait is deliberately symmetric, so the libfunc-level check matches by design.

4. **`enm.rs` — `EnumFromBoundedIntLibfunc` uses `reinterpret_cast_signature` (identity) for
   `n_variants <= 2` but `Deferred(Generic)` with an actual computation for `n_variants > 2`**
   (`:251-269`). Ruled out: cross-checked
   `crates/cairo-lang-sierra-to-casm/src/invocations/enm.rs::build_enum_from_bounded_int` — for
   `n_variants <= 2` the codegen is literally `build_identity` (no-op), while for
   `n_variants > 2` it computes a jump-offset variant selector via
   `-2 * (value - m)`, a real arithmetic transform. The differing `OutputVarReferenceInfo`
   matches the differing codegen exactly.

5. **`casts.rs` — `DowncastConcreteLibfunc::cast_type()` special-cases `from_ty == to_ty`
   to force `overflow_above: true`** even though intersection would normally yield no overflow
   at all (`:66-77`). Ruled out as a bug (comment explicitly says "Backwards compatibility"):
   traced through `cairo-lang-sierra-to-casm/src/invocations/casts.rs::build_downcast` and
   `add_directional_downcast` — the forced path emits one extra (always-true) range check
   using the type's own bound, which is sound, just historically kept for output stability; it
   does not admit any out-of-range value.

6. **`felt252_dict.rs` — `Felt252DictTypeWrapped` enum-value validation** requires
   `generic_args.len() <= 3` (≤ 2 variants) and all variant payload types zero-sized
   (`:71-88`). Cross-checked against `corelib/src/dict.cairo`/`bool`'s definition
   (`enum bool { False: (), True: () }`) — matches the known restriction that only
   `Felt252DictValue`-eligible numeric/Nullable/bool-like types can be dict values.

7. **`structure.rs`/`enm.rs` — `StructBoxedDeconstructLibfunc`/`EnumBoxedMatchLibfunc`
   `create_signature` loops** that mark a run of leading zero-sized members as
   `SameAsParam{0}` and the remainder as `Deferred(AddConst)`. Cross-checked against
   `cairo-lang-sierra-to-casm/src/invocations/structure.rs::boxed_members_cell_exprs`, which
   computes every member's address as `boxed_struct_ptr + current_offset` with
   `current_offset` staying `0` through every leading zero-sized member — confirms the
   `SameAsParam{0}` classification is literally correct (offset 0 is identical to the input
   pointer) and the post-offset members are indeed constant-offset (`AddConst`) computations.

8. **`range.rs` — `IntRangePopFrontLibfunc` branch order** (branch 0 = empty/no-vars is the
   *fallthrough*, branch 1 = non-empty is the jump target) — opposite ordering convention from
   most other "pop"/"match" libfuncs in this codebase (which put the "interesting"/non-empty
   case at index 0). Cross-checked `cairo-lang-sierra-to-casm/src/invocations/range.rs
   ::build_pop_front`, which builds `[("Fallthrough", &[], None), ("NonEmpty", ..., Some(...))]`
   — the ordering is internally consistent between the Sierra signature and the CASM builder, so
   this is just a different (valid) convention, not a bug.

9. **`gas_reserve.rs` — `GasReserveCreateLibfunc` success branch marks the new `GasReserve`
   output as `SameAsParam{param_idx: 2}`** (literally identical to the requested-amount input).
   Cross-checked `cairo-lang-sierra-to-casm/src/invocations/gas_reserve.rs
   ::build_gas_reserve_create` (built on `SmallDiffHelper`) — the reserve's runtime
   representation actually is the pass-through amount cell `b`, confirming the metadata is
   correct.

None of the above panned out into an actual bug; all are consistent, cross-checked pairs of
Sierra-level signature metadata and their corresponding CASM lowering (or corelib contract).

## Modules reviewed (no discrepancy found, or discrepancy found-and-resolved as above)

- `enm.rs` (Enum type + `enum_init`/`enum_from_bounded_int`/`enum_match`/
  `enum_snapshot_match`/`enum_boxed_match`)
- `structure.rs` (Struct type + `struct_construct`/`struct_deconstruct`/
  `struct_snapshot_deconstruct`/`struct_boxed_deconstruct`)
- `array.rs` (all `Array`/`Span` libfuncs: new, append, pop_front(_consume), get, slice, len,
  snapshot pop front/back, snapshot multi-pop front/back, span_from_tuple/tuple_from_span)
- `nullable.rs` (`null`, `nullable_from_box`, `match_nullable`, `nullable_forward_snapshot`)
- `boxing.rs` (`into_box`, `local_into_box`, `unbox`, `box_forward_snapshot`)
- `bounded_int.rs` (add/sub/mul/div_rem/constrain/trim_min/trim_max/is_zero/wrap_non_zero/
  guarantee_verify/u128_to_u32_guarantees)
- `felt252_dict.rs` (`felt252_dict_new`, `felt252_dict_squash`, entry get/finalize)
- `squashed_felt252_dict.rs` (`squashed_felt252_dict_entries`)
- `circuit.rs` (types + `add_circuit_input`, `eval_circuit`, `get_circuit_descriptor`,
  `init_circuit_data`, `get_circuit_output`, `try_into_circuit_modulus`,
  `circuit_failure_guarantee_verify`, `into_u96_guarantee`, `u96_guarantee_verify`,
  `u96_limbs_less_than_guarantee_verify(_single_limb)`, and `get_circuit_info`/
  `parse_circuit_inputs` gate-offset construction) — reviewed in full but not exhaustively
  proven against a from-scratch spec; see "Not fully verified" below.
- `bytes31.rs`, `coupon.rs`, `range.rs` (`int_range_try_new`/`int_range_pop_front`), `ec.rs`,
  `casts.rs` (`upcast`/`downcast`), `is_zero.rs`, `try_from_felt252.rs`,
  `int/unsigned.rs`, `int/unsigned128.rs`, `int/unsigned256.rs`, `consts.rs`, `const_type.rs`,
  `span.rs`, `gas_reserve.rs`

## Not fully verified (time-boxed out)

- `circuit.rs`'s `get_circuit_info`/`parse_circuit_inputs` gate-offset bookkeeping
  (`GateOffsets`, `add_offsets`/`mul_offsets`, the `SubModGate`-as-`AddModGate` transform) is
  intricate DFS-based logic that I traced by hand and found self-consistent (including the
  `output = sub_lhs - sub_rhs => output + sub_rhs = sub_lhs` gate-direction rewrite), but I did
  not build an independent reference implementation or fuzz it, so a subtle offset bug there
  cannot be ruled out with full confidence. I did not have time to write a circuit-evaluation
  Cairo test harness (circuits require constructing `AddModGate`/`MulModGate`/`InverseGate`
  trees and running `eval_circuit`, which is comparatively heavy to set up) to actively hunt
  here; flagging as **suspected area, not demonstrated** for a future pass, not a claimed bug.
- `int/signed.rs`, `int/signed128.rs`, `int/mod.rs` (shared `IntType`/`IntOperator`/
  `IntWideMulLibfunc` machinery), `bitwise.rs`, `pedersen.rs`, `poseidon.rs`, `blake.rs`,
  `qm31.rs`, `segment_arena.rs`, `starknet/*` were not reviewed at all due to time constraints.

## Files checked

- `crates/cairo-lang-sierra/src/extensions/modules/enm.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/structure.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/array.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/nullable.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/boxing.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/bounded_int.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/felt252_dict.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/squashed_felt252_dict.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/circuit.rs`
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
- `crates/cairo-lang-sierra/src/extensions/modules/utils.rs` (Range, peel_snapshot,
  reinterpret_cast_signature helpers)
- `crates/cairo-lang-sierra/src/extensions/modules/int/unsigned.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/int/unsigned128.rs`
- `crates/cairo-lang-sierra/src/extensions/modules/int/unsigned256.rs`
- `crates/cairo-lang-sierra/src/extensions/lib_func.rs` (read-only, for
  `OutputVarReferenceInfo`/`SierraApChange` semantics)

Cross-referenced (read-only, to confirm Sierra-level metadata against actual lowering —
no bugs filed against these off-limits/adjacent crates):
- `crates/cairo-lang-sierra-to-casm/src/invocations/array.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/enm.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/structure.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/range.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/casts.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/gas_reserve.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/mod.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/unsigned128.rs`
- `crates/cairo-lang-sierra-to-casm/src/invocations/int/bounded.rs`
- `corelib/src/array.cairo`, `corelib/src/dict.cairo`, `corelib/src/math.cairo`,
  `corelib/src/internal/bounded_int.cairo`
